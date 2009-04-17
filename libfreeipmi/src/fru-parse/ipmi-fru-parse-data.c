/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.

*/
/*****************************************************************************\
 *  $Id: ipmi-fru-parse-data.c,v 1.1.2.8 2009-04-17 16:47:10 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/fru-parse/ipmi-fru-parse.h"
#include "freeipmi/api/ipmi-fru-inventory-device-cmds-api.h"
#include "freeipmi/cmds/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-fru-information-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-fru-parse-common.h"
#include "ipmi-fru-parse-defs.h"
#include "ipmi-fru-parse-trace.h"
#include "ipmi-fru-parse-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

static int
_parse_type_length (ipmi_fru_parse_ctx_t ctx,
                    uint8_t *areabuf,
                    unsigned int areabuflen,
                    unsigned int current_area_offset,
                    uint8_t *number_of_data_bytes,
                    ipmi_fru_parse_field_t *field)
{
  uint8_t type_length;
  uint8_t type_code;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (areabuf);
  assert (areabuflen);
  assert (number_of_data_bytes);
  
  type_length = areabuf[current_area_offset];
  type_code = (type_length & IPMI_FRU_TYPE_LENGTH_TYPE_CODE_MASK) >> IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SHIFT;
  (*number_of_data_bytes) = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

  /* Special Case: This shouldn't be a length of 0x01 (see type/length
   * byte format in FRU Information Storage Definition).
   */
  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_LANGUAGE_CODE
      && (*number_of_data_bytes) == 0x01)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
      return (-1);
    }

  if ((current_area_offset + 1 + (*number_of_data_bytes)) > areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
      return (-1);
    }

  if (field)
    {
      memset (field->type_length_field,
              '\0',
              IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX);
      memcpy (field->type_length_field,
              &areabuf[current_area_offset],
              1 + (*number_of_data_bytes));
      field->type_length_field_length = 1 + (*number_of_data_bytes);
    }
      
  return (0);
}
                    
int
ipmi_fru_parse_chassis_info_area (ipmi_fru_parse_ctx_t ctx,
                                  uint8_t *areabuf,
                                  unsigned int areabuflen,
                                  uint8_t *chassis_type,
                                  ipmi_fru_parse_field_t *chassis_part_number,
                                  ipmi_fru_parse_field_t *chassis_serial_number,
                                  ipmi_fru_parse_field_t *chassis_custom_fields,
                                  unsigned int chassis_custom_fields_len)
{
  unsigned int area_offset = 0;
  unsigned int custom_fields_index = 0;
  uint8_t number_of_data_bytes;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  if (chassis_part_number)
    memset (chassis_part_number,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (chassis_serial_number)
    memset (chassis_serial_number,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (chassis_custom_fields && chassis_custom_fields_len)
    memset (chassis_custom_fields,
            '\0',
            sizeof (ipmi_fru_parse_field_t) * chassis_custom_fields_len);

  if (chassis_type)
    (*chassis_type) = areabuf[area_offset];
  area_offset++;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          chassis_part_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          chassis_serial_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  while (area_offset < areabuflen
         && areabuf[area_offset] != IPMI_FRU_SENTINEL_VALUE)
    {
      ipmi_fru_parse_field_t *field_ptr = NULL;

      if (chassis_custom_fields && chassis_custom_fields_len)
        {
          if (custom_fields_index < chassis_custom_fields_len)
            field_ptr = &chassis_custom_fields[custom_fields_index];
          else
            {
              FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_OVERFLOW);
              goto cleanup;
            }
        }

      if (_parse_type_length (ctx,
                              areabuf,
                              areabuflen,
                              area_offset,
                              &number_of_data_bytes,
                              field_ptr) < 0)
        goto cleanup;

      area_offset += 1;          /* type/length byte */
      area_offset += number_of_data_bytes;
      custom_fields_index++;
    }

#if 0
  if (area_offset > areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_SENTINEL_VALUE_NOT_FOUND);
      goto cleanup;
    }
#endif

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_parse_board_info_area (ipmi_fru_parse_ctx_t ctx,
                                uint8_t *areabuf,
                                unsigned int areabuflen,
                                uint8_t *language_code,
                                uint32_t *mfg_date_time,
                                ipmi_fru_parse_field_t *board_manufacturer,
                                ipmi_fru_parse_field_t *board_product_name,
                                ipmi_fru_parse_field_t *board_serial_number,
                                ipmi_fru_parse_field_t *board_part_number,
                                ipmi_fru_parse_field_t *board_fru_file_id,
                                ipmi_fru_parse_field_t *board_custom_fields,
                                unsigned int board_custom_fields_len)
{
  uint32_t mfg_date_time_tmp = 0;
  unsigned int area_offset = 0;
  unsigned int custom_fields_index = 0;
  uint8_t number_of_data_bytes;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  if (board_manufacturer)
    memset (board_manufacturer,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (board_product_name)
    memset (board_product_name,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (board_serial_number)
    memset (board_serial_number,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (board_part_number)
    memset (board_part_number,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (board_fru_file_id)
    memset (board_fru_file_id,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (board_custom_fields && board_custom_fields_len)
    memset (board_custom_fields,
            '\0',
            sizeof (ipmi_fru_parse_field_t) * board_custom_fields_len);

  if (language_code)
    (*language_code) = areabuf[area_offset];
  area_offset++;

  if (mfg_date_time)
    {
      /* mfg_date_time is little endian - see spec */
      mfg_date_time_tmp |= areabuf[area_offset];
      area_offset++;
      mfg_date_time_tmp |= (areabuf[area_offset] << 8);
      area_offset++;
      mfg_date_time_tmp |= (areabuf[area_offset] << 16);
      area_offset++;
      
      /* mfg_date_time is in minutes, so multiple by 60 to get seconds */
      mfg_date_time_tmp *= 60;

      /* In FRU, epoch is 0:00 hrs 1/1/96
       *
       * So convert into ansi epoch
       *
       * 26 years difference in epoch
       * 365 days/year
       * etc.
       *
       */
      mfg_date_time_tmp += (26 * 365 * 24 * 60 * 60);
      (*mfg_date_time) = mfg_date_time_tmp;
    }
  else
    area_offset += 3;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_manufacturer) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_product_name) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_serial_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_part_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_fru_file_id) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  while (area_offset < areabuflen
         && areabuf[area_offset] != IPMI_FRU_SENTINEL_VALUE)
    {
      ipmi_fru_parse_field_t *field_ptr = NULL;

      if (board_custom_fields && board_custom_fields_len)
        {
          if (custom_fields_index < board_custom_fields_len)
            field_ptr = &board_custom_fields[custom_fields_index];
          else
            {
              FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_OVERFLOW);
              goto cleanup;
            }
        }

      if (_parse_type_length (ctx,
                              areabuf,
                              areabuflen,
                              area_offset,
                              &number_of_data_bytes,
                              field_ptr) < 0)
        goto cleanup;

      area_offset += 1;          /* type/length byte */
      area_offset += number_of_data_bytes;
      custom_fields_index++;
    }

#if 0
  if (area_offset > areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_SENTINEL_VALUE_NOT_FOUND);
      goto cleanup;
    }
#endif

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_parse_product_info_area (ipmi_fru_parse_ctx_t ctx,
                                  uint8_t *areabuf,
                                  unsigned int areabuflen,
                                  uint8_t *language_code,
                                  ipmi_fru_parse_field_t *product_manufacturer_name,
                                  ipmi_fru_parse_field_t *product_name,
                                  ipmi_fru_parse_field_t *product_part_model_number,
                                  ipmi_fru_parse_field_t *product_version,
                                  ipmi_fru_parse_field_t *product_serial_number,
                                  ipmi_fru_parse_field_t *product_asset_tag,
                                  ipmi_fru_parse_field_t *product_fru_file_id,
                                  ipmi_fru_parse_field_t *product_custom_fields,
                                  unsigned int product_custom_fields_len)
{
  unsigned int area_offset = 0;
  unsigned int custom_fields_index = 0;
  uint8_t number_of_data_bytes;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  if (product_manufacturer_name)
    memset (product_manufacturer_name,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (product_name)
    memset (product_name,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (product_part_model_number)
    memset (product_part_model_number,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (product_version)
    memset (product_version,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (product_serial_number)
    memset (product_serial_number,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (product_asset_tag)
    memset (product_asset_tag,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (product_fru_file_id)
    memset (product_fru_file_id,
            '\0',
            sizeof (ipmi_fru_parse_field_t));
  if (product_custom_fields && product_custom_fields_len)
    memset (product_custom_fields,
            '\0',
            sizeof (ipmi_fru_parse_field_t) * product_custom_fields_len);

  if (language_code)
    (*language_code) = areabuf[area_offset];
  area_offset++;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_manufacturer_name) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_name) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_part_model_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_version) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_serial_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_asset_tag) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (_parse_type_length (ctx,
                          areabuf,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_fru_file_id) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  while (area_offset < areabuflen
         && areabuf[area_offset] != IPMI_FRU_SENTINEL_VALUE)
    {
      ipmi_fru_parse_field_t *field_ptr = NULL;

      if (product_custom_fields && product_custom_fields_len)
        {
          if (custom_fields_index < product_custom_fields_len)
            field_ptr = &product_custom_fields[custom_fields_index];
          else
            {
              FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_OVERFLOW);
              goto cleanup;
            }
        }

      if (_parse_type_length (ctx,
                              areabuf,
                              areabuflen,
                              area_offset,
                              &number_of_data_bytes,
                              field_ptr) < 0)
        goto cleanup;

      area_offset += 1;          /* type/length byte */
      area_offset += number_of_data_bytes;
      custom_fields_index++;
    }

#if 0
  if (area_offset > areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_SENTINEL_VALUE_NOT_FOUND);
      goto cleanup;
    }
#endif

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_parse_multirecord_power_supply_information (ipmi_fru_parse_ctx_t ctx,
                                                     uint8_t *areabuf,
                                                     unsigned int areabuflen,
                                                     unsigned int *overall_capacity,
                                                     unsigned int *peak_va,
                                                     unsigned int *inrush_current,
                                                     unsigned int *inrush_interval,
                                                     unsigned int *low_end_input_voltage_range_1,
                                                     unsigned int *high_end_input_voltage_range_1,
                                                     unsigned int *low_end_input_voltage_range_2,
                                                     unsigned int *high_end_input_voltage_range_2,
                                                     unsigned int *low_end_input_frequency_range,
                                                     unsigned int *high_end_input_frequency_range,
                                                     unsigned int *ac_dropout_tolerance,
                                                     unsigned int *predictive_fail_support,
                                                     unsigned int *power_factor_correction,
                                                     unsigned int *autoswitch,
                                                     unsigned int *hot_swap_support,
                                                     unsigned int *tachometer_pulses_per_rotation_predictive_fail_polarity,
                                                     unsigned int *peak_capacity,
                                                     unsigned int *hold_up_time,
                                                     unsigned int *voltage_1,
                                                     unsigned int *voltage_2,
                                                     unsigned int *total_combined_wattage,
                                                     unsigned int *predictive_fail_tachometer_lower_threshold)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  rv = 0;
 cleanup:
  return (rv);
}
