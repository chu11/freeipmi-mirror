/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
/*****************************************************************************\
 *  $Id: ipmi-fru-data.c,v 1.14 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include "freeipmi/fru/ipmi-fru.h"
#include "freeipmi/api/ipmi-fru-inventory-device-cmds-api.h"
#include "freeipmi/cmds/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-fru-information-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-fru-common.h"
#include "ipmi-fru-defs.h"
#include "ipmi-fru-trace.h"
#include "ipmi-fru-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

static int
_parse_type_length (ipmi_fru_ctx_t ctx,
                    const void *areabuf,
                    unsigned int areabuflen,
                    unsigned int current_area_offset,
                    uint8_t *number_of_data_bytes,
                    ipmi_fru_field_t *field)
{
  const uint8_t *areabufptr = areabuf;
  uint8_t type_length;
  uint8_t type_code;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (areabuf);
  assert (areabuflen);
  assert (number_of_data_bytes);
  
  type_length = areabufptr[current_area_offset];

  /* IPMI Workaround 
   *
   * Dell Poweredge R610
   *
   * My reading of the FRU Spec is that all non-custom fields are
   * required to be listed by the vendor.  However, on this
   * motherboard, some areas list this, indicating that there is
   * no more data to be parsed.  So now, for "required" fields, I
   * check to see if the type-length field is a sentinel before
   * calling this function.
   */

  assert (type_length != IPMI_FRU_SENTINEL_VALUE);

  type_code = (type_length & IPMI_FRU_TYPE_LENGTH_TYPE_CODE_MASK) >> IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SHIFT;
  (*number_of_data_bytes) = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

  /* Special Case: This shouldn't be a length of 0x01 (see type/length
   * byte format in FRU Information Storage Definition).
   */
  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_LANGUAGE_CODE
      && (*number_of_data_bytes) == 0x01)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT);
      return (-1);
    }

  if ((current_area_offset + 1 + (*number_of_data_bytes)) > areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT);
      return (-1);
    }

  if (field)
    {
      memset (field->type_length_field,
              '\0',
              IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX);
      memcpy (field->type_length_field,
              &areabufptr[current_area_offset],
              1 + (*number_of_data_bytes));
      field->type_length_field_length = 1 + (*number_of_data_bytes);
    }
      
  return (0);
}
                    
int
ipmi_fru_chassis_info_area (ipmi_fru_ctx_t ctx,
			    const void *areabuf,
			    unsigned int areabuflen,
			    uint8_t *chassis_type,
			    ipmi_fru_field_t *chassis_part_number,
			    ipmi_fru_field_t *chassis_serial_number,
			    ipmi_fru_field_t *chassis_custom_fields,
			    unsigned int chassis_custom_fields_len)
{
  const uint8_t *areabufptr = areabuf;
  unsigned int area_offset = 0;
  unsigned int custom_fields_index = 0;
  uint8_t number_of_data_bytes;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if (chassis_part_number)
    memset (chassis_part_number,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (chassis_serial_number)
    memset (chassis_serial_number,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (chassis_custom_fields && chassis_custom_fields_len)
    memset (chassis_custom_fields,
            '\0',
            sizeof (ipmi_fru_field_t) * chassis_custom_fields_len);

  if (chassis_type)
    (*chassis_type) = areabufptr[area_offset];
  area_offset++;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          chassis_part_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          chassis_serial_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  while (area_offset < areabuflen
         && areabufptr[area_offset] != IPMI_FRU_SENTINEL_VALUE)
    {
      ipmi_fru_field_t *field_ptr = NULL;

      if (chassis_custom_fields && chassis_custom_fields_len)
        {
          if (custom_fields_index < chassis_custom_fields_len)
            field_ptr = &chassis_custom_fields[custom_fields_index];
          else
            {
              FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_OVERFLOW);
              goto cleanup;
            }
        }

      if (_parse_type_length (ctx,
                              areabufptr,
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
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_SENTINEL_VALUE_NOT_FOUND);
      goto cleanup;
    }
#endif

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_board_info_area (ipmi_fru_ctx_t ctx,
			  const void *areabuf,
			  unsigned int areabuflen,
			  uint8_t *language_code,
			  uint32_t *mfg_date_time,
			  ipmi_fru_field_t *board_manufacturer,
			  ipmi_fru_field_t *board_product_name,
			  ipmi_fru_field_t *board_serial_number,
			  ipmi_fru_field_t *board_part_number,
			  ipmi_fru_field_t *board_fru_file_id,
			  ipmi_fru_field_t *board_custom_fields,
			  unsigned int board_custom_fields_len)
{
  const uint8_t *areabufptr = areabuf;
  uint32_t mfg_date_time_tmp = 0;
  unsigned int area_offset = 0;
  unsigned int custom_fields_index = 0;
  uint8_t number_of_data_bytes;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if (board_manufacturer)
    memset (board_manufacturer,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (board_product_name)
    memset (board_product_name,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (board_serial_number)
    memset (board_serial_number,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (board_part_number)
    memset (board_part_number,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (board_fru_file_id)
    memset (board_fru_file_id,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (board_custom_fields && board_custom_fields_len)
    memset (board_custom_fields,
            '\0',
            sizeof (ipmi_fru_field_t) * board_custom_fields_len);

  if (language_code)
    (*language_code) = areabufptr[area_offset];
  area_offset++;

  if (mfg_date_time)
    {
      struct tm tm;
      time_t t;

      /* mfg_date_time is little endian - see spec */
      mfg_date_time_tmp |= areabufptr[area_offset];
      area_offset++;
      mfg_date_time_tmp |= (areabufptr[area_offset] << 8);
      area_offset++;
      mfg_date_time_tmp |= (areabufptr[area_offset] << 16);
      area_offset++;
      
      /* mfg_date_time is in minutes, so multiple by 60 to get seconds */
      mfg_date_time_tmp *= 60;

      /* Posix says individual calls need not clear/set all portions of
       * 'struct tm', thus passing 'struct tm' between functions could
       * have issues.  So we need to memset.
       */
      memset (&tm, '\0', sizeof(struct tm));

      /* In FRU, epoch is 0:00 hrs 1/1/96
       *
       * So convert into ansi epoch
       */

      tm.tm_year = 96;          /* years since 1900 */
      tm.tm_mon = 0;            /* months since January */
      tm.tm_mday = 1;           /* 1-31 */
      tm.tm_hour = 0;
      tm.tm_min = 0;
      tm.tm_sec = 0;
      tm.tm_isdst = -1;

      if ((t = mktime (&tm)) == (time_t)-1)
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_SYSTEM_ERROR);
          goto cleanup;
        }

      mfg_date_time_tmp += (uint32_t)t;
      (*mfg_date_time) = mfg_date_time_tmp;
    }
  else
    area_offset += 3;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_manufacturer) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_product_name) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_serial_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_part_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          board_fru_file_id) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  while (area_offset < areabuflen
         && areabufptr[area_offset] != IPMI_FRU_SENTINEL_VALUE)
    {
      ipmi_fru_field_t *field_ptr = NULL;

      if (board_custom_fields && board_custom_fields_len)
        {
          if (custom_fields_index < board_custom_fields_len)
            field_ptr = &board_custom_fields[custom_fields_index];
          else
            {
              FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_OVERFLOW);
              goto cleanup;
            }
        }

      if (_parse_type_length (ctx,
                              areabufptr,
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
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_SENTINEL_VALUE_NOT_FOUND);
      goto cleanup;
    }
#endif

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_product_info_area (ipmi_fru_ctx_t ctx,
			    const void *areabuf,
			    unsigned int areabuflen,
			    uint8_t *language_code,
			    ipmi_fru_field_t *product_manufacturer_name,
			    ipmi_fru_field_t *product_name,
			    ipmi_fru_field_t *product_part_model_number,
			    ipmi_fru_field_t *product_version,
			    ipmi_fru_field_t *product_serial_number,
			    ipmi_fru_field_t *product_asset_tag,
			    ipmi_fru_field_t *product_fru_file_id,
			    ipmi_fru_field_t *product_custom_fields,
			    unsigned int product_custom_fields_len)
{
  const uint8_t *areabufptr = areabuf;
  unsigned int area_offset = 0;
  unsigned int custom_fields_index = 0;
  uint8_t number_of_data_bytes;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if (product_manufacturer_name)
    memset (product_manufacturer_name,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (product_name)
    memset (product_name,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (product_part_model_number)
    memset (product_part_model_number,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (product_version)
    memset (product_version,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (product_serial_number)
    memset (product_serial_number,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (product_asset_tag)
    memset (product_asset_tag,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (product_fru_file_id)
    memset (product_fru_file_id,
            '\0',
            sizeof (ipmi_fru_field_t));
  if (product_custom_fields && product_custom_fields_len)
    memset (product_custom_fields,
            '\0',
            sizeof (ipmi_fru_field_t) * product_custom_fields_len);

  if (language_code)
    (*language_code) = areabufptr[area_offset];
  area_offset++;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_manufacturer_name) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_name) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_part_model_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_version) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_serial_number) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_asset_tag) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  if (areabufptr[area_offset] == IPMI_FRU_SENTINEL_VALUE)
    goto out;

  if (_parse_type_length (ctx,
                          areabufptr,
                          areabuflen,
                          area_offset,
                          &number_of_data_bytes,
                          product_fru_file_id) < 0)
    goto cleanup;
  area_offset += 1;          /* type/length byte */
  area_offset += number_of_data_bytes;

  while (area_offset < areabuflen
         && areabufptr[area_offset] != IPMI_FRU_SENTINEL_VALUE)
    {
      ipmi_fru_field_t *field_ptr = NULL;

      if (product_custom_fields && product_custom_fields_len)
        {
          if (custom_fields_index < product_custom_fields_len)
            field_ptr = &product_custom_fields[custom_fields_index];
          else
            {
              FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_OVERFLOW);
              goto cleanup;
            }
        }

      if (_parse_type_length (ctx,
                              areabufptr,
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
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_SENTINEL_VALUE_NOT_FOUND);
      goto cleanup;
    }
#endif

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_multirecord_power_supply_information (ipmi_fru_ctx_t ctx,
					       const void *areabuf,
					       unsigned int areabuflen,
					       unsigned int *overall_capacity,
					       unsigned int *peak_va,
					       unsigned int *inrush_current,
					       unsigned int *inrush_interval,
					       int *low_end_input_voltage_range_1,
					       int *high_end_input_voltage_range_1,
					       int *low_end_input_voltage_range_2,
					       int *high_end_input_voltage_range_2,
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
  fiid_obj_t obj_record = NULL;
  int tmpl_record_length;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((tmpl_record_length = fiid_template_len_bytes (tmpl_fru_power_supply_information)) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (tmpl_record_length != areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_power_supply_information)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }

  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU Power Supply Information") < 0)
    goto cleanup;

  if (overall_capacity)
    {
      if (FIID_OBJ_GET (obj_record,
                        "overall_capacity",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*overall_capacity) = val;
    }
  if (peak_va)
    {
      if (FIID_OBJ_GET (obj_record,
                        "peak_va",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*peak_va) = val;
    }
  if (inrush_current)
    {
      if (FIID_OBJ_GET (obj_record,
                        "inrush_current",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*inrush_current) = val;
    }
  if (inrush_interval)
    {
      if (FIID_OBJ_GET (obj_record,
                        "inrush_interval",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*inrush_interval) = val;
    }
  if (low_end_input_voltage_range_1)
    {
      if (FIID_OBJ_GET (obj_record,
                        "low_end_input_voltage_range_1",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*low_end_input_voltage_range_1) = ((int16_t)val * 10);
    }
  if (high_end_input_voltage_range_1)
    {
      if (FIID_OBJ_GET (obj_record,
                        "high_end_input_voltage_range_1",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*high_end_input_voltage_range_1) = ((int16_t)val * 10);
    }
  if (low_end_input_voltage_range_2)
    {
      if (FIID_OBJ_GET (obj_record,
                        "low_end_input_voltage_range_2",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*low_end_input_voltage_range_2) = ((int16_t)val * 10);
    }
  if (high_end_input_voltage_range_2)
    {
      if (FIID_OBJ_GET (obj_record,
                        "high_end_input_voltage_range_2",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*high_end_input_voltage_range_2) = ((int16_t)val * 10);
    }
  if (low_end_input_frequency_range)
    {
      if (FIID_OBJ_GET (obj_record,
                        "low_end_input_frequency_range",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*low_end_input_frequency_range) = val;
    }
  if (high_end_input_frequency_range)
    {
      if (FIID_OBJ_GET (obj_record,
                        "high_end_input_frequency_range",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*high_end_input_frequency_range) = val;
    }
  if (ac_dropout_tolerance)
    {
      if (FIID_OBJ_GET (obj_record,
                        "ac_dropout_tolerance",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*ac_dropout_tolerance) = val;
    }
  if (predictive_fail_support)
    {
      if (FIID_OBJ_GET (obj_record,
                        "predictive_fail_support",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*predictive_fail_support) = val;
    }
  if (power_factor_correction)
    {
      if (FIID_OBJ_GET (obj_record,
                        "power_factor_correction",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*power_factor_correction) = val;
    }
  if (autoswitch)
    {
      if (FIID_OBJ_GET (obj_record,
                        "autoswitch",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*autoswitch) = val;
    }
  if (hot_swap_support)
    {
      if (FIID_OBJ_GET (obj_record,
                        "hot_swap_support",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*hot_swap_support) = val;
    }
  if (tachometer_pulses_per_rotation_predictive_fail_polarity)
    {
      if (FIID_OBJ_GET (obj_record,
                        "tachometer_pulses_per_rotation_predictive_fail_polarity",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*tachometer_pulses_per_rotation_predictive_fail_polarity) = val;
    }
  if (peak_capacity)
    {
      if (FIID_OBJ_GET (obj_record,
                        "peak_capacity",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*peak_capacity) = val;
    }
  if (hold_up_time)
    {
      if (FIID_OBJ_GET (obj_record,
                        "hold_up_time",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*hold_up_time) = val;
    }
  if (voltage_1)
    {
      if (FIID_OBJ_GET (obj_record,
                        "voltage_1",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*voltage_1) = val;
    }
  if (voltage_2)
    {
      if (FIID_OBJ_GET (obj_record,
                        "voltage_2",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*voltage_2) = val;
    }
  if (total_combined_wattage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "total_combined_wattage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*total_combined_wattage) = val;
    }
  if (predictive_fail_tachometer_lower_threshold)
    {
      if (FIID_OBJ_GET (obj_record,
                        "predictive_fail_tachometer_lower_threshold",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*predictive_fail_tachometer_lower_threshold) = val;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_multirecord_dc_output (ipmi_fru_ctx_t ctx,
				const void *areabuf,
				unsigned int areabuflen,
				unsigned int *output_number,
				unsigned int *standby,
				int *nominal_voltage,
				int *maximum_negative_voltage_deviation,
				int *maximum_positive_voltage_deviation,
				unsigned int *ripple_and_noise_pk_pk,
				unsigned int *minimum_current_draw,
				unsigned int *maximum_current_draw)
{
  fiid_obj_t obj_record = NULL;
  int tmpl_record_length;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((tmpl_record_length = fiid_template_len_bytes (tmpl_fru_dc_output)) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (tmpl_record_length != areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_dc_output)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }

  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU DC Output") < 0)
    goto cleanup;

  if (output_number)
    {
      if (FIID_OBJ_GET (obj_record,
                        "output_number",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*output_number) = val;
    }
  if (standby)
    {
      if (FIID_OBJ_GET (obj_record,
                        "standby",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*standby) = val;
    }
  if (nominal_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "nominal_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*nominal_voltage) = ((int16_t)val * 10);
    }
  if (maximum_negative_voltage_deviation)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_negative_voltage_deviation",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*maximum_negative_voltage_deviation) = ((int16_t)val * 10);
    }
  if (maximum_positive_voltage_deviation)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_positive_voltage_deviation",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*maximum_positive_voltage_deviation) = ((int16_t)val * 10);
    }
  if (ripple_and_noise_pk_pk)
    {
      if (FIID_OBJ_GET (obj_record,
                        "ripple_and_noise_pk_pk",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*ripple_and_noise_pk_pk) = val;
    }
  if (minimum_current_draw)
    {
      if (FIID_OBJ_GET (obj_record,
                        "minimum_current_draw",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*minimum_current_draw) = val;
    }
  if (maximum_current_draw)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_current_draw",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*maximum_current_draw) = val;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_multirecord_dc_load (ipmi_fru_ctx_t ctx,
			      const void *areabuf,
			      unsigned int areabuflen,
			      unsigned int *output_number,
			      unsigned int *standby,
			      int *nominal_voltage,
			      int *specd_minimum_voltage,
			      int *specd_maximum_voltage,
			      unsigned int *specd_ripple_and_noise_pk_pk,
			      unsigned int *minimum_current_load,
			      unsigned int *maximum_current_load)
{
  fiid_obj_t obj_record = NULL;
  int tmpl_record_length;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((tmpl_record_length = fiid_template_len_bytes (tmpl_fru_dc_load)) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (tmpl_record_length != areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_dc_load)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }

  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU DC Load") < 0)
    goto cleanup;

  if (output_number)
    {
      if (FIID_OBJ_GET (obj_record,
                        "output_number",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*output_number) = val;
    }
  if (standby)
    {
      if (FIID_OBJ_GET (obj_record,
                        "standby",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*standby) = val;
    }
  if (nominal_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "nominal_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*nominal_voltage) = ((int16_t)val * 10);
    }
  if (specd_minimum_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "specd_minimum_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*specd_minimum_voltage) = ((int16_t)val * 10);
    }
  if (specd_maximum_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "specd_maximum_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*specd_maximum_voltage) = ((int16_t)val * 10);
    }
  if (specd_ripple_and_noise_pk_pk)
    {
      if (FIID_OBJ_GET (obj_record,
                        "specd_ripple_and_noise_pk_pk",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*specd_ripple_and_noise_pk_pk) = val;
    }
  if (minimum_current_load)
    {
      if (FIID_OBJ_GET (obj_record,
                        "minimum_current_load",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*minimum_current_load) = val;
    }
  if (maximum_current_load)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_current_load",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*maximum_current_load) = val;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_multirecord_management_access_record (ipmi_fru_ctx_t ctx,
					       const void *areabuf,
					       unsigned int areabuflen,
					       uint8_t *sub_record_type,
					       void *sub_record_data,
					       unsigned int *sub_record_data_len)
{
  fiid_obj_t obj_record = NULL;
  int min_tmpl_record_length;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((min_tmpl_record_length = fiid_template_field_start_bytes (tmpl_fru_management_access_record, "record")) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (areabuflen < min_tmpl_record_length)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_management_access_record)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }
  
  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU Management Access Record") < 0)
    goto cleanup;

  if (sub_record_type)
    {
      if (FIID_OBJ_GET (obj_record,
                        "sub_record_type",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*sub_record_type) = val;
    }

  if (sub_record_data && sub_record_data_len && (*sub_record_data_len))
    {
      int len;
      
      if ((len = fiid_obj_get_data (obj_record,
                                    "record",
                                    sub_record_data,
                                    (*sub_record_data_len))) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }

      (*sub_record_data_len) = len;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_multirecord_base_compatibility_record (ipmi_fru_ctx_t ctx,
						const void *areabuf,
						unsigned int areabuflen,
						uint32_t *manufacturer_id,
						unsigned int *entity_id_code,
						unsigned int *compatibility_base,
						unsigned int *compatibility_code_start_value,
						uint8_t *code_range_mask,
						unsigned int *code_range_mask_len)
{
  fiid_obj_t obj_record = NULL;
  int min_tmpl_record_length;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((min_tmpl_record_length = fiid_template_field_start_bytes (tmpl_fru_base_compatibility_record, "code_range_mask")) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (areabuflen < min_tmpl_record_length)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_base_compatibility_record)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }
  
  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU Base Compatibility Record") < 0)
    goto cleanup;

  if (manufacturer_id)
    {
      if (FIID_OBJ_GET (obj_record,
                        "manufacturer_id",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*manufacturer_id) = val;
    }

  if (entity_id_code)
    {
      if (FIID_OBJ_GET (obj_record,
                        "entity_id_code",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*entity_id_code) = val;
    }

  if (compatibility_base)
    {
      if (FIID_OBJ_GET (obj_record,
                        "compatibility_base",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*compatibility_base) = val;
    }

  if (compatibility_code_start_value)
    {
      if (FIID_OBJ_GET (obj_record,
                        "compatibility_code_start_value",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*compatibility_code_start_value) = val;
    }

  if (code_range_mask && code_range_mask_len && (*code_range_mask_len))
    {
      int len;
      
      if ((len = fiid_obj_get_data (obj_record,
                                    "code_range_mask",
                                    code_range_mask,
                                    (*code_range_mask_len))) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }

      (*code_range_mask_len) = len;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_multirecord_extended_compatibility_record (ipmi_fru_ctx_t ctx,
						    const void *areabuf,
						    unsigned int areabuflen,
						    uint32_t *manufacturer_id,
						    unsigned int *entity_id_code,
						    unsigned int *compatibility_base,
						    unsigned int *compatibility_code_start_value,
						    uint8_t *code_range_mask,
						    unsigned int *code_range_mask_len)
{
  fiid_obj_t obj_record = NULL;
  int min_tmpl_record_length;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((min_tmpl_record_length = fiid_template_field_start_bytes (tmpl_fru_extended_compatibility_record, "code_range_mask")) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (areabuflen < min_tmpl_record_length)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_extended_compatibility_record)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }
  
  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU Extended Compatibility Record") < 0)
    goto cleanup;

  if (manufacturer_id)
    {
      if (FIID_OBJ_GET (obj_record,
                        "manufacturer_id",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*manufacturer_id) = val;
    }

  if (entity_id_code)
    {
      if (FIID_OBJ_GET (obj_record,
                        "entity_id_code",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*entity_id_code) = val;
    }

  if (compatibility_base)
    {
      if (FIID_OBJ_GET (obj_record,
                        "compatibility_base",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*compatibility_base) = val;
    }

  if (compatibility_code_start_value)
    {
      if (FIID_OBJ_GET (obj_record,
                        "compatibility_code_start_value",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*compatibility_code_start_value) = val;
    }

  if (code_range_mask && code_range_mask_len && (*code_range_mask_len))
    {
      int len;
      
      if ((len = fiid_obj_get_data (obj_record,
                                    "code_range_mask",
                                    code_range_mask,
                                    (*code_range_mask_len))) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }

      (*code_range_mask_len) = len;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_multirecord_extended_dc_output (ipmi_fru_ctx_t ctx,
					 const void *areabuf,
					 unsigned int areabuflen,
					 unsigned int *output_number,
					 unsigned int *current_units,
					 unsigned int *standby,
					 int *nominal_voltage,
					 int *maximum_negative_voltage_deviation,
					 int *maximum_positive_voltage_deviation,
					 unsigned int *ripple_and_noise_pk_pk,
					 unsigned int *minimum_current_draw,
					 unsigned int *maximum_current_draw)
{
  fiid_obj_t obj_record = NULL;
  int tmpl_record_length;
  uint64_t local_current_units = 0;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((tmpl_record_length = fiid_template_len_bytes (tmpl_fru_extended_dc_output)) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (tmpl_record_length != areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_extended_dc_output)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }

  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU Extended DC Output") < 0)
    goto cleanup;

  if (output_number)
    {
      if (FIID_OBJ_GET (obj_record,
                        "output_number",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*output_number) = val;
    }
  if (current_units || minimum_current_draw || maximum_current_draw)
    {
      if (FIID_OBJ_GET (obj_record,
                        "current_units",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      if (current_units)
	(*current_units) = val;
      local_current_units = val;
    }
  if (standby)
    {
      if (FIID_OBJ_GET (obj_record,
                        "standby",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*standby) = val;
    }
  if (nominal_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "nominal_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*nominal_voltage) = ((int16_t)val * 10);
    }
  if (maximum_negative_voltage_deviation)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_negative_voltage_deviation",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*maximum_negative_voltage_deviation) = ((int16_t)val * 10);
    }
  if (maximum_positive_voltage_deviation)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_positive_voltage_deviation",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*maximum_positive_voltage_deviation) = ((int16_t)val * 10);
    }
  if (ripple_and_noise_pk_pk)
    {
      if (FIID_OBJ_GET (obj_record,
                        "ripple_and_noise_pk_pk",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*ripple_and_noise_pk_pk) = val;
    }
  if (minimum_current_draw)
    {
      if (FIID_OBJ_GET (obj_record,
                        "minimum_current_draw",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      if (local_current_units == IPMI_FRU_CURRENT_UNITS_10MA)
	(*minimum_current_draw) = (val * 10);
      else
	(*minimum_current_draw) = (val * 100);
    }
  if (maximum_current_draw)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_current_draw",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      if (local_current_units == IPMI_FRU_CURRENT_UNITS_10MA)
	(*maximum_current_draw) = (val * 10);
      else
	(*maximum_current_draw) = (val * 100);
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_multirecord_extended_dc_load (ipmi_fru_ctx_t ctx,
				       const void *areabuf,
				       unsigned int areabuflen,
				       unsigned int *output_number,
				       unsigned int *current_units,
				       unsigned int *standby,
				       int *nominal_voltage,
				       int *specd_minimum_voltage,
				       int *specd_maximum_voltage,
				       unsigned int *specd_ripple_and_noise_pk_pk,
				       unsigned int *minimum_current_load,
				       unsigned int *maximum_current_load)
{
  fiid_obj_t obj_record = NULL;
  int tmpl_record_length;
  uint64_t local_current_units = 0;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((tmpl_record_length = fiid_template_len_bytes (tmpl_fru_extended_dc_load)) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (tmpl_record_length != areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_extended_dc_load)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }

  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU Extended DC Load") < 0)
    goto cleanup;

  if (output_number)
    {
      if (FIID_OBJ_GET (obj_record,
                        "output_number",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*output_number) = val;
    }
  if (current_units || minimum_current_load || maximum_current_load)
    {
      if (FIID_OBJ_GET (obj_record,
                        "current_units",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      if (current_units)
	(*current_units) = val;
      local_current_units = val;
    }
  if (standby)
    {
      if (FIID_OBJ_GET (obj_record,
                        "standby",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*standby) = val;
    }
  if (nominal_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "nominal_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*nominal_voltage) = ((int16_t)val * 10);
    }
  if (specd_minimum_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "specd_minimum_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*specd_minimum_voltage) = ((int16_t)val * 10);
    }
  if (specd_maximum_voltage)
    {
      if (FIID_OBJ_GET (obj_record,
                        "specd_maximum_voltage",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*specd_maximum_voltage) = ((int16_t)val * 10);
    }
  if (specd_ripple_and_noise_pk_pk)
    {
      if (FIID_OBJ_GET (obj_record,
                        "specd_ripple_and_noise_pk_pk",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*specd_ripple_and_noise_pk_pk) = val;
    }
  if (minimum_current_load)
    {
      if (FIID_OBJ_GET (obj_record,
                        "minimum_current_load",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      if (local_current_units == IPMI_FRU_CURRENT_UNITS_10MA)
	(*minimum_current_load) = (val * 10);
      else
	(*minimum_current_load) = (val * 100);
    }
  if (maximum_current_load)
    {
      if (FIID_OBJ_GET (obj_record,
                        "maximum_current_load",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      if (local_current_units == IPMI_FRU_CURRENT_UNITS_10MA)
	(*maximum_current_load) = (val * 10);
      else
	(*maximum_current_load) = (val * 100);
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}
int
ipmi_fru_multirecord_oem_record (ipmi_fru_ctx_t ctx,
				 const void *areabuf,
				 unsigned int areabuflen,
				 uint32_t *manufacturer_id,
				 void *oem_data,
				 unsigned int *oem_data_len)
{
  fiid_obj_t obj_record = NULL;
  int min_tmpl_record_length;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  if (!areabuf || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((min_tmpl_record_length = fiid_template_field_start_bytes (tmpl_fru_oem_record, "oem_data")) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (areabuflen < min_tmpl_record_length)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!(obj_record = fiid_obj_create (tmpl_fru_oem_record)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
                        areabuf,
                        areabuflen) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
      goto cleanup;
    }
  
  if (fru_dump_obj (ctx,
		    obj_record,
		    "FRU OEM Record") < 0)
    goto cleanup;

  if (manufacturer_id)
    {
      if (FIID_OBJ_GET (obj_record,
                        "manufacturer_id",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }
      (*manufacturer_id) = val;
    }

  if (oem_data && oem_data_len && (*oem_data_len))
    {
      int len;
      
      if ((len = fiid_obj_get_data (obj_record,
                                    "oem_data",
                                    oem_data,
                                    (*oem_data_len))) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, obj_record);
          goto cleanup;
        }

      (*oem_data_len) = len;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}
