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
                    unsigned int current_offset,
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
  
  type_length = areabuf[current_offset];
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

  if ((current_offset + 1 + (*number_of_data_bytes)) > areabuflen)
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
              areabuf[current_offset],
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
                                  ipmi_fru_parse_field_t *chassis_info_fields,
                                  unsigned int chassis_info_fields_len)
{
  unsigned int area_offset = 0;
  unsigned int info_fields_index = 0;
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
  if (chassis_info_fields && chassis_info_fields_len)
    memset (chassis_info_fields,
            '\0',
            sizeof (ipmi_fru_parse_field_t) * chassis_info_fields_len);

  area_offset = 2; /* 2 = version + length fields */
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

      if (chassis_info_fields && chassis_info_fields_len)
        {
          if (info_fields_index < chassis_info_fields_len)
            field_ptr = &chassis_info_fields[info_fields_index];
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
      info_fields_index++;
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
