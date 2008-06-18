/*****************************************************************************\
 *  $Id: ipmi-fru-util.c,v 1.20 2008-06-18 20:50:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "ipmi-fru.h"
#include "ipmi-fru-util.h"

#include "freeipmi-portability.h"
#include "tool-fiid-wrappers.h"

#define FRU_COUNT_TO_READ_BLOCK_SIZE  16

fru_err_t
ipmi_fru_read_fru_data (ipmi_fru_state_data_t *state_data,
                        uint8_t device_id,
                        uint8_t *frubuf,
                        unsigned int frubuflen,
                        unsigned int offset_in_bytes,
                        unsigned int fru_read_bytes)
{
  fiid_obj_t fru_read_data_rs = NULL;
  uint32_t num_bytes_read = 0;
  int32_t len = 0;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;

  assert(state_data);
  assert(frubuf);
  assert(frubuflen);
  assert(fru_read_bytes <= frubuflen);

  _FIID_OBJ_CREATE(fru_read_data_rs, tmpl_cmd_read_fru_data_rs);

  if ((offset_in_bytes + fru_read_bytes) > state_data->fru_inventory_area_size)
    {
      pstdout_printf(state_data->pstate, 
                     "invalid read range: %u %u\n", 
                     offset_in_bytes,
                     fru_read_bytes);
      goto cleanup;
    }

  while (num_bytes_read < fru_read_bytes)
    {
      uint8_t buf[FRU_BUF_LEN+1];
      uint8_t count_to_read;
      uint64_t count_returned;

      memset(buf, '\0', FRU_BUF_LEN+1);

      _FIID_OBJ_CLEAR(fru_read_data_rs);

      if ((fru_read_bytes - num_bytes_read) < FRU_COUNT_TO_READ_BLOCK_SIZE)
        count_to_read = fru_read_bytes - num_bytes_read;
      else
        count_to_read = FRU_COUNT_TO_READ_BLOCK_SIZE;
      
      /* XXX: achu: Implement retry mechanism? - see spec on
       * completion code 0x81 
       */
      if (ipmi_cmd_read_fru_data (state_data->ipmi_ctx,
                                  device_id,
                                  offset_in_bytes + num_bytes_read,
                                  count_to_read,
                                  fru_read_data_rs) < 0)
        {
          if (state_data->prog_data->args->verbose_count)
            pstdout_fprintf(state_data->pstate, 
                            stderr,
                            "  FRU Read FRU Failure: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
        
      _FIID_OBJ_GET (fru_read_data_rs, 
                     "count_returned",
                     &count_returned);

      if (!count_returned)
        {
          if (state_data->prog_data->args->verbose_count)
            pstdout_fprintf(state_data->pstate, 
                            stderr,
                            "  FRU Read FRU: No Data Returned\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      _FIID_OBJ_GET_DATA_LEN(len,
                             fru_read_data_rs,
                             "requested_data",
                             buf,
                             FRU_BUF_LEN);
      
      if (count_returned != len)
        {
          if (state_data->prog_data->args->verbose_count)
            pstdout_fprintf(state_data->pstate, 
                            stderr,
                            "  FRU Read FRU: Invalid Count Returned\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      memcpy(frubuf + num_bytes_read,
             buf,
             count_returned);
      num_bytes_read += count_returned;
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(fru_read_data_rs);
  return rv;
}

static fru_err_t
_get_type_length_bytes(ipmi_fru_state_data_t *state_data,
                       uint8_t *frubuf,
                       unsigned int frubuflen,
                       uint8_t type_length,
                       unsigned int offset_to_bytes,
                       unsigned int *len_parsed,
                       uint8_t type_code,
                       uint8_t *typebuf,
                       unsigned int typebuflen)
      
{
  uint8_t number_of_data_bytes;
  unsigned int bytes_parsed = 0;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;

  assert(state_data);
  assert(frubuf);
  assert(frubuflen);
  assert(offset_to_bytes);
  assert(len_parsed);
  assert(typebuf);
  assert(typebuflen);

  number_of_data_bytes = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

  if (!number_of_data_bytes)
    goto out;

  /* Special Case: This shouldn't be a length of 0x01 (see type/length
   * byte format in FRU Information Storage Definition).  I don't know
   * what to do.  I guess we'll just copy data until we hit the
   * sentinel value and pray for the best.
   */
  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_LANGUAGE_CODE
      && number_of_data_bytes == 0x01)
    {
      while (bytes_parsed < typebuflen
             && (offset_to_bytes + bytes_parsed) < frubuflen
             && typebuf[offset_to_bytes] != IPMI_FRU_SENTINEL_VALUE)
        {
          typebuf[bytes_parsed] = frubuf[offset_to_bytes + bytes_parsed];
          bytes_parsed++;
        }

      if (bytes_parsed >= typebuflen)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "  FRU Size too small\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if ((offset_to_bytes + bytes_parsed) >= frubuflen)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "  FRU Missing Sentinel Value\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
    }
  else
    {
      if (frubuflen < (offset_to_bytes + number_of_data_bytes))
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "  FRU Size too small\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (typebuflen < number_of_data_bytes)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "short typebuflen: %u\n",
                          typebuflen);
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      memcpy(typebuf, &frubuf[offset_to_bytes], number_of_data_bytes);
      bytes_parsed = number_of_data_bytes;
    }

 out:
  *len_parsed = bytes_parsed;
  rv = FRU_ERR_SUCCESS;
 cleanup:
  return rv;
}

static fru_err_t
_sixbitascii_to_ascii(ipmi_fru_state_data_t *state_data,
                      uint8_t *typebuf,
                      unsigned int typebuf_bytes,
                      uint8_t *typestr,
                      unsigned int typestrlen)
{
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  int i;
  uint32_t c = 0;

  assert(state_data);
  assert(typebuf);
  assert(typebuf_bytes);
  assert(typestr);
  assert(typestrlen);
   
  /* six bit ascii packs 4 chars in 3 bytes - see FRU Information Storage Definition */
  if (typestrlen < ((typebuf_bytes/3 + 1))*4)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "short typestrlen: %u\n",
                      typestrlen);
      goto cleanup;
    }

  /* six bit ascii "begins" at ' '. see FRU Information Storage Definition */
  for (i = 0; i < typebuf_bytes; i+=3)
    {
      typestr[c] = (typebuf[i] & 0x3F) + ' ';
      c++;
      if (typebuf_bytes > (i+1))
        {
          typestr[c] = (((typebuf[i+1] & 0x0F) << 2) | ((typebuf[i] & 0xC0) >> 6)) + ' ';
          c++;
        }
      if (typebuf_bytes > (i+2))
        {
          typestr[c] = (((typebuf[i+1] & 0xF0) >> 4) | ((typebuf[i+2] & 0x03) << 4)) + ' ';
          typestr[c+1] = ((typebuf[i+2] & 0xFC) >> 2) + ' ';
          c+=2;
        }
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  return rv;
}

static fru_err_t
_bcd_to_ascii(ipmi_fru_state_data_t *state_data,
              uint8_t *typebuf,
              unsigned int typebuf_bytes,
              uint8_t *typestr,
              unsigned int typestrlen)
{
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  int i;

  assert(state_data);
  assert(typebuf);
  assert(typebuf_bytes);
  assert(typestr);
  assert(typestrlen);

  if (typestrlen < typebuf_bytes)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "short typestrlen: %u\n",
                      typestrlen);
      goto cleanup;
    }

  for (i = 0; i < typebuf_bytes; i++)
    {
      /* +1/-1 hacker to get around warnings */
      if ((typebuf[i] + 1) > IPMI_FRU_BCD_DIGIT_MIN 
          && (typebuf[i] - 1) < IPMI_FRU_BCD_DIGIT_MAX)
        typestr[i] = '0' + typebuf[i];
      else if (typebuf[i] == IPMI_FRU_BCD_SPACE)
        typestr[i] = ' ';
      else if (typebuf[i] == IPMI_FRU_BCD_DASH)
        typestr[i] = '-';
      else if (typebuf[i] == IPMI_FRU_BCD_PERIOD)
        typestr[i] = '.';
      else
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "  FRU Unknown BCD Character: 0x%02X\n",
                          typebuf[i]);
          goto cleanup;
        }
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  return rv;
}

fru_err_t
ipmi_fru_output_type_length_field(ipmi_fru_state_data_t *state_data,
                                  uint8_t *frubuf,
                                  unsigned int frubuflen,
                                  unsigned int offset_in_bytes,
                                  uint8_t *language_code,
                                  unsigned int *len_parsed,
                                  char *str)
{
  uint8_t type_length;
  uint8_t typebuf[FRU_BUF_LEN+1];
  uint8_t typestr[FRU_BUF_LEN+1];
  uint8_t type_code;
  uint8_t number_of_data_bytes;
  unsigned int bytes_parsed = 0;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  int i;

  assert(state_data);
  assert(frubuf);
  assert(frubuflen);
  assert(offset_in_bytes);
  assert(len_parsed);
  assert(str);

  memset(typebuf, '\0', FRU_BUF_LEN+1);
  memset(typestr, '\0', FRU_BUF_LEN+1);

  type_length = frubuf[offset_in_bytes];

  if (state_data->prog_data->args->verbose_count >= 2)
    {
      pstdout_printf(state_data->pstate, 
                     "  FRU %s Type/Length: 0x%02X\n",
                     str,
                     type_length);
    }

  type_code = type_length & IPMI_FRU_TYPE_LENGTH_TYPE_CODE_MASK;
  number_of_data_bytes = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

  if (!number_of_data_bytes)
    goto out;

  if ((ret = _get_type_length_bytes(state_data,
                                    frubuf,
                                    frubuflen,
                                    type_length,
                                    offset_in_bytes + 1,
                                    &bytes_parsed,
                                    type_code,
                                    typebuf,
                                    FRU_BUF_LEN)) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BINARY)
    {
      /* Ummm - it's binary or unspecified.  I guess we'll output hex */

      pstdout_printf(state_data->pstate, 
                     "  FRU %s:",
                     str);

      for (i = 0; i < bytes_parsed; i++)
        {
          if (i && (i % 8) == 0)
            pstdout_printf(state_data->pstate, "\n  ");

          pstdout_printf(state_data->pstate,
                         " 0x%02X",
                         typebuf[i]);
        }

      pstdout_printf(state_data->pstate, "\n");
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BCD)
    {
      if ((ret = _bcd_to_ascii(state_data,
                               typebuf,
                               bytes_parsed,
                               typestr,
                               FRU_BUF_LEN)) != FRU_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }

      pstdout_printf(state_data->pstate, 
                     "  FRU %s: %s\n",
                     str,
                     typestr);
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SIXBIT_ASCII)
    {
      if ((ret = _sixbitascii_to_ascii(state_data,
                                       typebuf,
                                       bytes_parsed,
                                       typestr,
                                       FRU_BUF_LEN)) != FRU_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }

      pstdout_printf(state_data->pstate, 
                     "  FRU %s: %s\n",
                     str,
                     typestr);
    }
  else
    {
      /* XXX: achu: If there is no language_code (like in the
       * chassis area), we assume its English
       */
      if (language_code
          && *language_code != IPMI_FRU_LANGUAGE_CODE_ENGLISH_LEGACY
          && *language_code != IPMI_FRU_LANGUAGE_CODE_ENGLISH)
        {
          pstdout_printf(state_data->pstate, 
                         "  FRU %s: Unsupported Language Code: 0x%02X\n",
                         str,
                         *language_code);
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      memcpy(typestr, typebuf, bytes_parsed);

      pstdout_printf(state_data->pstate, 
                     "  FRU %s: %s\n",
                     str,
                     typestr);
    }
  
 out:  
  *len_parsed = bytes_parsed + 1;          /* +1 for type/length field */
  rv = FRU_ERR_SUCCESS;
 cleanup:
  return rv;
}

fru_err_t
ipmi_fru_get_info_area_length(ipmi_fru_state_data_t *state_data,
                              uint8_t device_id,
                              unsigned int offset_in_bytes,
                              char *str,
                              uint64_t *info_area_length)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  fiid_obj_t fru_info_area_header = NULL;
  int32_t len;
  int32_t info_area_header_len;
  uint64_t format_version;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;

  assert(state_data);
  assert(offset_in_bytes);
  assert(str);
  assert(info_area_length);
  
  _FIID_TEMPLATE_LEN_BYTES(info_area_header_len, tmpl_fru_info_area_header);
  
  if ((offset_in_bytes + info_area_header_len) > state_data->fru_inventory_area_size)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU %s Info Area size too small\n",
                      str);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((ret = ipmi_fru_read_fru_data (state_data,
                                     device_id,
                                     frubuf,
                                     IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                     offset_in_bytes,
                                     info_area_header_len)) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(fru_info_area_header, tmpl_fru_info_area_header);

  _FIID_OBJ_SET_ALL_LEN(len, 
                        fru_info_area_header, 
                        frubuf,
                        info_area_header_len);

  _FIID_OBJ_GET (fru_info_area_header,
                 "format_version",
                 &format_version);

  _FIID_OBJ_GET (fru_info_area_header, 
                 "info_area_length", 
                 info_area_length);
  
  if (state_data->prog_data->args->verbose_count >= 2)
    {
      pstdout_printf(state_data->pstate, 
                     "  FRU %s Info Area Format Version: 0x%02X\n",
                     str,
                     format_version);
      pstdout_printf(state_data->pstate,
                     "  FRU %s Info Area Length: %u\n", 
                     str,
                     *info_area_length);
    }

  if (format_version != IPMI_FRU_CHASSIS_INFO_AREA_FORMAT_VERSION)
    {
      pstdout_fprintf(state_data->pstate, 
                      stderr,
                      "  FRU %s Area Format Unknown: 0x%02X\n", 
                      str,
                      format_version);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (!(*info_area_length))
    {
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (state_data->fru_inventory_area_size < (offset_in_bytes + (*info_area_length)*8))
    {
      pstdout_fprintf(state_data->pstate, 
                      stderr,
                      "  FRU %s Info Area too small\n",
                      str);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = FRU_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(fru_info_area_header);
  return (rv);
}

fru_err_t
ipmi_fru_dump_hex(ipmi_fru_state_data_t *state_data,
                  uint8_t *frubuf,
                  uint64_t length_in_bytes,
                  char *str)
{
  assert(state_data);
  assert(frubuf);
  assert(length_in_bytes);
  assert(str);

  if (state_data->prog_data->args->common.debug)
    {
      int i;

      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "---------------\n"
                      " %s\n"
                      "---------------\n",
                      str);
      for (i = 0; i < length_in_bytes; i++)
        {
          if (i && i % 8 == 0)
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "\n");

          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "0x%02X ",
                          frubuf[i]);
        }
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "\n");
    }

  return FRU_ERR_SUCCESS;
}

fru_err_t
ipmi_fru_check_checksum(ipmi_fru_state_data_t *state_data,
                        uint8_t *frubuf,
                        uint64_t length_in_bytes,
                        uint8_t checksum_init,
                        char *str)
{
  assert(state_data);
  assert(frubuf);
  assert(length_in_bytes);
  assert(str);

  if (!state_data->prog_data->args->skip_checks)
    {
      uint8_t checksum = checksum_init;
      int i;

      for (i = 0; i < length_in_bytes; i++)
        checksum += frubuf[i];
      
      if (checksum)
        {
          if (state_data->prog_data->args->verbose_count)
            pstdout_fprintf(state_data->pstate, 
                            stderr,
                            "  FRU %s Checksum Invalid: 0x%02X\n", 
                            str,
                            checksum);
          return FRU_ERR_NON_FATAL_ERROR;
        }
    }

  return FRU_ERR_SUCCESS;
}
