/*****************************************************************************\
 *  $Id: ipmi-fru-parse-common.c,v 1.1.2.4 2009-04-10 21:28:02 chu11 Exp $
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

#if HAVE_CONFIG_H
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
#include <errno.h>
#include <assert.h>

#include "freeipmi/fru-parse/ipmi-fru-parse.h"
#include "freeipmi/api/ipmi-fru-inventory-device-cmds-api.h"
#include "freeipmi/cmds/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-fru-information-record-format.h"

#include "ipmi-fru-parse-common.h"
#include "ipmi-fru-parse-defs.h"
#include "ipmi-fru-parse-trace.h"
#include "ipmi-fru-parse-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

#define IPMI_FRU_PARSE_COUNT_TO_READ_BLOCK_SIZE  16

int
ipmi_fru_parse_read_fru_data (ipmi_fru_parse_ctx_t ctx,
                              uint8_t *frubuf,
                              unsigned int frubuflen,
                              unsigned int offset_in_bytes,
                              unsigned int fru_read_bytes)
{
  fiid_obj_t fru_read_data_rs = NULL;
  uint32_t num_bytes_read = 0;
  int32_t len = 0;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (frubuf);
  assert (frubuflen);
  assert (fru_read_bytes <= frubuflen);

  if (!(fru_read_data_rs = fiid_obj_create (tmpl_cmd_read_fru_data_rs)))
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((offset_in_bytes + fru_read_bytes) > ctx->fru_inventory_area_size)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }

  while (num_bytes_read < fru_read_bytes)
    {
      uint8_t buf[IPMI_FRU_PARSE_BUF_LEN+1];
      uint8_t count_to_read;
      uint64_t count_returned;

      memset (buf, '\0', IPMI_FRU_PARSE_BUF_LEN+1);

      if (fiid_obj_clear (fru_read_data_rs) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_read_data_rs);
          goto cleanup;
        }

      if ((fru_read_bytes - num_bytes_read) < IPMI_FRU_PARSE_COUNT_TO_READ_BLOCK_SIZE)
        count_to_read = fru_read_bytes - num_bytes_read;
      else
        count_to_read = IPMI_FRU_PARSE_COUNT_TO_READ_BLOCK_SIZE;

      /* XXX: achu: Implement retry mechanism? - see spec on
       * completion code 0x81
       */
      if (ipmi_cmd_read_fru_data (ctx->ipmi_ctx,
                                  ctx->fru_device_id,
                                  offset_in_bytes + num_bytes_read,
                                  count_to_read,
                                  fru_read_data_rs) < 0)
        {
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_IPMI_ERROR);
          goto cleanup;
        }

      if (FIID_OBJ_GET (fru_read_data_rs,
                        "count_returned",
                        &count_returned) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_read_data_rs);
          goto cleanup;
        }

      if (!count_returned)
        {
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_IPMI_ERROR);
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (fru_read_data_rs,
                                    "requested_data",
                                    buf,
                                    IPMI_FRU_PARSE_BUF_LEN)) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_read_data_rs);
          goto cleanup;
        }

      if (count_returned != len)
        {
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_IPMI_ERROR);
          goto cleanup;
        }

      memcpy (frubuf + num_bytes_read,
              buf,
              count_returned);
      num_bytes_read += count_returned;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (fru_read_data_rs);
  return (rv);
}

#if 0
static fru_err_t
_get_type_length_bytes (ipmi_fru_parse_ctx_t ctx,
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

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (frubuf);
  assert (frubuflen);
  assert (offset_to_bytes);
  assert (len_parsed);
  assert (typebuf);
  assert (typebuflen);

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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "  FRU Size too small\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if ((offset_to_bytes + bytes_parsed) >= frubuflen)
        {
          pstdout_fprintf (state_data->pstate,
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "  FRU Size too small\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (typebuflen < number_of_data_bytes)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "short typebuflen: %u\n",
                           typebuflen);
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      memcpy (typebuf, &frubuf[offset_to_bytes], number_of_data_bytes);
      bytes_parsed = number_of_data_bytes;
    }

 out:
  *len_parsed = bytes_parsed;
  rv = FRU_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static fru_err_t
_sixbitascii_to_ascii (ipmi_fru_parse_ctx_t ctx,
                       uint8_t *typebuf,
                       unsigned int typebuf_bytes,
                       uint8_t *typestr,
                       unsigned int typestrlen)
{
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  int i;
  uint32_t c = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (typebuf);
  assert (typebuf_bytes);
  assert (typestr);
  assert (typestrlen);

  /* six bit ascii packs 4 chars in 3 bytes - see FRU Information Storage Definition */
  if (typestrlen < ((typebuf_bytes/3 + 1))*4)
    {
      pstdout_fprintf (state_data->pstate,
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
  return (rv);
}

static fru_err_t
_bcd_to_ascii (ipmi_fru_parse_ctx_t ctx,
               uint8_t *typebuf,
               unsigned int typebuf_bytes,
               uint8_t *typestr,
               unsigned int typestrlen)
{
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  int i;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (typebuf);
  assert (typebuf_bytes);
  assert (typestr);
  assert (typestrlen);

  if (typestrlen < typebuf_bytes)
    {
      pstdout_fprintf (state_data->pstate,
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
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "  FRU Unknown BCD Character: %02Xh\n",
                           typebuf[i]);
          goto cleanup;
        }
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  return (rv);
}

fru_err_t
ipmi_fru_parse_output_type_length_field (ipmi_fru_parse_ctx_t ctx,
                                         uint8_t *frubuf,
                                         unsigned int frubuflen,
                                         unsigned int offset_in_bytes,
                                         uint8_t *language_code,
                                         unsigned int *len_parsed,
                                         char *str)
{
  uint8_t type_length;
  uint8_t typebuf[IPMI_FRU_PARSE_BUF_LEN+1];
  uint8_t typestr[IPMI_FRU_PARSE_BUF_LEN+1];
  uint8_t type_code;
  uint8_t number_of_data_bytes;
  unsigned int bytes_parsed = 0;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  int i;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (frubuf);
  assert (frubuflen);
  assert (offset_in_bytes);
  assert (len_parsed);
  assert (str);

  memset (typebuf, '\0', IPMI_FRU_PARSE_BUF_LEN+1);
  memset (typestr, '\0', IPMI_FRU_PARSE_BUF_LEN+1);

  type_length = frubuf[offset_in_bytes];

  if (state_data->prog_data->args->verbose_count >= 2)
    {
      pstdout_printf (state_data->pstate,
                      "  FRU %s Type/Length: %02Xh\n",
                      str,
                      type_length);
    }

  type_code = type_length & IPMI_FRU_TYPE_LENGTH_TYPE_CODE_MASK;
  number_of_data_bytes = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

  if (!number_of_data_bytes)
    goto out;

  if ((ret = _get_type_length_bytes (state_data,
                                     frubuf,
                                     frubuflen,
                                     type_length,
                                     offset_in_bytes + 1,
                                     &bytes_parsed,
                                     type_code,
                                     typebuf,
                                     IPMI_FRU_PARSE_BUF_LEN)) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BINARY)
    {
      /* Ummm - it's binary or unspecified.  I guess we'll output hex */

      pstdout_printf (state_data->pstate,
                      "  FRU %s:",
                      str);

      for (i = 0; i < bytes_parsed; i++)
        {
          if (i && (i % 8) == 0)
            pstdout_printf (state_data->pstate, "\n  ");

          pstdout_printf (state_data->pstate,
                          " %02Xh",
                          typebuf[i]);
        }

      pstdout_printf (state_data->pstate, "\n");
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BCD)
    {
      if ((ret = _bcd_to_ascii (state_data,
                                typebuf,
                                bytes_parsed,
                                typestr,
                                IPMI_FRU_PARSE_BUF_LEN)) != FRU_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "  FRU %s: %s\n",
                      str,
                      typestr);
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SIXBIT_ASCII)
    {
      if ((ret = _sixbitascii_to_ascii (state_data,
                                        typebuf,
                                        bytes_parsed,
                                        typestr,
                                        IPMI_FRU_PARSE_BUF_LEN)) != FRU_ERR_SUCCESS)
        {
          rv = ret;
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
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
          pstdout_printf (state_data->pstate,
                          "  FRU %s: Unsupported Language Code: %02Xh\n",
                          str,
                          *language_code);
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      memcpy (typestr, typebuf, bytes_parsed);

      pstdout_printf (state_data->pstate,
                      "  FRU %s: %s\n",
                      str,
                      typestr);
    }

 out:
  *len_parsed = bytes_parsed + 1;          /* +1 for type/length field */
  rv = FRU_ERR_SUCCESS;
 cleanup:
  return (rv);
}
#endif

int
ipmi_fru_parse_get_info_area_length (ipmi_fru_parse_ctx_t ctx,
                                     unsigned int offset_in_bytes,
                                     unsigned int *info_area_length,
                                     uint8_t expected_format_version,
                                     unsigned int err_code_format_invalid,
                                     const char *debug_hdr)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  fiid_obj_t fru_info_area_header = NULL;
  int32_t info_area_header_len;
  uint64_t format_version;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (offset_in_bytes);
  assert (info_area_length);
  assert (err_code_format_invalid != IPMI_FRU_PARSE_ERR_SUCCESS);

  if ((info_area_header_len = fiid_template_len_bytes (tmpl_fru_info_area_header)) < 0)
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((offset_in_bytes + info_area_header_len) > ctx->fru_inventory_area_size)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }

  if (ipmi_fru_parse_read_fru_data (ctx,
                                    frubuf,
                                    IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                    offset_in_bytes,
                                    info_area_header_len) < 0)
    goto cleanup;

  if (!(fru_info_area_header = fiid_obj_create (tmpl_fru_info_area_header)))
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (fru_info_area_header,
                        frubuf,
                        info_area_header_len) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_info_area_header,
                    "format_version",
                    &format_version) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_info_area_header,
                    "info_area_length",
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }
  (*info_area_length) = val;

  if (format_version != expected_format_version)
    {
#if 0
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "  FRU %s Area Format Unknown: %02Xh\n",
                       str,
                       format_version);
#endif
      FRU_PARSE_SET_ERRNUM (ctx, err_code_format_invalid);
      goto cleanup;
    }

  if (!(*info_area_length))
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  if (ctx->fru_inventory_area_size < (offset_in_bytes + (*info_area_length)*8))
    {
#if 0
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "  FRU %s Info Area too small\n",
                       str);
#endif
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (fru_info_area_header);
  return (rv);
}

int
ipmi_fru_parse_dump_hex (ipmi_fru_parse_ctx_t ctx,
                         uint8_t *frubuf,
                         uint64_t length_in_bytes,
                         const char *debug_hdr)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (frubuf);
  assert (length_in_bytes);
  assert (debug_hdr);

  if (ctx->flags & IPMI_FRU_PARSE_FLAGS_DEBUG_DUMP)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                     DEBUG_UTIL_DIRECTION_NONE,
                     debug_hdr,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      ipmi_dump_hex (STDERR_FILENO,
                     ctx->debug_prefix,
                     hdrbuf,
                     NULL,
                     frubuf,
                     length_in_bytes);
    }

  return (0);
}

int
ipmi_fru_parse_check_checksum (ipmi_fru_parse_ctx_t ctx,
                               uint8_t *frubuf,
                               uint64_t length_in_bytes,
                               uint8_t checksum_init)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (frubuf);
  assert (length_in_bytes);

  if (!(ctx->flags & IPMI_FRU_PARSE_FLAGS_SKIP_CHECKSUM_CHECKS))
    {
      uint8_t checksum = checksum_init;
      int i;

      for (i = 0; i < length_in_bytes; i++)
        checksum += frubuf[i];

      if (checksum)
        return (0);
    }

  return (1);
}
