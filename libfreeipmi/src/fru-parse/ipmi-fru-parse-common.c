/*****************************************************************************\
 *  $Id: ipmi-fru-parse-common.c,v 1.1.2.6 2009-04-15 17:14:49 chu11 Exp $
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
#include "freeipmi/spec/ipmi-fru-language-codes-spec.h"

#include "ipmi-fru-parse-common.h"
#include "ipmi-fru-parse-defs.h"
#include "ipmi-fru-parse-trace.h"
#include "ipmi-fru-parse-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

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
ipmi_fru_parse_dump_obj (ipmi_fru_parse_ctx_t ctx,
                         fiid_obj_t obj,
                         const char *debug_hdr)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (obj);
  assert (debug_hdr);

  if (ctx->flags & IPMI_FRU_PARSE_FLAGS_DEBUG_DUMP)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                     DEBUG_UTIL_DIRECTION_NONE,
                     debug_hdr,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      ipmi_obj_dump (STDERR_FILENO,
                     ctx->debug_prefix,
                     hdrbuf,
                     NULL,
                     obj);
    }

  return (0);
}

static int
_get_type_length_bytes (ipmi_fru_parse_ctx_t ctx,
                        uint8_t *areabuf,
                        unsigned int areabuflen,
                        unsigned int area_offset_to_bytes,
                        unsigned int *bytes_parsed,
                        uint8_t type_code,
                        uint8_t number_of_data_bytes,
                        uint8_t *typebuf,
                        unsigned int typebuflen)

{
  unsigned int bytes_offset = 0;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (areabuf);
  assert (areabuflen);
  assert (area_offset_to_bytes);
  assert (bytes_parsed);
  assert (typebuf);
  assert (typebuflen);

  if (!number_of_data_bytes)
    goto out;

  /* Special Case: This shouldn't be a length of 0x01 (see type/length
   * byte format in FRU Information Storage Definition).
   */
  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_LANGUAGE_CODE
      && number_of_data_bytes == 0x01)
    {
#if 0
      /* I don't know what to do.  I guess we'll just copy data until
       * we hit the sentinel value and pray for the best.
       */
      while (bytes_offset < typebuflen
             && (area_offset_to_bytes + bytes_offset) < areabuflen
             && typebuf[area_offset_to_bytes] != IPMI_FRU_SENTINEL_VALUE)
        {
          typebuf[bytes_offset] = areabuf[area_offset_to_bytes + bytes_offset];
          bytes_offset++;
        }

      if (bytes_offset >= typebuflen)
        {
          fprintf (stderr, "  FRU Size too small\n");
          goto cleanup;
        }

      if ((area_offset_to_bytes + bytes_offset) >= areabuflen)
        {
          fprintf (stderr, "  FRU Missing Sentinel Value\n");
          goto cleanup;
        }
#endif
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }
  else
    {
      if (areabuflen < (area_offset_to_bytes + number_of_data_bytes))
        {
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
          goto cleanup;
        }

      if (typebuflen < number_of_data_bytes)
        {
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_INTERNAL_ERROR);
          goto cleanup;
        }

      memcpy (typebuf, &areabuf[area_offset_to_bytes], number_of_data_bytes);
      bytes_offset = number_of_data_bytes;
    }

 out:
  rv = 0;
  *bytes_parsed = bytes_offset;
 cleanup:
  return (rv);
}

static int
_sixbitascii_to_ascii (ipmi_fru_parse_ctx_t ctx,
                       uint8_t *typebuf,
                       unsigned int typebuf_bytes,
                       char *typestr,
                       unsigned int typestrlen)
{
  int rv = -1;
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
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_INTERNAL_ERROR);
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

  rv = 0;
 cleanup:
  return (rv);
}

static int
_bcd_to_ascii (ipmi_fru_parse_ctx_t ctx,
               uint8_t *typebuf,
               unsigned int typebuf_bytes,
               char *typestr,
               unsigned int typestrlen)
{
  int rv = -1;
  int i;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (typebuf);
  assert (typebuf_bytes);
  assert (typestr);
  assert (typestrlen);

  if (typestrlen < typebuf_bytes)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  for (i = 0; i < typebuf_bytes; i++)
    {
      /* +1/-1 hackery to get around warnings */
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
#if 0
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "  FRU Unknown BCD Character: %02Xh\n",
                           typebuf[i]);
#endif
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INVALID_BCD_ENCODING);
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_parse_read_type_length_field (ipmi_fru_parse_ctx_t ctx,
                                       uint8_t *areabuf,
                                       unsigned int areabuflen,
                                       unsigned int area_offset_to_bytes,
                                       uint8_t *language_code,
                                       unsigned int *length_parsed,
                                       char *strbuf,
                                       unsigned int strbuflen)
{
  uint8_t type_length;
  uint8_t typebuf[IPMI_FRU_PARSE_BUF_LEN+1];
  char typestr[IPMI_FRU_PARSE_AREA_STRING_MAX+1];
  unsigned int typestrlen = 0;
  uint8_t type_code;
  uint8_t number_of_data_bytes;
  unsigned int bytes_parsed = 0;
  int rv = -1;
  int ret;
  int i;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (areabuf);
  assert (areabuflen);
  assert (area_offset_to_bytes);
  assert (length_parsed);
  assert (strbuf);
  assert (strbuflen);

  memset (typebuf, '\0', IPMI_FRU_PARSE_BUF_LEN+1);
  memset (typestr, '\0', IPMI_FRU_PARSE_AREA_STRING_MAX+1);

  if (ipmi_fru_parse_dump_hex (ctx,
                               areabuf,
                               1,
                               "Type/Length Field Header") < 0)
    goto cleanup;

  type_length = areabuf[area_offset_to_bytes];
  type_code = (type_length & IPMI_FRU_TYPE_LENGTH_TYPE_CODE_MASK) >> IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SHIFT;
  number_of_data_bytes = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

  if (!number_of_data_bytes)
    goto out;

  if (_get_type_length_bytes (ctx,
                              areabuf,
                              areabuflen,
                              area_offset_to_bytes + 1,
                              &bytes_parsed,
                              type_code,
                              number_of_data_bytes,
                              typebuf,
                              IPMI_FRU_PARSE_BUF_LEN) < 0)
    goto cleanup;

  if (!bytes_parsed)
    goto out;

  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BINARY)
    {
      /* Ummm - it's binary or unspecified.  I guess we'll output hex */

      /* must be atleast length of 1, b/c we check for bytes_parsed above */
      ret = snprintf (typestr + typestrlen,
                      IPMI_FRU_PARSE_AREA_STRING_MAX - typestrlen,
                      "%02Xh",
                      typebuf[i]);
      typestrlen += ret;
      for (i = 1; i < bytes_parsed; i++)
        {
          ret = snprintf (typestr + typestrlen,
                          IPMI_FRU_PARSE_AREA_STRING_MAX - typestrlen,
                          " %02Xh",
                          typebuf[i]);
          typestrlen += ret;
        }
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BCD)
    {
      if (_bcd_to_ascii (ctx,
                         typebuf,
                         bytes_parsed,
                         typestr,
                         IPMI_FRU_PARSE_AREA_STRING_MAX) < 0)
        goto cleanup;

      ret = snprintf (typestr + typestrlen,
                      IPMI_FRU_PARSE_AREA_STRING_MAX - typestrlen,
                      "%s",
                      typestr);
      typestrlen += ret;
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SIXBIT_ASCII)
    {
      if (_sixbitascii_to_ascii (ctx,
                                 typebuf,
                                 bytes_parsed,
                                 typestr,
                                 IPMI_FRU_PARSE_AREA_STRING_MAX) < 0)
        goto cleanup;

      ret = snprintf (typestr + typestrlen,
                      IPMI_FRU_PARSE_AREA_STRING_MAX - typestrlen,
                      "%s",
                      typestr);
      typestrlen += ret;
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
#if 0
          pstdout_printf (state_data->pstate,
                          "  FRU %s: Unsupported Language Code: %02Xh\n",
                          str,
                          *language_code);
#endif
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_LANGUAGE_CODE_NOT_SUPPORTED);
          goto cleanup;
        }
      memcpy (typestr, typebuf, bytes_parsed);

      ret = snprintf (typestr + typestrlen,
                      IPMI_FRU_PARSE_AREA_STRING_MAX - typestrlen,
                      "%s",
                      typestr);
      typestrlen += ret;
    }

  if (strbuflen < (typestrlen + 1))
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_OVERFLOW);
      goto cleanup;
    }
  memset (strbuf, '\0', strbuflen);
  memcpy (strbuf, typestr, typestrlen);
 out:
  rv = 0;
  *length_parsed = bytes_parsed + 1;          /* +1 for type/length field */
 cleanup:
  return (rv);
}
