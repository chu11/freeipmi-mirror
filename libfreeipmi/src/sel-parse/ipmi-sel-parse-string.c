/*****************************************************************************\
 *  $Id: ipmi-sel-parse-string.c,v 1.1.2.2 2008-12-31 18:26:06 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-string.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

#define NA_STRING "N/A"

static int
_SNPRINTF(char *buf,
          unsigned int buflen,
          unsigned int *wlen,
          char *fmt,
          ...)
{
  va_list ap;
  int ret;

  assert(buf);
  assert(buflen);
  assert(wlen);
  assert(fmt);

  va_start(ap, fmt);
  ret = vsnprintf(buf + *wlen, buflen - *wlen, fmt, ap);
  va_end(ap);
  if (ret >= (buflen - *wlen))
    {
      (*wlen) = buflen;
      return 1;
    }
  (*wlen) += ret;
  return 0;
}

static int
_invalid_sel_entry_common(ipmi_sel_parse_ctx_t ctx,
                          char *buf,
                          unsigned int buflen,
                          unsigned int flags,
                          unsigned int *wlen)
{
  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);
  
  if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD)
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
        {
          if (_SNPRINTF(buf, buflen, wlen, "%s", NA_STRING))
            return 1;
          return 0;
        }
      return 0;
    }
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
  return -1;
}                        

/* output functions
 *
 * return 0 - continue on
 * return 1 - buffer full, return full buffer to user
 * return -1 - error, cleanup and return error
 */

static int
_output_time(ipmi_sel_parse_ctx_t ctx,
             struct ipmi_sel_parse_entry *sel_parse_entry,
             uint8_t record_type,
             char *buf,
             unsigned int buflen,
             unsigned int flags,
             unsigned int *wlen)
{
  char tmpbuf[256];
  uint32_t timestamp;
  struct tm tmp;
  time_t t;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class(record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);
  
  if (sel_parse_get_timestamp(ctx, sel_parse_entry, &timestamp) < 0)
    return -1;
  
  t = timestamp;
  localtime_r (&t, &tmp);
  strftime (tmpbuf, 256, "%H:%M:%S", &tmp);
  
  if (_SNPRINTF(buf, buflen, wlen, "%s", tmpbuf))
    return 1;
  
  return 0;
}

static int
_output_date(ipmi_sel_parse_ctx_t ctx,
             struct ipmi_sel_parse_entry *sel_parse_entry,
             uint8_t record_type,
             char *buf,
             unsigned int buflen,
             unsigned int flags,
             unsigned int *wlen)
{
  char tmpbuf[256];
  uint32_t timestamp;
  struct tm tmp;
  time_t t;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class(record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);
          
  if (sel_parse_get_timestamp(ctx, sel_parse_entry, &timestamp) < 0)
    return -1;
          
  t = timestamp;
  localtime_r (&t, &tmp);
  if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_MONTH_STRING)
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_USE_SLASH)
        strftime (tmpbuf, 256, "%d/%b/%Y", &tmp);
      else
        strftime (tmpbuf, 256, "%d-%b-%Y", &tmp);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_USE_SLASH)
        strftime (tmpbuf, 256, "%d/%m/%Y", &tmp);
      else
        strftime (tmpbuf, 256, "%d-%m-%Y", &tmp);
    }
          
  if (_SNPRINTF(buf, buflen, wlen, "%s", tmpbuf))
    return 1;
  return 0;
}

static int
_output_manufacturer_id(ipmi_sel_parse_ctx_t ctx,
                        struct ipmi_sel_parse_entry *sel_parse_entry,
                        uint8_t record_type,
                        char *buf,
                        unsigned int buflen,
                        unsigned int flags,
                        unsigned int *wlen)
{
  uint32_t manufacturer_id;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_manufacturer_id(ctx, sel_parse_entry, &manufacturer_id) < 0)
    return -1;
  
  if (_SNPRINTF(buf, buflen, wlen, "%Xh", manufacturer_id))
    return 1;
  
  return 0;
}

static int
_output_oem(ipmi_sel_parse_ctx_t ctx,
            struct ipmi_sel_parse_entry *sel_parse_entry,
            uint8_t record_type,
            char *buf,
            unsigned int buflen,
            unsigned int flags,
            unsigned int *wlen)
{
  uint8_t oem_data[256];
  int oem_len;
  int oem_index;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && ipmi_sel_record_type_class(record_type) != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if ((oem_len = sel_parse_get_oem(ctx, sel_parse_entry, oem_data, 256)) < 0)
    return -1;

  if (_SNPRINTF(buf, buflen, wlen, "OEM defined = "))
    return 1;

  for (oem_index = 0; oem_index < oem_len; oem_index++)
    {
      if (oem_index)
        {
          if (_SNPRINTF(buf, buflen, wlen, " "))
            return 1;
        }
      if (_SNPRINTF(buf, buflen, wlen, "%Xh", oem_data[oem_index]))
        return 1;
    }
  
  return 0;
}

/*
 * %i - record ID in decimal
 * %t - time in format H:M:S using 24 hour clock
 * %d - date in format D-M-YEAR
 * %g - sensor group name
 * %s - sensor name
 * %e - offset from event/reading code string
 * %f - event data 2 string
 * %h - event data 3 string
 * %j - event direction
 * %m - manufacturer id
 * %o - oem data in hex
 * %% - percent sign
 *
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_OUTPUT_NOT_AVAILABLE
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_USE_SLASH
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_MONTH_STRING
 */
int
sel_parse_format_record_string(ipmi_sel_parse_ctx_t ctx,
                               char *fmt,
                               uint8_t *record_buf,
                               unsigned int record_buflen,
                               char *buf,
                               unsigned int buflen,
                               unsigned int flags)
{
  struct ipmi_sel_parse_entry sel_parse_entry;
  uint16_t record_id;
  uint8_t record_type;
  int percent_flag = 0;
  unsigned int wlen = 0;
  int rv = -1;
  int ret;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(fmt);
  assert(record_buf);
  assert(record_buflen >= IPMI_SEL_RECORD_LENGTH);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));

  memcpy(sel_parse_entry.sel_event_record, record_buf, record_buflen);
  sel_parse_entry.sel_event_record_len = record_buflen;

  if (sel_parse_get_record_header_info(ctx,
                                       &sel_parse_entry,
                                       &record_id,
                                       &record_type) < 0)
    goto cleanup;

  while (*fmt)
    {
      if (*fmt == '%')
        {
          if (percent_flag)
            {
              if (_SNPRINTF(buf, buflen, &wlen, "%"))
                goto out;
              percent_flag = 0;
            }
          else
            percent_flag = 1;
          goto end_loop;
        }
      else if (percent_flag && *fmt == 'i') /* record id */
        {
          if (_SNPRINTF(buf, buflen, &wlen, "%u", record_id))
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 't') /* time */
        {
          if ((ret = _output_time(ctx, 
                                  &sel_parse_entry, 
                                  record_type, 
                                  buf, 
                                  buflen, 
                                  flags,
                                  &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'd') /* date */
        {
          if ((ret = _output_date(ctx, 
                                  &sel_parse_entry, 
                                  record_type, 
                                  buf, 
                                  buflen, 
                                  flags,
                                  &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'g') /* sensor group name */
        {
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 's') /* sensor id name */
        {
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'e') /* event offset */
        {
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'f') /* event data2  */
        {
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'h') /* event data3 */
        {
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'j') /* event direction */
        {
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'm') /* manufacturer id */
        {
          if ((ret = _output_manufacturer_id(ctx, 
                                             &sel_parse_entry, 
                                             record_type, 
                                             buf, 
                                             buflen, 
                                             flags,
                                             &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;

          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'o') /* oem data */
        {
          if ((ret = _output_oem(ctx, 
                                 &sel_parse_entry, 
                                 record_type, 
                                 buf, 
                                 buflen, 
                                 flags,
                                 &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else
        {
          if (_SNPRINTF(buf, buflen, &wlen, "%c", *fmt))
            goto out;
        }

    end_loop:
      fmt++;
    }

 out:
  rv = wlen;
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
 cleanup:
  return rv;
}
