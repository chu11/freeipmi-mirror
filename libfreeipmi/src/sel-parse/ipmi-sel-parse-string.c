/*****************************************************************************\
 *  $Id: ipmi-sel-parse-string.c,v 1.1.2.12 2009-01-07 17:28:08 chu11 Exp $
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
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-string.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

#define NA_STRING         "N/A"
#define ASSERTION_EVENT   "Assertion Event"
#define DEASSERTION_EVENT "Deassertion Event"

#define EVENT_BUFFER_LENGTH     1024
#define SEL_PARSE_BUFFER_LENGTH 256
#define SDR_RECORD_LENGTH       256
#define ID_STRING_LENGTH        256

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

/* 
 * return 1 - parsed fine
 * return 0 - can't parse info/non-decodable
 * return -1 - error
 */
static int
_get_sdr_record_type(ipmi_sel_parse_ctx_t ctx,
                     uint8_t *sdr_record,
                     unsigned int sdr_record_len,
                     uint8_t *sdr_record_type)
{
  fiid_obj_t obj_sdr_record_header = NULL;
  int32_t sdr_record_header_len;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(sdr_record_type);

  SEL_PARSE_FIID_TEMPLATE_LEN_BYTES(sdr_record_header_len, tmpl_sdr_record_header);

  if (sdr_record_len < sdr_record_header_len)
    {
      rv = 0;
      goto cleanup;
    }
  
  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record_header, tmpl_sdr_record_header);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record_header,
                                     sdr_record,
                                     sdr_record_header_len);
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sdr_record_header, "record_type", &val);
  *sdr_record_type = val;

  rv = 1;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sdr_record_header);
  return rv;
}
                    
/* 
 * return 1 - found record
 * return 0 - can't find record
 * return -1 - error
 */
static int
_find_sdr_record(ipmi_sel_parse_ctx_t ctx,
                 struct ipmi_sel_system_event_record_data *system_event_record_data,
                 uint8_t *sdr_record,
                 unsigned int *sdr_record_len)
{
  uint8_t tmp_sdr_record[SDR_RECORD_LENGTH];
  int tmp_sdr_record_len;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(ctx->sdr_cache_ctx);   /* must be checked earlier */
  assert(system_event_record_data);
  assert(sdr_record);
  assert(sdr_record_len);

  if (ipmi_sdr_cache_search_sensor(ctx->sdr_cache_ctx,
                                   system_event_record_data->sensor_number,
                                   system_event_record_data->generator_id) < 0)
    {
      /* IPMI Workaround (achu)
       *
       * Discovered on Supermicro H8QME with SIMSO daughter card.
       *
       * The slave address is reportedly incorrectly by having the
       * generator_id be shifted over by one.  This is a special
       * "try again" corner case.
       */
      if (ipmi_sdr_cache_ctx_errnum(ctx->sdr_cache_ctx) == IPMI_SDR_CACHE_CTX_ERR_NOT_FOUND
          && (system_event_record_data->generator_id == (IPMI_SLAVE_ADDRESS_BMC << 1)))
        {
          if (!ipmi_sdr_cache_search_sensor(ctx->sdr_cache_ctx,
                                            system_event_record_data->sensor_number,
                                            (system_event_record_data->generator_id >> 1)))
            goto fall_through;
          /* else fall through to normal error path */
        }

      if (ipmi_sdr_cache_ctx_errnum(ctx->sdr_cache_ctx) != IPMI_SDR_CACHE_CTX_ERR_NOT_FOUND)
        {
          SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_SDR_CACHE_ERROR);
          return -1;
        }
      /* else can't find it */
      return 0;
    }

fall_through:
  memset(tmp_sdr_record, '\0', SDR_RECORD_LENGTH);

  if ((tmp_sdr_record_len = ipmi_sdr_cache_record_read(ctx->sdr_cache_ctx,
                                                       tmp_sdr_record,
                                                       SDR_RECORD_LENGTH)) < 0)
    {
      SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_SDR_CACHE_ERROR);
      return -1;
    }
  
  if ((*sdr_record_len) < tmp_sdr_record_len)
    {
      SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_INTERNAL_ERROR);
      return -1;
    }

  memcpy(sdr_record, tmp_sdr_record, tmp_sdr_record_len);
  (*sdr_record_len) = tmp_sdr_record_len;

  return 1;
}

/* 
 * return 1 - parsed fine
 * return 0 - can't parse info/non-decodable
 * return -1 - error
 */
static int
_get_sdr_id_string(ipmi_sel_parse_ctx_t ctx,
                   struct ipmi_sel_system_event_record_data *system_event_record_data,
                   char *id_string,
                   unsigned int id_string_len)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t sdr_record[SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = SDR_RECORD_LENGTH;
  uint8_t sdr_record_type;
  int rv = -1;
  int len;
  int ret;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(system_event_record_data);
  assert(id_string);
  assert(id_string_len);

  if (!ctx->sdr_cache_ctx)
    return 0;

  memset(sdr_record, '\0', SDR_RECORD_LENGTH);
  if ((ret = _find_sdr_record(ctx,
                              system_event_record_data,
                              sdr_record,
                              &sdr_record_len)) < 0)
    return -1;

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  if ((ret = _get_sdr_record_type(ctx,
                                  sdr_record,
                                  sdr_record_len,
                                  &sdr_record_type)) < 0)
    goto cleanup;

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }
  
  if (sdr_record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && sdr_record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      && sdr_record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      rv = 0;
      goto cleanup;
    }

  if (sdr_record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_full_sensor_record);
  else if (sdr_record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_compact_sensor_record);
  else
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_event_only_record);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record,
                                     sdr_record,
                                     sdr_record_len);

  SEL_PARSE_FIID_OBJ_GET_DATA_LEN_CLEANUP(len,
                                          obj_sdr_record,
                                          "id_string",
                                          (uint8_t *)id_string,
                                          id_string_len);

  rv = 1;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

/* 
 * return 1 - parsed fine
 * return 0 - can't parse info/non-decodable
 * return -1 - error
 */
static int
_get_sensor_reading(ipmi_sel_parse_ctx_t ctx,
                    struct ipmi_sel_system_event_record_data *system_event_record_data,
                    uint8_t raw_data,
                    double *reading,
                    uint8_t *sensor_unit)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t sdr_record[SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = SDR_RECORD_LENGTH;
  uint8_t sdr_record_type;
  uint8_t sdr_event_reading_type_code;
  int8_t r_exponent;
  int8_t b_exponent;
  int16_t m;
  int16_t b;
  uint8_t linearization;
  uint8_t analog_data_format;
  uint64_t val, val1, val2;
  int rv = -1;
  int ret;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(system_event_record_data);
  assert(ipmi_event_reading_type_code_class(system_event_record_data->event_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD);
  assert(reading);
  assert(sensor_unit);

  if (!ctx->sdr_cache_ctx)
    return 0;

  memset(sdr_record, '\0', SDR_RECORD_LENGTH);
  if ((ret = _find_sdr_record(ctx,
                              system_event_record_data,
                              sdr_record,
                              &sdr_record_len)) < 0)
    return -1;

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  if ((ret = _get_sdr_record_type(ctx,
                                  sdr_record,
                                  sdr_record_len,
                                  &sdr_record_type)) < 0)
    goto cleanup;

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  if (sdr_record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    {
      rv = 0;
      goto cleanup;
    }
  
  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_full_sensor_record);

  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record,
                                     sdr_record,
                                     sdr_record_len);

  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sdr_record, "event_reading_type_code", &val);
  sdr_event_reading_type_code = val;

  if (ipmi_event_reading_type_code_class(sdr_event_reading_type_code) != IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      rv = 0;
      goto cleanup;
    }

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "r_exponent", &val);
  r_exponent = (int8_t) val;
  if (r_exponent & 0x08)
    r_exponent |= 0xF0;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "b_exponent", &val);
  b_exponent = (int8_t) val;
  if (b_exponent & 0x08)
    b_exponent |= 0xF0;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "m_ls", &val1);
  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "m_ms", &val2);
  m = (int16_t)val1;
  m |= ((val2 & 0x3) << 8);
  if (m & 0x200)
    m |= 0xFE00;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "b_ls", &val1);
  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "b_ms", &val2);
  b = (int16_t)val1;
  b |= ((val2 & 0x3) << 8);
  if (b & 0x200)
    b |= 0xFE00;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "linearization", &val);
  linearization = (uint8_t)val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "sensor_unit1.analog_data_format", &val);
  analog_data_format = (uint8_t) val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sdr_record, "sensor_unit2.base_unit", &val);

  if (!IPMI_SENSOR_UNIT_VALID(val))
    val = IPMI_SENSOR_UNIT_UNSPECIFIED;

  *sensor_unit = (uint8_t)val;

  /* if the sensor is not analog, this is most likely a bug in the
   * SDR
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format))
    {
      rv = 0;
      goto cleanup;
    }

  /* We don't currently handle non-linear sensors */
  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization))
    {
      rv = 0;
      goto cleanup;
    }

  if (ipmi_sensor_decode_value (r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                raw_data,
                                reading) < 0)
    {
      SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  rv = 1;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
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
             uint8_t sel_record_type,
             char *buf,
             unsigned int buflen,
             unsigned int flags,
             unsigned int *wlen)
{
  char tmpbuf[SEL_PARSE_BUFFER_LENGTH];
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

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);
  
  if (sel_parse_get_timestamp(ctx, sel_parse_entry, &timestamp) < 0)
    return -1;
  
  t = timestamp;
  localtime_r (&t, &tmp);
  strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%H:%M:%S", &tmp);
  
  if (_SNPRINTF(buf, buflen, wlen, "%s", tmpbuf))
    return 1;
  
  return 0;
}

static int
_output_date(ipmi_sel_parse_ctx_t ctx,
             struct ipmi_sel_parse_entry *sel_parse_entry,
             uint8_t sel_record_type,
             char *buf,
             unsigned int buflen,
             unsigned int flags,
             unsigned int *wlen)
{
  char tmpbuf[SEL_PARSE_BUFFER_LENGTH];
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

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);
          
  if (sel_parse_get_timestamp(ctx, sel_parse_entry, &timestamp) < 0)
    return -1;
          
  t = timestamp;
  localtime_r (&t, &tmp);
  if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_MONTH_STRING)
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_USE_SLASH)
        strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%d/%b/%Y", &tmp);
      else
        strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%d-%b-%Y", &tmp);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_USE_SLASH)
        strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%d/%m/%Y", &tmp);
      else
        strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%d-%m-%Y", &tmp);
    }
          
  if (_SNPRINTF(buf, buflen, wlen, "%s", tmpbuf))
    return 1;
  return 0;
}

static int
_output_sensor_group(ipmi_sel_parse_ctx_t ctx,
                     struct ipmi_sel_parse_entry *sel_parse_entry,
                     uint8_t sel_record_type,
                     char *buf,
                     unsigned int buflen,
                     unsigned int flags,
                     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  const char *sensor_type_str = NULL;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_system_event_record(ctx, sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  sensor_type_str = ipmi_get_sensor_type_string (system_event_record_data.sensor_type);

  if (sensor_type_str)
    {
      if (_SNPRINTF(buf, buflen, wlen, "%s", sensor_type_str))
        return 1;
    }
  
  return 0;
}

static int
_output_sensor_name(ipmi_sel_parse_ctx_t ctx,
                    struct ipmi_sel_parse_entry *sel_parse_entry,
                    uint8_t sel_record_type,
                    char *buf,
                    unsigned int buflen,
                    unsigned int flags,
                    unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char id_string[ID_STRING_LENGTH];
  int ret;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_system_event_record(ctx, sel_parse_entry, &system_event_record_data) < 0)
    return -1;

  memset(id_string, '\0', ID_STRING_LENGTH);
  if ((ret = _get_sdr_id_string(ctx,
                                &system_event_record_data,
                                id_string,
                                ID_STRING_LENGTH)) < 0)
    return -1;
  
  if (ret)
    {
      if (_SNPRINTF(buf,
                    buflen,
                    wlen,
                    "%s",
                    id_string))
        return 1;
      return 0;
    }                             
  /* else fall through */

  if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_VERBOSE)
    {
      if (_SNPRINTF(buf,
                    buflen,
                    wlen,
                    "Sensor #%d (Generator ID %02Xh)",
                    system_event_record_data.sensor_number,
                    system_event_record_data.generator_id))
        return 1;
    }
  else
    {
      if (_SNPRINTF(buf,
                    buflen,
                    wlen,
                    "Sensor #%d",
                    system_event_record_data.sensor_number))
        return 1;
    }
  
  return 0;
}
                            
static int
_output_event_offset(ipmi_sel_parse_ctx_t ctx,
                     struct ipmi_sel_parse_entry *sel_parse_entry,
                     uint8_t sel_record_type,
                     char *buf,
                     unsigned int buflen,
                     unsigned int flags,
                     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbuf[EVENT_BUFFER_LENGTH];
  int output_flag = 0;
  int ret;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_system_event_record(ctx, sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  memset(tmpbuf, '\0', EVENT_BUFFER_LENGTH);

  switch (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      /* Don't use ipmi_get_threshold_message, b/c we didn't call
       * get_sensor_reading.  Fall through to below.
       */
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
      ret = ipmi_get_generic_event_message(system_event_record_data.event_type_code,
                                           system_event_record_data.offset_from_event_reading_type_code,
                                           tmpbuf,
                                           EVENT_BUFFER_LENGTH);
      if (!ret)
        output_flag++;
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:
      ret = ipmi_get_sensor_type_code_message(system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
      if (!ret)
        output_flag++;
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_VERBOSE)
        snprintf(tmpbuf,
                 EVENT_BUFFER_LENGTH,
                 "OEM Event Offset = %02Xh (Event Type Code = %02Xh)",
                 system_event_record_data.offset_from_event_reading_type_code,
                 system_event_record_data.event_type_code);
      else
        snprintf(tmpbuf,
                 EVENT_BUFFER_LENGTH,
                 "OEM Event Offset = %02Xh",
                 system_event_record_data.offset_from_event_reading_type_code);
      output_flag++;
      break;
    default:
      /* fall through to output default output */
      break;
    }

  if (output_flag)
    {
      if (_SNPRINTF(buf, buflen, wlen, "%s", tmpbuf))
        return 1;
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_VERBOSE)
        {
          if (_SNPRINTF(buf,
                        buflen,
                        wlen,
                        "Event Offset = %02Xh (Event Type Code = %02Xh)",
                        system_event_record_data.offset_from_event_reading_type_code,
                        system_event_record_data.event_type_code))
            return 1;
        }
      else
        {
          if (_SNPRINTF(buf,
                        buflen,
                        wlen,
                        "Event Offset = %02Xh",
                        system_event_record_data.offset_from_event_reading_type_code))
            return 1;
        }
    }
  
  return 0;
}

static double
_round_double2 (double d)
{
  double r = 0.0;

  r = (d - (long) d) * 100.0;

  if ((r - (long) r) > 0.5)
    return ((long) d + (((long) r + 1) / 100.0));

  return ((long) d + ((long) r / 100.0));
}

static int
_output_event_data2(ipmi_sel_parse_ctx_t ctx,
                    struct ipmi_sel_parse_entry *sel_parse_entry,
                    uint8_t sel_record_type,
                    char *buf,
                    unsigned int buflen,
                    unsigned int flags,
                    unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbuf[EVENT_BUFFER_LENGTH];
  int output_flag = 0;
  int ret;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_system_event_record(ctx, sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  memset(tmpbuf, '\0', EVENT_BUFFER_LENGTH);

  switch (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      switch (system_event_record_data.event_data2_flag)
        {
        case IPMI_SEL_EVENT_DATA_TRIGGER_READING:
          {
            double reading;
            uint8_t sensor_unit;
            
            if ((ret = _get_sensor_reading(ctx,
                                           &system_event_record_data,
                                           system_event_record_data.event_data2,
                                           &reading,
                                           &sensor_unit)) < 0)
              return -1;
            
            if (ret)
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "Trigger Reading = %.2f %s",
                       _round_double2 (reading),
                       ipmi_sensor_units_abbreviated[sensor_unit]);
            else
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "Trigger Reading = %02Xh",
                       system_event_record_data.event_data2);
            output_flag++;
          }
          break;
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:
          ret = ipmi_get_event_data2_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (!ret)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:
          snprintf(tmpbuf,
                   EVENT_BUFFER_LENGTH,
                   "OEM Event Data2 code = %02Xh",
                   system_event_record_data.event_data2);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "%s",
                       NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return 0;
          break;
        }
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:
      switch (system_event_record_data.event_data2_flag)
        {
        case IPMI_SEL_EVENT_DATA_PREVIOUS_STATE_OR_SEVERITY:
          {
            uint8_t previous_offset_from_event_reading_type_code;
            uint8_t offset_from_severity_event_reading_type_code;
            char tmppreviousbuf[EVENT_BUFFER_LENGTH];
            char tmpseveritybuf[EVENT_BUFFER_LENGTH];
            int previous_output_flag = 0;
            int severity_output_flag = 0;

            if (sel_parse_get_previous_state_or_severity(ctx,
                                                         sel_parse_entry,
                                                         &previous_offset_from_event_reading_type_code,
                                                         &offset_from_severity_event_reading_type_code) < 0)
              return -1;
            if (previous_offset_from_event_reading_type_code != IPMI_SEL_RECORD_UNSPECIFIED_OFFSET)
              {
                ret = ipmi_get_event_data2_message (system_event_record_data.sensor_type,
                                                    system_event_record_data.offset_from_event_reading_type_code,
                                                    previous_offset_from_event_reading_type_code,
                                                    tmppreviousbuf,
                                                    EVENT_BUFFER_LENGTH);
                if (!ret)
                  {
                    snprintf(tmpbuf,
                             EVENT_BUFFER_LENGTH,
                             "Previous State = %s",
                             tmppreviousbuf);
                    previous_output_flag++;
                  }
              }
            if (offset_from_severity_event_reading_type_code != IPMI_SEL_RECORD_UNSPECIFIED_OFFSET)
              {
                ret = ipmi_get_generic_event_message(0x07, /* 0x07 == severity event reading type code */
                                                     offset_from_severity_event_reading_type_code,
                                                     tmpseveritybuf,
                                                     EVENT_BUFFER_LENGTH);
                if (!ret)
                  {
                    snprintf(tmpbuf,
                             EVENT_BUFFER_LENGTH,
                             "Severity State = %s",
                             tmpseveritybuf);
                    severity_output_flag++;
                  }
              }
            /* achu: special case, we need to combine the outputs into one */
            if (previous_output_flag && severity_output_flag)
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "Previous State = %s; Severity State = %s",
                       tmppreviousbuf,
                       tmpseveritybuf);
            if (previous_output_flag || severity_output_flag)
              output_flag++;
          }
          break;
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:
          ret = ipmi_get_event_data2_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (!ret)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:
          snprintf(tmpbuf,
                   EVENT_BUFFER_LENGTH,
                   "OEM Event Data2 code = %02Xh",
                   system_event_record_data.event_data2);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "%s",
                       NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return 0;
          break;
        }
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:
      snprintf(tmpbuf,
               EVENT_BUFFER_LENGTH,
               "OEM Event Data2 code = %02Xh",
               system_event_record_data.event_data2);
      output_flag++;
      break;
    default:
      /* fall through to output default output */
      break;
    }

  if (output_flag)
    {
      if (_SNPRINTF(buf, buflen, wlen, "%s", tmpbuf))
        return 1;
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_VERBOSE)
        {
          if (_SNPRINTF(buf,
                        buflen,
                        wlen,
                        "Event Data2 = %02Xh (Event Type Code = %02Xh)",
                        system_event_record_data.event_data2,
                        system_event_record_data.event_type_code))
            return 1;
        }
      else
        {
          if (_SNPRINTF(buf,
                        buflen,
                        wlen,
                        "Event Data2 = %02Xh",
                        system_event_record_data.event_data2))
            return 1;
        }
    }
  
  return 0;
}

static int
_output_event_data3(ipmi_sel_parse_ctx_t ctx,
                    struct ipmi_sel_parse_entry *sel_parse_entry,
                    uint8_t sel_record_type,
                    char *buf,
                    unsigned int buflen,
                    unsigned int flags,
                    unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbuf[EVENT_BUFFER_LENGTH];
  int output_flag = 0;
  int ret;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_system_event_record(ctx, sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  memset(tmpbuf, '\0', EVENT_BUFFER_LENGTH);

  switch (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      switch (system_event_record_data.event_data3_flag)
        {
        case IPMI_SEL_EVENT_DATA_TRIGGER_THRESHOLD_VALUE:
          {
            double reading;
            uint8_t sensor_unit;
            
            if ((ret = _get_sensor_reading(ctx,
                                           &system_event_record_data,
                                           system_event_record_data.event_data3,
                                           &reading,
                                           &sensor_unit)) < 0)
              return -1;
            
            if (ret)
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "Threshold Reading = %.2f %s",
                       _round_double2 (reading),
                       ipmi_sensor_units_abbreviated[sensor_unit]);
            else
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "Threshold Reading = %02Xh",
                       system_event_record_data.event_data3);
            output_flag++;
          }
          break;
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:
          ret = ipmi_get_event_data3_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              system_event_record_data.event_data3,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (!ret)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:
          snprintf(tmpbuf,
                   EVENT_BUFFER_LENGTH,
                   "OEM Event Data3 code = %02Xh",
                   system_event_record_data.event_data3);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "%s",
                       NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return 0;
          break;
        }
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:
      switch (system_event_record_data.event_data3_flag)
        {
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:
          ret = ipmi_get_event_data3_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              system_event_record_data.event_data3,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (!ret)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:
          snprintf(tmpbuf,
                   EVENT_BUFFER_LENGTH,
                   "OEM Event Data3 code = %02Xh",
                   system_event_record_data.event_data3);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf(tmpbuf,
                       EVENT_BUFFER_LENGTH,
                       "%s",
                       NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return 0;
          break;
        }
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:
      snprintf(tmpbuf,
               EVENT_BUFFER_LENGTH,
               "OEM Event Data3 code = %02Xh",
               system_event_record_data.event_data3);
      output_flag++;
      break;
    default:
      /* fall through to output default output */
      break;
    }

  if (output_flag)
    {
      if (_SNPRINTF(buf, buflen, wlen, "%s", tmpbuf))
        return 1;
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_READ_STRING_FLAGS_VERBOSE)
        {
          if (_SNPRINTF(buf,
                        buflen,
                        wlen,
                        "Event Data3 = %02Xh (Event Type Code = %02Xh)",
                        system_event_record_data.event_data3,
                        system_event_record_data.event_type_code))
            return 1;
        }
      else
        {
          if (_SNPRINTF(buf,
                        buflen,
                        wlen,
                        "Event Data3 = %02Xh",
                        system_event_record_data.event_data3))
            return 1;
        }
    }
  
  return 0;
}

static int
_output_event_direction(ipmi_sel_parse_ctx_t ctx,
                        struct ipmi_sel_parse_entry *sel_parse_entry,
                        uint8_t sel_record_type,
                        char *buf,
                        unsigned int buflen,
                        unsigned int flags,
                        unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char *str = NULL;
  
  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_system_event_record(ctx, sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  if (system_event_record_data.event_direction)
    str = DEASSERTION_EVENT;
  else
    str = ASSERTION_EVENT;
  
  if (_SNPRINTF(buf, buflen, wlen, "%s", str))
    return 1;
  
  return 0;
}

static int
_output_manufacturer_id(ipmi_sel_parse_ctx_t ctx,
                        struct ipmi_sel_parse_entry *sel_parse_entry,
                        uint8_t sel_record_type,
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

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if (sel_parse_get_manufacturer_id(ctx, sel_parse_entry, &manufacturer_id) < 0)
    return -1;
  
  if (_SNPRINTF(buf, buflen, wlen, "Manufacturer ID = %Xh", manufacturer_id))
    return 1;
  
  return 0;
}

static int
_output_oem(ipmi_sel_parse_ctx_t ctx,
            struct ipmi_sel_parse_entry *sel_parse_entry,
            uint8_t sel_record_type,
            char *buf,
            unsigned int buflen,
            unsigned int flags,
            unsigned int *wlen)
{
  uint8_t oem_data[SEL_PARSE_BUFFER_LENGTH];
  int oem_len;
  int oem_index;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  assert(wlen);

  if (ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && ipmi_sel_record_type_class(sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    return _invalid_sel_entry_common(ctx, buf, buflen, flags, wlen);

  if ((oem_len = sel_parse_get_oem(ctx, sel_parse_entry, oem_data, SEL_PARSE_BUFFER_LENGTH)) < 0)
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
      if (_SNPRINTF(buf, buflen, wlen, "%02Xh", oem_data[oem_index]))
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
  uint8_t sel_record_type;
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
                                       &sel_record_type) < 0)
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
                                  sel_record_type, 
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
                                  sel_record_type, 
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
          if ((ret = _output_sensor_group(ctx, 
                                          &sel_parse_entry, 
                                          sel_record_type, 
                                          buf, 
                                          buflen, 
                                          flags,
                                          &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 's') /* sensor name */
        {
          if ((ret = _output_sensor_name(ctx, 
                                         &sel_parse_entry, 
                                         sel_record_type, 
                                         buf, 
                                         buflen, 
                                         flags,
                                         &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'e') /* event offset */
        {
          if ((ret = _output_event_offset(ctx,
                                          &sel_parse_entry,
                                          sel_record_type,
                                          buf,
                                          buflen,
                                          flags,
                                          &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'f') /* event data2  */
        {
          if ((ret = _output_event_data2(ctx,
                                         &sel_parse_entry,
                                         sel_record_type,
                                         buf,
                                         buflen,
                                         flags,
                                         &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'h') /* event data3 */
        {
          if ((ret = _output_event_data3(ctx,
                                         &sel_parse_entry,
                                         sel_record_type,
                                         buf,
                                         buflen,
                                         flags,
                                         &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'j') /* event direction */
        {
          if ((ret = _output_event_direction(ctx, 
                                             &sel_parse_entry, 
                                             sel_record_type, 
                                             buf, 
                                             buflen, 
                                             flags,
                                             &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;

          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'm') /* manufacturer id */
        {
          if ((ret = _output_manufacturer_id(ctx, 
                                             &sel_parse_entry, 
                                             sel_record_type, 
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
                                 sel_record_type, 
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
