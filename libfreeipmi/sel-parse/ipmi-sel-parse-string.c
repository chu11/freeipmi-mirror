/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-iana-enterprise-numbers-util.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-sensor-units-util.h"
#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-string.h"
#include "ipmi-sel-parse-string-dell.h"
#include "ipmi-sel-parse-string-fujitsu.h"
#include "ipmi-sel-parse-string-intel.h"
#include "ipmi-sel-parse-string-inventec.h"
#include "ipmi-sel-parse-string-quanta.h"
#include "ipmi-sel-parse-string-sun.h"
#include "ipmi-sel-parse-string-supermicro.h"
#include "ipmi-sel-parse-trace.h"
#include "ipmi-sel-parse-util.h"

#include "freeipmi-portability.h"

#define NA_STRING         "N/A"
#define ASSERTION_EVENT   "Assertion Event"
#define DEASSERTION_EVENT "Deassertion Event"

#define EVENT_BUFFER_LENGTH     4096
#define SEL_PARSE_BUFFER_LENGTH 256
#define ID_STRING_LENGTH        256
#define IANA_LENGTH             1024
#define UNITS_BUFFER_LENGTH     1024

/* returns 0 on success, 1 on success but w/ truncation */
int
ipmi_sel_parse_string_snprintf (char *buf,
				unsigned int buflen,
				unsigned int *wlen,
				const char *fmt,
				...)
{
  va_list ap;
  int ret;

  assert (buf);
  assert (buflen);
  assert (wlen);
  assert (fmt);

  va_start (ap, fmt);
  ret = vsnprintf (buf + *wlen, buflen - *wlen, fmt, ap);
  va_end (ap);
  if (ret >= (buflen - *wlen))
    {
      (*wlen) = buflen;
      return (1);
    }
  (*wlen) += ret;
  return (0);
}

static int
_invalid_sel_entry_common (ipmi_sel_parse_ctx_t ctx,
                           char *buf,
                           unsigned int buflen,
                           unsigned int flags,
                           unsigned int *wlen)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD)
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
        {
          if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
            return (1);
          return (0);
        }
      return (0);
    }
  ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
  return (-1);
}

/*
 * return (1) - found record
 * return (0) - can't find record
 * return (-1) - error
 */
static int
_find_sdr_record (ipmi_sel_parse_ctx_t ctx,
                  struct ipmi_sel_system_event_record_data *system_event_record_data,
                  void *sdr_record,
                  unsigned int *sdr_record_len)
{
  uint8_t tmp_sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  int tmp_sdr_record_len;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (ctx->sdr_cache_ctx);   /* must be checked earlier */
  assert (system_event_record_data);
  assert (sdr_record);
  assert (sdr_record_len);

  if (ipmi_sdr_cache_search_sensor (ctx->sdr_cache_ctx,
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
      if (ipmi_sdr_cache_ctx_errnum (ctx->sdr_cache_ctx) == IPMI_SDR_CACHE_ERR_NOT_FOUND
          && (system_event_record_data->generator_id == (IPMI_SLAVE_ADDRESS_BMC << 1)))
        {
          if (!ipmi_sdr_cache_search_sensor (ctx->sdr_cache_ctx,
                                             system_event_record_data->sensor_number,
                                             (system_event_record_data->generator_id >> 1)))
            goto fall_through;
          /* else fall through to normal error path */
        }

      if (ipmi_sdr_cache_ctx_errnum (ctx->sdr_cache_ctx) != IPMI_SDR_CACHE_ERR_NOT_FOUND)
        {
          SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_SDR_CACHE_ERROR);
          return (-1);
        }
      /* else can't find it */
      return (0);
    }

 fall_through:
  memset (tmp_sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);

  if ((tmp_sdr_record_len = ipmi_sdr_cache_record_read (ctx->sdr_cache_ctx,
                                                        tmp_sdr_record,
                                                        IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
    {
      SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_SDR_CACHE_ERROR);
      return (-1);
    }

  if ((*sdr_record_len) < tmp_sdr_record_len)
    {
      SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  memcpy (sdr_record, tmp_sdr_record, tmp_sdr_record_len);
  (*sdr_record_len) = tmp_sdr_record_len;

  return (1);
}

/*
 * return (1) - parsed fine
 * return (0) - can't parse info/non-decodable
 * return (-1) - error
 */
static int
_get_sdr_id_string (ipmi_sel_parse_ctx_t ctx,
                    struct ipmi_sel_system_event_record_data *system_event_record_data,
                    char *id_string,
                    unsigned int id_string_len)
{
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (system_event_record_data);
  assert (id_string);
  assert (id_string_len);

  if (!ctx->sdr_cache_ctx)
    return (0);

  memset (sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);
  if ((ret = _find_sdr_record (ctx,
                               system_event_record_data,
                               sdr_record,
                               &sdr_record_len)) < 0)
    return (-1);

  if (!ret)
    {
      rv = 0;      
      goto cleanup;
    }

  if (ipmi_sdr_parse_id_string (ctx->sdr_parse_ctx,
                                sdr_record,
                                sdr_record_len,
                                id_string,
                                id_string_len) < 0)
    {
      if (ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INVALID_SDR_RECORD
          || ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INCOMPLETE_SDR_RECORD)
        rv = 0;
      else
        SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  rv = 1;
 cleanup:
  return (rv);
}

/*
 * return (1) - parsed fine
 * return (0) - can't parse info/non-decodable
 * return (-1) - error
 */
static int
_get_sensor_reading (ipmi_sel_parse_ctx_t ctx,
                     struct ipmi_sel_system_event_record_data *system_event_record_data,
                     unsigned int flags,
                     uint8_t raw_data,
                     double *reading,
                     char *sensor_units_buf,
                     unsigned int sensor_units_buflen)
{
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH;
  uint8_t sdr_event_reading_type_code;
  int8_t r_exponent;
  int8_t b_exponent;
  int16_t m;
  int16_t b;
  uint8_t linearization;
  uint8_t analog_data_format;
  uint8_t sensor_units_percentage;
  uint8_t sensor_units_modifier;
  uint8_t sensor_units_rate;
  uint8_t sensor_base_unit_type;
  uint8_t sensor_modifier_unit_type;
  int abbreviated_units;
  int sensor_units_ret;
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (system_event_record_data);
  assert (ipmi_event_reading_type_code_class (system_event_record_data->event_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD);
  assert (reading);
  assert (sensor_units_buf);
  assert (sensor_units_buflen);

  if (!ctx->sdr_cache_ctx)
    return (0);

  memset (sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);
  if ((ret = _find_sdr_record (ctx,
                               system_event_record_data,
                               sdr_record,
                               &sdr_record_len)) < 0)
    return (-1);

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  if (ipmi_sdr_parse_event_reading_type_code (ctx->sdr_parse_ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &sdr_event_reading_type_code) < 0)
    {
      if (ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INVALID_SDR_RECORD
          || ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INCOMPLETE_SDR_RECORD)
        rv = 0;
      else
        SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (ipmi_event_reading_type_code_class (sdr_event_reading_type_code) != IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      rv = 0;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_decoding_data (ctx->sdr_parse_ctx,
                                           sdr_record,
                                           sdr_record_len,
                                           &r_exponent,
                                           &b_exponent,
                                           &m,
                                           &b,
                                           &linearization,
                                           &analog_data_format) < 0)
    {
      if (ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INVALID_SDR_RECORD
          || ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INCOMPLETE_SDR_RECORD)
        rv = 0;
      else
        SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_units (ctx->sdr_parse_ctx,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      if (ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INVALID_SDR_RECORD
          || ipmi_sdr_parse_ctx_errnum (ctx->sdr_parse_ctx) == IPMI_SDR_PARSE_ERR_INCOMPLETE_SDR_RECORD)
        rv = 0;
      else
        SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_NON_ABBREVIATED_UNITS)
    abbreviated_units = 0;
  else
    abbreviated_units = 1;

  memset (sensor_units_buf, '\0', sensor_units_buflen);
  sensor_units_ret = ipmi_sensor_units_string (sensor_units_percentage,
                                               sensor_units_modifier,
                                               sensor_units_rate,
                                               sensor_base_unit_type,
                                               sensor_modifier_unit_type,
                                               sensor_units_buf,
                                               sensor_units_buflen,
                                               abbreviated_units);

  if (sensor_units_ret <= 0)
    snprintf (sensor_units_buf,
              sensor_units_buflen,
              "%s",
              ipmi_sensor_units[IPMI_SENSOR_UNIT_UNSPECIFIED]);

  /* if the sensor is not analog, this is most likely a bug in the
   * SDR
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format))
    {
      rv = 0;
      goto cleanup;
    }

  /* We don't currently handle non-linear sensors */
  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
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
      SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  rv = 1;
 cleanup:
  return (rv);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_time (ipmi_sel_parse_ctx_t ctx,
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

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_timestamp (ctx, sel_parse_entry, &timestamp) < 0)
    return (-1);

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tmp, '\0', sizeof (struct tm));

  t = timestamp;
  localtime_r (&t, &tmp);
  strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%H:%M:%S", &tmp);

  if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
    return (1);

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_date (ipmi_sel_parse_ctx_t ctx,
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

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_timestamp (ctx, sel_parse_entry, &timestamp) < 0)
    return (-1);

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tmp, '\0', sizeof (struct tm));

  t = timestamp;
  localtime_r (&t, &tmp);
  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
    strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%d-%b-%Y", &tmp);
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_DATE_MONTH_STRING)
        {
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_DATE_USE_SLASH)
            strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%b/%d/%Y", &tmp);
          else
            strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%b-%d-%Y", &tmp);
        }
      else
        {
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_DATE_USE_SLASH)
            strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%m/%d/%Y", &tmp);
          else
            strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%m-%d-%Y", &tmp);
        }
    }

  if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
    return (1);
  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_sensor_type (ipmi_sel_parse_ctx_t ctx,
                     struct ipmi_sel_parse_entry *sel_parse_entry,
                     uint8_t sel_record_type,
                     char *buf,
                     unsigned int buflen,
                     unsigned int flags,
                     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  const char *sensor_type_str = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);
  
  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));
  
  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);
  
  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
    sensor_type_str = ipmi_get_oem_sensor_type_string (system_event_record_data.sensor_type,
                                                       (system_event_record_data.event_type_code & 0x7F), 
                                                       ctx->manufacturer_id,
                                                       ctx->product_id);
  else 
    sensor_type_str = ipmi_get_sensor_type_string (system_event_record_data.sensor_type);
  
  if (sensor_type_str)
    {
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", sensor_type_str))
        return (1);
    }
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
static int
_output_oem_sensor_name (ipmi_sel_parse_ctx_t ctx,
                         struct ipmi_sel_parse_entry *sel_parse_entry,
                         uint8_t sel_record_type,
                         char *buf,
                         unsigned int buflen,
                         unsigned int flags,
                         unsigned int *wlen,
                         struct ipmi_sel_system_event_record_data *system_event_record_data,
                         int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = ipmi_sel_parse_output_intel_sensor_name (ctx,
							  sel_parse_entry,
							  sel_record_type,
							  buf,
							  buflen,
							  flags,
							  wlen,
							  system_event_record_data,
							  oem_rv)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }
      
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC)
    {
      if ((ret = ipmi_sel_parse_output_inventec_sensor_name (ctx,
							     sel_parse_entry,
							     sel_record_type,
							     buf,
							     buflen,
							     flags,
							     wlen,
							     system_event_record_data,
							     oem_rv)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA)
    {
      if ((ret = ipmi_sel_parse_output_quanta_sensor_name (ctx,
							   sel_parse_entry,
							   sel_record_type,
							   buf,
							   buflen,
							   flags,
							   wlen,
							   system_event_record_data,
							   oem_rv)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_sensor_name (ipmi_sel_parse_ctx_t ctx,
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

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);

  memset (id_string, '\0', ID_STRING_LENGTH);
  if ((ret = _get_sdr_id_string (ctx,
                                 &system_event_record_data,
                                 id_string,
                                 ID_STRING_LENGTH)) < 0)
    return (-1);

  if (ret)
    {
      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "%s",
                                          id_string))
        return (1);
      return (0);
    }
  /* else fall through */

  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
    {
      int oem_rv = 0;

      if ((ret = _output_oem_sensor_name (ctx,
                                          sel_parse_entry,
                                          sel_record_type,
                                          buf,
                                          buflen,
                                          flags,
                                          wlen,
                                          &system_event_record_data,
                                          &oem_rv)) < 0)
        return (-1);

      if (ret)
        return (oem_rv);
    }

  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
    {
      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "Sensor #%d (Generator ID %02Xh)",
                                          system_event_record_data.sensor_number,
                                          system_event_record_data.generator_id))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
        { 
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "#%d",
                                              system_event_record_data.sensor_number))
            return (1);
        }
      else
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Sensor #%d",
                                              system_event_record_data.sensor_number))
            return (1);
        }
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data1_class_sensor_specific_discrete (ipmi_sel_parse_ctx_t ctx,
                                                        struct ipmi_sel_parse_entry *sel_parse_entry,
                                                        uint8_t sel_record_type,
                                                        char *tmpbuf,
                                                        unsigned int tmpbuflen,
                                                        unsigned int flags,
                                                        unsigned int *wlen,
                                                        struct ipmi_sel_system_event_record_data *system_event_record_data)

{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = ipmi_sel_parse_output_dell_event_data1_class_sensor_specific_discrete (ctx,
											sel_parse_entry,
											sel_record_type,
											tmpbuf,
											tmpbuflen,
											flags,
											wlen,
											system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU)
    {
      if ((ret = ipmi_sel_parse_output_fujitsu_event_data1_class_sensor_specific_discrete (ctx,
                                                                                           sel_parse_entry,
                                                                                           sel_record_type,
                                                                                           tmpbuf,
                                                                                           tmpbuflen,
                                                                                           flags,
                                                                                           wlen,
                                                                                           system_event_record_data)) < 0)
        return (-1);
      
      if (ret)
        return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data1_class_oem (ipmi_sel_parse_ctx_t ctx,
                                   struct ipmi_sel_parse_entry *sel_parse_entry,
                                   uint8_t sel_record_type,
                                   char *tmpbuf,
                                   unsigned int tmpbuflen,
                                   unsigned int flags,
                                   unsigned int *wlen,
                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = ipmi_sel_parse_output_dell_event_data1_class_oem (ctx,
								   sel_parse_entry,
								   sel_record_type,
								   tmpbuf,
								   tmpbuflen,
								   flags,
								   wlen,
								   system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = ipmi_sel_parse_output_intel_event_data1_class_oem (ctx,
								    sel_parse_entry,
								    sel_record_type,
								    tmpbuf,
								    tmpbuflen,
								    flags,
								    wlen,
								    system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC)
    {
      if ((ret = ipmi_sel_parse_output_inventec_event_data1_class_oem (ctx,
								       sel_parse_entry,
								       sel_record_type,
								       tmpbuf,
								       tmpbuflen,
								       flags,
								       wlen,
								       system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA)
    {
      if ((ret = ipmi_sel_parse_output_quanta_event_data1_class_oem (ctx,
                                                                     sel_parse_entry,
                                                                     sel_record_type,
                                                                     tmpbuf,
                                                                     tmpbuflen,
                                                                     flags,
                                                                     wlen,
                                                                     system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  /* achu: Some vendors re-flash the manufacturer id */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
      || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND
      || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_PEPPERCON
      || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_MAGNUM_TECHNOLOGIES)
    {
      if ((ret = ipmi_sel_parse_output_supermicro_event_data1_class_oem (ctx,
									 sel_parse_entry,
									 sel_record_type,
									 tmpbuf,
									 tmpbuflen,
									 flags,
									 wlen,
									 system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_data1 (ipmi_sel_parse_ctx_t ctx,
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

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);

  memset (tmpbuf, '\0', EVENT_BUFFER_LENGTH);

  switch (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      /* Don't use ipmi_get_threshold_message, b/c we didn't call
       * get_sensor_reading.  Fall through to below.
       */
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
      ret = ipmi_get_generic_event_message_short (system_event_record_data.event_type_code,
                                                  system_event_record_data.offset_from_event_reading_type_code,
                                                  tmpbuf,
                                                  EVENT_BUFFER_LENGTH);
      if (ret > 0)
        output_flag++;
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data1_class_sensor_specific_discrete (ctx,
                                                                             sel_parse_entry,
                                                                             sel_record_type,
                                                                             tmpbuf,
                                                                             EVENT_BUFFER_LENGTH,
                                                                             flags,
                                                                             wlen,
                                                                             &system_event_record_data)) < 0)
            return (-1);
          
          if (ret)
            {
              output_flag++;
              break;
            }
        }
      
      /* OEM Interpretation
       *
       * From Dell Spec and Dell Code
       *
       * Dell Poweredge R610
       * Dell Poweredge R710
       *
       * achu: This is a special case, event reading type code and
       * sensor type are non-OEM, Dell added an additional offset not
       * defined by the IPMI spec.
       */
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA
	  && ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
          && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
              || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
	  && system_event_record_data.sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
	  && system_event_record_data.offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_DELL_POST_FATAL_ERROR)
	{
	  snprintf (tmpbuf,
		    EVENT_BUFFER_LENGTH,
		    "POST Fatal Error");
	  output_flag++;
	  break;
	}

      ret = ipmi_get_sensor_type_message_short (system_event_record_data.sensor_type,
                                                system_event_record_data.offset_from_event_reading_type_code,
                                                tmpbuf,
                                                EVENT_BUFFER_LENGTH);
      if (ret > 0)
        output_flag++;
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data1_class_oem (ctx,
                                                        sel_parse_entry,
                                                        sel_record_type,
                                                        tmpbuf,
                                                        EVENT_BUFFER_LENGTH,
                                                        flags,
                                                        wlen,
                                                        &system_event_record_data)) < 0)
            return (-1);
          
          if (ret)
            {
              output_flag++;
              break;
            }
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
        snprintf (tmpbuf,
                  EVENT_BUFFER_LENGTH,
                  "Event Type Code = %02Xh",
                  system_event_record_data.event_type_code);
      else
        {
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM Event Offset = %02Xh (Event Type Code = %02Xh)",
                      system_event_record_data.offset_from_event_reading_type_code,
                      system_event_record_data.event_type_code);
          else
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM Event Offset = %02Xh",
                      system_event_record_data.offset_from_event_reading_type_code);
        }
      output_flag++;
      break;
    default:
      /* fall through to output default output */
      break;
    }

  if (output_flag)
    {
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Offset = %02Xh (Event Type Code = %02Xh)",
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Offset = %02Xh",
                                              system_event_record_data.offset_from_event_reading_type_code))
            return (1);
        }
    }

  return (0);
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

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data2_threshold_oem (ipmi_sel_parse_ctx_t ctx,
                                       struct ipmi_sel_parse_entry *sel_parse_entry,
                                       uint8_t sel_record_type,
                                       char *tmpbuf,
                                       unsigned int tmpbuflen,
                                       unsigned int flags,
                                       unsigned int *wlen,
                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* Nothing Yet */

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data2_discrete_oem (ipmi_sel_parse_ctx_t ctx,
                                      struct ipmi_sel_parse_entry *sel_parse_entry,
                                      uint8_t sel_record_type,
                                      char *tmpbuf,
                                      unsigned int tmpbuflen,
                                      unsigned int flags,
                                      unsigned int *wlen,
                                      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = ipmi_sel_parse_output_dell_event_data2_discrete_oem (ctx,
								      sel_parse_entry,
								      sel_record_type,
								      tmpbuf,
								      tmpbuflen,
								      flags,
								      wlen,
								      system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = ipmi_sel_parse_output_intel_event_data2_discrete_oem (ctx,
								       sel_parse_entry,
								       sel_record_type,
								       tmpbuf,
								       tmpbuflen,
								       flags,
								       wlen,
								       system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC)
    {
      if ((ret = ipmi_sel_parse_output_inventec_event_data2_discrete_oem (ctx,
									  sel_parse_entry,
									  sel_record_type,
									  tmpbuf,
									  tmpbuflen,
									  flags,
									  wlen,
									  system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA)
    {
      if ((ret = ipmi_sel_parse_output_quanta_event_data2_discrete_oem (ctx,
									sel_parse_entry,
									sel_record_type,
									tmpbuf,
									tmpbuflen,
									flags,
									wlen,
									system_event_record_data)) < 0)
	return (-1);
  
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data2_class_oem (ipmi_sel_parse_ctx_t ctx,
                                   struct ipmi_sel_parse_entry *sel_parse_entry,
                                   uint8_t sel_record_type,
                                   char *tmpbuf,
                                   unsigned int tmpbuflen,
                                   unsigned int flags,
                                   unsigned int *wlen,
                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = ipmi_sel_parse_output_dell_event_data2_class_oem (ctx,
								   sel_parse_entry,
								   sel_record_type,
								   tmpbuf,
								   tmpbuflen,
								   flags,
								   wlen,
								   system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }
  
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = ipmi_sel_parse_output_intel_event_data2_class_oem (ctx,
								    sel_parse_entry,
								    sel_record_type,
								    tmpbuf,
								    tmpbuflen,
								    flags,
								    wlen,
								    system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC)
    {
      if ((ret = ipmi_sel_parse_output_inventec_event_data2_class_oem (ctx,
								       sel_parse_entry,
								       sel_record_type,
								       tmpbuf,
								       tmpbuflen,
								       flags,
								       wlen,
								       system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA)
    {
      if ((ret = ipmi_sel_parse_output_quanta_event_data2_class_oem (ctx,
                                                                     sel_parse_entry,
                                                                     sel_record_type,
                                                                     tmpbuf,
                                                                     tmpbuflen,
                                                                     flags,
                                                                     wlen,
                                                                     system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_data2 (ipmi_sel_parse_ctx_t ctx,
                     struct ipmi_sel_parse_entry *sel_parse_entry,
                     uint8_t sel_record_type,
                     char *buf,
                     unsigned int buflen,
                     unsigned int flags,
                     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbuf[EVENT_BUFFER_LENGTH];
  int no_output_flag = 0;
  int output_flag = 0;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);

  memset (tmpbuf, '\0', EVENT_BUFFER_LENGTH);

  switch (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      switch (system_event_record_data.event_data2_flag)
        {
        case IPMI_SEL_EVENT_DATA_TRIGGER_READING:
          {
            double reading;
            char sensor_units_buf[UNITS_BUFFER_LENGTH+1];

            memset (sensor_units_buf, '\0', UNITS_BUFFER_LENGTH+1);
            if ((ret = _get_sensor_reading (ctx,
                                            &system_event_record_data,
                                            flags,
                                            system_event_record_data.event_data2,
                                            &reading,
                                            sensor_units_buf,
                                            UNITS_BUFFER_LENGTH)) < 0)
              return (-1);

            if (ret)
              {
                if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
                  snprintf (tmpbuf,
                            EVENT_BUFFER_LENGTH,
                            "Reading = %.2f %s",
                            _round_double2 (reading),
                            sensor_units_buf);
                else
                  snprintf (tmpbuf,
                            EVENT_BUFFER_LENGTH,
                            "Sensor Reading = %.2f %s",
                            _round_double2 (reading),
                            sensor_units_buf);
              }
            else
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "Sensor Reading = %02Xh",
                        system_event_record_data.event_data2);
            output_flag++;
          }
          break;
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:

          if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }

          ret = ipmi_get_event_data2_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (ret > 0)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:

          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data2_threshold_oem (ctx,
                                                                sel_parse_entry,
                                                                sel_record_type,
                                                                tmpbuf,
                                                                EVENT_BUFFER_LENGTH,
                                                                flags,
                                                                wlen,
                                                                &system_event_record_data)) < 0)
                return (-1);
              
              if (ret)
                {
                  output_flag++;
                  break;
                }
            }

          if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }

          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM code = %02Xh",
                      system_event_record_data.event_data2);
          else
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM Event Data2 code = %02Xh",
                      system_event_record_data.event_data2);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "%s",
                        NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return (0);
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

            if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
              {
                no_output_flag++;
                break;
              }

            if (sel_parse_get_previous_state_or_severity (ctx,
                                                          sel_parse_entry,
                                                          &previous_offset_from_event_reading_type_code,
                                                          &offset_from_severity_event_reading_type_code) < 0)
              return (-1);

            if (previous_offset_from_event_reading_type_code != IPMI_SEL_RECORD_UNSPECIFIED_OFFSET)
              {
                ret = ipmi_get_event_data2_message (system_event_record_data.sensor_type,
                                                    system_event_record_data.offset_from_event_reading_type_code,
                                                    previous_offset_from_event_reading_type_code,
                                                    tmppreviousbuf,
                                                    EVENT_BUFFER_LENGTH);
                if (ret > 0)
                  {
                    snprintf (tmpbuf,
                              EVENT_BUFFER_LENGTH,
                              "Previous State = %s",
                              tmppreviousbuf);
                    previous_output_flag++;
                  }
              }
            if (offset_from_severity_event_reading_type_code != IPMI_SEL_RECORD_UNSPECIFIED_OFFSET)
              {
                ret = ipmi_get_generic_event_message_short (IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY,
                                                            offset_from_severity_event_reading_type_code,
                                                            tmpseveritybuf,
                                                            EVENT_BUFFER_LENGTH);
                if (ret > 0)
                  {
                    snprintf (tmpbuf,
                              EVENT_BUFFER_LENGTH,
                              "Severity State = %s",
                              tmpseveritybuf);
                    severity_output_flag++;
                  }
              }
            /* achu: special case, we need to combine the outputs into one */
            if (previous_output_flag && severity_output_flag)
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "Previous State = %s%sSeverity State = %s",
                        tmppreviousbuf,
                        ctx->separator ? ctx->separator : IPMI_SEL_PARSE_SEPARATOR_STRING,
                        tmpseveritybuf);
            if (previous_output_flag || severity_output_flag)
              output_flag++;
          }
          break;
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:

          if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }

          ret = ipmi_get_event_data2_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (ret > 0)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:
          
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data2_discrete_oem (ctx,
                                                               sel_parse_entry,
                                                               sel_record_type,
                                                               tmpbuf,
                                                               EVENT_BUFFER_LENGTH,
                                                               flags,
                                                               wlen,
                                                               &system_event_record_data)) < 0)
                return (-1);
              
              if (ret)
                {
                  output_flag++;
                  break;
                }
            }
	  
          if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }

          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM code = %02Xh",
                      system_event_record_data.event_data2);
          else
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM Event Data2 code = %02Xh",
                      system_event_record_data.event_data2);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "%s",
                        NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return (0);
          break;
        }
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data2_class_oem (ctx,
                                                        sel_parse_entry,
                                                        sel_record_type,
                                                        tmpbuf,
                                                        EVENT_BUFFER_LENGTH,
                                                        flags,
                                                        wlen,
                                                        &system_event_record_data)) < 0)
            return (-1);
          
          if (ret)
            {
              output_flag++;
              break;
            }
        }
      
      if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
        {
          no_output_flag++;
          break;
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
        snprintf (tmpbuf,
                  EVENT_BUFFER_LENGTH,
                  "Event data2 = %02Xh",
                  system_event_record_data.event_data2);
      else
        snprintf (tmpbuf,
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
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
        return (0);

      if (no_output_flag)
        {
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Data2 = %02Xh (Event Type Code = %02Xh)",
                                              system_event_record_data.event_data2,
                                              system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Data2 = %02Xh",
                                              system_event_record_data.event_data2))
            return (1);
        }
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data3_threshold_oem (ipmi_sel_parse_ctx_t ctx,
                                       struct ipmi_sel_parse_entry *sel_parse_entry,
                                       uint8_t sel_record_type,
                                       char *tmpbuf,
                                       unsigned int tmpbuflen,
                                       unsigned int flags,
                                       unsigned int *wlen,
                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUN_MICROSYSTEMS)
    {
      if ((ret = ipmi_sel_parse_output_sun_event_data3_threshold_oem (ctx,
								      sel_parse_entry,
								      sel_record_type,
								      tmpbuf,
								      tmpbuflen,
								      flags,
								      wlen,
								      system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data3_discrete_oem (ipmi_sel_parse_ctx_t ctx,
                                      struct ipmi_sel_parse_entry *sel_parse_entry,
                                      uint8_t sel_record_type,
                                      char *tmpbuf,
                                      unsigned int tmpbuflen,
                                      unsigned int flags,
                                      unsigned int *wlen,
                                      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = ipmi_sel_parse_output_dell_event_data3_discrete_oem (ctx,
								      sel_parse_entry,
								      sel_record_type,
								      tmpbuf,
								      tmpbuflen,
								      flags,
								      wlen,
								      system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = ipmi_sel_parse_output_intel_event_data3_discrete_oem (ctx,
								       sel_parse_entry,
								       sel_record_type,
								       tmpbuf,
								       tmpbuflen,
								       flags,
								       wlen,
								       system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC)
    {
      if ((ret = ipmi_sel_parse_output_inventec_event_data3_discrete_oem (ctx,
									  sel_parse_entry,
									  sel_record_type,
									  tmpbuf,
									  tmpbuflen,
									  flags,
									  wlen,
									  system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA)
    {
      if ((ret = ipmi_sel_parse_output_quanta_event_data3_discrete_oem (ctx,
									sel_parse_entry,
									sel_record_type,
									tmpbuf,
									tmpbuflen,
									flags,
									wlen,
									system_event_record_data)) < 0)
	return (-1);
  
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUN_MICROSYSTEMS)
    {
      if ((ret = ipmi_sel_parse_output_sun_event_data3_discrete_oem (ctx,
								     sel_parse_entry,
								     sel_record_type,
								     tmpbuf,
								     tmpbuflen,
								     flags,
								     wlen,
								     system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_event_data3_class_oem (ipmi_sel_parse_ctx_t ctx,
                                   struct ipmi_sel_parse_entry *sel_parse_entry,
                                   uint8_t sel_record_type,
                                   char *tmpbuf,
                                   unsigned int tmpbuflen,
                                   unsigned int flags,
                                   unsigned int *wlen,
                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = ipmi_sel_parse_output_dell_event_data3_class_oem (ctx,
								   sel_parse_entry,
								   sel_record_type,
								   tmpbuf,
								   tmpbuflen,
								   flags,
								   wlen,
								   system_event_record_data)) < 0)
	return (-1);
  
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = ipmi_sel_parse_output_intel_event_data3_class_oem (ctx,
								    sel_parse_entry,
								    sel_record_type,
								    tmpbuf,
								    tmpbuflen,
								    flags,
								    wlen,
								    system_event_record_data)) < 0)
	return (-1);
  
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC)
    {
      if ((ret = ipmi_sel_parse_output_inventec_event_data3_class_oem (ctx,
								       sel_parse_entry,
								       sel_record_type,
								       tmpbuf,
								       tmpbuflen,
								       flags,
								       wlen,
								       system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA)
    {
      if ((ret = ipmi_sel_parse_output_quanta_event_data3_class_oem (ctx,
                                                                     sel_parse_entry,
                                                                     sel_record_type,
                                                                     tmpbuf,
                                                                     tmpbuflen,
                                                                     flags,
                                                                     wlen,
                                                                     system_event_record_data)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_data3 (ipmi_sel_parse_ctx_t ctx,
                     struct ipmi_sel_parse_entry *sel_parse_entry,
                     uint8_t sel_record_type,
                     char *buf,
                     unsigned int buflen,
                     unsigned int flags,
                     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbuf[EVENT_BUFFER_LENGTH];
  int no_output_flag = 0;
  int output_flag = 0;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);

  memset (tmpbuf, '\0', EVENT_BUFFER_LENGTH);

  switch (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code))
    {
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD:
      switch (system_event_record_data.event_data3_flag)
        {
        case IPMI_SEL_EVENT_DATA_TRIGGER_THRESHOLD_VALUE:
          {
            double reading;
            char sensor_units_buf[UNITS_BUFFER_LENGTH+1];
            
            memset (sensor_units_buf, '\0', UNITS_BUFFER_LENGTH+1);
            if ((ret = _get_sensor_reading (ctx,
                                            &system_event_record_data,
                                            flags,
                                            system_event_record_data.event_data3,
                                            &reading,
                                            sensor_units_buf,
                                            UNITS_BUFFER_LENGTH)) < 0)
              return (-1);
            
            if (ret)
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "Threshold = %.2f %s",
                        _round_double2 (reading),
                        sensor_units_buf);
            else
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "Threshold = %02Xh",
                        system_event_record_data.event_data3);
            output_flag++;
          }
          break;
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:
          
          if (system_event_record_data.event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }

          ret = ipmi_get_event_data3_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              system_event_record_data.event_data3,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (ret > 0)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:

          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data3_threshold_oem (ctx,
                                                                sel_parse_entry,
                                                                sel_record_type,
                                                                tmpbuf,
                                                                EVENT_BUFFER_LENGTH,
                                                                flags,
                                                                wlen,
                                                                &system_event_record_data)) < 0)
                return (-1);
              
              if (ret)
                {
                  output_flag++;
                  break;
                }
            }

          if (system_event_record_data.event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }

          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM code = %02Xh",
                      system_event_record_data.event_data3);
          else
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM Event Data3 code = %02Xh",
                      system_event_record_data.event_data3);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "%s",
                        NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return (0);
          break;
        }
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE:
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE:
      switch (system_event_record_data.event_data3_flag)
        {
        case IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE:
          if (system_event_record_data.event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }
          ret = ipmi_get_event_data3_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              system_event_record_data.event_data2,
                                              system_event_record_data.event_data3,
                                              tmpbuf,
                                              EVENT_BUFFER_LENGTH);
          if (ret > 0)
            output_flag++;
          break;
        case IPMI_SEL_EVENT_DATA_OEM_CODE:

          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data3_discrete_oem (ctx,
                                                               sel_parse_entry,
                                                               sel_record_type,
                                                               tmpbuf,
                                                               EVENT_BUFFER_LENGTH,
                                                               flags,
                                                               wlen,
                                                               &system_event_record_data)) < 0)
                return (-1);
              
              if (ret)
                {
                  output_flag++;
                  break;
                }
            }
          
          if (system_event_record_data.event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
            {
              no_output_flag++;
              break;
            }

          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM code = %02Xh",
                      system_event_record_data.event_data3);
          else
            snprintf (tmpbuf,
                      EVENT_BUFFER_LENGTH,
                      "OEM Event Data3 code = %02Xh",
                      system_event_record_data.event_data3);
          output_flag++;
          break;
        default:
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "%s",
                        NA_STRING);
              output_flag++;
            }
          else
            /* nothing to output */
            return (0);
          break;
        }
      break;
    case IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM:

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data3_class_oem (ctx,
                                                        sel_parse_entry,
                                                        sel_record_type,
                                                        tmpbuf,
                                                        EVENT_BUFFER_LENGTH,
                                                        flags,
                                                        wlen,
                                                        &system_event_record_data)) < 0)
            return (-1);
          
          if (ret)
            {
              output_flag++;
              break;
            }
        }
     
      if (system_event_record_data.event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
        {
          no_output_flag++;
          break;
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
        snprintf (tmpbuf,
                  EVENT_BUFFER_LENGTH,
                  "Event data3 = %02Xh",
                  system_event_record_data.event_data3);
      else
        snprintf (tmpbuf,
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
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
        return (0);

      if (no_output_flag)
        {
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Data3 = %02Xh (Event Type Code = %02Xh)",
                                              system_event_record_data.event_data3,
                                              system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Data3 = %02Xh",
                                              system_event_record_data.event_data3))
            return (1);
        }
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
static int
_output_oem_event_data2_event_data3 (ipmi_sel_parse_ctx_t ctx,
                                     struct ipmi_sel_parse_entry *sel_parse_entry,
                                     uint8_t sel_record_type,
                                     char *buf,
                                     unsigned int buflen,
                                     unsigned int flags,
                                     unsigned int *wlen,
                                     struct ipmi_sel_system_event_record_data *system_event_record_data,
                                     int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = ipmi_sel_parse_output_dell_event_data2_event_data3 (ctx,
								     sel_parse_entry,
								     sel_record_type,
								     buf,
								     buflen,
								     flags,
								     wlen,
								     system_event_record_data,
								     oem_rv)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU)
    {
      if ((ret = ipmi_sel_parse_output_fujitsu_event_data2_event_data3 (ctx,
                                                                        sel_parse_entry,
                                                                        sel_record_type,
                                                                        buf,
                                                                        buflen,
                                                                        flags,
                                                                        wlen,
                                                                        system_event_record_data,
                                                                        oem_rv)) < 0)
	return (-1);

      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = ipmi_sel_parse_output_intel_event_data2_event_data3 (ctx,
								      sel_parse_entry,
								      sel_record_type,
								      buf,
								      buflen,
								      flags,
								      wlen,
								      system_event_record_data,
								      oem_rv)) < 0)
	return (-1);

      if (ret)
	return (1);
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC)
    {
      if ((ret = ipmi_sel_parse_output_inventec_event_data2_event_data3 (ctx,
									 sel_parse_entry,
									 sel_record_type,
									 buf,
									 buflen,
									 flags,
									 wlen,
									 system_event_record_data,
									 oem_rv)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_data2_event_data3 (ipmi_sel_parse_ctx_t ctx,
                                 struct ipmi_sel_parse_entry *sel_parse_entry,
                                 uint8_t sel_record_type,
                                 char *buf,
                                 unsigned int buflen,
                                 unsigned int flags,
                                 unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbuf[EVENT_BUFFER_LENGTH];
  char tmpbufdata2[EVENT_BUFFER_LENGTH + 1];
  char tmpbufdata3[EVENT_BUFFER_LENGTH + 1];
  unsigned int tmpbufdata2_wlen = 0;
  unsigned int tmpbufdata3_wlen = 0;
  int data2_ret;
  int data3_ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);

  memset (tmpbuf, '\0', EVENT_BUFFER_LENGTH);
  memset (tmpbufdata2, '\0', EVENT_BUFFER_LENGTH+1);
  memset (tmpbufdata3, '\0', EVENT_BUFFER_LENGTH+1);

  /* Special case
   *
   * The event_data3 indicates what "type" event_data2 is, and
   * subsequently what should be output.  So we only need to output
   * event_data3.
   */
  if (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE
      && system_event_record_data.sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED
      && system_event_record_data.offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_CORRECTABLE_MACHINE_CHECK_ERROR_LOGGING_DISABLED
      && system_event_record_data.event_data2_flag == IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE
      && system_event_record_data.event_data3_flag == IPMI_SEL_EVENT_DATA_SENSOR_SPECIFIC_EVENT_EXTENSION_CODE)
    {
      if ((data3_ret = _output_event_data3 (ctx,
					    sel_parse_entry,
					    sel_record_type,
					    tmpbufdata3,
					    EVENT_BUFFER_LENGTH,
					    flags,
					    &tmpbufdata3_wlen)) < 0)
	return (-1);
      
      if (data3_ret)
	{
	  SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
	  return (-1);
	}

      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "%s",
                                          tmpbufdata3))
        return (1);
      return (0);
    }

  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
    {
      int ret;
      int oem_rv = 0;

      if ((ret = _output_oem_event_data2_event_data3 (ctx,
                                                      sel_parse_entry,
                                                      sel_record_type,
                                                      buf,
                                                      buflen,
                                                      flags,
                                                      wlen,
                                                      &system_event_record_data,
                                                      &oem_rv)) < 0)
        return (-1);
      
      if (ret)
        return (oem_rv);
    }

  if ((data2_ret = _output_event_data2 (ctx,
                                        sel_parse_entry,
                                        sel_record_type,
                                        tmpbufdata2,
                                        EVENT_BUFFER_LENGTH,
                                        flags,
                                        &tmpbufdata2_wlen)) < 0)
    return (-1);

  if ((data3_ret = _output_event_data3 (ctx,
                                        sel_parse_entry,
                                        sel_record_type,
                                        tmpbufdata3,
                                        EVENT_BUFFER_LENGTH,
                                        flags,
                                        &tmpbufdata3_wlen)) < 0)
    return (-1);

  if (data2_ret || data3_ret)
    {
      SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((strlen (tmpbufdata2) 
       && strcasecmp (tmpbufdata2, NA_STRING))
      && (strlen (tmpbufdata3)
          && strcasecmp (tmpbufdata3, NA_STRING)))
    {
      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "%s%s%s",
                                          tmpbufdata2,
                                          ctx->separator ? ctx->separator : IPMI_SEL_PARSE_SEPARATOR_STRING,
                                          tmpbufdata3))
        return (1);
    }
  else if (strlen (tmpbufdata2)
           && strcasecmp (tmpbufdata2, NA_STRING))
    {
      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "%s",
                                          tmpbufdata2))
        return (1);
    }
  else if (strlen (tmpbufdata3)
           && strcasecmp (tmpbufdata3, NA_STRING))
    {
      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "%s",
                                          tmpbufdata3))
        return (1);
    }
  else if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
    {
      if (ipmi_sel_parse_string_snprintf (buf,
                                          buflen,
                                          wlen,
                                          "%s",
                                          NA_STRING))
        return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_data2_previous_state_or_severity (ipmi_sel_parse_ctx_t ctx,
                                                struct ipmi_sel_parse_entry *sel_parse_entry,
                                                uint8_t sel_record_type,
                                                char *buf,
                                                unsigned int buflen,
                                                unsigned int flags,
                                                unsigned int *wlen,
                                                int previous_state_flag)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  uint8_t previous_offset_from_event_reading_type_code;
  uint8_t offset_from_severity_event_reading_type_code;
  char tmpbuf[EVENT_BUFFER_LENGTH];
  char tmpstatebuf[EVENT_BUFFER_LENGTH];
  int no_output_flag = 0;
  int output_flag = 0;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);

  if (sel_parse_get_previous_state_or_severity (ctx,
                                                sel_parse_entry,
                                                &previous_offset_from_event_reading_type_code,
                                                &offset_from_severity_event_reading_type_code) < 0)
    return (-1);

  memset (tmpbuf, '\0', EVENT_BUFFER_LENGTH);

  if ((ipmi_event_reading_type_code_class (system_event_record_data.event_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
       || ipmi_event_reading_type_code_class (system_event_record_data.event_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
      && system_event_record_data.event_data2_flag == IPMI_SEL_EVENT_DATA_PREVIOUS_STATE_OR_SEVERITY
      && ((previous_state_flag && previous_offset_from_event_reading_type_code != IPMI_SEL_RECORD_UNSPECIFIED_OFFSET)
          || (!previous_state_flag && offset_from_severity_event_reading_type_code != IPMI_SEL_RECORD_UNSPECIFIED_OFFSET)))
    {
      if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
        {
          no_output_flag++;
          goto out;
        }

      if (previous_state_flag)
        {
          ret = ipmi_get_event_data2_message (system_event_record_data.sensor_type,
                                              system_event_record_data.offset_from_event_reading_type_code,
                                              previous_offset_from_event_reading_type_code,
                                              tmpstatebuf,
                                              EVENT_BUFFER_LENGTH);
          if (ret > 0)
            {
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "Previous State = %s",
                        tmpstatebuf);
              output_flag++;
            }
        }
      else
        {
          ret = ipmi_get_generic_event_message_short (IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY,
                                                      offset_from_severity_event_reading_type_code,
                                                      tmpstatebuf,
                                                      EVENT_BUFFER_LENGTH);
          if (ret > 0)
            {
              snprintf (tmpbuf,
                        EVENT_BUFFER_LENGTH,
                        "Severity State = %s",
                        tmpstatebuf);
              output_flag++;
            }
        }
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD)
        {
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
              return (0);
            }
          return (0);
        }
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      return (-1);
    }

 out:
  if (output_flag)
    {
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
        return (0);

      if (no_output_flag)
        {
          if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Data2 = %02Xh (Event Type Code = %02Xh)",
                                              system_event_record_data.event_data2,
                                              system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Event Data2 = %02Xh",
                                              system_event_record_data.event_data2))
            return (1);
        }
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_data2_previous_state (ipmi_sel_parse_ctx_t ctx,
                                    struct ipmi_sel_parse_entry *sel_parse_entry,
                                    uint8_t sel_record_type,
                                    char *buf,
                                    unsigned int buflen,
                                    unsigned int flags,
                                    unsigned int *wlen)
{
  return (_output_event_data2_previous_state_or_severity (ctx,
                                                          sel_parse_entry,
                                                          sel_record_type,
                                                          buf,
                                                          buflen,
                                                          flags,
                                                          wlen,
                                                          1));
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_data2_severity (ipmi_sel_parse_ctx_t ctx,
                              struct ipmi_sel_parse_entry *sel_parse_entry,
                              uint8_t sel_record_type,
                              char *buf,
                              unsigned int buflen,
                              unsigned int flags,
                              unsigned int *wlen)
{
  return (_output_event_data2_previous_state_or_severity (ctx,
                                                          sel_parse_entry,
                                                          sel_record_type,
                                                          buf,
                                                          buflen,
                                                          flags,
                                                          wlen,
                                                          0));
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_direction (ipmi_sel_parse_ctx_t ctx,
                         struct ipmi_sel_parse_entry *sel_parse_entry,
                         uint8_t sel_record_type,
                         char *buf,
                         unsigned int buflen,
                         unsigned int flags,
                         unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char *str = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_system_event_record (ctx, sel_parse_entry, &system_event_record_data) < 0)
    return (-1);

  if (system_event_record_data.event_direction)
    str = DEASSERTION_EVENT;
  else
    str = ASSERTION_EVENT;

  if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", str))
    return (1);

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_manufacturer_id (ipmi_sel_parse_ctx_t ctx,
                         struct ipmi_sel_parse_entry *sel_parse_entry,
                         uint8_t sel_record_type,
                         char *buf,
                         unsigned int buflen,
                         unsigned int flags,
                         unsigned int *wlen)
{
  uint32_t manufacturer_id;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_parse_get_manufacturer_id (ctx, sel_parse_entry, &manufacturer_id) < 0)
    return (-1);

  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_LEGACY)
    {
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "Manufacturer ID = %02Xh", manufacturer_id))
        return (1);
    }
  else
    {
      char iana_buf[IANA_LENGTH + 1];
      int ret;

      memset (iana_buf, '\0', IANA_LENGTH + 1);
      
      /* if ret == 0 means no string, < 0 means bad manufacturer id
       * either way, output just the number
       */
      ret = ipmi_iana_enterprise_numbers_string (manufacturer_id,
                                                 iana_buf,
                                                 IANA_LENGTH);
      if (ret > 0)
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Manufacturer ID = %s (%02Xh)",
                                              iana_buf,
                                              manufacturer_id))
            return (1);

        }
      else
        {
          if (ipmi_sel_parse_string_snprintf (buf,
                                              buflen,
                                              wlen,
                                              "Manufacturer ID = %02Xh",
                                              manufacturer_id))
            return (1);
        }
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_interpreted_record_data (ipmi_sel_parse_ctx_t ctx,
                                     struct ipmi_sel_parse_entry *sel_parse_entry,
                                     uint8_t sel_record_type,
                                     char *tmpbuf,
                                     unsigned int tmpbuflen,
                                     unsigned int flags,
                                     unsigned int *wlen,
                                     int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (oem_rv);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU)
    {
      if ((ret = ipmi_sel_parse_output_fujitsu_oem_record_data (ctx,
                                                                sel_parse_entry,
                                                                sel_record_type,
                                                                tmpbuf,
                                                                tmpbuflen,
                                                                flags,
                                                                wlen,
                                                                oem_rv)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_record_data (ipmi_sel_parse_ctx_t ctx,
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

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA)
    {
      int ret;
      int oem_rv = 0;
      
      if ((ret = _output_oem_interpreted_record_data (ctx,
                                                      sel_parse_entry,
                                                      sel_record_type,
                                                      buf,
                                                      buflen,
                                                      flags,
                                                      wlen,
                                                      &oem_rv)) < 0)
        return (-1);
      
      if (ret)
        return (oem_rv);
    }

  if ((oem_len = sel_parse_get_oem (ctx, sel_parse_entry, oem_data, SEL_PARSE_BUFFER_LENGTH)) < 0)
    return (-1);

  if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "OEM defined = "))
    return (1);

  for (oem_index = 0; oem_index < oem_len; oem_index++)
    {
      if (oem_index)
        {
          if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, " "))
            return (1);
        }
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%02Xh", oem_data[oem_index]))
        return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_string (ipmi_sel_parse_ctx_t ctx,
                    struct ipmi_sel_parse_entry *sel_parse_entry,
                    uint8_t sel_record_type,
                    char *buf,
                    unsigned int buflen,
                    unsigned int flags,
                    unsigned int *wlen)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (wlen);
  
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU)
    {
      int oem_rv = 0;
      
      if ((ret = ipmi_sel_parse_output_fujitsu_oem_string (ctx,
                                                           sel_parse_entry,
                                                           sel_record_type,
                                                           buf,
                                                           buflen,
                                                           flags,
                                                           wlen,
                                                           &oem_rv)) < 0)
        return (-1);
      
      if (ret)
        return (oem_rv);
    }
  
  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
    {
      if (ipmi_sel_parse_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
        return (1);
    }
  
  return (0);
}

int
sel_parse_format_record_string (ipmi_sel_parse_ctx_t ctx,
                                const char *fmt,
                                const void *record_buf,
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

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (fmt);
  assert (record_buf);
  assert (record_buflen >= IPMI_SEL_RECORD_LENGTH);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));

  memcpy (sel_parse_entry.sel_event_record, record_buf, IPMI_SEL_RECORD_LENGTH);
  sel_parse_entry.sel_event_record_len = IPMI_SEL_RECORD_LENGTH;

  if (sel_parse_get_record_header_info (ctx,
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
              if (ipmi_sel_parse_string_snprintf (buf, buflen, &wlen, "%%"))
                goto out;
              percent_flag = 0;
            }
          else
            percent_flag = 1;
          goto end_loop;
        }
      else if (percent_flag && *fmt == 'i') /* record id */
        {
          if (ipmi_sel_parse_string_snprintf (buf, buflen, &wlen, "%u", record_id))
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 't') /* time */
        {
          if ((ret = _output_time (ctx,
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
          if ((ret = _output_date (ctx,
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
      else if (percent_flag && *fmt == 'T') /* sensor type */
        {
          if ((ret = _output_sensor_type (ctx,
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
          if ((ret = _output_sensor_name (ctx,
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
      else if (percent_flag && *fmt == 'e') /* event data1 */
        {
          if ((ret = _output_event_data1 (ctx,
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
          if ((ret = _output_event_data2 (ctx,
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
          if ((ret = _output_event_data3 (ctx,
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
      else if (percent_flag && *fmt == 'c') /* combined event data 2 and event data 3 string */
        {
          if ((ret = _output_event_data2_event_data3 (ctx,
                                                      &sel_parse_entry,
                                                      sel_record_type,
                                                      buf,
                                                      buflen,
                                                      flags,
                                                      &wlen)) < 0)
            goto cleanup;
          if (ret)
            goto out;
        }
      else if (percent_flag && *fmt == 'p') /* event data2 previous state */
        {
          if ((ret = _output_event_data2_previous_state (ctx,
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
      else if (percent_flag && *fmt == 's') /* event data3 severity */
        {
          if ((ret = _output_event_data2_severity (ctx,
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
      else if (percent_flag && *fmt == 'k') /* event direction */
        {
          if ((ret = _output_event_direction (ctx,
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
          if ((ret = _output_manufacturer_id (ctx,
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
          if ((ret = _output_oem_record_data (ctx,
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
      else if (percent_flag && *fmt == 'O') /* OEM string */
        {
          if ((ret = _output_oem_string (ctx,
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
          if (percent_flag)
            {
              if (ipmi_sel_parse_string_snprintf (buf, buflen, &wlen, "%%%c", *fmt))
                goto out;
              percent_flag = 0;
            }
          else
            {
              if (ipmi_sel_parse_string_snprintf (buf, buflen, &wlen, "%c", *fmt))
                goto out;
            }
        }

    end_loop:
      fmt++;
    }

 out:
  rv = wlen;
  ctx->errnum = IPMI_SEL_PARSE_ERR_SUCCESS;
 cleanup:
  return (rv);
}
