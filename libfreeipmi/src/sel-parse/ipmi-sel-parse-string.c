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

#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-numbers-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/spec/ipmi-slave-address-oem-spec.h"
#include "freeipmi/util/ipmi-iana-enterprise-numbers-util.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-sensor-units-util.h"
#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-string.h"
#include "ipmi-sel-parse-trace.h"
#include "ipmi-sel-parse-util.h"

#include "freeipmi-portability.h"

#define NA_STRING         "N/A"
#define ASSERTION_EVENT   "Assertion Event"
#define DEASSERTION_EVENT "Deassertion Event"

#define EVENT_BUFFER_LENGTH     2048
#define SEL_PARSE_BUFFER_LENGTH 256
#define SDR_RECORD_LENGTH       256
#define ID_STRING_LENGTH        256
#define IANA_LENGTH             1024

#define UNITS_BUFFER_LENGTH     1024

static int
_SNPRINTF (char *buf,
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
          if (_SNPRINTF (buf, buflen, wlen, "%s", NA_STRING))
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
  uint8_t tmp_sdr_record[SDR_RECORD_LENGTH];
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
  memset (tmp_sdr_record, '\0', SDR_RECORD_LENGTH);

  if ((tmp_sdr_record_len = ipmi_sdr_cache_record_read (ctx->sdr_cache_ctx,
                                                        tmp_sdr_record,
                                                        SDR_RECORD_LENGTH)) < 0)
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
  uint8_t sdr_record[SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = SDR_RECORD_LENGTH;
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (system_event_record_data);
  assert (id_string);
  assert (id_string_len);

  if (!ctx->sdr_cache_ctx)
    return (0);

  memset (sdr_record, '\0', SDR_RECORD_LENGTH);
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
  uint8_t sdr_record[SDR_RECORD_LENGTH];
  unsigned int sdr_record_len = SDR_RECORD_LENGTH;
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

  memset (sdr_record, '\0', SDR_RECORD_LENGTH);
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

  t = timestamp;
  localtime_r (&t, &tmp);
  strftime (tmpbuf, SEL_PARSE_BUFFER_LENGTH, "%H:%M:%S", &tmp);

  if (_SNPRINTF (buf, buflen, wlen, "%s", tmpbuf))
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

  if (_SNPRINTF (buf, buflen, wlen, "%s", tmpbuf))
    return (1);
  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_sensor_group (ipmi_sel_parse_ctx_t ctx,
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

  sensor_type_str = ipmi_get_sensor_type_string (system_event_record_data.sensor_type);

  if (sensor_type_str)
    {
      if (_SNPRINTF (buf, buflen, wlen, "%s", sensor_type_str))
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

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && ((system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_BIOS 
           && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INVENTEC_BIOS
           && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_START
           && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INVENTEC_BIOS)
          || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_BIOS 
              && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_OK
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC)
          || (system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_POST_ERROR_CODE
              && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_ERROR_CODE
              && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC)))
    {
      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "BIOS"))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
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
      if (_SNPRINTF (buf,
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
      if (_SNPRINTF (buf,
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
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "#%d",
                         system_event_record_data.sensor_number))
            return (1);
        }
      else
        {
          if (_SNPRINTF (buf,
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
static int
_output_oem_event_offset_class_sensor_specific_discrete (ipmi_sel_parse_ctx_t ctx,
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
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR
          || system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE))
    {
      int ret;

      ret = ipmi_get_oem_sensor_type_message (ctx->manufacturer_id,
                                              ctx->product_id,
                                              system_event_record_data->sensor_type,
                                              system_event_record_data->offset_from_event_reading_type_code,
                                              tmpbuf,
                                              tmpbuflen);
      
      if (ret > 0)
        return (1);
    }
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
static int
_output_oem_event_offset_class_oem (ipmi_sel_parse_ctx_t ctx,
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

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   *
   * achu note: There is no official "string" defining the event
   * from the vendor.  "BMC enabled by BIOS" is simply what
   * occurs, so that's what I'm going to say.
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_START
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INVENTEC_BIOS
      && !system_event_record_data->offset_from_event_reading_type_code /* no event */
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "BMC enabled by BIOS");

      return (1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS)
    {
      int ret;

      ret = ipmi_get_oem_generic_event_message (ctx->manufacturer_id,
                                                ctx->product_id,
                                                system_event_record_data->event_type_code,
                                                system_event_record_data->offset_from_event_reading_type_code,
                                                tmpbuf,
                                                tmpbuflen);

      if (ret > 0)
        return (1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge 2900
   * Dell Poweredge 2950
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   * achu: Unique special case
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "OEM Diagnostic Data Event");
      
      return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_offset (ipmi_sel_parse_ctx_t ctx,
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
          if ((ret = _output_oem_event_offset_class_sensor_specific_discrete (ctx,
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
       * Dell Poweredge R610
       * Dell Poweredge R710
       *
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
          if ((ret = _output_oem_event_offset_class_oem (ctx,
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
      if (_SNPRINTF (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Event Offset = %02Xh (Event Type Code = %02Xh)",
                         system_event_record_data.offset_from_event_reading_type_code,
                         system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (_SNPRINTF (buf,
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
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

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   *
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_SMI
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_MEMORY
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PARITY
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR_LOGGING_LIMIT_REACHED)
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && (system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INVENTEC_SBE_WARNING_THRESHOLD
          || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INVENTEC_SBE_CRITICAL_THRESHOLD
          || system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INVENTEC_OTHER))
    {
      /* achu: I'm assuming no output for this one */
      if (system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INVENTEC_OTHER)
        return (0);

      if (system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INVENTEC_SBE_WARNING_THRESHOLD)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "SBE warning threshold");
      else /* system_event_record_data->event_data2 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_INVENTEC_SBE_CRITICAL_THRESHOLD */
        snprintf (tmpbuf,
                  tmpbuflen,
                  "SBE critical threshold");

      return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Power Supply sensors with an event offset
   * IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED
   *
   * achu: XXX: why "event_data2 == 0x01", I don't know, need to get
   * more info from Dell.
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_POWER_SUPPLY_POWER_SUPPLY_FAILURE_DETECTED
      && system_event_record_data->event_data2 == 0x01)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "PMBus Communication Error");
      
      return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Intrusion sensors
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
    {
      if (system_event_record_data->event_data2 == 0x01)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Intrusion while system On");
          
          return (1);
        }
      else if (system_event_record_data->event_data2 == 0x02)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Intrusion while system Off");
          
          return (1);
        }
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   *
   * Specifically for Memory Sensors
   *
   * achu: XXX: event_data2 & 0x0F != 0x0F ??? Need info from Dell
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ctx->ipmi_version_major == IPMI_1_5_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_1_5_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY)
    {
      uint8_t memory_board;
      uint8_t bank_number;
      char memory_board_char;
      
      memory_board = system_event_record_data->event_data2 >> 4;

      /* Dell comments say "0x0F" means card not present */

      if (memory_board != 0x0F)
        memory_board_char = 'A' + memory_board;

      bank_number = system_event_record_data->event_data2 & 0x0F;
      
      if (bank_number != 0x0F && memory_board != 0x0F)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Memory Board %c, Bank %u",
                    memory_board_char,
                    bank_number);
          return (1);
        }
      else if (memory_board != 0x0F)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Memory Board %c",
                    memory_board_char);
          return (1);
        }
      else if (bank_number != 0x0F)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Bank %u",
                    bank_number);
          return (1);
        }
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   * achu: XXX: doc says "FSB" then "CPU", I'm assuming they mean FSB
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
           && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
           && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_IERR)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
              && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_RECOVERABLE
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CPU_PROTOCOL_ERROR)))
    {
      unsigned int num = 0;
      int found = 0;
      int i;

      for (i = 0; i < 8; i++)
        {
          if (system_event_record_data->event_data2 & (0x1 << i))
            {
              num = i + 1;
              found++;
              break;
            }
        }

      if (found)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Front Side Bus %u",
                    num);
          
          return (1);
        }
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_RECOVERABLE
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CPU_MACHINE_CHECK_ERROR)
    {
      unsigned int num = 0;
      int found = 0;
      int i;

      for (i = 0; i < 8; i++)
        {
          if (system_event_record_data->event_data2 & (0x1 << i))
            {
              num = i + 1;
              found++;
              break;
            }
        }

      if (found)
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "CPU %u",
                    num);
          
          return (1);
        }
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
           && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_FAILED_TO_PROGRAM_VIRTUAL_MAC_ADDRESS)
          || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CHIPSET_ERROR)))
    {
      uint8_t device, function;
      
      device = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_BITMASK);
      device >>= IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_SHIFT;

      function = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_BITMASK);
      function >>= IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_SHIFT;

      snprintf (tmpbuf,
                tmpbuflen,
                "Device %u, Function %u",
                device,
                function);

      return (1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   */
  if (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA
      && ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_DELL_POST_FATAL_ERROR)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "BIOS Fatal Error code: %Xh",
                system_event_record_data->event_data2);
      
      return (1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_REDUNDANCY_FULLY_REDUNDANT)
    {
      char *str = NULL;

      if (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_MEMORY_SPARE_MODE_BITMASK)
        str = "Memory is in Spare mode";
      else if (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_MEMORY_RAID_MODE_BITMASK)
        str = "Memory is in RAID mode";
      else /* system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_MEMORY_MIRROR_MODE_BITMASK */
        str = "Memory is in Mirror mode";

      snprintf (tmpbuf,
                tmpbuflen,
                "%s",
                str);

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
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
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_START
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INVENTEC_BIOS
      && !system_event_record_data->offset_from_event_reading_type_code /* no event */
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "BIOS Major Version %X",
                system_event_record_data->event_data2);

      return (1);
    }
  
  /* OEM Interpretation
   *
   * Dell Poweredge 2900
   * Dell Poweredge 2950
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   * offset_from_event_reading_type_code = register offset 11:8
   * data2 = register offset 0:7
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING)
    {
      uint16_t register_offset;
      
      register_offset = system_event_record_data->event_data2;
      register_offset |= (system_event_record_data->offset_from_event_reading_type_code) << 8;
      
      snprintf (tmpbuf,
                tmpbuflen,
                "Register Offset = %Xh",
                register_offset);
      
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
      if (_SNPRINTF (buf, buflen, wlen, "%s", tmpbuf))
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
              if (_SNPRINTF (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Event Data2 = %02Xh (Event Type Code = %02Xh)",
                         system_event_record_data.event_data2,
                         system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (_SNPRINTF (buf,
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
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
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

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   *
   * achu: Note that the Dimm locations are not in a pattern,
   * this is what the doc says.
   *
   * If an invalid dimm location is indicated, fall through
   * and output normal stuff.
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_SMI
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_MEMORY
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PARITY
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR_LOGGING_LIMIT_REACHED)
      && (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH0_DIM1
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH0_DIM0
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH1_DIM1
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH1_DIM0
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH2_DIM1
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH2_DIM0
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU1_CH0_DIM0
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU1_CH1_DIM0
          || system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU1_CH2_DIM0))
    {
      if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH0_DIM1)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU0/Ch0/DIM1");
      else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH0_DIM0)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU0/Ch0/DIM0");
      else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH1_DIM1)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU0/Ch1/DIM1");
      else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH1_DIM0)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU0/Ch1/DIM0");
      else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH2_DIM1)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU0/Ch2/DIM1");
      else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU0_CH2_DIM0)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU0/Ch2/DIM0");
      else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU1_CH0_DIM0)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU1/Ch0/DIM0");
      else if (system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU1_CH1_DIM0)
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU1/Ch1/DIM0");
      else /* system_event_record_data->event_data3 == IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_INVENTEC_DIMM_CPU1_CH2_DIM0 */
        snprintf (tmpbuf,
                  tmpbuflen,
                  "Dimm Number - CPU1/Ch2/DIM0");

      return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Version Change Sensors with an event offset
   * IPMI_SENSOR_TYPE_VERSION_CHANGE_HARDWARE_CHANGE_DETECTED_WITH_ASSOCIATED_ENTITY_WAS_SUCCESSFUL
   *
   * achu: XXX: event_data3 & 0x80 == 0x80 ??? need to ask dell
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_VERSION_CHANGE_HARDWARE_CHANGE_DETECTED_WITH_ASSOCIATED_ENTITY_WAS_SUCCESSFUL
      && (system_event_record_data->event_data3 & 0x80) == 0x80)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Device Slot %u",
                system_event_record_data->event_data3 & 0x7F);
      
      return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Memory Sensors
   *
   * achu: XXX: event_data2 & 0x0F != 0x0F ??? Need info from Dell
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ctx->ipmi_version_major == IPMI_1_5_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_1_5_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY)
    {
      char dimm_char;
      
      dimm_char = 'A' + system_event_record_data->event_data3;
      
      snprintf (tmpbuf,
                tmpbuflen,
                "DIMM %c",
                dimm_char);
      
      return (1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   * achu: XXX: doc says "unspecified" for data 3 flag, I am assuming this is a typo.
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ((system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
           && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
           && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_PROCESSOR_IERR)
          || (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY
              && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_PROCESSOR
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_GENERIC_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY_TRANSITION_TO_NON_RECOVERABLE
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CPU_MACHINE_CHECK_ERROR)))
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "APIC ID %u",
                system_event_record_data->event_data3);
      
      return (1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
           && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_FAILED_TO_PROGRAM_VIRTUAL_MAC_ADDRESS)
          || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
              && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_CHIPSET_ERROR)))
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Bus %u",
                system_event_record_data->event_data3);
      
      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
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
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_PARSE_STRING_MASK));
  assert (flags & IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_BIOS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_START
      && !system_event_record_data->offset_from_event_reading_type_code /* no event */
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "BIOS Minor Version %02X",
                system_event_record_data->event_data3);
      
      return (1);
    }
  
  /* OEM Interpretation
   *
   * Dell Poweredge 2900
   * Dell Poweredge 2950
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Register Value = %02Xh",
                system_event_record_data->event_data3);
      
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
      if (_SNPRINTF (buf, buflen, wlen, "%s", tmpbuf))
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
              if (_SNPRINTF (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Event Data3 = %02Xh (Event Type Code = %02Xh)",
                         system_event_record_data.event_data3,
                         system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Event Data3 = %02Xh",
                         system_event_record_data.event_data3))
            return (1);
        }
    }

  return (0);
}

static char *
_dell_version_change_entity_string (uint8_t data_entity)
{
  if (data_entity == 0)
    return "BIOS";
  else if (data_entity == 1)
    return "BMC";
  else if (data_entity == 2)
    return "iDRAC";
  else if (data_entity == 3)
    return "CMC";
  else if (data_entity == 4)
    return "NIC";
  else
    return "Unrecognized Entity";
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

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_BIOS
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_INVENTEC_BIOS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_START
      && !system_event_record_data->offset_from_event_reading_type_code /* no event */
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "BIOS Version %X.%02X",
                     system_event_record_data->event_data2,
                     system_event_record_data->event_data3))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
      return (1);
    }

  /* OEM Interpretation
   *
   * Inventec 5441/Dell Xanadu2
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
      && ctx->product_id == IPMI_INVENTEC_PRODUCT_ID_5441
      && system_event_record_data->generator_id == IPMI_GENERATOR_ID_OEM_INVENTEC_POST_ERROR_CODE
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS
      && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_ERROR_CODE)
    {
      uint16_t error_code;
      char *error_code_str = NULL;

      error_code = system_event_record_data->event_data2;
      error_code |= (system_event_record_data->event_data3 << 8);

      if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_TIMER_COUNT_READ_WRITE_ERROR)
        error_code_str = "Timer Count Read/Write Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_MASTER_PIC_ERROR)
        error_code_str = "Master PIC Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_SLAVE_PIC_ERROR)
        error_code_str = "Slave PIC Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CMOS_BATTERY_ERROR)
        error_code_str = "CMOS Battery Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CMOS_DIAGNOSTIC_STATUS_ERROR)
        error_code_str = "CMOS Diagnostic Status Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CMOS_CHECKSUM_ERROR)
        error_code_str = "CMOS Checksum Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CMOS_CONFIG_ERROR)
        error_code_str = "CMOS Config Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_KEYBOARD_LOCK_ERROR)
        error_code_str = "Keyboard Lock Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_NO_KEYBOARD_ERROR)
        error_code_str = "No Keyboard Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_KBC_BAT_TEST_ERROR)
        error_code_str = "KBC Bat Test Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CMOS_MEMORY_SIZE_ERROR)
        error_code_str = "CMOS Memory Size Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_RAM_READ_WRITE_TEST_ERROR)
        error_code_str = "RAM Read/Write Test Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_FDD_0_ERROR)
        error_code_str = "FDD 0 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_FLOPPY_CONTROLLER_ERROR)
        error_code_str = "Floppy Controller Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CMOS_DATE_TIME_ERROR)
        error_code_str = "CMOS Date Time Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_NO_PS2_MOUSE_ERROR)
        error_code_str = "No PS2 Mouse Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_REFRESH_TIMER_ERROR)
        error_code_str = "Refresh Timer Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_DISPLAY_MEMORY_ERROR)
        error_code_str = "Display Memory Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_POST_THE_INS_KEY_ERROR)
        error_code_str = "Post the <INS> key Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_DMAC_PAGE_REGISTER_ERROR)
        error_code_str = "DMAC Page Register Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_DMAC1_CHANNEL_REGISTER_ERROR)
        error_code_str = "DMAC1 Channel Register Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_DMAC2_CHANNEL_REGISTER_ERROR)
        error_code_str = "DMAC2 Channel Register Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_PMM_MEMORY_ALLOCATION_ERROR)
        error_code_str = "PMM Memory Allocation Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_PASSWORD_CHECK_ERROR)
        error_code_str = "Password Check Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ADM_MODULE_ERROR)
        error_code_str = "ADM Module Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_LANGUAGE_MODULE_ERROR)
        error_code_str = "Language Module Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_KBC_INTERFACE_ERROR)
        error_code_str = "KBC Interface Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_0_ERROR)
        error_code_str = "HDD 0 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_1_ERROR)
        error_code_str = "HDD 1 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_2_ERROR)
        error_code_str = "HDD 2 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_3_ERROR)
        error_code_str = "HDD 3 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_4_ERROR)
        error_code_str = "HDD 4 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_5_ERROR)
        error_code_str = "HDD 5 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_6_ERROR)
        error_code_str = "HDD 6 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_HDD_7_ERROR)
        error_code_str = "HDD 7 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_0_ERROR)
        error_code_str = "ATAPI 0 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_1_ERROR)
        error_code_str = "ATAPI 1 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_2_ERROR)
        error_code_str = "ATAPI 2 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_3_ERROR)
        error_code_str = "ATAPI 3 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_4_ERROR)
        error_code_str = "ATAPI 4 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_5_ERROR)
        error_code_str = "ATAPI 5 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_6_ERROR)
        error_code_str = "ATAPI 6 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATAPI_7_ERROR)
        error_code_str = "ATAPI 7 Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_ATA_SMART_FEATURE_ERROR)
        error_code_str = "ATA SMART Feature Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_NON_CRITICAL_PASSWORD_CHECK_ERROR)
        error_code_str = "Non-Critical Password Check Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_DUMMY_BIOS_ERROR)
        error_code_str = "Dummy BIOS Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_USB_HC_NOT_FOUND)
        error_code_str = "USB HC Not Found";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_USB_DEVICE_INIT_ERROR)
        error_code_str = "USB Device Init Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_USB_DEVICE_DISABLED)
        error_code_str = "USB Device Disabled";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_USB_OHCI_EMUL_NOT_SUPPORTED)
        error_code_str = "USB OHCI EMUL Not Supported";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_USB_EHCI_64BIT_DATA_STRUCTURE_ERROR)
        error_code_str = "USB EHCI 64bit Data Structure Error";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_SMBIOS_NOT_ENOUGH_SPACE_IN_F000)
        error_code_str = "SMBIOS Not Enough Space In F000";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_AP_APPLICATION_PROCESSOR_FAILED_BIST)
        error_code_str = "AP (Application Processor) failed BIST";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU1_THERMAL_FAILURE_DUE_TO_PROCHOT)
        error_code_str = "CPU1 Thermal Failure due to PROCHOT#";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU2_THERMAL_FAILURE_DUE_TO_PROCHOT)
        error_code_str = "CPU2 Thermal Failure due to PROCHOT#";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU3_THERMAL_FAILURE_DUE_TO_PROCHOT)
        error_code_str = "CPU3 Thermal Failure due to PROCHOT#";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU4_THERMAL_FAILURE_DUE_TO_PROCHOT)
        error_code_str = "CPU4 Thermal Failure due to PROCHOT#";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_PROCESSOR_FAILED_BIST_BSP)
        error_code_str = "Processor failed BIST (BSP)"; /* BSP = Baseboard Service Processor */
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU1_PROCESSOR_MISSING_MICROCODE)
        error_code_str = "CPU1 Processor missing microcode";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU2_PROCESSOR_MISSING_MICROCODE)
        error_code_str = "CPU2 Processor missing microcode";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU3_PROCESSOR_MISSING_MICROCODE)
        error_code_str = "CPU3 Processor missing microcode";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPU4_PROCESSOR_MISSING_MICROCODE)
        error_code_str = "CPU4 Processor missing microcode";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_L2_CACHE_SIZE_MISMATCH)
        error_code_str = "L2 cache size mismatch";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPUID_PROCESSOR_STEPPING_ARE_DIFFERENT)
        error_code_str = "CPUID, Processor stepping are different";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPUID_PROCESSOR_FAMILY_ARE_DIFFERENT)
        error_code_str = "CPUID, Processor family are different";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_FRONT_SIDE_BUS_MISMATCH)
        error_code_str = "Front side bus mismatch";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_CPUID_PROCESSOR_MODEL_ARE_DIFFERENT)
        error_code_str = "CPUID, Processor Model are different";
      else if (error_code == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_OEM_INVENTEC_POST_ERROR_CODE_PROCESSOR_SPEEDS_MISMATCHED)
        error_code_str = "Processor speeds mismatched";
      else
        error_code_str = "Undefined BIOS Error";
      
      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "%s",
                     error_code_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
      return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Power Supply sensors with an event offset 0x06
   *
   * achu: XXX: why "(event_data3 & 0x0F) == 0x03"??, I don't know,
   * need to get more info from Dell.  The comments in their code said
   * "check for error type in byte 3".
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && (system_event_record_data->event_data3 & 0x0F) == 0x03)
    {
      unsigned int watts;

      /* achu: that's not a typo, it's '+=' not a '|=', I'm just
       * copying Dell source at this point in time, don't know why
       * this is 
       */
      watts = system_event_record_data->event_data2 << 4;
      watts += system_event_record_data->event_data3 >> 4;
      
      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "Power Supply Mismatch (%u Watts)",
                     watts))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
      return (1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   *
   * Data2
   * [7:3] = Device Number
   * [2:0] = Function Number
   *
   * Data3
   * [7] = 0 = [6:0] contain a bus number
   *       1 = [6:0] contain a slot number
   * [6:0] = bus or slot number
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && ((system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
           && ((system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_PERR
                && system_event_record_data->sensor_number == IPMI_SENSOR_NUMBER_OEM_DELL_PCI_PARITY_ERROR)
               || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_PCI_SERR
               || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT_BUS_FATAL_ERROR))
          || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR_PCIE_ERROR)
          || (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR
              && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR_FATAL_IO_ERROR))
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      int slot_flag;
      int bus_slot_number;

      /* Dell documentation says to watch out for this specific case */
      if (system_event_record_data->event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT
          && system_event_record_data->event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT)
        return (0);

      slot_flag = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_FLAG_BITMASK);
      slot_flag >>= IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_FLAG_SHIFT;

      bus_slot_number = (system_event_record_data->event_data3 & IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_BITMASK);
      bus_slot_number >>= IPMI_OEM_DELL_EVENT_DATA3_BUS_SLOT_SHIFT;

      if (slot_flag)
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Slot %u",
                         bus_slot_number))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
          
          return (1);
        }
      else
        {
          uint8_t device, function;

          device = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_BITMASK);
          device >>= IPMI_OEM_DELL_EVENT_DATA2_DEVICE_NUMBER_SHIFT;
          
          function = (system_event_record_data->event_data2 & IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_BITMASK);
          function >>= IPMI_OEM_DELL_EVENT_DATA2_FUNCTION_NUMBER_SHIFT;

          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Bus %u, Device %u, Function %u",
                         bus_slot_number,
                         device,
                         function))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;

          return (1);
        }
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Version Change Sensors with an event offset
   * IPMI_SENSOR_TYPE_VERSION_CHANGE_FIRMWARE_OR_SOFTWARE_INCOMPATABILITY_DETECTED_WITH_ASSOCIATED_ENTITY
   *
   * achu: XXX: dataX & 0x1F != 1F ???  Need to ask Dell.
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_VERSION_CHANGE_FIRMWARE_OR_SOFTWARE_INCOMPATABILITY_DETECTED_WITH_ASSOCIATED_ENTITY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t data2_entity, data3_entity;
      uint8_t data2_number, data3_number;
      char *data2_entity_str = NULL;
      char *data3_entity_str = NULL;
      char data2_number_str[EVENT_BUFFER_LENGTH];
      char data3_number_str[EVENT_BUFFER_LENGTH];

      data2_entity = system_event_record_data->event_data2 >> 3;
      data2_number = system_event_record_data->event_data2 & 0x1F;
      data3_entity = system_event_record_data->event_data3 >> 3;
      data3_number = system_event_record_data->event_data3 & 0x1F;

      data2_entity_str = _dell_version_change_entity_string (data2_entity);
      data3_entity_str = _dell_version_change_entity_string (data3_entity);

      memset (data2_number_str, '\0', EVENT_BUFFER_LENGTH);
      memset (data3_number_str, '\0', EVENT_BUFFER_LENGTH);

      if (data2_number != 0x1F)
        snprintf (data2_number_str,
                  EVENT_BUFFER_LENGTH,
                  "%u",
                  data2_number);

      if (data3_number != 0x1F)
        snprintf (data3_number_str,
                  EVENT_BUFFER_LENGTH,
                  "%u",
                  data3_number);

      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "%s%s%s with %s%s%s",
                     data2_entity_str,
                     strlen (data2_number_str) ? " " : "",
                     data2_number_str,
                     data3_entity_str,
                     strlen (data3_number_str) ? " " : "",
                     data3_number_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
      return (1);     
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Version Change Sensors
   *
   * achu: XXX: data2 & 0x0F == 2 ???  Need to ask Dell.
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && (system_event_record_data->event_data2 & 0x0F) == 0x02)
    {
      char *data3_str = NULL;
      
      switch (system_event_record_data->event_data3 & 0x0F)
        {
        case 0:
          data3_str = "other hardware";
        case 1:
          data3_str = "CPU";
        default:
          data3_str = "unknown hardware";
        }
      
      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "BMC Firmware and %s mismatch",
                     data3_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
      return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Dell OEM sensor
   * IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING w/ event offset
   * IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_DEVICE_OPTION_ROM_FAILED_TO_SUPPORT_LINK_TUNING_OR_FLEX_ADDRESS.
   *
   * achu: XXX: data3 & 0x01 => 'B' or 'C' ???  Need to ask Dell
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING_DEVICE_OPTION_ROM_FAILED_TO_SUPPORT_LINK_TUNING_OR_FLEX_ADDRESS
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      if (system_event_record_data->event_data3 & 0x80)
        {
          uint8_t data2_slotspernode;
          uint8_t slotspernode;

          data2_slotspernode = system_event_record_data->event_data2 >> 6;
          
          /* Comments in Dell code referred to this as:
           *
           * "Single Height Blade Format" (1)
           * vs.
           * "Double Heigh Blade Format" (2)
           * vs.
           " "Double Height Double Width Blade Format" (4)
           */
          if (data2_slotspernode == 1)
            slotspernode = 2;
          else if (data2_slotspernode == 2)
            slotspernode = 4;
          else
            slotspernode = 1;   /* default to 1 */
          
          /* Comments in Dell code refer to 
           *
           * "Odd number is B"
           *
           * "Event number is C"
           *
           * I will assume this is some labeling of Dell hardware.
           */
          if (slotspernode >= 2)
            {
              uint8_t slot;
              uint8_t slot_position;

              /* need slot zero based */
              slot = (system_event_record_data->event_data3 & 0x7F) - 1;
              slot_position = (slot/slotspernode) + 1;

              if (_SNPRINTF (buf,
                             buflen,
                             wlen,
                             "Mezzanine %c%c",
                             (system_event_record_data->event_data3 & 0x01) ? 'B' : 'C',
                             '0' + slot_position))
                (*oem_rv) = 1;
              else
                (*oem_rv) = 0;
            }
          else
            {
              if (_SNPRINTF (buf,
                             buflen,
                             wlen,
                             "Mezzanine %c",
                             (system_event_record_data->event_data3 & 0x01) ? 'B' : 'C'))
                (*oem_rv) = 1;
              else
                (*oem_rv) = 0;
            }
        }
      else
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Device embedded"))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
        }
      
      return (1);
    }

  /* OEM Interpretation
   *
   * From Dell Provided Source Code
   * - Handle for Dell Poweredge R610
   * - Handle for Dell Poweredge R710
   *
   * Specifically for Memory Sensors
   *
   * achu: XXX: need info from dell, this doesn't make a lot of sense.
   */
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t memory_board;
      char memory_board_char;
      uint8_t increment = 0;

      memory_board = system_event_record_data->event_data2 >> 4;

      /* Dell comments say "0x0F" means card not present */

      if (memory_board != 0x0F)
        memory_board_char = 'A' + memory_board;

      if ((system_event_record_data->event_data2 & 0x0F) != 0x0F)
        increment = (system_event_record_data->event_data2 & 0x0F) << 3;
      
      /* Dell comments say "NUMA Format" */
      /* achu: XXX: what ??, why would you use the memory board for this?? */
      if (memory_board > 0x07 && memory_board != 0x0F)
        {
          uint8_t dimmspernode;
          char dimmstr[EVENT_BUFFER_LENGTH];
          unsigned int offset = 0;
          int i;

          if (memory_board == 0x09)
            dimmspernode = 6;
          else if (memory_board == 0x0A)
            dimmspernode = 8;
          else if (memory_board == 0x0B)
            dimmspernode = 9;
          else
            dimmspernode = 4;

          memset (dimmstr, '\0', EVENT_BUFFER_LENGTH);

          for (i = 0; i < 8; i++)
            {
              if (system_event_record_data->event_data3 & (0x1 << i))
                {
                  uint8_t node;
                  uint8_t dimmnum;
                  int len;
                  
                  node = (increment + i) / dimmspernode;
                  
                  dimmnum  = ((increment + i) % dimmspernode) + 1;

                  len = snprintf (dimmstr,
                                  EVENT_BUFFER_LENGTH - offset,
                                  "%c%u",
                                  'A' + node,
                                  dimmnum);

                  offset += len;
                  
                  if (offset >= EVENT_BUFFER_LENGTH)
                    break;
                }
            }

          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "DIMM_%s",
                         dimmstr))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
        }
      else
        {
          char dimmstr[EVENT_BUFFER_LENGTH];
          unsigned int offset = 0;
          int i;

          memset (dimmstr, '\0', EVENT_BUFFER_LENGTH);

          for (i = 0; i < 8; i++)
            {
              if (system_event_record_data->event_data3 & (0x1 << i))
                {
                  int len;
                  
                  len = snprintf (dimmstr,
                                  EVENT_BUFFER_LENGTH - offset,
                                  "%u",
                                  (increment + i + 1));
                  
                  offset += len;
                  
                  if (offset >= EVENT_BUFFER_LENGTH)
                    break;
                }
            }

          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "DIMM %s",
                         dimmstr))
            (*oem_rv) = 1;
          else
            (*oem_rv) = 0;
          
        }
      
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

  if (ipmi_event_reading_type_code_class (system_event_record_data.event_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD
      && system_event_record_data.event_data2_flag == IPMI_SEL_EVENT_DATA_TRIGGER_READING
      && system_event_record_data.event_data3_flag == IPMI_SEL_EVENT_DATA_TRIGGER_THRESHOLD_VALUE)
    {
      double trigger_reading;
      double threshold_reading;
      char trigger_sensor_units_buf[UNITS_BUFFER_LENGTH+1];
      char threshold_sensor_units_buf[UNITS_BUFFER_LENGTH+1];
      
      memset (trigger_sensor_units_buf, '\0', UNITS_BUFFER_LENGTH+1);
      if ((data2_ret = _get_sensor_reading (ctx,
                                            &system_event_record_data,
                                            flags,
                                            system_event_record_data.event_data2,
                                            &trigger_reading,
                                            trigger_sensor_units_buf,
                                            UNITS_BUFFER_LENGTH)) < 0)
        return (-1);
      
      memset (threshold_sensor_units_buf, '\0', UNITS_BUFFER_LENGTH+1);
      if ((data3_ret = _get_sensor_reading (ctx,
                                            &system_event_record_data,
                                            flags,
                                            system_event_record_data.event_data3,
                                            &threshold_reading,
                                            threshold_sensor_units_buf,
                                            UNITS_BUFFER_LENGTH)) < 0)
        return (-1);

      if (data2_ret && data3_ret)
        {
          /* achu:
           *
           * IPMI implementations aren't trustworthy for the > or < so
           * I'm doing an actual math check to determine the output.
           *
           * It really should be:
           *
           * if assertion_event
           *   if offset_from_event_reading_type_code & 0x1
           *     use ">"
           *   else
           *     use "<"
           * else
           *   < do opposite for deassertion event >
           */
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Sensor Reading = %.2f %s %s Threshold %.2f %s",
                         _round_double2 (trigger_reading),
                         trigger_sensor_units_buf,
                         _round_double2 (trigger_reading) < _round_double2 (threshold_reading) ? "<" : ">",
                         _round_double2 (threshold_reading),
                         threshold_sensor_units_buf))
            return (1);
          return (0);
        }
      /* else fall through to normal output */
    }

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

      if (_SNPRINTF (buf,
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
      if (_SNPRINTF (buf,
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
      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "%s",
                     tmpbufdata2))
        return (1);
    }
  else if (strlen (tmpbufdata3)
           && strcasecmp (tmpbufdata3, NA_STRING))
    {
      if (_SNPRINTF (buf,
                     buflen,
                     wlen,
                     "%s",
                     tmpbufdata3))
        return (1);
    }
  else if (flags & IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
    {
      if (_SNPRINTF (buf,
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
              if (_SNPRINTF (buf, buflen, wlen, "%s", NA_STRING))
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
      if (_SNPRINTF (buf, buflen, wlen, "%s", tmpbuf))
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
              if (_SNPRINTF (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE)
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Event Data2 = %02Xh (Event Type Code = %02Xh)",
                         system_event_record_data.event_data2,
                         system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (_SNPRINTF (buf,
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

  if (_SNPRINTF (buf, buflen, wlen, "%s", str))
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
      if (_SNPRINTF (buf, buflen, wlen, "Manufacturer ID = %02Xh", manufacturer_id))
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
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Manufacturer ID = %s (%02Xh)",
                         iana_buf,
                         manufacturer_id))
            return (1);

        }
      else
        {
          if (_SNPRINTF (buf,
                         buflen,
                         wlen,
                         "Manufacturer ID = %02Xh",
                         manufacturer_id))
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
_output_oem (ipmi_sel_parse_ctx_t ctx,
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

  if ((oem_len = sel_parse_get_oem (ctx, sel_parse_entry, oem_data, SEL_PARSE_BUFFER_LENGTH)) < 0)
    return (-1);

  if (_SNPRINTF (buf, buflen, wlen, "OEM defined = "))
    return (1);

  for (oem_index = 0; oem_index < oem_len; oem_index++)
    {
      if (oem_index)
        {
          if (_SNPRINTF (buf, buflen, wlen, " "))
            return (1);
        }
      if (_SNPRINTF (buf, buflen, wlen, "%02Xh", oem_data[oem_index]))
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
              if (_SNPRINTF (buf, buflen, &wlen, "%%"))
                goto out;
              percent_flag = 0;
            }
          else
            percent_flag = 1;
          goto end_loop;
        }
      else if (percent_flag && *fmt == 'i') /* record id */
        {
          if (_SNPRINTF (buf, buflen, &wlen, "%u", record_id))
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
      else if (percent_flag && *fmt == 'g') /* sensor group name */
        {
          if ((ret = _output_sensor_group (ctx,
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
      else if (percent_flag && *fmt == 'e') /* event offset */
        {
          if ((ret = _output_event_offset (ctx,
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
          if ((ret = _output_oem (ctx,
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
              if (_SNPRINTF (buf, buflen, &wlen, "%%%c", *fmt))
                goto out;
              percent_flag = 0;
            }
          else
            {
              if (_SNPRINTF (buf, buflen, &wlen, "%c", *fmt))
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
