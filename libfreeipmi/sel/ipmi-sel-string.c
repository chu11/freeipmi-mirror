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

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/interpret/ipmi-interpret.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/sdr/ipmi-sdr.h"
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
#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/util/ipmi-timestamp-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-dell.h"
#include "ipmi-sel-string-fujitsu.h"
#include "ipmi-sel-string-intel.h"
#include "ipmi-sel-string-inventec.h"
#include "ipmi-sel-string-quanta.h"
#include "ipmi-sel-string-sun.h"
#include "ipmi-sel-string-supermicro.h"
#include "ipmi-sel-string-wistron.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define NA_STRING         "N/A"
#define ASSERTION_EVENT   "Assertion Event"
#define DEASSERTION_EVENT "Deassertion Event"

#define EVENT_BUFFER_LENGTH     4096
#define SEL_BUFFER_LENGTH       256
#define SENSOR_NAME_LENGTH      256
#define IANA_LENGTH             1024
#define UNITS_BUFFER_LENGTH     1024

/* returns 0 on success, 1 on success but w/ truncation */
int
sel_string_snprintf (char *buf,
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

int
sel_string_strcat_comma_separate (char *buf,
				  unsigned int buflen,
				  unsigned int *wlen,
				  const char *str)
{
  assert (buf);
  assert (buflen);
  assert (wlen);
  assert (str); 

  if (strlen (buf) > 0)
    {
      if (sel_string_snprintf (buf, buflen, wlen, ", "))
	return (1);
    }

  return (sel_string_snprintf (buf, buflen, wlen, "%s", str));
}

static int
_invalid_sel_entry_common (ipmi_sel_ctx_t ctx,
                           char *buf,
                           unsigned int buflen,
                           unsigned int flags,
                           unsigned int *wlen)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (flags & IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD)
    {
      if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
        {
          if (sel_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
            return (1);
          return (0);
        }
      return (0);
    }
  ctx->errnum = IPMI_SEL_ERR_INVALID_SEL_ENTRY;
  return (-1);
}

/*
 * return (1) - found record
 * return (0) - can't find record
 * return (-1) - error
 */
static int
_find_and_seek_record (ipmi_sel_ctx_t ctx,
		       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->sdr_ctx);   /* must be checked earlier */
  assert (system_event_record_data);

  if (ipmi_sdr_cache_search_sensor (ctx->sdr_ctx,
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
      if (ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_NOT_FOUND
          && (system_event_record_data->generator_id == (IPMI_SLAVE_ADDRESS_BMC << 1)))
        {
          if (!ipmi_sdr_cache_search_sensor (ctx->sdr_ctx,
                                             system_event_record_data->sensor_number,
                                             (system_event_record_data->generator_id >> 1)))
            return (1);
          /* else fall through to normal error path */
        }

      if (ipmi_sdr_ctx_errnum (ctx->sdr_ctx) != IPMI_SDR_ERR_NOT_FOUND)
        {
          SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_SDR_CACHE_ERROR);
          return (-1);
        }
      /* else can't find it */
      return (0);
    }

  return (1);
}

/*
 * return (1) - parsed fine
 * return (0) - can't parse info/non-decodable
 * return (-1) - error
 */
static int
_get_sensor_name (ipmi_sel_ctx_t ctx,
		  struct ipmi_sel_system_event_record_data *system_event_record_data,
		  unsigned int flags,
		  char *sensor_name,
		  unsigned int sensor_name_len)
{
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (system_event_record_data);
  assert (sensor_name);
  assert (sensor_name_len);

  if (!ctx->sdr_ctx)
    return (0);

  if ((ret = _find_and_seek_record (ctx, system_event_record_data)) < 0)
    return (-1);

  if (!ret)
    {
      rv = 0;      
      goto cleanup;
    }

  if (flags & IPMI_SEL_STRING_FLAGS_ENTITY_SENSOR_NAMES)
    {
      if (ipmi_sdr_parse_entity_sensor_name (ctx->sdr_ctx,
					     NULL,
					     0,
					     system_event_record_data->sensor_number,
					     0, /* flags */
					     sensor_name,
					     sensor_name_len) < 0)
	{
	  if (ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD
	      || ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INCOMPLETE_SDR_RECORD)
	    rv = 0;
	  else
	    SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
	  goto cleanup;
	}
    }
  else
    {
      if (ipmi_sdr_parse_sensor_name (ctx->sdr_ctx,
				      NULL,
				      0,
				      system_event_record_data->sensor_number,
				      0, /* flags */
				      sensor_name,
				      sensor_name_len) < 0)
	{
	  if (ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD
	      || ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INCOMPLETE_SDR_RECORD)
	    rv = 0;
	  else
	    SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
	  goto cleanup;
	}
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
_get_sensor_reading (ipmi_sel_ctx_t ctx,
                     struct ipmi_sel_system_event_record_data *system_event_record_data,
                     unsigned int flags,
                     uint8_t raw_data,
                     double *reading,
                     char *sensor_units_buf,
                     unsigned int sensor_units_buflen)
{
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
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (system_event_record_data);
  assert (ipmi_event_reading_type_code_class (system_event_record_data->event_type_code) == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD);
  assert (reading);
  assert (sensor_units_buf);
  assert (sensor_units_buflen);

  if (!ctx->sdr_ctx)
    return (0);

  if ((ret = _find_and_seek_record (ctx, system_event_record_data)) < 0)
    return (-1);

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  if (ipmi_sdr_parse_event_reading_type_code (ctx->sdr_ctx,
					      NULL,
					      0,
                                              &sdr_event_reading_type_code) < 0)
    {
      if (ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD
          || ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INCOMPLETE_SDR_RECORD)
        rv = 0;
      else
        SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (ipmi_event_reading_type_code_class (sdr_event_reading_type_code) != IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      rv = 0;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_decoding_data (ctx->sdr_ctx,
					   NULL,
					   0,
                                           &r_exponent,
                                           &b_exponent,
                                           &m,
                                           &b,
                                           &linearization,
                                           &analog_data_format) < 0)
    {
      if (ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD
          || ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INCOMPLETE_SDR_RECORD)
        rv = 0;
      else
        SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_units (ctx->sdr_ctx,
				   NULL,
				   0,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      if (ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD
          || ipmi_sdr_ctx_errnum (ctx->sdr_ctx) == IPMI_SDR_ERR_PARSE_INCOMPLETE_SDR_RECORD)
        rv = 0;
      else
        SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (flags & IPMI_SEL_STRING_FLAGS_NON_ABBREVIATED_UNITS)
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
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  rv = 1;
 cleanup:
  return (rv);
}

static int
_get_previous_state_or_severity (ipmi_sel_ctx_t ctx,
				 struct ipmi_sel_entry *sel_entry,
				 uint8_t *previous_offset_from_event_reading_type_code,
				 uint8_t *offset_from_severity_event_reading_type_code)
{
  fiid_obj_t obj_sel_system_event_record = NULL;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (previous_offset_from_event_reading_type_code);
  assert (offset_from_severity_event_reading_type_code);

  if (!(obj_sel_system_event_record = fiid_obj_create (tmpl_sel_system_event_record_discrete_previous_state_severity)))
    {
      SEL_ERRNO_TO_SEL_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sel_system_event_record,
                        sel_entry->sel_event_record,
                        sel_entry->sel_event_record_len) < 0)
    {
      SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "previous_offset_from_event_reading_type_code",
                    &val) < 0)
    {
      SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  *previous_offset_from_event_reading_type_code = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "offset_from_severity_event_reading_type_code",
                    &val) < 0)
    {
      SEL_FIID_OBJECT_ERROR_TO_SEL_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  *offset_from_severity_event_reading_type_code = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_sel_system_event_record);
  return (rv);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_event_interpretation (ipmi_sel_ctx_t ctx,
			      struct ipmi_sel_entry *sel_entry,
			      uint8_t sel_record_type,
			      char *buf,
			      unsigned int buflen,
			      unsigned int flags,
			      unsigned int *wlen)
{
  unsigned int interpret_flags_save;
  unsigned int interpret_flags;
  unsigned int sel_state;
  char *sel_state_str = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (!ctx->interpret_ctx)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERPRET_ERROR);
      return (-1);
    }

  if (ipmi_interpret_ctx_get_flags (ctx->interpret_ctx, &interpret_flags_save) < 0)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERPRET_ERROR);
      return (-1);
    }

  interpret_flags = interpret_flags_save;
  
  if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
    interpret_flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;

  if (ipmi_interpret_ctx_set_flags (ctx->interpret_ctx, interpret_flags) < 0)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERPRET_ERROR);
      return (-1);
    }

  if (ipmi_interpret_sel (ctx->interpret_ctx,
			  sel_entry->sel_event_record,
			  sel_entry->sel_event_record_len,
			  &sel_state) < 0)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERPRET_ERROR);
      return (-1);
    }
  
  if (ipmi_interpret_ctx_set_flags (ctx->interpret_ctx, interpret_flags_save) < 0)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERPRET_ERROR);
      return (-1);
    }
  
  /* achu
   * 
   * NOT_AVAILABLE and IGNORE_UNAVAILABLE_FIELD not applicable here.
   * This output is always possible, the N/A indicates that there is
   * no interpretation available for the current event.
   */
  if (sel_state == IPMI_INTERPRET_STATE_NOMINAL)
    sel_state_str = "Nominal";
  else if (sel_state == IPMI_INTERPRET_STATE_WARNING)
    sel_state_str = "Warning";
  else if (sel_state == IPMI_INTERPRET_STATE_CRITICAL)
    sel_state_str = "Critical";
  else
    sel_state_str = NA_STRING;

  if (sel_string_snprintf (buf, buflen, wlen, "%s", sel_state_str))
    return (1);
  
  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_time (ipmi_sel_ctx_t ctx,
              struct ipmi_sel_entry *sel_entry,
              uint8_t sel_record_type,
              char *buf,
              unsigned int buflen,
              unsigned int flags,
              unsigned int *wlen)
{
  char tmpbuf[SEL_BUFFER_LENGTH + 1];
  unsigned int timestamp_flags;
  uint32_t timestamp;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_timestamp (ctx, sel_entry, &timestamp) < 0)
    return (-1);

  memset (tmpbuf, '\0', SEL_BUFFER_LENGTH + 1);

  timestamp_flags = IPMI_TIMESTAMP_FLAG_ABBREVIATE;
  if (flags & IPMI_SEL_STRING_FLAGS_UTC_TO_LOCALTIME)
    timestamp_flags |= IPMI_TIMESTAMP_FLAG_UTC_TO_LOCALTIME;
  if (flags & IPMI_SEL_STRING_FLAGS_LOCALTIME_TO_UTC)
    timestamp_flags |= IPMI_TIMESTAMP_FLAG_LOCALTIME_TO_UTC;

  if (ipmi_timestamp_string (timestamp,
			     ctx->utc_offset,
			     timestamp_flags,
			     "%H:%M:%S",
			     tmpbuf,
			     SEL_BUFFER_LENGTH) < 0)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (sel_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
    return (1);

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_date (ipmi_sel_ctx_t ctx,
              struct ipmi_sel_entry *sel_entry,
              uint8_t sel_record_type,
              char *buf,
              unsigned int buflen,
              unsigned int flags,
              unsigned int *wlen)
{
  char tmpbuf[SEL_BUFFER_LENGTH + 1];
  unsigned int timestamp_flags;
  uint32_t timestamp;
  char *date_format = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_timestamp (ctx, sel_entry, &timestamp) < 0)
    return (-1);

  if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
    date_format = "%d-%b-%Y";
  else
    {
      if (flags & IPMI_SEL_STRING_FLAGS_DATE_MONTH_STRING)
        {
          if (flags & IPMI_SEL_STRING_FLAGS_DATE_USE_SLASH)
            date_format = "%b/%d/%Y";
          else
            date_format = "%b-%d-%Y";
        }
      else
        {
          if (flags & IPMI_SEL_STRING_FLAGS_DATE_USE_SLASH)
            date_format = "%m/%d/%Y";
          else
            date_format = "%m-%d-%Y";
        }
    }

  memset (tmpbuf, '\0', SEL_BUFFER_LENGTH + 1);
  
  timestamp_flags = IPMI_TIMESTAMP_FLAG_ABBREVIATE;
  if (flags & IPMI_SEL_STRING_FLAGS_UTC_TO_LOCALTIME)
    timestamp_flags |= IPMI_TIMESTAMP_FLAG_UTC_TO_LOCALTIME;
  if (flags & IPMI_SEL_STRING_FLAGS_LOCALTIME_TO_UTC)
    timestamp_flags |= IPMI_TIMESTAMP_FLAG_LOCALTIME_TO_UTC;

  if (ipmi_timestamp_string (timestamp,
			     ctx->utc_offset,
			     timestamp_flags,
			     date_format,
			     tmpbuf,
			     SEL_BUFFER_LENGTH) < 0)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      return (-1);
    }
  
  if (sel_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
    return (1);
  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_sensor_type (ipmi_sel_ctx_t ctx,
                     struct ipmi_sel_entry *sel_entry,
                     uint8_t sel_record_type,
                     char *buf,
                     unsigned int buflen,
                     unsigned int flags,
                     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  const char *sensor_type_str = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);
  
  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));
  
  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
    return (-1);
  
  if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
    sensor_type_str = ipmi_get_oem_sensor_type_string (system_event_record_data.sensor_type,
                                                       (system_event_record_data.event_type_code & 0x7F), 
                                                       ctx->manufacturer_id,
                                                       ctx->product_id);
  else 
    sensor_type_str = ipmi_get_sensor_type_string (system_event_record_data.sensor_type);
  
  if (sensor_type_str)
    {
      if (sel_string_snprintf (buf, buflen, wlen, "%s", sensor_type_str))
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
_output_oem_sensor_name (ipmi_sel_ctx_t ctx,
                         struct ipmi_sel_entry *sel_entry,
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
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = sel_string_output_intel_sensor_name (ctx,
						      sel_entry,
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
      if ((ret = sel_string_output_inventec_sensor_name (ctx,
							 sel_entry,
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
      if ((ret = sel_string_output_quanta_sensor_name (ctx,
						       sel_entry,
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
_output_sensor_name (ipmi_sel_ctx_t ctx,
                     struct ipmi_sel_entry *sel_entry,
                     uint8_t sel_record_type,
                     char *buf,
                     unsigned int buflen,
                     unsigned int flags,
                     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char sensor_name_buf[SENSOR_NAME_LENGTH + 1];
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
    return (-1);

  memset (sensor_name_buf, '\0', SENSOR_NAME_LENGTH + 1);
  if ((ret = _get_sensor_name (ctx,
			       &system_event_record_data,
			       flags,
			       sensor_name_buf,
			       SENSOR_NAME_LENGTH)) < 0)
    return (-1);
  
  if (ret)
    {
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s",
			       sensor_name_buf))
        return (1);
      return (0);
    }
  /* else fall through */

  if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
    {
      int oem_rv = 0;

      if ((ret = _output_oem_sensor_name (ctx,
                                          sel_entry,
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

  if (flags & IPMI_SEL_STRING_FLAGS_VERBOSE)
    {
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "Sensor #%d (Generator ID %02Xh)",
			       system_event_record_data.sensor_number,
			       system_event_record_data.generator_id))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
        { 
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "#%d",
				   system_event_record_data.sensor_number))
            return (1);
        }
      else
        {
          if (sel_string_snprintf (buf,
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
_output_oem_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
                                                        struct ipmi_sel_entry *sel_entry,
                                                        uint8_t sel_record_type,
                                                        char *tmpbuf,
                                                        unsigned int tmpbuflen,
                                                        unsigned int flags,
                                                        unsigned int *wlen,
                                                        struct ipmi_sel_system_event_record_data *system_event_record_data)

{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = sel_string_output_dell_event_data1_class_sensor_specific_discrete (ctx,
										    sel_entry,
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
      if ((ret = sel_string_output_fujitsu_event_data1_class_sensor_specific_discrete (ctx,
										       sel_entry,
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
      if ((ret = sel_string_output_intel_event_data1_class_sensor_specific_discrete (ctx,
										     sel_entry,
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

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON)
    {
      if ((ret = sel_string_output_wistron_event_data1_class_sensor_specific_discrete (ctx,
										       sel_entry,
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
_output_oem_event_data1_class_oem (ipmi_sel_ctx_t ctx,
                                   struct ipmi_sel_entry *sel_entry,
                                   uint8_t sel_record_type,
                                   char *tmpbuf,
                                   unsigned int tmpbuflen,
                                   unsigned int flags,
                                   unsigned int *wlen,
                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = sel_string_output_dell_event_data1_class_oem (ctx,
							       sel_entry,
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
      if ((ret = sel_string_output_intel_event_data1_class_oem (ctx,
								sel_entry,
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
      if ((ret = sel_string_output_inventec_event_data1_class_oem (ctx,
								   sel_entry,
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
      if ((ret = sel_string_output_quanta_event_data1_class_oem (ctx,
								 sel_entry,
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
      if ((ret = sel_string_output_supermicro_event_data1_class_oem (ctx,
								     sel_entry,
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
_output_event_data1 (ipmi_sel_ctx_t ctx,
                     struct ipmi_sel_entry *sel_entry,
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
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
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

      if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data1_class_sensor_specific_discrete (ctx,
                                                                             sel_entry,
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
      if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA
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

      if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data1_class_oem (ctx,
                                                        sel_entry,
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

      if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
        snprintf (tmpbuf,
                  EVENT_BUFFER_LENGTH,
                  "Event Type Code = %02Xh",
                  system_event_record_data.event_type_code);
      else
        {
          if (flags & IPMI_SEL_STRING_FLAGS_VERBOSE)
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
      if (sel_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_STRING_FLAGS_VERBOSE)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "Event Offset = %02Xh (Event Type Code = %02Xh)",
				   system_event_record_data.offset_from_event_reading_type_code,
				   system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (sel_string_snprintf (buf,
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
_output_oem_event_data2_threshold_oem (ipmi_sel_ctx_t ctx,
                                       struct ipmi_sel_entry *sel_entry,
                                       uint8_t sel_record_type,
                                       char *tmpbuf,
                                       unsigned int tmpbuflen,
                                       unsigned int flags,
                                       unsigned int *wlen,
                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
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
_output_oem_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
                                      struct ipmi_sel_entry *sel_entry,
                                      uint8_t sel_record_type,
                                      char *tmpbuf,
                                      unsigned int tmpbuflen,
                                      unsigned int flags,
                                      unsigned int *wlen,
                                      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = sel_string_output_dell_event_data2_discrete_oem (ctx,
								  sel_entry,
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
      if ((ret = sel_string_output_intel_event_data2_discrete_oem (ctx,
								   sel_entry,
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
      if ((ret = sel_string_output_inventec_event_data2_discrete_oem (ctx,
								      sel_entry,
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
      if ((ret = sel_string_output_quanta_event_data2_discrete_oem (ctx,
								    sel_entry,
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

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON)
    {
      if ((ret = sel_string_output_wistron_event_data2_discrete_oem (ctx,
								     sel_entry,
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
_output_oem_event_data2_class_oem (ipmi_sel_ctx_t ctx,
                                   struct ipmi_sel_entry *sel_entry,
                                   uint8_t sel_record_type,
                                   char *tmpbuf,
                                   unsigned int tmpbuflen,
                                   unsigned int flags,
                                   unsigned int *wlen,
                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = sel_string_output_dell_event_data2_class_oem (ctx,
							       sel_entry,
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
      if ((ret = sel_string_output_intel_event_data2_class_oem (ctx,
								sel_entry,
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
      if ((ret = sel_string_output_inventec_event_data2_class_oem (ctx,
								   sel_entry,
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
      if ((ret = sel_string_output_quanta_event_data2_class_oem (ctx,
								 sel_entry,
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
_output_event_data2 (ipmi_sel_ctx_t ctx,
                     struct ipmi_sel_entry *sel_entry,
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
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
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
                if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
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

          if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data2_threshold_oem (ctx,
                                                                sel_entry,
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

          if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
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
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
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

            if (_get_previous_state_or_severity (ctx,
						 sel_entry,
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
                        ctx->separator ? ctx->separator : IPMI_SEL_SEPARATOR_STRING,
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
          
          if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data2_discrete_oem (ctx,
                                                               sel_entry,
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

          if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
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
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
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

      if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data2_class_oem (ctx,
                                                        sel_entry,
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
      
      if (system_event_record_data.event_data2 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT
	  || system_event_record_data.event_data2_flag == IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE)
        {
          no_output_flag++;
          break;
        }

      if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
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
      if (sel_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
        return (0);

      if (no_output_flag)
        {
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (sel_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_STRING_FLAGS_VERBOSE)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "Event Data2 = %02Xh (Event Type Code = %02Xh)",
				   system_event_record_data.event_data2,
				   system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (sel_string_snprintf (buf,
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
_output_oem_event_data3_threshold_oem (ipmi_sel_ctx_t ctx,
                                       struct ipmi_sel_entry *sel_entry,
                                       uint8_t sel_record_type,
                                       char *tmpbuf,
                                       unsigned int tmpbuflen,
                                       unsigned int flags,
                                       unsigned int *wlen,
                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUN_MICROSYSTEMS)
    {
      if ((ret = sel_string_output_sun_event_data3_threshold_oem (ctx,
								  sel_entry,
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
_output_oem_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
                                      struct ipmi_sel_entry *sel_entry,
                                      uint8_t sel_record_type,
                                      char *tmpbuf,
                                      unsigned int tmpbuflen,
                                      unsigned int flags,
                                      unsigned int *wlen,
                                      struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = sel_string_output_dell_event_data3_discrete_oem (ctx,
								  sel_entry,
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
      if ((ret = sel_string_output_intel_event_data3_discrete_oem (ctx,
								   sel_entry,
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
      if ((ret = sel_string_output_inventec_event_data3_discrete_oem (ctx,
								      sel_entry,
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
      if ((ret = sel_string_output_quanta_event_data3_discrete_oem (ctx,
								    sel_entry,
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
      if ((ret = sel_string_output_sun_event_data3_discrete_oem (ctx,
								 sel_entry,
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

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON)
    {
      if ((ret = sel_string_output_wistron_event_data3_discrete_oem (ctx,
								     sel_entry,
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
_output_oem_event_data3_class_oem (ipmi_sel_ctx_t ctx,
                                   struct ipmi_sel_entry *sel_entry,
                                   uint8_t sel_record_type,
                                   char *tmpbuf,
                                   unsigned int tmpbuflen,
                                   unsigned int flags,
                                   unsigned int *wlen,
                                   struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = sel_string_output_dell_event_data3_class_oem (ctx,
							       sel_entry,
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
      if ((ret = sel_string_output_intel_event_data3_class_oem (ctx,
								sel_entry,
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
      if ((ret = sel_string_output_inventec_event_data3_class_oem (ctx,
								   sel_entry,
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
      if ((ret = sel_string_output_quanta_event_data3_class_oem (ctx,
								 sel_entry,
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
_output_event_data3 (ipmi_sel_ctx_t ctx,
                     struct ipmi_sel_entry *sel_entry,
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
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
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

          if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data3_threshold_oem (ctx,
                                                                sel_entry,
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

          if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
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
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
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

          if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
            {
              if ((ret = _output_oem_event_data3_discrete_oem (ctx,
                                                               sel_entry,
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

          if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
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
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
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

      if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
        {
          if ((ret = _output_oem_event_data3_class_oem (ctx,
                                                        sel_entry,
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
     
      if (system_event_record_data.event_data3 == IPMI_SEL_RECORD_UNSPECIFIED_EVENT
	  || system_event_record_data.event_data3_flag == IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE)
        {
          no_output_flag++;
          break;
        }

      if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
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
      if (sel_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
        return (0);

      if (no_output_flag)
        {
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (sel_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_STRING_FLAGS_VERBOSE)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "Event Data3 = %02Xh (Event Type Code = %02Xh)",
				   system_event_record_data.event_data3,
				   system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (sel_string_snprintf (buf,
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
_output_oem_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
                                     struct ipmi_sel_entry *sel_entry,
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
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL)
    {
      if ((ret = sel_string_output_dell_event_data2_event_data3 (ctx,
								 sel_entry,
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
      if ((ret = sel_string_output_fujitsu_event_data2_event_data3 (ctx,
								    sel_entry,
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
      if ((ret = sel_string_output_intel_event_data2_event_data3 (ctx,
								  sel_entry,
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
      if ((ret = sel_string_output_inventec_event_data2_event_data3 (ctx,
								     sel_entry,
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

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON)
    {
      if ((ret = sel_string_output_wistron_event_data2_event_data3 (ctx,
								    sel_entry,
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
_output_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
                                 struct ipmi_sel_entry *sel_entry,
                                 uint8_t sel_record_type,
                                 char *buf,
                                 unsigned int buflen,
                                 unsigned int flags,
                                 unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbufdata2[EVENT_BUFFER_LENGTH + 1];
  char tmpbufdata3[EVENT_BUFFER_LENGTH + 1];
  unsigned int tmpbufdata2_wlen = 0;
  unsigned int tmpbufdata3_wlen = 0;
  int data2_ret;
  int data3_ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
    return (-1);

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
					    sel_entry,
					    sel_record_type,
					    tmpbufdata3,
					    EVENT_BUFFER_LENGTH,
					    flags,
					    &tmpbufdata3_wlen)) < 0)
	return (-1);
      
      if (data3_ret)
	{
	  SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
	  return (-1);
	}

      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s",
			       tmpbufdata3))
        return (1);
      return (0);
    }

  if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
    {
      int ret;
      int oem_rv = 0;

      if ((ret = _output_oem_event_data2_event_data3 (ctx,
                                                      sel_entry,
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
                                        sel_entry,
                                        sel_record_type,
                                        tmpbufdata2,
                                        EVENT_BUFFER_LENGTH,
                                        flags,
                                        &tmpbufdata2_wlen)) < 0)
    return (-1);

  if ((data3_ret = _output_event_data3 (ctx,
                                        sel_entry,
                                        sel_record_type,
                                        tmpbufdata3,
                                        EVENT_BUFFER_LENGTH,
                                        flags,
                                        &tmpbufdata3_wlen)) < 0)
    return (-1);

  if (data2_ret || data3_ret)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((strlen (tmpbufdata2) 
       && strcasecmp (tmpbufdata2, NA_STRING))
      && (strlen (tmpbufdata3)
          && strcasecmp (tmpbufdata3, NA_STRING)))
    {
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s%s%s",
			       tmpbufdata2,
			       ctx->separator ? ctx->separator : IPMI_SEL_SEPARATOR_STRING,
			       tmpbufdata3))
        return (1);
    }
  else if (strlen (tmpbufdata2)
           && strcasecmp (tmpbufdata2, NA_STRING))
    {
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s",
			       tmpbufdata2))
        return (1);
    }
  else if (strlen (tmpbufdata3)
           && strcasecmp (tmpbufdata3, NA_STRING))
    {
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s",
			       tmpbufdata3))
        return (1);
    }
  else if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
    {
      if (sel_string_snprintf (buf,
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
_output_event_data2_previous_state_or_severity (ipmi_sel_ctx_t ctx,
                                                struct ipmi_sel_entry *sel_entry,
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
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
    return (-1);

  if (_get_previous_state_or_severity (ctx,
				       sel_entry,
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
      if (flags & IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD)
        {
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (sel_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
              return (0);
            }
          return (0);
        }
      ctx->errnum = IPMI_SEL_ERR_INVALID_SEL_ENTRY;
      return (-1);
    }

 out:
  if (output_flag)
    {
      if (sel_string_snprintf (buf, buflen, wlen, "%s", tmpbuf))
        return (1);
    }
  else
    {
      if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
        return (0);

      if (no_output_flag)
        {
          if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
            {
              if (sel_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
                return (1);
            }
          return (0);
        }

      if (flags & IPMI_SEL_STRING_FLAGS_VERBOSE)
        {
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "Event Data2 = %02Xh (Event Type Code = %02Xh)",
				   system_event_record_data.event_data2,
				   system_event_record_data.event_type_code))
            return (1);
        }
      else
        {
          if (sel_string_snprintf (buf,
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
_output_event_data2_previous_state (ipmi_sel_ctx_t ctx,
                                    struct ipmi_sel_entry *sel_entry,
                                    uint8_t sel_record_type,
                                    char *buf,
                                    unsigned int buflen,
                                    unsigned int flags,
                                    unsigned int *wlen)
{
  return (_output_event_data2_previous_state_or_severity (ctx,
                                                          sel_entry,
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
_output_event_data2_severity (ipmi_sel_ctx_t ctx,
                              struct ipmi_sel_entry *sel_entry,
                              uint8_t sel_record_type,
                              char *buf,
                              unsigned int buflen,
                              unsigned int flags,
                              unsigned int *wlen)
{
  return (_output_event_data2_previous_state_or_severity (ctx,
                                                          sel_entry,
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
_output_event_data1_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
					     struct ipmi_sel_entry *sel_entry,
					     uint8_t sel_record_type,
					     char *buf,
					     unsigned int buflen,
					     unsigned int flags,
					     unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char tmpbufeventdata1[EVENT_BUFFER_LENGTH + 1];
  char tmpbufeventdata23[EVENT_BUFFER_LENGTH + 1];
  unsigned int tmpbufeventdata1_wlen = 0;
  unsigned int tmpbufeventdata23_wlen = 0;
  int data1_ret;
  int data23_ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
    return (-1);

  memset (tmpbufeventdata1, '\0', EVENT_BUFFER_LENGTH+1);
  memset (tmpbufeventdata23, '\0', EVENT_BUFFER_LENGTH+1);

  if ((data1_ret = _output_event_data1 (ctx,
					sel_entry,
					sel_record_type,
					tmpbufeventdata1,
					EVENT_BUFFER_LENGTH,
					flags,
					&tmpbufeventdata1_wlen)) < 0)
    return (-1);

  if ((data23_ret = _output_event_data2_event_data3 (ctx,
						     sel_entry,
						     sel_record_type,
						     tmpbufeventdata23,
						     EVENT_BUFFER_LENGTH,
						     flags,
						     &tmpbufeventdata23_wlen)) < 0)
    return (-1);
  
  if (data1_ret || data23_ret)
    {
      SEL_SET_ERRNUM (ctx, IPMI_SEL_ERR_INTERNAL_ERROR);
      return (-1);
    }
  
  if ((strlen (tmpbufeventdata1) 
       && strcasecmp (tmpbufeventdata1, NA_STRING))
      && (strlen (tmpbufeventdata23)
          && strcasecmp (tmpbufeventdata23, NA_STRING)))
    {
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s%s%s",
			       tmpbufeventdata1,
			       ctx->separator ? ctx->separator : IPMI_SEL_SEPARATOR_STRING,
			       tmpbufeventdata23))
        return (1);
    }
  else if (strlen (tmpbufeventdata1)
           && strcasecmp (tmpbufeventdata1, NA_STRING))
    {
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s",
			       tmpbufeventdata1))
        return (1);
    }
  else if (strlen (tmpbufeventdata23)
           && strcasecmp (tmpbufeventdata23, NA_STRING))
    {
      /* Having event_data1 as N/A and event data 2+3 not N/A is
       * probably impossible, but handle it anyway.
       */
      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s",
			       tmpbufeventdata23))
        return (1);
    }
  else if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
    {
      if (sel_string_snprintf (buf,
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
_output_event_direction (ipmi_sel_ctx_t ctx,
                         struct ipmi_sel_entry *sel_entry,
                         uint8_t sel_record_type,
                         char *buf,
                         unsigned int buflen,
                         unsigned int flags,
                         unsigned int *wlen)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  char *str = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_system_event_record (ctx, sel_entry, &system_event_record_data) < 0)
    return (-1);

  if (system_event_record_data.event_direction)
    str = DEASSERTION_EVENT;
  else
    str = ASSERTION_EVENT;

  if (sel_string_snprintf (buf, buflen, wlen, "%s", str))
    return (1);

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_manufacturer_id (ipmi_sel_ctx_t ctx,
                         struct ipmi_sel_entry *sel_entry,
                         uint8_t sel_record_type,
                         char *buf,
                         unsigned int buflen,
                         unsigned int flags,
                         unsigned int *wlen)
{
  uint32_t manufacturer_id;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (sel_get_manufacturer_id (ctx, sel_entry, &manufacturer_id) < 0)
    return (-1);

  if (flags & IPMI_SEL_STRING_FLAGS_LEGACY)
    {
      if (sel_string_snprintf (buf, buflen, wlen, "Manufacturer ID = %02Xh", manufacturer_id))
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
          if (sel_string_snprintf (buf,
				   buflen,
				   wlen,
				   "Manufacturer ID = %s (%02Xh)",
				   iana_buf,
				   manufacturer_id))
            return (1);

        }
      else
        {
          if (sel_string_snprintf (buf,
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
_output_oem_interpreted_record_data (ipmi_sel_ctx_t ctx,
                                     struct ipmi_sel_entry *sel_entry,
                                     uint8_t sel_record_type,
                                     char *tmpbuf,
                                     unsigned int tmpbuflen,
                                     unsigned int flags,
                                     unsigned int *wlen,
                                     int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (oem_rv);

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU)
    {
      if ((ret = sel_string_output_fujitsu_oem_record_data (ctx,
							    sel_entry,
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

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if ((ret = sel_string_output_intel_oem_record_data (ctx,
							  sel_entry,
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
_output_oem_record_data (ipmi_sel_ctx_t ctx,
                         struct ipmi_sel_entry *sel_entry,
                         uint8_t sel_record_type,
                         char *buf,
                         unsigned int buflen,
                         unsigned int flags,
                         unsigned int *wlen)
{
  uint8_t oem_data[SEL_BUFFER_LENGTH];
  int oem_len;
  int oem_index;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);

  if (ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && ipmi_sel_record_type_class (sel_record_type) != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    return (_invalid_sel_entry_common (ctx, buf, buflen, flags, wlen));

  if (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA)
    {
      int ret;
      int oem_rv = 0;
      
      if ((ret = _output_oem_interpreted_record_data (ctx,
                                                      sel_entry,
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

  if ((oem_len = sel_get_oem (ctx, sel_entry, oem_data, SEL_BUFFER_LENGTH)) < 0)
    return (-1);

  if (flags & IPMI_SEL_STRING_FLAGS_VERBOSE)
    {
      if (sel_string_snprintf (buf, buflen, wlen, "OEM defined (Record Type = %02Xh) = ", sel_record_type))
	return (1);
    }
  else
    {
      if (sel_string_snprintf (buf, buflen, wlen, "OEM defined = "))
	return (1);
    }
  
  for (oem_index = 0; oem_index < oem_len; oem_index++)
    {
      if (oem_index)
        {
          if (sel_string_snprintf (buf, buflen, wlen, " "))
            return (1);
        }
      if (sel_string_snprintf (buf, buflen, wlen, "%02Xh", oem_data[oem_index]))
        return (1);
    }

  return (0);
}

/* return (0) - continue on
 * return (1) - buffer full, return full buffer to user
 * return (-1) - error, cleanup and return error
 */
static int
_output_oem_string (ipmi_sel_ctx_t ctx,
                    struct ipmi_sel_entry *sel_entry,
                    uint8_t sel_record_type,
                    char *buf,
                    unsigned int buflen,
                    unsigned int flags,
                    unsigned int *wlen)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);
  
  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU)
    {
      int oem_rv = 0;
      
      if ((ret = sel_string_output_fujitsu_oem_string (ctx,
						       sel_entry,
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
  
  if (flags & IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE)
    {
      if (sel_string_snprintf (buf, buflen, wlen, "%s", NA_STRING))
        return (1);
    }
  
  return (0);
}

int
sel_format_record_string (ipmi_sel_ctx_t ctx,
			  const char *fmt,
			  const void *sel_record,
			  unsigned int sel_record_len,
			  char *buf,
			  unsigned int buflen,
			  unsigned int flags)
{
  struct ipmi_sel_entry sel_entry;
  uint16_t record_id;
  uint8_t sel_record_type;
  int percent_flag = 0;
  unsigned int wlen = 0;
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (fmt);
  assert (sel_record);
  assert (sel_record_len >= IPMI_SEL_RECORD_LENGTH);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));

  memcpy (sel_entry.sel_event_record, sel_record, IPMI_SEL_RECORD_LENGTH);
  sel_entry.sel_event_record_len = IPMI_SEL_RECORD_LENGTH;

  if (sel_get_record_header_info (ctx,
				  &sel_entry,
				  &record_id,
				  &sel_record_type) < 0)
    goto cleanup;

  while (*fmt)
    {
      if (*fmt == '%')
        {
          if (percent_flag)
            {
              if (sel_string_snprintf (buf, buflen, &wlen, "%%"))
                goto out;
              percent_flag = 0;
            }
          else
            percent_flag = 1;
          goto end_loop;
        }
      else if (percent_flag && *fmt == 'i') /* record id */
        {
          if (sel_string_snprintf (buf, buflen, &wlen, "%u", record_id))
            goto out;
          percent_flag = 0;
        }
      else if (percent_flag && *fmt == 'I') /* event interpretation */
	{
          if ((ret = _output_event_interpretation (ctx,
						   &sel_entry,
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
      else if (percent_flag && *fmt == 't') /* time */
        {
          if ((ret = _output_time (ctx,
                                   &sel_entry,
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
                                   &sel_entry,
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
                                          &sel_entry,
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
                                          &sel_entry,
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
                                          &sel_entry,
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
                                          &sel_entry,
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
                                          &sel_entry,
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
                                                      &sel_entry,
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
                                                         &sel_entry,
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
      else if (percent_flag && *fmt == 'S') /* event data3 severity */
        {
          if ((ret = _output_event_data2_severity (ctx,
                                                   &sel_entry,
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
      else if (percent_flag && *fmt == 'E') /* combined event data 1, 2, and 3 string */
	{
	  if ((ret = _output_event_data1_event_data2_event_data3 (ctx,
								  &sel_entry,
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
                                              &sel_entry,
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
                                              &sel_entry,
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
                                              &sel_entry,
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
                                         &sel_entry,
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
              if (sel_string_snprintf (buf, buflen, &wlen, "%%%c", *fmt))
                goto out;
              percent_flag = 0;
            }
          else
            {
              if (sel_string_snprintf (buf, buflen, &wlen, "%c", *fmt))
                goto out;
            }
        }

    end_loop:
      fmt++;
    }

 out:
  rv = wlen;
  ctx->errnum = IPMI_SEL_ERR_SUCCESS;
 cleanup:
  return (rv);
}
