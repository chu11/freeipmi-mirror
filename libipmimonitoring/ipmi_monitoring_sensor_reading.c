/*****************************************************************************\
 *  $Id: ipmi_monitoring_sensor_reading.c,v 1.94 2010-07-22 21:49:00 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_bitmasks.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_ipmi_communication.h"
#include "ipmi_monitoring_parse_common.h"
#include "ipmi_monitoring_sensor_reading.h"

#include "freeipmi-portability.h"

#define IPMI_MONITORING_SENSORS_OK_STRING "OK"

static void
_sensor_reading_cleanup (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  ipmi_sensor_read_ctx_destroy (c->sensor_read_ctx);
  c->sensor_read_ctx = NULL;
}

int
ipmi_monitoring_sensor_reading_init (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->ipmi_ctx);
  assert (!c->sensor_read_ctx);

  if (!(c->sensor_read_ctx = ipmi_sensor_read_ctx_create (c->ipmi_ctx)))
    {
      IPMI_MONITORING_DEBUG (("ipmi_sensor_read_ctx_create: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  return (0);

 cleanup:
  _sensor_reading_cleanup (c);
  return (-1);
}

int
ipmi_monitoring_sensor_reading_cleanup (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  _sensor_reading_cleanup (c);
  return (0);
}

static struct ipmi_monitoring_sensor_reading *
_allocate_sensor_reading (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (!(s = (struct ipmi_monitoring_sensor_reading *)malloc (sizeof (struct ipmi_monitoring_sensor_reading))))
    {
      IPMI_MONITORING_DEBUG (("malloc: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      return (NULL);
    }
  memset (s, '\0', sizeof (struct ipmi_monitoring_sensor_reading));
  return (s);
}

static int
_store_sensor_reading (ipmi_monitoring_ctx_t c,
                       unsigned int sensor_reading_flags,
                       int record_id,
                       int sensor_number,
                       int sensor_type,
                       char *sensor_name,
                       int sensor_state,
                       int sensor_units,
                       int sensor_reading_type,
                       int sensor_bitmask_type,
                       uint16_t sensor_bitmask,
                       char **sensor_bitmask_strings,
                       void *sensor_reading,
                       int event_reading_type_code)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_MONITORING_SENSOR_TYPE_VALID (sensor_type));
  assert (sensor_name);
  assert (IPMI_MONITORING_STATE_VALID (sensor_state));
  assert (IPMI_MONITORING_SENSOR_UNITS_VALID (sensor_units));
  assert (IPMI_MONITORING_SENSOR_READING_TYPE_VALID (sensor_reading_type));
  assert (IPMI_MONITORING_SENSOR_BITMASK_TYPE_VALID (sensor_bitmask_type));

  if ((sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS)
      && sensor_state == IPMI_MONITORING_STATE_UNKNOWN)
    return (0);

  if (!(s = _allocate_sensor_reading (c)))
    goto cleanup;

  s->record_id = record_id;
  s->sensor_number = sensor_number;
  s->sensor_type = sensor_type;
  strncpy (s->sensor_name, sensor_name, IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);
  s->sensor_state = sensor_state;
  s->sensor_units = sensor_units;
  s->sensor_reading_type = sensor_reading_type;
  s->sensor_bitmask_type = sensor_bitmask_type;
  s->sensor_bitmask = sensor_bitmask;
  s->sensor_bitmask_strings = sensor_bitmask_strings;

  if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
    s->sensor_reading.bool_val = *((uint8_t *)sensor_reading);
  else if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
    s->sensor_reading.integer_val = *((uint32_t *)sensor_reading);
  else if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
    s->sensor_reading.double_val = *((double *)sensor_reading);

  s->event_reading_type_code = event_reading_type_code;

  /* achu: should come before list_append to avoid having a freed entry on the list */
  if (c->callback)
    {
      c->callback_sensor_reading = s;
      if ((*c->callback)(c, c->callback_data) < 0)
        {
          IPMI_MONITORING_DEBUG (("callback error"));
          c->errnum = IPMI_MONITORING_ERR_CALLBACK_ERROR;
          goto cleanup;
        }
    }

  if (!list_append (c->sensor_readings, s))
    {
      IPMI_MONITORING_DEBUG (("list_append: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  return (0);

 cleanup:
  free (s);
  return (-1);
}

static int
_store_unreadable_sensor_reading (ipmi_monitoring_ctx_t c,
                                  unsigned int sensor_reading_flags,
                                  int record_id,
                                  int sensor_number,
                                  int sensor_type,
                                  char *sensor_name,
                                  int sensor_units,
                                  int event_reading_type_code)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_MONITORING_SENSOR_TYPE_VALID (sensor_type));
  assert (IPMI_MONITORING_SENSOR_UNITS_VALID (sensor_units));

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS)
    return (0);

  if (!(s = _allocate_sensor_reading (c)))
    goto cleanup;

  s->record_id = record_id;
  s->sensor_number = sensor_number;
  s->sensor_type = sensor_type;
  if (sensor_name)
    strncpy (s->sensor_name, sensor_name, IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);
  s->sensor_state = IPMI_MONITORING_STATE_UNKNOWN;
  s->sensor_units = sensor_units;
  s->sensor_reading_type = IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN;
  s->sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;
  s->sensor_bitmask = 0;
  s->sensor_bitmask_strings = NULL;
  s->event_reading_type_code = event_reading_type_code;

  /* achu: should come before list_append to avoid having a freed entry on the list */
  if (c->callback)
    {
      c->callback_sensor_reading = s;
      if ((*c->callback)(c, c->callback_data) < 0)
        {
          IPMI_MONITORING_DEBUG (("callback error"));
          c->errnum = IPMI_MONITORING_ERR_CALLBACK_ERROR;
          goto cleanup;
        }
    }

  if (!list_append (c->sensor_readings, s))
    {
      IPMI_MONITORING_DEBUG (("list_append: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  return (0);

 cleanup:
  free (s);
  return (-1);
}

static int
_get_sensor_state (ipmi_monitoring_ctx_t c,
                   uint8_t event_reading_type_code,
                   uint8_t sensor_type,
                   uint16_t sensor_event_bitmask)
{
  unsigned int sensor_state;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  
  if (ipmi_interpret_sensor (c->interpret_ctx,
                             event_reading_type_code,
                             sensor_type,
                             sensor_event_bitmask,
                             &sensor_state) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_interpret_sensor: %s",
                              ipmi_interpret_ctx_errormsg (c->interpret_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }
  
  if (sensor_state == IPMI_INTERPRET_STATE_NOMINAL)
    rv = IPMI_MONITORING_STATE_NOMINAL;
  else if (sensor_state == IPMI_INTERPRET_STATE_WARNING)
    rv = IPMI_MONITORING_STATE_WARNING;
  else if (sensor_state == IPMI_INTERPRET_STATE_CRITICAL)
    rv = IPMI_MONITORING_STATE_CRITICAL;
  else if (sensor_state == IPMI_INTERPRET_STATE_UNKNOWN)
    rv = IPMI_MONITORING_STATE_UNKNOWN;
  return (rv);
}

static void
_sensor_read_ctx_error_convert (ipmi_monitoring_ctx_t c)
{
  int errnum;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->ipmi_ctx);
  assert (c->sensor_read_ctx);

  errnum = ipmi_sensor_read_ctx_errnum (c->sensor_read_ctx);

  if (errnum == IPMI_SENSOR_READ_ERR_NODE_BUSY)
    c->errnum = IPMI_MONITORING_ERR_BMC_BUSY;
  else if (errnum == IPMI_SENSOR_READ_ERR_IPMI_ERROR)
    c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
  else if (errnum == IPMI_SENSOR_READ_ERR_OUT_OF_MEMORY)
    c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
  else if (errnum == IPMI_SENSOR_READ_ERR_SYSTEM_ERROR)
    c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
  else
    c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
}

/*
 * return value -1 = error, 0 = unreadable sensor reading, 1 = sensor reading success
 */
static int
_get_sensor_reading (ipmi_monitoring_ctx_t c,
                     unsigned int sensor_reading_flags,
                     unsigned int shared_sensor_number_offset,
                     double *sensor_reading,
                     uint16_t *sensor_event_bitmask)
{
  uint8_t sdr_record[IPMI_SDR_MAX_RECORD_LENGTH];
  int sdr_record_len;
  double *l_sensor_reading = NULL;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (sensor_reading);
  assert (sensor_event_bitmask);

  memset (sdr_record, '\0', IPMI_SDR_MAX_RECORD_LENGTH);
  if ((sdr_record_len = ipmi_sdr_cache_record_read (c->sdr_ctx,
						    sdr_record,
						    IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_record_read: %s", ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
  
  if (ipmi_sensor_read (c->sensor_read_ctx,
                        sdr_record,
                        sdr_record_len,
                        shared_sensor_number_offset,
                        NULL,
                        &l_sensor_reading,
                        sensor_event_bitmask) <= 0)
    {
      int errnum = ipmi_sensor_read_ctx_errnum (c->sensor_read_ctx);

      IPMI_MONITORING_DEBUG (("ipmi_sensor_read: %s", ipmi_sensor_read_ctx_errormsg (c->sensor_read_ctx)));
      if (errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_ANALOG
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_LINEAR
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_READING_UNAVAILABLE
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_SCANNING_DISABLED
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_ANALOG
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_LINEAR
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NOT_OWNED_BY_BMC
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_CANNOT_BE_BRIDGED
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_IS_SYSTEM_SOFTWARE
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_READING_CANNOT_BE_OBTAINED)
        {
          rv = 0;
          goto cleanup;
        }

      _sensor_read_ctx_error_convert (c);
      goto cleanup;
    }

  if (l_sensor_reading)
    {
      (*sensor_reading) = (*l_sensor_reading);
      free (l_sensor_reading);
    }

  rv = 1;
 cleanup:
  return (rv);
}

static int
_get_sensor_units (ipmi_monitoring_ctx_t c,
                   uint8_t sensor_units_percentage,
                   uint8_t sensor_units_modifier,
                   uint8_t sensor_units_rate,
                   uint8_t sensor_base_unit_type,
                   uint8_t sensor_modifier_unit_type)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (sensor_units_percentage == IPMI_SDR_PERCENTAGE_YES
      && sensor_units_modifier == IPMI_SDR_MODIFIER_UNIT_NONE
      && sensor_units_rate == IPMI_SENSOR_RATE_UNIT_NONE
      && sensor_base_unit_type == IPMI_SENSOR_UNIT_UNSPECIFIED)
    return (IPMI_MONITORING_SENSOR_UNITS_PERCENT);

  if (sensor_units_percentage != IPMI_SDR_PERCENTAGE_NO)
    {
      IPMI_MONITORING_DEBUG (("sensor_units_percentage 'yes' not supported"));
      return (IPMI_MONITORING_SENSOR_UNITS_UNKNOWN);
    }

  if (sensor_units_modifier != IPMI_SDR_MODIFIER_UNIT_NONE)
    {
      IPMI_MONITORING_DEBUG (("sensor_units_modifier '0x%X' not supported", sensor_units_modifier));
      return (IPMI_MONITORING_SENSOR_UNITS_UNKNOWN);
    }

  if (sensor_units_rate != IPMI_SENSOR_RATE_UNIT_NONE)
    {
      IPMI_MONITORING_DEBUG (("sensor_units_rate '0x%X' not supported", sensor_units_rate));
      return (IPMI_MONITORING_SENSOR_UNITS_UNKNOWN);
    }

  if (sensor_modifier_unit_type != IPMI_SENSOR_UNIT_UNSPECIFIED)
    {
      IPMI_MONITORING_DEBUG (("sensor_modifier_unit_type '0x%X' not supported", sensor_modifier_unit_type));
      return (IPMI_MONITORING_SENSOR_UNITS_UNKNOWN);
    }

  switch (sensor_base_unit_type)
    {
    case IPMI_SENSOR_UNIT_DEGREES_C:
      return (IPMI_MONITORING_SENSOR_UNITS_CELSIUS);
    case IPMI_SENSOR_UNIT_DEGREES_F:
      return (IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT);
    case IPMI_SENSOR_UNIT_VOLTS:
      return (IPMI_MONITORING_SENSOR_UNITS_VOLTS);
    case IPMI_SENSOR_UNIT_AMPS:
      return (IPMI_MONITORING_SENSOR_UNITS_AMPS);
    case IPMI_SENSOR_UNIT_RPM:
      return (IPMI_MONITORING_SENSOR_UNITS_RPM);
    case IPMI_SENSOR_UNIT_WATTS:
      return (IPMI_MONITORING_SENSOR_UNITS_WATTS);
    }

  IPMI_MONITORING_DEBUG (("sensor_base_unit_type '0x%X' not supported", sensor_base_unit_type));
  return (IPMI_MONITORING_SENSOR_UNITS_UNKNOWN);
}

static int
_get_sensor_bitmask_strings (ipmi_monitoring_ctx_t c,
                             unsigned int sensor_reading_flags,
                             uint8_t event_reading_type_code,
                             uint8_t sensor_type,
			     uint8_t sensor_number,
                             uint16_t sensor_event_bitmask,
                             char ***sensor_bitmask_strings)
{
  char **tmp_sensor_bitmask_strings = NULL;
  unsigned int tmp_sensor_bitmask_strings_count = 0;
  uint32_t manufacturer_id = 0;
  uint16_t product_id = 0;
  unsigned int flags;
  unsigned int i;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (sensor_bitmask_strings);
  
  flags = IPMI_GET_EVENT_MESSAGES_FLAGS_SENSOR_READING;
  
  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_INTERPRET_OEM_DATA)
    {
      manufacturer_id = c->manufacturer_id;
      product_id = c->product_id;
      flags |= IPMI_GET_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA;
    }

  if (ipmi_get_event_messages (event_reading_type_code,
                               sensor_type,
			       sensor_number,
                               sensor_event_bitmask,
                               manufacturer_id,
                               product_id,
                               &tmp_sensor_bitmask_strings,
                               &tmp_sensor_bitmask_strings_count,
                               IPMI_MONITORING_SENSORS_OK_STRING,
                               flags) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_get_event_messages: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  (*sensor_bitmask_strings) = tmp_sensor_bitmask_strings;

  rv = 0;
 cleanup:
  if (rv < 0)
    {
      if (tmp_sensor_bitmask_strings)
        {
          for (i = 0; i < tmp_sensor_bitmask_strings_count; i++)
            free (tmp_sensor_bitmask_strings[i]);
          free (tmp_sensor_bitmask_strings);
        }
    }
  return (rv);
}

static int
_threshold_sensor_reading (ipmi_monitoring_ctx_t c,
                           unsigned int sensor_reading_flags,
                           uint16_t record_id,
                           int sensor_number_base,
                           unsigned int shared_sensor_number_offset,
                           uint8_t event_reading_type_code,
                           uint8_t sdr_sensor_type,
                           int sensor_type,
                           char *sensor_name)
{
  uint8_t sensor_units_percentage;
  uint8_t sensor_units_modifier;
  uint8_t sensor_units_rate;
  uint8_t sensor_base_unit_type;
  uint8_t sensor_modifier_unit_type;
  double sensor_reading;
  uint16_t sensor_event_bitmask;
  char **sensor_bitmask_strings = NULL;
  int sensor_units;
  int sensor_state;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_MONITORING_SENSOR_TYPE_VALID (sensor_type));
  assert (sensor_name);

  if (ipmi_sdr_parse_sensor_units (c->sdr_ctx,
				   NULL,
				   0,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_units: %s",
                              ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if ((sensor_units = _get_sensor_units (c,
                                         sensor_units_percentage,
                                         sensor_units_modifier,
                                         sensor_units_rate,
                                         sensor_base_unit_type,
                                         sensor_modifier_unit_type)) < 0)
    return (-1);

  if ((ret = _get_sensor_reading (c,
                                  sensor_reading_flags,
                                  shared_sensor_number_offset,
                                  &sensor_reading,
                                  &sensor_event_bitmask)) < 0)
    return (-1);

  if (!ret)
    {
      IPMI_MONITORING_DEBUG (("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_number_base + shared_sensor_number_offset,
                                            sensor_type,
                                            sensor_name,
                                            sensor_units,
                                            event_reading_type_code) < 0)
        return (-1);
      return (0);
    }

  if ((sensor_state = _get_sensor_state (c,
                                         event_reading_type_code,
                                         sdr_sensor_type,
                                         sensor_event_bitmask)) < 0)
    return (-1);

  if (_get_sensor_bitmask_strings (c,
                                   sensor_reading_flags,
                                   event_reading_type_code,
                                   sdr_sensor_type,
				   sensor_number_base + shared_sensor_number_offset,
                                   sensor_event_bitmask,
                                   &sensor_bitmask_strings) < 0)
    return (-1);

  if (_store_sensor_reading (c,
                             sensor_reading_flags,
                             record_id,
                             sensor_number_base + shared_sensor_number_offset,
                             sensor_type,
                             sensor_name,
                             sensor_state,
                             sensor_units,
                             IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE,
                             IPMI_MONITORING_SENSOR_BITMASK_TYPE_THRESHOLD,
                             sensor_event_bitmask,
                             sensor_bitmask_strings,
                             &sensor_reading,
                             event_reading_type_code) < 0)
    return (-1);

  return (0);
}

static int
_get_sensor_bitmask_type (ipmi_monitoring_ctx_t c,
			  uint8_t event_reading_type_code,
			  uint8_t sdr_sensor_type)
{
  int sensor_bitmask_type;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);

  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_THRESHOLD)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_THRESHOLD;
  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_STATE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_STATE;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_STATE;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_PREDICTIVE_FAILURE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PREDICTIVE_FAILURE;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_LIMIT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_LIMIT;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_PERFORMANCE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PERFORMANCE;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_SEVERITY;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_DEVICE_PRESENT;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_DEVICE_ENABLED;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_AVAILABILITY)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_AVAILABILITY;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_REDUNDANCY;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_ACPI_POWER_STATE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_ACPI_POWER_STATE;
  else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC)
    {
      if (sdr_sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PHYSICAL_SECURITY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PROCESSOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_POWER_SUPPLY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_POWER_UNIT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_COOLING_DEVICE)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_COOLING_DEVICE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_OTHER_UNITS_BASED_SENSOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MEMORY)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MEMORY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_DRIVE_SLOT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_POST_MEMORY_RESIZE)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_POST_MEMORY_RESIZE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_FIRMWARE_PROGRESS;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_EVENT_LOGGING_DISABLED;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_WATCHDOG1)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_WATCHDOG1;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_EVENT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_CRITICAL_INTERRUPT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_BUTTON_SWITCH;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MODULE_BOARD;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MICROCONTROLLER_COPROCESSOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_ADD_IN_CARD)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_ADD_IN_CARD;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CHASSIS)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_CHASSIS;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CHIP_SET)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_CHIP_SET;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OTHER_FRU)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_OTHER_FRU;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_CABLE_INTERCONNECT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_TERMINATOR)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_TERMINATOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_BOOT_INITIATED;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_BOOT_ERROR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OS_BOOT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_OS_BOOT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_OS_CRITICAL_STOP;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SLOT_CONNECTOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_ACPI_POWER_STATE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_WATCHDOG2;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_PLATFORM_ALERT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PLATFORM_ALERT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_ENTITY_PRESENCE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MONITOR_ASIC_IC)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MONITOR_ASIC_IC;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_LAN)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_LAN;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_BATTERY)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_BATTERY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SESSION_AUDIT)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SESSION_AUDIT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_VERSION_CHANGE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
	sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_FRU_STATE;
      /* To avoid gcc warnings, subtract -1 in comparison */
      else if (sdr_sensor_type >= IPMI_SENSOR_TYPE_OEM_MIN
	       && (sdr_sensor_type - 1) <= (IPMI_SENSOR_TYPE_OEM_MAX - 1))
	{
	  IPMI_MONITORING_DEBUG (("sensor_type '0x%X' bitmask is OEM", sdr_sensor_type));
	  sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_OEM;
	}
      else
	{
	  IPMI_MONITORING_DEBUG (("sensor_type '0x%X' bitmask not supported", sdr_sensor_type));
	  sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;
	}
    }
  else if (event_reading_type_code >= IPMI_EVENT_READING_TYPE_CODE_OEM_MIN
	   && event_reading_type_code <= IPMI_EVENT_READING_TYPE_CODE_OEM_MAX)
    {
      IPMI_MONITORING_DEBUG (("event_reading_type_code '0x%X' bitmask is OEM",
                              event_reading_type_code));
      sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_OEM;
    }
  else
    {
      IPMI_MONITORING_DEBUG (("event_reading_type_code '0x%X' bitmask not supported",
                              event_reading_type_code));
      sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;
    }

  return (sensor_bitmask_type);
}

static int
_digital_sensor_reading (ipmi_monitoring_ctx_t c,
                         unsigned int sensor_reading_flags,
                         uint16_t record_id,
                         uint8_t sensor_number_base,
                         unsigned int shared_sensor_number_offset,
                         uint8_t event_reading_type_code,
                         uint8_t sdr_sensor_type,
                         int sensor_type,
                         char *sensor_name)
{
  double sensor_reading;
  uint16_t sensor_event_bitmask;
  char **sensor_bitmask_strings = NULL;
  int sensor_state;
  int sensor_bitmask_type;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code));
  assert (IPMI_MONITORING_SENSOR_TYPE_VALID (sensor_type));
  assert (sensor_name);

  if ((ret = _get_sensor_reading (c,
                                  sensor_reading_flags,
                                  shared_sensor_number_offset,
                                  &sensor_reading,
                                  &sensor_event_bitmask)) < 0)
    return (-1);

  if (!ret)
    {
      IPMI_MONITORING_DEBUG (("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_number_base + shared_sensor_number_offset,
                                            sensor_type,
                                            sensor_name,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN,
                                            event_reading_type_code) < 0)
        return (-1);
      return (0);
    }

  if ((sensor_state = _get_sensor_state (c,
                                         event_reading_type_code,
                                         sdr_sensor_type,
                                         sensor_event_bitmask)) < 0)
    return (-1);

  if ((sensor_bitmask_type = _get_sensor_bitmask_type (c,
						       event_reading_type_code,
						       sdr_sensor_type)) < 0)
    return (-1);

  if (_get_sensor_bitmask_strings (c,
                                   sensor_reading_flags,
                                   event_reading_type_code,
                                   sdr_sensor_type,
				   sensor_number_base + shared_sensor_number_offset,
                                   sensor_event_bitmask,
                                   &sensor_bitmask_strings) < 0)
    return (-1);

  /* No actual sensor reading, only a sensor event bitmask */
  if (_store_sensor_reading (c,
                             sensor_reading_flags,
                             record_id,
                             sensor_number_base + shared_sensor_number_offset,
                             sensor_type,
                             sensor_name,
                             sensor_state,
                             IPMI_MONITORING_SENSOR_UNITS_NONE,
                             IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN,
                             sensor_bitmask_type,
                             sensor_event_bitmask,
                             sensor_bitmask_strings,
                             NULL,
                             event_reading_type_code) < 0)
    return (-1);

  return (0);
}

static int
_specific_sensor_reading (ipmi_monitoring_ctx_t c,
                          unsigned int sensor_reading_flags,
                          uint16_t record_id,
                          uint8_t sensor_number_base,
                          unsigned int shared_sensor_number_offset,
                          uint8_t event_reading_type_code,
                          uint8_t sdr_sensor_type,
                          int sensor_type,
                          char *sensor_name)
{
  double sensor_reading;
  uint16_t sensor_event_bitmask;
  char **sensor_bitmask_strings = NULL;
  int sensor_state;
  int sensor_bitmask_type;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (sensor_name);

  if ((ret = _get_sensor_reading (c,
                                  sensor_reading_flags,
                                  shared_sensor_number_offset,
                                  &sensor_reading,
                                  &sensor_event_bitmask)) < 0)
    return (-1);

  if (!ret)
    {
      IPMI_MONITORING_DEBUG (("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_number_base + shared_sensor_number_offset,
                                            sensor_type,
                                            sensor_name,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN,
                                            event_reading_type_code) < 0)
        return (-1);
      return (0);
    }

  if ((sensor_state = _get_sensor_state (c,
                                         event_reading_type_code,
                                         sdr_sensor_type,
                                         sensor_event_bitmask)) < 0)
    return (-1);

  if ((sensor_bitmask_type = _get_sensor_bitmask_type (c,
						       event_reading_type_code,
						       sdr_sensor_type)) < 0)
    return (-1);

  if (_get_sensor_bitmask_strings (c,
                                   sensor_reading_flags,
                                   event_reading_type_code,
                                   sdr_sensor_type,
				   sensor_number_base + shared_sensor_number_offset,
                                   sensor_event_bitmask,
                                   &sensor_bitmask_strings) < 0)
    return (-1);

  /* No actual sensor reading, only a sensor event bitmask */
  if (_store_sensor_reading (c,
                             sensor_reading_flags,
                             record_id,
                             sensor_number_base + shared_sensor_number_offset,
                             sensor_type,
                             sensor_name,
                             sensor_state,
                             IPMI_MONITORING_SENSOR_UNITS_NONE,
                             IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN,
                             sensor_bitmask_type,
                             sensor_event_bitmask,
                             sensor_bitmask_strings,
                             NULL,
                             event_reading_type_code) < 0)
    return (-1);

  return (0);
}

static int
_oem_sensor_reading (ipmi_monitoring_ctx_t c,
		     unsigned int sensor_reading_flags,
		     uint16_t record_id,
		     uint8_t sensor_number_base,
		     unsigned int shared_sensor_number_offset,
		     uint8_t event_reading_type_code,
		     uint8_t sdr_sensor_type,
		     int sensor_type,
		     char *sensor_name)
{
  double sensor_reading;
  uint16_t sensor_event_bitmask;
  char **sensor_bitmask_strings = NULL;
  int sensor_state;
  int sensor_bitmask_type;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (sensor_name);

  if ((ret = _get_sensor_reading (c,
                                  sensor_reading_flags,
                                  shared_sensor_number_offset,
                                  &sensor_reading,
                                  &sensor_event_bitmask)) < 0)
    return (-1);

  if (!ret)
    {
      IPMI_MONITORING_DEBUG (("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_number_base + shared_sensor_number_offset,
                                            sensor_type,
                                            sensor_name,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN,
                                            event_reading_type_code) < 0)
        return (-1);
      return (0);
    }

  if ((sensor_state = _get_sensor_state (c,
                                         event_reading_type_code,
                                         sdr_sensor_type,
                                         sensor_event_bitmask)) < 0)
    return (-1);

  if ((sensor_bitmask_type = _get_sensor_bitmask_type (c,
						       event_reading_type_code,
						       sdr_sensor_type)) < 0)
    return (-1);

  if (_get_sensor_bitmask_strings (c,
                                   sensor_reading_flags,
                                   event_reading_type_code,
                                   sdr_sensor_type,
				   sensor_number_base + shared_sensor_number_offset,
                                   sensor_event_bitmask,
                                   &sensor_bitmask_strings) < 0)
    return (-1);

  /* No actual sensor reading, only a sensor event bitmask */
  if (_store_sensor_reading (c,
                             sensor_reading_flags,
                             record_id,
                             sensor_number_base + shared_sensor_number_offset,
                             sensor_type,
                             sensor_name,
                             sensor_state,
                             IPMI_MONITORING_SENSOR_UNITS_NONE,
                             IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN,
                             sensor_bitmask_type,
                             sensor_event_bitmask,
                             sensor_bitmask_strings,
                             NULL,
                             event_reading_type_code) < 0)
    return (-1);

  return (0);
}

int
ipmi_monitoring_get_sensor_reading (ipmi_monitoring_ctx_t c,
                                    unsigned int sensor_reading_flags,
                                    unsigned int shared_sensor_number_offset,
                                    unsigned int *sensor_types,
                                    unsigned int sensor_types_len)
{
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  uint16_t record_id;
  uint8_t record_type;
  uint8_t sensor_number_base;
  uint8_t event_reading_type_code;
  uint8_t sdr_sensor_type;
  int sensor_type;
  int len;
  unsigned int sensor_name_flags = 0;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->ipmi_ctx);
  assert (c->sensor_read_ctx);
  assert (c->sensor_readings);
  assert (!sensor_types || sensor_types_len);

  if (ipmi_sdr_parse_record_id_and_type (c->sdr_ctx,
					 NULL,
					 0,
                                         &record_id,
                                         &record_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_record_id_and_type: %s",
                              ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    {
      IPMI_MONITORING_DEBUG (("record_type '0x%X' not supported", record_type));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            0,
                                            IPMI_MONITORING_SENSOR_TYPE_UNKNOWN,
                                            NULL,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN,
                                            0) < 0)
        return (-1);
      return (0);
    }

  if (ipmi_sdr_parse_sensor_number (c->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number_base) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_number: %s",
                              ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if (ipmi_sdr_parse_sensor_type (c->sdr_ctx,
				  NULL,
				  0,
                                  &sdr_sensor_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_type: %s",
                              ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if ((sensor_type = ipmi_monitoring_get_sensor_type (c, sdr_sensor_type)) < 0)
    return (-1);

  if (sensor_types)
    {
      unsigned int i, found = 0;

      for (i = 0; i < sensor_types_len; i++)
        {
          if (sensor_types[i] == sensor_type)
            {
              found++;
              break;
            }
        }

      if (!found)
        return (0);
    }

  memset (sensor_name, '\0', IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);

  if (!(sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS))
    sensor_name_flags |= IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS;
  
  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_ENTITY_SENSOR_NAMES)
    {
      if ((len = ipmi_sdr_parse_entity_sensor_name (c->sdr_ctx,
						    NULL,
						    0,
						    sensor_number_base + shared_sensor_number_offset,
						    sensor_name_flags,
						    sensor_name,
						    IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH)) < 0)
	{
	  IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_entity_sensor_name: %s",
				  ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
	  c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
	  return (-1);
	}
    }
  else
    {
      if ((len = ipmi_sdr_parse_sensor_name (c->sdr_ctx,
					     NULL,
					     0,
					     sensor_number_base + shared_sensor_number_offset,
					     sensor_name_flags,
					     sensor_name,
					     IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH)) < 0)
	{
	  IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_name: %s",
				  ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
	  c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
	  return (-1);
	}
    }

  if (len >= IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH)
    {
      IPMI_MONITORING_DEBUG (("sensor_name buffer short: len = %d", len));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if (ipmi_sdr_parse_event_reading_type_code (c->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_event_reading_type_code: %s",
                              ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if (IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      if (_threshold_sensor_reading (c,
                                     sensor_reading_flags,
                                     record_id,
                                     sensor_number_base,
                                     shared_sensor_number_offset,
                                     event_reading_type_code,
                                     sdr_sensor_type,
                                     sensor_type,
                                     sensor_name) < 0)
        return (-1);
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code))
    {
      if (_digital_sensor_reading (c,
                                   sensor_reading_flags,
                                   record_id,
                                   sensor_number_base,
                                   shared_sensor_number_offset,
                                   event_reading_type_code,
                                   sdr_sensor_type,
                                   sensor_type,
                                   sensor_name) < 0)
        return (-1);
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
    {
      if (_specific_sensor_reading (c,
                                    sensor_reading_flags,
                                    record_id,
                                    sensor_number_base,
                                    shared_sensor_number_offset,
                                    event_reading_type_code,
                                    sdr_sensor_type,
                                    sensor_type,
                                    sensor_name) < 0)
        return (-1);
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_OEM (event_reading_type_code))
    {
      if (_oem_sensor_reading (c,
			       sensor_reading_flags,
			       record_id,
			       sensor_number_base,
			       shared_sensor_number_offset,
			       event_reading_type_code,
			       sdr_sensor_type,
			       sensor_type,
			       sensor_name) < 0)
        return (-1);
    }
  else
    {
      IPMI_MONITORING_DEBUG (("event_reading_type_code '0x%X' not supported",
                              event_reading_type_code));

      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_number_base + shared_sensor_number_offset,
                                            sensor_type,
                                            sensor_name,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN,
                                            event_reading_type_code) < 0)
        return (-1);
    }

  return (0);
}
