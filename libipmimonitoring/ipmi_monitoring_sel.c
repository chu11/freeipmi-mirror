/*****************************************************************************\
 *  $Id: ipmi_monitoring_sel.c,v 1.2 2010-07-22 21:49:00 chu11 Exp $
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
#include "ipmi_monitoring_offsets.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_ipmi_communication.h"
#include "ipmi_monitoring_parse_common.h"
#include "ipmi_monitoring_sensor_reading.h"

#include "freeipmi-portability.h"

#define IPMI_MONITORING_SEL_EVENT_OFFSET_STRING_MAX 1024

struct sel_parse_data
{
  ipmi_monitoring_ctx_t c;
  unsigned int sel_flags;
  unsigned int *sensor_types;
  unsigned int sensor_types_len;
  unsigned int date_begin;
  unsigned int date_end;
};

static void
_sel_cleanup (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  ipmi_sel_ctx_destroy (c->sel_parse_ctx);
  c->sel_parse_ctx = NULL;
}

int
ipmi_monitoring_sel_init (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->ipmi_ctx);
  assert (c->sdr_ctx);
  assert (!c->sel_parse_ctx);

  if (!(c->sel_parse_ctx = ipmi_sel_ctx_create (c->ipmi_ctx, c->sdr_ctx)))
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_ctx_create: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  return (0);

 cleanup:
  _sel_cleanup (c);
  return (-1);
}

int
ipmi_monitoring_sel_cleanup (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  _sel_cleanup (c);
  return (0);
}

static int
_get_event_offset_type (ipmi_monitoring_ctx_t c,
                        uint8_t event_type_code,
                        uint8_t sdr_sensor_type)
{
  int event_offset_type;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sel_records);

  if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_THRESHOLD)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_THRESHOLD;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_STATE)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_TRANSITION_STATE;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_STATE;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_PREDICTIVE_FAILURE)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_PREDICTIVE_FAILURE;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_LIMIT)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_LIMIT;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_PERFORMANCE)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_PERFORMANCE;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_TRANSITION_SEVERITY;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_DEVICE_PRESENT;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_DEVICE_ENABLED;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_AVAILABILITY)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_TRANSITION_AVAILABILITY;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_REDUNDANCY;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_ACPI_POWER_STATE)
    event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_ACPI_POWER_STATE;
  else if (event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC)
    {
      if (sdr_sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_PHYSICAL_SECURITY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_PROCESSOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_POWER_SUPPLY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_POWER_UNIT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_COOLING_DEVICE)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_COOLING_DEVICE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_OTHER_UNITS_BASED_SENSOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MEMORY)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_MEMORY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_DRIVE_SLOT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_POST_MEMORY_RESIZE)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_POST_MEMORY_RESIZE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_SYSTEM_FIRMWARE_PROGRESS;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_EVENT_LOGGING_DISABLED;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_WATCHDOG1)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_WATCHDOG1;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_SYSTEM_EVENT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_CRITICAL_INTERRUPT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_BUTTON_SWITCH;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_MODULE_BOARD;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_MICROCONTROLLER_COPROCESSOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_ADD_IN_CARD)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_ADD_IN_CARD;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CHASSIS)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_CHASSIS;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CHIP_SET)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_CHIP_SET;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OTHER_FRU)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_OTHER_FRU;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_CABLE_INTERCONNECT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_TERMINATOR)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_TERMINATOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_SYSTEM_BOOT_INITIATED;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_BOOT_ERROR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OS_BOOT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_OS_BOOT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_OS_CRITICAL_STOP;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_SLOT_CONNECTOR;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_SYSTEM_ACPI_POWER_STATE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_WATCHDOG2;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_PLATFORM_ALERT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_PLATFORM_ALERT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_ENTITY_PRESENCE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MONITOR_ASIC_IC)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_MONITOR_ASIC_IC;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_LAN)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_LAN;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_BATTERY)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_BATTERY;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_SESSION_AUDIT)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_SESSION_AUDIT;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_VERSION_CHANGE;
      else if (sdr_sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
	event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_FRU_STATE;
      /* To avoid gcc warnings, subtract -1 in comparison */
      else if (sdr_sensor_type >= IPMI_SENSOR_TYPE_OEM_MIN
	       && (sdr_sensor_type - 1) <= (IPMI_SENSOR_TYPE_OEM_MAX - 1))
	{
	  IPMI_MONITORING_DEBUG (("sensor_type '0x%X' event offset is OEM", sdr_sensor_type));
	  event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_OEM;
	}
      else
	{
	  IPMI_MONITORING_DEBUG (("sensor_type '0x%X' event offset not supported", sdr_sensor_type));
	  event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_UNKNOWN;
	}
    }
  else if (event_type_code >= IPMI_EVENT_READING_TYPE_CODE_OEM_MIN
	   && event_type_code <= IPMI_EVENT_READING_TYPE_CODE_OEM_MAX)
    {
      IPMI_MONITORING_DEBUG (("event_type_code '0x%X' event offset is OEM",
                              event_type_code));
      event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_OEM;
    }
  else
    {
      IPMI_MONITORING_DEBUG (("event_type_code '0x%X' event offset not supported",
                              event_type_code));
      event_offset_type = IPMI_MONITORING_EVENT_OFFSET_TYPE_UNKNOWN;
    }

  return (event_offset_type);
}

static void
_sel_parse_ctx_error_convert (ipmi_monitoring_ctx_t c)
{
  int errnum;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->ipmi_ctx);
  assert (c->sel_parse_ctx);

  errnum = ipmi_sel_ctx_errnum (c->sel_parse_ctx);

  /* if callback error - assume we set as needed */
  if (errnum != IPMI_SEL_ERR_CALLBACK_ERROR)
    {
      if (errnum == IPMI_SEL_ERR_IPMI_ERROR)
        c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
      else if (errnum == IPMI_SEL_ERR_OUT_OF_MEMORY)
        c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      else if (errnum == IPMI_SEL_ERR_SYSTEM_ERROR)
        c->errnum = IPMI_MONITORING_ERR_SYSTEM_ERROR;
      else
        c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
    }
}

static int
_ipmi_monitoring_sel_parse_system_event_record (ipmi_monitoring_ctx_t c,
                                                struct ipmi_monitoring_sel_record *s,
						unsigned int sel_flags)
{
  uint32_t timestamp;
  uint8_t sel_sensor_type;
  uint8_t sensor_number;
  uint8_t event_direction;
  uint8_t event_offset;
  uint8_t event_type_code;
  uint8_t event_data1;
  uint8_t event_data2;
  uint8_t event_data3;
  char event_offset_string[IPMI_MONITORING_SEL_EVENT_OFFSET_STRING_MAX + 1];
  int sensor_type;
  unsigned int sel_string_flags;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (s);

  if (ipmi_sel_parse_read_timestamp (c->sel_parse_ctx,
				     NULL,
				     0,
				     &timestamp) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_timestamp: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->timestamp = timestamp;

  if (ipmi_sel_parse_read_sensor_type (c->sel_parse_ctx,
				       NULL,
				       0,
				       &sel_sensor_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_sensor_type: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }

  if ((sensor_type = ipmi_monitoring_get_sensor_type (c, sel_sensor_type)) < 0)
    return (-1);

  s->sensor_type = sensor_type;

  if (ipmi_sel_parse_read_sensor_number (c->sel_parse_ctx,
					 NULL,
					 0,
					 &sensor_number) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_sensor_number: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }

  s->sensor_number = sensor_number;
  
  if (ipmi_sel_parse_read_event_direction (c->sel_parse_ctx,
					   NULL,
					   0,
					   &event_direction) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_event_direction: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }

  if (event_direction == IPMI_SEL_RECORD_ASSERTION_EVENT)
    s->event_direction = IPMI_MONITORING_SEL_EVENT_DIRECTION_ASSERTION;
  else
    s->event_direction = IPMI_MONITORING_SEL_EVENT_DIRECTION_DEASSERTION;
      
  if (ipmi_sel_parse_read_event_data1_offset_from_event_reading_type_code (c->sel_parse_ctx,
									   NULL,
									   0,
									   &event_offset) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_event_data1_offset_from_event_reading_type_code: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->event_offset = event_offset;

  if (ipmi_sel_parse_read_event_type_code (c->sel_parse_ctx,
					   NULL,
					   0,
					   &event_type_code) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_event_type_code: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->event_type_code = event_type_code;

  if ((s->event_offset_type = _get_event_offset_type (c,
                                                      event_type_code,
                                                      sel_sensor_type)) < 0)
    return (-1);

  if (ipmi_sel_parse_read_event_data1 (c->sel_parse_ctx,
				       NULL,
				       0,
				       &event_data1) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_event_data1: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->event_data1 = event_data1;

  if (ipmi_sel_parse_read_event_data2 (c->sel_parse_ctx,
				       NULL,
				       0,
				       &event_data2) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_event_data2: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->event_data2 = event_data2;

  if (ipmi_sel_parse_read_event_data3 (c->sel_parse_ctx,
				       NULL,
				       0,
				       &event_data3) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_event_data3: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->event_data3 = event_data3;

  sel_string_flags = IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD | IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE;
  if (sel_flags & IPMI_MONITORING_SEL_FLAGS_ENTITY_SENSOR_NAMES)
    sel_string_flags |= IPMI_SEL_STRING_FLAGS_ENTITY_SENSOR_NAMES;
  
  if (ipmi_sel_parse_read_record_string (c->sel_parse_ctx,
                                         "%s",
					 NULL,
					 0,
                                         s->sensor_name,
                                         IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH,
                                         sel_string_flags) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_record_string: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }

  memset (event_offset_string, '\0', IPMI_MONITORING_SEL_EVENT_OFFSET_STRING_MAX + 1);

  sel_string_flags = IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD | IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE;

  if ((ret = ipmi_sel_parse_read_record_string (c->sel_parse_ctx,
                                                "%e",
						NULL,
						0,
                                                event_offset_string,
                                                IPMI_MONITORING_SEL_EVENT_OFFSET_STRING_MAX,
						sel_string_flags)) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_record_string: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  
  if (ret)
    {
      if (!(s->event_offset_string = strdup (event_offset_string)))
        {
          IPMI_MONITORING_DEBUG (("strdup: %s", strerror (errno)));
          c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
          return (-1);
        }
    }
  else
    {
      /* return empty string */
      if (!(s->event_offset_string = strdup ("")))
        {
          IPMI_MONITORING_DEBUG (("strdup: %s", strerror (errno)));
          c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
          return (-1);
        }
    }

  return (0);
}

static int
_ipmi_monitoring_sel_parse_timestamped_oem_record (ipmi_monitoring_ctx_t c,
                                                   struct ipmi_monitoring_sel_record *s)
{
  uint32_t timestamp;
  uint32_t manufacturer_id;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (s);

  if (ipmi_sel_parse_read_timestamp (c->sel_parse_ctx,
				     NULL,
				     0,
				     &timestamp) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_timestamp: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->timestamp = timestamp;
  
  if (ipmi_sel_parse_read_manufacturer_id (c->sel_parse_ctx,
					   NULL,
					   0,
					   &manufacturer_id) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_manufacturer_id: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->manufacturer_id = manufacturer_id;

  if ((ret = ipmi_sel_parse_read_oem (c->sel_parse_ctx,
				      NULL,
				      0,
                                      s->oem_data,
                                      IPMI_MONITORING_OEM_DATA_MAX)) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_oem: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->oem_data_len = ret;

  return (0);
}

static int
_ipmi_monitoring_sel_parse_non_timestamped_oem_record (ipmi_monitoring_ctx_t c,
                                                       struct ipmi_monitoring_sel_record *s)
{
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (s);

  if ((ret = ipmi_sel_parse_read_oem (c->sel_parse_ctx,
				      NULL,
				      0,
                                      s->oem_data,
                                      IPMI_MONITORING_OEM_DATA_MAX)) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_oem: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      return (-1);
    }
  s->oem_data_len = ret;

  return (0);
}

static int
_store_sel_record (ipmi_monitoring_ctx_t c, unsigned int sel_flags)
{
  struct ipmi_monitoring_sel_record *s = NULL;
  uint8_t sel_record[IPMI_SEL_RECORD_MAX_RECORD_LENGTH];
  int sel_record_len;
  uint16_t record_id;
  uint8_t record_type;
  int record_type_class;
  unsigned int sel_state;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sel_records);
  
  if (!(s = (struct ipmi_monitoring_sel_record *)malloc (sizeof (struct ipmi_monitoring_sel_record))))
    {
      IPMI_MONITORING_DEBUG (("malloc: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  memset (s, '\0', sizeof (struct ipmi_monitoring_sel_record));

  if ((sel_record_len = ipmi_sel_parse_read_record (c->sel_parse_ctx,
                                                    sel_record,
                                                    IPMI_SEL_RECORD_MAX_RECORD_LENGTH)) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_record: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      goto cleanup;
    }
  
  if (ipmi_sel_parse_read_record_id (c->sel_parse_ctx,
				     NULL,
				     0,
				     &record_id) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_record_id: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      goto cleanup;
    }

  if (ipmi_sel_parse_read_record_type (c->sel_parse_ctx,
				       NULL,
				       0,
				       &record_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_record_id: %s",
                              ipmi_sel_ctx_errnum (c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (c);
      goto cleanup;
    }

  /* IPMI Workaround
   *
   * HP DL 380 G5
   *
   * Motherboard is reporting SEL Records of record type 0x00, which
   * is not a valid record type.
   */
  if (sel_flags & IPMI_MONITORING_SEL_FLAGS_ASSUME_SYSTEM_EVENT_RECORD
      && !IPMI_SEL_RECORD_TYPE_VALID (record_type))
    record_type = IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD;

  s->record_id = record_id;
  s->record_type = record_type;

  if (ipmi_interpret_sel (c->interpret_ctx,
                          sel_record,
                          sel_record_len,
                          &sel_state) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_interpret_sel: %s",
                              ipmi_interpret_ctx_errormsg (c->interpret_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (sel_state == IPMI_INTERPRET_STATE_NOMINAL)
    s->sel_state = IPMI_MONITORING_STATE_NOMINAL;
  else if (sel_state == IPMI_INTERPRET_STATE_WARNING)
    s->sel_state = IPMI_MONITORING_STATE_WARNING;
  else if (sel_state == IPMI_INTERPRET_STATE_CRITICAL)
    s->sel_state = IPMI_MONITORING_STATE_CRITICAL;
  else if (sel_state == IPMI_INTERPRET_STATE_UNKNOWN)
    s->sel_state = IPMI_MONITORING_STATE_UNKNOWN;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      s->record_type_class = IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD;

      if (_ipmi_monitoring_sel_parse_system_event_record (c, s, sel_flags) < 0)
        goto cleanup;
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      s->record_type_class = IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD;

      if (_ipmi_monitoring_sel_parse_timestamped_oem_record (c, s) < 0)
        goto cleanup;
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    {
      s->record_type_class = IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD;

      if (_ipmi_monitoring_sel_parse_non_timestamped_oem_record (c, s) < 0)
        goto cleanup;
    }
  else
    s->record_type_class = IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_UNKNOWN;
  

  /* achu: should come before list_append to avoid having a freed entry on the list */
  if (c->callback)
    {
      c->callback_sel_record = s;
      if ((*c->callback)(c, c->callback_data) < 0)
        {
          IPMI_MONITORING_DEBUG (("callback error"));
          c->errnum = IPMI_MONITORING_ERR_CALLBACK_ERROR;
          goto cleanup;
        }
    }

  if (!list_append (c->sel_records, s))
    {
      IPMI_MONITORING_DEBUG (("list_append: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  return (0);

 cleanup:
  if (s)
    {
      free (s->event_offset_string);
      free (s);
    }
  return (-1);
}

static int
_ipmi_monitoring_sel_parse_record_id (ipmi_sel_ctx_t ctx, void *callback_data)
{
  struct sel_parse_data *spd;

  assert (ctx);
  assert (callback_data);

  spd = (struct sel_parse_data *)callback_data;

  /* all SEL records get stored */
  if (_store_sel_record (spd->c, spd->sel_flags) < 0)
    return (-1);

  return (0);
}

static int
_ipmi_monitoring_sel_parse_sensor_types (ipmi_sel_ctx_t ctx, void *callback_data)
{
  struct sel_parse_data *spd;
  uint8_t record_type;
  int record_type_class;

  assert (ctx);
  assert (callback_data);

  spd = (struct sel_parse_data *)callback_data;

  if (ipmi_sel_parse_read_record_type (spd->c->sel_parse_ctx,
				       NULL,
				       0,
				       &record_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_record_id: %s",
                              ipmi_sel_ctx_errnum (spd->c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (spd->c);
      return (-1);
    }

  /* IPMI Workaround
   *
   * HP DL 380 G5
   *
   * Motherboard is reporting SEL Records of record type 0x00, which
   * is not a valid record type.
   */
  if (spd->sel_flags & IPMI_MONITORING_SEL_FLAGS_ASSUME_SYSTEM_EVENT_RECORD
      && !IPMI_SEL_RECORD_TYPE_VALID (record_type))
    record_type = IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD;
  
  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      uint8_t sel_sensor_type;
      unsigned int i;
      int sensor_type;
      int found = 0;

      if (ipmi_sel_parse_read_sensor_type (spd->c->sel_parse_ctx,
					   NULL,
					   0,
					   &sel_sensor_type) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_sensor_type: %s",
                                  ipmi_sel_ctx_errnum (spd->c->sel_parse_ctx)));
          _sel_parse_ctx_error_convert (spd->c);
          return (-1);
        }

      if ((sensor_type = ipmi_monitoring_get_sensor_type (spd->c, sel_sensor_type)) < 0)
        return (-1);
      
      for (i = 0; i < spd->sensor_types_len; i++)
        {
          if (spd->sensor_types[i] == sensor_type)
            {
              found++;
              break;
            }
        }
      
      if (found)
        {
          if (_store_sel_record (spd->c, spd->sel_flags) < 0)
            return (-1);
        }
    }

  return (0);
}

static int
_ipmi_monitoring_sel_parse_date_range (ipmi_sel_ctx_t ctx, void *callback_data)
{
  struct sel_parse_data *spd;
  uint8_t record_type;
  int record_type_class;

  assert (ctx);
  assert (callback_data);

  spd = (struct sel_parse_data *)callback_data;

  if (ipmi_sel_parse_read_record_type (spd->c->sel_parse_ctx,
				       NULL,
				       0,
				       &record_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_record_id: %s",
                              ipmi_sel_ctx_errnum (spd->c->sel_parse_ctx)));
      _sel_parse_ctx_error_convert (spd->c);
      return (-1);
    }

  /* IPMI Workaround
   *
   * HP DL 380 G5
   *
   * Motherboard is reporting SEL Records of record type 0x00, which
   * is not a valid record type.
   */
  if (spd->sel_flags & IPMI_MONITORING_SEL_FLAGS_ASSUME_SYSTEM_EVENT_RECORD
      && !IPMI_SEL_RECORD_TYPE_VALID (record_type))
    record_type = IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD;
  
  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      || record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      uint32_t timestamp;

      if (ipmi_sel_parse_read_timestamp (spd->c->sel_parse_ctx,
					 NULL,
					 0,
					 &timestamp) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sel_parse_read_timestamp: %s",
                                  ipmi_sel_ctx_errnum (spd->c->sel_parse_ctx)));
          _sel_parse_ctx_error_convert (spd->c);
          return (-1);
        }

      if (timestamp >= spd->date_begin
          && timestamp <= spd->date_end)
        {
          if (_store_sel_record (spd->c, spd->sel_flags) < 0)
            return (-1);
        }
    }

  return (0);
}

int
ipmi_monitoring_get_sel (ipmi_monitoring_ctx_t c,
                         unsigned int sel_flags,
                         unsigned int *record_ids,
                         unsigned int record_ids_len,
                         unsigned int *sensor_types,
                         unsigned int sensor_types_len,
                         unsigned int *date_begin,
                         unsigned int *date_end)
{
  struct sel_parse_data spd;
  uint16_t *record_ids_tmp = NULL;
  unsigned int i;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->ipmi_ctx);
  assert (c->sel_parse_ctx);
  assert (c->sel_records);

  spd.c = c;
  spd.sel_flags = sel_flags;

  if (record_ids
      && record_ids_len)
    {
      /* ipmi_sel_parse takes uint16_t */

      if (!((record_ids_tmp = malloc (sizeof (uint16_t) * record_ids_len))))
        {
          IPMI_MONITORING_DEBUG (("malloc: %s", strerror (errno)));
          c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
          goto cleanup;
        }

      memset (record_ids_tmp, '\0', sizeof (uint16_t) * record_ids_len);

      for (i = 0; i < record_ids_len; i++)
        record_ids_tmp[i] = record_ids[i];
      
      if (ipmi_sel_parse_record_ids (c->sel_parse_ctx,
                                     record_ids_tmp,
                                     record_ids_len,
                                     _ipmi_monitoring_sel_parse_record_id,
                                     &spd) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sel_parse: %s",
                                  ipmi_sel_ctx_errormsg (c->sel_parse_ctx)));
          _sel_parse_ctx_error_convert (c);
          goto cleanup;
        }
    }
  else if (sensor_types
           && sensor_types_len)
    {
      spd.sensor_types = sensor_types;
      spd.sensor_types_len = sensor_types_len;

      if (ipmi_sel_parse (c->sel_parse_ctx,
                          IPMI_SEL_RECORD_ID_FIRST,
                          IPMI_SEL_RECORD_ID_LAST,
                          _ipmi_monitoring_sel_parse_sensor_types,
                          &spd) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sel_parse: %s",
                                  ipmi_sel_ctx_errormsg (c->sel_parse_ctx)));
          _sel_parse_ctx_error_convert (c);
          goto cleanup;
        }
    }
  else if (date_begin
           && date_end)
    {
      spd.date_begin = (*date_begin);
      spd.date_end = (*date_end);

      if (ipmi_sel_parse (c->sel_parse_ctx,
                          IPMI_SEL_RECORD_ID_FIRST,
                          IPMI_SEL_RECORD_ID_LAST,
                          _ipmi_monitoring_sel_parse_date_range,
                          &spd) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sel_parse: %s",
                                  ipmi_sel_ctx_errormsg (c->sel_parse_ctx)));
          _sel_parse_ctx_error_convert (c);
          goto cleanup;
        }
    }
  else
    {
      if (ipmi_sel_parse (c->sel_parse_ctx,
                          IPMI_SEL_RECORD_ID_FIRST,
                          IPMI_SEL_RECORD_ID_LAST,
                          _ipmi_monitoring_sel_parse_record_id,
                          &spd) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sel_parse: %s",
                                  ipmi_sel_ctx_errormsg (c->sel_parse_ctx)));
          _sel_parse_ctx_error_convert (c);
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  free (record_ids_tmp);
  return (rv);
}
