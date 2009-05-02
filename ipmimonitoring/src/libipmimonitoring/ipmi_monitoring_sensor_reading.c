/*****************************************************************************\
 *  $Id: ipmi_monitoring_sensor_reading.c,v 1.71 2009-05-02 02:17:36 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
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
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_ipmi_communication.h"
#include "ipmi_monitoring_sensor_config.h"
#include "ipmi_monitoring_sensor_reading.h"

#include "freeipmi-portability.h"

static void
_sensor_reading_cleanup (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (c->sensor_read_ctx)
    {
      ipmi_sensor_read_ctx_destroy (c->sensor_read_ctx);
      c->sensor_read_ctx = NULL;
    }
  if (c->sdr_parse_ctx)
    {
      ipmi_sdr_parse_ctx_destroy (c->sdr_parse_ctx);
      c->sdr_parse_ctx = NULL;
    }
}

int
ipmi_monitoring_sensor_reading_init (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->ipmi_ctx);
  assert (!c->sensor_read_ctx);
  assert (!c->sdr_parse_ctx);

  if (!(c->sensor_read_ctx = ipmi_sensor_read_ctx_create (c->ipmi_ctx)))
    {
      IPMI_MONITORING_DEBUG (("ipmi_sensor_read_ctx_create: %s", strerror (errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  if (!(c->sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_ctx_create: %s", strerror (errno)));
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
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  memset (s, '\0', sizeof (struct ipmi_monitoring_sensor_reading));

  return (s);

 cleanup:
  if (s)
    free (s);
  return (NULL);
}

static int
_store_sensor_reading (ipmi_monitoring_ctx_t c,
                       unsigned int sensor_reading_flags,
                       int record_id,
                       int sensor_group,
                       char *sensor_name,
                       int sensor_state,
                       int sensor_units,
                       int sensor_reading_type,
                       int sensor_bitmask_type,
                       uint16_t sensor_bitmask,
                       void *sensor_reading)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_MONITORING_SENSOR_GROUP_VALID (sensor_group));
  assert (sensor_name);
  assert (IPMI_MONITORING_SENSOR_STATE_VALID (sensor_state));
  assert (IPMI_MONITORING_SENSOR_UNITS_VALID (sensor_units));
  assert (IPMI_MONITORING_SENSOR_READING_TYPE_VALID (sensor_reading_type));
  assert (IPMI_MONITORING_SENSOR_BITMASK_TYPE_VALID (sensor_bitmask_type));

  if ((sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS)
      && sensor_state == IPMI_MONITORING_SENSOR_STATE_UNKNOWN)
    return (0);

  if (!(s = _allocate_sensor_reading (c)))
    goto cleanup;

  s->record_id = record_id;
  s->sensor_group = sensor_group;
  strncpy (s->sensor_name, sensor_name, IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);
  s->sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH - 1] = '\0';
  s->sensor_state = sensor_state;
  s->sensor_units = sensor_units;
  s->sensor_reading_type = sensor_reading_type;
  s->sensor_bitmask_type = sensor_bitmask_type;
  s->sensor_bitmask = sensor_bitmask;

  if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
    s->sensor_reading.bool_val = *((uint8_t *)sensor_reading);
  else if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
    s->sensor_reading.integer_val = *((uint32_t *)sensor_reading);
  else if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
    s->sensor_reading.double_val = *((double *)sensor_reading);

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
  if (s)
    free (s);
  return (-1);
}

static int
_store_unreadable_sensor_reading (ipmi_monitoring_ctx_t c,
                                  unsigned int sensor_reading_flags,
                                  int record_id,
                                  int sensor_group,
                                  char *sensor_name,
                                  int sensor_units)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_MONITORING_SENSOR_GROUP_VALID (sensor_group));
  assert (IPMI_MONITORING_SENSOR_UNITS_VALID (sensor_units));

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS)
    return (0);

  if (!(s = _allocate_sensor_reading (c)))
    goto cleanup;

  s->record_id = record_id;
  s->sensor_group = sensor_group;
  if (sensor_name)
    {
      strncpy (s->sensor_name, sensor_name, IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);
      s->sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH - 1] = '\0';
    }
  s->sensor_state = IPMI_MONITORING_SENSOR_STATE_UNKNOWN;
  s->sensor_units = sensor_units;
  s->sensor_reading_type = IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN;
  s->sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;
  s->sensor_bitmask = 0;

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
  if (s)
    free (s);
  return (-1);
}

static int
_get_sensor_state (ipmi_monitoring_ctx_t c,
                   uint16_t sensor_event_bitmask,
                   struct ipmi_sensor_config *config)
{
  int sensor_state = IPMI_MONITORING_SENSOR_STATE_NOMINAL;
  int i = 0;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (config);

  i = 0;
  while (config[i].option_str && i < 16)
    {
      if (sensor_event_bitmask & (0x1 << i))
        {
          if (config[i].sensor_state > sensor_state)
            sensor_state = config[i].sensor_state;
        }
      i++;
    }

  return (sensor_state);
}

static int
_get_threshold_sensor_state (ipmi_monitoring_ctx_t c,
                             uint16_t sensor_event_bitmask)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  return (_get_sensor_state (c, sensor_event_bitmask, ipmi_threshold_sensor_config));
}

static int
_get_digital_sensor_state (ipmi_monitoring_ctx_t c,
                           uint8_t event_reading_type_code,
                           uint8_t sensor_type,
                           uint16_t sensor_event_bitmask)
{
  struct ipmi_sensor_config *config;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code));

  /* achu: there are no "names" associated with
   * event_reading_type_codes in the spec (table 42-2), so there are
   * no macros.  We just gotta hard code numbers.
   */

  if (event_reading_type_code == 0x03
      && sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
    config = ipmi_voltage_state_config;
  else if (event_reading_type_code == 0x06
           && sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
    config = ipmi_voltage_performance_config;
  else if (event_reading_type_code == 0x08
           && sensor_type == IPMI_SENSOR_TYPE_FAN)
    config = ipmi_fan_device_install_config;
  else if (event_reading_type_code == 0x0A
           && sensor_type == IPMI_SENSOR_TYPE_FAN)
    config = ipmi_fan_transition_availability_config;
  else if (event_reading_type_code == 0x0B
           && sensor_type == IPMI_SENSOR_TYPE_FAN)
    config = ipmi_fan_redundancy_config;
  else if (event_reading_type_code == 0x03
           && sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
    config = ipmi_processor_state_config;
  else if (event_reading_type_code == 0x03
           && sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    config = ipmi_power_supply_state_config;
  else if (event_reading_type_code == 0x0B
           && sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    config = ipmi_power_supply_redundancy_config;
  else if (event_reading_type_code == 0x08
           && sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    config = ipmi_power_unit_device_install_config;
  else if (event_reading_type_code == 0x0B
           && sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    config = ipmi_power_unit_redundancy_config;
  else if (event_reading_type_code == 0x03
           && sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    config = ipmi_module_board_state_config;
  else if (event_reading_type_code == 0x08
           && sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    config = ipmi_module_board_device_install_config;
  else if (event_reading_type_code == 0x03
           && sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    config = ipmi_drive_slot_state_config;
  else if (event_reading_type_code == 0x04
           && sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    config = ipmi_drive_slot_predictive_failure_config;
  else if (event_reading_type_code == 0x08
           && sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    config = ipmi_drive_slot_device_install_config;
  else if (event_reading_type_code == 0x03
           && sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
    config = ipmi_button_switch_state_config;
  else if (event_reading_type_code == 0x08
           && sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
    config = ipmi_entity_presence_device_install_config;
  else
    {
      IPMI_MONITORING_DEBUG (("event_reading_type_code '0x%X' and sensor_type '0x%X' not supported",
                              event_reading_type_code,
                              sensor_type));
      return (IPMI_MONITORING_SENSOR_STATE_UNKNOWN);
    }

  return (_get_sensor_state (c, sensor_event_bitmask, config));
}

static int
_get_specific_sensor_state (ipmi_monitoring_ctx_t c,
                            uint8_t sensor_type,
                            uint16_t sensor_event_bitmask)
{
  struct ipmi_sensor_config *config;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
    config = ipmi_physical_security_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
    config = ipmi_platform_security_violation_attempt_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
    config = ipmi_processor_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    config = ipmi_power_supply_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    config = ipmi_power_unit_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_MEMORY)
    config = ipmi_memory_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    config = ipmi_drive_slot_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
    config = ipmi_system_firmware_progress_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
    config = ipmi_event_logging_disabled_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
    config = ipmi_system_event_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
    config = ipmi_critical_interrupt_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
    config = ipmi_slot_connector_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
    config = ipmi_watchdog2_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
    config = ipmi_entity_presence_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
    config = ipmi_management_subsystem_health_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_BATTERY)
    config = ipmi_battery_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
    config = ipmi_fru_state_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
    config = ipmi_cable_interconnect_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
    config = ipmi_boot_error_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
    config = ipmi_button_switch_config;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
    config = ipmi_system_acpi_power_state_config;
  else
    {
      IPMI_MONITORING_DEBUG (("sensor_type '0x%X' not supported", sensor_type));
      return (IPMI_MONITORING_SENSOR_STATE_UNKNOWN);
    }

  return (_get_sensor_state (c, sensor_event_bitmask, config));
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
                     uint8_t *sdr_record,
                     unsigned int sdr_record_len,
                     double *sensor_reading,
                     uint16_t *sensor_event_bitmask)
{
  double *l_sensor_reading = NULL;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (sensor_reading);
  assert (sensor_event_bitmask);

  if (ipmi_sensor_read (c->sensor_read_ctx,
                        sdr_record,
                        sdr_record_len,
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

  if (sensor_units_modifier != IPMI_SDR_PERCENTAGE_NO)
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

  if (sensor_base_unit_type == IPMI_SENSOR_UNIT_DEGREES_C)
    return (IPMI_MONITORING_SENSOR_UNITS_CELSIUS);
  else if (sensor_base_unit_type == IPMI_SENSOR_UNIT_DEGREES_F)
    return (IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT);
  else if (sensor_base_unit_type == IPMI_SENSOR_UNIT_VOLTS)
    return (IPMI_MONITORING_SENSOR_UNITS_VOLTS);
  else if (sensor_base_unit_type == IPMI_SENSOR_UNIT_AMPS)
    return (IPMI_MONITORING_SENSOR_UNITS_AMPS);
  else if (sensor_base_unit_type == IPMI_SENSOR_UNIT_RPM)
    return (IPMI_MONITORING_SENSOR_UNITS_RPM);
  else if (sensor_base_unit_type == IPMI_SENSOR_UNIT_WATTS)
    return (IPMI_MONITORING_SENSOR_UNITS_WATTS);

  IPMI_MONITORING_DEBUG (("sensor_base_unit_type '0x%X' not supported", sensor_base_unit_type));
  return (IPMI_MONITORING_SENSOR_UNITS_UNKNOWN);
}

static int
_threshold_sensor_reading (ipmi_monitoring_ctx_t c,
                           unsigned int sensor_reading_flags,
                           uint16_t record_id,
                           int sensor_group,
                           char *sensor_name,
                           uint8_t *sdr_record,
                           unsigned int sdr_record_len)
{
  uint8_t sensor_units_percentage;
  uint8_t sensor_units_modifier;
  uint8_t sensor_units_rate;
  uint8_t sensor_base_unit_type;
  uint8_t sensor_modifier_unit_type;
  double sensor_reading;
  uint16_t sensor_event_bitmask;
  int sensor_units;
  int sensor_state;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_MONITORING_SENSOR_GROUP_VALID (sensor_group));
  assert (sensor_name);
  assert (sdr_record);
  assert (sdr_record_len);

  if (ipmi_sdr_parse_sensor_units (c->sdr_parse_ctx,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_units: %s",
                              ipmi_sdr_parse_ctx_errormsg (c->sdr_parse_ctx)));
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
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_reading,
                                  &sensor_event_bitmask)) < 0)
    return (-1);

  if (!ret)
    {
      IPMI_MONITORING_DEBUG (("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_group,
                                            sensor_name,
                                            sensor_units) < 0)
        return (-1);
      return (0);
    }

  if ((sensor_state = _get_threshold_sensor_state (c, sensor_event_bitmask)) < 0)
    return (-1);

  if (_store_sensor_reading (c,
                             sensor_reading_flags,
                             record_id,
                             sensor_group,
                             sensor_name,
                             sensor_state,
                             sensor_units,
                             IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE,
                             IPMI_MONITORING_SENSOR_BITMASK_TYPE_THRESHOLD,
                             sensor_event_bitmask,
                             &sensor_reading) < 0)
    return (-1);

  return (0);
}

static int
_get_digital_sensor_bitmask_type (ipmi_monitoring_ctx_t c,
                                  uint8_t event_reading_type_code)
{
  int sensor_bitmask_type;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code));

  /* achu: there are no "names" associated with
   * event_reading_type_codes in the spec (table 42-2), so there are
   * no macros.  We just gotta hard code numbers.
   */

  if (event_reading_type_code == 0x01)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_THRESHOLD;
  if (event_reading_type_code == 0x02)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION;
  else if (event_reading_type_code == 0x03)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_STATE;
  else if (event_reading_type_code == 0x04)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PREDICTIVE_FAILURE;
  else if (event_reading_type_code == 0x05)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_LIMIT;
  else if (event_reading_type_code == 0x06)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PERFORMANCE;
  else if (event_reading_type_code == 0x07)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_SEVERITY;
  else if (event_reading_type_code == 0x08)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_DEVICE_INSTALL;
  else if (event_reading_type_code == 0x09)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_DEVICE_STATE;
  else if (event_reading_type_code == 0x0A)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION_DEVICE;
  else if (event_reading_type_code == 0x0B)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_REDUNDANCY;
  else if (event_reading_type_code == 0x0C)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_POWER_STATE;
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
                         uint8_t event_reading_type_code,
                         uint8_t sensor_type,
                         int sensor_group,
                         char *sensor_name,
                         uint8_t *sdr_record,
                         unsigned int sdr_record_len)
{
  double sensor_reading;
  uint16_t sensor_event_bitmask;
  int sensor_state;
  int sensor_bitmask_type;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code));
  assert (IPMI_MONITORING_SENSOR_GROUP_VALID (sensor_group));
  assert (sensor_name);
  assert (sdr_record);
  assert (sdr_record_len);

  if ((ret = _get_sensor_reading (c,
                                  sensor_reading_flags,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_reading,
                                  &sensor_event_bitmask)) < 0)
    return (-1);

  if (!ret)
    {
      IPMI_MONITORING_DEBUG (("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_group,
                                            sensor_name,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        return (-1);
      return (0);
    }

  if ((sensor_state = _get_digital_sensor_state (c,
                                                 event_reading_type_code,
                                                 sensor_type,
                                                 sensor_event_bitmask)) < 0)
    return (-1);

  if ((sensor_bitmask_type = _get_digital_sensor_bitmask_type (c,
                                                               event_reading_type_code)) < 0)
    return (-1);

  /* No actual sensor reading, only a sensor event bitmask */
  if (_store_sensor_reading (c,
                             sensor_reading_flags,
                             record_id,
                             sensor_group,
                             sensor_name,
                             sensor_state,
                             IPMI_MONITORING_SENSOR_UNITS_NONE,
                             IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN,
                             sensor_bitmask_type,
                             sensor_event_bitmask,
                             NULL) < 0)
    return (-1);

  return (0);
}

static int
_get_specific_sensor_bitmask_type (ipmi_monitoring_ctx_t c,
                                   uint8_t sensor_type)
{
  int sensor_bitmask_type;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PHYSICAL_SECURITY;
  else if (sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT;
  else if (sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_PROCESSOR;
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_POWER_SUPPLY;
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_POWER_UNIT;
  else if (sensor_type == IPMI_SENSOR_TYPE_MEMORY)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MEMORY;
  else if (sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_DRIVE_SLOT;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_FIRMWARE_PROGRESS;
  else if (sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_EVENT_LOGGING_DISABLED;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_EVENT;
  else if (sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_CRITICAL_INTERRUPT;
  else if (sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MODULE_BOARD;
  else if (sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SLOT_CONNECTOR;
  else if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_WATCHDOG2;
  else if (sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_ENTITY_PRESENCE;
  else if (sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH;
  else if (sensor_type == IPMI_SENSOR_TYPE_BATTERY)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_BATTERY;
  else if (sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_FRU_STATE;
  else if (sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_CABLE_INTERCONNECT;
  else if (sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_BOOT_ERROR;
  else if (sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_BUTTON_SWITCH;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
    sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_SYSTEM_ACPI_POWER_STATE;
  else
    {
      IPMI_MONITORING_DEBUG (("sensor_type '0x%X' bitmask not supported", sensor_type));
      sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;
    }

  return (sensor_bitmask_type);
}

static int
_specific_sensor_reading (ipmi_monitoring_ctx_t c,
                          unsigned int sensor_reading_flags,
                          uint16_t record_id,
                          uint8_t sensor_type,
                          int sensor_group,
                          char *sensor_name,
                          uint8_t *sdr_record,
                          unsigned int sdr_record_len)
{
  double sensor_reading;
  uint16_t sensor_event_bitmask;
  int sensor_state;
  int sensor_bitmask_type;
  int ret;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (sensor_name);
  assert (sdr_record);
  assert (sdr_record_len);

  if ((ret = _get_sensor_reading (c,
                                  sensor_reading_flags,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_reading,
                                  &sensor_event_bitmask)) < 0)
    return (-1);

  if (!ret)
    {
      IPMI_MONITORING_DEBUG (("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_group,
                                            sensor_name,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        return (-1);
      return (0);
    }

  if ((sensor_state = _get_specific_sensor_state (c,
                                                  sensor_type,
                                                  sensor_event_bitmask)) < 0)
    return (-1);

  if ((sensor_bitmask_type = _get_specific_sensor_bitmask_type (c,
                                                                sensor_type)) < 0)
    return (-1);

  /* No actual sensor reading, only a sensor event bitmask */
  if (_store_sensor_reading (c,
                             sensor_reading_flags,
                             record_id,
                             sensor_group,
                             sensor_name,
                             sensor_state,
                             IPMI_MONITORING_SENSOR_UNITS_NONE,
                             IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN,
                             sensor_bitmask_type,
                             sensor_event_bitmask,
                             NULL) < 0)
    return (-1);

  return (0);
}

static int
_get_sensor_group (ipmi_monitoring_ctx_t c,
                   uint8_t sensor_type)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (sensor_type == IPMI_SENSOR_TYPE_TEMPERATURE)
    return (IPMI_MONITORING_SENSOR_GROUP_TEMPERATURE);
  else if (sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
    return (IPMI_MONITORING_SENSOR_GROUP_VOLTAGE);
  else if (sensor_type == IPMI_SENSOR_TYPE_CURRENT)
    return (IPMI_MONITORING_SENSOR_GROUP_CURRENT);
  else if (sensor_type == IPMI_SENSOR_TYPE_FAN)
    return (IPMI_MONITORING_SENSOR_GROUP_FAN);
  else if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
    return (IPMI_MONITORING_SENSOR_GROUP_PHYSICAL_SECURITY);
  else if (sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
    return (IPMI_MONITORING_SENSOR_GROUP_PLATFORM_SECURITY_VIOLATION_ATTEMPT);
  else if (sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
    return (IPMI_MONITORING_SENSOR_GROUP_PROCESSOR);
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    return (IPMI_MONITORING_SENSOR_GROUP_POWER_SUPPLY);
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    return (IPMI_MONITORING_SENSOR_GROUP_POWER_UNIT);
  else if (sensor_type == IPMI_SENSOR_TYPE_MEMORY)
    return (IPMI_MONITORING_SENSOR_GROUP_MEMORY);
  else if (sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    return (IPMI_MONITORING_SENSOR_GROUP_DRIVE_SLOT);
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
    return (IPMI_MONITORING_SENSOR_GROUP_SYSTEM_FIRMWARE_PROGRESS);
  else if (sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
    return (IPMI_MONITORING_SENSOR_GROUP_EVENT_LOGGING_DISABLED);
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
    return (IPMI_MONITORING_SENSOR_GROUP_SYSTEM_EVENT);
  else if (sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
    return (IPMI_MONITORING_SENSOR_GROUP_CRITICAL_INTERRUPT);
  else if (sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    return (IPMI_MONITORING_SENSOR_GROUP_MODULE_BOARD);
  else if (sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
    return (IPMI_MONITORING_SENSOR_GROUP_SLOT_CONNECTOR);
  else if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
    return (IPMI_MONITORING_SENSOR_GROUP_WATCHDOG2);
  else if (sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
    return (IPMI_MONITORING_SENSOR_GROUP_ENTITY_PRESENCE);
  else if (sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
    return (IPMI_MONITORING_SENSOR_GROUP_MANAGEMENT_SUBSYSTEM_HEALTH);
  else if (sensor_type == IPMI_SENSOR_TYPE_BATTERY)
    return (IPMI_MONITORING_SENSOR_GROUP_BATTERY);
  else if (sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
    return (IPMI_MONITORING_SENSOR_GROUP_FRU_STATE);
  else if (sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
    return (IPMI_MONITORING_SENSOR_GROUP_CABLE_INTERCONNECT);
  else if (sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
    return (IPMI_MONITORING_SENSOR_GROUP_BOOT_ERROR);
  else if (sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
    return (IPMI_MONITORING_SENSOR_GROUP_BUTTON_SWITCH);
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
    return (IPMI_MONITORING_SENSOR_GROUP_SYSTEM_ACPI_POWER_STATE);

  IPMI_MONITORING_DEBUG (("sensor_type '0x%X' not supported", sensor_type));
  return (IPMI_MONITORING_SENSOR_GROUP_UNKNOWN);
}

int
ipmi_monitoring_get_sensor_reading (ipmi_monitoring_ctx_t c,
                                    unsigned int sensor_reading_flags,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    unsigned int *sensor_groups,
                                    unsigned int sensor_groups_len)
{
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  uint16_t record_id;
  uint8_t record_type;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  int sensor_group;
  int len;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->sensor_readings);
  assert (c->ipmi_ctx);
  assert (c->sensor_read_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (!sensor_groups || sensor_groups_len);

  if (ipmi_sdr_parse_record_id_and_type (c->sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         &record_id,
                                         &record_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_record_id_and_type: %s",
                              ipmi_sdr_parse_ctx_errormsg (c->sdr_parse_ctx)));
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
                                            IPMI_MONITORING_SENSOR_GROUP_UNKNOWN,
                                            NULL,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        return (-1);
      return (0);
    }

  if (ipmi_sdr_parse_sensor_type (c->sdr_parse_ctx,
                                  sdr_record,
                                  sdr_record_len,
                                  &sensor_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_type: %s",
                              ipmi_sdr_parse_ctx_errormsg (c->sdr_parse_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if ((sensor_group = _get_sensor_group (c, sensor_type)) < 0)
    return (-1);

  if (sensor_groups)
    {
      int i, found = 0;

      for (i = 0; i < sensor_groups_len; i++)
        {
          if (sensor_groups[i] == sensor_group)
            {
              found++;
              break;
            }
        }

      if (!found)
        return (0);
    }

  memset (sensor_name, '\0', IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);

  if ((len = ipmi_sdr_parse_id_string (c->sdr_parse_ctx,
                                       sdr_record,
                                       sdr_record_len,
                                       sensor_name,
                                       IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH)) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_id_string: %s",
                              ipmi_sdr_parse_ctx_errormsg (c->sdr_parse_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if (len >= IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH)
    {
      IPMI_MONITORING_DEBUG (("sensor_name buffer short: len = %d", len));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if (ipmi_sdr_parse_event_reading_type_code (c->sdr_parse_ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_event_reading_type_code: %s",
                              ipmi_sdr_parse_ctx_errormsg (c->sdr_parse_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }


  if (IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      if (_threshold_sensor_reading (c,
                                     sensor_reading_flags,
                                     record_id,
                                     sensor_group,
                                     sensor_name,
                                     sdr_record,
                                     sdr_record_len) < 0)
        return (-1);
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code))
    {
      if (_digital_sensor_reading (c,
                                   sensor_reading_flags,
                                   record_id,
                                   event_reading_type_code,
                                   sensor_type,
                                   sensor_group,
                                   sensor_name,
                                   sdr_record,
                                   sdr_record_len) < 0)
        return (-1);
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
    {
      if (_specific_sensor_reading (c,
                                    sensor_reading_flags,
                                    record_id,
                                    sensor_type,
                                    sensor_group,
                                    sensor_name,
                                    sdr_record,
                                    sdr_record_len) < 0)
        return (-1);
    }
  else
    {
      IPMI_MONITORING_DEBUG (("event_reading_type_code '0x%X' not supported",
                              event_reading_type_code));

      if (_store_unreadable_sensor_reading (c,
                                            sensor_reading_flags,
                                            record_id,
                                            sensor_group,
                                            sensor_name,
                                            IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        return (-1);
    }

  return (0);
}
