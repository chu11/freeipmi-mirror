/*****************************************************************************\
 *  $Id: ipmi_monitoring_sensor_reading.c,v 1.26 2008-08-01 23:53:56 chu11 Exp $
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
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_fiid_wrappers.h"
#include "ipmi_monitoring_ipmi_communication.h"
#include "ipmi_monitoring_sensor_config.h"
#include "ipmi_monitoring_sensor_reading.h"

#include "freeipmi-portability.h"

static struct ipmi_monitoring_sensor_reading *
_allocate_sensor_reading(ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (!(s = (struct ipmi_monitoring_sensor_reading *)malloc(sizeof(struct ipmi_monitoring_sensor_reading))))
    {
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  memset(s, '\0', sizeof(struct ipmi_monitoring_sensor_reading));

  return s;

 cleanup:
  if (s)
    free(s);
  return NULL;
}

static int
_store_sensor_reading(ipmi_monitoring_ctx_t c,
                      unsigned int sensor_reading_flags,
                      int record_id,
                      int sensor_group,
                      char *sensor_name,
                      int sensor_state,
                      int sensor_units,
                      int sensor_reading_type,
                      int sensor_bitmask_type,
                      void *sensor_reading)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(IPMI_MONITORING_SENSOR_GROUP_VALID(sensor_group));
  assert(sensor_name);
  assert(IPMI_MONITORING_SENSOR_STATE_VALID(sensor_state));
  assert(IPMI_MONITORING_SENSOR_UNITS_VALID(sensor_units));
  assert(IPMI_MONITORING_SENSOR_READING_TYPE_VALID(sensor_reading_type));
  assert(IPMI_MONITORING_SENSOR_BITMASK_TYPE_VALID(sensor_bitmask_type));

  if ((sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS)
      && sensor_state == IPMI_MONITORING_SENSOR_STATE_UNKNOWN)
    return 0;

  if (!(s = _allocate_sensor_reading(c)))
    goto cleanup;
  
  s->record_id = record_id;
  s->sensor_group = sensor_group;
  strncpy(s->sensor_name, sensor_name, IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);
  s->sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH - 1] = '\0';
  s->sensor_state = sensor_state;
  s->sensor_units = sensor_units;
  s->sensor_reading_type = sensor_reading_type;
  s->sensor_bitmask_type = sensor_bitmask_type;

  if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
    s->sensor_reading.bool_val = *((uint8_t *)sensor_reading);
  else if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
    s->sensor_reading.integer_val = *((uint32_t *)sensor_reading);
  else if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
    s->sensor_reading.double_val = *((double *)sensor_reading);
  else if (s->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER16_BITMASK)
    s->sensor_reading.integer_bitmask_val = *((uint16_t *)sensor_reading);
  
  if (!list_append(c->sensor_readings, s))
    {
      IPMI_MONITORING_DEBUG(("list_append: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  return 0;

 cleanup:
  if (s)
    free(s);
  return -1;
}

static int
_store_unreadable_sensor_reading(ipmi_monitoring_ctx_t c,
                                 unsigned int sensor_reading_flags,
                                 int record_id,
                                 int sensor_group,
                                 char *sensor_name,
                                 int sensor_units)
{
  struct ipmi_monitoring_sensor_reading *s = NULL;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(IPMI_MONITORING_SENSOR_GROUP_VALID(sensor_group));
  assert(IPMI_MONITORING_SENSOR_UNITS_VALID(sensor_units));

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS)
    return 0;

  if (!(s = _allocate_sensor_reading(c)))
    goto cleanup;
  
  s->record_id = record_id;
  s->sensor_group = sensor_group;
  if (sensor_name)
    {
      strncpy(s->sensor_name, sensor_name, IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH);
      s->sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH - 1] = '\0';
    }
  s->sensor_state = IPMI_MONITORING_SENSOR_STATE_UNKNOWN;
  s->sensor_units = sensor_units;
  s->sensor_reading_type = IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN;
  s->sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;

  if (!list_append(c->sensor_readings, s))
    {
      IPMI_MONITORING_DEBUG(("list_append: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  return 0;

 cleanup:
  if (s)
    free(s);
  return -1;
}

static int
_get_sensor_units(ipmi_monitoring_ctx_t c,
                  uint8_t modifier_unit,
                  uint8_t rate_unit,
                  uint8_t sensor_base_unit,
                  uint8_t sensor_modifier_unit)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (modifier_unit != IPMI_SDR_MODIFIER_UNIT_NONE)
    {
      IPMI_MONITORING_DEBUG(("modifier_unit '0x%X' not supported", modifier_unit));
      return IPMI_MONITORING_SENSOR_UNITS_UNKNOWN;
    }

  if (rate_unit != IPMI_SENSOR_RATE_UNIT_NONE)
    {
      IPMI_MONITORING_DEBUG(("rate_unit '0x%X' not supported", rate_unit));
      return IPMI_MONITORING_SENSOR_UNITS_UNKNOWN;
    }

  if (sensor_modifier_unit != IPMI_SENSOR_UNIT_UNSPECIFIED)
    {
      IPMI_MONITORING_DEBUG(("sensor_modifier_unit '0x%X' not supported", sensor_modifier_unit));
      return IPMI_MONITORING_SENSOR_UNITS_UNKNOWN;
    }

  if (sensor_base_unit == IPMI_SENSOR_UNIT_DEGREES_C)
    return IPMI_MONITORING_SENSOR_UNITS_CELSIUS;
  else if (sensor_base_unit == IPMI_SENSOR_UNIT_DEGREES_F)
    return IPMI_MONITORING_SENSOR_UNITS_FAHRENHEIT;
  else if (sensor_base_unit == IPMI_SENSOR_UNIT_VOLTS)
    return IPMI_MONITORING_SENSOR_UNITS_VOLTS;
  else if (sensor_base_unit == IPMI_SENSOR_UNIT_AMPS)
    return IPMI_MONITORING_SENSOR_UNITS_AMPS;
  else if (sensor_base_unit == IPMI_SENSOR_UNIT_RPM)
    return IPMI_MONITORING_SENSOR_UNITS_RPM;
  else if (sensor_base_unit == IPMI_SENSOR_UNIT_WATTS)
    return IPMI_MONITORING_SENSOR_UNITS_WATTS;

  IPMI_MONITORING_DEBUG(("sensor_base_unit '0x%X' not supported", sensor_base_unit));
  return IPMI_MONITORING_SENSOR_UNITS_UNKNOWN;
}

static int
_get_sensor_state(ipmi_monitoring_ctx_t c,
                  uint16_t sensor_event_bitmask,
                  struct ipmi_sensor_config *config)
{
  int sensor_state = IPMI_MONITORING_SENSOR_STATE_NOMINAL;
  int i = 0;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(config);

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

  return sensor_state;
}

static int
_get_threshold_sensor_state(ipmi_monitoring_ctx_t c,
                            uint16_t sensor_event_bitmask)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  return _get_sensor_state(c, sensor_event_bitmask, ipmi_threshold_sensor_config);
}

static int
_get_digital_sensor_state(ipmi_monitoring_ctx_t c,
                          uint8_t event_reading_type_code,
                          uint8_t sensor_type,
                          uint16_t sensor_event_bitmask)
{
  struct ipmi_sensor_config *config;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code));

  /* achu: there are no "names" associated with
   * event_reading_type_codes in the spec (table 42-2), so there are
   * no macros.  We just gotta hard code numbers.
   */

  if (event_reading_type_code == 0x06 && sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
    config = ipmi_voltage_performance_config;
  else if (event_reading_type_code == 0x08 && sensor_type == IPMI_SENSOR_TYPE_FAN)
    config = ipmi_fan_device_install_config;
  else if (event_reading_type_code == 0x03 && sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    config = ipmi_module_board_state_config;
  else if (event_reading_type_code == 0x08 && sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    config = ipmi_module_board_device_install_config;
  else if (event_reading_type_code == 0x0B && sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    config = ipmi_power_unit_redundancy_config;
  else if (event_reading_type_code == 0x08 && sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    config = ipmi_drive_slot_device_install_config;
  else
    {
      IPMI_MONITORING_DEBUG(("event_reading_type_code '0x%X' and sensor_type '0x%X' not supported", event_reading_type_code, sensor_type));
      return IPMI_MONITORING_SENSOR_STATE_UNKNOWN;
    }

  return _get_sensor_state(c, sensor_event_bitmask, config);
}

static int
_get_specific_sensor_state(ipmi_monitoring_ctx_t c,
                           uint8_t sensor_type,
                           uint16_t sensor_event_bitmask)
{
  struct ipmi_sensor_config *config;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY_CHASSIS_INTRUSION)
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
  else
    {
      IPMI_MONITORING_DEBUG(("sensor_type '0x%X' not supported", sensor_type));
      return IPMI_MONITORING_SENSOR_STATE_UNKNOWN;
    }

  return _get_sensor_state(c, sensor_event_bitmask, config);
}

static int
_get_sensor_name(ipmi_monitoring_ctx_t c,
                 fiid_obj_t obj_sdr_record,
                 char *sensor_name,
                 unsigned int sensor_name_len)
{
  int32_t len;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(sensor_name);
  assert(sensor_name_len);
  assert(fiid_obj_valid(obj_sdr_record));

  memset(sensor_name, '\0', sensor_name_len);
  if ((len = Fiid_obj_get_data(c,
                               obj_sdr_record,
                               "id_string",
                               (uint8_t *)sensor_name,
                               sensor_name_len)) < 0)
    return -1;

  if (len >= sensor_name_len)
    {
      IPMI_MONITORING_DEBUG(("sensor_name buffer short: sensor_name_len = %d; len = %d", sensor_name_len, len));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return 0;
}

/*
 * return value -1 = error, 0 = unreadable sensor reading, 1 = sensor reading success
 */
static int
_get_sensor_reading(ipmi_monitoring_ctx_t c,
                    unsigned int sensor_reading_flags,
                    fiid_obj_t obj_sdr_record,
                    uint8_t *sensor_number,
                    uint8_t *sensor_reading,
                    uint16_t *sensor_event_bitmask)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  uint64_t val;
  uint64_t sensor_event_bitmask1;
  uint64_t sensor_event_bitmask2;
  int8_t sensor_event_bitmask1_len, sensor_event_bitmask2_len;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(fiid_obj_valid(obj_sdr_record));
  assert(sensor_number);
  assert(sensor_reading);
  assert(sensor_event_bitmask);

  if (!(obj_cmd_rs = Fiid_obj_create(c, tmpl_cmd_get_sensor_reading_rs)))
    goto cleanup;
     
  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_number",
                   &val) < 0)
    return -1;
  *sensor_number = val;

  if (ipmi_cmd_get_sensor_reading(c->ipmi_ctx, 
                                  *sensor_number, 
                                  obj_cmd_rs) < 0)
    {
      if (ipmi_check_completion_code(obj_cmd_rs, 
                                     IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
        {
          /* A sensor listed by the SDR is not present.  Tell the
           * caller to store this as an unreadable sensor
           */
          rv = 0;
          goto cleanup;
        }
      ipmi_monitoring_ipmi_ctx_error_convert(c);
      goto cleanup;
    }

  if (Fiid_obj_get(c,
                   obj_cmd_rs,
                   "reading_state",
                   &val) < 0)
    goto cleanup;
  
  if (val == IPMI_SENSOR_READING_STATE_UNAVAILABLE)
    {
      IPMI_MONITORING_DEBUG(("sensor reading unavailable"));
      /* sensor reading not available.  Tell the caller to store this
       * as an unreadable sensor
       */
      rv = 0;
      goto cleanup;
    }

  if (Fiid_obj_get(c,
                   obj_cmd_rs,
                   "sensor_reading",
                   &val) < 0)
    goto cleanup;
  *sensor_reading = val;

  /* 
   * IPMI Workaround (achu)
   *
   * Discovered on Dell 2950.
   *
   * It seems the sensor_event_bitmask may not be returned by the server
   * at all for some sensors.  Under this situation, there's not
   * much that can be done.  Since there is no sensor_event_bitmask, we
   * just assume that no states have been asserted and the
   * sensor_event_bitmask = 0;
   */
  /* Don't use wrapper, we need the return value */
  if ((sensor_event_bitmask1_len = fiid_obj_get(obj_cmd_rs, 
                                                "sensor_event_bitmask1", 
                                                &sensor_event_bitmask1)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_get: field=%s; %s", 
                             "sensor_event_bitmask1", 
                             fiid_strerror(fiid_obj_errnum(obj_cmd_rs))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if ((sensor_event_bitmask2_len = fiid_obj_get(obj_cmd_rs, 
                                                "sensor_event_bitmask2", 
                                                &sensor_event_bitmask2)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_get: field=%s; %s", 
                             "sensor_event_bitmask2", 
                             fiid_strerror(fiid_obj_errnum(obj_cmd_rs))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (!sensor_event_bitmask1_len && !sensor_event_bitmask2_len)
    {
      IPMI_MONITORING_DEBUG(("assume sensor_event_bitmask = 0"));
      *sensor_event_bitmask = 0;
    }
  else if (sensor_event_bitmask1_len && sensor_event_bitmask2_len)
    *sensor_event_bitmask = sensor_event_bitmask1 | (sensor_event_bitmask2 << 8);
  else if (sensor_event_bitmask1_len && !sensor_event_bitmask2_len)
    *sensor_event_bitmask = sensor_event_bitmask1;
  else
    {
      IPMI_MONITORING_DEBUG(("invalid sensor_event_bitmask conditions"));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  rv = 1;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return rv;
}

static int
_threshold_sensor_reading(ipmi_monitoring_ctx_t c,
                          unsigned int sensor_reading_flags,
                          uint16_t record_id,
                          uint8_t sensor_type,
                          int sensor_group,
                          fiid_obj_t obj_sdr_record)
{
  uint8_t sensor_number;
  uint8_t analog_data_format;
  uint8_t rate_unit;
  uint8_t modifier_unit;
  uint8_t sensor_base_unit;
  uint8_t sensor_modifier_unit;
  uint8_t linearization;
  int16_t m;
  int16_t b;
  int8_t r_exponent;
  int8_t b_exponent;
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  uint8_t sensor_reading;
  uint16_t sensor_event_bitmask;
  uint64_t val;
  int sensor_units;
  int sensor_state;
  double sensor_value;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(IPMI_MONITORING_SENSOR_GROUP_VALID(sensor_group));
  assert(fiid_obj_valid(obj_sdr_record));
  assert(fiid_obj_template_compare(obj_sdr_record, tmpl_sdr_full_sensor_record));

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_unit1.analog_data_format",
                   &val) < 0)
    return -1;
  analog_data_format = val;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_unit1.rate_unit",
                   &val) < 0)
    return -1;
  rate_unit = val;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_unit1.modifier_unit",
                   &val) < 0)
    return -1;
  modifier_unit = val;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_unit2.base_unit",
                   &val) < 0)
    return -1;
  sensor_base_unit = val;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_unit3.modifier_unit",
                   &val) < 0)
    return -1;
  sensor_modifier_unit = val;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "linearization",
                   &val) < 0)
    return -1;
  linearization = val;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "m_ls",
                   &val) < 0)
    return -1;
  m = val;
  
  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "m_ms",
                   &val) < 0)
    return -1;
  m |= ((val & 0x3) << 8);
  if (m & 0x200)
    m |= 0xFE00;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "b_ls",
                   &val) < 0)
    return -1;
  b = val;
  
  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "b_ms",
                   &val) < 0)
    return -1;
  b |= ((val & 0x3) << 8);
  if (b & 0x200)
    b |= 0xFE00;
  
  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "r_exponent",
                   &val) < 0)
    return -1;
  r_exponent = val;
  if (r_exponent & 0x08)
    r_exponent |= 0xF0;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "b_exponent",
                   &val) < 0)
    return -1;
  b_exponent = val;
  if (b_exponent & 0x08)
    b_exponent |= 0xF0;

  if (_get_sensor_name(c,
                       obj_sdr_record,
                       sensor_name,
                       IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH) < 0)
    return -1;

  if ((sensor_units = _get_sensor_units(c,
                                        modifier_unit,
                                        rate_unit,
                                        sensor_base_unit,
                                        sensor_modifier_unit)) < 0)
    return -1;

  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format))
    {
      IPMI_MONITORING_DEBUG(("non-analog sensors not currently supported: '0x%X'", analog_data_format));
      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           sensor_group,
                                           sensor_name,
                                           sensor_units) < 0)
        return -1;
      return 0;
    }

  if (IPMI_SDR_LINEARIZATION_IS_NON_LINEAR(linearization))
    {
      IPMI_MONITORING_DEBUG(("non-linear sensors not currently supported: '0x%X'", linearization));
      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           sensor_group,
                                           sensor_name,
                                           sensor_units) < 0)
        return -1;
      return 0;
    }

  if ((ret = _get_sensor_reading(c,
                                 sensor_reading_flags,
                                 obj_sdr_record,
                                 &sensor_number,
                                 &sensor_reading,
                                 &sensor_event_bitmask)) < 0)
    return -1;
  
  if (!ret)
    {
      IPMI_MONITORING_DEBUG(("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           sensor_group,
                                           sensor_name,
                                           sensor_units) < 0)
        return -1;
      return 0;
    }

  if (ipmi_sensor_decode_value(r_exponent,
                               b_exponent,
                               m,
                               b,
                               linearization,
                               analog_data_format,
                               sensor_reading,
                               &sensor_value) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_sensor_decode_value: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  if ((sensor_state = _get_threshold_sensor_state(c, sensor_event_bitmask)) < 0)
    return -1;

  if (_store_sensor_reading(c,
                            sensor_reading_flags,
                            record_id,
                            sensor_group,
                            sensor_name,
                            sensor_state,
                            sensor_units,
                            IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE,
                            IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN,
                            &sensor_value) < 0)
    return -1;

  return 0;
}

static int
_get_digital_sensor_bitmask_type(ipmi_monitoring_ctx_t c,
                                 uint8_t event_reading_type_code)
{
  int sensor_bitmask_type;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code));

  /* achu: there are no "names" associated with
   * event_reading_type_codes in the spec (table 42-2), so there are
   * no macros.  We just gotta hard code numbers.
   */

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
      IPMI_MONITORING_DEBUG(("event_reading_type_code '0x%X' bitmask not supported", event_reading_type_code));
      sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;
    }

  return sensor_bitmask_type;
}

static int
_digital_sensor_reading(ipmi_monitoring_ctx_t c,
                        unsigned int sensor_reading_flags,
                        uint16_t record_id,
                        uint8_t event_reading_type_code,
                        uint8_t sensor_type,
                        int sensor_group,
                        fiid_obj_t obj_sdr_record)
{
  uint8_t sensor_number;
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  uint8_t sensor_reading;
  uint16_t sensor_event_bitmask;
  int sensor_state;
  int sensor_bitmask_type;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code));
  assert(IPMI_MONITORING_SENSOR_GROUP_VALID(sensor_group));
  assert(fiid_obj_valid(obj_sdr_record));

  if (_get_sensor_name(c,
                       obj_sdr_record,
                       sensor_name,
                       IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH) < 0)
    return -1;

  if ((ret = _get_sensor_reading(c,
                                 sensor_reading_flags,
                                 obj_sdr_record,
                                 &sensor_number,
                                 &sensor_reading,
                                 &sensor_event_bitmask)) < 0)
    return -1;
  
  if (!ret)
    {
      IPMI_MONITORING_DEBUG(("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           sensor_group,
                                           sensor_name,
                                           IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        return -1;
      return 0;
    }

  if ((sensor_state = _get_digital_sensor_state(c, 
                                                event_reading_type_code, 
                                                sensor_type,
                                                sensor_event_bitmask)) < 0)
    return -1;

  if ((sensor_bitmask_type = _get_digital_sensor_bitmask_type(c, 
                                                              event_reading_type_code)) < 0)
    return -1;
  
  /* The sensor "reading" is a bitmask */
  if (_store_sensor_reading(c,
                            sensor_reading_flags,
                            record_id,
                            sensor_group,
                            sensor_name,
                            sensor_state,
                            IPMI_MONITORING_SENSOR_UNITS_NONE,
                            IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER16_BITMASK,
                            sensor_bitmask_type,
                            &sensor_event_bitmask) < 0)
    return -1;
  
  return 0;
}

static int
_get_specific_sensor_bitmask_type(ipmi_monitoring_ctx_t c,
                                  uint8_t sensor_type)
{
  int sensor_bitmask_type;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY_CHASSIS_INTRUSION)
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
  else
    {
      IPMI_MONITORING_DEBUG(("sensor_type '0x%X' bitmask not supported", sensor_type));
      sensor_bitmask_type = IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN;
    }
  
  return sensor_bitmask_type;
}

static int
_specific_sensor_reading(ipmi_monitoring_ctx_t c,
                         unsigned int sensor_reading_flags,
                         uint16_t record_id,
                         uint8_t sensor_type,
                         int sensor_group,
                         fiid_obj_t obj_sdr_record)
{
  uint8_t sensor_number;
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  uint8_t sensor_reading;
  uint16_t sensor_event_bitmask;
  int sensor_state;
  int sensor_bitmask_type;
  int ret;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(fiid_obj_valid(obj_sdr_record));

  if (_get_sensor_name(c,
                       obj_sdr_record,
                       sensor_name,
                       IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH) < 0)
    return -1;

  if ((ret = _get_sensor_reading(c,
                                 sensor_reading_flags,
                                 obj_sdr_record,
                                 &sensor_number,
                                 &sensor_reading,
                                 &sensor_event_bitmask)) < 0)
    return -1;
  
  if (!ret)
    {
      IPMI_MONITORING_DEBUG(("cannot read sensor for record id '%u'", record_id));
      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           sensor_group,
                                           sensor_name,
                                           IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        return -1;
      return 0;
    }
  
  if ((sensor_state = _get_specific_sensor_state(c, 
                                                 sensor_type, 
                                                 sensor_event_bitmask)) < 0)
    return -1;

  if ((sensor_bitmask_type = _get_specific_sensor_bitmask_type(c,
                                                               sensor_type)) < 0)
    return -1;

  /* The sensor "reading" is the bitmask */
  if (_store_sensor_reading(c,
                            sensor_reading_flags,
                            record_id,
                            sensor_group,
                            sensor_name,
                            sensor_state,
                            IPMI_MONITORING_SENSOR_UNITS_NONE,
                            IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER16_BITMASK,
                            sensor_bitmask_type,
                            &sensor_event_bitmask) < 0)
    return -1;
  
  return 0;
}

static int
_get_sensor_group(ipmi_monitoring_ctx_t c,
                  uint8_t sensor_type)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (sensor_type == IPMI_SENSOR_TYPE_TEMPERATURE)
    return IPMI_MONITORING_SENSOR_GROUP_TEMPERATURE;
  else if (sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
    return IPMI_MONITORING_SENSOR_GROUP_VOLTAGE;
  else if (sensor_type == IPMI_SENSOR_TYPE_CURRENT)
    return IPMI_MONITORING_SENSOR_GROUP_CURRENT;
  else if (sensor_type == IPMI_SENSOR_TYPE_FAN)
    return IPMI_MONITORING_SENSOR_GROUP_FAN;
  else if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY_CHASSIS_INTRUSION)
    return IPMI_MONITORING_SENSOR_GROUP_PHYSICAL_SECURITY;
  else if (sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
    return IPMI_MONITORING_SENSOR_GROUP_PLATFORM_SECURITY_VIOLATION_ATTEMPT;
  else if (sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
    return IPMI_MONITORING_SENSOR_GROUP_PROCESSOR;
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    return IPMI_MONITORING_SENSOR_GROUP_POWER_SUPPLY;
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    return IPMI_MONITORING_SENSOR_GROUP_POWER_UNIT;
  else if (sensor_type == IPMI_SENSOR_TYPE_MEMORY)
    return IPMI_MONITORING_SENSOR_GROUP_MEMORY;
  else if (sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    return IPMI_MONITORING_SENSOR_GROUP_DRIVE_SLOT;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
    return IPMI_MONITORING_SENSOR_GROUP_SYSTEM_FIRMWARE_PROGRESS;
  else if (sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
    return IPMI_MONITORING_SENSOR_GROUP_EVENT_LOGGING_DISABLED;
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
    return IPMI_MONITORING_SENSOR_GROUP_SYSTEM_EVENT;
  else if (sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
    return IPMI_MONITORING_SENSOR_GROUP_CRITICAL_INTERRUPT;
  else if (sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    return IPMI_MONITORING_SENSOR_GROUP_MODULE_BOARD;
  else if (sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
    return IPMI_MONITORING_SENSOR_GROUP_SLOT_CONNECTOR;
  else if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
    return IPMI_MONITORING_SENSOR_GROUP_WATCHDOG2;
  else if (sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
    return IPMI_MONITORING_SENSOR_GROUP_ENTITY_PRESENCE;
  else if (sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
    return IPMI_MONITORING_SENSOR_GROUP_MANAGEMENT_SUBSYSTEM_HEALTH;
  else if (sensor_type == IPMI_SENSOR_TYPE_BATTERY)
    return IPMI_MONITORING_SENSOR_GROUP_BATTERY;
  else if (sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
    return IPMI_MONITORING_SENSOR_GROUP_FRU_STATE;
  else if (sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
    return IPMI_MONITORING_SENSOR_GROUP_CABLE_INTERCONNECT;
  else if (sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
    return IPMI_MONITORING_SENSOR_GROUP_BOOT_ERROR;

  IPMI_MONITORING_DEBUG(("sensor_type '0x%X' not supported", sensor_type));
  return IPMI_MONITORING_SENSOR_GROUP_UNKNOWN;
}

static int
_full_record_sensor_reading(ipmi_monitoring_ctx_t c,
                            unsigned int sensor_reading_flags,
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            unsigned int *sensor_groups,
                            unsigned int sensor_groups_len,
                            uint16_t record_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  int sensor_group;
  uint64_t val;
  int rv = -1;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(!sensor_groups || sensor_groups_len);

  if (!(obj_sdr_record = Fiid_obj_create(c, tmpl_sdr_full_sensor_record)))
    goto cleanup;

  if (Fiid_obj_set_all(c,
                       obj_sdr_record,
                       sdr_record,
                       sdr_record_len) < 0)
    goto cleanup;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_type",
                   &val) < 0)
    goto cleanup;
  sensor_type = val;

  if ((sensor_group = _get_sensor_group(c, sensor_type)) < 0)
    goto cleanup;

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
        goto out;
    }
                                   
  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "event_reading_type_code",
                   &val) < 0)
    goto cleanup;
  event_reading_type_code = val;

  if (IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code))
    {
      if (_threshold_sensor_reading(c, 
                                    sensor_reading_flags, 
                                    record_id, 
                                    sensor_type,
                                    sensor_group,
                                    obj_sdr_record) < 0)
        goto cleanup;
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code))
    {
      if (_digital_sensor_reading(c, 
                                  sensor_reading_flags, 
                                  record_id, 
                                  event_reading_type_code,
                                  sensor_type,
                                  sensor_group,
                                  obj_sdr_record) < 0)
        goto cleanup;
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC(event_reading_type_code))
    {
      if (_specific_sensor_reading(c,
                                   sensor_reading_flags,
                                   record_id,
                                   sensor_type,
                                   sensor_group,
                                   obj_sdr_record) < 0)
        goto cleanup;
    }
  else
    {
      char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  
      IPMI_MONITORING_DEBUG(("full_record event_reading_type_code '0x%X' not supported", event_reading_type_code));

      if (_get_sensor_name(c,
                           obj_sdr_record,
                           sensor_name,
                           IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH) < 0)
        return -1;

      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           sensor_group,
                                           sensor_name,
                                           IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)                                          
        goto cleanup;
      goto out;
    }

 out:
  rv = 0;
 cleanup:
  if (obj_sdr_record)
    fiid_obj_destroy(obj_sdr_record);
  return rv;
}

static int
_compact_record_sensor_reading(ipmi_monitoring_ctx_t c,
                               unsigned int sensor_reading_flags,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               unsigned int *sensor_groups,
                               unsigned int sensor_groups_len,
                               uint16_t record_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  int sensor_group;
  uint64_t val;
  int rv = -1;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(!sensor_groups || sensor_groups_len);

  if (!(obj_sdr_record = Fiid_obj_create(c, tmpl_sdr_compact_sensor_record)))
    goto cleanup;

  if (Fiid_obj_set_all(c,
                       obj_sdr_record,
                       sdr_record,
                       sdr_record_len) < 0)
    goto cleanup;

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "sensor_type",
                   &val) < 0)
    goto cleanup;
  sensor_type = val;

  if ((sensor_group = _get_sensor_group(c, sensor_type)) < 0)
    goto cleanup;

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
        goto out;
    }

  if (Fiid_obj_get(c,
                   obj_sdr_record,
                   "event_reading_type_code",
                   &val) < 0)
    goto cleanup;
  event_reading_type_code = val;

  if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code))
    {
      if (_digital_sensor_reading(c, 
                                  sensor_reading_flags, 
                                  record_id, 
                                  event_reading_type_code,
                                  sensor_type,
                                  sensor_group,
                                  obj_sdr_record) < 0)
        goto cleanup;
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC(event_reading_type_code))
    {
      if (_specific_sensor_reading(c,
                                   sensor_reading_flags,
                                   record_id,
                                   sensor_type,
                                   sensor_group,
                                   obj_sdr_record) < 0)
        goto cleanup;
    }
  else
    {
      char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  
      IPMI_MONITORING_DEBUG(("compact_record event_reading_type_code '0x%X' not supported", event_reading_type_code));

      if (_get_sensor_name(c,
                           obj_sdr_record,
                           sensor_name,
                           IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH) < 0)
        return -1;

      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           sensor_group,
                                           sensor_name,
                                           IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        goto cleanup;
      goto out;
    }

 out:
  rv = 0;
 cleanup:
  if (obj_sdr_record)
    fiid_obj_destroy(obj_sdr_record);
  return rv;
}

static int
_get_sdr_record_id_and_type(ipmi_monitoring_ctx_t c, 
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            uint16_t *record_id,
                            uint8_t *record_type)
{
  fiid_obj_t obj_sdr_record_header = NULL;
  int32_t sdr_record_header_length;
  uint64_t val;
  int rv = -1;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(record_id);
  assert(record_type);

  if ((sdr_record_header_length = Fiid_template_len_bytes(c, tmpl_sdr_record_header)) < 0)
    goto cleanup;

  if (sdr_record_len < sdr_record_header_length)
    {
      IPMI_MONITORING_DEBUG(("invalid sdr length: %d", sdr_record_len));
      c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
      goto cleanup;
    }

  if (!(obj_sdr_record_header = Fiid_obj_create(c, tmpl_sdr_record_header)))
    goto cleanup;
  
  if (Fiid_obj_set_all(c,
                       obj_sdr_record_header,
                       sdr_record,
                       sdr_record_header_length) < 0)
    goto cleanup;
  
  if (Fiid_obj_get(c,
                   obj_sdr_record_header,
                   "record_id",
                   &val) < 0)
    goto cleanup;
  *record_id = val;

  if (Fiid_obj_get(c,
                   obj_sdr_record_header,
                   "record_type",
                   &val) < 0)
    goto cleanup;
  *record_type = val;

  rv = 0;
 cleanup:
  if (obj_sdr_record_header)
    fiid_obj_destroy(obj_sdr_record_header);
  return rv;
}

int 
ipmi_monitoring_get_sensor_reading(ipmi_monitoring_ctx_t c, 
                                   unsigned int sensor_reading_flags,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   unsigned int *sensor_groups,
                                   unsigned int sensor_groups_len)
{
  uint16_t record_id;
  uint8_t record_type;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sensor_readings);
  assert(c->ipmi_ctx);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(!sensor_groups || sensor_groups_len);

  if (_get_sdr_record_id_and_type(c, 
                                  sdr_record, 
                                  sdr_record_len, 
                                  &record_id,
                                  &record_type) < 0)
    return -1;
  
  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      if (_full_record_sensor_reading(c, 
                                      sensor_reading_flags, 
                                      sdr_record, 
                                      sdr_record_len,
                                      sensor_groups,
                                      sensor_groups_len,
                                      record_id) < 0)
        return -1;
    }
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      if (_compact_record_sensor_reading(c, 
                                         sensor_reading_flags, 
                                         sdr_record,
                                         sdr_record_len,
                                         sensor_groups,
                                         sensor_groups_len,
                                         record_id) < 0)
        return -1;
    }
  else /* IPMI_SDR_FORMAT_EVENT_ONLY_RECORD
          IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD
          IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD
          IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD
          IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD
          IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD
          IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD
          IPMI_SDR_FORMAT_BMC_MESAAGE_CHANNEL_INFO_RECORD
          IPMI_SDR_FORMAT_OEM_RECORD 
       */
    {
      IPMI_MONITORING_DEBUG(("record_type '0x%X' not supported", record_type));
      if (_store_unreadable_sensor_reading(c, 
                                           sensor_reading_flags, 
                                           record_id,
                                           IPMI_MONITORING_SENSOR_GROUP_UNKNOWN,
                                           NULL,
                                           IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) < 0)
        return -1;
    }

  return 0;
}
