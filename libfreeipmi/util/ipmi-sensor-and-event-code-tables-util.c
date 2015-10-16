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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-numbers-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

static char *_ipmi_event_message_separator = " ; ";

#define EVENT_BUFLEN       1024
#define EVENT_MAX_MESSAGES   16

/* 16th bit reserved, see Get Sensor Reading command in spec */
#define IPMI_MAX_SENSOR_AND_EVENT_OFFSET 15

int
ipmi_event_reading_type_code_class (uint8_t event_reading_type_code)
{
  if (IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD);

  if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE);

  if (IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE);

  if (IPMI_EVENT_READING_TYPE_CODE_IS_OEM (event_reading_type_code))
    return (IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM);

  return (IPMI_EVENT_READING_TYPE_CODE_CLASS_UNKNOWN);
}

static int
_snprintf (char *buf, unsigned int buflen, const char * const fmt, ...)
{
  int rv;
  va_list ap;

  assert (buf && buflen && fmt);

  va_start (ap, fmt);
  rv = vsnprintf (buf, buflen, fmt, ap);
  va_end (ap);

  return (rv);
}

static int
get_physical_security_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY_LAN_LEASH_LOST)
    return (_snprintf (buf, buflen, "Network controller #%d", event_data2));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_system_firmware_progress_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_SYSTEM_FIRMWARE_ERROR
      && event_data2 <= ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_error_max_index)
    return (_snprintf (buf, buflen, ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_error[event_data2]));
  if (offset == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_SYSTEM_FIRMWARE_HANG
      && event_data2 <= ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_hang_max_index)
    return (_snprintf (buf, buflen, ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_hang[event_data2]));
  if (offset == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS_SYSTEM_FIRMWARE_PROGRESS
      && event_data2 <= ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_progress_max_index)
    return (_snprintf (buf, buflen, ipmi_sensor_type_system_firmware_progress_event_data2_offset_system_firmware_progress[event_data2]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_event_logging_disabled_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_CORRECTABLE_MEMORY_ERROR_LOGGING_DISABLED)
    return (_snprintf (buf, buflen, "Memory module/device #%d", event_data2));

  if (offset == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_EVENT_TYPE_LOGGING_DISABLED)
    return (_snprintf (buf, buflen, "Event/Reading Type Code #%d", event_data2));

  if (offset == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_CORRECTABLE_MACHINE_CHECK_ERROR_LOGGING_DISABLED)
    return (_snprintf (buf, buflen, "Instance ID #%d", event_data2));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
_get_system_event_event_data2_message_offset_entry_added_to_auxiliary_log (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "log_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 4, "log_entry_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t log_type;
  uint8_t log_entry_action;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "log_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  log_type = val;

  if (FIID_OBJ_GET (obj, "log_entry_action", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  log_entry_action = val;

  if (log_type <= ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_entry_action_max_index)
    str1 = (char *)ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_entry_action[log_type];

  if (log_entry_action <= ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_type_max_index)
    str2 = (char *)ipmi_sensor_type_system_event_event_data2_offset_entry_added_to_auxiliary_log_log_type[log_entry_action];

  if (str1 || str2)
    rv = _snprintf (buf, buflen, "%s%s%s",
                    (str1 ? str1 : ""),
                    ((str1 && str2) ? _ipmi_event_message_separator : ""),
                    (str2 ? str2 : ""));

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
_strcat_pef_action (char *buf, unsigned int buflen, uint8_t flag, int str_len, int index)
{
  if (flag)
    {
      int len_temp;

      len_temp = strlen (ipmi_sensor_type_system_event_event_data2_offset_pef_action[index]);
      if (str_len)
        len_temp += strlen (_ipmi_event_message_separator);

      if ((str_len + len_temp) > buflen)
        {
          SET_ERRNO (ENOSPC);
          return (-1);
        }

      if (!str_len)
        strcat (buf, ipmi_sensor_type_system_event_event_data2_offset_pef_action[index]);
      else
        {
          strcat (buf, _ipmi_event_message_separator);
          strcat (buf, "%s");
        }

      return (str_len + len_temp);
    }
  return (str_len);
}

static int
_get_system_event_event_data2_message_offset_pef_action (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 1, "alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "power_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "power_cycle", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "oem_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "diagonstic_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t alert, power_off, reset, power_cycle, oem_action, diagnostic_interrupt;
  fiid_obj_t obj = NULL;
  int str_len = 0;
  int rv = -1;

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "alert", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  alert = val;

  if (FIID_OBJ_GET (obj, "power_off", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  power_off = val;

  if (FIID_OBJ_GET (obj, "reset", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  reset = val;

  if (FIID_OBJ_GET (obj, "power_cycle", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  power_cycle = val;

  if (FIID_OBJ_GET (obj, "oem_action", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  oem_action = val;

  if (FIID_OBJ_GET (obj, "diagnostic_interrupt", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  diagnostic_interrupt = val;

  memset (buf, '\0', buflen);

  if ((str_len = _strcat_pef_action (buf, buflen, alert, str_len, 0)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat_pef_action (buf, buflen, power_off, str_len, 1)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat_pef_action (buf, buflen, reset, str_len, 2)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat_pef_action (buf, buflen, power_cycle, str_len, 3)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat_pef_action (buf, buflen, oem_action, str_len, 4)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((str_len = _strcat_pef_action (buf, buflen, diagnostic_interrupt, str_len, 5)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
_get_system_event_event_data2_message_offset_timestamp_clock_synch (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "timestamp_clock_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "first_second", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t timestamp_clock_type;
  uint8_t first_second;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "timestamp_clock_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  timestamp_clock_type = val;

  if (FIID_OBJ_GET (obj, "first_second", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  first_second = val;

  if (timestamp_clock_type <= ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_timestamp_clock_type_max_index)
    str1 = (char *)ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_timestamp_clock_type[timestamp_clock_type];

  if (first_second <= ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_first_second_max_index)
    str2 = (char *)ipmi_sensor_type_system_event_event_data2_offset_timestamp_clock_synch_first_second[first_second];

  rv = _snprintf (buf, buflen, "%s%s%s",
                  str1 ? str1 : "",
                  _ipmi_event_message_separator,
                  str2 ? str2 : "");

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_system_event_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_SYSTEM_EVENT_ENTRY_ADDED_TO_AUXILIARY_LOG)
    return (_get_system_event_event_data2_message_offset_entry_added_to_auxiliary_log (offset, event_data2, buf, buflen));

  if (offset == IPMI_SENSOR_TYPE_SYSTEM_EVENT_PEF_ACTION)
    return (_get_system_event_event_data2_message_offset_pef_action (offset, event_data2, buf, buflen));

  if (offset == IPMI_SENSOR_TYPE_SYSTEM_EVENT_TIMESTAMP_CLOCK_SYNCH)
    return (_get_system_event_event_data2_message_offset_timestamp_clock_synch (offset, event_data2, buf, buflen));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_chip_set_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_CHIP_SET_SOFT_POWER_CONTROL_FAILURE
      && event_data2 <= ipmi_sensor_type_chip_set_event_data2_offset_soft_power_control_failure_max_index)
    return (_snprintf (buf, buflen, ipmi_sensor_type_chip_set_event_data2_offset_soft_power_control_failure[event_data2]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_system_boot_initiated_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED_SYSTEM_RESTART)
    {
      fiid_template_t tmpl_event_data2 =
        {
          { 4, "restart_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      fiid_obj_t obj = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create ( tmpl_event_data2)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "restart_cause", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (val <= ipmi_sensor_type_system_boot_initiated_event_data2_offset_system_restart_restart_cause_max_index)
        rv = _snprintf (buf, buflen, ipmi_sensor_type_system_boot_initiated_event_data2_offset_system_restart_restart_cause[val]);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_slot_connector_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 7, "slot_connector_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  fiid_obj_t obj = NULL;
  int rv = -1;

  assert (buf && buflen);

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "slot_connector_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (val <= ipmi_sensor_type_slot_connector_event_data2_offset_slot_holds_spare_device_slot_connector_type_max_index)
    rv = _snprintf (buf, buflen, ipmi_sensor_type_slot_connector_event_data2_offset_slot_holds_spare_device_slot_connector_type[val]);

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_watchdog2_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "timer_at_expiration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 4, "interrupt_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t timer_at_expiration;
  uint8_t interrupt_type;
  char *str1 = NULL;
  char *str2 = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  assert (buf && buflen);

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "timer_at_expiration", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  timer_at_expiration = val;

  if (FIID_OBJ_GET (obj, "interrupt_type", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  interrupt_type = val;

  if (timer_at_expiration <= ipmi_sensor_type_watchdog2_event_data2_timer_use_at_expiration_max_index)
    str1 = (char *)ipmi_sensor_type_watchdog2_event_data2_timer_use_at_expiration[timer_at_expiration];

  if (interrupt_type <= ipmi_sensor_type_watchdog2_event_data2_interrupt_type_max_index)
    str2 = (char *)ipmi_sensor_type_watchdog2_event_data2_interrupt_type[interrupt_type];

  if (str1 || str2)
    rv = _snprintf (buf, buflen, "%s%s%s",
                    (str1 ? str1 : ""),
                    ((str1 && str2) ? _ipmi_event_message_separator : ""),
                    (str2 ? str2 : ""));

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_management_subsystem_health_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  int rv = -1;

  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH_SENSOR_ACCESS_DEGRADED_OR_UNAVAILABLE
      || offset == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH_SENSOR_FAILURE)
    {
      rv = _snprintf (buf, buflen, "Sensor Number #%d", event_data2);
      return (rv);
    }
  else if (offset == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH_FRU_FAILURE)
    {
      fiid_template_t tmpl_event_data2 =
        {
          { 3, "private_bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 1, "fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      uint8_t private_bus_id, lun, fru_device;
      fiid_obj_t obj = NULL;
      char *str = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create (tmpl_event_data2)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "private_bus_id", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      private_bus_id = val;

      if (FIID_OBJ_GET (obj, "lun", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      lun = val;

      if (FIID_OBJ_GET (obj, "fru_device", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      fru_device = val;

      if (fru_device <= ipmi_sensor_type_management_subsystem_health_event_data2_offset_fru_failure_logical_fru_device_max_index)
        str = (char *)ipmi_sensor_type_management_subsystem_health_event_data2_offset_fru_failure_logical_fru_device[fru_device];

      rv = _snprintf (buf, buflen, "%s%sLUN for Master Write-Read command or FRU Command #%d%sPrivate bus ID #%d",
                      str ? str : "",
                      str ? _ipmi_event_message_separator : "",
                      lun,
                      _ipmi_event_message_separator,
                      private_bus_id);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_session_audit_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_SESSION_AUDIT_SESSION_ACTIVATED)
    {
      fiid_template_t tmpl_event_data2 =
        {
          { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      uint8_t user_id;
      fiid_obj_t obj = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create (tmpl_event_data2)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "user_id", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }
      user_id = val;

      if (!user_id)
        rv = _snprintf (buf, buflen, "User ID for user that activated session = Unspecified");
      else
        rv = _snprintf (buf, buflen, "User ID for user that activated session #%u", user_id);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_version_change_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (event_data2 <= ipmi_sensor_type_version_change_event_data2_offset_software_or_fw_change_detected_with_associated_entity_was_successful_version_change_type_max_index)
    return (_snprintf (buf, buflen, ipmi_sensor_type_version_change_event_data2_offset_software_or_fw_change_detected_with_associated_entity_was_successful_version_change_type[event_data2]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_fru_state_event_data2_message (unsigned int offset, uint8_t event_data2, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data2 =
    {
      { 4, "previous_state_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 4, "cause_of_state_change", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t previous_state_offset;
  uint8_t cause_of_state_change;
  char *previous_state_offset_str = NULL;
  char *cause_of_state_change_str = NULL;
  fiid_obj_t obj = NULL;
  int rv = -1;

  assert (buf && buflen);

  if (!(obj = fiid_obj_create (tmpl_event_data2)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj, &event_data2, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj, "previous_state_offset", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  previous_state_offset = val;

  if (FIID_OBJ_GET (obj, "cause_of_state_change", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  cause_of_state_change = val;

  if (previous_state_offset <= ipmi_sensor_type_fru_state_max_index)
    previous_state_offset_str = (char *)ipmi_sensor_type_fru_state[previous_state_offset];

  if (cause_of_state_change <= ipmi_sensor_type_fru_state_event_data2_offset_communication_lost_cause_of_state_change_max_index)
    cause_of_state_change_str = (char *)ipmi_sensor_type_fru_state_event_data2_offset_communication_lost_cause_of_state_change[cause_of_state_change];

  rv = _snprintf (buf,
                  buflen,
                  "Previous State = %s%s%s",
                  previous_state_offset_str ? previous_state_offset_str : "",
                  cause_of_state_change_str ? _ipmi_event_message_separator : "",
                  cause_of_state_change_str ? cause_of_state_change_str : "");

 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

static int
get_power_supply_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_POWER_SUPPLY_CONFIGURATION_ERROR)
    {
      fiid_template_t tmpl_event_data3 =
        {
          { 4, "event_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
          { 0, "", 0}
        };
      uint64_t val;
      fiid_obj_t obj = NULL;
      int rv = -1;

      if (!(obj = fiid_obj_create (tmpl_event_data3)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj, "event_type", &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj);
          goto cleanup;
        }

      if (val <= ipmi_sensor_type_power_supply_event_data3_offset_configuration_error_error_type_max_index)
        rv = _snprintf (buf, buflen, ipmi_sensor_type_power_supply_event_data3_offset_configuration_error_error_type[val]);

    cleanup:
      fiid_obj_destroy (obj);
      return (rv);
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_memory_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_MEMORY_SPARE)
    return (_snprintf (buf, buflen, "Memory module/device #%d", event_data3));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_event_logging_disabled_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  switch (offset)
    {
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_EVENT_TYPE_LOGGING_DISABLED:
      {
        fiid_template_t tmpl_event_data3 =
          {
            { 4, "event_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 1, "assertion_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 1, "logging_disabled_all_events", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 0, "", 0}
          };
        uint64_t val;
        uint8_t event_offset;
        uint8_t assertion_deassertion_event;
        uint8_t logging_disabled_all_events;
        fiid_obj_t obj = NULL;
        char *str1 = NULL;
        char *str2 = NULL;
        int rv = -1;

        if (!(obj = fiid_obj_create (tmpl_event_data3)))
          {
            ERRNO_TRACE (errno);
            goto cleanup1;
          }

        if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }

        if (FIID_OBJ_GET (obj, "event_offset", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }
        event_offset = val;

        if (FIID_OBJ_GET (obj, "assertion_deassertion_event", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }
        assertion_deassertion_event = val;

        if (FIID_OBJ_GET (obj, "logging_disabled_all_events", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup1;
          }
        logging_disabled_all_events = val;

        if (assertion_deassertion_event <= ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_assertion_event_max_index)
          str1 = (char *)ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_assertion_event[assertion_deassertion_event];

        if (logging_disabled_all_events <= ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_logging_disabled_all_events_max_index)
          str2 = (char *)ipmi_sensor_type_event_logging_disabled_event_data3_offset_event_type_logging_disabled_logging_disabled_all_events[logging_disabled_all_events];

        rv = _snprintf (buf, buflen, "Event Offset #%d%s%s%s%s",
                        event_offset,
                        (str1 || str2) ? _ipmi_event_message_separator : "",
                        (str1 ? str1 : ""),
                        ((str1 && str2 && strlen (str2)) ? _ipmi_event_message_separator : ""),
                        (str2 ? str2 : ""));

      cleanup1:
        fiid_obj_destroy (obj);
        return (rv);
      }
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_SEL_ALMOST_FULL:
      return (_snprintf (buf, buflen, "%d%% full", event_data3));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_CORRECTABLE_MACHINE_CHECK_ERROR_LOGGING_DISABLED:
      {
        fiid_template_t tmpl_event_data3 =
          {
            { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 1, "number_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
            { 0, "", 0}
          };
        uint64_t val;
        uint8_t number_type;
        char *str = NULL;
        fiid_obj_t obj = NULL;
        int rv = -1;
        
        if (!(obj = fiid_obj_create (tmpl_event_data3)))
          {
            ERRNO_TRACE (errno);
            goto cleanup2;
          }
        
        if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup2;
          }
        
        if (FIID_OBJ_GET (obj, "number_type", &val) < 0)
          {
            FIID_OBJECT_ERROR_TO_ERRNO (obj);
            goto cleanup2;
          }
        number_type = val;
        
        if (number_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED_EVENT_DATA3_OFFSET_CORRECTABLE_MACHINE_CHECK_ERROR_LOGGING_DISABLED_ENTITY_INSTANCE_NUMBER)
          str = "Entity Instance Number";
        else 
          str = "Vendor-specific Processor Number";

        rv = _snprintf (buf,
                        buflen,
                        "%s = #%d",
                        str,
                        event_data2);
        
      cleanup2:
        fiid_obj_destroy (obj);
        return (rv);
      }
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_chip_set_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_CHIP_SET_SOFT_POWER_CONTROL_FAILURE
      && event_data3 <= ipmi_sensor_type_chip_set_event_data3_offset_soft_power_control_failure_max_index)
    return (_snprintf (buf, buflen, ipmi_sensor_type_chip_set_event_data3_offset_soft_power_control_failure[event_data3]));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_system_boot_initiated_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED_SYSTEM_RESTART)
    return (_snprintf (buf, buflen, "Channel Number used to deliver command that generated restart #%d", event_data3));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_slot_connector_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  return (_snprintf (buf, buflen, "Slot/Connector #%d", event_data3));
}

static int
get_management_subsystem_health_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  assert (buf && buflen);

  if (offset == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH_FRU_FAILURE)
    return (_snprintf (buf, buflen, "FRU Device ID/Slave Address #%d", event_data3));

  SET_ERRNO (EINVAL);
  return (-1);
}

static int
get_session_audit_event_data3_message (unsigned int offset, uint8_t event_data2, uint8_t event_data3, char *buf, unsigned int buflen)
{
  fiid_template_t tmpl_event_data3 =
    {
      { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 2, "deactivation_cause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
      { 0, "", 0}
    };
  uint64_t val;
  uint8_t channel_number;
  uint8_t deactivation_cause;
  fiid_obj_t obj = NULL;
  char *str = NULL;
  int rv = -1;
  
  assert (buf && buflen);

  if (!(obj = fiid_obj_create (tmpl_event_data3)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  
  if (fiid_obj_set_all (obj, &event_data3, sizeof (uint8_t)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj, "channel_number", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  channel_number = val;
  
  if (FIID_OBJ_GET (obj, "deactivation_cause", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj);
      goto cleanup;
    }
  deactivation_cause = val;
  
  /* output deactivation case only if deactivation offset occurred */
  if (offset == IPMI_SENSOR_TYPE_SESSION_AUDIT_SESSION_DEACTIVATED)
    {
      if (deactivation_cause <= ipmi_sensor_type_session_audit_event_data3_offset_session_deactivated_deactivation_cause_max_index)
        str = (char *)ipmi_sensor_type_session_audit_event_data3_offset_session_deactivated_deactivation_cause[deactivation_cause];
    }
  
  rv = _snprintf (buf, buflen, "Channel number that session was activated/deactivated = %d%s%s",
                  channel_number,
                  (str) ? _ipmi_event_message_separator : "",
                  str ? str : "");
  
 cleanup:
  fiid_obj_destroy (obj);
  return (rv);
}

/***************************************************/

static int
_get_event_message (unsigned int offset,
                    char *buf,
                    unsigned int buflen,
                    unsigned int offset_max,
                    const char * const string_array[])
{
  assert (buf && buflen);

  if (offset > offset_max)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (snprintf (buf, buflen, "%s", string_array[offset]));
}

int
ipmi_event_message_separator (const char *separator)
{
  if (!separator)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  _ipmi_event_message_separator = (char *)separator;
  return (0);
}

int
ipmi_get_generic_event_message (uint8_t event_reading_type_code,
                                unsigned int offset,
                                char *buf,
                                unsigned int buflen)
{
  if (!buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (event_reading_type_code)
    {
    case IPMI_EVENT_READING_TYPE_CODE_THRESHOLD:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_threshold_max_index,
                                  ipmi_generic_event_reading_type_code_threshold));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_state_max_index,
                                  ipmi_generic_event_reading_type_code_transition_state));
    case IPMI_EVENT_READING_TYPE_CODE_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_state_max_index,
                                  ipmi_generic_event_reading_type_code_state));
    case IPMI_EVENT_READING_TYPE_CODE_PREDICTIVE_FAILURE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_predictive_failure_max_index,
                                  ipmi_generic_event_reading_type_code_predictive_failure));
    case IPMI_EVENT_READING_TYPE_CODE_LIMIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_limit_max_index,
                                  ipmi_generic_event_reading_type_code_limit));
    case IPMI_EVENT_READING_TYPE_CODE_PERFORMANCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_performance_max_index,
                                  ipmi_generic_event_reading_type_code_performance));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_severity_max_index,
                                  ipmi_generic_event_reading_type_code_transition_severity));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_present_max_index,
                                  ipmi_generic_event_reading_type_code_device_present));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_enabled_max_index,
                                  ipmi_generic_event_reading_type_code_device_enabled));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_AVAILABILITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_availability_max_index,
                                  ipmi_generic_event_reading_type_code_transition_availability));
    case IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_redundancy_max_index,
                                  ipmi_generic_event_reading_type_code_redundancy));
    case IPMI_EVENT_READING_TYPE_CODE_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_acpi_power_state_max_index,
                                  ipmi_generic_event_reading_type_code_acpi_power_state));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_sensor_type_message (uint8_t sensor_type,
                              unsigned int offset,
                              char *buf,
                              unsigned int buflen)
{
  if (!buf
      || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type)
    {
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_physical_security_max_index,
                                  ipmi_sensor_type_physical_security));
    case IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_platform_security_violation_attempt_max_index,
                                  ipmi_sensor_type_platform_security_violation_attempt));
    case IPMI_SENSOR_TYPE_PROCESSOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_processor_max_index,
                                  ipmi_sensor_type_processor));
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_power_supply_max_index,
                                  ipmi_sensor_type_power_supply));
    case IPMI_SENSOR_TYPE_POWER_UNIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_power_unit_max_index,
                                  ipmi_sensor_type_power_unit));
    case IPMI_SENSOR_TYPE_MEMORY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_memory_max_index,
                                  ipmi_sensor_type_memory));
    case IPMI_SENSOR_TYPE_DRIVE_SLOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_drive_slot_max_index,
                                  ipmi_sensor_type_drive_slot));
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_system_firmware_progress_max_index,
                                  ipmi_sensor_type_system_firmware_progress));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_event_logging_disabled_max_index,
                                  ipmi_sensor_type_event_logging_disabled));
    case IPMI_SENSOR_TYPE_WATCHDOG1:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_watchdog1_max_index,
                                  ipmi_sensor_type_watchdog1));
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_system_event_max_index,
                                  ipmi_sensor_type_system_event));
    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_critical_interrupt_max_index,
                                  ipmi_sensor_type_critical_interrupt));
    case IPMI_SENSOR_TYPE_BUTTON_SWITCH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_button_switch_max_index,
                                  ipmi_sensor_type_button_switch));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_chip_set_max_index,
                                  ipmi_sensor_type_chip_set));
    case IPMI_SENSOR_TYPE_CABLE_INTERCONNECT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_cable_interconnect_max_index,
                                  ipmi_sensor_type_cable_interconnect));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_system_boot_initiated_max_index,
                                  ipmi_sensor_type_system_boot_initiated));
    case IPMI_SENSOR_TYPE_BOOT_ERROR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_boot_error_max_index,
                                  ipmi_sensor_type_boot_error));
    case IPMI_SENSOR_TYPE_OS_BOOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_os_boot_max_index,
                                  ipmi_sensor_type_os_boot));
    case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_os_critical_stop_max_index,
                                  ipmi_sensor_type_os_critical_stop));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_slot_connector_max_index,
                                  ipmi_sensor_type_slot_connector));
    case IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_acpi_power_state_max_index,
                                  ipmi_sensor_type_acpi_power_state));
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_watchdog2_max_index,
                                  ipmi_sensor_type_watchdog2));
    case IPMI_SENSOR_TYPE_PLATFORM_ALERT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_platform_alert_max_index,
                                  ipmi_sensor_type_platform_alert));
    case IPMI_SENSOR_TYPE_ENTITY_PRESENCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_entity_presence_max_index,
                                  ipmi_sensor_type_entity_presence));
    case IPMI_SENSOR_TYPE_LAN:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_lan_max_index,
                                  ipmi_sensor_type_lan));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_management_subsystem_health_max_index,
                                  ipmi_sensor_type_management_subsystem_health));
    case IPMI_SENSOR_TYPE_BATTERY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_battery_max_index,
                                  ipmi_sensor_type_battery));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_session_audit_max_index,
                                  ipmi_sensor_type_session_audit));
    case IPMI_SENSOR_TYPE_VERSION_CHANGE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_version_change_max_index,
                                  ipmi_sensor_type_version_change));
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_fru_state_max_index,
                                  ipmi_sensor_type_fru_state));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_generic_event_message_short (uint8_t event_reading_type_code,
                                      unsigned int offset,
                                      char *buf,
                                      unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (event_reading_type_code)
    {
    case IPMI_EVENT_READING_TYPE_CODE_THRESHOLD:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_threshold_short_max_index,
                                  ipmi_generic_event_reading_type_code_threshold_short));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_state_short_max_index,
                                  ipmi_generic_event_reading_type_code_transition_state_short));
    case IPMI_EVENT_READING_TYPE_CODE_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_state_short_max_index,
                                  ipmi_generic_event_reading_type_code_state_short));
    case IPMI_EVENT_READING_TYPE_CODE_PREDICTIVE_FAILURE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_predictive_failure_short_max_index,
                                  ipmi_generic_event_reading_type_code_predictive_failure_short));
    case IPMI_EVENT_READING_TYPE_CODE_LIMIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_limit_short_max_index,
                                  ipmi_generic_event_reading_type_code_limit_short));
    case IPMI_EVENT_READING_TYPE_CODE_PERFORMANCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_performance_short_max_index,
                                  ipmi_generic_event_reading_type_code_performance_short));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_SEVERITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_severity_short_max_index,
                                  ipmi_generic_event_reading_type_code_transition_severity_short));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_present_short_max_index,
                                  ipmi_generic_event_reading_type_code_device_present_short));
    case IPMI_EVENT_READING_TYPE_CODE_DEVICE_ENABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_device_enabled_short_max_index,
                                  ipmi_generic_event_reading_type_code_device_enabled_short));
    case IPMI_EVENT_READING_TYPE_CODE_TRANSITION_AVAILABILITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_transition_availability_short_max_index,
                                  ipmi_generic_event_reading_type_code_transition_availability_short));
    case IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_redundancy_short_max_index,
                                  ipmi_generic_event_reading_type_code_redundancy_short));
    case IPMI_EVENT_READING_TYPE_CODE_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_generic_event_reading_type_code_acpi_power_state_short_max_index,
                                  ipmi_generic_event_reading_type_code_acpi_power_state_short));
    }

  return (-1);
}

int
ipmi_get_sensor_type_message_short (uint8_t sensor_type,
                                    unsigned int offset,
                                    char *buf,
                                    unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type)
    {
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_physical_security_short_max_index,
                                  ipmi_sensor_type_physical_security_short));
    case IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_platform_security_violation_attempt_short_max_index,
                                  ipmi_sensor_type_platform_security_violation_attempt_short));
    case IPMI_SENSOR_TYPE_PROCESSOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_processor_short_max_index,
                                  ipmi_sensor_type_processor_short));
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_power_supply_short_max_index,
                                  ipmi_sensor_type_power_supply_short));
    case IPMI_SENSOR_TYPE_POWER_UNIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_power_unit_short_max_index,
                                  ipmi_sensor_type_power_unit_short));
    case IPMI_SENSOR_TYPE_MEMORY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_memory_short_max_index,
                                  ipmi_sensor_type_memory_short));
    case IPMI_SENSOR_TYPE_DRIVE_SLOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_drive_slot_short_max_index,
                                  ipmi_sensor_type_drive_slot_short));
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_system_firmware_progress_short_max_index,
                                  ipmi_sensor_type_system_firmware_progress_short));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_event_logging_disabled_short_max_index,
                                  ipmi_sensor_type_event_logging_disabled_short));
    case IPMI_SENSOR_TYPE_WATCHDOG1:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_watchdog1_short_max_index,
                                  ipmi_sensor_type_watchdog1_short));
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_system_event_short_max_index,
                                  ipmi_sensor_type_system_event_short));
    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_critical_interrupt_short_max_index,
                                  ipmi_sensor_type_critical_interrupt_short));
    case IPMI_SENSOR_TYPE_BUTTON_SWITCH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_button_switch_short_max_index,
                                  ipmi_sensor_type_button_switch_short));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_chip_set_short_max_index,
                                  ipmi_sensor_type_chip_set_short));
    case IPMI_SENSOR_TYPE_CABLE_INTERCONNECT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_cable_interconnect_short_max_index,
                                  ipmi_sensor_type_cable_interconnect_short));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_system_boot_initiated_short_max_index,
                                  ipmi_sensor_type_system_boot_initiated_short));
    case IPMI_SENSOR_TYPE_BOOT_ERROR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_boot_error_short_max_index,
                                  ipmi_sensor_type_boot_error_short));
    case IPMI_SENSOR_TYPE_OS_BOOT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_os_boot_short_max_index,
                                  ipmi_sensor_type_os_boot_short));
    case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_os_critical_stop_short_max_index,
                                  ipmi_sensor_type_os_critical_stop_short));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_slot_connector_short_max_index,
                                  ipmi_sensor_type_slot_connector_short));
    case IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_acpi_power_state_short_max_index,
                                  ipmi_sensor_type_acpi_power_state_short));
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_watchdog2_short_max_index,
                                  ipmi_sensor_type_watchdog2_short));
    case IPMI_SENSOR_TYPE_PLATFORM_ALERT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_platform_alert_short_max_index,
                                  ipmi_sensor_type_platform_alert_short));
    case IPMI_SENSOR_TYPE_ENTITY_PRESENCE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_entity_presence_short_max_index,
                                  ipmi_sensor_type_entity_presence_short));
    case IPMI_SENSOR_TYPE_LAN:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_lan_short_max_index,
                                  ipmi_sensor_type_lan_short));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_management_subsystem_health_short_max_index,
                                  ipmi_sensor_type_management_subsystem_health_short));
    case IPMI_SENSOR_TYPE_BATTERY:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_battery_short_max_index,
                                  ipmi_sensor_type_battery_short));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_session_audit_short_max_index,
                                  ipmi_sensor_type_session_audit_short));
    case IPMI_SENSOR_TYPE_VERSION_CHANGE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_version_change_short_max_index,
                                  ipmi_sensor_type_version_change_short));
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (_get_event_message (offset,
                                  buf,
                                  buflen,
                                  ipmi_sensor_type_fru_state_short_max_index,
                                  ipmi_sensor_type_fru_state_short));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_event_data2_message (uint8_t sensor_type,
                              unsigned int offset,
                              uint8_t event_data2,
                              char *buf,
                              unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type)
    {
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (get_physical_security_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (get_system_firmware_progress_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (get_event_logging_disabled_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (get_system_event_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (get_chip_set_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (get_system_boot_initiated_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (get_slot_connector_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (get_watchdog2_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (get_management_subsystem_health_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (get_session_audit_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_VERSION_CHANGE:
      return (get_version_change_event_data2_message (offset, event_data2, buf, buflen));
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (get_fru_state_event_data2_message (offset, event_data2, buf, buflen));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_event_data3_message (uint8_t sensor_type,
                              unsigned int offset,
                              uint8_t event_data2,
                              uint8_t event_data3,
                              char *buf,
                              unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  switch (sensor_type)
    {
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (get_power_supply_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_MEMORY:
      return (get_memory_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (get_event_logging_disabled_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (get_chip_set_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (get_system_boot_initiated_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (get_slot_connector_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (get_management_subsystem_health_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (get_session_audit_event_data3_message (offset, event_data2, event_data3, buf, buflen));
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_oem_generic_event_message (uint32_t manufacturer_id,
                                    uint16_t product_id,
                                    uint8_t event_reading_type_code,
                                    unsigned int offset,
                                    char *buf,
                                    unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   * Dell Poweredge R720
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710
          || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720))
    {
      switch (event_reading_type_code)
        {
        case IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_generic_event_reading_type_code_oem_dell_status_max_index,
                                      ipmi_generic_event_reading_type_code_oem_dell_status));
        case IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_FAILURE:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_generic_event_reading_type_code_oem_dell_failure_max_index,
                                      ipmi_generic_event_reading_type_code_oem_dell_failure));
        }
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_oem_sensor_type_message (uint32_t manufacturer_id,
                                  uint16_t product_id,
                                  uint8_t sensor_type,
				  uint8_t sensor_number,
                                  unsigned int offset,
                                  char *buf,
                                  unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   * Dell Poweredge R720
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710
	  || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720))
    {
      switch (sensor_type)
        {
        case IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_dell_system_performance_degradation_status_max_index,
                                      ipmi_sensor_type_oem_dell_system_performance_degradation_status));
        case IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_dell_link_tuning_max_index,
                                      ipmi_sensor_type_oem_dell_link_tuning));
        case IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_dell_non_fatal_error_max_index,
                                      ipmi_sensor_type_oem_dell_non_fatal_error));
        case IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_dell_fatal_io_error_max_index,
                                      ipmi_sensor_type_oem_dell_fatal_io_error));
        case IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_dell_upgrade_max_index,
                                      ipmi_sensor_type_oem_dell_upgrade));
        }
    }

  /* 
   * OEM Interpretation
   *
   * Fujitsu iRMC S1 / iRMC S2
   *
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU
      && (product_id >= IPMI_FUJITSU_PRODUCT_ID_MIN 
          && product_id <= IPMI_FUJITSU_PRODUCT_ID_MAX))
    {
      switch (sensor_type)
        {
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_I2C_BUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_i2c_bus_max_index,
                                      ipmi_sensor_type_oem_fujitsu_i2c_bus));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_SYSTEM_POWER_CONSUMPTION:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_system_power_consumption_max_index,
                                      ipmi_sensor_type_oem_fujitsu_system_power_consumption));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_STATUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_memory_status_max_index,
                                      ipmi_sensor_type_oem_fujitsu_memory_status));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_CONFIG:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_memory_config_max_index,
                                      ipmi_sensor_type_oem_fujitsu_memory_config));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_memory_max_index,
                                      ipmi_sensor_type_oem_fujitsu_memory));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_HW_ERROR:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_hw_error_max_index,
                                      ipmi_sensor_type_oem_fujitsu_hw_error));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_SYS_ERROR:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_sys_error_max_index,
                                      ipmi_sensor_type_oem_fujitsu_sys_error));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_FAN_STATUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_fan_status_max_index,
                                      ipmi_sensor_type_oem_fujitsu_fan_status));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_STATUS:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_psu_status_max_index,
                                      ipmi_sensor_type_oem_fujitsu_psu_status));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_REDUNDANCY:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_psu_redundancy_max_index,
                                      ipmi_sensor_type_oem_fujitsu_psu_redundancy));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_FLASH:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_flash_max_index,
                                      ipmi_sensor_type_oem_fujitsu_flash));
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_CONFIG_BACKUP:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_fujitsu_config_backup_max_index,
                                      ipmi_sensor_type_oem_fujitsu_config_backup));
        /* These are reserved */
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_COMMUNICATION:
        case IPMI_SENSOR_TYPE_OEM_FUJITSU_EVENT:
        default:
          break;
        }
    }

  /* OEM Interpretation
   *
   * Wistron / Dell Poweredge C6220
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON
      && product_id == IPMI_WISTRON_PRODUCT_ID_C6220)
    {
      switch (sensor_type)
        {
	case IPMI_SENSOR_TYPE_OEM_WISTRON_IOH_CORE_ERROR:
          return (_get_event_message (offset,
                                      buf,
                                      buflen,
                                      ipmi_sensor_type_oem_wistron_ioh_core_error_max_index,
                                      ipmi_sensor_type_oem_wistron_ioh_core_error));
        }
    }

  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  /* 
   * achu: Ugh .. vendor uses same sensor type for multiple OEM
   * sensors ... gotta use sensor number to differentiate.  This is
   * awful.
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
      && product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL
      && sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC)
    {
      unsigned int sensor_reading = (1 << offset);
      if (sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_CPU_SEL_STATUS)
	{
	  if (sensor_reading & IPMI_SENSOR_TYPE_OEM_INTEL_SEL_CLEAR_BITMASK)
	    return (snprintf (buf, buflen, "SEL Clear"));
	  else if (sensor_reading & IPMI_SENSOR_TYPE_OEM_INTEL_SEL_ROLLOVER_BITMASK)
	    return (snprintf (buf, buflen, "SEL Rollover"));
	}
      else if (sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS)
	{
	  /* achu: A/C Lost is 4h, not 3h, so the below may not be correct.  Had to guess */  
	  if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_POWER_DOWN))
	    return (snprintf (buf, buflen, "Power Down"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_POWER_CYCLE_RESET))
	    return (snprintf (buf, buflen, "Power Cycle/Reset"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_POWER_ON))
	    return (snprintf (buf, buflen, "Power On"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS_AC_LOST))
	    return (snprintf (buf, buflen, "A/C Lost"));
	}
      else if (sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW)
	{
	  if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_NONE_OF_THE_ABOVE))
	    return (snprintf (buf, buflen, "Active status bits are waiting to be read"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_CML_ERROR))
	    return (snprintf (buf, buflen, "An error was detected on the I2C/PMBus interface"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_VIN_UV_FAULT))
	    return (snprintf (buf, buflen, "An undervoltage input fault was detected on the UV pin"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_IOUT_OC_FAULT))
	    return (snprintf (buf, buflen, "The hot swap controller detected an overcurrent condition"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_LOW_HOTSWAP_OFF))
	    return (snprintf (buf, buflen, "The hot swap gate driver output is disabled"));
	}
      else if (sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH)
	{
	  if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_POWER_GOOD))
	    return (snprintf (buf, buflen, "The voltage on the FLB pin is below the required threshold"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_MFR_STATUS))
	    return (snprintf (buf, buflen, "There are one or more active status bits to be read by STATUS_MFR_SPECIFIC"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_INPUT_STATUS))
	    return (snprintf (buf, buflen, "There are one or more active status bits to be read by STATUS_INPUT"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_IOUT_STATUS))
	    return (snprintf (buf, buflen, "There are one or more active status bits to be read by STATUS_IOUT"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH_VOUT_STATUS))
	    return (snprintf (buf, buflen, "There are one or more active status bits to be read by STATUS_VOUT"));
	}
      else if (sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC)
	{
	  if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_IOUT_WARN2))
	    return (snprintf (buf, buflen, "An undercurrent or overcurrent condition on the output supply detected"));
	  /* achu: HS_SHUTDOWN_CAUSE1 & HS_SHUTDOWN_CAUSE2 list 4 error messages
	   * with <00>, <01>, <10>, & <11> listed next to them.  I have no idea
	   * where these other bits come from.
	   *
	   * So all user gets is a generic "hotswap shutdown"
	   */
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_SHUTDOWN_CAUSE1))
	    return (snprintf (buf, buflen, "Hotswap shutdown"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_SHUTDOWN_CAUSE2))
	    return (snprintf (buf, buflen, "Hotswap shutdown"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_HS_INLIM))
	    return (snprintf (buf, buflen, "The ADM1276 has actively limited current into the load"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_OV_CMP_OUT))
	    return (snprintf (buf, buflen, "Input Voltage to OV pin is above threshold"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_UV_CMP_OUT))
	    return (snprintf (buf, buflen, "Input voltage to UV pin is below threshold"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC_FET_HEALTH_BAD))
	    return (snprintf (buf, buflen, "FET behavior suggests that the FET may be shorted"));
	}
      else if (sensor_number == IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT)
	{
	  if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_PIN_OP_WARN))
	    return (snprintf (buf, buflen, "An overpower condition on the input supply was detected by power monitor"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_UV_FAULT))
	    return (snprintf (buf, buflen, "An undervoltage was detected on the UV pin"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_UV_WARN))
	    return (snprintf (buf, buflen, "An undervoltage condition on the input supply was detected by the power monitor"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_OV_WARN))
	    return (snprintf (buf, buflen, "An overvoltage condition on the input supply was detected by hte power monitor"));
	  else if (sensor_reading & (1 << IPMI_SENSOR_TYPE_OEM_INTEL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT_VIN_OV_FAULT))
	    return (snprintf (buf, buflen, "An overvoltage was detected on the OV pin"));
	}
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_oem_specific_message (uint32_t manufacturer_id,
                               uint16_t product_id,
                               uint8_t event_reading_type_code,
                               uint8_t sensor_type,
                               unsigned int offset,
                               char *buf,
                               unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  /* OEM Interpretation
   *
   * HP Proliant DL160 G8
   */
  if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_HP)
    {
      if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_HP_UID_LIGHT
	  && sensor_type == IPMI_SENSOR_TYPE_OEM_HP_LED)
	return (_get_event_message (offset,
				    buf,
				    buflen,
				    ipmi_oem_hp_uid_light_max_index,
				    ipmi_oem_hp_uid_light));
      
      if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_HP_HEALTH_LED
	  && sensor_type == IPMI_SENSOR_TYPE_OEM_HP_LED)
	return (_get_event_message (offset,
				    buf,
				    buflen,
				    ipmi_oem_hp_health_led_max_index,
				    ipmi_oem_hp_health_led));
    }
  /* OEM Interpretation
   *
   * Intel S5500WB/Penguin Computing Relion 700
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   * Intel S2600JF/Appro 512X
   */
  else if (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
    {
      if (product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
	{
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_specific_pci_fatal_sensor_max_index,
					ipmi_oem_intel_specific_pci_fatal_sensor));
	  
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_specific_pci_correctable_sensor_max_index,
					ipmi_oem_intel_specific_pci_correctable_sensor));
	}
      else if (product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
	{
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_quanta_qssc_s4r_specific_pci_fatal_sensor_max_index,
					ipmi_oem_intel_quanta_qssc_s4r_specific_pci_fatal_sensor));
	  
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_quanta_qssc_s4r_specific_pci_correctable_sensor_max_index,
					ipmi_oem_intel_quanta_qssc_s4r_specific_pci_correctable_sensor));
	}
      else if (product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
	{
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_s2600jf_specific_pci_fatal_error_max_index,
					ipmi_oem_intel_s2600jf_specific_pci_fatal_error));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_s2600jf_specific_pci_fatal_error_2_max_index,
					ipmi_oem_intel_s2600jf_specific_pci_fatal_error_2));
	  
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_s2600jf_specific_pci_correctable_error_max_index,
					ipmi_oem_intel_s2600jf_specific_pci_correctable_error));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_s2600jf_specific_opi_fatal_error_max_index,
					ipmi_oem_intel_s2600jf_specific_opi_fatal_error));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR_2
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2_max_index,
					ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2));
	  
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_QPI_LINK_WIDTH_REDUCED
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_s2600jf_specific_qpi_link_width_reduced_max_index,
					ipmi_oem_intel_s2600jf_specific_qpi_link_width_reduced));
	}
      else if (product_id == IPMI_INTEL_PRODUCT_ID_S2600KP
	       || product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2
	       || product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
	{
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_max_index,
					ipmi_oem_intel_e52600v3_specific_qpi_fatal_error));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR_2
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_2_max_index,
					ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_2));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_LINK_WIDTH_REDUCED
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_qpi_link_width_reduced_max_index,
					ipmi_oem_intel_e52600v3_specific_qpi_link_width_reduced));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_max_index,
					ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS_2
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_2_max_index,
					ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_2));
	    
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_CORRECTABLE_ERRORS
	      && sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_pci_express_correctable_errors_max_index,
					ipmi_oem_intel_e52600v3_specific_pci_express_correctable_errors));
 
	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_FIRMWARE_UPDATE_STATUS_SENSOR
	      && sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_firmware_update_status_sensor_max_index,
					ipmi_oem_intel_e52600v3_specific_firmware_update_status_sensor));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_START
	      && sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_bios_recovery_start_max_index,
					ipmi_oem_intel_e52600v3_specific_bios_recovery_start));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_FINISH
	      && sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_bios_recovery_finish_max_index,
					ipmi_oem_intel_e52600v3_specific_bios_recovery_finish));

	  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO
	      && sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO)
	    return (_get_event_message (offset,
					buf,
					buflen,
					ipmi_oem_intel_e52600v3_specific_ierr_recovery_dump_info_max_index,
					ipmi_oem_intel_e52600v3_specific_ierr_recovery_dump_info));
	}
    }
  
  SET_ERRNO (EINVAL);
  return (-1);
}

static int
_supermicro_oem_temp_level_sensor_supported (uint32_t manufacturer_id, uint16_t product_id)
{
  /* OEM Interpretation
   *
   * Supermicro X7DBR-3 (X7DBR_3)
   * Supermicro X7DB8
   * Supermicro X8DTN
   * Supermicro X7SBI-LN4 (X7SBI_LN4)
   * Supermicro X8DTH
   * Supermicro X8DTG
   * Supermicro X8DTU
   * Supermicro X8DT3-LN4F (X8DT3_LN4F)
   * Supermicro X8DTU-6+ (X8DTU_6PLUS)
   * Supermicro X8DTL
   * Supermicro X8DTL-3F (X8DTL_3F)
   * Supermicro X8SIL-F  (X8SIL_F)
   * Supermicro X9SCL
   * Supermicro X9SCM
   * Supermicro X8DTN+-F (X8DTNPLUS_F)
   * Supermicro X8SIE
   * Supermicro X9SCA-F-O (X9SCA_F_O)
   * Supermicro H8DGU-F (H8DGU_F)
   * Supermicro X9DRi-F (X9DRI_F)
   * Supermicro X9DRI-LN4F+ (X9DRI_LN4F_PLUS)
   * Supermicro X9SPU-F-O (X9SPU_F_O)
   * Supermicro X9SCM-iiF (X9SCM_IIF)
   *
   * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC
   * Sensor Type = IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP
   * - 0 = Low
   * - 1 = Medium
   * - 2 = High
   * - 4 = Overheat
   * - 7 = Not Installed
   *
   * Note: Early Supermicro motherboards used the "Peppercon" Manufacturer ID
   * Note: Some Supermicro motherboards are rebranded with random manufacturer IDs
   */
  if ((manufacturer_id == IPMI_IANA_ENTERPRISE_ID_PEPPERCON
       && (product_id == IPMI_SUPERMICRO_PRODUCT_ID_X7DBR_3
	   || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X7DB8
	   || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTN
	   || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X7SBI_LN4))
      || ((manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
	   || manufacturer_id ==  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
	  && (product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTH
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTG
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTU
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DT3_LN4F
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTU_6PLUS
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL_3F
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8SIL_F
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCL
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCM
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTNPLUS_F
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8SIE
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCA_F_O
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DGU_F
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DGU
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DG6
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_F
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_LN4F_PLUS
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SPU_F_O
	      || product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCM_IIF))
      || (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_MAGNUM_TECHNOLOGIES
	  && product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL))
    return (1);

  return (0);
}

int
ipmi_get_oem_event_bitmask_message (uint32_t manufacturer_id,
                                    uint16_t product_id,
                                    uint8_t event_reading_type_code,
                                    uint8_t sensor_type,
                                    uint16_t event_bitmask,
                                    char *buf,
                                    unsigned int buflen)
{
  if (!buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* OEM Interpretation
   *
   * See notes in _supermicro_oem_temp_level_sensor_supported()
   */
  if (_supermicro_oem_temp_level_sensor_supported (manufacturer_id, product_id))
    {
      switch (event_reading_type_code)
	{
	case IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC:
	  {
	    switch (sensor_type)
	      {
	      case IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP:
		{
		  switch (event_bitmask)
		    {
		    case IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_LOW:
		      return (snprintf (buf, buflen, "Low"));
		    case IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_MEDIUM:
		      return (snprintf (buf, buflen, "Medium"));
		    case IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_HIGH:
		      return (snprintf (buf, buflen, "High"));
		    case IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_OVERHEAT:
		      return (snprintf (buf, buflen, "Overheat"));
		    case IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_NOT_INSTALLED:
		      return (snprintf (buf, buflen, "Not Installed"));
		    }
		}
		break;
	      }
	  }
	  break;
	  /* end case IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC: */
	}
    }

  SET_ERRNO (EINVAL);
  return (-1);
}

int
ipmi_get_event_messages (uint8_t event_reading_type_code,
                         uint8_t sensor_type, /* ignored if not relevant for event_reading_type_code */
			 uint8_t sensor_number, /* ignored if not relevant for event_reading_type_code or sensor_type */
                         uint16_t event_bitmask, /* ignored if not relevant for event_reading_type_code */
                         uint32_t manufacturer_id, /* ignored if INTERPRET_OEM_DATA not set */
                         uint16_t product_id, /* ignored if INTERPRET_OEM_DATA not set */
                         char ***event_messages,
                         unsigned int *event_messages_count,
                         const char *no_event_message_string,
                         unsigned int flags)
{
  char **tmp_event_messages_ptr = NULL;
  char *tmp_event_messages[EVENT_MAX_MESSAGES];
  unsigned int tmp_event_messages_count = 0;
  char buf[EVENT_BUFLEN + 1];
  int event_reading_type_code_class;
  uint16_t bitmask;
  unsigned int i;
  int len;

  if (!event_messages
      || !event_messages_count
      || (flags & ~(IPMI_GET_EVENT_MESSAGES_FLAGS_SHORT
                    | IPMI_GET_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
                    | IPMI_GET_EVENT_MESSAGES_FLAGS_SENSOR_READING
		    | IPMI_GET_EVENT_MESSAGES_FLAGS_IGNORE_UNRECOGNIZED_EVENTS)))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  memset (buf, '\0', EVENT_BUFLEN + 1);

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD
      && flags & IPMI_GET_EVENT_MESSAGES_FLAGS_SENSOR_READING)
    {
      int j;

      /* achu: multiple threshold flags can be set (e.g. if we pass
       * the critical threshold, we've also passed the non-critical
       * threshold).  It makes no sense to output multiple in this
       * case, so one message is returned at the max.  Luckily for us
       * (and due to smarts by the IPMI specification authors) if we
       * go from high bits to low bits, we will read the flags in the
       * correct order for output.
       *
       * If you're confused why were use 'ipmi_get_threshold_message'
       * instead of 'ipmi_get_generic_event_message' (b/c this is
       * presumably event_reading_type_code == 0x01), the reason is
       * b/c this is for sensors, not sel events.  In other words, the
       * result we care about comes from the Get Sensor Reading
       * command.
       */

      /* use 'j' instead of 'i', b/c needs to be signed integer */
      for (j = 5; j >= 0; j--)
        {
          bitmask = 0x1 << j;
          
          if (event_bitmask & bitmask)
            {
              memset (buf, '\0', EVENT_BUFLEN + 1);
              
              if ((len = ipmi_get_threshold_message (j,
                                                     buf,
                                                     EVENT_BUFLEN)) < 0)
                goto cleanup;
              
              if (len)
                {
                  if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
                    {
                      SET_ERRNO (ENOMEM);
                      goto cleanup;
                    }

                  tmp_event_messages_count++;
                  break;
                }
            }
        }
    }
  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD
           || event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
           || event_reading_type_code_class ==  IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE
           || (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM
               && flags & IPMI_GET_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
               && ((manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
                    && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
                        || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
                    && event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS))))

    {
      for (i = 0; i < IPMI_MAX_SENSOR_AND_EVENT_OFFSET; i++)
        {
          bitmask = 0x1 << i;

          if (event_bitmask & bitmask)
            {
              memset (buf, '\0', EVENT_BUFLEN + 1);

              if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD
                  || event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE)
                {
                  if (flags & IPMI_GET_EVENT_MESSAGES_FLAGS_SHORT)
                    len = ipmi_get_generic_event_message_short (event_reading_type_code,
                                                                i,
                                                                buf,
                                                                EVENT_BUFLEN);
                  else
                    len = ipmi_get_generic_event_message (event_reading_type_code,
                                                          i,
                                                          buf,
                                                          EVENT_BUFLEN);
                }
              else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
                {
                  if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
                    {
                      if (flags & IPMI_GET_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
                          && ((manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
                               && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
                                   || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
                               && (sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS
                                   || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
                                   || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
                                   || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR
                                   || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE)
			       )
                              || (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU
                                  && (product_id >= IPMI_FUJITSU_PRODUCT_ID_MIN
                                      && product_id <= IPMI_FUJITSU_PRODUCT_ID_MAX)
                                  && (sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_I2C_BUS
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_SYSTEM_POWER_CONSUMPTION
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_STATUS
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_CONFIG
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_FAN_STATUS
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_STATUS
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_REDUNDANCY
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_CONFIG_BACKUP
                                      /* These are for events only --begin */
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_FLASH
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_EVENT
                                      || sensor_type == IPMI_SENSOR_TYPE_OEM_FUJITSU_COMMUNICATION
                                      /* These are for events only --end */
                                      )
				  )
			      || (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
				  && (product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
				  && (sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC)
				  )
			      )
			  )
                        {
                          len = ipmi_get_oem_sensor_type_message (manufacturer_id,
                                                                  product_id,
                                                                  sensor_type,
								  sensor_number,
                                                                  i,
                                                                  buf,
                                                                  EVENT_BUFLEN);
                        }
                      else
                        goto oem_default_output;
                    }
                  else
                    {
                      if (flags & IPMI_GET_EVENT_MESSAGES_FLAGS_SHORT)
                        len = ipmi_get_sensor_type_message_short (sensor_type,
                                                                  i,
                                                                  buf,
                                                                  EVENT_BUFLEN);
                      else
                        len = ipmi_get_sensor_type_message (sensor_type,
                                                            i,
                                                            buf,
                                                            EVENT_BUFLEN);
                    }
                }
              else
                len = ipmi_get_oem_generic_event_message (manufacturer_id,
                                                          product_id,
                                                          event_reading_type_code,
                                                          i,
                                                          buf,
                                                          EVENT_BUFLEN);

	      if (len < 0)
		{
		  if (!(flags & IPMI_GET_EVENT_MESSAGES_FLAGS_IGNORE_UNRECOGNIZED_EVENTS))
		    {
		      snprintf (buf,
				EVENT_BUFLEN,
				"Unrecognized Event = %04Xh",
				bitmask);
		      
		      if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
			{
			  SET_ERRNO (ENOMEM);
			  goto cleanup;
			}
		      
		      tmp_event_messages_count++;
		      continue;
		    }
		  else
		    continue;
		}
              
              if (len)
                {
                  if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
                    {
                      SET_ERRNO (ENOMEM);
                      goto cleanup;
                    }
                  
                  tmp_event_messages_count++;
                  continue;
                }
            }
        }
    }
  /* OEM Interpretation
   *
   * See notes in _supermicro_oem_temp_level_sensor_supported()
   */
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM
           && flags & IPMI_GET_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
	   && _supermicro_oem_temp_level_sensor_supported (manufacturer_id, product_id)
           && event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC)
    {
      len = ipmi_get_oem_event_bitmask_message (manufacturer_id,
                                                product_id,
                                                event_reading_type_code,
                                                sensor_type,
                                                event_bitmask,
                                                buf,
                                                EVENT_BUFLEN);
      
      if (len > 0)
        {
          if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
            {
              SET_ERRNO (ENOMEM);
              goto cleanup;
            }
          
          tmp_event_messages_count++;
        }
      else
        goto oem_default_output;
    }
  /* OEM Interpretation
   *
   * HP Proliant DL160 G8
   */
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM
           && flags & IPMI_GET_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA
	   && manufacturer_id == IPMI_IANA_ENTERPRISE_ID_HP
	   && product_id == IPMI_HP_PRODUCT_ID_PROLIANT_DL160_G8
	   && (sensor_type == IPMI_SENSOR_TYPE_OEM_HP_LED
	       && (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_HP_UID_LIGHT
		   || event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_HP_HEALTH_LED)))
    {
      for (i = 0; i < IPMI_MAX_SENSOR_AND_EVENT_OFFSET; i++)
        {
          bitmask = 0x1 << i;

          if (event_bitmask & bitmask)
            {
	      len = ipmi_get_oem_specific_message (manufacturer_id,
						   product_id,
						   event_reading_type_code,
						   sensor_type,
						   i,
						   buf,
						   EVENT_BUFLEN);

	      if (len > 0)
		{
		  if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
		    {
		      SET_ERRNO (ENOMEM);
		      goto cleanup;
		    }
		  
		  tmp_event_messages_count++;
		  break;
		}
	      else
		goto oem_default_output;
	    }
	}
    }
  else /* OEM Event */
    {
    oem_default_output:

      memset (buf, '\0', EVENT_BUFLEN + 1);
      
      snprintf (buf,
                EVENT_BUFLEN,
                "OEM Event = %04Xh",
                event_bitmask);
      
      if (!(tmp_event_messages[tmp_event_messages_count] = strdup (buf)))
        {
          SET_ERRNO (ENOMEM);
          goto cleanup;
        }

      tmp_event_messages_count++;
    }

  if (!tmp_event_messages_count
      && no_event_message_string)
    {     
      if (!(tmp_event_messages[0] = strdup (no_event_message_string)))
        {
          SET_ERRNO (ENOMEM);
          goto cleanup;
        }
      
      tmp_event_messages_count++;
    }
  
  if (tmp_event_messages_count)
    {
      if (!(tmp_event_messages_ptr = (char **) malloc (sizeof (char *) * (tmp_event_messages_count + 1))))
        {
          SET_ERRNO (ENOMEM);
          goto cleanup;
        }
      
      for (i = 0; i < tmp_event_messages_count; i++)
        tmp_event_messages_ptr[i] = tmp_event_messages[i];
      
      tmp_event_messages_ptr[tmp_event_messages_count] = NULL;
    }

  (*event_messages) = tmp_event_messages_ptr;
  (*event_messages_count) = tmp_event_messages_count;
  
  return (0);

 cleanup:
  free (tmp_event_messages_ptr);
  for (i = 0; i < tmp_event_messages_count; i++)
    free (tmp_event_messages[i]);
  return (-1);
}
