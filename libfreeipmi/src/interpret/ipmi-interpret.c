/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"

#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"

#include "ipmi-interpret-defs.h"
#include "ipmi-interpret-trace.h"
#include "ipmi-interpret-config.h"
#include "ipmi-interpret-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

static char *ipmi_interpret_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "permission denied",
    "sel config file does not exist",
    "sel config file parse error",
    "sensor config file does not exist",
    "sensor config file parse error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_interpret_ctx_t
ipmi_interpret_ctx_create (void)
{
  struct ipmi_interpret_ctx *ctx = NULL;

  if (!(ctx = (ipmi_interpret_ctx_t)malloc (sizeof (struct ipmi_interpret_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_interpret_ctx));
  ctx->magic = IPMI_INTERPRET_CTX_MAGIC;
  ctx->flags = IPMI_INTERPRET_FLAGS_DEFAULT;

  if (ipmi_interpret_sel_init (ctx) < 0)
    goto cleanup;

  if (ipmi_interpret_sensor_init (ctx) < 0)
    goto cleanup;

  return (ctx);

 cleanup:
  if (ctx)
    {
      ipmi_interpret_sel_destroy (ctx);
      ipmi_interpret_sensor_destroy (ctx);
      free (ctx);
    }
  return (NULL);
}

void
ipmi_interpret_ctx_destroy (ipmi_interpret_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    return;

  ipmi_interpret_sel_destroy (ctx);
  ipmi_interpret_sensor_destroy (ctx);

  ctx->magic = ~IPMI_INTERPRET_CTX_MAGIC;
  free (ctx);
}

int
ipmi_interpret_ctx_errnum (ipmi_interpret_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_INTERPRET_ERR_CONTEXT_NULL);
  else if (ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    return (IPMI_INTERPRET_ERR_CONTEXT_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_interpret_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_INTERPRET_ERR_SUCCESS && errnum <= IPMI_INTERPRET_ERR_ERRNUMRANGE)
    return (ipmi_interpret_errmsgs[errnum]);
  else
    return (ipmi_interpret_errmsgs[IPMI_INTERPRET_ERR_ERRNUMRANGE]);
}

char *
ipmi_interpret_ctx_errormsg (ipmi_interpret_ctx_t ctx)
{
  return (ipmi_interpret_ctx_strerror (ipmi_interpret_ctx_errnum (ctx)));
}

int
ipmi_interpret_ctx_get_flags (ipmi_interpret_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_ctx_set_flags (ipmi_interpret_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_INTERPRET_FLAGS_MASK)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_ctx_get_manufacturer_id (ipmi_interpret_ctx_t ctx, uint32_t *manufacturer_id)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (!manufacturer_id)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  *manufacturer_id = ctx->manufacturer_id;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_ctx_set_manufacturer_id (ipmi_interpret_ctx_t ctx, uint32_t manufacturer_id)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  ctx->manufacturer_id = manufacturer_id;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_ctx_get_product_id (ipmi_interpret_ctx_t ctx, uint16_t *product_id)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (!product_id)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  *product_id = ctx->product_id;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_ctx_set_product_id (ipmi_interpret_ctx_t ctx, uint16_t product_id)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  ctx->product_id = product_id;
  ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  return (0);
}

int
ipmi_interpret_load_sel_config (ipmi_interpret_ctx_t ctx,
                                const char *sel_config_file)
{
  struct stat buf;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (sel_config_file)
    {
      if (stat (sel_config_file, &buf) < 0)
        {
          if (errno == EACCES || errno == EPERM)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PERMISSION);
          else if (errno == ENOENT)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST);
          else
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
          goto cleanup;
        }
    }
  else
    {
      if (stat (INTERPRET_SEL_CONFIG_FILE_DEFAULT, &buf) < 0)
        {
          /* Its not an error if the default configuration file doesn't exist */
          if (errno == ENOENT)
            goto out;
          else if (errno == EACCES || errno == EPERM)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PERMISSION);
          else
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
          goto cleanup;
        }
    }

  if (ipmi_interpret_sel_config_parse (ctx, sel_config_file) < 0)
    goto cleanup;

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_interpret_load_sensor_config (ipmi_interpret_ctx_t ctx,
                                   const char *sensor_config_file)
{
  struct stat buf;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (sensor_config_file)
    {
      if (stat (sensor_config_file, &buf) < 0)
        {
          if (errno == EACCES || errno == EPERM)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PERMISSION);
          else if (errno == ENOENT)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST);
          else
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
          goto cleanup;
        }
    }
  else
    {
      if (stat (INTERPRET_SENSOR_CONFIG_FILE_DEFAULT, &buf) < 0)
        {
          /* Its not an error if the default configuration file doesn't exist */
          if (errno == ENOENT)
            goto out;
          else if (errno == EACCES || errno == EPERM)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PERMISSION);
          else
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
          goto cleanup;
        }
    }

  if (ipmi_interpret_sensor_config_parse (ctx, sensor_config_file) < 0)
    goto cleanup;

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_interpret_sel (ipmi_interpret_ctx_t ctx,
                    const void *record_buf,
                    unsigned int record_buflen,
                    unsigned int *sel_state)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (!sel_state)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  return (0);
}

static int
_get_sensor_state (ipmi_interpret_ctx_t ctx,
                   uint16_t sensor_event_bitmask,
                   unsigned int *sensor_state,
                   struct ipmi_interpret_config **config)
{
  int i = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (sensor_state);
  assert (config);

  (*sensor_state) = IPMI_INTERPRET_STATE_NOMINAL;

  i = 0;
  while (config[i] && i < 16)
    {
      if (sensor_event_bitmask & (0x1 << i))
        {
          if (config[i]->state > (*sensor_state))
            (*sensor_state) = config[i]->state;
        }
      i++;
    }

  return (0);
}

static int
_get_sensor_oem_state (ipmi_interpret_ctx_t ctx,
		       uint8_t event_reading_type_code,
		       uint8_t sensor_type,
		       uint16_t sensor_event_bitmask,
		       unsigned int *sensor_state)
{
  char keybuf[IPMI_OEM_HASH_KEY_BUFLEN + 1];
  struct ipmi_interpret_sensor_oem_config *oem_conf;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (sensor_state);

  memset (keybuf, '\0', IPMI_OEM_HASH_KEY_BUFLEN + 1);

  snprintf (keybuf,
            IPMI_OEM_HASH_KEY_BUFLEN,
            "%u:%u:%u:%u",
            ctx->manufacturer_id,
            ctx->product_id,
            event_reading_type_code,
            sensor_type);

  if ((oem_conf = hash_find (ctx->interpret_sensor.oem_config,
			     keybuf)))
    {
      unsigned int i;
      int found = 0;

      (*sensor_state) = IPMI_INTERPRET_STATE_NOMINAL;

      for (i = 0; i < oem_conf->oem_state_count; i++)
	{
	  if (oem_conf->oem_state[i].oem_state_type == IPMI_OEM_STATE_TYPE_BITMASK)
	    {
	      if (oem_conf->oem_state[i].sensor_event_bitmask & sensor_event_bitmask)
		{
		  if (oem_conf->oem_state[i].sensor_state > (*sensor_state))
		    (*sensor_state) = oem_conf->oem_state[i].sensor_state;
		  found++;
		}
	    }
	  else
	    {
	      if (oem_conf->oem_state[i].sensor_event_bitmask == sensor_event_bitmask)
		{
		  if (oem_conf->oem_state[i].sensor_state > (*sensor_state))
		    (*sensor_state) = oem_conf->oem_state[i].sensor_state;
		  found++;
		}
	    }
	}
      
      if (!found)
	(*sensor_state) = IPMI_INTERPRET_STATE_UNKNOWN;
    }
  else
    (*sensor_state) = IPMI_INTERPRET_STATE_UNKNOWN;

  return (0);
}


int
ipmi_interpret_sensor (ipmi_interpret_ctx_t ctx,
                       uint8_t event_reading_type_code,
                       uint8_t sensor_type,
                       uint16_t sensor_event_bitmask,
                       unsigned int *sensor_state)
{
  struct ipmi_interpret_config **sensor_config = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_interpret_ctx_errormsg (ctx), ipmi_interpret_ctx_errnum (ctx));
      return (-1);
    }

  if (!sensor_state)
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_PARAMETERS);
      return (-1);
    }

  if (IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      if (_get_sensor_state (ctx,
                             sensor_event_bitmask,
                             sensor_state,
                             ctx->interpret_sensor.ipmi_interpret_threshold_sensor_config) < 0)
        goto cleanup;
    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code))
    {
      if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
          && sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_voltage_state_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_PERFORMANCE
               && sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_voltage_performance_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT
               && sensor_type == IPMI_SENSOR_TYPE_FAN)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_fan_device_present_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_TRANSITION_AVAILABILITY
               && sensor_type == IPMI_SENSOR_TYPE_FAN)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_fan_transition_availability_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
               && sensor_type == IPMI_SENSOR_TYPE_FAN)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_fan_redundancy_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
               && sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_processor_state_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
               && sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_power_supply_state_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
               && sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_power_supply_redundancy_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT
               && sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_power_unit_device_present_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_REDUNDANCY
               && sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_power_unit_redundancy_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
               && sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_drive_slot_state_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_PREDICTIVE_FAILURE
               && sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_drive_slot_predictive_failure_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT
               && sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_drive_slot_device_present_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
               && sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_button_switch_state_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_STATE
               && sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_module_board_state_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT
               && sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_module_board_device_present_config;
      else if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_DEVICE_PRESENT
               && sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_entity_presence_device_present_config;
      else
        {
          (*sensor_state) = IPMI_INTERPRET_STATE_UNKNOWN;
          rv = 0;
          goto cleanup;
        }

      if (_get_sensor_state (ctx,
                             sensor_event_bitmask,
                             sensor_state,
                             sensor_config) < 0)
        goto cleanup;

    }
  else if (IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
    {
      if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_physical_security_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_platform_security_violation_attempt_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_processor_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_power_supply_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_power_unit_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_MEMORY)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_memory_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_drive_slot_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_system_firmware_progress_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_event_logging_disabled_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_system_event_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_critical_interrupt_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_button_switch_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_cable_interconnect_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_boot_error_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_slot_connector_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_system_acpi_power_state_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_watchdog2_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_entity_presence_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_management_subsystem_health_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_BATTERY)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_battery_config;
      else if (sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
        sensor_config = ctx->interpret_sensor.ipmi_interpret_fru_state_config;
      else if (ctx->flags & IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA
	       && IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
	{
	  if (_get_sensor_oem_state (ctx,
				     event_reading_type_code,
				     sensor_type,
				     sensor_event_bitmask,
				     sensor_state) < 0)
	    goto cleanup;
          rv = 0;
          goto cleanup;
	}
      else
        {
          (*sensor_state) = IPMI_INTERPRET_STATE_UNKNOWN;
          rv = 0;
          goto cleanup;
        }

      if (_get_sensor_state (ctx,
                             sensor_event_bitmask,
                             sensor_state,
                             sensor_config) < 0)
        goto cleanup;
    }
  else if (ctx->flags & IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA
	   && IPMI_EVENT_READING_TYPE_CODE_IS_OEM (event_reading_type_code))
    {
      if (_get_sensor_oem_state (ctx,
				 event_reading_type_code,
				 sensor_type,
				 sensor_event_bitmask,
				 sensor_state) < 0)
	goto cleanup;
    }
  else
    {
      (*sensor_state) = IPMI_INTERPRET_STATE_UNKNOWN;
      rv = 0;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}
