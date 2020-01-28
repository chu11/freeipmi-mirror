/*
 * Copyright (C) 2007-2015 FreeIPMI Core Team
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-chassis.h"
#include "ipmi-chassis-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-util-common.h"

static int
get_chassis_capabilities (ipmi_chassis_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t fru_info_device_address;
  uint8_t sdr_device_address;
  uint8_t sel_device_address;
  uint8_t system_management_device_address;
  uint8_t bridge_device_address;
  int flag;
  uint64_t val = 0;
  int rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_chassis_capabilities_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_chassis_capabilities (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_chassis_capabilities: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "capabilities_flags.provides_intrusion_sensor",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'capabilities_flags.provides_intrusion_sensor': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Intrusion sensor        : %s\n",
                  (val ? "provided" : "not provided"));

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "capabilities_flags.provides_front_panel_lockout",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'capabilities_flags.provides_front_panel_lockout': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Front Panel Lockout     : %s\n",
                  (val ? "provided" : "not provided"));

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "capabilities_flags.provides_diagnostic_interrupt",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'capabilities_flags.provides_diagnostic_interrupt': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Diagnostic Interrupt    : %s\n",
                  (val ? "provided" : "not provided"));

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "capabilities_flags.provides_power_interlock",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'capabilities_flags.provides_power_interlock': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Power interlock         : %s\n",
                  (val ? "provided" : "not provided"));

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "fru_info_device_address",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'fru_info_device_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  fru_info_device_address = val;

  pstdout_printf (state_data->pstate,
                  "FRU Info Device Address : %Xh %s\n",
                  fru_info_device_address,
                  (val ? "" : "(Unspecified)"));

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "sdr_device_address",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'sdr_device_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sdr_device_address = val;

  pstdout_printf (state_data->pstate,
                  "SDR Device Address      : %Xh\n",
                  sdr_device_address);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "sel_device_address",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'sel_device_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_device_address = val;

  pstdout_printf (state_data->pstate,
                  "SEL Device Address      : %Xh\n",
                  sel_device_address);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "system_management_device_address",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'system_management_device_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  system_management_device_address = val;

  pstdout_printf (state_data->pstate,
                  "Sys Mgmt Device Address : %Xh\n",
                  system_management_device_address);

  if ((flag = fiid_obj_get (obj_cmd_rs, "bridge_device_address", &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'system_management_device_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bridge_device_address = val;

  if (flag)
    pstdout_printf (state_data->pstate,
                    "Bridge Device Address   : %Xh\n",
                    bridge_device_address);
  else
    pstdout_printf (state_data->pstate,
                    "Bridge Device Address   : 20h (assuming default)\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_chassis_status (ipmi_chassis_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0, temp_val;
  char *str;
  int rv = -1;
  int flag;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_chassis_status_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_chassis_status (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_chassis_status: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "current_power_state.power_is_on", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'current_power_state.power_is_on': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "System Power                        : %s\n",
                  val ? "on" : "off");

  if (FIID_OBJ_GET (obj_cmd_rs, "current_power_state.power_overload", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'current_power_state.power_overload': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Power overload                      : %s\n",
                  val ? "true" : "false");

  if (FIID_OBJ_GET (obj_cmd_rs, "current_power_state.interlock", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'current_power_state.interlock': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Interlock                           : %s\n",
                  val ? "active" : "inactive");

  if (FIID_OBJ_GET (obj_cmd_rs, "current_power_state.power_fault", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'current_power_state.power_fault': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Power fault                         : %s\n",
                  val ? "true" : "false");

  if (FIID_OBJ_GET (obj_cmd_rs, "current_power_state.power_control_fault", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'current_power_state.power_control_fault': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Power control fault                 : %s\n",
                  val ? "true" : "false");

  if (FIID_OBJ_GET (obj_cmd_rs, "current_power_state.power_restore_policy", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'current_power_state.power_restore_policy': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  switch (val)
    {
    case IPMI_POWER_RESTORE_POLICY_POWERED_OFF_AFTER_AC_RETURNS:
      str = "Always off";
      break;

    case IPMI_POWER_RESTORE_POLICY_POWER_RESTORED_TO_STATE:
      str = "Restore";
      break;

    case IPMI_POWER_RESTORE_POLICY_POWERS_UP_AFTER_AC_RETURNS:
      str = "Always on";
      break;

    case IPMI_POWER_RESTORE_POLICY_UNKNOWN:
    default:
      str = "unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
                  "Power restore policy                : %s\n",
                  str);

  temp_val = IPMI_LAST_POWER_EVENT_UNKNOWN;

  if (FIID_OBJ_GET (obj_cmd_rs, "last_power_event.ac_failed", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'last_power_event.ac_failed': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (val)
    {
      temp_val = IPMI_LAST_POWER_EVENT_AC_FAILED;
      goto print;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "last_power_event.power_down_caused_by_power_overload", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'last_power_event.power_down_caused_by_power_overload': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (val)
    {
      temp_val = IPMI_LAST_POWER_EVENT_POWER_DOWN_POWER_OVERLOAD;
      goto print;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "last_power_event.power_down_caused_by_power_interlock_being_activated", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'last_power_event.power_down_caused_by_power_interlock_being_activated': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (val)
    {
      temp_val = IPMI_LAST_POWER_EVENT_POWER_DOWN_INTERLOCK_ACTIVATED;
      goto print;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "last_power_event.power_down_caused_by_power_fault", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'last_power_event.power_down_caused_by_power_fault': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (val)
    {
      temp_val = IPMI_LAST_POWER_EVENT_POWER_DOWN_POWER_FAULT;
      goto print;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "last_power_event.power_on_entered_via_ipmi", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'last_power_event.power_on_entered_via_ipmi': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (val)
    temp_val = IPMI_LAST_POWER_EVENT_POWER_ON_VIA_IPMI;

 print:

  switch (temp_val)
    {
    case IPMI_LAST_POWER_EVENT_AC_FAILED:
      str = "ac failed";
      break;
    case IPMI_LAST_POWER_EVENT_POWER_DOWN_POWER_OVERLOAD:
      str = "power down due to power overload";
      break;
    case IPMI_LAST_POWER_EVENT_POWER_DOWN_INTERLOCK_ACTIVATED:
      str = "power down due to Activation of interlock switch";
      break;
    case IPMI_LAST_POWER_EVENT_POWER_DOWN_POWER_FAULT:
      str = "power down due to power fault";
      break;
    case IPMI_LAST_POWER_EVENT_POWER_ON_VIA_IPMI:
      str = "power on via ipmi command";
      break;
    default:
      str = "unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
                  "Last Power Event                    : %s\n",
                  str);

  if (FIID_OBJ_GET (obj_cmd_rs, "misc_chassis_state.chassis_intrusion_active", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'misc_chassis_state.chassis_intrusion_active': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Chassis intrusion                   : %s\n",
                  val ? "active" : "inactive");

  if (FIID_OBJ_GET (obj_cmd_rs,  "misc_chassis_state.front_panel_lockout_active", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'misc_chassis_state.front_panel_lockout_active': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Front panel lockout                 : %s\n",
                  val ? "active" : "inactive");

  if (FIID_OBJ_GET (obj_cmd_rs, "misc_chassis_state.drive_fault", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'misc_chassis_state.drive_fault': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Drive Fault                         : %s\n",
                  val ? "true" : "false");

  if (FIID_OBJ_GET (obj_cmd_rs, "misc_chassis_state.cooling_fan_fault_detected", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'misc_chassis_state.cooling_fan_fault_detected': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Cooling/fan fault                   : %s\n",
                  val ? "true" : "false");

  if (FIID_OBJ_GET (obj_cmd_rs, "misc_chassis_state.chassis_identify_command_and_state_info_supported", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'misc_chassis_state.chassis_identify_command_and_state_info_supported': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (val)
    {
      if (FIID_OBJ_GET (obj_cmd_rs, "misc_chassis_state.chassis_identify_state", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'misc_chassis_state.chassis_identify_state': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      switch (val)
        {
        case IPMI_CHASSIS_IDENTIFY_STATE_OFF:
          str = "off";
          break;

        case IPMI_CHASSIS_IDENTIFY_STATE_TEMPORARY_ON:
          str = "Timed on";
          break;

        case IPMI_CHASSIS_IDENTIFY_STATE_INDEFINITE_ON:
          str = "Indefinite on";
          break;

        default:
          str = "unknown";
          break;
        }

      pstdout_printf (state_data->pstate,
                      "Chassis Identify state              : %s\n",
                      str);

    }

  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "front_panel.power_off_button_disabled",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'front_panel.power_off_button_disabled': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (flag)
    {
      pstdout_printf (state_data->pstate,
                      "Power off button                    : %s\n",
                      val ? "disabled" : "enabled");

      if (FIID_OBJ_GET (obj_cmd_rs, "front_panel.reset_button_disabled", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'front_panel.reset_button_disabled': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Reset button                        : %s\n",
                      val ? "disabled" : "enabled");

      if (FIID_OBJ_GET (obj_cmd_rs, "front_panel.diagnostic_interrupt_button_disabled", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'front_panel.diagnostic_interrupt_button_disabled': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Diagnostic Interrupt button         : %s\n",
                      val ? "disabled" : "enabled");

      if (FIID_OBJ_GET (obj_cmd_rs, "front_panel.standby_button_disabled", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'front_panel.standby_button_disabled': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Standby button                      : %s\n",
                      val ? "disabled" : "enabled");

      if (FIID_OBJ_GET (obj_cmd_rs, "front_panel.power_off_button_disable_allowed", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'front_panel.power_off_button_disable_allowed': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Power off button disable            : %s\n",
                      val ? "allowed" : "unallowed");

      if (FIID_OBJ_GET (obj_cmd_rs, "front_panel.reset_button_disable_allowed", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'front_panel.reset_button_disable_allowed': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Reset button disable                : %s\n",
                      val ? "allowed" : "unallowed");

      if (FIID_OBJ_GET (obj_cmd_rs, "front_panel.diagnostic_interrupt_button_disable_allowed", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'front_panel.diagnostic_interrupt_button_disable_allowed': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Diagnostic interrupt button disable : %s\n",
                      val ? "allowed" : "unallowed");

      if (FIID_OBJ_GET (obj_cmd_rs, "front_panel.standby_button_disable_allowed", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'front_panel.standby_button_disable_allowed': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Standby button disable              : %s\n",
                      val ? "allowed" : "unallowed");
    }

  pstdout_printf (state_data->pstate, "\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
chassis_control (ipmi_chassis_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  struct ipmi_chassis_arguments *args;

  args = state_data->prog_data->args;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_chassis_control_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_chassis_control (state_data->ipmi_ctx,
                                args->chassis_control_arg,
                                obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_chassis_control: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
chassis_identify (ipmi_chassis_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  struct ipmi_chassis_arguments *args;

  args = state_data->prog_data->args;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_chassis_identify_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_chassis_identify (state_data->ipmi_ctx,
                                 (args->chassis_identify_args.identify_interval) ? &args->chassis_identify_args.identify_interval_arg : NULL,
                                 (args->chassis_identify_args.force_identify) ? &args->chassis_identify_args.force_identify_arg : NULL,
                                 obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_chassis_identify: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_system_restart_cause (ipmi_chassis_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  char *restart_cause_str;
  int rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_restart_cause_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_system_restart_cause (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_restart_cause: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "restart_cause", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'restart_cause': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  switch (val)
    {
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_UNKNOWN:
      restart_cause_str = "unknown";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_CHASSIS_CONTROL_COMMAND:
      restart_cause_str = "Chassis control command";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_RESET_VIA_PUSHBUTTON:
      restart_cause_str = "Reset via pushbutton";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_POWER_UP_VIA_POWER_PUSHBUTTON:
      restart_cause_str = "Power up via power pushbutton";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_WATCHDOG_EXPIRATION:
      restart_cause_str = "Watchdog expiration";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_OEM:
      restart_cause_str = "OEM";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_AUTOMATIC_POWER_UP_ALWAYS_RESTORE:
      restart_cause_str = "Automatic power-up on AC being applied due to \"always restore\" power restore policy";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_AUTOMATIC_POWER_UP_RESTORE_PREVIOUS:
      restart_cause_str = "Automatic power-up on AC being applied due to \"restore previous power state\" power restore policy";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_RESET_VIA_PEF:
      restart_cause_str = "Reset via PEF";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_POWER_CYCLE_VIA_PEF:
      restart_cause_str = "Power cycle via PEF";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_SOFT_RESET:
      restart_cause_str = "Soft reset";
      break;
    case IPMI_CHASSIS_SYSTEM_RESTART_CAUSE_POWER_UP_VIA_RTC:
      restart_cause_str = "Power up via RTC";
      break;
    default:
      restart_cause_str = "unknown";
    }

  pstdout_printf (state_data->pstate,
                  "Restart cause : %s\n",
                  restart_cause_str);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_power_on_hours_counter (ipmi_chassis_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  uint8_t minutes_per_counter;
  uint32_t counter_reading, min, hrs;
  int rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_power_on_hours_counter_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_power_on_hours_counter (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_power_on_hours_counter: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "minutes_per_counter", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'minutes_per_counter': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  minutes_per_counter = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "counter_reading", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'counter_reading': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  counter_reading = val;

  min = counter_reading / minutes_per_counter;

  hrs = min / 60;

  min = min % 60;

  pstdout_printf (state_data->pstate,
                  "Power on hours : %u Hours %u Minutes\n",
                  hrs,
                  min);
  rv = 0;

 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
run_cmd_args (ipmi_chassis_state_data_t *state_data)
{
  struct ipmi_chassis_arguments *args;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->get_chassis_capabilities)
    return (get_chassis_capabilities (state_data));

  if (args->get_chassis_status)
    return (get_chassis_status (state_data));

  if (args->chassis_control)
    return (chassis_control (state_data));

  if (args->chassis_identify)
    return (chassis_identify (state_data));

  if (args->get_system_restart_cause)
    return (get_system_restart_cause (state_data));

  if (args->get_power_on_hours_counter)
    return (get_power_on_hours_counter (state_data));

  rv = 0;
  return (rv);
}

static int
_ipmi_chassis (pstdout_state_t pstate,
               const char *hostname,
               void *arg)
{
  ipmi_chassis_state_data_t state_data;
  ipmi_chassis_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_chassis_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmi_chassis_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->common_args),
                                         state_data.pstate,
                                         0)))
    goto cleanup;

  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_chassis_prog_data_t prog_data;
  struct ipmi_chassis_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  prog_data.progname = argv[0];
  ipmi_chassis_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
                                    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _ipmi_chassis,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
