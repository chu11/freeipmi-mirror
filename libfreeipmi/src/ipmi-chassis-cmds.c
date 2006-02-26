/* 
   Ipmi-chassis-cmds.c - IPMI Chassis Commands

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/ipmi-chassis-cmds.h"

#include "freeipmi/ipmi-cmd-spec.h"

#include "freeipmi-portability.h"
#include "fiid-wrappers.h"

fiid_template_t tmpl_cmd_get_chassis_status_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_chassis_status_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_is_on", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.interlock", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.power_control_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "current_power_state.power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "current_power_state.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.ac_failed", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_caused_by_power_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_caused_by_power_interlock_being_activated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_caused_by_power_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_on_entered_via_ipmi", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "last_power_event.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.chassis_intrusion_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.front_panel_lockout_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.drive_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.cooling_fan_fault_detected", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "misc_chassis_state.chassis_identify_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.chassis_identify_command_and_state_info_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis_state.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.power_off_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.reset_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.diagnostic_interrupt_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.standy_button_disabled", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.power_off_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.reset_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.diagnostic_interrupt_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "front_panel.standby_button_disable_allowed", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "chassis_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* NOTE: 
   The implementation is allowed to return the completion code prior
   to performing the selected control action if necessary. 
*/
fiid_template_t tmpl_cmd_chassis_control_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_identify_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "identify_interval", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1, "force_identify", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {7, "reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_identify_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_power_restore_policy_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "power_restore_policy.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_power_restore_policy_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "powered_off_after_ac_mains_returns", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "restoring_power_to_state_when_ac_mains_was_lost", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "always_powering_up_after_ac_mains_returns", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

int8_t
fill_cmd_get_chassis_status (fiid_obj_t obj_cmd)
{ 
  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_cmd_get_chassis_status_rq);

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_GET_CHASSIS_STATUS);
  return 0;
}

int8_t
fill_cmd_chassis_control (uint8_t chassis_control, fiid_obj_t obj_cmd)
{
  if (!IPMI_CHASSIS_CONTROL_VALID(chassis_control)
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_cmd_chassis_control_rq);

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_CHASSIS_CONTROL);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"chassis_control", chassis_control);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved", 0);
  return 0;
}  

int8_t
fill_cmd_chassis_identify (uint8_t *identify_interval, 
			   uint8_t *force_identify,
                           fiid_obj_t obj_cmd)
{
  if ((force_identify 
       && !IPMI_CHASSIS_FORCE_IDENTIFY_VALID(*force_identify))
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_cmd_chassis_identify_rq);

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_CHASSIS_IDENTIFY);
  if (identify_interval)
    {
      FIID_OBJ_SET (obj_cmd, (uint8_t *)"identify_interval", *identify_interval);
      if (force_identify)
	{
	  FIID_OBJ_SET (obj_cmd, (uint8_t *)"force_identify", *force_identify);
	  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved", 0);
	}
    }

  return 0;
}  

int8_t 
fill_cmd_set_power_restore_policy (uint8_t power_restore_policy, fiid_obj_t obj_cmd)
{
  if (!IPMI_POWER_RESTORE_POLICY_VALID(power_restore_policy)
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_set_power_restore_policy_rq);

  FIID_OBJ_SET (obj_cmd,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_POWER_RESTORE_POLICY);
  
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"power_restore_policy", power_restore_policy);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"power_restore_policy.reserved", 0);
  
  return 0;
}
