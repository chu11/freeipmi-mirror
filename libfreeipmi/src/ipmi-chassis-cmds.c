/* 
   ipmi-chassis-cmds.c - IPMI Chassis Commands

   Copyright (C) 2003 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include "freeipmi.h"

fiid_template_t tmpl_set_power_restore_policy_rq =
  {
    {8, "cmd"}, 
    
    {3, "power_restore_policy"}, 
    {5, "power_restore_policy.reserved"}, 
    
    {0, ""}
  };

fiid_template_t tmpl_set_power_restore_policy_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    
    {1, "powered_off_after_ac_mains_returns_flag"}, 
    {1, "restoring_power_to_state_when_ac_mains_was_lost_flag"}, 
    {1, "always_powering_up_after_ac_mains_returns_flag"}, 
    {5, "power_restore_policy_support.reserved"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_cmd_get_chassis_status_rq =
  {
    {8, "cmd"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_chassis_status_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {1, "power_state.power_on"},
    {1, "power_state.power_overload"},
    {1, "power_state.interlock"},
    {1, "power_state.power_fault"},
    {1, "power_state.power_control_fault"},
    {2, "power_state.power_restore_policy"},
    {1, "power_state.reserved"},
    {1, "last_power_event.ac_failed"},
    {1, "last_power_event.power_down_overload"},
    {1, "last_power_event.power_down_interlock"},
    {1, "last_power_event.power_down_fault"},
    {1, "last_power_event.power_on_via_ipmi"},
    {3, "last_power_event.reserved"},
    {1, "misc_chassis.chassis_intrusion_active"},
    {1, "misc_chassis.front_panel_lockout_active"},
    {1, "misc_chassis.drive_fault"},
    {1, "misc_chassis.cooling_fan_fault"},
    {4, "misc_chassis.reserved"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_chassis_ctrl_rq =
  {
    {8, "cmd"},
    {4, "chassis_ctrl"},
    {4, "reserved1"},
    {0, ""}
  };

/* NOTE: 
   The implementation is allowed to return the completion code prior
   to performing the selected control action if necessary. 
*/
fiid_template_t tmpl_cmd_chassis_ctrl_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {0, ""}
  };

int8_t 
fill_cmd_set_power_restore_policy (fiid_obj_t obj_data_rq, 
                                   u_int8_t power_restore_policy)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_power_restore_policy_rq, 
		"cmd", 
		IPMI_CMD_SET_POWER_RESTORE_POLICY);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_power_restore_policy_rq, 
		"power_restore_policy", 
		power_restore_policy);
  
  return 0;
}

int8_t 
ipmi_set_power_restore_policy (u_int8_t power_restore_policy, 
			       fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_power_restore_policy_rq);
  fill_cmd_set_power_restore_policy (obj_data_rq, power_restore_policy);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_CHASSIS_RQ, 
			 obj_data_rq, tmpl_set_power_restore_policy_rq, 
			 obj_data_rs, tmpl_set_power_restore_policy_rs);
  free (obj_data_rq);
  return status;
}

int8_t
fill_cmd_get_chassis_status (fiid_obj_t obj_cmd)
{ 
  if (obj_cmd == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_chassis_status_rq, "cmd", 
		IPMI_CMD_GET_CHASSIS_STATUS);
  return 0;
}

int8_t 
ipmi_get_chassis_status (fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_cmd_get_chassis_status_rq);
  fill_cmd_get_chassis_status (obj_data_rq);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_CHASSIS_RQ, 
			 obj_data_rq, tmpl_cmd_get_chassis_status_rq, 
			 obj_data_rs, tmpl_cmd_get_chassis_status_rs);
  free (obj_data_rq);
  return status;
}

int8_t
fill_cmd_chassis_ctrl (u_int8_t chassis_ctrl, fiid_obj_t obj_cmd)
{
  if (chassis_ctrl > IPMI_CHASSIS_CTRL_INIT_SOFT_SHUTDOWN
      || obj_cmd == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_chassis_ctrl_rq, "cmd",
		IPMI_CMD_CHASSIS_CTRL);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_chassis_ctrl_rq, "chassis_ctrl",
		chassis_ctrl);
  return 0;
}  
