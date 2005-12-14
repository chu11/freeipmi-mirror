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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

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

fiid_template_t tmpl_cmd_chassis_identify_rq = 
  {
    {8, "cmd"},
    {7, "reserved"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_chassis_identify_interval_rq = 
  {
    {8, "cmd"},
    {8, "identify_interval"},
    {7, "reserved"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_chassis_identify_interval_force_rq = 
  {
    {8, "cmd"},
    {8, "identify_interval"},
    {1, "force_identify"},
    {7, "reserved"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_chassis_identify_rs = 
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
  
  obj_data_rq = fiid_obj_calloc (tmpl_set_power_restore_policy_rq);
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
  
  obj_data_rq = fiid_obj_calloc (tmpl_cmd_get_chassis_status_rq);
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

int8_t 
ipmi_cmd_set_power_restore_policy2 (ipmi_device_t *dev, 
				    u_int8_t power_restore_policy, 
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  if (IPMI_POWER_RESTORE_POLICY_VALID (power_restore_policy) == 0)
    {
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_power_restore_policy_rq);
  ERR (fill_cmd_set_power_restore_policy (obj_cmd_rq, 
					  power_restore_policy) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_CHASSIS_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_set_power_restore_policy_rq, 
		 obj_cmd_rs, 
		 tmpl_set_power_restore_policy_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_chassis_status2 (ipmi_device_t *dev, 
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_get_chassis_status_rq);
  ERR (fill_cmd_get_chassis_status (obj_cmd_rq) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_CHASSIS_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_cmd_get_chassis_status_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_get_chassis_status_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t
fill_cmd_chassis_identify (fiid_template_t tmpl_identify_cmd,
                           u_int8_t identify_interval, 
                           u_int8_t force_identify_flag,
                           fiid_obj_t obj_cmd)
{
  if (!tmpl_identify_cmd
      || (force_identify_flag != IPMI_CHASSIS_FORCE_IDENTIFY_OFF
          && force_identify_flag != IPMI_CHASSIS_FORCE_IDENTIFY_ON)
      || !obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_identify_cmd, "cmd",
		IPMI_CMD_CHASSIS_IDENTIFY);
  if (fiid_obj_field_lookup(tmpl_identify_cmd, "identify_interval"))
    FIID_OBJ_SET (obj_cmd, tmpl_identify_cmd, 
                  "identify_interval", identify_interval);
  if (fiid_obj_field_lookup(tmpl_identify_cmd, "force_identify"))
    FIID_OBJ_SET (obj_cmd, tmpl_identify_cmd,
                  "force_identify", force_identify_flag);

  return 0;
}  
