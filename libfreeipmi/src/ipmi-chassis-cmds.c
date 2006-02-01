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
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {3, "power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "power_restore_policy.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0, ""}
  };

fiid_template_t tmpl_set_power_restore_policy_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "powered_off_after_ac_mains_returns_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "restoring_power_to_state_when_ac_mains_was_lost_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "always_powering_up_after_ac_mains_returns_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "power_restore_policy_support.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_cmd_get_chassis_status_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_chassis_status_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_state.power_on", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_state.power_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_state.interlock", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_state.power_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_state.power_control_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "power_state.power_restore_policy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "power_state.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.ac_failed", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_overload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_interlock", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_down_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "last_power_event.power_on_via_ipmi", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "last_power_event.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis.chassis_intrusion_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis.front_panel_lockout_active", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis.drive_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "misc_chassis.cooling_fan_fault", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "misc_chassis.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_chassis_ctrl_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "chassis_ctrl", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* NOTE: 
   The implementation is allowed to return the completion code prior
   to performing the selected control action if necessary. 
*/
fiid_template_t tmpl_cmd_chassis_ctrl_rs =
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
    {0, ""}
  };

fiid_template_t tmpl_cmd_chassis_identify_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, ""}
  };

int8_t 
fill_cmd_set_power_restore_policy (uint8_t power_restore_policy, fiid_obj_t obj_cmd)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_cmd)
      || !IPMI_POWER_RESTORE_POLICY_VALID(power_restore_policy))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_set_power_restore_policy_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_cmd,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_POWER_RESTORE_POLICY);
  
  FIID_OBJ_SET (obj_cmd, 
		(uint8_t *)"power_restore_policy", 
		power_restore_policy);

  FIID_OBJ_SET (obj_cmd,
		(uint8_t *)"power_restore_policy.reserved",
		0);
  
  return 0;
}

int8_t
fill_cmd_get_chassis_status (fiid_obj_t obj_cmd)
{ 
  int8_t rv;

  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }
  
  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_get_chassis_status_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_GET_CHASSIS_STATUS);
  return 0;
}

int8_t
fill_cmd_chassis_ctrl (uint8_t chassis_ctrl, fiid_obj_t obj_cmd)
{
  int8_t rv;

  if (!IPMI_CHASSIS_CTRL_VALID(chassis_ctrl)
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_chassis_ctrl_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_CHASSIS_CTRL);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"chassis_ctrl", chassis_ctrl);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved1", 0);
  return 0;
}  

int8_t 
ipmi_cmd_set_power_restore_policy2 (ipmi_device_t *dev, 
				    uint8_t power_restore_policy, 
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;

  if (!dev
      || !IPMI_POWER_RESTORE_POLICY_VALID (power_restore_policy)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_power_restore_policy_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_power_restore_policy_rq)))
    goto cleanup;

  if (fill_cmd_set_power_restore_policy (power_restore_policy, 
					 obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		IPMI_BMC_IPMB_LUN_BMC, 
		IPMI_NET_FN_CHASSIS_RQ, 
		obj_cmd_rq, 
		obj_cmd_rs) < 0)
    goto cleanup;

  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_chassis_status2 (ipmi_device_t *dev, 
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;

  if (!dev
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_cmd_get_chassis_status_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_cmd_get_chassis_status_rq)))
    goto cleanup;

  if (fill_cmd_get_chassis_status (obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		IPMI_BMC_IPMB_LUN_BMC, 
		IPMI_NET_FN_CHASSIS_RQ, 
		obj_cmd_rq, 
		obj_cmd_rs) < 0)
    goto cleanup;

  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t
fill_cmd_chassis_identify (uint8_t *identify_interval, 
			   uint8_t *force_identify_flag,
                           fiid_obj_t obj_cmd)
{
  int8_t rv;

  if ((force_identify_flag 
       && !IPMI_CHASSIS_FORCE_IDENTIFY_VALID(*force_identify_flag))
      || !fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_cmd, tmpl_cmd_chassis_identify_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_CHASSIS_IDENTIFY);
  if (identify_interval)
    {
      FIID_OBJ_SET (obj_cmd, (uint8_t *)"identify_interval", *identify_interval);
      if (force_identify_flag)
	{
	  FIID_OBJ_SET (obj_cmd, (uint8_t *)"force_identify", *force_identify_flag);
	  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved", 0);
	}
    }

  return 0;
}  
