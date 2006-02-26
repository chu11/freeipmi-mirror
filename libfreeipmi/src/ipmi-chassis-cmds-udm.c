/* 
   Ipmi-chassis-cmds-udm.c - IPMI UDM Chassis Commands

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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-chassis-cmds-udm.h"
#include "freeipmi/ipmi-chassis-cmds.h"
#include "freeipmi/ipmi-ipmb-interface.h"
#include "freeipmi/ipmi-netfn-spec.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

int8_t 
ipmi_cmd_get_chassis_status2 (ipmi_device_t *dev, 
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_chassis_status_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_chassis_status_rq);

  ERR_CLEANUP (!(fill_cmd_get_chassis_status (obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_CHASSIS_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_power_restore_policy2 (ipmi_device_t *dev, 
				    uint8_t power_restore_policy, 
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev
      || !IPMI_POWER_RESTORE_POLICY_VALID (power_restore_policy)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_power_restore_policy_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_power_restore_policy_rq);

  ERR_CLEANUP (!(fill_cmd_set_power_restore_policy (power_restore_policy, 
						    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_CHASSIS_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}
