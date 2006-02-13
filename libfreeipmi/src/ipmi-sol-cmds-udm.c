/* 
   ipmi-sol-cmds-udm.c - IPMI UDM SOL Commands

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

#include "freeipmi.h"

int8_t 
ipmi_cmd_sol_conf_sol_enable_disable2 (ipmi_device_t *dev, 
				       uint8_t channel_number, 
				       uint8_t sol_payload, 
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_sol_conf_param_sol_enable_rq);
  ERR (fill_sol_conf_sol_enable_disable (channel_number, 
					 sol_payload,
                                         obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_sol_conf_param_sol_enable_rq, 
		 obj_cmd_rs, 
		 tmpl_set_sol_conf_param_sol_enable_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_sol_conf_sol_enable2 (ipmi_device_t *dev, 
			       uint8_t channel_number, 
			       fiid_obj_t obj_cmd_rs)
{
  return ipmi_cmd_sol_conf_sol_enable_disable2 (dev, 
						channel_number, 
						IPMI_SOL_PAYLOAD_ENABLE, 
						obj_cmd_rs);
}

int8_t 
ipmi_cmd_sol_conf_sol_disable2 (ipmi_device_t *dev, 
				uint8_t channel_number, 
				fiid_obj_t obj_cmd_rs)
{
  return ipmi_cmd_sol_conf_sol_enable_disable2 (dev, 
						channel_number, 
						IPMI_SOL_PAYLOAD_DISABLE, 
						obj_cmd_rs);
}

int8_t 
ipmi_cmd_sol_conf_get_sol_enable2 (ipmi_device_t *dev, 
				   uint8_t channel_number,
				   uint8_t parameter_type,
				   uint8_t set_selector,
				   uint8_t block_selector,
				   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sol_conf_param_rq);
  ERR (fill_get_sol_conf_param (IPMI_SOL_PARAM_SELECTOR_SOL_ENABLE, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_sol_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sol_conf_param_sol_enable_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

