/* 
   ipmi-lan-cmds.c - IPMI LAN Commands

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

#include "freeipmi.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>
#endif

/* #define DEBUG */

//#include "ipmi-lan-cmds.h"

fiid_template_t tmpl_set_lan_conf_param_bmc_generated_arp_control_rq =
  {
    {8, "cmd"}, 
    {4, "channel_number"}, 
    {4, "reserved1"}, 
    {8, "parameter_selector"}, 
    /* for BMC generated ARP control */
    {1, "bmc_generated_gratuitous_arps_flag"}, 
    {1, "bmc_generated_arp_responses_flag"}, 
    {6, "reserved2"}, 
    {0, ""}
  };

fiid_template_t tmpl_set_lan_conf_param_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_lan_conf_param_gratuitous_arp_interval_rq =
  {
    {8, "cmd"}, 
    {4, "channel_number"}, 
    {4, "reserved1"}, 
    {8, "parameter_selector"}, 
    {8, "gratuitous_arp_interval"}, 
    {0, ""}
  };

fiid_template_t tmpl_set_lan_conf_param_auth_type_enables_rq =
  {
    {8, "cmd"}, 
    {4, "channel_number"}, 
    {4, "reserved1"}, 
    {8, "parameter_selector"}, 
    /* for Authentication type enables */
    /* byte 1 */
/*     {1, "max_privilege_auth_type_callback_level.none"},  */
/*     {1, "max_privilege_auth_type_callback_level.md2"},  */
/*     {1, "max_privilege_auth_type_callback_level.md5"},  */
/*     {1, "max_privilege_auth_type_callback_level.reserved1"},  */
/*     {1, "max_privilege_auth_type_callback_level.straight_password"},  */
/*     {1, "max_privilege_auth_type_callback_level.oem_proprietary"},  */
    {6, "max_privilege_auth_type_callback_level"}, 
    {2, "max_privilege_auth_type_callback_level.reserved2"}, 
    /* byte 2 */
/*     {1, "max_privilege_auth_type_user_level.none"},  */
/*     {1, "max_privilege_auth_type_user_level.md2"},  */
/*     {1, "max_privilege_auth_type_user_level.md5"},  */
/*     {1, "max_privilege_auth_type_user_level.reserved1"},  */
/*     {1, "max_privilege_auth_type_user_level.straight_password"},  */
/*     {1, "max_privilege_auth_type_user_level.oem_proprietary"},  */
    {6, "max_privilege_auth_type_user_level"}, 
    {2, "max_privilege_auth_type_user_level.reserved2"}, 
    /* byte 3 */
/*     {1, "max_privilege_auth_type_operator_level.none"},  */
/*     {1, "max_privilege_auth_type_operator_level.md2"},  */
/*     {1, "max_privilege_auth_type_operator_level.md5"},  */
/*     {1, "max_privilege_auth_type_operator_level.reserved1"},  */
/*     {1, "max_privilege_auth_type_operator_level.straight_password"},  */
/*     {1, "max_privilege_auth_type_operator_level.oem_proprietary"},  */
    {6, "max_privilege_auth_type_operator_level"}, 
    {2, "max_privilege_auth_type_operator_level.reserved2"}, 
    /* byte 4 */
/*     {1, "max_privilege_auth_type_admin_level.none"},  */
/*     {1, "max_privilege_auth_type_admin_level.md2"},  */
/*     {1, "max_privilege_auth_type_admin_level.md5"},  */
/*     {1, "max_privilege_auth_type_admin_level.reserved1"},  */
/*     {1, "max_privilege_auth_type_admin_level.straight_password"},  */
/*     {1, "max_privilege_auth_type_admin_level.oem_proprietary"},  */
    {6, "max_privilege_auth_type_admin_level"}, 
    {2, "max_privilege_auth_type_admin_level.reserved2"}, 
    /* byte 5 */
/*     {1, "max_privilege_auth_type_oem_level.none"},  */
/*     {1, "max_privilege_auth_type_oem_level.md2"},  */
/*     {1, "max_privilege_auth_type_oem_level.md5"},  */
/*     {1, "max_privilege_auth_type_oem_level.reserved1"},  */
/*     {1, "max_privilege_auth_type_oem_level.straight_password"},  */
/*     {1, "max_privilege_auth_type_oem_level.oem_proprietary"},  */
    {6, "max_privilege_auth_type_oem_level"}, 
    {2, "max_privilege_auth_type_oem_level.reserved2"}, 
    {0, ""}
  };

fiid_template_t tmpl_set_lan_conf_param_ip_addr_source_rq =
  {
    {8, "cmd"},
    {4, "channel_number"},
    {4, "reserved1"},
    {8, "parameter_selector"}, 

    {4, "ip_addr_source"},
    {4, "reserved2"},

    {0, ""}
  };

fiid_template_t tmpl_set_lan_conf_param_ip_addr_rq =
  {
    {8, "cmd"},
    {4, "channel_number"},
    {4, "reserved"},
    {8, "parameter_selector"},

    {32, "ip_addr"},

    {0, ""}
  };
    
fiid_template_t tmpl_set_lan_conf_param_subnet_mask_rq =
  {
    {8, "cmd"},
    {4, "channel_number"},
    {4, "reserved"},
    {8, "parameter_selector"},

    {32, "subnet_mask"},

    {0, ""}
  };

fiid_template_t tmpl_set_lan_conf_param_mac_addr_rq =
  {
    {8, "cmd"},
    {4, "channel_number"},
    {4, "reserved"},
    {8, "parameter_selector"},

    {48, "mac_addr"},

    {0, ""}
  };

fiid_template_t tmpl_suspend_bmc_arps_rq =
  {
    {8, "cmd"}, 
    {4, "channel_number"}, 
    {4, "reserved1"}, 
    
    {1, "gratuitous_arp_suspend"}, 
    {1, "arp_response_suspend"}, 
    {6, "reserved2"}, 
    
    {0, ""}
  };

fiid_template_t tmpl_suspend_bmc_arps_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    
    {1, "gratuitous_arp_response_status"}, 
    {1, "arp_response_status"}, 
    {6, "reserved2"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_get_lan_conf_param_rq =
  {
    {8, "cmd"}, 
    
    {4, "channel_number"}, 
    {3, "reserved1"}, 
    {1, "parameter_type"}, 
    
    {8, "parameter_selector"}, 
    
    {8, "set_selector"}, 
    {8, "block_selector"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_lan_conf_param_auth_type_enables_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {4, "present_revision"}, 
    {4, "oldest_revision_parameter"}, 

    /* for Authentication type enables */
    /* byte 1 */
    {1, "max_privilege_auth_type_callback_level.none"},
    {1, "max_privilege_auth_type_callback_level.md2"},
    {1, "max_privilege_auth_type_callback_level.md5"},
    {1, "max_privilege_auth_type_callback_level.reserved1"},
    {1, "max_privilege_auth_type_callback_level.straight_password"},
    {1, "max_privilege_auth_type_callback_level.oem_proprietary"},
/*     {6, "max_privilege_auth_type_callback_level"},  */
    {2, "max_privilege_auth_type_callback_level.reserved2"}, 
    /* byte 2 */
    {1, "max_privilege_auth_type_user_level.none"},
    {1, "max_privilege_auth_type_user_level.md2"},
    {1, "max_privilege_auth_type_user_level.md5"},
    {1, "max_privilege_auth_type_user_level.reserved1"},
    {1, "max_privilege_auth_type_user_level.straight_password"},
    {1, "max_privilege_auth_type_user_level.oem_proprietary"},
/*     {6, "max_privilege_auth_type_user_level"},  */
    {2, "max_privilege_auth_type_user_level.reserved2"}, 
    /* byte 3 */
    {1, "max_privilege_auth_type_operator_level.none"},
    {1, "max_privilege_auth_type_operator_level.md2"},
    {1, "max_privilege_auth_type_operator_level.md5"},
    {1, "max_privilege_auth_type_operator_level.reserved1"},
    {1, "max_privilege_auth_type_operator_level.straight_password"},
    {1, "max_privilege_auth_type_operator_level.oem_proprietary"},
/*     {6, "max_privilege_auth_type_operator_level"},  */
    {2, "max_privilege_auth_type_operator_level.reserved2"}, 
    /* byte 4 */
    {1, "max_privilege_auth_type_admin_level.none"},
    {1, "max_privilege_auth_type_admin_level.md2"},
    {1, "max_privilege_auth_type_admin_level.md5"},
    {1, "max_privilege_auth_type_admin_level.reserved1"},
    {1, "max_privilege_auth_type_admin_level.straight_password"},
    {1, "max_privilege_auth_type_admin_level.oem_proprietary"},
/*     {6, "max_privilege_auth_type_admin_level"},  */
    {2, "max_privilege_auth_type_admin_level.reserved2"}, 
    /* byte 5 */
    {1, "max_privilege_auth_type_oem_level.none"},
    {1, "max_privilege_auth_type_oem_level.md2"},
    {1, "max_privilege_auth_type_oem_level.md5"},
    {1, "max_privilege_auth_type_oem_level.reserved1"},
    {1, "max_privilege_auth_type_oem_level.straight_password"},
    {1, "max_privilege_auth_type_oem_level.oem_proprietary"},
/*     {6, "max_privilege_auth_type_oem_level"},  */
    {2, "max_privilege_auth_type_oem_level.reserved2"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_lan_conf_param_bmc_generated_arp_control_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    
    {4, "present_revision"}, 
    {4, "oldest_revision_parameter"}, 
    
    /* for BMC generated ARP control */
    {1, "bmc_generated_gratuitous_arps_flag"}, 
    {1, "bmc_generated_arp_responses_flag"}, 
    {6, "reserved2"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_get_lan_conf_param_gratuitous_arp_interval_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    
    {4, "present_revision"}, 
    {4, "oldest_revision_parameter"}, 
    
    {8, "gratuitous_arp_interval"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_get_lan_conf_param_ip_addr_source_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {4, "ip_addr_source"},
    {4, "reserved"},

    {0, ""}
  };

fiid_template_t tmpl_get_lan_conf_param_ip_addr_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {32, "ip_addr"},

    {0, ""}
  };

fiid_template_t tmpl_get_lan_conf_param_mac_addr_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {48, "mac_addr"},

    {0, ""}
  };

fiid_template_t tmpl_get_lan_conf_param_subnet_mask_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {32, "subnet_mask"},

    {0, ""}
  };

fiid_template_t tmpl_get_lan_conf_param_gw_ip_addr_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {32, "ip_addr"},

    {0, ""}
  };

fiid_template_t tmpl_get_lan_conf_param_gw_mac_addr_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {48, "mac_addr"},

    {0, ""}
  };

int8_t 
fill_lan_set_arp (fiid_obj_t obj_data_rq, 
		  u_int8_t channel_number, 
		  u_int8_t bmc_generated_gratuitous_arps_flag, 
		  u_int8_t bmc_generated_arp_responses_flag)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		"parameter_selector", 
		IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		"bmc_generated_gratuitous_arps_flag", 
		bmc_generated_gratuitous_arps_flag);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		"bmc_generated_arp_responses_flag", 
		bmc_generated_arp_responses_flag);
  
  return 0;
}

int8_t 
ipmi_lan_set_arp (u_int8_t channel_number, 
		  u_int8_t bmc_generated_gratuitous_arps_flag, 
		  u_int8_t bmc_generated_arp_responses_flag, 
		  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_bmc_generated_arp_control_rq);
  fill_lan_set_arp (obj_data_rq, 
		    channel_number, 
		    bmc_generated_gratuitous_arps_flag, 
		    bmc_generated_arp_responses_flag);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_lan_set_gratuitous_arp_interval (fiid_obj_t obj_data_rq, 
				      u_int8_t channel_number, 
				      u_int8_t gratuitous_arp_interval)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		"parameter_selector", 
		IPMI_LAN_PARAM_GRATUITOUS_ARP_INTERVAL);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		"gratuitous_arp_interval", 
		gratuitous_arp_interval);
  
  return 0;
}

int8_t 
ipmi_lan_set_gratuitous_arp_interval (u_int8_t channel_number, 
				      u_int8_t gratuitous_arp_interval, 
				      fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_gratuitous_arp_interval_rq);
  fill_lan_set_gratuitous_arp_interval (obj_data_rq, 
					channel_number, 
					gratuitous_arp_interval);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_lan_set_auth_type_enables (fiid_obj_t obj_data_rq, 
				u_int8_t channel_number, 
				u_int8_t max_privilege_auth_type_callback_level, 
				u_int8_t max_privilege_auth_type_user_level, 
				u_int8_t max_privilege_auth_type_operator_level, 
				u_int8_t max_privilege_auth_type_admin_level, 
				u_int8_t max_privilege_auth_type_oem_level)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"parameter_selector", 
		IPMI_LAN_PARAM_AUTH_TYPE_ENABLES);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"max_privilege_auth_type_callback_level", 
		max_privilege_auth_type_callback_level);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"max_privilege_auth_type_user_level", 
		max_privilege_auth_type_user_level);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"max_privilege_auth_type_operator_level", 
		max_privilege_auth_type_operator_level);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"max_privilege_auth_type_admin_level", 
		max_privilege_auth_type_admin_level);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		"max_privilege_auth_type_oem_level", 
		max_privilege_auth_type_oem_level);
  
  return 0;
}

int8_t 
ipmi_lan_set_auth_type_enables (u_int8_t channel_number, 
				u_int8_t max_privilege_auth_type_callback_level, 
				u_int8_t max_privilege_auth_type_user_level, 
				u_int8_t max_privilege_auth_type_operator_level, 
				u_int8_t max_privilege_auth_type_admin_level, 
				u_int8_t max_privilege_auth_type_oem_level, 
				fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_auth_type_enables_rq);
  fill_lan_set_auth_type_enables (obj_data_rq, 
				  channel_number, 
				  max_privilege_auth_type_callback_level, 
				  max_privilege_auth_type_user_level, 
				  max_privilege_auth_type_operator_level, 
				  max_privilege_auth_type_admin_level, 
				  max_privilege_auth_type_oem_level);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_auth_type_enables_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_lan_set_ip_addr_source (fiid_obj_t obj_data_rq, 
			     u_int8_t channel_number, 
			     u_int8_t ip_addr_source)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq,
		"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq, 
		"parameter_selector", 
		IPMI_LAN_PARAM_IP_ADDR_SOURCE);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq, 
		"ip_addr_source", 
		ip_addr_source);
  
  return 0;
}

int8_t 
ipmi_lan_set_ip_addr_source (u_int8_t channel_number,
			     u_int8_t ip_addr_source,
			     fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_ip_addr_source_rq);
  fill_lan_set_ip_addr_source (obj_data_rq, 
			       channel_number, 
			       ip_addr_source);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_ip_addr_source_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_lan_set_ip_addr (fiid_obj_t obj_data_rq, 
		      u_int8_t parameter_selector, 
		      u_int8_t channel_number, 
		      u_int32_t ip_addr)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq,
		"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq, 
		"parameter_selector", 
		parameter_selector);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq, 
		"ip_addr", 
		ip_addr);
  
  return 0;
}

int8_t 
ipmi_lan_set_ip_addr (u_int8_t channel_number,
		      u_int32_t ip_addr,
		      fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_ip_addr_rq);
  fill_lan_set_ip_addr (obj_data_rq, 
			IPMI_LAN_PARAM_IP_ADDR, 
			channel_number, 
			ip_addr);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_ip_addr_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_set_gw1_ip_addr (u_int8_t channel_number,
			  u_int32_t ip_addr,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_ip_addr_rq);
  fill_lan_set_ip_addr (obj_data_rq, 
			IPMI_LAN_PARAM_DEFAULT_GATEWAY_IP_ADDR, 
			channel_number, 
			ip_addr);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_ip_addr_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_set_gw2_ip_addr (u_int8_t channel_number,
			  u_int32_t ip_addr,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_ip_addr_rq);
  fill_lan_set_ip_addr (obj_data_rq, 
			IPMI_LAN_PARAM_BACKUP_GATEWAY_IP_ADDR, 
			channel_number, 
			ip_addr);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_ip_addr_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_lan_set_subnet_mask (fiid_obj_t obj_data_rq, 
			  u_int8_t channel_number, 
			  u_int32_t subnet_mask)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq,
		"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq, 
		"parameter_selector", 
		IPMI_LAN_PARAM_SUBNET_MASK);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq, 
		"subnet_mask", 
		subnet_mask);
  
  return 0;
}

int8_t 
ipmi_lan_set_subnet_mask (u_int8_t channel_number,
			  u_int32_t subnet_mask,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_subnet_mask_rq);
  fill_lan_set_subnet_mask (obj_data_rq, 
			    channel_number, 
			    subnet_mask);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_subnet_mask_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_lan_set_mac_addr (fiid_obj_t obj_data_rq, 
		       u_int8_t parameter_selector, 
		       u_int8_t channel_number, 
		       u_int64_t mac_addr)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq,
		"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq, 
		"parameter_selector", 
		parameter_selector);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq, 
		"mac_addr", 
		mac_addr);
  
  return 0;
}

int8_t 
ipmi_lan_set_mac_addr (u_int8_t channel_number,
		       u_int64_t mac_addr,
		       fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_mac_addr_rq);
  fill_lan_set_mac_addr (obj_data_rq, 
			 IPMI_LAN_PARAM_MAC_ADDR, 
			 channel_number, 
			 mac_addr);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_mac_addr_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_set_gw1_mac_addr (u_int8_t channel_number,
			   u_int64_t mac_addr,
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_mac_addr_rq);
  fill_lan_set_mac_addr (obj_data_rq, 
			 IPMI_LAN_PARAM_DEFAULT_GATEWAY_MAC_ADDR, 
			 channel_number, 
			 mac_addr);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_mac_addr_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_set_gw2_mac_addr (u_int8_t channel_number,
			   u_int64_t mac_addr,
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_set_lan_conf_param_mac_addr_rq);
  fill_lan_set_mac_addr (obj_data_rq, 
			 IPMI_LAN_PARAM_BACKUP_GATEWAY_MAC_ADDR, 
			 channel_number, 
			 mac_addr);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_lan_conf_param_mac_addr_rq, 
			 obj_data_rs, tmpl_set_lan_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_get_lan_conf_param (fiid_obj_t obj_data_rq, 
			 u_int8_t parameter_selector, 
			 u_int8_t channel_number,
			 u_int8_t parameter_type,
			 u_int8_t set_selector,
			 u_int8_t block_selector)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		"cmd", 
		IPMI_CMD_GET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		"parameter_type", 
		parameter_type);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		"parameter_selector", 
		parameter_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		"set_selector", 
		set_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		"block_selector", 
		block_selector);
  
  return 0;
}

int8_t 
ipmi_lan_get_arp (u_int8_t channel_number, 
		  u_int8_t parameter_type, 
		  u_int8_t set_selector, 
		  u_int8_t block_selector, 
		  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_bmc_generated_arp_control_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_gratuitous_arp_interval (u_int8_t channel_number, 
				      u_int8_t parameter_type, 
				      u_int8_t set_selector, 
				      u_int8_t block_selector, 
				      fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_GRATUITOUS_ARP_INTERVAL, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_gratuitous_arp_interval_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_auth_type_enables (u_int8_t channel_number, 
				u_int8_t parameter_type, 
				u_int8_t set_selector, 
				u_int8_t block_selector, 
				fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_AUTH_TYPE_ENABLES, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_auth_type_enables_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_ip_addr_source (u_int8_t channel_number, 
			     u_int8_t parameter_type, 
			     u_int8_t set_selector, 
			     u_int8_t block_selector, 
			     fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_IP_ADDR_SOURCE, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_ip_addr_source_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_ip_addr (u_int8_t channel_number,
		      u_int8_t parameter_type,
		      u_int8_t set_selector,
		      u_int8_t block_selector,
		      fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_IP_ADDR, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_ip_addr_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_gw1_ip_addr (u_int8_t channel_number,
			  u_int8_t parameter_type,
			  u_int8_t set_selector,
			  u_int8_t block_selector,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_DEFAULT_GATEWAY_IP_ADDR, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_gw_ip_addr_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_gw2_ip_addr (u_int8_t channel_number,
			  u_int8_t parameter_type,
			  u_int8_t set_selector,
			  u_int8_t block_selector,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_BACKUP_GATEWAY_IP_ADDR, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_gw_ip_addr_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_subnet_mask (u_int8_t channel_number,
			  u_int8_t parameter_type,
			  u_int8_t set_selector,
			  u_int8_t block_selector,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_SUBNET_MASK, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_subnet_mask_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_mac_addr (u_int8_t channel_number,
		       u_int8_t parameter_type,
		       u_int8_t set_selector,
		       u_int8_t block_selector,
		       fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_MAC_ADDR, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_mac_addr_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_gw1_mac_addr (u_int8_t channel_number,
			   u_int8_t parameter_type,
			   u_int8_t set_selector,
			   u_int8_t block_selector,
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_DEFAULT_GATEWAY_MAC_ADDR, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_mac_addr_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_lan_get_gw2_mac_addr (u_int8_t channel_number,
			   u_int8_t parameter_type,
			   u_int8_t set_selector,
			   u_int8_t block_selector,
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_lan_conf_param_rq);
  fill_get_lan_conf_param (obj_data_rq, 
			   IPMI_LAN_PARAM_BACKUP_GATEWAY_MAC_ADDR, 
			   channel_number, 
			   parameter_type, 
			   set_selector, 
			   block_selector);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_lan_conf_param_rq, 
			 obj_data_rs, tmpl_get_lan_conf_param_mac_addr_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_suspend_bmc_arps (fiid_obj_t obj_data_rq, 
		       u_int8_t channel_number, 
		       u_int8_t gratuitous_arp_suspend, 
		       u_int8_t arp_response_suspend)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		"cmd", 
		IPMI_CMD_SUSPEND_BMC_ARPS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		"gratuitous_arp_suspend", 
		gratuitous_arp_suspend);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		"arp_response_suspend", 
		arp_response_suspend);
  
  return 0;
}

int8_t 
ipmi_suspend_bmc_arps (u_int8_t channel_number, 
		       u_int8_t gratuitous_arp_suspend, 
		       u_int8_t arp_response_suspend, 
		       fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_suspend_bmc_arps_rq);
  fill_suspend_bmc_arps (obj_data_rq, 
			 channel_number, 
			 gratuitous_arp_suspend, 
			 arp_response_suspend);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_suspend_bmc_arps_rq, 
			 obj_data_rs, tmpl_suspend_bmc_arps_rs);
  free (obj_data_rq);
  return status;
}

