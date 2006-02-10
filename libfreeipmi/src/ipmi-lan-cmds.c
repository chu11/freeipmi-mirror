/* 
   ipmi-lan-cmds.c - IPMI LAN Commands

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
    {1, "max_privilege_auth_type_callback_level.none"},
    {1, "max_privilege_auth_type_callback_level.md2"},
    {1, "max_privilege_auth_type_callback_level.md5"},
    {1, "max_privilege_auth_type_callback_level.reserved1"},
    {1, "max_privilege_auth_type_callback_level.straight_password"},
    {1, "max_privilege_auth_type_callback_level.oem_proprietary"},
    {2, "max_privilege_auth_type_callback_level.reserved2"}, 
    /* byte 2 */
    {1, "max_privilege_auth_type_user_level.none"},
    {1, "max_privilege_auth_type_user_level.md2"},
    {1, "max_privilege_auth_type_user_level.md5"},
    {1, "max_privilege_auth_type_user_level.reserved1"},
    {1, "max_privilege_auth_type_user_level.straight_password"},
    {1, "max_privilege_auth_type_user_level.oem_proprietary"},
    {2, "max_privilege_auth_type_user_level.reserved2"}, 
    /* byte 3 */
    {1, "max_privilege_auth_type_operator_level.none"},
    {1, "max_privilege_auth_type_operator_level.md2"},
    {1, "max_privilege_auth_type_operator_level.md5"},
    {1, "max_privilege_auth_type_operator_level.reserved1"},
    {1, "max_privilege_auth_type_operator_level.straight_password"},
    {1, "max_privilege_auth_type_operator_level.oem_proprietary"},
    {2, "max_privilege_auth_type_operator_level.reserved2"}, 
    /* byte 4 */
    {1, "max_privilege_auth_type_admin_level.none"},
    {1, "max_privilege_auth_type_admin_level.md2"},
    {1, "max_privilege_auth_type_admin_level.md5"},
    {1, "max_privilege_auth_type_admin_level.reserved1"},
    {1, "max_privilege_auth_type_admin_level.straight_password"},
    {1, "max_privilege_auth_type_admin_level.oem_proprietary"},
    {2, "max_privilege_auth_type_admin_level.reserved2"}, 
    /* byte 5 */
    {1, "max_privilege_auth_type_oem_level.none"},
    {1, "max_privilege_auth_type_oem_level.md2"},
    {1, "max_privilege_auth_type_oem_level.md5"},
    {1, "max_privilege_auth_type_oem_level.reserved1"},
    {1, "max_privilege_auth_type_oem_level.straight_password"},
    {1, "max_privilege_auth_type_oem_level.oem_proprietary"},
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

fiid_template_t tmpl_set_lan_conf_param_vlan_id_rq =
  {
    {8, "cmd"},
    {4, "channel_number"},
    {4, "reserved1"},
    {8, "parameter_selector"},

    {8, "vlan_id_ls"},
    {4, "vlan_id_ms"},
    {3, "reserved2"},
    {1, "vlan_id_enable"},

    {0, ""}
  };

fiid_template_t tmpl_set_lan_conf_param_vlan_priority_rq =
  {
    {8, "cmd"},
    {4, "channel_number"},
    {4, "reserved1"},
    {8, "parameter_selector"},

    {3, "vlan_priority"},
    {2, "unspecified"},
    {3, "reserved2"},

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
    {2, "max_privilege_auth_type_callback_level.reserved2"}, 
    /* byte 2 */
    {1, "max_privilege_auth_type_user_level.none"},
    {1, "max_privilege_auth_type_user_level.md2"},
    {1, "max_privilege_auth_type_user_level.md5"},
    {1, "max_privilege_auth_type_user_level.reserved1"},
    {1, "max_privilege_auth_type_user_level.straight_password"},
    {1, "max_privilege_auth_type_user_level.oem_proprietary"},
    {2, "max_privilege_auth_type_user_level.reserved2"}, 
    /* byte 3 */
    {1, "max_privilege_auth_type_operator_level.none"},
    {1, "max_privilege_auth_type_operator_level.md2"},
    {1, "max_privilege_auth_type_operator_level.md5"},
    {1, "max_privilege_auth_type_operator_level.reserved1"},
    {1, "max_privilege_auth_type_operator_level.straight_password"},
    {1, "max_privilege_auth_type_operator_level.oem_proprietary"},
    {2, "max_privilege_auth_type_operator_level.reserved2"}, 
    /* byte 4 */
    {1, "max_privilege_auth_type_admin_level.none"},
    {1, "max_privilege_auth_type_admin_level.md2"},
    {1, "max_privilege_auth_type_admin_level.md5"},
    {1, "max_privilege_auth_type_admin_level.reserved1"},
    {1, "max_privilege_auth_type_admin_level.straight_password"},
    {1, "max_privilege_auth_type_admin_level.oem_proprietary"},
    {2, "max_privilege_auth_type_admin_level.reserved2"}, 
    /* byte 5 */
    {1, "max_privilege_auth_type_oem_level.none"},
    {1, "max_privilege_auth_type_oem_level.md2"},
    {1, "max_privilege_auth_type_oem_level.md5"},
    {1, "max_privilege_auth_type_oem_level.reserved1"},
    {1, "max_privilege_auth_type_oem_level.straight_password"},
    {1, "max_privilege_auth_type_oem_level.oem_proprietary"},
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

fiid_template_t tmpl_get_lan_conf_param_vlan_id_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    
    {4, "present_revision"}, 
    {4, "oldest_revision_parameter"}, 
    
    {8, "vlan_id_ls"},
    {4, "vlan_id_ms"},
    {3, "reserved"},
    {1, "vlan_id_enable"},
    
    {0,  ""}
  };

fiid_template_t tmpl_get_lan_conf_param_vlan_priority_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    
    {4, "present_revision"}, 
    {4, "oldest_revision_parameter"}, 
    
    {3, "vlan_priority"},
    {2, "unspecified"},
    {3, "reserved"},
    
    {0,  ""}
  };

int8_t 
fill_lan_set_arp (uint8_t channel_number, 
		  uint8_t bmc_generated_gratuitous_arps_flag, 
		  uint8_t bmc_generated_arp_responses_flag,
                  fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		(uint8_t *)"bmc_generated_gratuitous_arps_flag", 
		bmc_generated_gratuitous_arps_flag);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		(uint8_t *)"bmc_generated_arp_responses_flag", 
		bmc_generated_arp_responses_flag);
  
  return 0;
}

int8_t 
fill_lan_set_gratuitous_arp_interval (uint8_t channel_number, 
				      uint8_t gratuitous_arp_interval,
                                      fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_GRATUITOUS_ARP_INTERVAL);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		(uint8_t *)"gratuitous_arp_interval", 
		gratuitous_arp_interval);
  
  return 0;
}

int8_t 
fill_lan_set_auth_type_enables (uint8_t channel_number, 
                                int8_t auth_type_callback_none,
                                int8_t auth_type_callback_md2,
                                int8_t auth_type_callback_md5,
                                int8_t auth_type_callback_straight_password,
                                int8_t auth_type_callback_oem_proprietary,
                                int8_t auth_type_user_none,
                                int8_t auth_type_user_md2,
                                int8_t auth_type_user_md5,
                                int8_t auth_type_user_straight_password,
                                int8_t auth_type_user_oem_proprietary,
                                int8_t auth_type_operator_none,
                                int8_t auth_type_operator_md2,
                                int8_t auth_type_operator_md5,
                                int8_t auth_type_operator_straight_password,
                                int8_t auth_type_operator_oem_proprietary,
                                int8_t auth_type_admin_none,
                                int8_t auth_type_admin_md2,
                                int8_t auth_type_admin_md5,
                                int8_t auth_type_admin_straight_password,
                                int8_t auth_type_admin_oem_proprietary,
                                int8_t auth_type_oem_none,
                                int8_t auth_type_oem_md2,
                                int8_t auth_type_oem_md5,
                                int8_t auth_type_oem_straight_password,
                                int8_t auth_type_oem_oem_proprietary,
                                fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_AUTH_TYPE_ENABLES);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_callback_level.none",
                auth_type_callback_none);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_callback_level.md2",
                auth_type_callback_md2);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_callback_level.md5",
                auth_type_callback_md5);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_callback_level.straight_password",
                auth_type_callback_straight_password);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_callback_level.oem_proprietary",
                auth_type_callback_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_user_level.none",
                auth_type_user_none);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_user_level.md2",
                auth_type_user_md2);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_user_level.md5",
                auth_type_user_md5);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_user_level.straight_password",
                auth_type_user_straight_password);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_user_level.oem_proprietary",
                auth_type_user_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_operator_level.none",
                auth_type_operator_none);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_operator_level.md2",
                auth_type_operator_md2);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_operator_level.md5",
                auth_type_operator_md5);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_operator_level.straight_password",
                auth_type_operator_straight_password);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_operator_level.oem_proprietary",
                auth_type_operator_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_admin_level.none",
                auth_type_admin_none);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_admin_level.md2",
                auth_type_admin_md2);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_admin_level.md5",
                auth_type_admin_md5);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_admin_level.straight_password",
                auth_type_admin_straight_password);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_admin_level.oem_proprietary",
                auth_type_admin_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_oem_level.none",
                auth_type_oem_none);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_oem_level.md2",
                auth_type_oem_md2);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_oem_level.md5",
                auth_type_oem_md5);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_oem_level.straight_password",
                auth_type_oem_straight_password);

  FIID_OBJ_SET (obj_data_rq,
		tmpl_set_lan_conf_param_auth_type_enables_rq, 
                (uint8_t *)"max_privilege_auth_type_oem_level.oem_proprietary",
                auth_type_oem_oem_proprietary);
  
  return 0;
}

int8_t 
fill_lan_set_ip_addr_source (uint8_t channel_number, 
			     uint8_t ip_addr_source,
                             fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_IP_ADDR_SOURCE_VALID(ip_addr_source))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_IP_ADDR_SOURCE);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_source_rq, 
		(uint8_t *)"ip_addr_source", 
		ip_addr_source);
  
  return 0;
}

int8_t 
fill_lan_set_ip_addr (uint8_t parameter_selector, 
		      uint8_t channel_number, 
		      uint32_t ip_addr,
                      fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_ip_addr_rq, 
		(uint8_t *)"ip_addr", 
		ip_addr);
  
  return 0;
}

int8_t 
fill_lan_set_vlan_id (uint8_t channel_number, 
                      uint8_t vlan_id_enable,
                      uint32_t vlan_id,
                      fiid_obj_t obj_data_rq)
{
  uint8_t *ptr, ls, ms;

  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_id_rq,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_id_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_id_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_VLAN_ID);

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_id_rq, 
		(uint8_t *)"vlan_id_enable", 
		vlan_id_enable);

  ptr = (uint8_t *)&vlan_id;
#if WORDS_BIGENDIAN
  ls = ptr[3];
  ms = ptr[2];
#else
  ls = ptr[0];
  ms = ptr[1];
#endif
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_id_rq, 
		(uint8_t *)"vlan_id_ls", 
		ls);

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_id_rq, 
		(uint8_t *)"vlan_id_ms", 
		ms);
  
  return 0;
}

int8_t 
fill_lan_set_vlan_priority (uint8_t channel_number, 
                            uint8_t vlan_priority,
                            fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_priority_rq,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_priority_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_priority_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_VLAN_PRIORITY);

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_vlan_priority_rq, 
		(uint8_t *)"vlan_priority", 
		vlan_priority);
  
  return 0;
}

int8_t 
fill_lan_set_subnet_mask (uint8_t channel_number, 
			  uint32_t subnet_mask,
                          fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_SUBNET_MASK);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_subnet_mask_rq, 
		(uint8_t *)"subnet_mask", 
		subnet_mask);
  
  return 0;
}

int8_t 
fill_lan_set_mac_addr (uint8_t parameter_selector, 
		       uint8_t channel_number, 
		       uint64_t mac_addr,
                       fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq,
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_lan_conf_param_mac_addr_rq, 
		(uint8_t *)"mac_addr", 
		mac_addr);
  
  return 0;
}

int8_t 
fill_get_lan_conf_param (uint8_t parameter_selector, 
			 uint8_t channel_number,
			 uint8_t parameter_type,
			 uint8_t set_selector,
			 uint8_t block_selector,
                         fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		(uint8_t *)"parameter_type", 
		parameter_type);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		(uint8_t *)"set_selector", 
		set_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_lan_conf_param_rq, 
		(uint8_t *)"block_selector", 
		block_selector);
  
  return 0;
}

int8_t 
fill_suspend_bmc_arps (uint8_t channel_number, 
		       uint8_t gratuitous_arp_suspend, 
		       uint8_t arp_response_suspend,
                       fiid_obj_t obj_data_rq)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(gratuitous_arp_suspend)
      || !IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(arp_response_suspend))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SUSPEND_BMC_ARPS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		(uint8_t *)"gratuitous_arp_suspend", 
		gratuitous_arp_suspend);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_suspend_bmc_arps_rq, 
		(uint8_t *)"arp_response_suspend", 
		arp_response_suspend);
  
  return 0;
}

