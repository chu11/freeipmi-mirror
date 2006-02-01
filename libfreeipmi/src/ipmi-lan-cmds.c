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

fiid_template_t tmpl_set_lan_conf_param_rq = 
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,    "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,    "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,    "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_bmc_generated_arp_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* for BMC generated ARP control */
    {1, "bmc_generated_gratuitous_arps_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "bmc_generated_arp_responses_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_gratuitous_arp_interval_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "gratuitous_arp_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_auth_type_enables_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* for Authentication type enables */
    /* byte 1 */
    {1, "max_privilege_auth_type_callback_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_callback_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 2 */
    {1, "max_privilege_auth_type_user_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_user_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 3 */
    {1, "max_privilege_auth_type_operator_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_operator_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 4 */
    {1, "max_privilege_auth_type_admin_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_admin_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 5 */
    {1, "max_privilege_auth_type_oem_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_oem_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_ip_addr_source_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    {4, "ip_addr_source", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_ip_addr_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {32, "ip_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };
    
fiid_template_t tmpl_set_lan_conf_param_subnet_mask_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {32, "subnet_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_mac_addr_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {48, "mac_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_vlan_id_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "vlan_id_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "vlan_id_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "vlan_id_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_lan_conf_param_vlan_priority_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {3, "vlan_priority", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bits 3:4 in the IPMI spec do not exist.  */
    {2, "unspecified", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_suspend_bmc_arps_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "gratuitous_arp_suspend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "arp_response_suspend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0, "", 0}
  };

fiid_template_t tmpl_suspend_bmc_arps_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "gratuitous_arp_response_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "arp_response_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "parameter_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_rs = 
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,    "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_auth_type_enables_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 

    /* for Authentication type enables */
    /* byte 1 */
    {1, "max_privilege_auth_type_callback_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_callback_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_callback_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 2 */
    {1, "max_privilege_auth_type_user_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_user_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_user_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 3 */
    {1, "max_privilege_auth_type_operator_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_operator_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_operator_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 4 */
    {1, "max_privilege_auth_type_admin_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_admin_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_admin_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 5 */
    {1, "max_privilege_auth_type_oem_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "max_privilege_auth_type_oem_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "max_privilege_auth_type_oem_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_bmc_generated_arp_control_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    /* for BMC generated ARP control */
    {1, "bmc_generated_gratuitous_arps_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "bmc_generated_arp_responses_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_gratuitous_arp_interval_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "gratuitous_arp_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_ip_addr_source_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "ip_addr_source", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_ip_addr_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {32, "ip_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_mac_addr_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {48, "mac_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_subnet_mask_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {32, "subnet_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_gw_ip_addr_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {32, "ip_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_gw_mac_addr_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {48, "mac_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_vlan_id_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "vlan_id_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "vlan_id_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "vlan_id_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_lan_conf_param_vlan_priority_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {3, "vlan_priority", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bits 3:4 in the IPMI spec do not exist.  */
    {2, "unspecified", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {0,  "", 0}
  };

int8_t
fill_lan_set_conf_param (fiid_obj_t obj_data_rq,
                         uint8_t channel_number,
                         uint8_t *configuration_parameter_data,
                         uint8_t configuration_parameter_data_len)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !configuration_parameter_data
      || !configuration_parameter_data_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved1",
                0);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL);
  
  FIID_OBJ_SET_DATA (obj_data_rq, 
                     (uint8_t *)"configuration_parameter_data", 
                     configuration_parameter_data,
                     configuration_parameter_data_len);
  
  return 0;
}

int8_t 
fill_lan_set_arp (fiid_obj_t obj_data_rq, 
		  uint8_t channel_number, 
		  uint8_t bmc_generated_gratuitous_arps_flag, 
		  uint8_t bmc_generated_arp_responses_flag)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_bmc_generated_arp_control_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved1",
                0);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"bmc_generated_gratuitous_arps_flag", 
		bmc_generated_gratuitous_arps_flag);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"bmc_generated_arp_responses_flag", 
		bmc_generated_arp_responses_flag);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  return 0;
}

int8_t 
fill_lan_set_gratuitous_arp_interval (fiid_obj_t obj_data_rq, 
				      uint8_t channel_number, 
				      uint8_t gratuitous_arp_interval)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_gratuitous_arp_interval_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_GRATUITOUS_ARP_INTERVAL);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"gratuitous_arp_interval", 
		gratuitous_arp_interval);
  
  return 0;
}

int8_t 
fill_lan_set_auth_type_enables (fiid_obj_t obj_data_rq, 
				uint8_t channel_number, 
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
                                int8_t auth_type_oem_oem_proprietary)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_auth_type_enables_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_AUTH_TYPE_ENABLES);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_callback_level.none",
                auth_type_callback_none);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_callback_level.md2",
                auth_type_callback_md2);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_callback_level.md5",
                auth_type_callback_md5);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_callback_level.reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_callback_level.straight_password",
                auth_type_callback_straight_password);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_callback_level.oem_proprietary",
                auth_type_callback_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_callback_level.reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_user_level.none",
                auth_type_user_none);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_user_level.md2",
                auth_type_user_md2);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_user_level.md5",
                auth_type_user_md5);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_user_level.reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_user_level.straight_password",
                auth_type_user_straight_password);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_user_level.oem_proprietary",
                auth_type_user_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_user_level.reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_operator_level.none",
                auth_type_operator_none);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_operator_level.md2",
                auth_type_operator_md2);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_operator_level.md5",
                auth_type_operator_md5);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_operator_level.reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_operator_level.straight_password",
                auth_type_operator_straight_password);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_operator_level.oem_proprietary",
                auth_type_operator_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_operator_level.reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_admin_level.none",
                auth_type_admin_none);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_admin_level.md2",
                auth_type_admin_md2);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_admin_level.md5",
                auth_type_admin_md5);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_admin_level.reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_admin_level.straight_password",
                auth_type_admin_straight_password);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_admin_level.oem_proprietary",
                auth_type_admin_oem_proprietary);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_admin_level.reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_oem_level.none",
                auth_type_oem_none);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_oem_level.md2",
                auth_type_oem_md2);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_oem_level.md5",
                auth_type_oem_md5);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_oem_level.reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_oem_level.straight_password",
                auth_type_oem_straight_password);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_oem_level.oem_proprietary",
                auth_type_oem_oem_proprietary);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"max_privilege_auth_type_oem_level.reserved2",
                0);

  return 0;
}

int8_t 
fill_lan_set_ip_addr_source (fiid_obj_t obj_data_rq, 
			     uint8_t channel_number, 
			     uint8_t ip_addr_source)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_IP_ADDR_SOURCE_VALID(ip_addr_source))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_ip_addr_source_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_IP_ADDR_SOURCE);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"ip_addr_source", 
		ip_addr_source);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  return 0;
}

int8_t 
fill_lan_set_ip_addr (fiid_obj_t obj_data_rq, 
		      uint8_t parameter_selector, 
		      uint8_t channel_number, 
		      uint32_t ip_addr)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_ip_addr_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"ip_addr", 
		ip_addr);
  
  return 0;
}

int8_t 
fill_lan_set_vlan_id (fiid_obj_t obj_data_rq, 
                      uint8_t channel_number, 
                      uint8_t vlan_id_enable,
                      uint32_t vlan_id)
{
  uint8_t *ptr, ls, ms;
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_vlan_id_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved1",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_VLAN_ID);

  ptr = (uint8_t *)&vlan_id;
#if WORDS_BIGENDIAN
  ls = ptr[3];
  ms = ptr[2];
#else
  ls = ptr[0];
  ms = ptr[1];
#endif
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"vlan_id_ls", 
		ls);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"vlan_id_ms", 
		ms);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"vlan_id_enable", 
		vlan_id_enable);

  return 0;
}

int8_t 
fill_lan_set_vlan_priority (fiid_obj_t obj_data_rq, 
                            uint8_t channel_number, 
                            uint8_t vlan_priority)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_vlan_priority_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved1", 
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_VLAN_PRIORITY);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"vlan_priority", 
		vlan_priority);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"unspecified", 
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved2", 
		0);

  return 0;
}

int8_t 
fill_lan_set_subnet_mask (fiid_obj_t obj_data_rq, 
			  uint8_t channel_number, 
			  uint32_t subnet_mask)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_subnet_mask_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved", 
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_LAN_PARAM_SUBNET_MASK);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"subnet_mask", 
		subnet_mask);
  
  return 0;
}

int8_t 
fill_lan_set_mac_addr (fiid_obj_t obj_data_rq, 
		       uint8_t parameter_selector, 
		       uint8_t channel_number, 
		       uint64_t mac_addr)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_lan_conf_param_mac_addr_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved", 
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"mac_addr", 
		mac_addr);
  
  return 0;
}

int8_t 
fill_get_lan_conf_param (fiid_obj_t obj_data_rq, 
			 uint8_t parameter_selector, 
			 uint8_t channel_number,
			 uint8_t parameter_type,
			 uint8_t set_selector,
			 uint8_t block_selector)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_lan_conf_param_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_LAN_CONF_PARAMS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved1", 
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_type", 
		parameter_type);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"set_selector", 
		set_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"block_selector", 
		block_selector);
  
  return 0;
}

int8_t 
fill_suspend_bmc_arps (fiid_obj_t obj_data_rq, 
		       uint8_t channel_number, 
		       uint8_t gratuitous_arp_suspend, 
		       uint8_t arp_response_suspend)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(gratuitous_arp_suspend)
      || !IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(arp_response_suspend))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_suspend_bmc_arps_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SUSPEND_BMC_ARPS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved1", 
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"gratuitous_arp_suspend", 
		gratuitous_arp_suspend);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"arp_response_suspend", 
		arp_response_suspend);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"reserved2", 
		0);
  
  return 0;
}

int8_t 
ipmi_cmd_lan_set_arp2 (ipmi_device_t *dev, 
		       uint8_t channel_number, 
		       uint8_t bmc_generated_gratuitous_arps_flag, 
		       uint8_t bmc_generated_arp_responses_flag, 
		       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;

  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_BMC_GENERATED_GRATUITOUS_ARPS_VALID(bmc_generated_gratuitous_arps_flag)
      || !IPMI_BMC_GENERATED_ARP_RESPONSES_VALID(bmc_generated_arp_responses_flag)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_bmc_generated_arp_control_rq)))
    goto cleanup;

  if (fill_lan_set_arp (obj_cmd_rq, 
                        channel_number, 
                        bmc_generated_gratuitous_arps_flag, 
                        bmc_generated_arp_responses_flag) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_lan_set_gratuitous_arp_interval2 (ipmi_device_t *dev, 
				       uint8_t channel_number, 
				       uint8_t gratuitous_arp_interval, 
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_gratuitous_arp_interval_rq)))
    goto cleanup;

  if (fill_lan_set_gratuitous_arp_interval (obj_cmd_rq, 
                                            channel_number, 
                                            gratuitous_arp_interval) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_auth_type_enables2 (ipmi_device_t *dev, 
				     uint8_t channel_number, 
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
                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_auth_type_enables_rq)))
    goto cleanup;

  if (fill_lan_set_auth_type_enables (obj_cmd_rq, 
                                      channel_number, 
                                      auth_type_callback_none,
                                      auth_type_callback_md2,
                                      auth_type_callback_md5,
                                      auth_type_callback_straight_password,
                                      auth_type_callback_oem_proprietary,
                                      auth_type_user_none,
                                      auth_type_user_md2,
                                      auth_type_user_md5,
                                      auth_type_user_straight_password,
                                      auth_type_user_oem_proprietary,
                                      auth_type_operator_none,
                                      auth_type_operator_md2,
                                      auth_type_operator_md5,
                                      auth_type_operator_straight_password,
                                      auth_type_operator_oem_proprietary,
                                      auth_type_admin_none,
                                      auth_type_admin_md2,
                                      auth_type_admin_md5,
                                      auth_type_admin_straight_password,
                                      auth_type_admin_oem_proprietary,
                                      auth_type_oem_none,
                                      auth_type_oem_md2,
                                      auth_type_oem_md5,
                                      auth_type_oem_straight_password,
                                      auth_type_oem_oem_proprietary) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_ip_addr_source2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t ip_addr_source, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_ip_addr_source_rq)))
    goto cleanup;

  if (fill_lan_set_ip_addr_source (obj_cmd_rq, 
                                   channel_number, 
                                   ip_addr_source) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_ip_addr2 (ipmi_device_t *dev, 
			   uint8_t channel_number, 
			   uint32_t ip_addr, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_ip_addr_rq)))
    goto cleanup;

  if (fill_lan_set_ip_addr (obj_cmd_rq, 
                            IPMI_LAN_PARAM_IP_ADDR, 
                            channel_number, 
                            ip_addr) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_default_gw_ip_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number, 
				      uint32_t ip_addr, 
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_ip_addr_rq)))
    goto cleanup;

  if (fill_lan_set_ip_addr (obj_cmd_rq, 
                            IPMI_LAN_PARAM_DEFAULT_GATEWAY_IP_ADDR, 
                            channel_number, 
                            ip_addr) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_backup_gw_ip_addr2 (ipmi_device_t *dev, 
				     uint8_t channel_number, 
				     uint32_t ip_addr, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_ip_addr_rq)))
    goto cleanup;

  if (fill_lan_set_ip_addr (obj_cmd_rq, 
                            IPMI_LAN_PARAM_BACKUP_GATEWAY_IP_ADDR, 
                            channel_number, 
                            ip_addr) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_vlan_id2 (ipmi_device_t *dev, 
			   uint8_t channel_number, 
			   uint8_t vlan_id_enable, 
			   uint32_t vlan_id, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_vlan_id_rq)))
    goto cleanup;

  if (fill_lan_set_vlan_id (obj_cmd_rq, 
                            channel_number,
                            vlan_id_enable,
                            vlan_id) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_vlan_priority2 (ipmi_device_t *dev, 
				 uint8_t channel_number,
				 uint32_t vlan_priority,
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_vlan_priority_rq)))
    goto cleanup;

  if (fill_lan_set_vlan_priority (obj_cmd_rq, 
                                  channel_number,
                                  vlan_priority) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_subnet_mask2 (ipmi_device_t *dev, 
			       uint8_t channel_number, 
			       uint32_t subnet_mask, 
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_subnet_mask_rq)))
    goto cleanup;

  if (fill_lan_set_subnet_mask (obj_cmd_rq, 
                                channel_number, 
                                subnet_mask) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_mac_addr2 (ipmi_device_t *dev, 
			    uint8_t channel_number,
			    uint64_t mac_addr,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_mac_addr_rq)))
    goto cleanup;

  if (fill_lan_set_mac_addr (obj_cmd_rq, 
                             IPMI_LAN_PARAM_MAC_ADDR, 
                             channel_number, 
                             mac_addr) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_default_gw_mac_addr2 (ipmi_device_t *dev, 
				       uint8_t channel_number,
				       uint64_t mac_addr,
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_mac_addr_rq)))
    goto cleanup;

  if (fill_lan_set_mac_addr (obj_cmd_rq, 
                             IPMI_LAN_PARAM_DEFAULT_GATEWAY_MAC_ADDR, 
                             channel_number, 
                             mac_addr) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_set_backup_gw_mac_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint64_t mac_addr,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_lan_conf_param_mac_addr_rq)))
    goto cleanup;

  if (fill_lan_set_mac_addr (obj_cmd_rq, 
                             IPMI_LAN_PARAM_BACKUP_GATEWAY_MAC_ADDR, 
                             channel_number, 
                             mac_addr) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_conf_param (ipmi_device_t *dev, 
                             uint8_t parameter_selector,
                             uint8_t channel_number, 
                             uint8_t parameter_type, 
                             uint8_t set_selector, 
                             uint8_t block_selector, 
                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;
  
  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               parameter_selector, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;
  
  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_arp2 (ipmi_device_t *dev, 
		       uint8_t channel_number, 
		       uint8_t parameter_type, 
		       uint8_t set_selector, 
		       uint8_t block_selector, 
		       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_bmc_generated_arp_control_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_gratuitous_arp_interval2 (ipmi_device_t *dev, 
					   uint8_t channel_number, 
					   uint8_t parameter_type, 
					   uint8_t set_selector, 
					   uint8_t block_selector, 
					   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_gratuitous_arp_interval_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               IPMI_LAN_PARAM_GRATUITOUS_ARP_INTERVAL, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_auth_type_enables2 (ipmi_device_t *dev, 
				     uint8_t channel_number, 
				     uint8_t parameter_type, 
				     uint8_t set_selector, 
				     uint8_t block_selector, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_auth_type_enables_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               IPMI_LAN_PARAM_AUTH_TYPE_ENABLES, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_ip_addr_source2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t parameter_type, 
				  uint8_t set_selector, 
				  uint8_t block_selector, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_ip_addr_source_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               IPMI_LAN_PARAM_IP_ADDR_SOURCE, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_ip_addr2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
			   uint8_t parameter_type,
			   uint8_t set_selector,
			   uint8_t block_selector,
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;  
  int ret, rv = -1;

  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_ip_addr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               IPMI_LAN_PARAM_IP_ADDR, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_default_gw_ip_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint8_t parameter_type,
				      uint8_t set_selector,
				      uint8_t block_selector,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_gw_ip_addr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               IPMI_LAN_PARAM_DEFAULT_GATEWAY_IP_ADDR, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_backup_gw_ip_addr2 (ipmi_device_t *dev, 
				     uint8_t channel_number,
				     uint8_t parameter_type,
				     uint8_t set_selector,
				     uint8_t block_selector,
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_gw_ip_addr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
                               IPMI_LAN_PARAM_BACKUP_GATEWAY_IP_ADDR, 
                               channel_number, 
                               parameter_type, 
                               set_selector, 
                               block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_subnet_mask2 (ipmi_device_t *dev, 
			       uint8_t channel_number,
			       uint8_t parameter_type,
			       uint8_t set_selector,
			       uint8_t block_selector,
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_subnet_mask_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
				IPMI_LAN_PARAM_SUBNET_MASK, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_mac_addr2 (ipmi_device_t *dev, 
			    uint8_t channel_number,
			    uint8_t parameter_type,
			    uint8_t set_selector,
			    uint8_t block_selector,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_mac_addr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
				IPMI_LAN_PARAM_MAC_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_default_gw_mac_addr2 (ipmi_device_t *dev, 
				       uint8_t channel_number,
				       uint8_t parameter_type,
				       uint8_t set_selector,
				       uint8_t block_selector,
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_mac_addr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
				IPMI_LAN_PARAM_DEFAULT_GATEWAY_MAC_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_backup_gw_mac_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint8_t parameter_type,
				      uint8_t set_selector,
				      uint8_t block_selector,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_mac_addr_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
				IPMI_LAN_PARAM_BACKUP_GATEWAY_MAC_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_vlan_id2 (ipmi_device_t *dev, 
			   uint8_t channel_number, 
			   uint8_t parameter_type, 
			   uint8_t set_selector, 
			   uint8_t block_selector, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_vlan_id_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
				IPMI_LAN_PARAM_VLAN_ID,
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_lan_get_vlan_priority2 (ipmi_device_t *dev, 
				 uint8_t channel_number, 
				 uint8_t parameter_type, 
				 uint8_t set_selector, 
				 uint8_t block_selector, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_lan_conf_param_vlan_priority_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_lan_conf_param_rq)))
    goto cleanup;

  if (fill_get_lan_conf_param (obj_cmd_rq, 
				IPMI_LAN_PARAM_VLAN_PRIORITY,
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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
ipmi_cmd_suspend_bmc_arps2 (ipmi_device_t *dev, 
			    uint8_t channel_number, 
			    uint8_t gratuitous_arp_suspend, 
			    uint8_t arp_response_suspend, 
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int ret, rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_suspend_bmc_arps_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_suspend_bmc_arps_rq)))
    goto cleanup;

  if (fill_suspend_bmc_arps (obj_cmd_rq, 
			      channel_number, 
			      gratuitous_arp_suspend, 
			      arp_response_suspend) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
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

