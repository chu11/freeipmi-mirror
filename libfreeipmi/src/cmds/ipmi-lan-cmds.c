/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-lan-cmds.h"
#include "freeipmi/spec/ipmi-channel-spec.h" 
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-lan-parameter-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rq = 
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,    "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,    "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,    "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_authentication_type_enables_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 1 */
    {1, "callback_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "callback_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 2 */
    {1, "user_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "user_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 3 */
    {1, "operator_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "operator_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 4 */
    {1, "admin_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "admin_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    /* byte 5 */
    {1, "oem_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "oem_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ip_address_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ip_address_source_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "ip_address_source", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_mac_address_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };
    
fiid_template_t tmpl_cmd_set_lan_configuration_parameters_subnet_mask_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "subnet_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_bmc_generated_arp_control_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "bmc_generated_gratuitous_arps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "bmc_generated_arp_responses", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_gratuitous_arp_interval_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "gratuitous_arp_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_default_gateway_address_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_default_gateway_mac_address_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_backup_gateway_address_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_backup_gateway_mac_address_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_community_string_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {144, "community_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_destination_type_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "destination_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "alert_acknowledge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "alert_acknowledge_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "retries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_destination_addresses_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "address_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "gateway_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "alerting_ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "alerting_mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_vlan_id_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {12, "vlan_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {3,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "vlan_id_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_vlan_priority_rq =
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

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rq = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "maximum_privilege_for_cipher_suite_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_15", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_16", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rs = 
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,    "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_lan_configuration_parameters_authentication_type_support_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "callback_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "callback_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "callback_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "user_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "user_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "operator_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "operator_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "operator_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "admin_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "admin_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "admin_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "oem_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oem_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "oem_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ip_address_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "ip_address_source", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_mac_address_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "subnet_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "bmc_generated_gratuitous_arps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "bmc_generated_arp_responses", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "gratuitous_arp_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_community_string_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {144, "community_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "number_of_lan_destinations", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_destination_type_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "destination_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "alert_acknowledge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "alert_acknowledge_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "retries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "address_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "gateway_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "alerting_ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "alerting_mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {12, "vlan_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {3,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "vlan_id_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "vlan_priority", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bits 3:4 in the IPMI spec do not exist.  */
    {2, "unspecified", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {0, "", 0}
  };

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "cipher_suite_entry_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_A", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_B", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_C", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_D", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_E", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_F", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_G", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_H", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_I", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_J", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_K", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_L", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_M", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_N", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_O", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {8, "cipher_suite_id_entry_P", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs = 
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "maximum_privilege_for_cipher_suite_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_15", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_for_cipher_suite_16", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_suspend_bmc_arps_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "gratuitous_arp_suspend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "arp_response_suspend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_suspend_bmc_arps_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "gratuitous_arp_response_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "arp_response_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_ip_udp_rmcp_statistics_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "clear_all_statistics", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_ip_udp_rmcp_statistics_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "ip_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "received_ip_header_errors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "received_ip_address_errors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "fragmented_ip_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "ip_packets_transmitted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "udp_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "valid_rmcp_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "udp_proxy_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "udp_proxy_packets_dropped", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

int8_t
fill_cmd_set_lan_configuration_parameters (uint8_t channel_number,
                                           uint8_t parameter_selector,
                                           uint8_t *configuration_parameter_data,
                                           uint8_t configuration_parameter_data_len,
					   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && configuration_parameter_data
	      && configuration_parameter_data_len
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, 
		"cmd", 
		IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  
  FIID_OBJ_SET_DATA (obj_cmd_rq, 
                     "configuration_parameter_data", 
                     configuration_parameter_data,
                     configuration_parameter_data_len);
  
  return 0;
}

int8_t 
fill_cmd_set_lan_configuration_parameters_authentication_type_enables (uint8_t channel_number, 
                                                                       uint8_t callback_level_none,
                                                                       uint8_t callback_level_md2,
                                                                       uint8_t callback_level_md5,
                                                                       uint8_t callback_level_straight_password,
                                                                       uint8_t callback_level_oem_proprietary,
                                                                       uint8_t user_level_none,
                                                                       uint8_t user_level_md2,
                                                                       uint8_t user_level_md5,
                                                                       uint8_t user_level_straight_password,
                                                                       uint8_t user_level_oem_proprietary,
                                                                       uint8_t operator_level_none,
                                                                       uint8_t operator_level_md2,
                                                                       uint8_t operator_level_md5,
                                                                       uint8_t operator_level_straight_password,
                                                                       uint8_t operator_level_oem_proprietary,
                                                                       uint8_t admin_level_none,
                                                                       uint8_t admin_level_md2,
                                                                       uint8_t admin_level_md5,
                                                                       uint8_t admin_level_straight_password,
                                                                       uint8_t admin_level_oem_proprietary,
                                                                       uint8_t oem_level_none,
                                                                       uint8_t oem_level_md2,
                                                                       uint8_t oem_level_md5,
                                                                       uint8_t oem_level_straight_password,
                                                                       uint8_t oem_level_oem_proprietary,
                                                                       fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_none)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_md2)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_md5)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_straight_password)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_oem_proprietary)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_none)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_md2)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_md5)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_straight_password)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_oem_proprietary)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_none)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_md2)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_md5)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_straight_password)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_oem_proprietary)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_none)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_md2)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_md5)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_straight_password)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_oem_proprietary)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_none)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_md2)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_md5)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_straight_password)
	      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_oem_proprietary)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_authentication_type_enables_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_AUTHENTICATION_TYPE_ENABLES);
  FIID_OBJ_SET (obj_cmd_rq, "callback_level.none", callback_level_none);
  FIID_OBJ_SET (obj_cmd_rq, "callback_level.md2", callback_level_md2);
  FIID_OBJ_SET (obj_cmd_rq, "callback_level.md5", callback_level_md5);
  FIID_OBJ_SET (obj_cmd_rq, "callback_level.reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "callback_level.straight_password", callback_level_straight_password);
  FIID_OBJ_SET (obj_cmd_rq, "callback_level.oem_proprietary", callback_level_oem_proprietary);
  FIID_OBJ_SET (obj_cmd_rq, "callback_level.reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "user_level.none", user_level_none);
  FIID_OBJ_SET (obj_cmd_rq, "user_level.md2", user_level_md2);
  FIID_OBJ_SET (obj_cmd_rq, "user_level.md5", user_level_md5);
  FIID_OBJ_SET (obj_cmd_rq, "user_level.reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "user_level.straight_password", user_level_straight_password);
  FIID_OBJ_SET (obj_cmd_rq, "user_level.oem_proprietary", user_level_oem_proprietary);
  FIID_OBJ_SET (obj_cmd_rq, "user_level.reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "operator_level.none", operator_level_none);
  FIID_OBJ_SET (obj_cmd_rq, "operator_level.md2", operator_level_md2);
  FIID_OBJ_SET (obj_cmd_rq, "operator_level.md5", operator_level_md5);
  FIID_OBJ_SET (obj_cmd_rq, "operator_level.reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "operator_level.straight_password", operator_level_straight_password);
  FIID_OBJ_SET (obj_cmd_rq, "operator_level.oem_proprietary", operator_level_oem_proprietary);
  FIID_OBJ_SET (obj_cmd_rq, "operator_level.reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "admin_level.none", admin_level_none);
  FIID_OBJ_SET (obj_cmd_rq, "admin_level.md2", admin_level_md2);
  FIID_OBJ_SET (obj_cmd_rq, "admin_level.md5", admin_level_md5);
  FIID_OBJ_SET (obj_cmd_rq, "admin_level.reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "admin_level.straight_password", admin_level_straight_password);
  FIID_OBJ_SET (obj_cmd_rq, "admin_level.oem_proprietary", admin_level_oem_proprietary);
  FIID_OBJ_SET (obj_cmd_rq, "admin_level.reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "oem_level.none", oem_level_none);
  FIID_OBJ_SET (obj_cmd_rq, "oem_level.md2", oem_level_md2);
  FIID_OBJ_SET (obj_cmd_rq, "oem_level.md5", oem_level_md5);
  FIID_OBJ_SET (obj_cmd_rq, "oem_level.reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "oem_level.straight_password", oem_level_straight_password);
  FIID_OBJ_SET (obj_cmd_rq, "oem_level.oem_proprietary", oem_level_oem_proprietary);
  FIID_OBJ_SET (obj_cmd_rq, "oem_level.reserved2", 0);
  return 0;
}

static int8_t
_fill_lan_ip(uint8_t parameter_selector,
             uint8_t channel_number,
             uint32_t ip_address,
             fiid_obj_t obj_cmd_rq)
{
  assert((parameter_selector == IPMI_LAN_PARAMETER_IP_ADDRESS
          || parameter_selector == IPMI_LAN_PARAMETER_DEFAULT_GATEWAY_ADDRESS
          || parameter_selector == IPMI_LAN_PARAMETER_BACKUP_GATEWAY_ADDRESS)
         && IPMI_CHANNEL_NUMBER_VALID(channel_number)
         && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_cmd_rq, "ip_address", ip_address);

  return (0);
}

int8_t 
fill_cmd_set_lan_configuration_parameters_ip_address (uint8_t channel_number, 
                                                      uint32_t ip_address,
                                                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_ip_address_rq);

  return (_fill_lan_ip(IPMI_LAN_PARAMETER_IP_ADDRESS,
                       channel_number,
                       ip_address,
                       obj_cmd_rq));
}

int8_t 
fill_cmd_set_lan_configuration_parameters_ip_address_source (uint8_t channel_number, 
                                                             uint8_t ip_address_source,
                                                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_IP_ADDRESS_SOURCE_VALID(ip_address_source)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_ip_address_source_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_IP_ADDRESS_SOURCE);
  FIID_OBJ_SET (obj_cmd_rq, "ip_address_source", ip_address_source);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return 0;
}

static int8_t
_fill_lan_mac_address(uint8_t parameter_selector,
                      uint8_t channel_number,
                      uint64_t mac_address,
                      fiid_obj_t obj_cmd_rq)
{
  assert((parameter_selector == IPMI_LAN_PARAMETER_MAC_ADDRESS
          || parameter_selector == IPMI_LAN_PARAMETER_DEFAULT_GATEWAY_MAC_ADDRESS
          || parameter_selector == IPMI_LAN_PARAMETER_BACKUP_GATEWAY_MAC_ADDRESS)
         && IPMI_CHANNEL_NUMBER_VALID(channel_number)
         && fiid_obj_valid(obj_cmd_rq));
         
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_cmd_rq, "mac_address", mac_address);

  return (0);
}

int8_t 
fill_cmd_set_lan_configuration_parameters_mac_address (uint8_t channel_number, 
                                                       uint64_t mac_address,
                                                       fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_mac_address_rq);

  return (_fill_lan_mac_address(IPMI_LAN_PARAMETER_MAC_ADDRESS,
                                channel_number,
                                mac_address,
                                obj_cmd_rq));
}

int8_t 
fill_cmd_set_lan_configuration_parameters_subnet_mask (uint8_t channel_number, 
                                                       uint32_t subnet_mask,
                                                       fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_subnet_mask_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_SUBNET_MASK);
  FIID_OBJ_SET (obj_cmd_rq, "subnet_mask", subnet_mask);
  
  return 0;
}

int8_t 
fill_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (uint8_t channel_number, 
                                                                     uint8_t bmc_generated_gratuitous_arps, 
                                                                     uint8_t bmc_generated_arp_responses,
                                                                     fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(bmc_generated_gratuitous_arps)
	      && IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(bmc_generated_arp_responses)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_bmc_generated_arp_control_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_BMC_GENERATED_ARP_CONTROL);
  FIID_OBJ_SET (obj_cmd_rq, "bmc_generated_gratuitous_arps", bmc_generated_gratuitous_arps);
  FIID_OBJ_SET (obj_cmd_rq, "bmc_generated_arp_responses", bmc_generated_arp_responses);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return 0;
}

int8_t 
fill_cmd_set_lan_configuration_parameters_gratuitous_arp_interval (uint8_t channel_number, 
                                                                   uint8_t gratuitous_arp_interval,
                                                                   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number) 
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_gratuitous_arp_interval_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_GRATUITOUS_ARP_INTERVAL);
  FIID_OBJ_SET (obj_cmd_rq, "gratuitous_arp_interval", gratuitous_arp_interval);
  
  return 0;
}

int8_t 
fill_cmd_set_lan_configuration_parameters_default_gateway_address (uint8_t channel_number, 
                                                                   uint32_t ip_address,
                                                                   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_default_gateway_address_rq);

  return (_fill_lan_ip(IPMI_LAN_PARAMETER_DEFAULT_GATEWAY_ADDRESS,
                       channel_number,
                       ip_address,
                       obj_cmd_rq));
}

int8_t 
fill_cmd_set_lan_configuration_parameters_default_gateway_mac_address (uint8_t channel_number, 
                                                                       uint64_t mac_address,
                                                                       fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_default_gateway_mac_address_rq);

  return (_fill_lan_mac_address(IPMI_LAN_PARAMETER_DEFAULT_GATEWAY_MAC_ADDRESS,
                                channel_number,
                                mac_address,
                                obj_cmd_rq));
}

int8_t 
fill_cmd_set_lan_configuration_parameters_backup_gateway_address (uint8_t channel_number, 
                                                                  uint32_t ip_address,
                                                                  fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_backup_gateway_address_rq);

  return (_fill_lan_ip(IPMI_LAN_PARAMETER_BACKUP_GATEWAY_ADDRESS,
                       channel_number,
                       ip_address,
                       obj_cmd_rq));
}

int8_t 
fill_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (uint8_t channel_number, 
                                                                      uint64_t mac_address,
                                                                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_backup_gateway_mac_address_rq);

  return (_fill_lan_mac_address(IPMI_LAN_PARAMETER_BACKUP_GATEWAY_MAC_ADDRESS,
                                channel_number,
                                mac_address,
                                obj_cmd_rq));
}

int8_t
fill_cmd_set_lan_configuration_parameters_community_string (uint8_t channel_number,
                                                            char *community_string,
                                                            unsigned int community_string_len,
                                                            fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_MAX_COMMUNITY_STRING_LENGTH];
  
  /* achu: community_string can be the max length.  Null termination in IPMI
   * packet not required.
   */
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && !(community_string
                   && community_string_len > IPMI_MAX_COMMUNITY_STRING_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_community_string_rq);
  
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_COMMUNITY_STRING);
  
  /* achu: community_string should be zero extended */
  memset(buf, '\0', IPMI_MAX_COMMUNITY_STRING_LENGTH);
  if (community_string)
    strncpy((char *)buf, community_string, IPMI_MAX_COMMUNITY_STRING_LENGTH);
  
  FIID_OBJ_SET_DATA (obj_cmd_rq, "community_string", buf, IPMI_MAX_COMMUNITY_STRING_LENGTH);

  return 0;
}

int8_t
fill_cmd_set_lan_configuration_parameters_destination_type (uint8_t channel_number,
                                                            uint8_t destination_selector,
                                                            uint8_t destination_type,
                                                            uint8_t alert_acknowledge,
                                                            uint8_t alert_acknowledge_timeout,
                                                            uint8_t retries,
                                                            fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && IPMI_DESTINATION_SELECTOR_VALID(destination_selector)
              && IPMI_DESTINATION_TYPE_VALID(destination_type)
              && IPMI_ALERT_VALID(alert_acknowledge)
              && (retries <= IPMI_ALERT_RETRIES_MAX)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_destination_type_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_DESTINATION_TYPE);
  FIID_OBJ_SET (obj_cmd_rq, "destination_selector", destination_selector);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "destination_type", destination_type);
  FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FIID_OBJ_SET (obj_cmd_rq, "alert_acknowledge", alert_acknowledge);
  FIID_OBJ_SET (obj_cmd_rq, "alert_acknowledge_timeout", alert_acknowledge_timeout);
  FIID_OBJ_SET (obj_cmd_rq, "retries", retries);
  FIID_OBJ_SET (obj_cmd_rq, "reserved4", 0);
  FIID_OBJ_SET (obj_cmd_rq, "reserved5", 0);

  return 0;
}

int8_t
fill_cmd_set_lan_configuration_parameters_destination_addresses (uint8_t channel_number,
                                                                 uint8_t destination_selector,
                                                                 uint8_t gateway_selector,
                                                                 uint32_t alerting_ip_address,
                                                                 uint64_t alerting_mac_address,
                                                                 fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && IPMI_DESTINATION_SELECTOR_VALID(destination_selector)
              && IPMI_GATEWAY_SELECTOR_VALID(gateway_selector)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_destination_addresses_rq);
 
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_DESTINATION_ADDRESSES);
  FIID_OBJ_SET (obj_cmd_rq, "destination_selector", destination_selector);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FIID_OBJ_SET (obj_cmd_rq, "address_format", IPMI_ADDRESS_FORMAT_IPV4);
  FIID_OBJ_SET (obj_cmd_rq, "gateway_selector", gateway_selector);
  FIID_OBJ_SET (obj_cmd_rq, "reserved4", 0);
  FIID_OBJ_SET (obj_cmd_rq, "alerting_ip_address", alerting_ip_address);
  FIID_OBJ_SET (obj_cmd_rq, "alerting_mac_address", alerting_mac_address);

  return 0;
}

int8_t 
fill_cmd_set_lan_configuration_parameters_vlan_id (uint8_t channel_number, 
                                                   uint16_t vlan_id,
                                                   uint8_t vlan_id_enable,
                                                   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_VLAN_ID_ENABLE_VALID(vlan_id_enable)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_vlan_id_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_VLAN_ID);
  FIID_OBJ_SET (obj_cmd_rq, "vlan_id", vlan_id);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "vlan_id_enable", vlan_id_enable);

  return 0;
}

int8_t 
fill_cmd_set_lan_configuration_parameters_vlan_priority (uint8_t channel_number, 
                                                         uint8_t vlan_priority,
                                                         fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_vlan_priority_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_VLAN_PRIORITY);
  FIID_OBJ_SET (obj_cmd_rq, "vlan_priority", vlan_priority);
  FIID_OBJ_SET (obj_cmd_rq, "unspecified", 0);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return 0;
}

int8_t
fill_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels(uint8_t channel_number,
											   uint8_t maximum_privilege_for_cipher_suite_1,
											   uint8_t maximum_privilege_for_cipher_suite_2,
											   uint8_t maximum_privilege_for_cipher_suite_3,
											   uint8_t maximum_privilege_for_cipher_suite_4,
											   uint8_t maximum_privilege_for_cipher_suite_5,
											   uint8_t maximum_privilege_for_cipher_suite_6,
											   uint8_t maximum_privilege_for_cipher_suite_7,
											   uint8_t maximum_privilege_for_cipher_suite_8,
											   uint8_t maximum_privilege_for_cipher_suite_9,
											   uint8_t maximum_privilege_for_cipher_suite_10,
											   uint8_t maximum_privilege_for_cipher_suite_11,
											   uint8_t maximum_privilege_for_cipher_suite_12,
											   uint8_t maximum_privilege_for_cipher_suite_13,
											   uint8_t maximum_privilege_for_cipher_suite_14,
											   uint8_t maximum_privilege_for_cipher_suite_15,
											   uint8_t maximum_privilege_for_cipher_suite_16,
											   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_1)
		  || !maximum_privilege_for_cipher_suite_1)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_2)
		  || !maximum_privilege_for_cipher_suite_2)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_3)
		  || !maximum_privilege_for_cipher_suite_3)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_4)
		  || !maximum_privilege_for_cipher_suite_4)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_5)
		  || !maximum_privilege_for_cipher_suite_5)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_6)
		  || !maximum_privilege_for_cipher_suite_6)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_7)
		  || !maximum_privilege_for_cipher_suite_7)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_8)
		  || !maximum_privilege_for_cipher_suite_8)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_9)
		  || !maximum_privilege_for_cipher_suite_9)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_10)
		  || !maximum_privilege_for_cipher_suite_10)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_11)
		  || !maximum_privilege_for_cipher_suite_11)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_12)
		  || !maximum_privilege_for_cipher_suite_12)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_13)
		  || !maximum_privilege_for_cipher_suite_13)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_14)
		  || !maximum_privilege_for_cipher_suite_14)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_15)
		  || !maximum_privilege_for_cipher_suite_15)
	      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_16)
		  || !maximum_privilege_for_cipher_suite_16)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_LAN_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_PRIVILEGE_LEVELS);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_1", maximum_privilege_for_cipher_suite_1); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_2", maximum_privilege_for_cipher_suite_2); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_3", maximum_privilege_for_cipher_suite_3); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_4", maximum_privilege_for_cipher_suite_4); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_5", maximum_privilege_for_cipher_suite_5); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_6", maximum_privilege_for_cipher_suite_6); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_7", maximum_privilege_for_cipher_suite_7); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_8", maximum_privilege_for_cipher_suite_8); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_9", maximum_privilege_for_cipher_suite_9); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_10", maximum_privilege_for_cipher_suite_10); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_11", maximum_privilege_for_cipher_suite_11); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_12", maximum_privilege_for_cipher_suite_12); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_13", maximum_privilege_for_cipher_suite_13); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_14", maximum_privilege_for_cipher_suite_14); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_15", maximum_privilege_for_cipher_suite_15); 
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_for_cipher_suite_16", maximum_privilege_for_cipher_suite_16); 

  return 0;
}

int8_t 
fill_cmd_get_lan_configuration_parameters (uint8_t channel_number,
                                           uint8_t get_parameter,
                                           uint8_t parameter_selector, 
                                           uint8_t set_selector,
                                           uint8_t block_selector,
                                           fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
	      && IPMI_LAN_PARAMETER_VALID(parameter_selector)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_LAN_CONFIGURATION_PARAMETERS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "get_parameter", get_parameter);
  FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FIID_OBJ_SET (obj_cmd_rq, "block_selector", block_selector);
  
  return 0;
}

int8_t 
fill_cmd_suspend_bmc_arps (uint8_t channel_number, 
			   uint8_t gratuitous_arp_suspend, 
			   uint8_t arp_response_suspend,
			   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(gratuitous_arp_suspend)
	      && IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(arp_response_suspend)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_suspend_bmc_arps_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SUSPEND_BMC_ARPS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "gratuitous_arp_suspend", gratuitous_arp_suspend);
  FIID_OBJ_SET (obj_cmd_rq, "arp_response_suspend", arp_response_suspend);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  
  return 0;
}

int8_t 
fill_cmd_get_ip_udp_rmcp_statistics (uint8_t channel_number, 
                                     uint8_t clear_all_statistics, 
                                     fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_CLEAR_ALL_STATISTICS_VALID(clear_all_statistics)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_ip_udp_rmcp_statistics_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_IP_UDP_RMCP_STATISTICS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "clear_all_statistics", clear_all_statistics);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  
  return 0;
}



