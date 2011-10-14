/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef _IPMI_LAN_CMDS_TEMPLATES_H
#define _IPMI_LAN_CMDS_TEMPLATES_H

#ifdef __cplusplus
extern "C" {
#endif

/* This header file is for documentation only */

#if 0

Format = { bits, "field name", field flags }

FIID_FIELD_REQUIRED - field is required for the payload
FIID_FIELD_OPTIONAL - field is optional for the payload

FIID_FIELD_LENGTH_FIXED - field length is fixed at the number of bits listed
FIID_FIELD_LENGTH_VARIABLE - field length is variable for the number of bits listed

FIID_FIELD_MAKES_PACKET_SUFFICIENT - indicates field or fields are "sufficient" to make a valid packet

Set LAN Configuration Parameters Request
----------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set LAN Configuration Parameters Response
-----------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Set In Progress) Request
----------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Authentication Type Enables) Request
----------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_authentication_type_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* byte 1 */
    { 1, "callback_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "callback_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* byte 2 */
    { 1, "user_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* byte 3 */
    { 1, "operator_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operator_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* byte 4 */
    { 1, "admin_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "admin_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* byte 5 */
    { 1, "oem_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "oem_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (IP Address) Request
-----------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ip_address_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (IP Address Source) Request
------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ip_address_source_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "ip_address_source", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (MAC Address) Request
------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_mac_address_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Subnet Mask) Request
------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_subnet_mask_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "subnet_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (BMC Generated ARP Control) Request
--------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_bmc_generated_arp_control_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bmc_generated_gratuitous_arps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bmc_generated_arp_responses", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Gratuitous ARP Interval) Request
------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_gratuitous_arp_interval_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "gratuitous_arp_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Default Gateway Address) Request
------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_default_gateway_address_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Default Gateway MAC Address) Request
----------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_default_gateway_mac_address_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Backup Gateway Address) Request
-----------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_backup_gateway_address_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Backup Gateway MAC Address) Request
---------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_backup_gateway_mac_address_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Community String) Request
-----------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_community_string_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 144, "community_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Destination Type) Request
-----------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_destination_type_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "destination_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_acknowledge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "alert_acknowledge_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "retries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Destination Addresses) Request
----------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_destination_addresses_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "address_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "gateway_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "alerting_ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "alerting_mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (VLAN ID) Request
--------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_vlan_id_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 12, "vlan_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "vlan_id_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (VLAN Priority) Request
--------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_vlan_priority_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "vlan_priority", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bits 3:4 in the IPMI spec do not exist.  */
    { 2, "unspecified", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (RMCP+ Messaging Cipher Suite Privilege Levels) Request
----------------------------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_15", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_16", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set LAN Configuration Parameters (Bad Password Threshold) Request
-----------------------------------------------------------------

fiid_template_t tmpl_cmd_set_lan_configuration_parameters_bad_password_threshold_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_disabled_event_message", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "bad_password_threshold_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "attempt_count_reset_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "user_lockout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters Request
----------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters Response
-----------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Set In Progress) Response
-----------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_set_in_progress_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Authentication Type Support) Response
-----------------------------------------------------------------------

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_lan_configuration_parameters_authentication_type_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Authentication Type Enables) Response
-----------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "callback_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "callback_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operator_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operator_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "admin_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "admin_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.straight_password", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_level.oem_proprietary", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "oem_level.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (IP Address) Response
------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ip_address_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (IP Address Source) Response
-------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "ip_address_source", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (MAC Address) Response
-------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_mac_address_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Subnet Mask) Response
-------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "subnet_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

Get LAN Configuration Parameters (BMC Generated ARP Control) Response
---------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bmc_generated_gratuitous_arps", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bmc_generated_arp_responses", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Gratuitous ARP Interval) Response
-------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "gratuitous_arp_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Default Gateway Address) Response
-------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Default Gateway MAC Address) Response
-----------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Backup Gateway Address) Response
------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Backup Gateway MAC Address) Response
----------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Community String) Response
------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_community_string_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 144, "community_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Number of Destinations) Response
------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "number_of_lan_destinations", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Destination Type) Response
------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_destination_type_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "destination_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "alert_acknowledge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "alert_acknowledge_timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "retries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Destination Addresses) Response
-----------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "address_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "gateway_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "alerting_ip_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "alerting_mac_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (VLAN ID) Response
---------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 12, "vlan_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "vlan_id_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (VLAN Priority) Response
---------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "vlan_priority", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bits 3:4 in the IPMI spec do not exist.  */
    { 2, "unspecified", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

Get LAN Configuration Parameters (RMCP+ Messaging Cipher Suite Entry Support) Response
--------------------------------------------------------------------------------------

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "cipher_suite_entry_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (RMCP+ Messaging Cipher Suite Entries) Response
--------------------------------------------------------------------------------

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_A", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_B", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_C", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_D", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_E", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_F", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_G", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_H", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_I", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_J", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_K", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_L", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_M", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_N", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_O", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id_entry_P", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (RMCP+ Messaging Cipher Suite Privilege Levels) Response
-----------------------------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_15", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_for_cipher_suite_16", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get LAN Configuration Parameters (Bad Password Threshold) Response
------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_lan_configuration_parameters_bad_password_threshold_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_disabled_event_message", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "bad_password_threshold_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "attempt_count_reset_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "user_lockout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Suspend BMC ARPs Request
------------------------

fiid_template_t tmpl_cmd_suspend_bmc_arps_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "gratuitous_arp_suspend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "arp_response_suspend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Suspend BMC ARPs Response
-------------------------

fiid_template_t tmpl_cmd_suspend_bmc_arps_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "gratuitous_arp_response_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "arp_response_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get IP UDP RMCP Statistics Request
----------------------------------

fiid_template_t tmpl_cmd_get_ip_udp_rmcp_statistics_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_all_statistics", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get IP UDP RMCP Statistics Response
-----------------------------------

fiid_template_t tmpl_cmd_get_ip_udp_rmcp_statistics_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "ip_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "received_ip_header_errors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "received_ip_address_errors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "fragmented_ip_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "ip_packets_transmitted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "udp_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "valid_rmcp_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "udp_proxy_packets_received", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "udp_proxy_packets_dropped", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_LAN_CMDS_TEMPLATES_H */
