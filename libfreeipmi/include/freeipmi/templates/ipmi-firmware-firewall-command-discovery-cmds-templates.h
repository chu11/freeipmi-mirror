/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef _IPMI_FIRMWARE_FIRMWARE_COMMAND_DISCOVERY_CMDS_TEMPLATES_H
#define _IPMI_FIRMWARE_FIRMWARE_COMMAND_DISCOVERY_CMDS_TEMPLATES_H

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

Get NetFN Support Request
-------------------------

fiid_template_t tmpl_cmd_get_netfn_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get NetFN Support Response
--------------------------

fiid_template_t tmpl_cmd_get_netfn_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 2, "lun0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun0_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun1_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun2_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun3_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Command Support Request
---------------------------

fiid_template_t tmpl_cmd_get_command_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Command Support Response
----------------------------

fiid_template_t tmpl_cmd_get_command_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "command_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Command Sub-Function Support Request
----------------------------------------

fiid_template_t tmpl_cmd_get_command_sub_function_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Command Sub-Function Support (Specification Errata) Response
----------------------------------------------------------------

fiid_template_t tmpl_cmd_get_command_sub_function_support_specification_errata_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "errata_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "specification_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Command Sub-Function Support (Extension Errata) Response
------------------------------------------------------------

fiid_template_t tmpl_cmd_get_command_sub_function_support_extension_errata_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "oem_group_defining_body_errata", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Configurable Commands Request
---------------------------------

fiid_template_t tmpl_cmd_get_configurable_commands_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Configurable Commands Response
----------------------------------

fiid_template_t tmpl_cmd_get_configurable_commands_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "command_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Configurable Command Sub-Functions Request
----------------------------------------------

fiid_template_t tmpl_cmd_get_configurable_command_sub_functions_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Configurable Command Sub-Functions Response
-----------------------------------------------

fiid_template_t tmpl_cmd_get_configurable_command_sub_functions_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Command Enables Request
---------------------------

fiid_template_t tmpl_cmd_set_command_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "enable_disable_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set Command Enables Response
----------------------------

fiid_template_t tmpl_cmd_set_command_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Command Enables Request
---------------------------

fiid_template_t tmpl_cmd_get_command_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Command Enables Response
----------------------------

fiid_template_t tmpl_cmd_get_command_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "enable_disable_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Command Sub-Function Enables Request
----------------------------------------

fiid_template_t tmpl_cmd_set_command_sub_function_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Command Sub-Function Enables (Defining Body Code) Request
-------------------------------------------------------------

fiid_template_t tmpl_cmd_set_command_sub_function_enables_defining_body_code_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "defining_body_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };


Set Command Sub-Function Enables (OEM IANA) Request
---------------------------------------------------

fiid_template_t tmpl_cmd_set_command_sub_function_enables_oem_iana_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };


Set Command Sub-Function Enables Response
-----------------------------------------

fiid_template_t tmpl_cmd_set_command_sub_function_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Command Sub-Function Enables Request
----------------------------------------

fiid_template_t tmpl_cmd_get_command_sub_function_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Command Sub-Function Enables Response
-----------------------------------------

fiid_template_t tmpl_cmd_get_command_sub_function_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get OEM NetFN IANA Support Request
----------------------------------

fiid_template_t tmpl_cmd_get_oem_netfn_iana_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "list_index", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get OEM NetFN IANA Support Response
-----------------------------------

fiid_template_t tmpl_cmd_get_oem_netfn_iana_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 7, "last_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };


#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_EVENT_CMDS_TEMPLATES_H */
