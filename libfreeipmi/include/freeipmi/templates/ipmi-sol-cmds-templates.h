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

#ifndef _IPMI_SOL_CMDS_TEMPLATES_H
#define _IPMI_SOL_CMDS_TEMPLATES_H

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

Set SOL Configuration Parameters Request
----------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set SOL Configuration Parameters Response
-----------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (Set In Progress) Request
----------------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (SOL Enable) Request
-----------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_enable_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sol_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (SOL Authentication) Request
--------------------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_authentication_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "sol_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_encryption", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (Character Accumulate Interval and Send Threshold) Request
-------------------------------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_accumulate_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_send_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (SOL Retry) Request
----------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_retry_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "retry_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (SOL Non-Volatile Bit Rate) Request
--------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (SOL Volatile Bit Rate) Request
----------------------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SOL Configuration Parameters (SOL Payload Port Number) Request
------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_payload_port_number_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters Request
----------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters Response
-----------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (Set In Progress) Response
-----------------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_set_in_progress_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (SOL Enable) Response
------------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_enable_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sol_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (SOL Authentication) Response
--------------------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_authentication_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "sol_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_encryption", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (Character Accumulate Interval and Send Threshold) Response
--------------------------------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_accumulate_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_send_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (SOL Retry) Response
-----------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_retry_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "retry_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (SOL Non-Volatile Bit Rate) Response
---------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (SOL Volatile Bit Rate) Response
-----------------------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (SOL Payload Channel) Response
---------------------------------------------------------------

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_channel", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SOL Configuration Parameters (SOL Payload Port Number) Response
-------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_payload_port_number_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_SOL_CMDS_TEMPLATES_H */
