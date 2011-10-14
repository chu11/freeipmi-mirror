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

#ifndef _IPMI_RMCPPLUS_SUPPORT_AND_PAYLOAD_CMDS_TEMPLATES_H
#define _IPMI_RMCPPLUS_SUPPORT_AND_PAYLOAD_CMDS_TEMPLATES_H

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

Activate Payload Request
------------------------

fiid_template_t tmpl_cmd_activate_payload_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "auxiliary_request_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Activate Payload (SOL) Request
------------------------------

fiid_template_t tmpl_cmd_activate_payload_sol_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sol_startup_handshake", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "shared_serial_alert_behavior", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "test_mode", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_activation", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "encryption_activation", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Activate Payload Response
-------------------------

fiid_template_t tmpl_cmd_activate_payload_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "auxiliary_response_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "inbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "outbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_udp_port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_vlan_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Activate Payload (SOL) Response
-------------------------------

fiid_template_t tmpl_cmd_activate_payload_sol_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "test_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 31, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "inbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "outbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_udp_port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_vlan_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Deactivate Payload Request
--------------------------

fiid_template_t tmpl_cmd_deactivate_payload_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "payload_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Deactivate Payload Response
---------------------------

fiid_template_t tmpl_cmd_deactivate_payload_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Suspend Resume Payload Encryption Request
-----------------------------------------

fiid_template_t tmpl_cmd_suspend_resume_payload_encryption_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* achu: The IPMI spec says [4:0] for Operation and [7:2] for
       reserved3.  Needless to say, one is wrong.  Since there are
       only three operations, we'll assume they meant for operation to
       be a 2 bit field
    */
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Suspend Resume Payload Encryption Response
------------------------------------------

fiid_template_t tmpl_cmd_suspend_resume_payload_encryption_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Payload Activation Status Request
-------------------------------------

fiid_template_t tmpl_cmd_get_payload_activation_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Payload Activation Status Response
--------------------------------------

fiid_template_t tmpl_cmd_get_payload_activation_status_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "instance_capacity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_15", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_16", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Payload Instance Info Request
---------------------------------

fiid_template_t tmpl_cmd_get_payload_instance_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Payload Instance Info Response
----------------------------------

fiid_template_t tmpl_cmd_get_payload_instance_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 56, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set User Payload Access Request
-------------------------------

fiid_template_t tmpl_cmd_set_user_payload_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set User Payload Access Response
--------------------------------

fiid_template_t tmpl_cmd_set_user_payload_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get User Payload Access Request
-------------------------------

fiid_template_t tmpl_cmd_get_user_payload_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get User Payload Access Response
--------------------------------

fiid_template_t tmpl_cmd_get_user_payload_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Payload Support Request
-----------------------------------

fiid_template_t tmpl_cmd_get_channel_payload_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Payload Support Response
------------------------------------

fiid_template_t tmpl_cmd_get_channel_payload_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "standard_payload_type_0_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_1_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_2_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_3_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_4_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_5_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_6_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_7_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_8_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_9_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_10_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_11_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_12_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_13_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_14_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_15_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_0_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_1_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_2_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_3_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_4_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_5_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_6_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_7_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_8_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_9_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_10_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_11_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_12_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_13_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_14_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_15_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_20h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_21h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_22h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_23h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_24h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_25h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_26h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_27h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_28h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_29h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Ah_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Bh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Ch_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Dh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Eh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Fh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Payload Version Request
-----------------------------------

fiid_template_t tmpl_cmd_get_channel_payload_version_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Payload Version Response
------------------------------------

fiid_template_t tmpl_cmd_get_channel_payload_version_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "minor_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "major_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel OEM Payload Info Request
------------------------------------

fiid_template_t tmpl_cmd_get_channel_oem_payload_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "oem_payload_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel OEM Payload Info Response
-------------------------------------

fiid_template_t tmpl_cmd_get_channel_oem_payload_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "oem_payload_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "minor_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "major_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_RMCPPLUS_SUPPORT_AND_PAYLOAD_CMDS_TEMPLATES_H */
