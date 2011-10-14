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

#ifndef _IPMI_MESSAGING_SUPPORT_CMDS_TEMPLATES_H
#define _IPMI_MESSAGING_SUPPORT_CMDS_TEMPLATES_H

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

FIID_FIELD_MAKES_PACKET_SUFFICIENT - indicates field or fields are "sufficient" to make a valid packet

Set BMC Global Enables Request
------------------------------

fiid_template_t tmpl_cmd_set_bmc_global_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "receive_message_queue_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer_full_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "system_event_logging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set BMC Global Enables Response
-------------------------------

fiid_template_t tmpl_cmd_set_bmc_global_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get BMC Global Enables Request
------------------------------

fiid_template_t tmpl_cmd_get_bmc_global_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get BMC Global Enables Response
-------------------------------

fiid_template_t tmpl_cmd_get_bmc_global_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "receive_message_queue_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer_full_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "system_event_logging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Clear Message Flags Request
---------------------------

fiid_template_t tmpl_cmd_clear_message_flags_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_receive_message_queue", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_event_message_buffer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_watchdog_pre_timeout_interrupt_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_oem_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_oem_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_oem_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Clear Message Flags Response
----------------------------

fiid_template_t tmpl_cmd_clear_message_flags_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Message Flags Request
-------------------------

fiid_template_t tmpl_cmd_get_message_flags_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Message Flags Response
--------------------------

fiid_template_t tmpl_cmd_get_message_flags_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "receive_message_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer_full", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "watchdog_pre_timeout_interrupt_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_0_data_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_1_data_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_2_data_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Enable Message Channel Receive Request
--------------------------------------

fiid_template_t tmpl_cmd_enable_message_channel_receive_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Enable Message Channel Receive Response
---------------------------------------

fiid_template_t tmpl_cmd_enable_message_channel_receive_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Message Request
-------------------

fiid_template_t tmpl_cmd_get_message_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Message Response
--------------------

fiid_template_t tmpl_cmd_get_message_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "inferred_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "message_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Send Message Request
--------------------

fiid_template_t tmpl_cmd_send_message_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "send_message_with_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "send_message_with_encryption", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "tracking_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "message_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Send Message Response
---------------------

fiid_template_t tmpl_cmd_send_message_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1024, "response_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Read Event Message Buffer Request
---------------------------------

fiid_template_t tmpl_cmd_read_event_message_buffer_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Read Event Message Buffer Response
----------------------------------

fiid_template_t tmpl_cmd_read_event_message_buffer_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "message_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Interface Capabilities Request
-----------------------------------------

fiid_template_t tmpl_cmd_get_system_interface_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "system_interface", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };


Get System Interface Capabilities Response
------------------------------------------

fiid_template_t tmpl_cmd_get_system_interface_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get System Interface Capabilities (SSIF) Response
--------------------------------------------------

fiid_template_t tmpl_cmd_get_system_interface_capabilities_ssif_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "ssif_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pec_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "transaction_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "input_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "output_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Interface Capabilities (KCS) Response
------------------------------------------------

fiid_template_t tmpl_cmd_get_system_interface_capabilities_kcs_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "system_interface_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "input_maximum_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get BT Interface Capabilities Request
-------------------------------------

fiid_template_t tmpl_cmd_get_bt_interface_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get BT Interface Capabilities Response
--------------------------------------

fiid_template_t tmpl_cmd_get_bt_interface_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "number_of_outstanding_requests_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "input_buffer_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* in bytes */
    { 8, "output_buffer_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* in bytes */
    { 8, "bmc_request_to_response_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* in seconds */
    { 8, "recommended_retries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Master Write-Read Request
-------------------------

fiid_template_t tmpl_cmd_master_write_read_rq = 
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bus_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "read_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2040, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Master Write-Read Response
--------------------------

fiid_template_t tmpl_cmd_master_write_read_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 2040, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Channel Authentication Capabilities Request
-----------------------------------------------

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_ipmi_v2.0_extended_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Authentication Capabilities Response
------------------------------------------------

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.straight_password_key", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.oem_prop", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.ipmi_v2.0_extended_capabilities_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.anonymous_login", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.non_null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.k_g", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "authentication_status.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_supports_ipmi_v1.5_connections", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_supports_ipmi_v2.0_connections", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "oem_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System GUID Request
-----------------------

fiid_template_t tmpl_cmd_get_system_guid_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System GUID Response
------------------------

fiid_template_t tmpl_cmd_get_system_guid_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System GUID (with Format) Response
--------------------------------------

fiid_template_t tmpl_cmd_get_system_guid_format_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 48, "node", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 8, "clock_seq_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "clock_seq_hi_and_reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "time_high_and_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 16, "time_mid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 32, "time_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 0, "", 0}
  };

Set System Info Parameters Request
----------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set System Info Parameters Response
-----------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set System Info Parameters (Set In Progress) Request
----------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Info Parameters (System Firmware Version First Set) Request
----------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_system_firmware_version_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set System Info Parameters (System Firmware Version) Request
------------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_system_firmware_version_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Info Parameters (System Name First Set) Request
----------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_system_name_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set System Info Parameters (System Name) Request
------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_system_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Info Parameters (Primary Operating System Name First Set) Request
----------------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_primary_operating_system_name_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set System Info Parameters (Primary Operating System Name) Request
------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_primary_operating_system_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set System Info Parameters (Operating System Name First Set) Request
--------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_operating_system_name_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set System Info Parameters (Operating System Name) Request
----------------------------------------------------------

fiid_template_t tmpl_cmd_set_system_info_parameters_operating_system_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Info Parameters Request
----------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Info Parameters Response
-----------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get System Info Parameters (Set In Progress) Response
-----------------------------------------------------

Get System Info Parameters (System Firmware Version First Set) Response
-----------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_system_firmware_version_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get System Info Parameters (System Firmware Version) Response
-------------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_system_firmware_version_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Info Parameters (System Name First Set) Response
-----------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_system_name_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get System Info Parameters (System Name) Response
-------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_system_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Info Parameters (Primary Operating System Name First Set) Response
-----------------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_primary_operating_system_name_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get System Info Parameters (Primary Operating System Name) Response
-------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_primary_operating_system_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get System Info Parameters (Operating System Name First Set) Response
---------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_operating_system_name_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get System Info Parameters (Operating System Name) Response
-----------------------------------------------------------

fiid_template_t tmpl_cmd_get_system_info_parameters_operating_system_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Cipher Suites Request
---------------------------------

fiid_template_t tmpl_cmd_get_channel_cipher_suites_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "list_index", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "list_algorithm_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Cipher Suites Response
----------------------------------

fiid_template_t tmpl_cmd_get_channel_cipher_suites_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "cipher_suite_record_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Session Challenge Request
-----------------------------

fiid_template_t tmpl_cmd_get_session_challenge_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Session Challenge Response
------------------------------

fiid_template_t tmpl_cmd_get_session_challenge_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "temp_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Activiate Session Request
-------------------------

fiid_template_t tmpl_cmd_activate_session_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "initial_outbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Activiate Session Response
--------------------------

fiid_template_t tmpl_cmd_activate_session_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "initial_inbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Session Privilege Level Request
-----------------------------------

fiid_template_t tmpl_cmd_set_session_privilege_level_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Session Privilege Level Response
------------------------------------

fiid_template_t tmpl_cmd_set_session_privilege_level_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Close Session Request
---------------------

fiid_template_t tmpl_cmd_close_session_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "session_handle", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Close Session Response
----------------------

fiid_template_t tmpl_cmd_close_session_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };


Set Channel Access Request
--------------------------

fiid_template_t tmpl_cmd_set_channel_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_access_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_privilege_level_limit_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Channel Access Response
---------------------------

fiid_template_t tmpl_cmd_set_channel_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Channel Access Request
--------------------------

fiid_template_t tmpl_cmd_get_channel_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_access_get", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Access Response
---------------------------

fiid_template_t tmpl_cmd_get_channel_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Info Command Request
--------------------------------

fiid_template_t tmpl_cmd_get_channel_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Channel Info Command Response
---------------------------------

fiid_template_t tmpl_cmd_get_channel_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "actual_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "actual_channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "channel_medium_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_medium_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "channel_protocol_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "channel_protocol_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "active_session_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "session_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "vendor_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "auxiliary_channel_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Channel Security Keys Request
---------------------------------

fiid_template_t tmpl_cmd_set_channel_security_keys_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "key_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 160, "key_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set Channel Security Keys Response
----------------------------------

fiid_template_t tmpl_cmd_set_channel_security_keys_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 2, "lock_status", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 160, "key_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set User Access Command Request
-------------------------------

fiid_template_t tmpl_cmd_set_user_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "change_bits_in_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "user_session_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set User Access Command Response
--------------------------------

fiid_template_t tmpl_cmd_set_user_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get User Access Command Request
-------------------------------

fiid_template_t tmpl_cmd_get_user_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get User Access Command Response
--------------------------------

fiid_template_t tmpl_cmd_get_user_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 6, "max_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "current_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_id_enable_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "current_channel_fixed_names", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set User Name Command Request
-----------------------------

fiid_template_t tmpl_cmd_set_user_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set User Name Command Response
------------------------------

fiid_template_t tmpl_cmd_set_user_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get User Name Command Request
-----------------------------

fiid_template_t tmpl_cmd_get_user_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get User Name Command Response
------------------------------

fiid_template_t tmpl_cmd_get_user_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set User Password Command Request
---------------------------------

/* achu: Note that the password is variable length, but it must be
 * fixed to 0, 16, or 20 bytes.  We may try and amend this situation
 * in fiid at a later time.
 */
fiid_template_t tmpl_cmd_set_user_password_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "password_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "operation.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 160, "password", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set User Password Command Response
----------------------------------

fiid_template_t tmpl_cmd_set_user_password_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_MESSAGING_SUPPORT_CMDS_TEMPLATES_H */
