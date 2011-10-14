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

#ifndef _IPMI_SERIAL_MODEM_CMDS_TEMPLATES_H
#define _IPMI_SERIAL_MODEM_CMDS_TEMPLATES_H

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

Set Serial Modem Configuration Request
--------------------------------------

fiid_template_t tmpl_cmd_set_serial_modem_configuration_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set Serial Modem Configuration Response
---------------------------------------

fiid_template_t tmpl_cmd_set_serial_modem_configuration_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set Serial Modem Configuration (Set In Progress) Request
--------------------------------------------------------

fiid_template_t tmpl_cmd_set_serial_modem_configuration_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Serial Modem Configuration (Connection Mode) Request
--------------------------------------------------------

fiid_template_t tmpl_cmd_set_serial_modem_configuration_connection_mode_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "basic_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "ppp_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "terminal_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "connect_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Serial Modem Configuration (IPMI Messaging Comm Settings) Request
---------------------------------------------------------------------

fiid_template_t tmpl_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dtr_hangup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "flow_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Serial Modem Configuration (Page Blackout Interval) Request
---------------------------------------------------------------

fiid_template_t tmpl_cmd_set_serial_modem_configuration_page_blackout_interval_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "page_blackout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Serial Modem Configuration (Call Retry Interval) Request
------------------------------------------------------------

fiid_template_t tmpl_cmd_set_serial_modem_configuration_call_retry_interval_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "call_retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Serial Modem Configuration Request
--------------------------------------

fiid_template_t tmpl_cmd_get_serial_modem_configuration_rq =
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

Get Serial Modem Configuration Response
---------------------------------------

fiid_template_t tmpl_cmd_get_serial_modem_configuration_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Serial Modem Configuration (Set In Progress) Response
---------------------------------------------------------

fiid_template_t tmpl_cmd_get_serial_modem_configuration_set_in_progress_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Serial Modem Configuration (Connection Mode) Response
---------------------------------------------------------

fiid_template_t tmpl_cmd_get_serial_modem_configuration_connection_mode_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "basic_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "ppp_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "terminal_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "connect_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Serial Modem Configuration (IPMI Messaging Comm Settings) Response
----------------------------------------------------------------------

fiid_template_t tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dtr_hangup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "flow_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Serial Modem Configuration (Page Blackout Interval) Response
----------------------------------------------------------------

fiid_template_t tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "page_blackout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Serial Modem Configuration (Call Retry Interval) Response
-------------------------------------------------------------

fiid_template_t tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "call_retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_SERIAL_MODEM_CMDS_TEMPLATES_H */
