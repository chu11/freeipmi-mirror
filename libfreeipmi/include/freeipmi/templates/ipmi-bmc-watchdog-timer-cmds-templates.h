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

#ifndef _IPMI_BMC_WATCHDOG_TIMER_CMDS_TEMPLATES_H
#define _IPMI_BMC_WATCHDOG_TIMER_CMDS_TEMPLATES_H

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

Reset Watchdog Timer Request
----------------------------

fiid_template_t tmpl_cmd_reset_watchdog_timer_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Reset Watchdog Timer Response
-----------------------------

fiid_template_t tmpl_cmd_reset_watchdog_timer_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set Watchdog Timer Request
--------------------------

fiid_template_t tmpl_cmd_set_watchdog_timer_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "timer_use", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "stop_timer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "log", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "timeout_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "pre_timeout_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pre_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.bios_frb2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.bios_post", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.os_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.sms_os", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "initial_countdown_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Watchdog Timer Response
---------------------------

fiid_template_t tmpl_cmd_set_watchdog_timer_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Watchdog Timer Request
--------------------------

fiid_template_t tmpl_cmd_get_watchdog_timer_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Watchdog Timer Response
---------------------------

fiid_template_t tmpl_cmd_get_watchdog_timer_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 3, "timer_use", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "log", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "timeout_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "pre_timeout_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pre_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.bios_frb2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.bios_post", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.os_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.sms_os", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "timer_use_expiration_flag.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "initial_countdown_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "present_countdown_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_BMC_WATCHDOG_TIMER_CMDS_TEMPLATES_H */
