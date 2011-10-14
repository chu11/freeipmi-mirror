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

#ifndef _IPMI_EVENT_CMDS_TEMPLATES_H
#define _IPMI_EVENT_CMDS_TEMPLATES_H

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

Set Event Receiver Request
--------------------------

/* event_receiver_slave_address is presumably always a slave address, not a
 * software id, so no "id type" field.
 */
fiid_template_t tmpl_cmd_set_event_receiver_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_receiver_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_receiver_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Event Receiver Response
---------------------------

fiid_template_t tmpl_cmd_set_event_receiver_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Event Receiver Request
--------------------------

/* event_receiver_slave_address is presumably always a slave address, not a
 * software id, so no "id type" field.
 */
fiid_template_t tmpl_cmd_get_event_receiver_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Event Receiver Response
---------------------------

fiid_template_t tmpl_cmd_get_event_receiver_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "event_receiver_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_receiver_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Platform Event Request
----------------------

fiid_template_t tmpl_cmd_platform_event_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "generator_id", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* EvMRev in spec */
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Platform Event Response
-----------------------

fiid_template_t tmpl_cmd_platform_event_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_EVENT_CMDS_TEMPLATES_H */
