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

#ifndef _IPMI_SEL_CMDS_TEMPLATES_H
#define _IPMI_SEL_CMDS_TEMPLATES_H

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

Get SEL Info Request
--------------------

fiid_template_t tmpl_cmd_get_sel_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SEL Info Response
---------------------

fiid_template_t tmpl_cmd_get_sel_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "sel_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "sel_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "free_space", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "most_recent_addition_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "most_recent_erase_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_sel_allocation_info_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserve_sel_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "partial_add_sel_entry_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "delete_sel_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "overflow_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SEL Allocation Info Request
-------------------------------

fiid_template_t tmpl_cmd_get_sel_allocation_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SEL Allocation Info Response
--------------------------------

fiid_template_t tmpl_cmd_get_sel_allocation_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "number_of_possible_allocation_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "allocation_unit_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "number_of_free_allocation_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "largest_free_block", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "maximum_record_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Reserve SEL Request
-------------------

fiid_template_t tmpl_cmd_reserve_sel_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Reserve SEL Response
--------------------

fiid_template_t tmpl_cmd_reserve_sel_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SEL Entry Request
---------------------

fiid_template_t tmpl_cmd_get_sel_entry_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_into_record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SEL Entry Response
----------------------

fiid_template_t tmpl_cmd_get_sel_entry_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "next_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "record_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Delete SEL Entry Request
------------------------

fiid_template_t tmpl_cmd_delete_sel_entry_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Delete SEL Entry Response
-------------------------

fiid_template_t tmpl_cmd_delete_sel_entry_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Clear SEL Request
-----------------

fiid_template_t tmpl_cmd_clear_sel_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "C", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "L", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "R", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Clear SEL Response
------------------

fiid_template_t tmpl_cmd_clear_sel_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "erasure_progress", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SEL Time Request
--------------------

fiid_template_t tmpl_cmd_get_sel_time_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get SEL Time Response
---------------------

fiid_template_t tmpl_cmd_get_sel_time_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SEL Time Request
--------------------

fiid_template_t tmpl_cmd_set_sel_time_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set SEL Time Response
---------------------

fiid_template_t tmpl_cmd_set_sel_time_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Auxiliary Log Status Request
--------------------------------

fiid_template_t tmpl_cmd_get_auxiliary_log_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "log_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Auxiliary Log Status Response
---------------------------------

fiid_template_t tmpl_cmd_get_auxiliary_log_status_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 120, "log_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Auxiliary Log Status (MCA) Response
---------------------------------------

fiid_template_t tmpl_cmd_get_auxiliary_log_status_mca_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "mca_log_entry_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 56, "log_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Auxiliary Log Status (OEM) Response
---------------------------------------

fiid_template_t tmpl_cmd_get_auxiliary_log_status_oem_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 64, "log_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set Auxiliary Log Status Request
--------------------------------

fiid_template_t tmpl_cmd_set_auxiliary_log_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "log_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 120, "log_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Set Auxiliary Log Status Response
---------------------------------

fiid_template_t tmpl_cmd_set_auxiliary_log_status_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_SEL_CMDS_TEMPLATES_H */
