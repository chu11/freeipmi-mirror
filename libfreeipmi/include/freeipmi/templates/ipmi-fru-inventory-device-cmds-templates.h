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

#ifndef _IPMI_FRU_INVENTORY_DEVICE_CMDS_TEMPLATES_H
#define _IPMI_FRU_INVENTORY_DEVICE_CMDS_TEMPLATES_H

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

Get FRU Inventory Area Info Request
-----------------------------------

fiid_template_t tmpl_cmd_get_fru_inventory_area_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get FRU Inventory Area Info Response
------------------------------------

fiid_template_t tmpl_cmd_get_fru_inventory_area_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "fru_inventory_area_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "device_is_accessed", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Read FRU Data Request
---------------------

fiid_template_t tmpl_cmd_read_fru_data_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "fru_inventory_offset_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "count_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Read FRU Data Response
----------------------

/* 2040 = 255 * 8, 255 b/c count_returned field in request is 1 byte long */
fiid_template_t tmpl_cmd_read_fru_data_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "count_returned", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2040, "requested_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Write FRU Data Request
----------------------

/* 2040 = 255 * 8, 255 b/c bytes_to_write field in request is 1 byte long */
fiid_template_t tmpl_cmd_write_fru_data_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "fru_inventory_offset_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2040, "data_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Write FRU Data Response
-----------------------

fiid_template_t tmpl_cmd_write_fru_data_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "count_written", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_FRU_INVENTORY_DEVICE_CMDS_TEMPLATES_H */
