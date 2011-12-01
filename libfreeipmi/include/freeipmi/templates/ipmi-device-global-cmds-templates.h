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

#ifndef _IPMI_DEVICE_GLOBAL_CMDS_TEMPLATES_H
#define _IPMI_DEVICE_GLOBAL_CMDS_TEMPLATES_H

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

Get Device ID Request
---------------------

fiid_template_t tmpl_cmd_get_device_id_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Device ID Response
----------------------

fiid_template_t tmpl_cmd_get_device_id_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "device_revision.revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* binary encoded */
    { 3, "device_revision.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "device_revision.sdr_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "firmware_revision1.major_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "firmware_revision1.device_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "firmware_revision2.minor_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* BCD encoded */
    { 4, "ipmi_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "ipmi_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.sdr_repository_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.fru_inventory_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.ipmb_event_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.ipmb_event_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.chassis_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 20, "manufacturer_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "manufacturer_id.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "product_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "auxiliary_firmware_revision_information", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Cold Reset Request
------------------

fiid_template_t tmpl_cmd_cold_reset_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Cold Reset Response
-------------------

fiid_template_t tmpl_cmd_cold_reset_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Warm Reset Request
------------------

fiid_template_t tmpl_cmd_warm_reset_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Warm Reset Response
-------------------

fiid_template_t tmpl_cmd_warm_reset_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Set ACPI Power State Request
----------------------------

fiid_template_t tmpl_cmd_set_acpi_power_state_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "system_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_system_power_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "device_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_device_power_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set ACPI Power State Response
-----------------------------

fiid_template_t tmpl_cmd_set_acpi_power_state_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get ACPI Power State Request
----------------------------

fiid_template_t tmpl_cmd_get_acpi_power_state_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get ACPI Power State Response
-----------------------------

fiid_template_t tmpl_cmd_get_acpi_power_state_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 7, "system_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "device_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Self Test Results Request
-----------------------------

fiid_template_t tmpl_cmd_get_self_test_results_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Self Test Results Response
------------------------------

/* note: bitfield results only if self_test_results == 0x57.  See
 * specification for more information
 */
fiid_template_t tmpl_cmd_get_self_test_results_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "self_test_result", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "controller_operation_firmware_corrupted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "controller_update_boot_block_firmware_corrupted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "internal_use_area_of_bmc_fru_corrupted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sdr_repository_empty", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "ipmb_signal_lines_do_not_respond", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cannot_access_bmc_fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cannot_access_sdr_repository", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cannot_access_sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Device GUID Request
-----------------------

fiid_template_t tmpl_cmd_get_device_guid_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Device GUID Response
------------------------

fiid_template_t tmpl_cmd_get_device_guid_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Device GUID (with Format) Response
--------------------------------------

fiid_template_t tmpl_cmd_get_device_guid_format_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 48, "node", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "clock_seq_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "clock_seq_hi_and_reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "time_high_and_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "time_mid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "time_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_DEVICE_GLOBAL_CMDS_TEMPLATES_H */
