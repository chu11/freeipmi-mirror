/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
/*****************************************************************************\
 *  $Id: ipmi-dcmi-cmds.c,v 1.7 2010-07-28 21:19:56 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2009-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-413270
 *
 *  This file is part of Ipmi-Dcmi, tools and libraries to support the
 *  data center manageability interface (DCMI).  For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Ipmi-Dcmi is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-Dcmi is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-Dcmi.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-dcmi-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-cmd-dcmi-spec.h"
#include "freeipmi/spec/ipmi-entity-ids-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_dcmi_rolling_average_time_period =
  {
    { 6, "time_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "time_duration_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "mandatory_platform_capabilities.identification_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "mandatory_platform_capabilities.sel_logging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "mandatory_platform_capabilities.chassis_power", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "mandatory_platform_capabilities.temperature_monitor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "mandatory_platform_capabilities.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "optional_platform_capabilities.power_management_monitoring_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "optional_platform_capabilities.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "manageability_access_capabilities.in_band_system_interface_channel_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "manageability_access_capabilities.serial_tmode_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "manageability_access_capabilities.out_of_band_secondary_lan_channel_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "manageability_access_capabilities.out_of_band_primary_lan_channel_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "manageability_access_capabilities.sol_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "manageability_access_capabilities.vlan_capable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "manageability_access_capabilities.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_mandatory_platform_attributes_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 12, "sel_attributes.number_of_sel_entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sel_attributes.record_level_sel_flush_upon_rollover", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sel_attributes.entire_sel_flush_upon_rollover", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sel_attributes.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sel_attributes.sel_automatic_rollover_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "identification_attributes.guid_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "identification_attributes.dhcp_host_name_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "identification_attributes.asset_tag_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "identification_attributes.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "temperature_monitoring.inlet_temperature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "temperature_monitoring.processors_temperature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1, "temperature_monitoring.baseboard_temperature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "temperature_monitoring.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h */
    { 8, "temperature_monitoring.sampling_period", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_optional_platform_attributes_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_management_device_slave_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "power_management_device_slave_address.slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "power_management_controller_channel_number.device_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "power_management_controller_channel_number.channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu: for consistency, renamed "oob" to "out_of_band" */
fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_manageability_access_attributes_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "mandatory_primary_lan_out_of_band_support_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "optional_secondary_lan_out_of_band_support_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "optional_serial_out_of_band_tmode_capability_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* 256 * 8 = 2048 */
fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_enhanced_system_power_statistics_attributes_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_supported_rolling_average_time_periods", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2048, "rolling_average_time_periods", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_dcmi_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_dcmi_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_dcmi_configuration_parameters_activate_dhcp_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "activate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_dcmi_configuration_parameters_discovery_configuration_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "option_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "option_60_with_option_43", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "random_back_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_1_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "initial_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_2_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "server_contact_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_3_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "server_contact_retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},   
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_configuration_parameters_discovery_configuration_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "option_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "option_60_with_option_43", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "random_back_off", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_configuration_parameters_dhcp_timing_1_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "initial_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_configuration_parameters_dhcp_timing_2_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "server_contact_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_configuration_parameters_dhcp_timing_3_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "server_contact_retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_asset_tag_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu: number_of_bytes_to_read is max 16, so presumably data can
 * only be max 16, but asset tag max is 64 bytes.  We'll use 64 bytes
 * (512 bits) as the max then.
 */
fiid_template_t tmpl_cmd_dcmi_get_asset_tag_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "total_asset_tag_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

/* achu: number_of_bytes_to_read is max 16, so presumably data can
 * only be max 16, but asset tag max is 64 bytes.  We'll use 64 bytes
 * (512 bits) as the max then.
 */
fiid_template_t tmpl_cmd_dcmi_set_asset_tag_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_bytes_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_asset_tag_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "total_asset_tag_length_written", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_management_controller_identifier_string_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu: number_of_bytes_to_read is max 16, so presumably data can
 * only be max 16, but identifier max is 64 bytes.  We'll use 64 bytes
 * (512 bits) as the max then.
 */
fiid_template_t tmpl_cmd_dcmi_get_management_controller_identifier_string_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "total_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

/* achu: number_of_bytes_to_read is max 16, so presumably data can
 * only be max 16, but identifier max is 64 bytes.  We'll use 64 bytes
 * (512 bits) as the max then.
 */
fiid_template_t tmpl_cmd_dcmi_set_management_controller_identifier_string_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_bytes_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_management_controller_identifier_string_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "total_length_written", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_dcmi_sensor_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance_start", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* presumably a max of 256 entity_instances (b/c 1 byte field), so 
 * assume max record ids of 256.  record_id = 16 bits, so 256 * 16 = 4096
 */
fiid_template_t tmpl_cmd_dcmi_get_dcmi_sensor_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "total_number_of_available_instances", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_record_ids_in_this_response", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4096, "sdr_record_ids", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, /* 16 bit fields of record ids, LS byte first */
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_power_reading_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "mode_attributes", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_power_reading_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "current_power", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum_power_over_sampling_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_power_over_sampling_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "average_power_over_sampling_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "time_stamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "statistics_reporting_time_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "power_reading_state.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_reading_state.power_measurement", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_reading_state.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_power_limit_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_power_limit_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "exception_actions", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "power_limit_requested", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "management_application_statistics_sampling_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_power_limit_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "exception_actions", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "power_limit_requested", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "management_application_statistics_sampling_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_power_limit_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_activate_deactivate_power_limit_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "power_limit_activation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_activate_deactivate_power_limit_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_thermal_limit_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_thermal_limit_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "temperature_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "exception_actions.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "exception_actions.log_event_to_sel_only", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "exception_actions.hard_power_off_system_and_log_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "exception_actions.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "exception_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_thermal_limit_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "temperature_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "exception_actions.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "exception_actions.log_event_to_sel_only", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "exception_actions.hard_power_off_system_and_log_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "exception_actions.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "exception_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_set_thermal_limit_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_temperature_reading_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance_start", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_dcmi_get_temperature_reading_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "total_number_of_available_instances", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_sets_of_temperature_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "temperature1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 8, "temperature2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 8, "temperature3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 8, "temperature4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 8, "temperature5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 8, "temperature6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 8, "temperature7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 8, "temperature8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, /* signed 8 bit int */
    { 0, "", 0}
  };

/* 
 * Fill Functions
 */

int
fill_cmd_dcmi_get_dcmi_capability_info (uint8_t parameter_selector,
                                        fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SELECTOR_VALID (parameter_selector)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_dcmi_capability_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_DCMI_CAPABILITY_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  
  return (0);
}

int
fill_cmd_dcmi_set_dcmi_configuration_parameters (uint8_t parameter_selector,
						 uint8_t set_selector,
						 const void *configuration_parameter_data,
						 unsigned int configuration_parameter_data_len,
						 fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_CONFIGURATION_PARAMETER_SELECTOR_VALID (parameter_selector)
      || !configuration_parameter_data
      || !configuration_parameter_data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_dcmi_configuration_parameters_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_DCMI_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "configuration_parameter_data",
                          configuration_parameter_data,
                          configuration_parameter_data_len);

  return (0);
}

int
fill_cmd_dcmi_set_dcmi_configuration_parameters_activate_dhcp (uint8_t set_selector,
							       uint8_t activate,
							       fiid_obj_t obj_cmd_rq)
{
  /* technically, user can input anything for activate, but only 0x01 will do anything */
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_dcmi_configuration_parameters_activate_dhcp_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_DCMI_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_DCMI_CONFIGURATION_PARAMETER_ACTIVATE_DHCP);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "activate", activate);

  return (0);
}

int
fill_cmd_dcmi_set_dcmi_configuration_parameters_discovery_configuration (uint8_t set_selector,
									 uint8_t option_12,
									 uint8_t option_60_with_option_43,
									 uint8_t random_back_off,
									 fiid_obj_t obj_cmd_rq)
{
  if (IPMI_DCMI_DHCP_INCLUDE_OPTION_VALID (option_12)
      || IPMI_DCMI_DHCP_INCLUDE_OPTION_VALID (option_60_with_option_43)
      || !IPMI_DCMI_DHCP_RANDOM_BACK_OFF_VALID (random_back_off)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_dcmi_configuration_parameters_discovery_configuration_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_DCMI_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_DCMI_CONFIGURATION_PARAMETER_DISCOVERY_CONFIGURATION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "option_12", option_12);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "option_60_with_option_43", option_60_with_option_43);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "random_back_off", random_back_off);

  return (0);
}

int
fill_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_1 (uint8_t set_selector,
							       uint8_t initial_timeout_interval,
							       fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_1_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_DCMI_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_DCMI_CONFIGURATION_PARAMETER_DHCP_TIMING_1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "initial_timeout_interval", initial_timeout_interval);

  return (0);
}

int
fill_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_2 (uint8_t set_selector,
							       uint16_t server_contact_timeout_interval,
							       fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_2_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_DCMI_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_DCMI_CONFIGURATION_PARAMETER_DHCP_TIMING_2);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "server_contact_timeout_interval", server_contact_timeout_interval);

  return (0);
}

int
fill_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_3 (uint8_t set_selector,
							       uint16_t server_contact_retry_interval,
							       fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_dcmi_configuration_parameters_dhcp_timing_3_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_DCMI_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_DCMI_CONFIGURATION_PARAMETER_DHCP_TIMING_3);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "server_contact_retry_interval", server_contact_retry_interval);

  return (0);
}

int
fill_cmd_dcmi_get_dcmi_configuration_parameters (uint8_t parameter_selector,
						 uint8_t set_selector,
						 fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_CONFIGURATION_PARAMETER_SELECTOR_VALID (parameter_selector)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_dcmi_configuration_parameters_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_DCMI_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  
  return (0);
}

int
fill_cmd_dcmi_get_asset_tag (uint8_t offset_to_read,
                             uint8_t number_of_bytes_to_read,
                             fiid_obj_t obj_cmd_rq)
{
  if (number_of_bytes_to_read > IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_asset_tag_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_ASSET_TAG);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "offset_to_read", offset_to_read);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "number_of_bytes_to_read", number_of_bytes_to_read);

  return (0);
}

int
fill_cmd_dcmi_set_asset_tag (uint8_t offset_to_write,
                             uint8_t number_of_bytes_to_write,
                             const void *data,
                             unsigned int data_len,
                             fiid_obj_t obj_cmd_rq)
{
  if (number_of_bytes_to_write > IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX
      || !data
      || !data_len
      || number_of_bytes_to_write > data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_asset_tag_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_ASSET_TAG);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "offset_to_write", offset_to_write);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "number_of_bytes_to_write", number_of_bytes_to_write);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "data",
                          data,
                          number_of_bytes_to_write);

  return (0);
}

int
fill_cmd_dcmi_get_management_controller_identifier_string (uint8_t offset_to_read,
                                                           uint8_t number_of_bytes_to_read,
                                                           fiid_obj_t obj_cmd_rq)
{
  if (number_of_bytes_to_read > IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_management_controller_identifier_string_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "offset_to_read", offset_to_read);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "number_of_bytes_to_read", number_of_bytes_to_read);

  return (0);
}

int
fill_cmd_dcmi_set_management_controller_identifier_string (uint8_t offset_to_write,
                                                           uint8_t number_of_bytes_to_write,
                                                           const void *data,
                                                           unsigned int data_len,
                                                           fiid_obj_t obj_cmd_rq)
{
  if (number_of_bytes_to_write > IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX
      || !data
      || !data_len
      || number_of_bytes_to_write > data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_management_controller_identifier_string_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  
  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "offset_to_write", offset_to_write);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "number_of_bytes_to_write", number_of_bytes_to_write);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "data",
                          data,
                          number_of_bytes_to_write);
  
  return (0);
}

int
fill_cmd_dcmi_get_dcmi_sensor_info (uint8_t sensor_type,
                                    uint8_t entity_id,
                                    uint8_t entity_instance,
                                    uint8_t entity_instance_start,
                                    fiid_obj_t obj_cmd_rq)
{
  if (sensor_type != IPMI_SENSOR_TYPE_TEMPERATURE
      || (!IPMI_DCMI_ENTITY_ID_VALID(entity_id)
	  && entity_id != IPMI_ENTITY_ID_PROCESSOR
	  && entity_id != IPMI_ENTITY_ID_SYSTEM_BOARD
	  && entity_id != IPMI_ENTITY_ID_AIR_INLET_B)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_dcmi_sensor_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_DCMI_SENSOR_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_type", sensor_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_id", entity_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance", entity_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance_start", entity_instance_start);

  return (0);
}

int
fill_cmd_dcmi_get_power_reading (uint8_t mode,
                                 uint8_t mode_attributes,
                                 fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_POWER_READING_MODE_VALID (mode)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_power_reading_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_POWER_READING);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "mode", mode);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "mode_attributes", mode_attributes);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_dcmi_get_power_limit (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_power_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_POWER_LIMIT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_dcmi_set_power_limit (uint8_t exception_actions,
                               uint16_t power_limit_requested,
                               uint32_t correction_time_limit,
                               uint16_t management_application_statistics_sampling_period,
                               fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_EXCEPTION_ACTION_VALID (exception_actions)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_power_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_POWER_LIMIT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "exception_actions", exception_actions);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "power_limit_requested", power_limit_requested);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "correction_time_limit", correction_time_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "management_application_statistics_sampling_period", management_application_statistics_sampling_period);

  return (0);
}

int
fill_cmd_dcmi_activate_deactivate_power_limit (uint8_t power_limit_activation,
                                               fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_POWER_LIMIT_ACTIVATION_VALID (power_limit_activation)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_activate_deactivate_power_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_ACTIVATE_DEACTIVATE_POWER_LIMIT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "power_limit_activation", power_limit_activation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_dcmi_get_thermal_limit (uint8_t entity_id,
				 uint8_t entity_instance,
				 fiid_obj_t obj_cmd_rq)
{
  if ((entity_id != IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE
       && entity_id != IPMI_ENTITY_ID_AIR_INLET_B)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_thermal_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_THERMAL_LIMIT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_id", entity_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance", entity_instance);

  return (0);
}

int
fill_cmd_dcmi_set_thermal_limit (uint8_t entity_id,
				 uint8_t entity_instance,
				 uint8_t temperature_limit,
				 uint8_t exception_actions_log_event_to_sel_only,
				 uint8_t exception_actions_hard_power_off_system_and_log_event,
				 uint16_t exception_time,
				 fiid_obj_t obj_cmd_rq)
{
  if ((entity_id != IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE
       && entity_id != IPMI_ENTITY_ID_AIR_INLET_B)
      || !IPMI_DCMI_EXCEPTION_ACTION_BIT_VALID (exception_actions_log_event_to_sel_only)
      || !IPMI_DCMI_EXCEPTION_ACTION_BIT_VALID (exception_actions_hard_power_off_system_and_log_event)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_set_thermal_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_SET_THERMAL_LIMIT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_id", entity_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance", entity_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "temperature_limit", temperature_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "exception_actions.reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "exception_actions.log_event_to_sel_only", exception_actions_log_event_to_sel_only);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "exception_actions.hard_power_off_system_and_log_event", exception_actions_hard_power_off_system_and_log_event);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "exception_actions.reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "exception_time", exception_time);

  return (0);
}

int
fill_cmd_dcmi_get_temperature_reading (uint8_t sensor_type,
				       uint8_t entity_id,
				       uint8_t entity_instance,
				       uint8_t entity_instance_start,
				       fiid_obj_t obj_cmd_rq)
{
  if (sensor_type != IPMI_SENSOR_TYPE_TEMPERATURE
      || (!IPMI_DCMI_ENTITY_ID_VALID(entity_id)
	  && entity_id != IPMI_ENTITY_ID_PROCESSOR
	  && entity_id != IPMI_ENTITY_ID_SYSTEM_BOARD
	  && entity_id != IPMI_ENTITY_ID_AIR_INLET_B)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_dcmi_get_temperature_reading_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DCMI_GET_TEMPERATURE_READING);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_type", sensor_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_id", entity_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance", entity_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance_start", entity_instance_start);

  return (0);
}

