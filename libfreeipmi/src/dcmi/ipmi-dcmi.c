/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.

*/
/*****************************************************************************\
 *  $Id: ipmi-dcmi.c,v 1.4 2009-08-05 21:59:36 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2009 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmi-Dcmi, tools and libraries to support the
 *  data center manageability interface (DCMI).  For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Ipmi-Dcmi is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
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
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/dcmi/ipmi-dcmi.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/util/ipmi-error-util.h"

#include "api/ipmi-api-defs.h"
#include "api/ipmi-api-trace.h"
#include "api/ipmi-api-util.h"
#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_dcmi_rolling_average_time_period =
  {
    { 6, "time_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "time_duration_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  };

fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_rs =
  {
    { 8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,    "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

/* achu: assume typo "Out-Of-B" means "Out-Of-Band" */
fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_supported_dcmi_capabilities_rs =
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
    { 1, "optional_platform_capabilities.power_management", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "optional_platform_capabilities.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "manageability_access_capabilities.in_band_kcs_channel_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "manageability_access_capabilities.out_of_band_serial_tmode_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
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

fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_mandatory_platform_attributes_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 12, "sel_attributes.number_of_sel_entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "sel_attributes.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "sel_attributes.sel_automatic_rollover_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1,  "identification_attributes.guid_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "identification_attributes.dhcp_host_name_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "identification_attributes.asset_tag_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5,  "identification_attributes.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1,  "temperature_monitoring.inlet_temperature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1,  "temperature_monitoring.processors_temperature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* in parameter revision >= 02h, reserved */
    { 1,  "temperature_monitoring.baseboard_temperature", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5,  "temperature_monitoring.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_optional_platform_attributes_rs =
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
fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_manageability_access_attributes_rs =
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
fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_enhanced_system_power_statistics_attributes_rs =
  {
    { 8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,    "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "dcmi_specification_conformance.major_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "dcmi_specification_conformance.minor_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "number_of_supported_rolling_average_time_periods", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2048, "rolling_average_time_periods", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_power_reading_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "mode_attributes", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_power_reading_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "current_power", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum_power_over_sampling_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_power_over_sampling_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "average_power_over_sampling_duration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "time_stamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "statistics_reporting_time_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6,  "power_reading_state.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "power_reading_state.power_measurement", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "power_reading_state.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_power_limit_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_power_limit_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "exception_actions", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "power_limit_requested", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "management_application_statistics_sampling_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_set_power_limit_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "exception_actions", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "power_limit_requested", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "management_application_statistics_sampling_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_set_power_limit_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_activate_deactivate_power_limit_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "power_limit_activation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_activate_deactivate_power_limit_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_asset_tag_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "number_of_bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu: number_of_bytes_to_read is max 16, so presumably data can only be max 16, but
 * asset tag max is 64 bytes.  We'll use 64 bytes (512 bits) as the max then.
 */
fiid_template_t tmpl_dcmi_cmd_get_asset_tag_rs =
  {
    { 8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,   "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,   "total_asset_tag_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_dcmi_cmd_get_dcmi_sensor_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_instance_start", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* presumably a max of 256 entity_instances (b/c 1 byte field), so 
 * assume max record ids of 256.  record_id = 16 bits, so 256 * 16 = 4096
 */
fiid_template_t tmpl_dcmi_cmd_get_dcmi_sensor_info_rs =
  {
    { 8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,    "group_extension_identification", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "total_number_of_available_instances", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "number_of_record_ids_in_this_response", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4096, "sdr_record_ids", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, /* 16 bit fields of record ids, LS byte first */
    { 0, "", 0}
  };

/* 
 * Fill Functions
 */

int
fill_dcmi_cmd_get_dcmi_capability_info (uint8_t parameter_selector,
                                        fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SELECTOR_VALID (parameter_selector)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_dcmi_cmd_get_dcmi_capability_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_DCMI_CMD_GET_DCMI_CAPABILITIY_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  
  return (0);
}

int
fill_dcmi_cmd_get_power_reading (uint8_t mode,
                                 uint8_t mode_attributes,
                                 fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_POWER_READING_MODE_VALID (mode)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_dcmi_cmd_get_power_reading_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_DCMI_CMD_GET_POWER_READING);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "mode", mode);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "mode_attributes", mode_attributes);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_dcmi_cmd_get_power_limit (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_dcmi_cmd_get_power_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_DCMI_CMD_GET_POWER_LIMIT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_dcmi_cmd_set_power_limit (uint8_t exception_actions,
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

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_dcmi_cmd_set_power_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_DCMI_CMD_SET_POWER_LIMIT);
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
fill_dcmi_cmd_activate_deactivate_power_limit (uint8_t power_limit_activation,
                                               fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_DCMI_POWER_LIMIT_ACTIVATION_VALID (power_limit_activation)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_dcmi_cmd_activate_deactivate_power_limit_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_DCMI_CMD_ACTIVATE_DEACTIVATE_POWER_LIMIT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "power_limit_activation", power_limit_activation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_dcmi_cmd_get_asset_tag (uint8_t offset_to_read,
                             uint8_t number_of_bytes_to_read,
                             fiid_obj_t obj_cmd_rq)
{
  if (number_of_bytes_to_read > IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_dcmi_cmd_get_asset_tag_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_DCMI_CMD_GET_ASSET_TAG);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "offset_to_read", offset_to_read);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "number_of_bytes_to_read", number_of_bytes_to_read);

  return (0);
}

int
fill_dcmi_cmd_get_dcmi_sensor_info (uint8_t sensor_type,
                                    uint8_t entity_id,
                                    uint8_t entity_instance,
                                    uint8_t entity_instance_start,
                                    fiid_obj_t obj_cmd_rq)
{
  /* achu: only entity id's listed in the spec, or all possible entity IDs? */
  if (sensor_type != IPMI_SENSOR_TYPE_TEMPERATURE
      || !IPMI_DCMI_ENTITY_ID_VALID(entity_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_dcmi_cmd_get_dcmi_sensor_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_DCMI_CMD_GET_DCMI_SENSOR_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "group_extension_identification", IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_id", entity_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance", entity_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "entity_instance_start", entity_instance_start);

  return (0);
}

/*
 * API functions
 */

int
ipmi_dcmi_cmd_get_dcmi_capability_info (ipmi_ctx_t ctx,
                                        uint8_t parameter_selector,
                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SELECTOR_VALID (parameter_selector)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_dcmi_capability_info_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_dcmi_capability_info (parameter_selector, obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_dcmi_capability_info_supported_dcmi_capabilities (ipmi_ctx_t ctx,
                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_dcmi_capability_info_supported_dcmi_capabilities_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SUPPORTED_DCMI_CAPABILITIES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_dcmi_capability_info_mandatory_platform_attributes (ipmi_ctx_t ctx,
                                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_dcmi_capability_info_mandatory_platform_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_MANDATORY_PLATFORM_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_dcmi_capability_info_optional_platform_attributes (ipmi_ctx_t ctx,
                                                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_dcmi_capability_info_optional_platform_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_OPTIONAL_PLATFORM_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_dcmi_capability_info_manageability_access_attributes (ipmi_ctx_t ctx,
                                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_dcmi_capability_info_manageability_access_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_MANAGEABILITY_ACCESS_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_dcmi_capability_info_enhanced_system_power_statistics_attributes (ipmi_ctx_t ctx,
                                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_dcmi_capability_info_enhanced_system_power_statistics_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_ENHANCED_SYSTEM_POWER_STATISTICS_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_power_reading (ipmi_ctx_t ctx,
                                 uint8_t mode,
                                 uint8_t mode_attributes,
                                 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_POWER_READING_MODE_VALID (mode)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_power_reading_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_power_reading_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_power_reading (mode,
                                       mode_attributes,
                                       obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_power_limit (ipmi_ctx_t ctx,
                               fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_power_limit_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_power_limit_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_power_limit (obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_set_power_limit (ipmi_ctx_t ctx,
                               uint8_t exception_actions,
                               uint16_t power_limit_requested,
                               uint32_t correction_time_limit,
                               uint16_t management_application_statistics_sampling_period,
                               fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_EXCEPTION_ACTION_VALID (exception_actions)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_set_power_limit_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_set_power_limit_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_set_power_limit (exception_actions,
                                     power_limit_requested,
                                     correction_time_limit,
                                     management_application_statistics_sampling_period,
                                     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_activate_deactivate_power_limit (ipmi_ctx_t ctx,
                                               uint8_t power_limit_activation,
                                               fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_POWER_LIMIT_ACTIVATION_VALID (power_limit_activation)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_activate_deactivate_power_limit_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_activate_deactivate_power_limit_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_activate_deactivate_power_limit (power_limit_activation,
                                                     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_asset_tag (ipmi_ctx_t ctx,
                             uint8_t offset_to_read,
                             uint8_t number_of_bytes_to_read,
                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (number_of_bytes_to_read > IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_asset_tag_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_asset_tag_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_asset_tag (offset_to_read,
                                   number_of_bytes_to_read,
                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_dcmi_cmd_get_dcmi_sensor_info (ipmi_ctx_t ctx,
                                    uint8_t sensor_type,
                                    uint8_t entity_id,
                                    uint8_t entity_instance,
                                    uint8_t entity_instance_start,
                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* achu: only entity id's listed in the spec, or all possible entity IDs? */
  if (sensor_type != IPMI_SENSOR_TYPE_TEMPERATURE
      || !IPMI_DCMI_ENTITY_ID_VALID(entity_id)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_dcmi_cmd_get_dcmi_sensor_info_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_dcmi_cmd_get_dcmi_sensor_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_dcmi_cmd_get_dcmi_sensor_info (sensor_type,
                                          entity_id,
                                          entity_instance,
                                          entity_instance_start,
                                          obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}


/*
 * util functions
 */

int
ipmi_dcmi_completion_code_strerror_r (uint8_t cmd,
                                      uint8_t netfn,
                                      uint8_t comp_code,
                                      char *errstr,
                                      size_t len)
{
  if (!errstr)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (netfn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || netfn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      switch (cmd)
        {
        case IPMI_DCMI_CMD_GET_POWER_LIMIT:
          switch (comp_code)
            {
            case IPMI_DCMI_COMP_CODE_NO_SET_POWER_LIMIT:
              snprintf (errstr, len, IPMI_DCMI_COMP_CODE_NO_SET_POWER_LIMIT_STR);
              return (0);
            }
          break;
        case IPMI_DCMI_CMD_SET_POWER_LIMIT:
          switch (comp_code)
            {
            case IPMI_DCMI_COMP_CODE_POWER_LIMIT_OUT_OF_RANGE:
              snprintf (errstr, len, IPMI_DCMI_COMP_CODE_POWER_LIMIT_OUT_OF_RANGE_STR);
              return (0);
            case IPMI_DCMI_COMP_CODE_CORRECTION_TIME_OUT_OF_RANGE:
              snprintf (errstr, len, IPMI_DCMI_COMP_CODE_CORRECTION_TIME_OUT_OF_RANGE_STR);
              return (0);
            case IPMI_DCMI_COMP_CODE_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE:
              snprintf (errstr, len, IPMI_DCMI_COMP_CODE_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE_STR);
              return (0);
            }
          break;
        }
    }

  return ipmi_completion_code_strerror_r (cmd,
                                          netfn,
                                          comp_code,
                                          errstr,
                                          len);
}

int
ipmi_dcmi_completion_code_strerror_cmd_r (fiid_obj_t obj_cmd,
                                          uint8_t netfn,
                                          char *errstr,
                                          size_t len)
{
  uint8_t cmd, comp_code;
  uint64_t val;

  /* The netfn need not be valid */
  if (!fiid_obj_valid (obj_cmd)
      || !errstr)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_FIELD_LOOKUP (obj_cmd, "cmd") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (FIID_OBJ_FIELD_LOOKUP (obj_cmd, "comp_code") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd, "cmd", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  cmd = val;

  if (FIID_OBJ_GET (obj_cmd, "comp_code", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  comp_code = val;

  return (ipmi_dcmi_completion_code_strerror_r (cmd, netfn, comp_code, errstr, len));
}

const char *
ipmi_dcmi_cmd_str (uint8_t cmd)
{
  switch (cmd)
    {
    case IPMI_DCMI_CMD_GET_DCMI_CAPABILITIY_INFO:
      return "Get DCMI Capability Info";
    case IPMI_DCMI_CMD_GET_POWER_READING:
      return "Get Power Reading";
    case IPMI_DCMI_CMD_GET_POWER_LIMIT:
      return "Get Power Limit";
    case IPMI_DCMI_CMD_SET_POWER_LIMIT:
      return "Set Power LIMIT";
    case IPMI_DCMI_CMD_ACTIVATE_DEACTIVATE_POWER_LIMIT:
      return "Activate/Deactivate Power Limit";
    case IPMI_DCMI_CMD_GET_ASSET_TAG:
      return "Get Asset Tag";
    case IPMI_DCMI_CMD_GET_DCMI_SENSOR_INFO:
      return "Get DCMI Sensor Info";
    default:
      return "Unknown";
    }
}
