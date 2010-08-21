/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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
 *  Copyright (C) 2009-2010 Lawrence Livermore National Security, LLC.
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

#ifndef _IPMI_DCMI_CMDS_H
#define _IPMI_DCMI_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SUPPORTED_DCMI_CAPABILITIES                 0x01
#define IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_MANDATORY_PLATFORM_ATTRIBUTES               0x02
#define IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_OPTIONAL_PLATFORM_ATTRIBUTES                0x03
#define IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_MANAGEABILITY_ACCESS_ATTRIBUTES             0x04
#define IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_ENHANCED_SYSTEM_POWER_STATISTICS_ATTRIBUTES 0x05

#define IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SELECTOR_VALID(__parameter_selector) \
  ((((__parameter_selector)) >= (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SUPPORTED_DCMI_CAPABILITIES) && \
    ((__parameter_selector)) <= (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_ENHANCED_SYSTEM_POWER_STATISTICS_ATTRIBUTES)) ? 1 : 0)

#define IPMI_DCMI_COMPLIANT_WITH_DCMI_SPECIFICATION       0x1
#define IPMI_DCMI_NOT_COMPLIANT_WITH_DCMI_SPECIFICATION   0x0

#define IPMI_DCMI_AVAILABLE          0x1
#define IPMI_DCMI_AT_LEAST_1_PRESENT 0x1
#define IPMI_DCMI_NOT_PRESENT        0x0

#define IPMI_DCMI_CHANNEL_NOT_SUPPORTED 0xFF

#define IPMI_DCMI_TIME_DURATION_UNITS_SECONDS 0x00
#define IPMI_DCMI_TIME_DURATION_UNITS_MINUTES 0x01
#define IPMI_DCMI_TIME_DURATION_UNITS_HOURS   0x02
#define IPMI_DCMI_TIME_DURATION_UNITS_DAYS    0x03

#define IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX  16
#define IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX 16

#define IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX  16
#define IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX 16

#define IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE     0x40
#define IPMI_DCMI_ENTITY_ID_CPU_TEMPERATURE       0x41
#define IPMI_DCMI_ENTITY_ID_BASEBOARD_TEMPERATURE 0x42

#define IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE_STR     "Inlet Temperature"
#define IPMI_DCMI_ENTITY_ID_CPU_TEMPERATURE_STR       "CPU Temperature"
#define IPMI_DCMI_ENTITY_ID_BASEBOARD_TEMPERATURE_STR "Baseboard temperature"

#define IPMI_DCMI_ENTITY_ID_VALID(__entity_id) \
  (((__entity_id) == IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE \
    || (__entity_id) == IPMI_DCMI_ENTITY_ID_CPU_TEMPERATURE \
    || (__entity_id) == IPMI_DCMI_ENTITY_ID_BASEBOARD_TEMPERATURE) ? 1 : 0)

#define IPMI_DCMI_ENTITY_INSTANCE_ALL 0x00

#define IPMI_DCMI_POWER_READING_MODE_SYSTEM_POWER_STATISTICS          0x01
#define IPMI_DCMI_POWER_READING_MODE_POWER_STATISTICS                 IPMI_DCMI_POWER_READING_MODE_SYSTEM_POWER_STATISTICS
#define IPMI_DCMI_POWER_READING_MODE_ENHANCED_SYSTEM_POWER_STATISTICS 0x02

/* spec only lists one mode right now, assume there can/will be more in the future */
#define IPMI_DCMI_POWER_READING_MODE_VALID(__mode) \
  (((__mode) == IPMI_DCMI_POWER_READING_MODE_POWER_STATISTICS \
    || (__mode) == IPMI_DCMI_POWER_READING_MODE_ENHANCED_SYSTEM_POWER_STATISTICS) ? 1 : 0)

#define IPMI_DCMI_POWER_READING_STATE_POWER_MEASUREMENT_ACTIVE       0x1
#define IPMI_DCMI_POWER_READING_STATE_NO_POWER_MEASUREMENT_AVAILABLE 0x0

/* HLiebig: specific value, not a bitmask */
#define IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM 0x01
#define IPMI_DCMI_EXCEPTION_ACTION_OEM_MIN 0x02
#define IPMI_DCMI_EXCEPTION_ACTION_OEM_MAX 0x10

/* achu: it's an 8 bit field, why not allow all 8 bitmasks?  Beats
 * me, that's what's in the spec
 */
#define IPMI_DCMI_EXCEPTION_ACTIONS_MIN 0x00
#define IPMI_DCMI_EXCEPTION_ACTIONS_MAX 0x1F

#define IPMI_DCMI_POWER_LIMIT_REQUESTED_MIN 0x0000
#define IPMI_DCMI_POWER_LIMIT_REQUESTED_MAX 0xFFFF

#define IPMI_DCMI_CORRECTION_TIME_LIMIT_MIN 0x00000000
#define IPMI_DCMI_CORRECTION_TIME_LIMIT_MAX 0xFFFFFFFF

#define IPMI_DCMI_MANAGEMENT_APPLICATION_STATISTICS_SAMPLING_PERIOD_MIN 0x0000
#define IPMI_DCMI_MANAGEMENT_APPLICATION_STATISTICS_SAMPLING_PERIOD_MAX 0xFFFF

#define IPMI_DCMI_EXCEPTION_ACTION_VALID(__exception_action) \
  (((__exception_action) == IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM \
    || ((__exception_action) >= 0x02 && (__exception_action) <= 0x10)) ? 1 : 0)

#define IPMI_DCMI_POWER_LIMIT_ACTIVATION_DEACTIVATE_POWER_LIMIT 0x0
#define IPMI_DCMI_POWER_LIMIT_ACTIVATION_ACTIVATE_POWER_LIMIT   0x1

#define IPMI_DCMI_POWER_LIMIT_ACTIVATION_VALID(__power_limit_activation) \
  (((__power_limit_activation) == IPMI_DCMI_POWER_LIMIT_ACTIVATION_DEACTIVATE_POWER_LIMIT \
    || (__power_limit_activation) == IPMI_DCMI_POWER_LIMIT_ACTIVATION_ACTIVATE_POWER_LIMIT) ? 1 : 0)

/*
 * fill* functions return 0 on success, -1 on error.
 *
 * obj_cmd_rq must be for the fill function's respective fiid
 * template request.
 *
 * see freeipmi/templates/ for template definitions
 */

extern fiid_template_t tmpl_dcmi_rolling_average_time_period;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_rq;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_mandatory_platform_attributes_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_optional_platform_attributes_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_manageability_access_attributes_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_capability_info_enhanced_system_power_statistics_attributes_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_asset_tag_rq;
extern fiid_template_t tmpl_cmd_dcmi_get_asset_tag_rs;
extern fiid_template_t tmpl_cmd_dcmi_set_asset_tag_rq;
extern fiid_template_t tmpl_cmd_dcmi_set_asset_tag_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_management_controller_identifier_string_rq;
extern fiid_template_t tmpl_cmd_dcmi_get_management_controller_identifier_string_rs;
extern fiid_template_t tmpl_cmd_dcmi_set_management_controller_identifier_string_rq;
extern fiid_template_t tmpl_cmd_dcmi_set_management_controller_identifier_string_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_sensor_info_rq;
extern fiid_template_t tmpl_cmd_dcmi_get_dcmi_sensor_info_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_power_reading_rq;
extern fiid_template_t tmpl_cmd_dcmi_get_power_reading_rs;
extern fiid_template_t tmpl_cmd_dcmi_get_power_limit_rq;
extern fiid_template_t tmpl_cmd_dcmi_get_power_limit_rs;
extern fiid_template_t tmpl_cmd_dcmi_set_power_limit_rq;
extern fiid_template_t tmpl_cmd_dcmi_set_power_limit_rs;
extern fiid_template_t tmpl_cmd_dcmi_activate_deactivate_power_limit_rq;
extern fiid_template_t tmpl_cmd_dcmi_activate_deactivate_power_limit_rs;

int fill_cmd_dcmi_get_dcmi_capability_info (uint8_t parameter_selector,
                                            fiid_obj_t obj_cmd_rq);

int fill_cmd_dcmi_get_asset_tag (uint8_t offset_to_read,
                                 uint8_t number_of_bytes_to_read,
                                 fiid_obj_t obj_cmd_rq);
  
int fill_cmd_dcmi_set_asset_tag (uint8_t offset_to_write,
                                 uint8_t number_of_bytes_to_write,
                                 const void *data,
                                 unsigned int data_len,
                                 fiid_obj_t obj_cmd_rq);

int fill_cmd_dcmi_get_management_controller_identifier_string (uint8_t offset_to_read,
                                                               uint8_t number_of_bytes_to_read,
                                                               fiid_obj_t obj_cmd_rq);
  
int fill_cmd_dcmi_set_management_controller_identifier_string (uint8_t offset_to_write,
                                                               uint8_t number_of_bytes_to_write,
                                                               const void *data,
                                                               unsigned int data_len,
                                                               fiid_obj_t obj_cmd_rq);

int fill_cmd_dcmi_get_dcmi_sensor_info (uint8_t sensor_type,
                                        uint8_t entity_id,
                                        uint8_t entity_instance,
                                        uint8_t entity_instance_start,
                                        fiid_obj_t obj_cmd_rq);
  
int fill_cmd_dcmi_get_power_reading (uint8_t mode,
                                     uint8_t mode_attributes,
                                     fiid_obj_t obj_cmd_rq);

int fill_cmd_dcmi_get_power_limit (fiid_obj_t obj_cmd_rq);

int fill_cmd_dcmi_set_power_limit (uint8_t exception_actions,
                                   uint16_t power_limit_requested,
                                   uint32_t correction_time_limit,
                                   uint16_t management_application_statistics_sampling_period,
                                   fiid_obj_t obj_cmd_rq);

int fill_cmd_dcmi_activate_deactivate_power_limit (uint8_t power_limit_activation,
                                                   fiid_obj_t obj_cmd_rq);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-dcmi-cmds.h */
