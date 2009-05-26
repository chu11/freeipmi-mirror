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

#ifndef _IPMI_DCMI_H
#define _IPMI_DCMI_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_NET_FN_DCMI_RQ  0x2C
#define IPMI_NET_FN_DCMI_RS  0x2D

#define IPMI_DCMI_GROUP_EXTENSION_IDENTIFICATION 0xDC

#define IPMI_DCMI_CMD_GET_DCMI_CAPABILITIY_INFO        0x01
#define IPMI_DCMI_CMD_GET_POWER_READING                0x02
#define IPMI_DCMI_CMD_GET_POWER_LIMIT                  0x03
#define IPMI_DCMI_CMD_SET_POWER_LIMIT                  0x04
#define IPMI_DCMI_CMD_ACTIVATE_DEACTIVATE_POWER_LIMIT  0x05
#define IPMI_DCMI_CMD_GET_ASSET_TAG                    0x06
#define IPMI_DCMI_CMD_GET_DCMI_SENSOR_INFO             0x07

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

#define IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX 16

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

#define IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM 0x01

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

/* DCMI Command Specific Completion Codes */

/* IPMI_DCMI_CMD_GET_POWER_LIMIT */

#define IPMI_DCMI_COMP_CODE_NO_SET_POWER_LIMIT                        0x80
#define IPMI_DCMI_COMP_CODE_NO_SET_POWER_LIMIT_STR \
  "No Set Power Limit"

/* IPMI_DCMI_CMD_SET_POWER_LIMIT */

#define IPMI_DCMI_COMP_CODE_POWER_LIMIT_OUT_OF_RANGE                  0x84
#define IPMI_DCMI_COMP_CODE_POWER_LIMIT_OUT_OF_RANGE_STR \
  "Power Limit out of range"

#define IPMI_DCMI_COMP_CODE_CORRECTION_TIME_OUT_OF_RANGE              0x85
#define IPMI_DCMI_COMP_CODE_CORRECTION_TIME_OUT_OF_RANGE_STR \
  "Correction Time out of range"

#define IPMI_DCMI_COMP_CODE_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE  0x89 /* not a typo, is 0x89 */
#define IPMI_DCMI_COMP_CODE_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE_STR \
  "Statistics Reporting Period out of range"

/* 
 * fill* functions return 0 on success, -1 on error.
 *
 * obj_cmd_rq must be for the fill function's respective fiid
 * template request.
 *
 * ipmi_cmd* functions return 0 on success, -1 on error.
 *
 * obj_cmd_rs must be for the fill function's respective fiid
 * template response.
 *
 * see freeipmi/templates/ for template definitions 
 */

extern fiid_template_t tmpl_dcmi_rolling_average_time_period; 
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_rq;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_supported_dcmi_capabilities_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_mandatory_platform_attributes_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_optional_platform_attributes_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_manageability_access_attributes_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_capability_info_enhanced_system_power_statistics_attributes_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_power_reading_rq;
extern fiid_template_t tmpl_dcmi_cmd_get_power_reading_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_power_limit_rq;
extern fiid_template_t tmpl_dcmi_cmd_get_power_limit_rs;
extern fiid_template_t tmpl_dcmi_cmd_set_power_limit_rq;
extern fiid_template_t tmpl_dcmi_cmd_set_power_limit_rs;
extern fiid_template_t tmpl_dcmi_cmd_activate_deactivate_power_limit_rq;
extern fiid_template_t tmpl_dcmi_cmd_activate_deactivate_power_limit_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_asset_tag_rq;
extern fiid_template_t tmpl_dcmi_cmd_get_asset_tag_rs;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_sensor_info_rq;
extern fiid_template_t tmpl_dcmi_cmd_get_dcmi_sensor_info_rs;

/*
 * fill functions
 */

int fill_dcmi_cmd_get_dcmi_capability_info (uint8_t parameter_selector,
                                            fiid_obj_t obj_cmd_rq);

int fill_dcmi_cmd_get_power_reading (uint8_t mode,
                                     uint8_t mode_attributes,
                                     fiid_obj_t obj_cmd_rq);

int fill_dcmi_cmd_get_power_limit (fiid_obj_t obj_cmd_rq);

int fill_dcmi_cmd_set_power_limit (uint8_t exception_actions,
                                   uint16_t power_limit_requested,
                                   uint32_t correction_time_limit,
                                   uint16_t management_application_statistics_sampling_period,
                                   fiid_obj_t obj_cmd_rq);

int fill_dcmi_cmd_activate_deactivate_power_limit (uint8_t power_limit_activation,
                                                   fiid_obj_t obj_cmd_rq);

int fill_dcmi_cmd_get_asset_tag (uint8_t offset_to_read,
                                 uint8_t number_of_bytes_to_read,
                                 fiid_obj_t obj_cmd_rq);

int fill_dcmi_cmd_get_dcmi_sensor_info (uint8_t sensor_type,
                                        uint8_t entity_id,
                                        uint8_t entity_instance,
                                        uint8_t entity_instance_start,
                                        fiid_obj_t obj_cmd_rq);

/*
 * api functions
 */

int ipmi_dcmi_cmd_get_dcmi_capability_info (ipmi_ctx_t ctx,
                                            uint8_t parameter_selector,
                                            fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_dcmi_capability_info_supported_dcmi_capabilities (ipmi_ctx_t ctx,
                                                                        fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_dcmi_capability_info_mandatory_platform_attributes (ipmi_ctx_t ctx,
                                                                          fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_dcmi_capability_info_optional_platform_attributes (ipmi_ctx_t ctx,
                                                                         fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_dcmi_capability_info_manageability_access_attributes (ipmi_ctx_t ctx,
                                                                            fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_dcmi_capability_info_enhanced_system_power_statistics_attributes (ipmi_ctx_t ctx,
                                                                                        fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_power_reading (ipmi_ctx_t ctx,
                                     uint8_t mode,
                                     uint8_t mode_attributes,
                                     fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_power_limit (ipmi_ctx_t ctx,
                                   fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_set_power_limit (ipmi_ctx_t ctx,
                                   uint8_t exception_actions,
                                   uint16_t power_limit_requested,
                                   uint32_t correction_time_limit,
                                   uint16_t management_application_statistics_sampling_period,
                                   fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_activate_deactivate_power_limit (ipmi_ctx_t ctx,
                                                   uint8_t power_limit_activation,
                                                   fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_asset_tag (ipmi_ctx_t ctx,
                                 uint8_t offset_to_read,
                                 uint8_t number_of_bytes_to_read,
                                 fiid_obj_t obj_cmd_rs);

int ipmi_dcmi_cmd_get_dcmi_sensor_info (ipmi_ctx_t ctx,
                                        uint8_t sensor_type,
                                        uint8_t entity_id,
                                        uint8_t entity_instance,
                                        uint8_t entity_instance_start,
                                        fiid_obj_t obj_cmd_rs);

/* 
 * util functions
 */

/* returns 0 on success, -1 on error */
int ipmi_dcmi_completion_code_strerror_r (uint8_t cmd,
                                          uint8_t netfn,
                                          uint8_t comp_code,
                                          char *errstr,
                                          size_t len);

/* returns 0 on success, -1 on error */
int ipmi_dcmi_completion_code_strerror_cmd_r (fiid_obj_t obj_cmd,
                                              uint8_t netfn,
                                              char *errstr,
                                              size_t len);

const char *ipmi_dcmi_cmd_str (uint8_t cmd);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_DCMI_H */
