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

#ifndef _IPMI_OEM_INTEL_NODE_MANAGER_CMDS_H
#define _IPMI_OEM_INTEL_NODE_MANAGER_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel Node Manager
 *
 * http://download.intel.com/support/motherboards/server/s5500wb/sb/s5500wb_tps_1_0.pdf
 * 
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 */

#define IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MIN 0x0
#define IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MAX 0xF

/* To avoid gcc warnings, add +1 in comparison */
/* Include checks for possible oem network functions */
#define IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID(__val)              \
  ((((__val) + 1) >= (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MIN + 1)    \
    && (__val) <= IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MAX) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_GLOBAL_DISABLE_NODE_MANAGER_POLICY_CONTROL 0x00
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_GLOBAL_ENABLE_NODE_MANAGER_POLICY_CONTROL  0x01
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_DOMAIN_DISABLE_NODE_MANAGER_POLICIES   0x02
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_DOMAIN_ENABLE_NODE_MANAGER_POLITICES   0x03
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_POLICY_DISABLE_NODE_MANAGER_POLICIES   0x04
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_POLICY_ENABLE_NODE_MANAGER_POLITICES   0x05

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_VALID(__val)  \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_GLOBAL_DISABLE_NODE_MANAGER_POLICY_CONTROL \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_GLOBAL_ENABLE_NODE_MANAGER_POLICY_CONTROL \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_DOMAIN_DISABLE_NODE_MANAGER_POLICIES \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_DOMAIN_ENABLE_NODE_MANAGER_POLITICES \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_POLICY_DISABLE_NODE_MANAGER_POLICIES \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_POLICY_ENABLE_NODE_MANAGER_POLITICES) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED  0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_DISABLED 0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED_VALID(__val)         \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED               \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_DISABLED) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER                       0x0
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLENT_TEMPERATURE_LIMIT_POLICY_TRIGGER 0x1

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_VALID(__val)    \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLENT_TEMPERATURE_LIMIT_POLICY_TRIGGER) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_POLICY_POINTED_BY_POLICY_ID_SHALL_BE_REMOVED 0x0
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_ADD_POWER_POLICY                             0x1

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_VALID(__val) \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_POLICY_POINTED_BY_POLICY_ID_SHALL_BE_REMOVED \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_ADD_POWER_POLICY) ? 1 : 0)

/* achu: not in spec, assume it to be true */
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE  0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE 0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_VALID(__val) \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_PER_DOMAIN_NODE_MANAGER_POLICY_CONTROL_ENABLED  0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_PER_DOMAIN_NODE_MANAGER_POLICY_CONTROL_DISABLED 0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_GLOBAL_NODE_MANAGER_POLICY_CONTROL_ENABLED  0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_GLOBAL_NODE_MANAGER_POLICY_CONTROL_DISABLED 0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY 0x1

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_VALID(__val)            \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_MIN 0
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_MAX 239

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_VALID(__val) \
  ((((__val) + 1) >= (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_MIN + 1) \
    && (__val) <= IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_MAX) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_MIN 0
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_MAX 240

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_VALID(__val) \
  ((((__val) + 1) >= (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_MIN + 1) \
    && (__val) <= IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_MAX) ? 1 : 0)

/* achu: not in spec, assume it to be true */
#define IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD        0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD 0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID(__val)         \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD    \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_RESET_GLOBAL_STATISTICS 0x00
#define IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_PER_POLICY_STATISTICS   0x01

#define IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_VALID(__val)             \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_RESET_GLOBAL_STATISTICS \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_PER_POLICY_STATISTICS) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_POWER_STATISTICS             0x01
#define IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_INLET_TEMPERATURE_STATISTICS 0x02
#define IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_POWER_STATISTICS         0x11
#define IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_TRIGGER_STATISTICS       0x12

#define IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_VALID(__val)        \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_POWER_STATISTICS \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_INLET_TEMPERATURE_STATISTICS \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_POWER_STATISTICS \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_TRIGGER_STATISTICS) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_GLOBAL_ADMINISTRATIVE_STATE_ENABLED   0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_GLOBAL_ADMINISTRATIVE_STATE_OTHERWISE 0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_OPERATIONAL_STATE_ACTIVELY_MONITORING_DEFINED_TRIGGER 0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_OPERATIONAL_STATE_SUSPENDED                           0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_MEASUREMENTS_STATE_IN_PROGRESS 0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_MEASUREMENTS_STATE_SUSPENDED   0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ACTIVATION_STATE_TRIGGERED_AND_ACTIVELY_LIMITING_TARGET 0x1
#define IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ACTIVATION_STATE_NOT_TRIGGERED                          0x0

#define IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_LIMITING_SCOPE_PLATFORM_POWER_LIMITING 0
#define IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_LIMITING_SCOPE_CPU_POWER_LIMITING      1

#define IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_BASED_ON_WALL_INPUT_POWER_PSU_INPUT_POWER           0
#define IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_BASED_ON_DC_POWER_PSU_OUTPUT_POWER_OR_BLADED_SYSTEM 1

#define IPMI_OEM_INTEL_NODE_MANAGER_VERSION_1_0 0x01
#define IPMI_OEM_INTEL_NODE_MANAGER_VERSION_1_5 0x02

#define IPMI_OEM_INTEL_NODE_MANAGER_IPMI_INTERFACE_VERSION_1_0 0x01

#define IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_REGISTER_ALERT_RECEIVER   0x0
#define IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_UNREGISTER_ALERT_RECEIVER 0x1

#define IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_VALID(__val) \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_REGISTER_ALERT_RECEIVER \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_UNREGISTER_ALERT_RECEIVER) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_USE_VOLATILE_ALERT_STRING 0x00

#define IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_MIN 0x00
#define IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_MAX 0x7F

#define IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_VALID(__val)  \
  ((((__val) + 1) >= (IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_MIN + 1) \
    && (__val) <= IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_MAX) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_DONT_SEND_AN_ALERT_STRING                       0x0
#define IPMI_OEM_INTEL_NODE_MANAGER_SEND_ALERT_STRING_IDENTIFIED_BY_STRING_SELECTOR 0x1

#define IPMI_OEM_INTEL_NODE_MANAGER_SEND_ALERT_STRING_VALID(__val)      \
  (((__val) == IPMI_OEM_INTEL_NODE_MANAGER_DONT_SEND_AN_ALERT_STRING    \
    || (__val) == IPMI_OEM_INTEL_NODE_MANAGER_SEND_ALERT_STRING_IDENTIFIED_BY_STRING_SELECTOR) ? 1 : 0)

#define IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_CONFIGURATION_VALID   0x0
#define IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_CONFIGURATION_INVALID 0x1

#define IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_SELECTOR_OPERATOR_USE_VOLATILE_DESTINATION_INFO 0x0

/* 
 * fill* functions return 0 on success, -1 on error.
 *
 * obj_cmd_rq must be for the fill function's respective fiid
 * template request.
 *
 * see freeipmi/templates/ for template definitions 
 */
 
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rs;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rq;
extern fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rs;

int fill_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control (uint8_t policy_enable_disable,
                                                                                uint8_t domain_id,
                                                                                uint8_t policy_id,
                                                                                fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_set_node_manager_policy (uint8_t domain_id,
                                                             uint8_t policy_enabled,
                                                             uint8_t policy_id,
                                                             uint8_t policy_trigger_type,
                                                             uint8_t policy_configuration_action,
                                                             uint8_t policy_exception_actions_send_alert,
                                                             uint8_t policy_exception_actions_shutdown_system,
                                                             uint8_t power_limit,
                                                             uint8_t correction_time_limit,
                                                             uint8_t policy_trigger_limit,
                                                             uint8_t statistics_reporting_period,
                                                             fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_get_node_manager_policy (uint8_t domain_id,
                                                             uint8_t policy_id,
                                                             fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds (uint8_t domain_id,
                                                                       uint8_t policy_id,
                                                                       uint8_t *alert_threshold1,
                                                                       uint8_t *alert_threshold2,
                                                                       uint8_t *alert_threshold3,
                                                                       fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds (uint8_t domain_id,
                                                                       uint8_t policy_id,
                                                                       fiid_obj_t obj_cmd_rq);
  
int fill_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods (uint8_t domain_id,
                                                                             uint8_t policy_id,
                                                                             uint8_t *policy1_suspend_start_time,
                                                                             uint8_t *policy1_suspend_stop_time,
                                                                             uint8_t *policy1_suspend_period_recurrence_monday,
                                                                             uint8_t *policy1_suspend_period_recurrence_tuesday,
                                                                             uint8_t *policy1_suspend_period_recurrence_wednesday,
                                                                             uint8_t *policy1_suspend_period_recurrence_thursday,
                                                                             uint8_t *policy1_suspend_period_recurrence_friday,
                                                                             uint8_t *policy1_suspend_period_recurrence_saturday,
                                                                             uint8_t *policy1_suspend_period_recurrence_sunday,
                                                                             uint8_t *policy2_suspend_start_time,
                                                                             uint8_t *policy2_suspend_stop_time,
                                                                             uint8_t *policy2_suspend_period_recurrence_monday,
                                                                             uint8_t *policy2_suspend_period_recurrence_tuesday,
                                                                             uint8_t *policy2_suspend_period_recurrence_wednesday,
                                                                             uint8_t *policy2_suspend_period_recurrence_thursday,
                                                                             uint8_t *policy2_suspend_period_recurrence_friday,
                                                                             uint8_t *policy2_suspend_period_recurrence_saturday,
                                                                             uint8_t *policy2_suspend_period_recurrence_sunday,
                                                                             uint8_t *policy3_suspend_start_time,
                                                                             uint8_t *policy3_suspend_stop_time,
                                                                             uint8_t *policy3_suspend_period_recurrence_monday,
                                                                             uint8_t *policy3_suspend_period_recurrence_tuesday,
                                                                             uint8_t *policy3_suspend_period_recurrence_wednesday,
                                                                             uint8_t *policy3_suspend_period_recurrence_thursday,
                                                                             uint8_t *policy3_suspend_period_recurrence_friday,
                                                                             uint8_t *policy3_suspend_period_recurrence_saturday,
                                                                             uint8_t *policy3_suspend_period_recurrence_sunday,
                                                                             uint8_t *policy4_suspend_start_time,
                                                                             uint8_t *policy4_suspend_stop_time,
                                                                             uint8_t *policy4_suspend_period_recurrence_monday,
                                                                             uint8_t *policy4_suspend_period_recurrence_tuesday,
                                                                             uint8_t *policy4_suspend_period_recurrence_wednesday,
                                                                             uint8_t *policy4_suspend_period_recurrence_thursday,
                                                                             uint8_t *policy4_suspend_period_recurrence_friday,
                                                                             uint8_t *policy4_suspend_period_recurrence_saturday,
                                                                             uint8_t *policy4_suspend_period_recurrence_sunday,
                                                                             uint8_t *policy5_suspend_start_time,
                                                                             uint8_t *policy5_suspend_stop_time,
                                                                             uint8_t *policy5_suspend_period_recurrence_monday,
                                                                             uint8_t *policy5_suspend_period_recurrence_tuesday,
                                                                             uint8_t *policy5_suspend_period_recurrence_wednesday,
                                                                             uint8_t *policy5_suspend_period_recurrence_thursday,
                                                                             uint8_t *policy5_suspend_period_recurrence_friday,
                                                                             uint8_t *policy5_suspend_period_recurrence_saturday,
                                                                             uint8_t *policy5_suspend_period_recurrence_sunday,
                                                                             fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods (uint8_t domain_id,
                                                                             uint8_t policy_id,
                                                                             fiid_obj_t obj_cmd_rq);
  
int fill_cmd_oem_intel_node_manager_reset_node_manager_statistics (uint8_t mode,
                                                                   uint8_t domain_id,
                                                                   uint8_t policy_id,
                                                                   fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_get_node_manager_statistics (uint8_t mode,
                                                                 uint8_t domain_id,
                                                                 uint8_t policy_id,
                                                                 fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_get_node_manager_capabilities (uint8_t domain_id,
                                                                   uint8_t policy_trigger_type,
                                                                   uint8_t policy_type,
                                                                   fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_get_node_manager_version (fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_set_node_manager_power_draw_range (uint8_t domain_id,
                                                                       uint8_t minimum_power_draw,
                                                                       uint8_t maximum_power_draw,
                                                                       fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination (uint8_t channel_number,
                                                                        uint8_t destination_information_operation,
                                                                        uint8_t destination_information,
                                                                        uint8_t alert_string_selector,
                                                                        uint8_t send_alert_string,
                                                                        fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb (uint8_t channel_number,
                                                                             uint8_t destination_information_operation,
                                                                             uint8_t slave_address,
                                                                             uint8_t alert_string_selector,
                                                                             uint8_t send_alert_string,
                                                                             fiid_obj_t obj_cmd_rq);
  
int fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan (uint8_t channel_number,
                                                                            uint8_t destination_information_operation,
                                                                            uint8_t destination_selector,
                                                                            uint8_t alert_string_selector,
                                                                            uint8_t send_alert_string,
                                                                            fiid_obj_t obj_cmd_rq);

int fill_cmd_oem_intel_node_manager_get_node_manager_alert_destination (fiid_obj_t obj_cmd_rq);

#ifdef __cplusplus
}
#endif
#endif
