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

#ifndef _IPMI_OEM_INTEL_NODE_MANAGER_CMDS_TEMPLATES_H
#define _IPMI_OEM_INTEL_NODE_MANAGER_CMDS_TEMPLATES_H

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

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel Node Manager
 *
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 */

Enable Disable Node Manager Policy Control Request
--------------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_enable_disable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Enable Disable Node Manager Policy Control Response
---------------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},   
    { 0, "", 0}
  };

Set Node Manager Policy Request
-------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_trigger_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_configuration_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.send_alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.shutdown_system", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6,  "policy_exception_actions.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "power_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "policy_trigger_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Policy Response
--------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Policy Request
-------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Policy Response
--------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "per_domain_node_manager_policy_control_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "global_node_manager_policy_control_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_trigger_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.send_alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.shutdown_system", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6,  "policy_exception_actions.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "power_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "policy_trigger_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Alert Thresholds Request
-----------------------------------------

/* achu: spec lists "alert threshold array", however each alert
 * threshold is 2 bytes and there is a max of 3, so I list as the
 * entries and make them optional
 */
fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "number_of_alert_thresholds", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "alert_threshold1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "alert_threshold2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "alert_threshold3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Alert Thresholds Response
------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Alert Thresholds Request
-----------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Alert Thresholds Response
------------------------------------------

/* achu: spec lists "alert threshold array", however each alert
 * threshold is 2 bytes and there is a max of 3, so I list the
 * entries and make them optional
 */
fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "number_of_alert_thresholds", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "alert_threshold1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "alert_threshold2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "alert_threshold3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Policy Suspend Periods Request
-----------------------------------------------

/* achu: spec lists "array of policy suspend periods", however each
 * alert threshold is 3 bytes and there is a max of 5, so I list the
 * entries and make them optional
 */
fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "number_of_policy_suspend_periods", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy1.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy1.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy2.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy2.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy3.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy3.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy4.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy4.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy5.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy5.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Policy Suspend Periods Response
------------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Policy Suspend Periods Request
-----------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Policy Suspend Periods Response
------------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "number_of_policy_suspend_periods", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy1.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy1.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy1.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy2.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy2.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy2.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy3.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy3.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy3.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy4.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy4.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy4.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy5.suspend_start_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy5.suspend_stop_time", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.monday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.tuesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.wednesday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.thursday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.friday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.saturday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.sunday", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy5.suspend_period_recurrence.reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Reset Node Manager Statistics Request
-------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Reset Node Manager Statistics Response
--------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Statistics Request
-----------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Statistics Response
------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "current", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "average", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_global_administrative_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_operational_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "measurements_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_activation_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Capabilities Request
-------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_trigger_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Capabilities Response
--------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "max_concurrent_settings", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "max power_thermal", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "min_power_thermal", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "min_correction_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "max_correction_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "min_statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "max_statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "domain_limiting_scope", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "limiting_based_on", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Version Request
--------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Version Response
---------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "node_manager_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "ipmi_interface_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "patch_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "major_firmware_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "minor_firmware_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Power Draw Range Request
-----------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum_power_draw", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_power_draw", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Power Draw Range Response
------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Alert Destination Request
------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "destination_information_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "destination_information", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "alert_string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "send_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Alert Destination (IPMB) Request
-------------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "destination_information_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "alert_string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "send_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Alert Destination (LAN) Request
------------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "destination_information_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "alert_string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "send_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Node Manager Alert Destination Response
-------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Alert Destination Request
------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Node Manager Alert Destination Response
-------------------------------------------

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "destination_information_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "destination_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "alert_string_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "send_alert_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_OEM_CMDS_TEMPLATES_H */
