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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-oem-intel-node-manager-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},   
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_trigger_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_configuration_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2,  "aggressive_cpu_power_correction", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_storage_option", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.send_alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.shutdown_system", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6,  "policy_exception_actions.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "policy_target_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "policy_trigger_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "per_domain_node_manager_policy_control_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "global_node_manager_policy_control_enabled", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_created_and_managed_by_other_management", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_trigger_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2,  "aggressive_cpu_power_correction", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_storage_option", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.send_alert", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "policy_exception_actions.shutdown_system", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6,  "policy_exception_actions.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* listed as "power limit" in 2.0 spec but "policy target limit"
     * in set version.  changing to 'policy target limit' to be
     * consistent
     */
    { 16, "policy_target_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "correction_time_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "policy_trigger_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_trigger_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "policy_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "max_concurrent_settings", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "max_power_thermal_time_after_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "min_power_thermal_time_after_reset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "min_correction_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "max_correction_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "min_statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "max_statistics_reporting_period", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_limiting_scope.domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3,  "domain_limiting_scope.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "domain_limiting_scope.limiting_based_on", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_limiting_policy_id_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "domain_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_oem_intel_node_manager_get_limiting_policy_id_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "policy_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

int
fill_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control (uint8_t policy_enable_disable,
                                                                            uint8_t domain_id,
                                                                            uint8_t policy_id,
                                                                            fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_VALID (policy_enable_disable)
      || !IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_ENABLE_DISABLE_NODE_MANAGER_POLICY_CONTROL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_enable_disable", policy_enable_disable);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_set_node_manager_policy (uint8_t domain_id,
                                                         uint8_t policy_enabled,
                                                         uint8_t policy_id,
                                                         uint8_t policy_trigger_type,
                                                         uint8_t policy_configuration_action,
							 uint8_t aggressive_cpu_power_correction,
							 uint8_t policy_storage_option,
                                                         uint8_t policy_exception_actions_send_alert,
                                                         uint8_t policy_exception_actions_shutdown_system,
                                                         uint16_t policy_target_limit,
                                                         uint32_t correction_time_limit,
                                                         uint16_t policy_trigger_limit,
                                                         uint16_t statistics_reporting_period,
                                                         fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED_VALID (policy_enabled)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_VALID (policy_trigger_type)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_VALID (policy_configuration_action)
      || !IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_VALID (aggressive_cpu_power_correction)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_STORAGE_VALID (policy_storage_option)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_VALID (policy_exception_actions_send_alert)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_VALID (policy_exception_actions_shutdown_system)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_enabled", policy_enabled);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_trigger_type", policy_trigger_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_configuration_action", policy_configuration_action);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "aggressive_cpu_power_correction", aggressive_cpu_power_correction);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_storage_option", policy_storage_option);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_exception_actions.send_alert", policy_exception_actions_send_alert);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_exception_actions.shutdown_system", policy_exception_actions_shutdown_system);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_exception_actions.reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_target_limit", policy_target_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "correction_time_limit", correction_time_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_trigger_limit", policy_trigger_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "statistics_reporting_period", statistics_reporting_period);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_node_manager_policy (uint8_t domain_id,
                                                         uint8_t policy_id,
                                                         fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_POLICY);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds (uint8_t domain_id,
                                                                   uint8_t policy_id,
                                                                   uint16_t *alert_threshold1,
                                                                   uint16_t *alert_threshold2,
                                                                   uint16_t *alert_threshold3,
                                                                   fiid_obj_t obj_cmd_rq)
{
  uint8_t number_of_alert_thresholds = 0;

  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_THRESHOLDS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);
  /* Note: Do not need to "order" these, these fields are optional, and thus
   * the fiid library can "pack" them accordingly.
   */
  if (alert_threshold1)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "alert_threshold1", *alert_threshold1);
      number_of_alert_thresholds++;
    }
  if (alert_threshold2)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "alert_threshold2", *alert_threshold2);
      number_of_alert_thresholds++;
    }
  if (alert_threshold3)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "alert_threshold3", *alert_threshold3);
      number_of_alert_thresholds++;
    }
  FILL_FIID_OBJ_SET (obj_cmd_rq, "number_of_alert_thresholds", number_of_alert_thresholds);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds (uint8_t domain_id,
                                                                   uint8_t policy_id,
                                                                   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_ALERT_THRESHOLDS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods (uint8_t domain_id,
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
                                                                         fiid_obj_t obj_cmd_rq)
{
  uint8_t number_of_policy_suspend_periods = 0;

  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !((!policy1_suspend_start_time
            && !policy1_suspend_stop_time
            && !policy1_suspend_period_recurrence_monday
            && !policy1_suspend_period_recurrence_tuesday
            && !policy1_suspend_period_recurrence_wednesday
            && !policy1_suspend_period_recurrence_thursday
            && !policy1_suspend_period_recurrence_friday
            && !policy1_suspend_period_recurrence_saturday
            && !policy1_suspend_period_recurrence_sunday)
           || (policy1_suspend_start_time
               && policy1_suspend_stop_time
               && policy1_suspend_period_recurrence_monday
               && policy1_suspend_period_recurrence_tuesday
               && policy1_suspend_period_recurrence_wednesday
               && policy1_suspend_period_recurrence_thursday
               && policy1_suspend_period_recurrence_friday
               && policy1_suspend_period_recurrence_saturday
               && policy1_suspend_period_recurrence_sunday
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_VALID (*policy1_suspend_start_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_VALID (*policy1_suspend_stop_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy1_suspend_period_recurrence_monday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy1_suspend_period_recurrence_tuesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy1_suspend_period_recurrence_wednesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy1_suspend_period_recurrence_thursday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy1_suspend_period_recurrence_friday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy1_suspend_period_recurrence_saturday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy1_suspend_period_recurrence_sunday)))
      || !((!policy2_suspend_start_time
            && !policy2_suspend_stop_time
            && !policy2_suspend_period_recurrence_monday
            && !policy2_suspend_period_recurrence_tuesday
            && !policy2_suspend_period_recurrence_wednesday
            && !policy2_suspend_period_recurrence_thursday
            && !policy2_suspend_period_recurrence_friday
            && !policy2_suspend_period_recurrence_saturday
            && !policy2_suspend_period_recurrence_sunday)
           || (policy2_suspend_start_time
               && policy2_suspend_stop_time
               && policy2_suspend_period_recurrence_monday
               && policy2_suspend_period_recurrence_tuesday
               && policy2_suspend_period_recurrence_wednesday
               && policy2_suspend_period_recurrence_thursday
               && policy2_suspend_period_recurrence_friday
               && policy2_suspend_period_recurrence_saturday
               && policy2_suspend_period_recurrence_sunday
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_VALID (*policy2_suspend_start_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_VALID (*policy2_suspend_stop_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy2_suspend_period_recurrence_monday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy2_suspend_period_recurrence_tuesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy2_suspend_period_recurrence_wednesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy2_suspend_period_recurrence_thursday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy2_suspend_period_recurrence_friday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy2_suspend_period_recurrence_saturday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy2_suspend_period_recurrence_sunday)))
      || !((!policy3_suspend_start_time
            && !policy3_suspend_stop_time
            && !policy3_suspend_period_recurrence_monday
            && !policy3_suspend_period_recurrence_tuesday
            && !policy3_suspend_period_recurrence_wednesday
            && !policy3_suspend_period_recurrence_thursday
            && !policy3_suspend_period_recurrence_friday
            && !policy3_suspend_period_recurrence_saturday
            && !policy3_suspend_period_recurrence_sunday)
           || (policy3_suspend_start_time
               && policy3_suspend_stop_time
               && policy3_suspend_period_recurrence_monday
               && policy3_suspend_period_recurrence_tuesday
               && policy3_suspend_period_recurrence_wednesday
               && policy3_suspend_period_recurrence_thursday
               && policy3_suspend_period_recurrence_friday
               && policy3_suspend_period_recurrence_saturday
               && policy3_suspend_period_recurrence_sunday
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_VALID (*policy3_suspend_start_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_VALID (*policy3_suspend_stop_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy3_suspend_period_recurrence_monday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy3_suspend_period_recurrence_tuesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy3_suspend_period_recurrence_wednesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy3_suspend_period_recurrence_thursday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy3_suspend_period_recurrence_friday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy3_suspend_period_recurrence_saturday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy3_suspend_period_recurrence_sunday)))
      || !((!policy4_suspend_start_time
            && !policy4_suspend_stop_time
            && !policy4_suspend_period_recurrence_monday
            && !policy4_suspend_period_recurrence_tuesday
            && !policy4_suspend_period_recurrence_wednesday
            && !policy4_suspend_period_recurrence_thursday
            && !policy4_suspend_period_recurrence_friday
            && !policy4_suspend_period_recurrence_saturday
            && !policy4_suspend_period_recurrence_sunday)
           || (policy4_suspend_start_time
               && policy4_suspend_stop_time
               && policy4_suspend_period_recurrence_monday
               && policy4_suspend_period_recurrence_tuesday
               && policy4_suspend_period_recurrence_wednesday
               && policy4_suspend_period_recurrence_thursday
               && policy4_suspend_period_recurrence_friday
               && policy4_suspend_period_recurrence_saturday
               && policy4_suspend_period_recurrence_sunday
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_VALID (*policy4_suspend_start_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_VALID (*policy4_suspend_stop_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy4_suspend_period_recurrence_monday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy4_suspend_period_recurrence_tuesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy4_suspend_period_recurrence_wednesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy4_suspend_period_recurrence_thursday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy4_suspend_period_recurrence_friday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy4_suspend_period_recurrence_saturday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy4_suspend_period_recurrence_sunday)))
      || !((!policy5_suspend_start_time
            && !policy5_suspend_stop_time
            && !policy5_suspend_period_recurrence_monday
            && !policy5_suspend_period_recurrence_tuesday
            && !policy5_suspend_period_recurrence_wednesday
            && !policy5_suspend_period_recurrence_thursday
            && !policy5_suspend_period_recurrence_friday
            && !policy5_suspend_period_recurrence_saturday
            && !policy5_suspend_period_recurrence_sunday)
           || (policy5_suspend_start_time
               && policy5_suspend_stop_time
               && policy5_suspend_period_recurrence_monday
               && policy5_suspend_period_recurrence_tuesday
               && policy5_suspend_period_recurrence_wednesday
               && policy5_suspend_period_recurrence_thursday
               && policy5_suspend_period_recurrence_friday
               && policy5_suspend_period_recurrence_saturday
               && policy5_suspend_period_recurrence_sunday
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_VALID (*policy5_suspend_start_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_VALID (*policy5_suspend_stop_time)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy5_suspend_period_recurrence_monday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy5_suspend_period_recurrence_tuesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy5_suspend_period_recurrence_wednesday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy5_suspend_period_recurrence_thursday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy5_suspend_period_recurrence_friday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy5_suspend_period_recurrence_saturday)
               && IPMI_OEM_INTEL_NODE_MANAGER_SUSPEND_PERIOD_VALID (*policy5_suspend_period_recurrence_sunday)))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_SUSPEND_PERIODS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);
  
  /* Note: Do not need to "order" these, these fields are optional, and thus
   * the fiid library can "pack" them accordingly.
   */

  /* single pointer check sufficient given earlier checks */
  if (policy1_suspend_start_time)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_start_time", *policy1_suspend_start_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_stop_time", *policy1_suspend_stop_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.monday", *policy1_suspend_period_recurrence_monday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.tuesday", *policy1_suspend_period_recurrence_tuesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.wednesday", *policy1_suspend_period_recurrence_wednesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.thursday", *policy1_suspend_period_recurrence_thursday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.friday", *policy1_suspend_period_recurrence_friday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.saturday", *policy1_suspend_period_recurrence_saturday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.sunday", *policy1_suspend_period_recurrence_sunday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy1.suspend_period_recurrence.reserved", 0);
      number_of_policy_suspend_periods++;
    }

  /* single pointer check sufficient given earlier checks */
  if (policy2_suspend_start_time)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_start_time", *policy2_suspend_start_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_stop_time", *policy2_suspend_stop_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.monday", *policy2_suspend_period_recurrence_monday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.tuesday", *policy2_suspend_period_recurrence_tuesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.wednesday", *policy2_suspend_period_recurrence_wednesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.thursday", *policy2_suspend_period_recurrence_thursday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.friday", *policy2_suspend_period_recurrence_friday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.saturday", *policy2_suspend_period_recurrence_saturday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.sunday", *policy2_suspend_period_recurrence_sunday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy2.suspend_period_recurrence.reserved", 0);
      number_of_policy_suspend_periods++;
    }

  /* single pointer check sufficient given earlier checks */
  if (policy3_suspend_start_time)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_start_time", *policy3_suspend_start_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_stop_time", *policy3_suspend_stop_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.monday", *policy3_suspend_period_recurrence_monday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.tuesday", *policy3_suspend_period_recurrence_tuesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.wednesday", *policy3_suspend_period_recurrence_wednesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.thursday", *policy3_suspend_period_recurrence_thursday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.friday", *policy3_suspend_period_recurrence_friday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.saturday", *policy3_suspend_period_recurrence_saturday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.sunday", *policy3_suspend_period_recurrence_sunday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy3.suspend_period_recurrence.reserved", 0);
      number_of_policy_suspend_periods++;
    }

  /* single pointer check sufficient given earlier checks */
  if (policy4_suspend_start_time)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_start_time", *policy4_suspend_start_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_stop_time", *policy4_suspend_stop_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.monday", *policy4_suspend_period_recurrence_monday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.tuesday", *policy4_suspend_period_recurrence_tuesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.wednesday", *policy4_suspend_period_recurrence_wednesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.thursday", *policy4_suspend_period_recurrence_thursday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.friday", *policy4_suspend_period_recurrence_friday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.saturday", *policy4_suspend_period_recurrence_saturday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.sunday", *policy4_suspend_period_recurrence_sunday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy4.suspend_period_recurrence.reserved", 0);
      number_of_policy_suspend_periods++;
    }

  /* single pointer check sufficient given earlier checks */
  if (policy5_suspend_start_time)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_start_time", *policy5_suspend_start_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_stop_time", *policy5_suspend_stop_time);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.monday", *policy5_suspend_period_recurrence_monday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.tuesday", *policy5_suspend_period_recurrence_tuesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.wednesday", *policy5_suspend_period_recurrence_wednesday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.thursday", *policy5_suspend_period_recurrence_thursday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.friday", *policy5_suspend_period_recurrence_friday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.saturday", *policy5_suspend_period_recurrence_saturday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.sunday", *policy5_suspend_period_recurrence_sunday);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "policy5.suspend_period_recurrence.reserved", 0);
      number_of_policy_suspend_periods++;
    }

  FILL_FIID_OBJ_SET (obj_cmd_rq, "number_of_policy_suspend_periods", number_of_policy_suspend_periods);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods (uint8_t domain_id,
                                                                         uint8_t policy_id,
                                                                         fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_POLICY_SUSPEND_PERIODS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_reset_node_manager_statistics (uint8_t mode,
                                                               uint8_t domain_id,
                                                               uint8_t policy_id,
                                                               fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_VALID (mode)
      || !IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_RESET_NODE_MANAGER_STATISTICS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "mode", mode);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_node_manager_statistics (uint8_t mode,
                                                             uint8_t domain_id,
                                                             uint8_t policy_id,
                                                             fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_VALID (mode)
      || !IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_STATISTICS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "mode", mode);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_id", policy_id);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_node_manager_capabilities (uint8_t domain_id,
                                                               uint8_t policy_trigger_type,
                                                               uint8_t policy_type,
                                                               fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_VALID (policy_trigger_type)
      || !IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_VALID (policy_type)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_CAPABILITIES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_trigger_type", policy_trigger_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "policy_type", policy_type);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_node_manager_version (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_VERSION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_set_node_manager_power_draw_range (uint8_t domain_id,
                                                                   uint16_t minimum_power_draw,
                                                                   uint16_t maximum_power_draw,
                                                                   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POWER_DRAW_RANGE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "minimum_power_draw", minimum_power_draw);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "maximum_power_draw", maximum_power_draw);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination (uint8_t channel_number,
                                                                    uint8_t destination_information_operation,
                                                                    uint8_t destination_information,
                                                                    uint8_t alert_string_selector,
                                                                    uint8_t send_alert_string,
                                                                    fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_VALID (destination_information_operation)
      || !IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_VALID (alert_string_selector)
      || !IPMI_OEM_INTEL_NODE_MANAGER_SEND_ALERT_STRING_VALID (send_alert_string)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_DESTINATION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "destination_information_operation", destination_information_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "destination_information", destination_information);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "alert_string_selector", alert_string_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "send_alert_string", send_alert_string);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb (uint8_t channel_number,
                                                                         uint8_t destination_information_operation,
                                                                         uint8_t slave_address,
                                                                         uint8_t alert_string_selector,
                                                                         uint8_t send_alert_string,
                                                                         fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_VALID (destination_information_operation)
      || !IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_VALID (alert_string_selector)
      || !IPMI_OEM_INTEL_NODE_MANAGER_SEND_ALERT_STRING_VALID (send_alert_string)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_DESTINATION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "destination_information_operation", destination_information_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "slave_address", slave_address);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "alert_string_selector", alert_string_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "send_alert_string", send_alert_string);

  return (0);
}
  
int
fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan (uint8_t channel_number,
                                                                        uint8_t destination_information_operation,
                                                                        uint8_t destination_selector,
                                                                        uint8_t alert_string_selector,
                                                                        uint8_t send_alert_string,
                                                                        fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_VALID (destination_information_operation)
      || !IPMI_OEM_INTEL_NODE_MANAGER_ALERT_STRING_SELECTOR_VALID (alert_string_selector)
      || !IPMI_OEM_INTEL_NODE_MANAGER_SEND_ALERT_STRING_VALID (send_alert_string)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_DESTINATION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "destination_information_operation", destination_information_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "destination_selector", destination_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "alert_string_selector", alert_string_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "send_alert_string", send_alert_string);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_node_manager_alert_destination (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_ALERT_DESTINATION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);

  return (0);
}

int
fill_cmd_oem_intel_node_manager_get_limiting_policy_id (uint8_t domain_id,
							fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_oem_intel_node_manager_get_limiting_policy_id_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_LIMITING_POLICY_ID);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "manufacturer_id", IPMI_IANA_ENTERPRISE_ID_INTEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "domain_id", domain_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}
