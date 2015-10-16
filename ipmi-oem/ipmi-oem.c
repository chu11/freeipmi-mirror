/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-dell.h"
#include "ipmi-oem-fujitsu.h"
#include "ipmi-oem-ibm.h"
#include "ipmi-oem-intel.h"
#include "ipmi-oem-intelnm.h"
#include "ipmi-oem-inventec.h"
#include "ipmi-oem-quanta.h"
#include "ipmi-oem-sun.h"
#include "ipmi-oem-supermicro.h"
#include "ipmi-oem-wistron.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-hostrange-common.h"
#include "tool-util-common.h"

typedef int (*oem_callback)(ipmi_oem_state_data_t *);

#define IPMI_OEM_COMMAND_FLAGS_DEFAULT                0x0000
#define IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE 0x0001
#define IPMI_OEM_COMMAND_FLAGS_HIDDEN                 0x0100

struct ipmi_oem_command
{
  char *oem_command;
  char *command_options;
  int required_oem_options;
  unsigned int flags;
  oem_callback func;
};

struct ipmi_oem_id
{
  char *oem_id;
  struct ipmi_oem_command *oem_commands;
};

struct ipmi_oem_command oem_dell[] =
  {
    {
      "get-system-info",
      "<KEY>",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_get_system_info
    },
    {
      "get-nic-selection",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_nic_selection
    },
    {
      "set-nic-selection",
      "<dedicated|shared|shared_failover_nic2|shared_failover_all>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_set_nic_selection
    },
    {
      "get-nic-selection-failover",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_nic_selection_failover
    },
    {
      "set-nic-selection-failover",
      "<dedicated|lom1|lom2|lom3|lom4> <none|lom1|lom2|lom3|lom4|all>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_set_nic_selection_failover
    },
    {
      "get-active-lom-status",
      "[v1|v2]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_get_active_lom_status
    },
    {
      "get-ssh-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_ssh_config
    },
    {
      "set-ssh-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_set_ssh_config
    },
    {
      "get-telnet-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_telnet_config
    },
    {
      "set-telnet-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_set_telnet_config
    },
    {
      "get-web-server-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_web_server_config
    },
    {
      "set-web-server-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_set_web_server_config
    },
    {
      "get-active-directory-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_active_directory_config
    },
    {
      "set-active-directory-config",
      "KEY=VALUE ...",
      1,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_set_active_directory_config
    },
    {
      "reset-to-defaults",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_reset_to_defaults
    },
    {
      "get-power-consumption-data",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_power_consumption_data
    },
    /* legacy */
    {
      "get-power-info",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_dell_get_power_consumption_data
    },
    /* legacy */
    {
      "reset-power-info",
      "<cumulative|peak>",
      1,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_dell_reset_power_consumption_data
    },
    {
      "reset-power-consumption-data",
      "<cumulative|peak>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_reset_power_consumption_data
    },
    /* legacy */
    {
      "get-power-supply-info",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_dell_power_supply_info
    },
    {
      "power-supply-info",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_power_supply_info
    },
    /* legacy */
    {
      "get-instantaneous-power-consumption-info",
      "[power_supply_instance]",
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN | IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_get_instantaneous_power_consumption_data
    },
    {
      "get-instantaneous-power-consumption-data",
      "[power_supply_instance]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_get_instantaneous_power_consumption_data
    },
    /* legacy */
    {
      "get-power-headroom-info",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN | IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_dell_get_power_head_room
    },
    {
      "get-power-head-room",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_power_head_room
    },
    {
      "get-power-consumption-statistics",
      "<average|max|min>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_power_consumption_statistics
    },
    /* legacy */
    {
      "get-average-power-history",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_dell_get_average_power_history
    },
    /* legacy */
    {
      "get-peak-power-history",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_dell_get_peak_power_history
    },
    {
      "get-power-capacity",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_power_capacity
    },
    {
      "set-power-capacity",
      "<power-capacity>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_set_power_capacity
    },
    {
      "get-power-capacity-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_power_capacity_status
    },
    {
      "set-power-capacity-status",
      "<enable|disable>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_set_power_capacity_status
    },
    {
      "get-chassis-identify-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_chassis_identify_status,
    },
    /* legacy */
    {
      "get-board-id",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_get_board_id
    },
    /* legacy */
    {
      "set-board-id",
      "<boardid>",
      1,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_set_board_id
    },
    /* legacy */
    {
      "get-fcb-version",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_get_fcb_version
    },
    /* legacy */
    {
      "set-fcb-version",
      "<majorversion> <minorversion>",
      2,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_set_fcb_version
    },
    /* legacy */
    {
      "get-sol-inactivity-timeout",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_get_sol_inactivity_timeout
    },
    /* legacy */
    {
      "set-sol-inactivity-timeout",
      "<inactivity-timeout>",
      1,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_set_sol_inactivity_timeout
    },
    {
      "power-monitoring-over-interval",
      "<interval> <systempower|cpu1|cpu2|cpu3|cpu4|memory1|memory2|memory3|memory4|drives|fans|pciecards|gpucables>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_power_monitoring_over_interval
    },
    {
      "power-monitoring-interval-range",
      "<systempower|cpu1|cpu2|cpu3|cpu4|memory1|memory2|memory3|memory4|drives|fans|pciecards|gpucables>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_power_monitoring_interval_range
    },
#if 0
/* cannot verify */
    {
      "get-blade-slot-id",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_blade_slot_id
    },
#endif
    { 
      "get-last-post-code",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_last_post_code
    },
    {
      "slot-power-toggle",
      "<slot-number>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_slot_power_toggle,
    },
    {
      "slot-power-control",
      "<platform-model> <on|off|status> <slot-number>",
      3,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_slot_power_control,
    },
    {
      "get-port-map",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_get_port_map,
    },
    {
      "set-port-map",
      "<jumper|bmc> <ipass-mapping> <1:2|1:4|1:8>",
      3,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_dell_set_port_map,
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_fujitsu[] =
  {
    {
      "get-power-on-source",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_power_on_source
    },
    {
      "get-power-off-source",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_power_off_source
    },
    {
      "get-remote-storage-status",
      "<connection_number>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_remote_storage_status
    },
    {
      "get-system-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_system_status
    },
    {
      "get-eeprom-version-info",
      "<eeprom_number>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_eeprom_version_info
    },
    {
      "get-identify-led",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_identify_led
    },
    {
      "set-identify-led",
      "<on|off>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_set_identify_led
    },
    {
      "get-error-led",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_error_led
    },
    {
      "get-sel-entry-long-text",
      "<sel_record_id>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_fujitsu_get_sel_entry_long_text
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_ibm[] =
  {
    {
      "get-led",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_ibm_get_led
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_intel[] =
  {
    {
      "get-smtp-config",
      "[channel-number]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intel_get_smtp_config
    },
    {
      "set-smtp-config",
      "[channel-number] KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intel_set_smtp_config
    },
    {
      "get-power-restore-delay",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intel_get_power_restore_delay
    },
    {
      "set-power-restore-delay",
      "<seconds>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intel_set_power_restore_delay
    },
#if 0
    {
      "get-bmc-service-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intel_get_bmc_service_status
    },
    {
      "set-bmc-service-status",
      "<enable|disable> <ssh|http|kvm>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intel_set_bmc_service_status
    },
#endif
    {
      "restore-configuration",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intel_restore_configuration
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_intelnm[] =
  {
    {
      "get-node-manager-statistics",
      "mode=globalpower|globaltemp|policypower|policytrigger|policythrottling|hostunhandledrequests|hostresponsetime|cputhrottling|memorythrottling|hostcommfailure [domainid=platform|cpu|memory|highpowerio] [policyid=num]",
      1,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_get_node_manager_statistics
    },
    {
      "reset-node-manager-statistics",
      "[mode=global|policy|hostunhandledrequests|hostresponsetime|cputhrottling|memorythrottling|hostcommfailure] [domainid=platform|cpu|memory|highpowerio] [policyid=num]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_reset_node_manager_statistics
    },
    {
      "get-node-manager-capabilities",
      "[domainid=platform|cpu|memory|highpowerio] [policytrigger=none|inlettemperaturelimitpolicytrigger|missingpowerreadingtimeout|timeafterplatformresettrigger|boottimepolicy] [policytype=powercontrol]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_get_node_manager_capabilities
    },
    {
      "node-manager-policy-control",
      "<enable|disable> [domainid=platform|cpu|memory|highpowerio] [policyid=num]",
      1,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_node_manager_policy_control
    },
    {
      "get-node-manager-policy",
      "[domainid=platform|cpu|memory|highpowerio] [policyid=num]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_get_node_manager_policy
    },
    {
      "set-node-manager-policy",
      "domainid=platform|cpu|memory|highpowerio policyid=num policytrigger=none|inlettemperaturelimitpolicytrigger|missingpowerreadingtimeout|timeafterplatformresettrigger|boottimepolicy policytargetlimit=num [platformbootingmode=performance|power] correctiontimelimit=ms statisticsreportingperiod=seconds policystorage=persistent|volatile [policytriggerlimit=num] [policystate=enable|disable] [policyexceptionaction=alert|shutdown] [aggressivepowercorrection=automatic|notaggressive|aggressive]",
      7,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_set_node_manager_policy
    },
    {
      "remove-node-manager-policy",
      "domainid=platform|cpu|memory|highpowerio policyid=num",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intelnm_remove_node_manager_policy
    },
    {
      "get-node-manager-alert-thresholds",
      "[domainid=platform|cpu|memory|highpowerio] [policyid=num]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_get_node_manager_alert_thresholds
    },
    {
      "set-node-manager-alert-thresholds",
      "domainid=platform|cpu|memory|highpowerio policyid=num [threshold1=num] [threshold2=num] [threshold3=num]",
      2,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_set_node_manager_alert_thresholds
    },
    {
      "get-node-manager-policy-suspend-periods",
      "[domainid=platform|cpu|memory|highpowerio] [policyid=num]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_get_node_manager_policy_suspend_periods
    },
    {
      "set-node-manager-policy-suspend-periods",
      "domainid=platform|cpu|memory|highpowerio policyid=num suspendperiodstartX=time suspendperiodstopX=time suspendperiodrepeatX=monday|tuesday|wednesday|thursday|friday|saturday|sunday",
      2,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_set_node_manager_policy_suspend_periods
    },
    {
      "set-node-manager-power-draw-range",
      "domainid=platform|cpu|memory|highpowerio minpowerdrawrange=watts maxpowerdrawrange=watts",
      3,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intelnm_set_node_manager_power_draw_range
    },
#if 0
    {
      "get-limiting-policy-id",
      "domainid=platform|cpu|memory",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intelnm_get_limiting_policy_id
    },
#endif
    /* can't verify */
    {
      "get-node-manager-alert-destination",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intelnm_get_node_manager_alert_destination
    },
    {
      "set-node-manager-alert-destination",
      "[channelnumber=num] [slaveaddress=address] [destinationselector=num] [alertstringselector=num] [sendalertstring=yes|no] [register=yes|no]",
      1,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_intelnm_set_node_manager_alert_destination
    },
    {
      "get-node-manager-version",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_intelnm_get_node_manager_version
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_inventec[] =
  {
    /* legacy */
    {
      "get-nic-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_get_nic_mode
    },
    /* legacy */
    {
      "set-nic-status",
      "<dedicated|shared>",
      1,
      IPMI_OEM_COMMAND_FLAGS_HIDDEN,
      ipmi_oem_inventec_set_nic_mode
    },
    {
      "get-nic-mode",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_nic_mode
    },
    {
      "set-nic-mode",
      "<dedicated|shared>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_nic_mode
    },
    {
      "get-mac-address",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_mac_address
    },
    {
      "set-mac-address",
      "<dedicated|shared> <MACADDRESS>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_mac_address
    },
    {
      "get-bmc-services",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_bmc_services
    },
    {
      "set-bmc-services",
      "<enable|disable> <all|kvm|http|ssh>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_bmc_services
    },
    {
      "get-authentication-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_authentication_config
    },
    {
      "set-authentication-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_inventec_set_authentication_config
    },
    {
      "get-account-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_account_status
    },
    {
      "get-dns-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_dns_config
    },
    {
      "set-dns-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_inventec_set_dns_config
    },
    {
      "get-web-server-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_web_server_config
    },
    {
      "set-web-server-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_inventec_set_web_server_config
    },
    {
      "get-power-management-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_power_management_config
    },
    {
      "set-power-management-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_inventec_set_power_management_config
    },
    {
      "get-sol-idle-timeout",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_sol_idle_timeout
    },
    {
      "set-sol-idle-timeout",
      "<idle-timeout>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_sol_idle_timeout
    },
    {
      "get-telnet-ssh-redirect-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_telnet_ssh_redirect_status
    },
    {
      "set-telnet-ssh-redirect-status",
      "<enable|disable>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_telnet_ssh_redirect_status
    },
#if 0
    /* waiting for verification from Dell */
    {
      "get-firmware-update-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_firmware_update_config
    },
    {
      "set-firmware-update-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_inventec_set_firmware_update_config
    },
#endif
#if 0
    /* cannot verify */
    {
      "get-firmware-information",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_firmware_information
    },
#endif
#if 0
    /* waiting for verification from Dell */
    {
      "update-firmware",
      "<tftp|ftp|http> [config=preserve|nopreserve]",
      1,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_inventec_update_firmware
    },
#endif
    {
      "get-board-id",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_board_id
    },
    {
      "set-board-id",
      "<boardid>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_board_id
    },
    {
      "get-fcb-version",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_fcb_version
    },
    {
      "set-fcb-version",
      "<majorversion> <minorversion>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_fcb_version
    },
#if 0
    /* cannot verify */
    {
      "set-asset-tag",
      "<asset-tag>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_asset_tag
    },
#endif
#if 0
    /* cannot verify */
    {
      "get-dhcp-retry",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_dhcp_retry
    },
    {
      "set-dhcp-retry",
      "<retry-count> <retry-interval> <retry-timeout>",
      3,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_dhcp_retry
    },
#endif
    {
      "get-sol-inactivity-timeout",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_get_sol_inactivity_timeout
    },
    {
      "set-sol-inactivity-timeout",
      "<inactivity-timeout>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_sol_inactivity_timeout
    },
    {
      "restore-to-defaults",
      "<all|user|lan|sol|serial|pef>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_restore_to_defaults
    },
#if 0
    /* cannot verify */
    {
      "set-system-guid",
      "<system_guid>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_set_system_guid
    },
#endif
    {
      "read-eeprom",
      "<at24c256n>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_read_eeprom
    },
    {
      "clear-eeprom",
      "<at24c256n>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_inventec_clear_eeprom
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_sun[] =
  {
    {
      "get-led",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_sun_get_led
    },
    {
      "set-led",
      "<record_id> <off|on|standby|slow|fast>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_sun_set_led
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_quanta[] =
  {
    {
      "get-nic-mode",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_nic_mode
    },
    {
      "set-nic-mode",
      "<dedicated|shared>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_set_nic_mode
    },
    {
      "get-bmc-services",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_bmc_services
    },
    {
      "set-bmc-services",
      "<enable|disable> <all|kvm|http|ssh>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_set_bmc_services
    },
    {
      "get-account-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_account_status
    },
    {
      "get-dns-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_dns_config
    },
    {
      "set-dns-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_quanta_set_dns_config
    },
    {
      "get-web-server-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_web_server_config
    },
    {
      "set-web-server-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_quanta_set_web_server_config
    },
    {
      "get-power-management-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_power_management_config
    },
    {
      "set-power-management-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_quanta_set_power_management_config
    },
    {
      "get-sol-idle-timeout",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_sol_idle_timeout
    },
    {
      "set-sol-idle-timeout",
      "<idle-timeout>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_set_sol_idle_timeout
    },
    {
      "get-telnet-ssh-redirect-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_get_telnet_ssh_redirect_status
    },
    {
      "set-telnet-ssh-redirect-status",
      "<enable|disable>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_set_telnet_ssh_redirect_status
    },
    {
      "reset-to-defaults",
      "<all|user|lan|sol|serial|pef>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_reset_to_defaults
    },
    {
      "get-processor-information",
      "[processor-index]",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_quanta_get_processor_information
    },
    {
      "read-mac-address",
      "<s99q> <dedicated|shared>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_read_mac_address
    },
    {
      "write-mac-address",
      "<s99q> <dedicated|shared> <MACADDRESS>",
      3,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_quanta_write_mac_address
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_supermicro[] =
  {
    {
      "extra-firmware-info",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_supermicro_extra_firmware_info
    },
    {
      "reset-intrusion",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_supermicro_reset_intrusion
    },
    {
      "get-bmc-services-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_supermicro_get_bmc_services_status
    },
    {
      "set-bmc-services-status",
      "<enable|disable>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_supermicro_set_bmc_services_status
    },
    {
      "get-power-supply-status",
      "<ps_num>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_supermicro_get_power_supply_status
    },
    {
      "get-power-supply-status2",
      "<ps_num>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_supermicro_get_power_supply_status2
    },
    {
      "get-pmbus-power-supply-status",
      "<ps_num>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_supermicro_get_pmbus_power_supply_status
    },
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_command oem_wistron[] =
  {
    {
      "get-system-info",
      "<KEY>",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_wistron_get_system_info
    },
    {
      "get-nic-mode",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_nic_mode
    },
    {
      "set-nic-mode",
      "<dedicated|shared>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_nic_mode
    },
    {
      "get-shared-nic-selection",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_shared_nic_selection
    },
    {
      "set-shared-nic-selection",
      "<nic1|nic2|nic3|nic4|clear>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_shared_nic_selection
    },
    {
      "get-bmc-services",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_bmc_services
    },
    {
      "set-bmc-services",
      "<enable|disable> <all|kvm|http|ssh|snmp|telnet>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_bmc_services
    },
    {
      "get-account-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_account_status
    },
    {
      "get-dns-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_dns_config
    },
    {
      "set-dns-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_wistron_set_dns_config
    },
    {
      "get-web-server-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_web_server_config
    },
    {
      "set-web-server-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_wistron_set_web_server_config
    },
#if 0
/* can't verify - doesn't appear to work */
    {
      "get-server-services-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_server_services_config
    },
    {
      "set-server-services-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_wistron_set_server_services_config
    },
#endif
    {
      "get-power-management-config",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_power_management_config
    },
    {
      "set-power-management-config",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_wistron_set_power_management_config
    },
#if 0
/* can't verify - doesn't appear to work */
    {
      "get-firmware-information",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_firmware_information
    },
#endif
#if 0
/* can't verify - doesn't appear to work */
    {
      "user-default-setting",
      "<set|reset>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_user_default_setting
    },
#endif
    {
      "get-ipv6-settings",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_ipv6_settings
    },
    {
      "set-ipv6-settings",
      "KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_wistron_set_ipv6_settings
    },
    {
      "get-ipv6-trap-settings",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_ipv6_trap_settings
    },
    {
      "set-ipv6-trap-settings",
      "index KEY=VALUE ...",
      0,
      IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE,
      ipmi_oem_wistron_set_ipv6_trap_settings
    },
    {
      "get-sol-idle-timeout",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_sol_idle_timeout
    },
    {
      "set-sol-idle-timeout",
      "<idle-timeout>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_sol_idle_timeout
    },
    {
      "get-telnet-redirect-function",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_telnet_redirect_function
    },
    {
      "set-telnet-redirect-function",
      "<disable|solenable|smashenable>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_telnet_redirect_function
    },
#if 0
/* can't verify - doesn't appear to work */
    {
      "get-ssh-redirect-function",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_ssh_redirect_function
    },
    {
      "set-ssh-redirect-function",
      "<disable|solenable|smashenable>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_ssh_redirect_function
    },
#endif
#if 0
/* can't verify - doesn't appear to work */
    {
      "get-chassis-power-readings",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_chassis_power_readings
    },
#endif
#if 0
/* can't verify - doesn't appear to work */
    {
      "get-chassis-led-status",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_chassis_led_status
    },
    {
      "set-chassis-led-status",
      "<identify-off|identify-solid|identify-blink> <fault-off|fault-solid|fault-blink>",
      2,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_chassis_led_status
    },
#endif
    {
      "get-dhcp-retry",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_dhcp_retry
    },
    {
      "set-dhcp-retry",
      "<retry-count> <retry-interval> <retry-timeout>",
      3,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_dhcp_retry
    },
    {
      "get-link-status-change-control",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_get_link_status_change_control
    },
    {
      "set-link-status-change-control",
      "<link_resilience|dhcp_rediscovery>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_link_status_change_control
    },
    {
      "set-password-policy",
      "<enable|disable>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_set_password_policy
    },
    {
      "read-proprietary-string",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_read_proprietary_string
    },
    {
      "write-proprietary-string",
      "<string>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_write_proprietary_string
    },
    {
      "clear-proprietary-string",
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_clear_proprietary_string
    },
#if 0
/* can't verify - doesn't appear to work */
    {
      "reset-to-defaults",
      "<all|user|lan|sol|serial|pef>",
      1,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      ipmi_oem_wistron_reset_to_defaults
    },
#endif
    {
      NULL,
      NULL,
      0,
      IPMI_OEM_COMMAND_FLAGS_DEFAULT,
      NULL
    },
  };

struct ipmi_oem_id oem_cb[] =
  {
    {
      "Dell",
      oem_dell
    },
    {
      "Fujitsu",
      oem_fujitsu
    },
    {
      "IBM",
      oem_ibm
    },
    {
      "Intel",
      oem_intel
    },
    {
      "Intelnm",
      oem_intelnm
    },
    {
      "Inventec",
      oem_inventec
    },
    {
      "Quanta",
      oem_quanta
    },
    {
      "Sun",
      oem_sun
    },
    {
      "Supermicro",
      oem_supermicro
    },
    {
      "Wistron",
      oem_wistron
    },
    {
      NULL,
      NULL
    },
  };

static int
_list (void)
{
  struct ipmi_oem_id *oem_id = oem_cb;

  while (oem_id && oem_id->oem_id)
    {
      struct ipmi_oem_command *oem_cmd = oem_id->oem_commands;

      printf ("OEM ID: %s\n", oem_id->oem_id);

      while (oem_cmd && oem_cmd->oem_command)
        {
          if (!(oem_cmd->flags & IPMI_OEM_COMMAND_FLAGS_HIDDEN))
            {
              if (oem_cmd->command_options)
                printf ("    Command: %s %s\n",
                        oem_cmd->oem_command,
                        oem_cmd->command_options);
              else
                printf ("    Command: %s\n",
                        oem_cmd->oem_command);
            }
          oem_cmd++;
        }

      printf ("\n");
      oem_id++;
    }

  return (0);
}

static int
_run_oem_cmd (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_arguments *args;
  struct ipmi_oem_id *oem_id = oem_cb;
  int id_found = 0;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  while (oem_id && oem_id->oem_id)
    {
      if (!strcasecmp (oem_id->oem_id, args->oem_id))
        {
          struct ipmi_oem_command *oem_cmd = oem_id->oem_commands;
          int cmd_found = 0;

          id_found++;

	  /* offer "help" as well as list, for those used to ipmitool */
          if (!args->oem_command
              || !strcasecmp (args->oem_command, "list")
	      || !strcasecmp (args->oem_command, "help"))
            {
              while (oem_cmd && oem_cmd->oem_command)
                {
                  if (!(oem_cmd->flags & IPMI_OEM_COMMAND_FLAGS_HIDDEN))
                    {
                      if (oem_cmd->command_options)
                        printf ("%s Command: %s %s\n",
                                oem_id->oem_id,
                                oem_cmd->oem_command,
                                oem_cmd->command_options);
                      else
                        printf ("%s Command: %s\n",
                                oem_id->oem_id,
                                oem_cmd->oem_command);
                    }
                  oem_cmd++;
                }

              break;
            }

          while (oem_cmd && oem_cmd->oem_command)
            {
              if (!strcasecmp (oem_cmd->oem_command,
                               args->oem_command))
                {
                  cmd_found++;

                  if ((oem_cmd->flags & IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE
		       && state_data->prog_data->args->oem_options_count < oem_cmd->required_oem_options)
		      || (!(oem_cmd->flags & IPMI_OEM_COMMAND_FLAGS_OPTIONS_COUNT_VARIABLE)
			  && state_data->prog_data->args->oem_options_count != oem_cmd->required_oem_options))
                    {
                      pstdout_fprintf (state_data->pstate,
                                       stderr,
                                       "%s:%s invalid number of OEM option arguments\n",
                                       state_data->prog_data->args->oem_id,
                                       state_data->prog_data->args->oem_command);
                      goto cleanup;
                    }
                  

                  if (((*oem_cmd->func)(state_data)) < 0)
                    goto cleanup;

                  break;
                }

              oem_cmd++;
            }

          if (!cmd_found)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "OEM Command '%s' unknown\n",
                               args->oem_command);
              goto cleanup;
            }

          break;
        }

      oem_id++;
    }

  if (!id_found)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Id '%s' unknown\n",
                       args->oem_id);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
run_cmd_args (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_arguments *args;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  /* shouldn't be possible at this point, make sure we've already
   * exitted
   */
  assert (!args->list);
  assert (args->oem_id);
  assert (strcasecmp (args->oem_id, "list"));
  assert (strcasecmp (args->oem_id, "help"));
  assert (!args->common_args.flush_cache);

  if (_run_oem_cmd (state_data) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_oem_state_data_t state_data;
  ipmi_oem_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_oem_prog_data_t *)arg;

  if (prog_data->args->common_args.flush_cache)
    {
      if (sdr_cache_flush_cache (pstate,
                                 hostname,
                                 &prog_data->args->common_args) < 0)
	return (EXIT_FAILURE);
      return (EXIT_SUCCESS);
    }

  memset (&state_data, '\0', sizeof (ipmi_oem_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* Special case, we're going to output help info, don't do an IPMI connection */
  if (prog_data->args->oem_command)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
					     hostname,
					     &(prog_data->args->common_args),
					     state_data.pstate)))
	goto cleanup;
    }

  if (!(state_data.sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_ctx_create()");
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    goto cleanup;
  
  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_oem_prog_data_t prog_data;
  struct ipmi_oem_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_oem_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_oem_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  /* Special case, just output list, don't do anything else */
  /* offer "help" as well as list, for those used to ipmitool */
  if (!cmd_args.oem_id
      || !strcasecmp (cmd_args.oem_id, "list")
      || !strcasecmp (cmd_args.oem_id, "help")
      || cmd_args.list)
    {
      if (_list () < 0)
	return (EXIT_FAILURE);
      return (EXIT_SUCCESS);
    }

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
				    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->common_args.quiet_cache = 1;

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _ipmi_oem,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
