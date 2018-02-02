/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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

#ifndef IPMI_LAN_CMDS_H
#define IPMI_LAN_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR               0x0
#define IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR             0x0

#define IPMI_LAN_CONFIGURATION_PARAMETERS_SET_COMPLETE                 0x00
#define IPMI_LAN_CONFIGURATION_PARAMETERS_SET_IN_PROGRESS              0x01
#define IPMI_LAN_CONFIGURATION_PARAMETERS_SET_COMMIT_WRITE             0x02

#define IPMI_LAN_CONFIGURATION_PARAMETERS_SET_IN_PROGRESS_VALID(__value) \
  (((__value) == IPMI_LAN_CONFIGURATION_PARAMETERS_SET_COMPLETE          \
    || (__value) == IPMI_LAN_CONFIGURATION_PARAMETERS_SET_IN_PROGRESS    \
    || (__value) == IPMI_LAN_CONFIGURATION_PARAMETERS_SET_COMMIT_WRITE) ? 1 : 0)

#define IPMI_AUTHENTICATION_TYPE_SUPPORTED             0x1
#define IPMI_AUTHENTICATION_TYPE_UNSUPPORTED           0x0

#define IPMI_AUTHENTICATION_TYPE_ENABLE                0x1
#define IPMI_AUTHENTICATION_TYPE_DISABLE               0x0

#define IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(__val) \
  (((__val) == IPMI_AUTHENTICATION_TYPE_ENABLE       \
    || (__val) == IPMI_AUTHENTICATION_TYPE_DISABLE) ? 1 : 0)

#define IPMI_IP_ADDRESS_SOURCE_UNSPECIFIED                0x0
#define IPMI_IP_ADDRESS_SOURCE_STATIC                     0x1
#define IPMI_IP_ADDRESS_SOURCE_DHCP                       0x2
#define IPMI_IP_ADDRESS_SOURCE_BIOS                       0x3
#define IPMI_IP_ADDRESS_SOURCE_OTHER                      0x4

#define IPMI_IP_ADDRESS_SOURCE_VALID(__val)       \
  (((__val) == IPMI_IP_ADDRESS_SOURCE_UNSPECIFIED \
    || (__val) == IPMI_IP_ADDRESS_SOURCE_STATIC   \
    || (__val) == IPMI_IP_ADDRESS_SOURCE_DHCP     \
    || (__val) == IPMI_IP_ADDRESS_SOURCE_BIOS     \
    || (__val) == IPMI_IP_ADDRESS_SOURCE_OTHER) ? 1 : 0)

#define IPMI_IPV6_IPV4_ADDRESSING_ENABLES_IPV6_DISABLED 0x0
#define IPMI_IPV6_IPV4_ADDRESSING_ENABLES_IPV6_ONLY     0x1
#define IPMI_IPV6_IPV4_ADDRESSING_ENABLES_IPV6_AND_IPV4 0x2

#define IPMI_IPV6_IPV4_ADDRESSING_ENABLES_VALID(__val)       \
  (((__val) == IPMI_IPV6_IPV4_ADDRESSING_ENABLES_IPV6_DISABLED \
    || (__val) == IPMI_IPV6_IPV4_ADDRESSING_ENABLES_IPV6_ONLY   \
    || (__val) == IPMI_IPV6_IPV4_ADDRESSING_ENABLES_IPV6_AND_IPV4) ? 1 : 0)

#define IPMI_BMC_GENERATED_GRATUITOUS_ARPS_ENABLE     0x1
#define IPMI_BMC_GENERATED_GRATUITOUS_ARPS_DISABLE    0x0

#define IPMI_BMC_GENERATED_GRATUITOUS_ARPS_VALID(__val)  \
  (((__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARPS_ENABLE \
    || (__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARPS_DISABLE) ? 1 : 0)

#define IPMI_BMC_GENERATED_ARP_RESPONSES_ENABLE       0x1
#define IPMI_BMC_GENERATED_ARP_RESPONSES_DISABLE      0x0

#define IPMI_BMC_GENERATED_ARP_RESPONSES_VALID(__val)  \
  (((__val) == IPMI_BMC_GENERATED_ARP_RESPONSES_ENABLE \
    || (__val) == IPMI_BMC_GENERATED_ARP_RESPONSES_DISABLE) ? 1 : 0)

#define IPMI_MAX_COMMUNITY_STRING_LENGTH              18

#define IPMI_DESTINATION_SELECTOR_MIN                 0
#define IPMI_DESTINATION_SELECTOR_MAX                 15

/* To avoid gcc warnings, add +1 in comparison */
#define IPMI_DESTINATION_SELECTOR_VALID(__val)         \
  (((__val + 1) >= (IPMI_DESTINATION_SELECTOR_MIN + 1) \
    || (__val) <= IPMI_DESTINATION_SELECTOR_MAX) ? 1 : 0)


#define IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION      0x0
#define IPMI_DESTINATION_TYPE_OEM1                      0x6
#define IPMI_DESTINATION_TYPE_OEM2                      0x7

#define IPMI_DESTINATION_TYPE_VALID(__val)                \
  (((__val) == IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION \
    || (__val) == IPMI_DESTINATION_TYPE_OEM1              \
    || (__val) == IPMI_DESTINATION_TYPE_OEM2) ? 1 : 0)

#define IPMI_ALERT_UNACKNOWLEDGED                       0
#define IPMI_ALERT_ACKNOWLEDGED                         1

#define IPMI_ALERT_VALID(__val)           \
  (((__val) == IPMI_ALERT_UNACKNOWLEDGED \
    || (__val) == IPMI_ALERT_ACKNOWLEDGED) ? 1 : 0)

#define IPMI_ADDRESS_FORMAT_IPV4                        0

#define IPMI_GATEWAY_SELECTOR_DEFAULT                   0
#define IPMI_GATEWAY_SELECTOR_BACKUP                    1

#define IPMI_GATEWAY_SELECTOR_VALID(__val)    \
  (((__val) == IPMI_GATEWAY_SELECTOR_DEFAULT \
    || (__val) == IPMI_GATEWAY_SELECTOR_BACKUP) ? 1 : 0)

/* 3 bit number */
#define IPMI_ALERT_RETRIES_MAX                          7

#define IPMI_VLAN_ID_ENABLE                             0x1
#define IPMI_VLAN_ID_DISABLE                            0x0

#define IPMI_VLAN_ID_ENABLE_VALID(__val) \
  (((__val) == IPMI_VLAN_ID_ENABLE      \
    || (__val) == IPMI_VLAN_ID_DISABLE) ? 1 : 0)

#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_DO_NOT_SUSPEND    0x0
#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPEND           0x1

#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(__val)           \
  (((__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARP_DO_NOT_SUSPEND \
    || (__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPEND) ? 1 : 0)

#define IPMI_BMC_GENERATED_ARP_RESPONSE_DO_NOT_SUSPEND      0x0
#define IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPEND             0x1

#define IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(__val)           \
  (((__val) == IPMI_BMC_GENERATED_ARP_RESPONSE_DO_NOT_SUSPEND \
    || (__val) == IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPEND) ? 1 : 0)

#define IPMI_USER_DISABLED_EVENT_MESSAGE_ON             0x1
#define IPMI_USER_DISABLED_EVENT_MESSAGE_OFF            0x0

#define IPMI_USER_DISABLED_EVENT_MESSAGE_VALID(__val) \
  (((__val) == IPMI_USER_DISABLED_EVENT_MESSAGE_ON \
    || (__val) == IPMI_USER_DISABLED_EVENT_MESSAGE_OFF) ? 1 : 0)

#define IPMI_GET_LAN_PARAMETER                          0x0
#define IPMI_GET_LAN_PARAMETER_REVISION_ONLY            0x1

#define IPMI_GET_LAN_PARAMETER_VALID(__val) \
  (((__val) == IPMI_GET_LAN_PARAMETER       \
    || (__val) == IPMI_GET_LAN_PARAMETER_REVISION_ONLY) ? 1 : 0)

#define IPMI_CLEAR_ALL_STATISTICS                       0x1
#define IPMI_DONT_CLEAR_ALL_STATISTICS                  0x0

#define IPMI_IPV6_BYTES 16

#define IPMI_IPV6_PREFIX_LENGTH_MAX 128

#define IPMI_CLEAR_ALL_STATISTICS_VALID(__val) \
  (((__val) == IPMI_CLEAR_ALL_STATISTICS       \
    || (__val) == IPMI_DONT_CLEAR_ALL_STATISTICS) ? 1 : 0)

#define IPMI_IPV6_ADDRESS_SOURCE_STATIC      0x00
#define IPMI_IPV6_ADDRESS_SOURCE_SLAAC       0x01
#define IPMI_IPV6_ADDRESS_SOURCE_DHCPV6      0x02

#define IPMI_IPV6_STATIC_ADDRESS_SOURCE_VALID(__address_status) \
  (((__address_status) == IPMI_IPV6_ADDRESS_SOURCE_STATIC) ? 1 : 0)

#define IPMI_IPV6_DYNAMIC_ADDRESS_SOURCE_VALID(__address_status) \
  (((__address_status) == IPMI_IPV6_ADDRESS_SOURCE_SLAAC         \
  || (__address_status) == IPMI_IPV6_ADDRESS_SOURCE_DHCPV6) ? 1 : 0)

#define IPMI_IPV6_ADDRESS_SOURCE_ENABLE      0x01
#define IPMI_IPV6_ADDRESS_SOURCE_DISABLE     0x00

#define IPMI_IPV6_ADDRESS_SOURCE_ENABLE_VALID(__val) \
  (((__val) == IPMI_IPV6_ADDRESS_SOURCE_ENABLE       \
    || (__val) == IPMI_IPV6_ADDRESS_SOURCE_DISABLE) ? 1 : 0)

#define IPMI_IPV6_ADDRESS_STATUS_ACTIVE      0x00
#define IPMI_IPV6_ADDRESS_STATUS_DISABLED    0x01
#define IPMI_IPV6_ADDRESS_STATUS_PENDING     0x02
#define IPMI_IPV6_ADDRESS_STATUS_FAILED      0x03
#define IPMI_IPV6_ADDRESS_STATUS_DEPRECATED  0x04
#define IPMI_IPV6_ADDRESS_STATUS_INVALID     0x05

#define IPMI_IPV6_ADDRESS_STATUS_VALID(__address_status)           \
  (((__address_status) == IPMI_IPV6_ADDRESS_STATUS_ACTIVE          \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_DISABLED     \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_PENDING      \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_FAILED       \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_DEPRECATED   \
    || (__address_status) == IPMI_IPV6_ADDRESS_STATUS_INVALID) ? 1 : 0)

#define IPMI_IPV6_STATIC_ROUTER_ADDRESS_ENABLE           0x1
#define IPMI_IPV6_STATIC_ROUTER_ADDRESS_DISABLE          0x0

#define IPMI_IPV6_STATIC_ROUTER_ADDRESS_ENABLE_VALID(__val) \
  (((__val) == IPMI_IPV6_STATIC_ROUTER_ADDRESS_ENABLE       \
    || (__val) == IPMI_IPV6_STATIC_ROUTER_ADDRESS_DISABLE) ? 1 : 0)

#define IPMI_IPV6_DYNAMIC_ROUTER_ADDRESS_ENABLE          0x1
#define IPMI_IPV6_DYNAMIC_ROUTER_ADDRESS_DISABLE         0x0

#define IPMI_IPV6_DYNAMIC_ROUTER_ADDRESS_ENABLE_VALID(__val) \
  (((__val) == IPMI_IPV6_DYNAMIC_ROUTER_ADDRESS_ENABLE       \
    || (__val) == IPMI_IPV6_DYNAMIC_ROUTER_ADDRESS_DISABLE) ? 1 : 0)

/*
 * fill* functions return 0 on success, -1 on error.
 *
 * obj_cmd_rq must be for the fill function's respective fiid
 * template request.
 *
 * see freeipmi/templates/ for template definitions
 */

extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rs;

extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_set_in_progress_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_authentication_type_enables_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ip_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ip_address_source_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_mac_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_subnet_mask_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv4_header_parameters_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_primary_rmcp_port_number_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_secondary_rmcp_port_number_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_bmc_generated_arp_control_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_gratuitous_arp_interval_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_default_gateway_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_default_gateway_mac_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_backup_gateway_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_backup_gateway_mac_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_community_string_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_destination_type_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_destination_addresses_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_vlan_id_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_vlan_priority_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_bad_password_threshold_rq;

extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_ipv4_addressing_enables_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_header_static_traffic_class_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_header_static_hop_limit_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_header_flow_label_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_addresses_rq;
/* TODO extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_dhcpv6_static_duids_rq; */
/* TODO extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_dhcpv6_dynamic_duids_rq; */
/* TODO extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_dhcpv6_timing_and_configuration_rq; */
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_router_address_configuration_control_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_1_ip_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_1_mac_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_1_prefix_length_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_1_prefix_value_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_2_ip_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_2_mac_address_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_2_prefix_length_rq;
extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_static_router_2_prefix_value_rq;
/* TODO extern fiid_template_t tmpl_cmd_set_lan_configuration_parameters_ipv6_neighbor_discovery_slaac_timing_configuration_rq; */

extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rq;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rs;

extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_set_in_progress_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_authentication_type_support_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ip_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_mac_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv4_header_parameters_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_primary_rmcp_port_number_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_secondary_rmcp_port_number_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_community_string_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_destination_type_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_bad_password_threshold_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_ipv4_support_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_ipv4_addressing_enables_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_header_static_traffic_class_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_header_static_hop_limit_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_header_flow_label_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_status_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_addresses_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dhcpv6_static_duid_storage_length_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dhcpv6_static_duids_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dhcpv6_dynamic_duid_storage_length_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dhcpv6_dynamic_duids_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dhcpv6_timing_configuration_support_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dhcpv6_timing_and_configuration_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_router_address_configuration_control_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_ip_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_mac_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_prefix_length_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_prefix_value_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_ip_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_mac_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_prefix_length_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_prefix_value_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_number_of_dynamic_router_info_sets_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_router_info_ip_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_router_info_mac_address_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_router_info_prefix_length_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_router_info_prefix_value_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_router_received_hop_limit_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_neighbor_discovery_slaac_timing_configuration_support_rs;
extern fiid_template_t tmpl_cmd_get_lan_configuration_parameters_ipv6_neighbor_discovery_slaac_timing_configuration_rs;

extern fiid_template_t tmpl_cmd_suspend_bmc_arps_rq;
extern fiid_template_t tmpl_cmd_suspend_bmc_arps_rs;

extern fiid_template_t tmpl_cmd_get_ip_udp_rmcp_statistics_rq;
extern fiid_template_t tmpl_cmd_get_ip_udp_rmcp_statistics_rs;

int fill_cmd_set_lan_configuration_parameters (uint8_t channel_number,
                                               uint8_t parameter_selector,
                                               const void *configuration_parameter_data,
                                               unsigned int configuration_parameter_data_len,
                                               fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_set_in_progress (uint8_t channel_number,
                                                               uint8_t state,
                                                               fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_authentication_type_enables (uint8_t channel_number,
                                                                           uint8_t callback_level_none,
                                                                           uint8_t callback_level_md2,
                                                                           uint8_t callback_level_md5,
                                                                           uint8_t callback_level_straight_password,
                                                                           uint8_t callback_level_oem_proprietary,
                                                                           uint8_t user_level_none,
                                                                           uint8_t user_level_md2,
                                                                           uint8_t user_level_md5,
                                                                           uint8_t user_level_straight_password,
                                                                           uint8_t user_level_oem_proprietary,
                                                                           uint8_t operator_level_none,
                                                                           uint8_t operator_level_md2,
                                                                           uint8_t operator_level_md5,
                                                                           uint8_t operator_level_straight_password,
                                                                           uint8_t operator_level_oem_proprietary,
                                                                           uint8_t admin_level_none,
                                                                           uint8_t admin_level_md2,
                                                                           uint8_t admin_level_md5,
                                                                           uint8_t admin_level_straight_password,
                                                                           uint8_t admin_level_oem_proprietary,
                                                                           uint8_t oem_level_none,
                                                                           uint8_t oem_level_md2,
                                                                           uint8_t oem_level_md5,
                                                                           uint8_t oem_level_straight_password,
                                                                           uint8_t oem_level_oem_proprietary,
                                                                           fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ip_address (uint8_t channel_number,
                                                          uint32_t ip_address,
                                                          fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ip_address_source (uint8_t channel_number,
                                                                 uint8_t ip_address_source,
                                                                 fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_mac_address (uint8_t channel_number,
                                                           uint64_t mac_address,
                                                           fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_subnet_mask (uint8_t channel_number,
                                                           uint32_t subnet_mask,
                                                           fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv4_header_parameters (uint8_t channel_number,
                                                                      uint8_t time_to_live,
                                                                      uint8_t flags,
                                                                      uint8_t type_of_service,
                                                                      uint8_t precedence,
                                                                      fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_primary_rmcp_port_number (uint8_t channel_number,
                                                                        uint16_t primary_rmcp_port_number,
                                                                        fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_secondary_rmcp_port_number (uint8_t channel_number,
                                                                          uint16_t secondary_rmcp_port_number,
                                                                          fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (uint8_t channel_number,
                                                                         uint8_t bmc_generated_gratuitous_arps,
                                                                         uint8_t bmc_generated_arp_responses,
                                                                         fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_gratuitous_arp_interval (uint8_t channel_number,
                                                                       uint8_t gratuitous_arp_interval,
                                                                       fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_default_gateway_address (uint8_t channel_number,
                                                                       uint32_t ip_address,
                                                                       fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_default_gateway_mac_address (uint8_t channel_number,
                                                                           uint64_t mac_address,
                                                                           fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_backup_gateway_address (uint8_t channel_number,
                                                                      uint32_t ip_address,
                                                                      fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (uint8_t channel_number,
                                                                          uint64_t mac_address,
                                                                          fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_community_string (uint8_t channel_number,
                                                                const char *community_string,
                                                                unsigned int community_string_len,
                                                                fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_destination_type (uint8_t channel_number,
                                                                uint8_t destination_selector,
                                                                uint8_t destination_type,
                                                                uint8_t alert_acknowledge,
                                                                uint8_t alert_acknowledge_timeout,
                                                                uint8_t retries,
                                                                fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_destination_addresses (uint8_t channel_number,
                                                                     uint8_t destination_selector,
                                                                     uint8_t gateway_selector,
                                                                     uint32_t alerting_ip_address,
                                                                     uint64_t alerting_mac_address,
                                                                     fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_vlan_id (uint8_t channel_number,
                                                       uint16_t vlan_id,
                                                       uint8_t vlan_id_enable,
                                                       fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_vlan_priority (uint8_t channel_number,
                                                             uint8_t vlan_priority,
                                                             fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (uint8_t channel_number,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_1,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_2,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_3,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_4,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_5,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_6,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_7,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_8,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_9,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_10,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_11,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_12,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_13,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_14,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_15,
                                                                                                uint8_t maximum_privilege_for_cipher_suite_16,
                                                                                                fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_bad_password_threshold (uint8_t channel_number,
                                                                      uint8_t user_disabled_event_message,
                                                                      uint8_t bad_password_threshold_number,
                                                                      uint16_t attempt_count_reset_interval,
                                                                      uint16_t user_lockout_interval,
                                                                      fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_ipv4_addressing_enables (uint8_t channel_number,
                                                                            uint8_t enables,
                                                                            fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_header_static_traffic_class (uint8_t channel_number,
                                                                                uint8_t traffic_class,
                                                                                fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_header_static_hop_limit (uint8_t channel_number,
                                                                            uint8_t static_hop_limit,
                                                                            fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_header_flow_label (uint8_t channel_number,
                                                                      uint64_t flow_label,
                                                                      fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_addresses (uint8_t channel_number,
                                                                     uint8_t set_selector,
                                                                     uint8_t source,
                                                                     uint8_t enable,
                                                                     uint8_t address[IPMI_IPV6_BYTES],
                                                                     uint8_t address_prefix_length,
                                                                     uint8_t address_status,
                                                                     fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_router_address_configuration_control (uint8_t channel_number,
                                                                                         uint8_t enable_static_router_address,
                                                                                         uint8_t enable_dynamic_router_address,
                                                                                         fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_1_ip_address (uint8_t channel_number,
                                                                               uint8_t router_ip_address[IPMI_IPV6_BYTES],
                                                                               fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_1_mac_address (uint8_t channel_number,
                                                                                uint64_t router_mac_address,
                                                                                fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_1_prefix_length (uint8_t channel_number,
                                                                                  uint8_t prefix_length,
                                                                                  fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_1_prefix_value (uint8_t channel_number,
                                                                                 uint8_t prefix_value[IPMI_IPV6_BYTES],
                                                                                 fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_2_ip_address (uint8_t channel_number,
                                                                               uint8_t router_ip_address[IPMI_IPV6_BYTES],
                                                                               fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_2_mac_address (uint8_t channel_number,
                                                                                uint64_t router_mac_address,
                                                                                fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_2_prefix_length (uint8_t channel_number,
                                                                                  uint8_t prefix_length,
                                                                                  fiid_obj_t obj_cmd_rq);

int fill_cmd_set_lan_configuration_parameters_ipv6_static_router_2_prefix_value (uint8_t channel_number,
                                                                                 uint8_t prefix_value[IPMI_IPV6_BYTES],
                                                                                 fiid_obj_t obj_cmd_rq);

int fill_cmd_get_lan_configuration_parameters (uint8_t channel_number,
                                               uint8_t get_parameter,
                                               uint8_t parameter_selector,
                                               uint8_t set_selector,
                                               uint8_t block_selector,
                                               fiid_obj_t obj_cmd_rq);

int fill_cmd_suspend_bmc_arps (uint8_t channel_number,
                               uint8_t gratuitous_arp_suspend,
                               uint8_t arp_response_suspend,
                               fiid_obj_t obj_cmd_rq);

int fill_cmd_get_ip_udp_rmcp_statistics (uint8_t channel_number,
                                         uint8_t clear_all_statistics,
                                         fiid_obj_t obj_cmd_rq);

#ifdef __cplusplus
}
#endif

#endif /* IPMI_LAN_CMDS_H */
