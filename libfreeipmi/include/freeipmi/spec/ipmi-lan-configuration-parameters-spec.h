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

#ifndef IPMI_LAN_CONFIGURATION_PARAMETERS_SPEC_H
#define IPMI_LAN_CONFIGURATION_PARAMETERS_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_LAN_CONFIGURATION_PARAMETER_SET_IN_PROGRESS                                            0
#define IPMI_LAN_CONFIGURATION_PARAMETER_AUTHENTICATION_TYPE_SUPPORT                                1
#define IPMI_LAN_CONFIGURATION_PARAMETER_AUTHENTICATION_TYPE_ENABLES                                2
#define IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS                                                 3
#define IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS_SOURCE                                          4
#define IPMI_LAN_CONFIGURATION_PARAMETER_MAC_ADDRESS                                                5
#define IPMI_LAN_CONFIGURATION_PARAMETER_SUBNET_MASK                                                6
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV4_HEADER_PARAMETERS                                     7
#define IPMI_LAN_CONFIGURATION_PARAMETER_PRIMARY_RMCP_PORT_NUMBER                                   8
#define IPMI_LAN_CONFIGURATION_PARAMETER_SECONDARY_RMCP_PORT_NUMBER                                 9
#define IPMI_LAN_CONFIGURATION_PARAMETER_BMC_GENERATED_ARP_CONTROL                                  10
#define IPMI_LAN_CONFIGURATION_PARAMETER_GRATUITOUS_ARP_INTERVAL                                    11
#define IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY_ADDRESS                                    12
#define IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY_MAC_ADDRESS                                13
#define IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY_ADDRESS                                     14
#define IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY_MAC_ADDRESS                                 15
#define IPMI_LAN_CONFIGURATION_PARAMETER_COMMUNITY_STRING                                           16
#define IPMI_LAN_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS                                     17
#define IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_TYPE                                           18
#define IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_ADDRESSES                                      19
#define IPMI_LAN_CONFIGURATION_PARAMETER_VLAN_ID                                                    20
#define IPMI_LAN_CONFIGURATION_PARAMETER_VLAN_PRIORITY                                              21
#define IPMI_LAN_CONFIGURATION_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_ENTRY_SUPPORT              22
#define IPMI_LAN_CONFIGURATION_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_ENTRIES                    23
#define IPMI_LAN_CONFIGURATION_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_PRIVILEGE_LEVELS           24
#define IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_ADDRESS_VLAN_TAGS                              25
#define IPMI_LAN_CONFIGURATION_PARAMETER_BAD_PASSWORD_THRESHOLD                                     26
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_IPV4_SUPPORT                                          50
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_IPV4_ADDRESSING_ENABLES                               51
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_HEADER_STATIC_TRAFFIC_CLASS                           52
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_HEADER_STATIC_HOP_LIMIT                               53
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_HEADER_FLOW_LABEL                                     54
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATUS                                                55
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ADDRESSES                                      56
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DHCPV6_STATIC_DUID_STORAGE_LENGTH                     57
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DHCPV6_STATIC_DUIDS                                   58
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DYNAMIC_ADDRESS                                       59
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DHCPV6_DYNAMIC_DUID_STORAGE_LENGTH                    60
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DHCPV6_DYNAMIC_DUIDS                                  61
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DHCPV6_TIMING_CONFIGURATION_SUPPORT                   62
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DHCPV6_TIMING_AND_CONFIGURATION                       63
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_ROUTER_ADDRESS_CONFIGURATION_CONTROL                  64
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_1_IP_ADDRESS                            65
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_1_MAC_ADDRESS                           66
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_1_PREFIX_LENGTH                         67
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_1_PREFIX_VALUE                          68
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_2_IP_ADDRESS                            69
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_2_MAC_ADDRESS                           70
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_2_PREFIX_LENGTH                         71
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_STATIC_ROUTER_2_PREFIX_VALUE                          72
#define IPMI_LAN_CONFIGURATION_PARAMETER_NUMBER_OF_DYNAMIC_ROUTER_INFO_SETS                         73
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DYNAMIC_ROUTER_INFO_IP_ADDRESS                        74
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DYNAMIC_ROUTER_INFO_MAC_ADDRESS                       75
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DYNAMIC_ROUTER_INFO_PREFIX_LENGTH                     76
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DYNAMIC_ROUTER_INFO_PREFIX_VALUE                      77
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_DYNAMIC_ROUTER_RECEIVED_HOP_LIMIT                     78
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_NEIGHBOR_DISCOVERY_SLAAC_TIMING_CONFIGURATION_SUPPORT 79
#define IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_NEIGHBOR_DISCOVERY_SLAAC_TIMING_CONFIGURATION         80
#define IPMI_LAN_CONFIGURATION_PARAMETER_OEM_MIN                                                    192
#define IPMI_LAN_CONFIGURATION_PARAMETER_OEM_MAX                                                    255

/* To avoid gcc warnings, add +1 in comparison */
#define IPMI_LAN_CONFIGURATION_PARAMETER_SELECTOR_VALID(__parameter_selector)             \
  ((((__parameter_selector) + 1) > (IPMI_LAN_CONFIGURATION_PARAMETER_SET_IN_PROGRESS + 1) \
    && (__parameter_selector) <= IPMI_LAN_CONFIGURATION_PARAMETER_IPV6_NEIGHBOR_DISCOVERY_SLAAC_TIMING_CONFIGURATION) ? 1 : 0)

/* To avoid gcc warnings, subtract -1 in comparison */
#define IPMI_LAN_CONFIGURATION_PARAMETER_SELECTOR_IS_OEM(__parameter_selector) \
  (((__parameter_selector) >= IPMI_LAN_CONFIGURATION_PARAMETER_OEM_MIN \
    && ((__parameter_selector) - 1) <= (IPMI_LAN_CONFIGURATION_PARAMETER_OEM_MAX - 1)) ? 1 : 0)

#ifdef __cplusplus
}
#endif

#endif /* IPMI_LAN_CONFIGURATION_PARAMETERS_SPEC_H */
