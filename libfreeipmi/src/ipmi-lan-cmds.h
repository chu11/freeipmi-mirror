/*
   ipmi-lan-cmds.h - IPMI LAN Commands

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

#ifndef _IPMI_LAN_CMDS_H
#define _IPMI_LAN_CMDS_H

#define IPMI_AUTHENTICATION_TYPE_ENABLE                0x1
#define IPMI_AUTHENTICATION_TYPE_DISABLE               0x0

#define IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(__val) \
        (((__val) == IPMI_AUTHENTICATION_TYPE_ENABLE \
         || (__val) == IPMI_AUTHENTICATION_TYPE_DISABLE) ? 1 : 0)

#define IPMI_IP_ADDRESS_SOURCE_UNSPECIFIED                0x0
#define IPMI_IP_ADDRESS_SOURCE_STATIC                     0x1
#define IPMI_IP_ADDRESS_SOURCE_DHCP                       0x2
#define IPMI_IP_ADDRESS_SOURCE_BIOS                       0x3
#define IPMI_IP_ADDRESS_SOURCE_OTHER                      0x4

#define IPMI_IP_ADDRESS_SOURCE_VALID(__val) \
        (((__val) == IPMI_IP_ADDRESS_SOURCE_UNSPECIFIED \
          || (__val) == IPMI_IP_ADDRESS_SOURCE_STATIC \
          || (__val) == IPMI_IP_ADDRESS_SOURCE_DHCP \
          || (__val) == IPMI_IP_ADDRESS_SOURCE_BIOS \
          || (__val) == IPMI_IP_ADDRESS_SOURCE_OTHER) ? 1 : 0)

#define IPMI_BMC_GENERATED_GRATUITOUS_ARPS_ENABLE     0x1
#define IPMI_BMC_GENERATED_GRATUITOUS_ARPS_DISABLE    0x0

#define IPMI_BMC_GENERATED_GRATUITOUS_ARPS_VALID(__val) \
        (((__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARPS_ENABLE \
         || (__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARPS_DISABLE) ? 1 : 0)

#define IPMI_BMC_GENERATED_ARP_RESPONSES_ENABLE       0x1
#define IPMI_BMC_GENERATED_ARP_RESPONSES_DISABLE      0x0

#define IPMI_BMC_GENERATED_ARP_RESPONSES_VALID(__val) \
        (((__val) == IPMI_BMC_GENERATED_ARP_RESPONSES_ENABLE \
         || (__val) == IPMI_BMC_GENERATED_ARP_RESPONSES_DISABLE) ? 1 : 0)

#define IPMI_VLAN_ID_ENABLE                             0x1
#define IPMI_VLAN_ID_DISABLE                            0x0

#define IPMI_VLAN_ID_ENABLE_VALID(__val) \
        (((__val) ==  IPMI_VLAN_ID_ENABLE \
          || (__val) == IPMI_VLAN_ID_DISABLE) ? 1 : 0)

#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_DO_NOT_SUSPEND    0x0
#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPEND           0x1

#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(__val) \
        (((__val) ==  IPMI_BMC_GENERATED_GRATUITOUS_ARP_DO_NOT_SUSPEND \
          || (__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPEND) ? 1 : 0)

#define IPMI_BMC_GENERATED_ARP_RESPONSE_DO_NOT_SUSPEND      0x0
#define IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPEND             0x1

#define IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(__val) \
        (((__val) ==  IPMI_BMC_GENERATED_ARP_RESPONSE_DO_NOT_SUSPEND \
          || (__val) == IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPEND) ? 1 : 0)

#define IPMI_GET_LAN_PARAMETER                          0x0
#define IPMI_GET_LAN_PARAMETER_REVISION_ONLY            0x1

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_set_lan_conf_param_rq;
extern fiid_template_t tmpl_set_lan_conf_param_rs;

extern fiid_template_t tmpl_set_lan_conf_param_authentication_type_enables_rq;
extern fiid_template_t tmpl_set_lan_conf_param_ip_address_rq;
extern fiid_template_t tmpl_set_lan_conf_param_ip_address_source_rq;
extern fiid_template_t tmpl_set_lan_conf_param_mac_address_rq;
extern fiid_template_t tmpl_set_lan_conf_param_subnet_mask_rq;
extern fiid_template_t tmpl_set_lan_conf_param_bmc_generated_arp_control_rq;
extern fiid_template_t tmpl_set_lan_conf_param_gratuitous_arp_interval_rq;
extern fiid_template_t tmpl_set_lan_conf_param_default_gateway_address_rq;
extern fiid_template_t tmpl_set_lan_conf_param_default_gateway_mac_address_rq;
extern fiid_template_t tmpl_set_lan_conf_param_backup_gateway_address_rq;
extern fiid_template_t tmpl_set_lan_conf_param_backup_gateway_mac_address_rq;
extern fiid_template_t tmpl_set_lan_conf_param_vlan_id_rq;
extern fiid_template_t tmpl_set_lan_conf_param_vlan_priority_rq;

extern fiid_template_t tmpl_get_lan_conf_param_rq;
extern fiid_template_t tmpl_get_lan_conf_param_rs;

extern fiid_template_t tmpl_get_lan_conf_param_authentication_type_enables_rs;
extern fiid_template_t tmpl_get_lan_conf_param_ip_address_rs;
extern fiid_template_t tmpl_get_lan_conf_param_ip_address_source_rs;
extern fiid_template_t tmpl_get_lan_conf_param_mac_address_rs;
extern fiid_template_t tmpl_get_lan_conf_param_subnet_mask_rs;
extern fiid_template_t tmpl_get_lan_conf_param_bmc_generated_arp_control_rs;
extern fiid_template_t tmpl_get_lan_conf_param_gratuitous_arp_interval_rs;
extern fiid_template_t tmpl_get_lan_conf_param_default_gateway_address_rs;
extern fiid_template_t tmpl_get_lan_conf_param_default_gateway_mac_address_rs;
extern fiid_template_t tmpl_get_lan_conf_param_backup_gateway_address_rs;
extern fiid_template_t tmpl_get_lan_conf_param_backup_gateway_mac_address_rs;
extern fiid_template_t tmpl_get_lan_conf_param_vlan_id_rs;
extern fiid_template_t tmpl_get_lan_conf_param_vlan_priority_rs;

extern fiid_template_t tmpl_suspend_bmc_arps_rq;
extern fiid_template_t tmpl_suspend_bmc_arps_rs;

int8_t fill_cmd_set_lan_conf_param (fiid_obj_t obj_data_rq,
                                    uint8_t channel_number,
                                    uint8_t parameter_selector,
                                    uint8_t *configuration_parameter_data,
                                    uint8_t configuration_parameter_data_len);

int8_t fill_cmd_set_lan_authentication_type_enables (uint8_t channel_number,
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
                                                     fiid_obj_t obj_data_rq);
 
int8_t fill_cmd_set_lan_ip_address (uint8_t channel_number,
                                    uint32_t ip_address,
                                    fiid_obj_t obj_data_rq);
   
int8_t fill_cmd_set_lan_ip_address_source (uint8_t channel_number,
                                           uint8_t ip_address_source,
                                           fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_mac_address (uint8_t channel_number,
                                     uint64_t mac_address,
                                     fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_subnet_mask (uint8_t channel_number,
                                     uint32_t subnet_mask,
                                     fiid_obj_t obj_data_rq);
  
int8_t fill_cmd_set_lan_bmc_generated_arp_control (uint8_t channel_number,
                                                   uint8_t bmc_generated_gratuitous_arps,
                                                   uint8_t bmc_generated_arp_responses,
                                                   fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_gratuitous_arp_interval (uint8_t channel_number,
                                                 uint8_t gratuitous_arp_interval,
                                                 fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_default_gateway_address (uint8_t channel_number,
                                                 uint32_t ip_address,
                                                 fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_default_gateway_mac_address (uint8_t channel_number,
                                                     uint64_t mac_address,
                                                     fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_backup_gateway_address (uint8_t channel_number,
                                                uint32_t ip_address,
                                                fiid_obj_t obj_data_rq);
  
int8_t fill_cmd_set_lan_backup_gateway_mac_address (uint8_t channel_number,
                                                    uint64_t mac_address,
                                                    fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_vlan_id (uint8_t channel_number,
                                 uint8_t vlan_id_ls,
                                 uint8_t vlan_id_ms,
                                 uint8_t vlan_id_enable,
                                 fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_lan_vlan_priority (uint8_t channel_number,
                                       uint8_t vlan_priority,
                                       fiid_obj_t obj_data_rq);

int8_t fill_cmd_get_lan_conf_param (uint8_t channel_number,
                                    uint8_t parameter_type,
                                    uint8_t parameter_selector,
                                    uint8_t set_selector,
                                    uint8_t block_selector,
                                    fiid_obj_t obj_data_rq);

int8_t fill_cmd_suspend_bmc_arps (uint8_t channel_number,
                                  uint8_t gratuitous_arp_suspend,
                                  uint8_t arp_response_suspend,
                                  fiid_obj_t obj_data_rq);

#ifdef __cplusplus
}
#endif


#endif
