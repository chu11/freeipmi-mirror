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

#define IPMI_ENABLE_BMC_GENERATED_GRATUITOUS_ARPS     0x1
#define IPMI_DISABLE_BMC_GENERATED_GRATUITOUS_ARPS    0x0

#define IPMI_BMC_GENERATED_GRATUITOUS_ARPS_VALID(__val) \
        (((__val) == IPMI_ENABLE_BMC_GENERATED_GRATUITOUS_ARPS \
          || (__val) == IPMI_DISABLE_BMC_GENERATED_GRATUITOUS_ARPS) ? 1 : 0)

#define IPMI_ENABLE_BMC_GENERATED_ARP_RESPONSES       0x1
#define IPMI_DISABLE_BMC_GENERATED_ARP_RESPONSES      0x0

#define IPMI_BMC_GENERATED_ARP_RESPONSES_VALID(__val) \
        (((__val) == IPMI_ENABLE_BMC_GENERATED_ARP_RESPONSES \
          || (__val) == IPMI_DISABLE_BMC_GENERATED_ARP_RESPONSES) ? 1 : 0)

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

#define IPMI_AUTH_TYPE_NONE                  0x01
#define IPMI_AUTH_TYPE_MD2                   0x02
#define IPMI_AUTH_TYPE_MD5                   0x04
#define IPMI_AUTH_TYPE_STRAIGHT_PASSWORD     0x10
#define IPMI_AUTH_TYPE_OEM_PROPRIETARY       0x20

#define IPMI_AUTH_TYPE_ENABLE                0x1
#define IPMI_AUTH_TYPE_DISABLE               0x0

#define IPMI_AUTH_TYPE_ENABLE_VALID(__val) \
        (((__val) == IPMI_AUTH_TYPE_ENABLE \
         || (__val) == IPMI_AUTH_TYPE_DISABLE) ? 1 : 0)

#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_NO_SUSPEND    0x0
#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPEND       0x1

#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(__val) \
        (((__val) ==  IPMI_BMC_GENERATED_GRATUITOUS_ARP_NO_SUSPEND \
          || (__val) == IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPEND) ? 1 : 0)

#define IPMI_BMC_GENERATED_ARP_RESPONSE_NO_SUSPEND      0x0
#define IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPEND         0x1

#define IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(__val) \
        (((__val) ==  IPMI_BMC_GENERATED_ARP_RESPONSE_NO_SUSPEND \
          || (__val) == IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPEND) ? 1 : 0)

#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPENDED     0x0
#define IPMI_BMC_GENERATED_GRATUITOUS_ARP_OCCURRING     0x1
#define IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPENDED       0x0
#define IPMI_BMC_GENERATED_ARP_RESPONSE_OCCURRING       0x1

#define IPMI_GET_LAN_PARAMETER                          0x0
#define IPMI_GET_LAN_PARAMETER_REVISION_ONLY            0x1

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_set_lan_conf_param_rq;
extern fiid_template_t tmpl_set_lan_conf_param_rs;

extern fiid_template_t tmpl_set_lan_conf_param_auth_type_enables_rq;
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

extern fiid_template_t tmpl_get_lan_conf_param_auth_type_enables_rs;
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

int8_t fill_lan_set_conf_param (fiid_obj_t obj_data_rq,
                                uint8_t channel_number,
                                uint8_t parameter_selector,
                                uint8_t *configuration_parameter_data,
                                uint8_t configuration_parameter_data_len);

int8_t fill_lan_set_auth_type_enables (uint8_t channel_number,
                                       uint8_t auth_type_callback_none,
                                       uint8_t auth_type_callback_md2,
                                       uint8_t auth_type_callback_md5,
                                       uint8_t auth_type_callback_straight_password,
                                       uint8_t auth_type_callback_oem_proprietary,
                                       uint8_t auth_type_user_none,
                                       uint8_t auth_type_user_md2,
                                       uint8_t auth_type_user_md5,
                                       uint8_t auth_type_user_straight_password,
                                       uint8_t auth_type_user_oem_proprietary,
                                       uint8_t auth_type_operator_none,
                                       uint8_t auth_type_operator_md2,
                                       uint8_t auth_type_operator_md5,
                                       uint8_t auth_type_operator_straight_password,
                                       uint8_t auth_type_operator_oem_proprietary,
                                       uint8_t auth_type_admin_none,
                                       uint8_t auth_type_admin_md2,
                                       uint8_t auth_type_admin_md5,
                                       uint8_t auth_type_admin_straight_password,
                                       uint8_t auth_type_admin_oem_proprietary,
                                       uint8_t auth_type_oem_none,
                                       uint8_t auth_type_oem_md2,
                                       uint8_t auth_type_oem_md5,
                                       uint8_t auth_type_oem_straight_password,
                                       uint8_t auth_type_oem_oem_proprietary,
                                       fiid_obj_t obj_data_rq);
 
int8_t fill_lan_set_ip_address (uint8_t channel_number,
                                uint32_t ip_address,
                                fiid_obj_t obj_data_rq);
   
int8_t fill_lan_set_ip_address_source (uint8_t channel_number,
                                       uint8_t ip_address_source,
                                       fiid_obj_t obj_data_rq);

int8_t fill_lan_set_mac_address (uint8_t channel_number,
                                 uint64_t mac_address,
                                 fiid_obj_t obj_data_rq);

int8_t fill_lan_set_subnet_mask (uint8_t channel_number,
                                 uint32_t subnet_mask,
                                 fiid_obj_t obj_data_rq);
  
int8_t fill_lan_set_bmc_generated_arp_control (uint8_t channel_number,
                                               uint8_t bmc_generated_gratuitous_arps_flag,
                                               uint8_t bmc_generated_arp_responses_flag,
                                               fiid_obj_t obj_data_rq);

int8_t fill_lan_set_gratuitous_arp_interval (uint8_t channel_number,
                                             uint8_t gratuitous_arp_interval,
                                             fiid_obj_t obj_data_rq);

int8_t fill_lan_set_default_gateway_address (uint8_t channel_number,
                                             uint32_t ip_address,
                                             fiid_obj_t obj_data_rq);

int8_t fill_lan_set_default_gateway_mac_address (uint8_t channel_number,
                                                 uint64_t mac_address,
                                                 fiid_obj_t obj_data_rq);

int8_t fill_lan_set_backup_gateway_address (uint8_t channel_number,
                                            uint32_t ip_address,
                                            fiid_obj_t obj_data_rq);
  
int8_t fill_lan_set_backup_gateway_mac_address (uint8_t channel_number,
                                                uint64_t mac_address,
                                                fiid_obj_t obj_data_rq);

int8_t fill_lan_set_vlan_id (uint8_t channel_number,
                             uint8_t vlan_id_enable,
                             uint32_t vlan_id,
                             fiid_obj_t obj_data_rq);

int8_t fill_lan_set_vlan_priority (uint8_t channel_number,
                                   uint8_t vlan_priority,
                                   fiid_obj_t obj_data_rq);

int8_t fill_get_lan_conf_param (uint8_t parameter_selector,
                                uint8_t channel_number,
                                uint8_t parameter_type,
                                uint8_t set_selector,
                                uint8_t block_selector,
                                fiid_obj_t obj_data_rq);

int8_t fill_suspend_bmc_arps (uint8_t channel_number,
                              uint8_t gratuitous_arp_suspend,
                              uint8_t arp_response_suspend,
                              fiid_obj_t obj_data_rq);

#ifdef __cplusplus
}
#endif


#endif
