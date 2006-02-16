/*
   ipmi-lan-cmds-udm.h - IPMI UDM LAN Commands

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

#ifndef _IPMI_LAN_CMDS_UDM_H
#define _IPMI_LAN_CMDS_UDM_H

int8_t ipmi_cmd_set_lan_authentication_type_enables2 (ipmi_device_t *dev, 
                                                      uint8_t channel_number, 
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
                                                      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_ip_address2 (ipmi_device_t *dev, 
                                     uint8_t channel_number, 
                                     uint32_t ip_address, 
                                     fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_ip_address_source2 (ipmi_device_t *dev, 
                                            uint8_t channel_number, 
                                            uint8_t ip_address_source, 
                                            fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_subnet_mask2 (ipmi_device_t *dev, 
				      uint8_t channel_number, 
				      uint32_t subnet_mask, 
				      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_mac_address2 (ipmi_device_t *dev, 
                                      uint8_t channel_number,
                                      uint64_t mac_address,
                                      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_default_gateway_address2 (ipmi_device_t *dev, 
                                                  uint8_t channel_number, 
                                                  uint32_t ip_address, 
                                                  fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_default_gateway_mac_address2 (ipmi_device_t *dev, 
                                                      uint8_t channel_number,
                                                      uint64_t mac_address,
                                                      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_backup_gateway_address2 (ipmi_device_t *dev, 
                                                 uint8_t channel_number, 
                                                 uint32_t ip_address, 
                                                 fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_backup_gateway_mac_address2 (ipmi_device_t *dev, 
                                                     uint8_t channel_number,
                                                     uint64_t mac_address,
                                                     fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_bmc_generated_arp_control2 (ipmi_device_t *dev, 
                                                    uint8_t channel_number, 
                                                    uint8_t bmc_generated_gratuitous_arps, 
                                                    uint8_t bmc_generated_arp_responses, 
                                                    fiid_obj_t obj_cmd_rs);

int8_t ipmi_lan_set_gratuitous_arp_interval2 (ipmi_device_t *dev, 
					      uint8_t channel_number, 
					      uint8_t gratuitous_arp_interval, 
					      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_vlan_id2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t vlan_id_ls,
                                  uint8_t vlan_id_ms,
				  uint8_t vlan_id_enable, 
				  fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_set_lan_vlan_priority2 (ipmi_device_t *dev, 
					uint8_t channel_number,
					uint32_t vlan_priority,
					fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_authentication_type_enables2 (ipmi_device_t *dev, 
                                                      uint8_t channel_number, 
                                                      uint8_t get_parameter, 
                                                      uint8_t set_selector, 
                                                      uint8_t block_selector, 
                                                      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_ip_address2 (ipmi_device_t *dev, 
                                     uint8_t channel_number,
                                     uint8_t get_parameter,
                                     uint8_t set_selector,
                                     uint8_t block_selector,
                                     fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_ip_address_source2 (ipmi_device_t *dev, 
                                            uint8_t channel_number, 
                                            uint8_t get_parameter, 
                                            uint8_t set_selector, 
                                            uint8_t block_selector, 
                                            fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_mac_address2 (ipmi_device_t *dev, 
                                      uint8_t channel_number,
                                      uint8_t get_parameter,
                                      uint8_t set_selector,
                                      uint8_t block_selector,
                                      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_subnet_mask2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint8_t get_parameter,
				      uint8_t set_selector,
				      uint8_t block_selector,
				      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_bmc_generated_arp_control2 (ipmi_device_t *dev, 
                                                    uint8_t channel_number, 
                                                    uint8_t get_parameter, 
                                                    uint8_t set_selector, 
                                                    uint8_t block_selector, 
                                                    fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_gratuitous_arp_interval2 (ipmi_device_t *dev, 
						  uint8_t channel_number, 
						  uint8_t get_parameter, 
						  uint8_t set_selector, 
						  uint8_t block_selector, 
						  fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_default_gateway_address2 (ipmi_device_t *dev, 
                                                  uint8_t channel_number,
                                                  uint8_t get_parameter,
                                                  uint8_t set_selector,
                                                  uint8_t block_selector,
                                                  fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_default_gateway_mac_address2 (ipmi_device_t *dev, 
                                                      uint8_t channel_number,
                                                      uint8_t get_parameter,
                                                      uint8_t set_selector,
                                                      uint8_t block_selector,
                                                      fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_backup_gateway_address2 (ipmi_device_t *dev, 
                                                 uint8_t channel_number,
                                                 uint8_t get_parameter,
                                                 uint8_t set_selector,
                                                 uint8_t block_selector,
                                                 fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_backup_gateway_mac_address2 (ipmi_device_t *dev, 
                                                     uint8_t channel_number,
                                                     uint8_t get_parameter,
                                                     uint8_t set_selector,
                                                     uint8_t block_selector,
                                                     fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_vlan_id2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t get_parameter, 
				  uint8_t set_selector, 
				  uint8_t block_selector, 
				  fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_vlan_priority2 (ipmi_device_t *dev, 
					uint8_t channel_number, 
					uint8_t get_parameter, 
					uint8_t set_selector, 
					uint8_t block_selector, 
					fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_suspend_bmc_arps2 (ipmi_device_t *dev, 
				   uint8_t channel_number, 
				   uint8_t gratuitous_arp_suspend, 
				   uint8_t arp_response_suspend, 
				   fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif


#endif
