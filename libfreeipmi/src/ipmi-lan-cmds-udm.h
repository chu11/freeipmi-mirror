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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _IPMI_LAN_CMDS_UDM_H
#define _IPMI_LAN_CMDS_UDM_H

int8_t ipmi_cmd_lan_set_arp2 (ipmi_device_t *dev, 
			      uint8_t channel_number, 
			      uint8_t bmc_generated_gratuitous_arps_flag, 
			      uint8_t bmc_generated_arp_responses_flag, 
			      fiid_obj_t obj_cmd_rs);
int8_t ipmi_lan_set_gratuitous_arp_interval2 (ipmi_device_t *dev, 
					      uint8_t channel_number, 
					      uint8_t gratuitous_arp_interval, 
					      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_auth_type_enables2 (ipmi_device_t *dev, 
					    uint8_t channel_number, 
                                            int8_t auth_type_callback_none,
                                            int8_t auth_type_callback_md2,
                                            int8_t auth_type_callback_md5,
                                            int8_t auth_type_callback_straight_password,
                                            int8_t auth_type_callback_oem_proprietary,
                                            int8_t auth_type_user_none,
                                            int8_t auth_type_user_md2,
                                            int8_t auth_type_user_md5,
                                            int8_t auth_type_user_straight_password,
                                            int8_t auth_type_user_oem_proprietary,
                                            int8_t auth_type_operator_none,
                                            int8_t auth_type_operator_md2,
                                            int8_t auth_type_operator_md5,
                                            int8_t auth_type_operator_straight_password,
                                            int8_t auth_type_operator_oem_proprietary,
                                            int8_t auth_type_admin_none,
                                            int8_t auth_type_admin_md2,
                                            int8_t auth_type_admin_md5,
                                            int8_t auth_type_admin_straight_password,
                                            int8_t auth_type_admin_oem_proprietary,
                                            int8_t auth_type_oem_none,
                                            int8_t auth_type_oem_md2,
                                            int8_t auth_type_oem_md5,
                                            int8_t auth_type_oem_straight_password,
                                            int8_t auth_type_oem_oem_proprietary,
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_ip_addr_source2 (ipmi_device_t *dev, 
					 uint8_t channel_number, 
					 uint8_t ip_addr_source, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_ip_addr2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint32_t ip_addr, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_default_gw_ip_addr2 (ipmi_device_t *dev, 
					     uint8_t channel_number, 
					     uint32_t ip_addr, 
					     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_backup_gw_ip_addr2 (ipmi_device_t *dev, 
					    uint8_t channel_number, 
					    uint32_t ip_addr, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_vlan_id2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t vlan_id_flag, 
				  uint32_t vlan_id, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_vlan_priority2 (ipmi_device_t *dev, 
					uint8_t channel_number,
					uint32_t vlan_priority,
					fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_subnet_mask2 (ipmi_device_t *dev, 
				      uint8_t channel_number, 
				      uint32_t subnet_mask, 
				      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_mac_addr2 (ipmi_device_t *dev, 
				   uint8_t channel_number,
				   uint64_t mac_addr,
				   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_default_gw_mac_addr2 (ipmi_device_t *dev, 
					      uint8_t channel_number,
					      uint64_t mac_addr,
					      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_set_backup_gw_mac_addr2 (ipmi_device_t *dev, 
					     uint8_t channel_number,
					     uint64_t mac_addr,
					     fiid_obj_t obj_cmd_rs);

int8_t ipmi_cmd_lan_get_arp2 (ipmi_device_t *dev, 
			      uint8_t channel_number, 
			      uint8_t parameter_type, 
			      uint8_t set_selector, 
			      uint8_t block_selector, 
			      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_gratuitous_arp_interval2 (ipmi_device_t *dev, 
						  uint8_t channel_number, 
						  uint8_t parameter_type, 
						  uint8_t set_selector, 
						  uint8_t block_selector, 
						  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_auth_type_enables2 (ipmi_device_t *dev, 
					    uint8_t channel_number, 
					    uint8_t parameter_type, 
					    uint8_t set_selector, 
					    uint8_t block_selector, 
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_ip_addr_source2 (ipmi_device_t *dev, 
					 uint8_t channel_number, 
					 uint8_t parameter_type, 
					 uint8_t set_selector, 
					 uint8_t block_selector, 
					 fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_ip_addr2 (ipmi_device_t *dev, 
				  uint8_t channel_number,
				  uint8_t parameter_type,
				  uint8_t set_selector,
				  uint8_t block_selector,
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_default_gw_ip_addr2 (ipmi_device_t *dev, 
					     uint8_t channel_number,
					     uint8_t parameter_type,
					     uint8_t set_selector,
					     uint8_t block_selector,
					     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_backup_gw_ip_addr2 (ipmi_device_t *dev, 
					    uint8_t channel_number,
					    uint8_t parameter_type,
					    uint8_t set_selector,
					    uint8_t block_selector,
					    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_subnet_mask2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint8_t parameter_type,
				      uint8_t set_selector,
				      uint8_t block_selector,
				      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_mac_addr2 (ipmi_device_t *dev, 
				   uint8_t channel_number,
				   uint8_t parameter_type,
				   uint8_t set_selector,
				   uint8_t block_selector,
				   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_default_gw_mac_addr2 (ipmi_device_t *dev, 
					      uint8_t channel_number,
					      uint8_t parameter_type,
					      uint8_t set_selector,
					      uint8_t block_selector,
					      fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_backup_gw_mac_addr2 (ipmi_device_t *dev, 
					     uint8_t channel_number,
					     uint8_t parameter_type,
					     uint8_t set_selector,
					     uint8_t block_selector,
					     fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_suspend_bmc_arps2 (ipmi_device_t *dev, 
				   uint8_t channel_number, 
				   uint8_t gratuitous_arp_suspend, 
				   uint8_t arp_response_suspend, 
				   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_vlan_id2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t parameter_type, 
				  uint8_t set_selector, 
				  uint8_t block_selector, 
				  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_lan_get_vlan_priority2 (ipmi_device_t *dev, 
					uint8_t channel_number, 
					uint8_t parameter_type, 
					uint8_t set_selector, 
					uint8_t block_selector, 
					fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif


#endif
