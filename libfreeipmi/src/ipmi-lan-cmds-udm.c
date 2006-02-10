/* 
   ipmi-lan-cmds-udm.c - IPMI UDM LAN Commands

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

#include "freeipmi.h"

int8_t 
ipmi_cmd_lan_set_arp2 (ipmi_device_t *dev, 
		       uint8_t channel_number, 
		       uint8_t bmc_generated_gratuitous_arps_flag, 
		       uint8_t bmc_generated_arp_responses_flag, 
		       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;

  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_BMC_GENERATED_GRATUITOUS_ARPS_VALID(bmc_generated_gratuitous_arps_flag)
      || !IPMI_BMC_GENERATED_ARP_RESPONSES_VALID(bmc_generated_arp_responses_flag)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_bmc_generated_arp_control_rq);
  ERR (fill_lan_set_arp (channel_number, 
			 bmc_generated_gratuitous_arps_flag, 
			 bmc_generated_arp_responses_flag,
                         obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_bmc_generated_arp_control_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_lan_set_gratuitous_arp_interval2 (ipmi_device_t *dev, 
				       uint8_t channel_number, 
				       uint8_t gratuitous_arp_interval, 
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_gratuitous_arp_interval_rq);
  ERR (fill_lan_set_gratuitous_arp_interval (channel_number, 
					     gratuitous_arp_interval,
                                             obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_gratuitous_arp_interval_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_auth_type_enables2 (ipmi_device_t *dev, 
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
                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_auth_type_enables_rq);
  ERR (fill_lan_set_auth_type_enables (channel_number, 
                                       auth_type_callback_none,
                                       auth_type_callback_md2,
                                       auth_type_callback_md5,
                                       auth_type_callback_straight_password,
                                       auth_type_callback_oem_proprietary,
                                       auth_type_user_none,
                                       auth_type_user_md2,
                                       auth_type_user_md5,
                                       auth_type_user_straight_password,
                                       auth_type_user_oem_proprietary,
                                       auth_type_operator_none,
                                       auth_type_operator_md2,
                                       auth_type_operator_md5,
                                       auth_type_operator_straight_password,
                                       auth_type_operator_oem_proprietary,
                                       auth_type_admin_none,
                                       auth_type_admin_md2,
                                       auth_type_admin_md5,
                                       auth_type_admin_straight_password,
                                       auth_type_admin_oem_proprietary,
                                       auth_type_oem_none,
                                       auth_type_oem_md2,
                                       auth_type_oem_md5,
                                       auth_type_oem_straight_password,
                                       auth_type_oem_oem_proprietary,
                                       obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_auth_type_enables_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_ip_addr_source2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t ip_addr_source, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_ip_addr_source_rq);
  ERR (fill_lan_set_ip_addr_source (channel_number, 
				    ip_addr_source,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_ip_addr_source_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_ip_addr2 (ipmi_device_t *dev, 
			   uint8_t channel_number, 
			   uint32_t ip_addr, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_ip_addr_rq);
  ERR (fill_lan_set_ip_addr (IPMI_LAN_PARAM_IP_ADDR, 
			     channel_number, 
			     ip_addr,
                             obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_ip_addr_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_default_gw_ip_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number, 
				      uint32_t ip_addr, 
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_ip_addr_rq);
  ERR (fill_lan_set_ip_addr (IPMI_LAN_PARAM_DEFAULT_GATEWAY_IP_ADDR, 
			     channel_number, 
			     ip_addr,
                             obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_ip_addr_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_backup_gw_ip_addr2 (ipmi_device_t *dev, 
				     uint8_t channel_number, 
				     uint32_t ip_addr, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_ip_addr_rq);
  ERR (fill_lan_set_ip_addr (IPMI_LAN_PARAM_BACKUP_GATEWAY_IP_ADDR, 
			     channel_number, 
			     ip_addr,
                             obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_ip_addr_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_vlan_id2 (ipmi_device_t *dev, 
			   uint8_t channel_number, 
			   uint8_t vlan_id_enable, 
			   uint32_t vlan_id, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_vlan_id_rq);
  ERR (fill_lan_set_vlan_id (channel_number,
                             vlan_id_enable,
			     vlan_id,
                             obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_vlan_id_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_vlan_priority2 (ipmi_device_t *dev, 
				 uint8_t channel_number,
				 uint32_t vlan_priority,
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_vlan_priority_rq);
  ERR (fill_lan_set_vlan_priority (channel_number,
                                   vlan_priority,
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_vlan_priority_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_subnet_mask2 (ipmi_device_t *dev, 
			       uint8_t channel_number, 
			       uint32_t subnet_mask, 
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_subnet_mask_rq);
  ERR (fill_lan_set_subnet_mask (channel_number, 
				 subnet_mask,
                                 obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_subnet_mask_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_mac_addr2 (ipmi_device_t *dev, 
			    uint8_t channel_number,
			    uint64_t mac_addr,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_mac_addr_rq);
  ERR (fill_lan_set_mac_addr (IPMI_LAN_PARAM_MAC_ADDR, 
			      channel_number, 
			      mac_addr,
                              obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_mac_addr_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_default_gw_mac_addr2 (ipmi_device_t *dev, 
				       uint8_t channel_number,
				       uint64_t mac_addr,
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_mac_addr_rq);
  ERR (fill_lan_set_mac_addr (IPMI_LAN_PARAM_DEFAULT_GATEWAY_MAC_ADDR, 
			      channel_number, 
			      mac_addr,
                              obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_mac_addr_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_set_backup_gw_mac_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint64_t mac_addr,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_lan_conf_param_mac_addr_rq);
  ERR (fill_lan_set_mac_addr (IPMI_LAN_PARAM_BACKUP_GATEWAY_MAC_ADDR, 
			      channel_number, 
			      mac_addr,
                              obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_lan_conf_param_mac_addr_rq, 
		 obj_cmd_rs, 
		 tmpl_set_lan_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_arp2 (ipmi_device_t *dev, 
		       uint8_t channel_number, 
		       uint8_t parameter_type, 
		       uint8_t set_selector, 
		       uint8_t block_selector, 
		       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_bmc_generated_arp_control_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_gratuitous_arp_interval2 (ipmi_device_t *dev, 
					   uint8_t channel_number, 
					   uint8_t parameter_type, 
					   uint8_t set_selector, 
					   uint8_t block_selector, 
					   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_GRATUITOUS_ARP_INTERVAL, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_gratuitous_arp_interval_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_auth_type_enables2 (ipmi_device_t *dev, 
				     uint8_t channel_number, 
				     uint8_t parameter_type, 
				     uint8_t set_selector, 
				     uint8_t block_selector, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_AUTH_TYPE_ENABLES, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_auth_type_enables_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_ip_addr_source2 (ipmi_device_t *dev, 
				  uint8_t channel_number, 
				  uint8_t parameter_type, 
				  uint8_t set_selector, 
				  uint8_t block_selector, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_IP_ADDR_SOURCE, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_ip_addr_source_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_ip_addr2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
			   uint8_t parameter_type,
			   uint8_t set_selector,
			   uint8_t block_selector,
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_IP_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector, 
				obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_ip_addr_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_default_gw_ip_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint8_t parameter_type,
				      uint8_t set_selector,
				      uint8_t block_selector,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_DEFAULT_GATEWAY_IP_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_gw_ip_addr_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_backup_gw_ip_addr2 (ipmi_device_t *dev, 
				     uint8_t channel_number,
				     uint8_t parameter_type,
				     uint8_t set_selector,
				     uint8_t block_selector,
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_BACKUP_GATEWAY_IP_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_gw_ip_addr_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_subnet_mask2 (ipmi_device_t *dev, 
			       uint8_t channel_number,
			       uint8_t parameter_type,
			       uint8_t set_selector,
			       uint8_t block_selector,
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_SUBNET_MASK, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_subnet_mask_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_mac_addr2 (ipmi_device_t *dev, 
			    uint8_t channel_number,
			    uint8_t parameter_type,
			    uint8_t set_selector,
			    uint8_t block_selector,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_MAC_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_mac_addr_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_default_gw_mac_addr2 (ipmi_device_t *dev, 
				       uint8_t channel_number,
				       uint8_t parameter_type,
				       uint8_t set_selector,
				       uint8_t block_selector,
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_DEFAULT_GATEWAY_MAC_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_mac_addr_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_backup_gw_mac_addr2 (ipmi_device_t *dev, 
				      uint8_t channel_number,
				      uint8_t parameter_type,
				      uint8_t set_selector,
				      uint8_t block_selector,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_BACKUP_GATEWAY_MAC_ADDR, 
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_mac_addr_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_vlan_id2 (ipmi_device_t *dev, 
			   uint8_t channel_number, 
			   uint8_t parameter_type, 
			   uint8_t set_selector, 
			   uint8_t block_selector, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_VLAN_ID,
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_vlan_id_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_lan_get_vlan_priority2 (ipmi_device_t *dev, 
				 uint8_t channel_number, 
				 uint8_t parameter_type, 
				 uint8_t set_selector, 
				 uint8_t block_selector, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_lan_conf_param_rq);
  ERR (fill_get_lan_conf_param (IPMI_LAN_PARAM_VLAN_PRIORITY,
				channel_number, 
				parameter_type, 
				set_selector, 
				block_selector,
                                obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_lan_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_lan_conf_param_vlan_priority_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_suspend_bmc_arps2 (ipmi_device_t *dev, 
			    uint8_t channel_number, 
			    uint8_t gratuitous_arp_suspend, 
			    uint8_t arp_response_suspend, 
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_suspend_bmc_arps_rq);
  ERR (fill_suspend_bmc_arps (channel_number, 
			      gratuitous_arp_suspend, 
			      arp_response_suspend,
                              obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_suspend_bmc_arps_rq, 
		 obj_cmd_rs, 
		 tmpl_suspend_bmc_arps_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

