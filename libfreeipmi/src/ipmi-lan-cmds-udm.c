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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "ipmi-lan-cmds-udm.h"
#include "ipmi-lan-cmds.h"
#include "ipmi-lan-param-spec.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "ipmi-netfn-spec.h"
#include "ipmi-ipmb-interface.h"

int8_t 
ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables2 (ipmi_device_t *dev, 
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
                                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_none)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_md2)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_md5)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_straight_password)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_oem_proprietary)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_none)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_md2)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_md5)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_straight_password)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_oem_proprietary)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_none)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_md2)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_md5)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_straight_password)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_oem_proprietary)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_none)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_md2)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_md5)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_straight_password)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_oem_proprietary)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_none)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_md2)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_md5)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_straight_password)
      || !IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_oem_proprietary)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_authentication_type_enables_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_authentication_type_enables (channel_number, 
											callback_level_none,
											callback_level_md2,
											callback_level_md5,
											callback_level_straight_password,
											callback_level_oem_proprietary,
											user_level_none,
											user_level_md2,
											user_level_md5,
											user_level_straight_password,
											user_level_oem_proprietary,
											operator_level_none,
											operator_level_md2,
											operator_level_md5,
											operator_level_straight_password,
											operator_level_oem_proprietary,
											admin_level_none,
											admin_level_md2,
											admin_level_md5,
											admin_level_straight_password,
											admin_level_oem_proprietary,
											oem_level_none,
											oem_level_md2,
											oem_level_md5,
											oem_level_straight_password,
											oem_level_oem_proprietary,
											obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_ip_address2 (ipmi_device_t *dev, 
                                                       uint8_t channel_number, 
                                                       uint32_t ip_address, 
                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_ip_address_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_ip_address (channel_number, 
								       ip_address,
								       obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_ip_address_source2 (ipmi_device_t *dev, 
                                                              uint8_t channel_number, 
                                                              uint8_t ip_address_source, 
                                                              fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_ip_address_source_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_ip_address_source (channel_number, 
									      ip_address_source,
									      obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_mac_address2 (ipmi_device_t *dev, 
                                                        uint8_t channel_number,
                                                        uint64_t mac_address,
                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_mac_address_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_mac_address (channel_number, 
									mac_address,
									obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_subnet_mask2 (ipmi_device_t *dev, 
                                                        uint8_t channel_number, 
                                                        uint32_t subnet_mask, 
                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_subnet_mask_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_subnet_mask (channel_number, 
									subnet_mask,
									obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control2 (ipmi_device_t *dev, 
                                                                      uint8_t channel_number, 
                                                                      uint8_t bmc_generated_gratuitous_arps, 
                                                                      uint8_t bmc_generated_arp_responses, 
                                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(bmc_generated_gratuitous_arps)
      || !IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(bmc_generated_arp_responses)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_bmc_generated_arp_control_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (channel_number, 
										      bmc_generated_gratuitous_arps, 
										      bmc_generated_arp_responses,
										      obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval2 (ipmi_device_t *dev, 
                                                                    uint8_t channel_number, 
                                                                    uint8_t gratuitous_arp_interval, 
                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_gratuitous_arp_interval_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_gratuitous_arp_interval (channel_number, 
										    gratuitous_arp_interval,
										    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_default_gateway_address2 (ipmi_device_t *dev, 
                                                                    uint8_t channel_number, 
                                                                    uint32_t ip_address, 
                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_ip_address_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_default_gateway_address (channel_number, 
										    ip_address,
										    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address2 (ipmi_device_t *dev, 
                                                                        uint8_t channel_number,
                                                                        uint64_t mac_address,
                                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_mac_address_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_default_gateway_mac_address (channel_number, 
											mac_address,
											obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address2 (ipmi_device_t *dev, 
                                                                   uint8_t channel_number, 
                                                                   uint32_t ip_address, 
                                                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_ip_address_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_backup_gateway_address (channel_number, 
										   ip_address,
										   obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address2 (ipmi_device_t *dev, 
                                                                       uint8_t channel_number,
                                                                       uint64_t mac_address,
                                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_mac_address_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (channel_number, 
										       mac_address,
										       obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_vlan_id2 (ipmi_device_t *dev, 
                                                    uint8_t channel_number, 
                                                    uint8_t vlan_id_ls,
                                                    uint8_t vlan_id_ms,
                                                    uint8_t vlan_id_enable, 
                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_VLAN_ID_ENABLE_VALID(vlan_id_enable)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_vlan_id_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_vlan_id (channel_number,
								    vlan_id_ls,
								    vlan_id_ms,
								    vlan_id_enable,
								    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_vlan_priority2 (ipmi_device_t *dev, 
                                                          uint8_t channel_number,
                                                          uint32_t vlan_priority,
                                                          fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_lan_configuration_parameters_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_lan_configuration_parameters_vlan_priority_rq);

  ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_vlan_priority (channel_number,
									  vlan_priority,
									  obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables2 (ipmi_device_t *dev, 
                                                                        uint8_t channel_number, 
                                                                        uint8_t get_parameter, 
                                                                        uint8_t set_selector, 
                                                                        uint8_t block_selector, 
                                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_authentication_type_enables_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_AUTHENTICATION_TYPE_ENABLES, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_ip_address2 (ipmi_device_t *dev, 
                                                       uint8_t channel_number,
                                                       uint8_t get_parameter,
                                                       uint8_t set_selector,
                                                       uint8_t block_selector,
                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;  
  int8_t rv = -1;

  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_ip_address_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_IP_ADDRESS, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_ip_address_source2 (ipmi_device_t *dev, 
                                                              uint8_t channel_number, 
                                                              uint8_t get_parameter, 
                                                              uint8_t set_selector, 
                                                              uint8_t block_selector, 
                                                              fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_ip_address_source_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_IP_ADDRESS_SOURCE, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_mac_address2 (ipmi_device_t *dev, 
                                                        uint8_t channel_number,
                                                        uint8_t get_parameter,
                                                        uint8_t set_selector,
                                                        uint8_t block_selector,
                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_mac_address_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_MAC_ADDRESS, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_subnet_mask2 (ipmi_device_t *dev, 
                                                        uint8_t channel_number,
                                                        uint8_t get_parameter,
                                                        uint8_t set_selector,
                                                        uint8_t block_selector,
                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_subnet_mask_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_SUBNET_MASK,                                
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control2 (ipmi_device_t *dev, 
                                                                      uint8_t channel_number, 
                                                                      uint8_t get_parameter, 
                                                                      uint8_t set_selector, 
                                                                      uint8_t block_selector, 
                                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_bmc_generated_arp_control_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_BMC_GENERATED_ARP_CONTROL, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval2 (ipmi_device_t *dev, 
                                                                    uint8_t channel_number, 
                                                                    uint8_t get_parameter, 
                                                                    uint8_t set_selector, 
                                                                    uint8_t block_selector, 
                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_gratuitous_arp_interval_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_GRATUITOUS_ARP_INTERVAL, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_default_gateway_address2 (ipmi_device_t *dev, 
                                                                    uint8_t channel_number,
                                                                    uint8_t get_parameter,
                                                                    uint8_t set_selector,
                                                                    uint8_t block_selector,
                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_default_gateway_address_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_DEFAULT_GATEWAY_ADDRESS, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address2 (ipmi_device_t *dev, 
                                                                        uint8_t channel_number,
                                                                        uint8_t get_parameter,
                                                                        uint8_t set_selector,
                                                                        uint8_t block_selector,
                                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_mac_address_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_DEFAULT_GATEWAY_MAC_ADDRESS, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address2 (ipmi_device_t *dev, 
                                                                   uint8_t channel_number,
                                                                   uint8_t get_parameter,
                                                                   uint8_t set_selector,
                                                                   uint8_t block_selector,
                                                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_backup_gateway_address_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_BACKUP_GATEWAY_ADDRESS, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address2 (ipmi_device_t *dev, 
                                                                       uint8_t channel_number,
                                                                       uint8_t get_parameter,
                                                                       uint8_t set_selector,
                                                                       uint8_t block_selector,
                                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_mac_address_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_BACKUP_GATEWAY_MAC_ADDRESS, 
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_vlan_id2 (ipmi_device_t *dev, 
                                                    uint8_t channel_number, 
                                                    uint8_t get_parameter, 
                                                    uint8_t set_selector, 
                                                    uint8_t block_selector, 
                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_vlan_id_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_VLAN_ID,
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_vlan_priority2 (ipmi_device_t *dev, 
                                                          uint8_t channel_number, 
                                                          uint8_t get_parameter, 
                                                          uint8_t set_selector, 
                                                          uint8_t block_selector, 
                                                          fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_lan_configuration_parameters_vlan_priority_rs);
  
  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_lan_configuration_parameters_rq);

  ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
							    get_parameter, 
							    IPMI_LAN_PARAM_VLAN_PRIORITY,
							    set_selector, 
							    block_selector,
							    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_suspend_bmc_arps2 (ipmi_device_t *dev, 
			    uint8_t channel_number, 
			    uint8_t gratuitous_arp_suspend, 
			    uint8_t arp_response_suspend, 
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_suspend_bmc_arps_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_suspend_bmc_arps_rq);

  ERR_CLEANUP (!(fill_cmd_suspend_bmc_arps (channel_number, 
					    gratuitous_arp_suspend, 
					    arp_response_suspend,
					    obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_TRANSPORT_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}
