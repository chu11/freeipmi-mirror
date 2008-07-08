/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/api/ipmi-lan-cmds-api.h"
#include "freeipmi/cmds/ipmi-lan-cmds.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-lan-parameter-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables (ipmi_ctx_t ctx, 
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
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_none)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_md2)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_md5)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_straight_password)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(callback_level_oem_proprietary)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_none)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_md2)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_md5)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_straight_password)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(user_level_oem_proprietary)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_none)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_md2)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_md5)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_straight_password)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(operator_level_oem_proprietary)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_none)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_md2)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_md5)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_straight_password)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(admin_level_oem_proprietary)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_none)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_md2)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_md5)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_straight_password)
                      && IPMI_AUTHENTICATION_TYPE_ENABLE_VALID(oem_level_oem_proprietary)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_authentication_type_enables_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_authentication_type_enables (channel_number, 
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

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_ip_address (ipmi_ctx_t ctx, 
						      uint8_t channel_number, 
						      uint32_t ip_address, 
						      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_ip_address_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_ip_address (channel_number, 
									   ip_address,
									   obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_ip_address_source (ipmi_ctx_t ctx, 
							     uint8_t channel_number, 
							     uint8_t ip_address_source, 
							     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_ip_address_source_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_ip_address_source (channel_number, 
										  ip_address_source,
										  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_mac_address (ipmi_ctx_t ctx, 
						       uint8_t channel_number,
						       uint64_t mac_address,
						       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_mac_address_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_mac_address (channel_number, 
									    mac_address,
									    obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_subnet_mask (ipmi_ctx_t ctx, 
						       uint8_t channel_number, 
						       uint32_t subnet_mask, 
						       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_subnet_mask_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_subnet_mask (channel_number, 
									    subnet_mask,
									    obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (ipmi_ctx_t ctx, 
								     uint8_t channel_number, 
								     uint8_t bmc_generated_gratuitous_arps, 
								     uint8_t bmc_generated_arp_responses, 
								     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(bmc_generated_gratuitous_arps)
                      && IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(bmc_generated_arp_responses)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_bmc_generated_arp_control_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (channel_number, 
											  bmc_generated_gratuitous_arps, 
											  bmc_generated_arp_responses,
											  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval (ipmi_ctx_t ctx, 
								   uint8_t channel_number, 
								   uint8_t gratuitous_arp_interval, 
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_gratuitous_arp_interval_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_gratuitous_arp_interval (channel_number, 
											gratuitous_arp_interval,
											obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_default_gateway_address (ipmi_ctx_t ctx, 
								   uint8_t channel_number, 
								   uint32_t ip_address, 
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_ip_address_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_default_gateway_address (channel_number, 
											ip_address,
											obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address (ipmi_ctx_t ctx, 
								       uint8_t channel_number,
								       uint64_t mac_address,
								       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_mac_address_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_default_gateway_mac_address (channel_number, 
											    mac_address,
											    obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address (ipmi_ctx_t ctx, 
								  uint8_t channel_number, 
								  uint32_t ip_address, 
								  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_ip_address_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_backup_gateway_address (channel_number, 
										       ip_address,
										       obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (ipmi_ctx_t ctx, 
								      uint8_t channel_number,
								      uint64_t mac_address,
								      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_mac_address_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (channel_number, 
											   mac_address,
											   obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_cmd_set_lan_configuration_parameters_community_string (ipmi_ctx_t ctx, 
                                                            uint8_t channel_number, 
                                                            char *community_string,
                                                            unsigned int community_string_len,
                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && !(community_string
                           && community_string_len > IPMI_MAX_COMMUNITY_STRING_LENGTH)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_community_string_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_community_string (channel_number, 
										 community_string,
										 community_string_len,
										 obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_cmd_set_lan_configuration_parameters_destination_type (ipmi_ctx_t ctx, 
                                                            uint8_t channel_number, 
                                                            uint8_t destination_selector,
                                                            uint8_t destination_type,
                                                            uint8_t alert_acknowledge,
                                                            uint8_t alert_acknowledge_timeout,
                                                            uint8_t retries,
                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_DESTINATION_SELECTOR_VALID(destination_selector)
                      && IPMI_DESTINATION_TYPE_VALID(destination_type)
                      && IPMI_ALERT_VALID(alert_acknowledge)
                      && (retries <= IPMI_ALERT_RETRIES_MAX)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_destination_type_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_destination_type (channel_number, 
										 destination_selector,
										 destination_type,
										 alert_acknowledge,
										 alert_acknowledge_timeout,
										 retries,
										 obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_cmd_set_lan_configuration_parameters_destination_addresses (ipmi_ctx_t ctx, 
                                                                 uint8_t channel_number, 
                                                                 uint8_t destination_selector,
                                                                 uint8_t gateway_selector,
                                                                 uint32_t alerting_ip_address,
                                                                 uint64_t alerting_mac_address,
                                                                 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_destination_addresses_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_destination_addresses (channel_number, 
										      destination_selector,
										      gateway_selector,
										      alerting_ip_address,
										      alerting_mac_address,
										      obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}



int8_t 
ipmi_cmd_set_lan_configuration_parameters_vlan_id (ipmi_ctx_t ctx, 
						   uint8_t channel_number, 
						   uint16_t vlan_id,
						   uint8_t vlan_id_enable, 
						   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_VLAN_ID_ENABLE_VALID(vlan_id_enable)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_vlan_id_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_vlan_id (channel_number,
									vlan_id,
									vlan_id_enable,
									obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_vlan_priority (ipmi_ctx_t ctx, 
							 uint8_t channel_number,
							 uint32_t vlan_priority,
							 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_vlan_priority_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_vlan_priority (channel_number,
									      vlan_priority,
									      obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels(ipmi_ctx_t ctx, 
											   uint8_t channel_number,
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
											   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_1)
                          || !maximum_privilege_for_cipher_suite_1)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_2)
                          || !maximum_privilege_for_cipher_suite_2)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_3)
                          || !maximum_privilege_for_cipher_suite_3)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_4)
                          || !maximum_privilege_for_cipher_suite_4)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_5)
                          || !maximum_privilege_for_cipher_suite_5)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_6)
                          || !maximum_privilege_for_cipher_suite_6)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_7)
                          || !maximum_privilege_for_cipher_suite_7)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_8)
                          || !maximum_privilege_for_cipher_suite_8)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_9)
                          || !maximum_privilege_for_cipher_suite_9)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_10)
                          || !maximum_privilege_for_cipher_suite_10)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_11)
                          || !maximum_privilege_for_cipher_suite_11)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_12)
                          || !maximum_privilege_for_cipher_suite_12)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_13)
                          || !maximum_privilege_for_cipher_suite_13)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_14)
                          || !maximum_privilege_for_cipher_suite_14)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_15)
                          || !maximum_privilege_for_cipher_suite_15)
                      && (IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_for_cipher_suite_16)
                          || !maximum_privilege_for_cipher_suite_16)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);
 
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels(channel_number,
														maximum_privilege_for_cipher_suite_1,
														maximum_privilege_for_cipher_suite_2,
														maximum_privilege_for_cipher_suite_3,
														maximum_privilege_for_cipher_suite_4,
														maximum_privilege_for_cipher_suite_5,
														maximum_privilege_for_cipher_suite_6,
														maximum_privilege_for_cipher_suite_7,
														maximum_privilege_for_cipher_suite_8,
														maximum_privilege_for_cipher_suite_9,
														maximum_privilege_for_cipher_suite_10,
														maximum_privilege_for_cipher_suite_11,
														maximum_privilege_for_cipher_suite_12,
														maximum_privilege_for_cipher_suite_13,
														maximum_privilege_for_cipher_suite_14,
														maximum_privilege_for_cipher_suite_15,
														maximum_privilege_for_cipher_suite_16,
														obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);


}

int8_t
ipmi_cmd_get_lan_configuration_parameters_authentication_type_support (ipmi_ctx_t ctx, 
								       uint8_t channel_number, 
								       uint8_t get_parameter, 
								       uint8_t set_selector, 
								       uint8_t block_selector, 
								       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_authentication_type_support_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_AUTHENTICATION_TYPE_SUPPORT, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables (ipmi_ctx_t ctx, 
								       uint8_t channel_number, 
								       uint8_t get_parameter, 
								       uint8_t set_selector, 
								       uint8_t block_selector, 
								       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_AUTHENTICATION_TYPE_ENABLES, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_ip_address (ipmi_ctx_t ctx, 
						      uint8_t channel_number,
						      uint8_t get_parameter,
						      uint8_t set_selector,
						      uint8_t block_selector,
						      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;  
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_ip_address_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_IP_ADDRESS, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_ip_address_source (ipmi_ctx_t ctx, 
							     uint8_t channel_number, 
							     uint8_t get_parameter, 
							     uint8_t set_selector, 
							     uint8_t block_selector, 
							     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_IP_ADDRESS_SOURCE, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_mac_address (ipmi_ctx_t ctx, 
						       uint8_t channel_number,
						       uint8_t get_parameter,
						       uint8_t set_selector,
						       uint8_t block_selector,
						       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_mac_address_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_MAC_ADDRESS, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_subnet_mask (ipmi_ctx_t ctx, 
						       uint8_t channel_number,
						       uint8_t get_parameter,
						       uint8_t set_selector,
						       uint8_t block_selector,
						       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_SUBNET_MASK,                                
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control (ipmi_ctx_t ctx, 
								     uint8_t channel_number, 
								     uint8_t get_parameter, 
								     uint8_t set_selector, 
								     uint8_t block_selector, 
								     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_BMC_GENERATED_ARP_CONTROL, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval (ipmi_ctx_t ctx, 
								   uint8_t channel_number, 
								   uint8_t get_parameter, 
								   uint8_t set_selector, 
								   uint8_t block_selector, 
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_GRATUITOUS_ARP_INTERVAL, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_default_gateway_address (ipmi_ctx_t ctx, 
								   uint8_t channel_number,
								   uint8_t get_parameter,
								   uint8_t set_selector,
								   uint8_t block_selector,
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_DEFAULT_GATEWAY_ADDRESS, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address (ipmi_ctx_t ctx, 
								       uint8_t channel_number,
								       uint8_t get_parameter,
								       uint8_t set_selector,
								       uint8_t block_selector,
								       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_mac_address_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_DEFAULT_GATEWAY_MAC_ADDRESS, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address (ipmi_ctx_t ctx, 
								  uint8_t channel_number,
								  uint8_t get_parameter,
								  uint8_t set_selector,
								  uint8_t block_selector,
								  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_BACKUP_GATEWAY_ADDRESS, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address (ipmi_ctx_t ctx, 
								      uint8_t channel_number,
								      uint8_t get_parameter,
								      uint8_t set_selector,
								      uint8_t block_selector,
								      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_mac_address_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_BACKUP_GATEWAY_MAC_ADDRESS, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_community_string (ipmi_ctx_t ctx, 
                                                            uint8_t channel_number, 
                                                            uint8_t get_parameter, 
                                                            uint8_t set_selector, 
                                                            uint8_t block_selector, 
                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_community_string_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_COMMUNITY_STRING,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_number_of_destinations (ipmi_ctx_t ctx, 
                                                                  uint8_t channel_number, 
                                                                  uint8_t get_parameter, 
                                                                  uint8_t set_selector, 
                                                                  uint8_t block_selector, 
                                                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_NUMBER_OF_DESTINATIONS,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_destination_type (ipmi_ctx_t ctx, 
                                                            uint8_t channel_number, 
                                                            uint8_t get_parameter, 
                                                            uint8_t set_selector, 
                                                            uint8_t block_selector, 
                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && IPMI_DESTINATION_SELECTOR_VALID(set_selector)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_destination_type_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_DESTINATION_TYPE,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_destination_addresses (ipmi_ctx_t ctx, 
                                                                 uint8_t channel_number, 
                                                                 uint8_t get_parameter, 
                                                                 uint8_t set_selector, 
                                                                 uint8_t block_selector, 
                                                                 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && IPMI_DESTINATION_SELECTOR_VALID(set_selector)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_DESTINATION_ADDRESSES,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_vlan_id (ipmi_ctx_t ctx, 
						   uint8_t channel_number, 
						   uint8_t get_parameter, 
						   uint8_t set_selector, 
						   uint8_t block_selector, 
						   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_VLAN_ID,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_vlan_priority (ipmi_ctx_t ctx, 
							 uint8_t channel_number, 
							 uint8_t get_parameter, 
							 uint8_t set_selector, 
							 uint8_t block_selector, 
							 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_VLAN_PRIORITY,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support (ipmi_ctx_t ctx, 
											 uint8_t channel_number,
											 uint8_t get_parameter, 
											 uint8_t set_selector, 
											 uint8_t block_selector, 
											 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_ENTRY_SUPPORT,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries (ipmi_ctx_t ctx, 
										   uint8_t channel_number,
										   uint8_t get_parameter, 
										   uint8_t set_selector, 
										   uint8_t block_selector, 
										   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_ENTRIES,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels(ipmi_ctx_t ctx, 
											   uint8_t channel_number,
											   uint8_t get_parameter, 
											   uint8_t set_selector, 
											   uint8_t block_selector, 
											   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_GET_LAN_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_lan_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_lan_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_LAN_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_PRIVILEGE_LEVELS,
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_suspend_bmc_arps (ipmi_ctx_t ctx, 
			   uint8_t channel_number, 
			   uint8_t gratuitous_arp_suspend, 
			   uint8_t arp_response_suspend, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID(gratuitous_arp_suspend)
                      && IPMI_BMC_GENERATED_ARP_RESPONSE_VALID(arp_response_suspend)
                      && fiid_obj_valid(obj_cmd_rs));
   
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_suspend_bmc_arps_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_suspend_bmc_arps_rq);

  API_ERR_CLEANUP (!(fill_cmd_suspend_bmc_arps (channel_number, 
						gratuitous_arp_suspend, 
						arp_response_suspend,
						obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
 
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_ip_udp_rmcp_statistics (ipmi_ctx_t ctx, 
                                     uint8_t channel_number, 
                                     uint8_t clear_all_statistics, 
                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_CLEAR_ALL_STATISTICS_VALID(clear_all_statistics)
                      && fiid_obj_valid(obj_cmd_rs));
   
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_ip_udp_rmcp_statistics_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_ip_udp_rmcp_statistics_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_ip_udp_rmcp_statistics (channel_number, 
                                                          clear_all_statistics,
                                                          obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_TRANSPORT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
 
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}
