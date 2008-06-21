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

#include "freeipmi/api/ipmi-sol-cmds-api.h"
#include "freeipmi/cmds/ipmi-sol-cmds.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-sol-parameter-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cmd_set_sol_configuration_parameters_sol_enable (ipmi_ctx_t ctx, 
						      uint8_t channel_number, 
						      uint8_t sol_enable, 
						      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_SOL_SOL_ENABLE_VALID(sol_enable)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sol_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_enable_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_sol_configuration_parameters_sol_enable (channel_number, 
									   sol_enable,
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
ipmi_cmd_set_sol_configuration_parameters_sol_authentication (ipmi_ctx_t ctx, 
                                                              uint8_t channel_number, 
                                                              uint8_t sol_privilege_level,
                                                              uint8_t force_sol_payload_authentication,
                                                              uint8_t force_sol_payload_encryption,
                                                              fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_PRIVILEGE_LEVEL_VALID(sol_privilege_level)
                      && IPMI_SOL_FORCE_SOL_PAYLOAD_AUTHENTICATION_VALID(force_sol_payload_authentication)
                      && IPMI_SOL_FORCE_SOL_PAYLOAD_ENCRYPTION_VALID(force_sol_payload_encryption)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sol_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_authentication_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_sol_configuration_parameters_sol_authentication (channel_number, 
										   sol_privilege_level,
										   force_sol_payload_authentication,
										   force_sol_payload_encryption,
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
ipmi_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (ipmi_ctx_t ctx, 
                                                                                            uint8_t channel_number, 
                                                                                            uint8_t character_accumulate_interval,
                                                                                            uint8_t character_send_threshold,
                                                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sol_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (channel_number, 
														 character_accumulate_interval,
														 character_send_threshold,
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
ipmi_cmd_set_sol_configuration_parameters_sol_retry (ipmi_ctx_t ctx, 
                                                     uint8_t channel_number, 
                                                     uint8_t retry_count,
                                                     uint8_t retry_interval,
                                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sol_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_retry_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_sol_configuration_parameters_sol_retry (channel_number, 
									  retry_count,
									  retry_interval,
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
ipmi_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (ipmi_ctx_t ctx, 
                                                                     uint8_t channel_number, 
                                                                     uint8_t bit_rate,
                                                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_SOL_BIT_RATE_VALID(bit_rate)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sol_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (channel_number, 
											  bit_rate,
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
ipmi_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (ipmi_ctx_t ctx, 
                                                                 uint8_t channel_number, 
                                                                 uint8_t bit_rate,
                                                                 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_SOL_BIT_RATE_VALID(bit_rate)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sol_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (channel_number, 
										      bit_rate,
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
ipmi_cmd_set_sol_configuration_parameters_sol_payload_port_number (ipmi_ctx_t ctx, 
                                                                   uint8_t channel_number, 
                                                                   uint16_t port_number,
                                                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sol_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_payload_port_number_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_sol_configuration_parameters_sol_payload_port_number (channel_number, 
											port_number,
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
ipmi_cmd_get_sol_configuration_parameters_sol_enable (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_enable_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_SOL_ENABLE, 
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
ipmi_cmd_get_sol_configuration_parameters_sol_authentication (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_authentication_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_SOL_AUTHENTICATION, 
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
ipmi_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_CHARACTER_ACCUMULATE_INTERVAL_AND_SEND_THRESHOLD, 
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
ipmi_cmd_get_sol_configuration_parameters_sol_retry (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_retry_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_SOL_RETRY, 
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
ipmi_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_SOL_NON_VOLATILE_BIT_RATE, 
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
ipmi_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_SOL_VOLATILE_BIT_RATE, 
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
ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_SOL_PAYLOAD_CHANNEL, 
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
ipmi_cmd_get_sol_configuration_parameters_sol_payload_port_number (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sol_configuration_parameters_sol_payload_port_number_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sol_configuration_parameters (channel_number, 
								get_parameter, 
								IPMI_SOL_PARAMETER_SOL_PAYLOAD_PORT_NUMBER, 
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
