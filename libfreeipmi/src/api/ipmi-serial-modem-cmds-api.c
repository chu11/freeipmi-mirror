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

#include "freeipmi/api/ipmi-serial-modem-cmds-api.h"
#include "freeipmi/cmds/ipmi-serial-modem-cmds.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-serial-modem-parameter-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cmd_set_serial_modem_configuration_connection_mode (ipmi_ctx_t ctx, 
							 uint8_t channel_number, 
							 uint8_t basic_mode,
							 uint8_t ppp_mode,
							 uint8_t terminal_mode,
							 uint8_t connect_mode,
							 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_BASIC_MODE_VALID(basic_mode)
                      && IPMI_PPP_MODE_VALID(ppp_mode)
                      && IPMI_TERMINAL_MODE_VALID(terminal_mode)
                      && IPMI_CONNECT_MODE_VALID(connect_mode)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_serial_modem_configuration_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_serial_modem_configuration_connection_mode_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_connection_mode (channel_number, 
									      basic_mode, 
									      ppp_mode, 
									      terminal_mode, 
									      connect_mode,
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
ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (ipmi_ctx_t ctx, 
								      uint8_t channel_number, 
								      uint8_t dtr_hangup,
								      uint8_t flow_control,
								      uint8_t bit_rate,
								      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_DTR_HANGUP_VALID(dtr_hangup)
                      && IPMI_FLOW_CONTROL_VALID(flow_control)
                      && IPMI_BIT_RATE_VALID(bit_rate)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_serial_modem_configuration_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (channel_number, 
											   dtr_hangup,
											   flow_control,
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
ipmi_cmd_set_serial_modem_configuration_page_blackout_interval (ipmi_ctx_t ctx, 
								uint8_t channel_number, 
								uint8_t page_blackout_interval, 
								fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_serial_modem_configuration_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_serial_modem_configuration_page_blackout_interval_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_page_blackout_interval (channel_number, 
										     page_blackout_interval,
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
ipmi_cmd_set_serial_modem_configuration_call_retry_interval (ipmi_ctx_t ctx, 
							     uint8_t channel_number, 
							     uint8_t call_retry_interval, 
							     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_serial_modem_configuration_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_serial_modem_configuration_call_retry_interval_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_call_retry_interval (channel_number, 
										  call_retry_interval,
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
ipmi_cmd_get_serial_modem_configuration_connection_mode (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_serial_modem_configuration_connection_mode_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_serial_modem_configuration_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							      get_parameter, 
							      IPMI_SERIAL_MODEM_PARAMETER_CONNECTION_MODE, 
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
ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_serial_modem_configuration_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							      get_parameter, 
							      IPMI_SERIAL_MODEM_PARAMETER_IPMI_MESSAGING_COMM_SETTINGS, 
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
ipmi_cmd_get_serial_modem_configuration_call_retry_interval (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_serial_modem_configuration_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							      get_parameter, 
							      IPMI_SERIAL_MODEM_PARAMETER_CALL_RETRY_INTERVAL, 
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
ipmi_cmd_get_serial_modem_configuration_page_blackout_interval (ipmi_ctx_t ctx, 
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
                      && IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_serial_modem_configuration_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							      get_parameter, 
							      IPMI_SERIAL_MODEM_PARAMETER_PAGE_BLACKOUT_INTERVAL, 
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

