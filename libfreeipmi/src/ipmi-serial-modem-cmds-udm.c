/* 
   ipmi-serial-modem-cmds-udm.c - IPMI UDM serial port settings commands

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

#include "ipmi-serial-modem-cmds-udm.h"
#include "ipmi-serial-modem-cmds.h"
#include "ipmi-serial-modem-param-spec.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "ipmi-netfn-spec.h"
#include "ipmi-ipmb-interface.h"

int8_t 
ipmi_cmd_set_serial_modem_configuration_connection_mode2 (ipmi_device_t *dev, 
                                                          uint8_t channel_number, 
                                                          uint8_t basic_mode,
                                                          uint8_t ppp_mode,
                                                          uint8_t terminal_mode,
                                                          uint8_t connect_mode,
                                                          fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_BASIC_MODE_VALID(basic_mode)
      || !IPMI_PPP_MODE_VALID(ppp_mode)
      || !IPMI_TERMINAL_MODE_VALID(terminal_mode)
      || !IPMI_CONNECT_MODE_VALID(connect_mode)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_serial_modem_configuration_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_serial_modem_configuration_connection_mode_rq);
  
  ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_connection_mode (channel_number, 
									  basic_mode, 
									  ppp_mode, 
									  terminal_mode, 
									  connect_mode,
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
ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings2 (ipmi_device_t *dev, 
								       uint8_t channel_number, 
								       uint8_t dtr_hangup,
								       uint8_t flow_control,
								       uint8_t bit_rate,
								       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_DTR_HANGUP_VALID(dtr_hangup)
      || !IPMI_FLOW_CONTROL_VALID(flow_control)
      || !IPMI_BIT_RATE_VALID(bit_rate)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_serial_modem_configuration_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_serial_modem_configuration_ipmi_messaging_comm_settings_rq);

  ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (channel_number, 
										       dtr_hangup,
										       flow_control,
										       bit_rate,
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
ipmi_cmd_set_serial_modem_configuration_page_blackout_interval2 (ipmi_device_t *dev, 
								 uint8_t channel_number, 
								 uint8_t page_blackout_interval, 
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
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_serial_modem_configuration_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_serial_modem_configuration_page_blackout_interval_rq);

  ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_page_blackout_interval (channel_number, 
										 page_blackout_interval,
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
ipmi_cmd_set_serial_modem_configuration_call_retry_interval2 (ipmi_device_t *dev, 
							      uint8_t channel_number, 
							      uint8_t call_retry_interval, 
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
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_serial_modem_configuration_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_set_serial_modem_configuration_call_retry_interval_rq);

  ERR_CLEANUP (!(fill_cmd_set_serial_modem_configuration_call_retry_interval (channel_number, 
									      call_retry_interval,
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
ipmi_cmd_get_serial_modem_configuration_connection_mode2 (ipmi_device_t *dev, 
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
      || !IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_serial_modem_configuration_connection_mode_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_serial_modem_configuration_rq);

  ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							  get_parameter, 
							  IPMI_SERIAL_MODEM_PARAM_CONNECTION_MODE, 
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
ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings2 (ipmi_device_t *dev, 
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
      || !IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_serial_modem_configuration_rq);

  ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							  get_parameter, 
							  IPMI_SERIAL_MODEM_PARAM_IPMI_MESSAGING_COMM_SETTINGS, 
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
ipmi_cmd_get_serial_modem_configuration_call_retry_interval2 (ipmi_device_t *dev, 
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
      || !IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_serial_modem_configuration_call_retry_interval_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_serial_modem_configuration_rq);

  ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							  get_parameter, 
							  IPMI_SERIAL_MODEM_PARAM_CALL_RETRY_INTERVAL, 
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
ipmi_cmd_get_serial_modem_configuration_page_blackout_interval2 (ipmi_device_t *dev, 
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
      || !IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_serial_modem_configuration_page_blackout_interval_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_serial_modem_configuration_rq);

  ERR_CLEANUP (!(fill_cmd_get_serial_modem_configuration (channel_number, 
							  get_parameter, 
							  IPMI_SERIAL_MODEM_PARAM_PAGE_BLACKOUT_INTERVAL, 
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

