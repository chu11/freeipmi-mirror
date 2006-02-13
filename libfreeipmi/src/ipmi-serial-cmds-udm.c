/* 
   ipmi-serial-cmds-udm.c - IPMI UDM serial port settings commands

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

#include "freeipmi.h"

int8_t 
ipmi_cmd_set_serial_connmode2 (ipmi_device_t *dev, 
			       uint8_t channel_number, 
			       uint8_t basic_mode_enable,
			       uint8_t ppp_mode_enable,
			       uint8_t terminal_mode_enable,
			       uint8_t direct,
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_connmode_rq);
  ERR (fill_set_serial_connmode (channel_number, 
				 basic_mode_enable, 
				 ppp_mode_enable, 
				 terminal_mode_enable, 
				 direct,
                                 obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_serial_conf_param_connmode_rq, 
		 obj_cmd_rs, 
		 tmpl_set_serial_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_serial_page_blackout_interval2 (ipmi_device_t *dev, 
					     uint8_t channel_number, 
					     uint8_t page_blackout_interval, 
					     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_pageblackout_rq);
  ERR (fill_set_serial_page_blackout_interval (channel_number, 
					       page_blackout_interval,
                                               obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_serial_conf_param_pageblackout_rq, 
		 obj_cmd_rs, 
		 tmpl_set_serial_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_serial_retry_time2 (ipmi_device_t *dev, 
				 uint8_t channel_number, 
				 uint8_t retry_time, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_retry_rq);
  ERR (fill_set_serial_retry_time (channel_number, 
				   retry_time,
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_serial_conf_param_retry_rq, 
		 obj_cmd_rs, 
		 tmpl_set_serial_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_serial_comm_bits2 (ipmi_device_t *dev, 
				uint8_t channel_number, 
				uint8_t dtr_hangup,
				uint8_t flow_control,
				uint8_t bit_rate,
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_commbits_rq);
  ERR (fill_set_serial_comm_bits (channel_number, 
				  dtr_hangup,
				  flow_control,
				  bit_rate,
                                  obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_serial_conf_param_commbits_rq, 
		 obj_cmd_rs, 
		 tmpl_set_serial_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_serial_connmode2 (ipmi_device_t *dev, 
			       uint8_t channel_number,
			       uint8_t parameter_type,
			       uint8_t set_selector,
			       uint8_t block_selector,
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_CONNECTION_MODE, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector,
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_serial_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_serial_conf_param_connmode_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_serial_page_blackout2 (ipmi_device_t *dev, 
				    uint8_t channel_number,
				    uint8_t parameter_type,
				    uint8_t set_selector,
				    uint8_t block_selector,
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_PAGE_BLACKOUT_INTERVAL, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector,
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_serial_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_serial_conf_param_pageblackout_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_serial_retry_time2 (ipmi_device_t *dev, 
				 uint8_t channel_number,
				 uint8_t parameter_type,
				 uint8_t set_selector,
				 uint8_t block_selector,
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_RETRY_TIME, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector,
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_serial_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_serial_conf_param_retry_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_serial_comm_bits2 (ipmi_device_t *dev, 
				uint8_t channel_number,
				uint8_t parameter_type,
				uint8_t set_selector,
				uint8_t block_selector,
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_COMM_BITS, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector,
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_serial_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_serial_conf_param_commbits_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}
