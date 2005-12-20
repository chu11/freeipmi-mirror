/* 
   ipmi-serial-cmds.c - IPMI serial port settings commands

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

fiid_template_t tmpl_set_serial_conf_param_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {0, ""}
  };

fiid_template_t tmpl_get_serial_conf_param_rq =
  {
    {8, "cmd"},

    {4, "channel_number"},
    {3, "reserved"},
    {1, "parameter_type"},

    {8, "parameter_selector"},
    {8, "set_selector"},
    {8, "block_selector"},

    {0, ""}
  };

fiid_template_t tmpl_set_serial_conf_param_connmode_rq =
  {
    {8, "cmd"},
    
    {4, "channel_number"},
    {4, "reserved"},
    
    {8, "parameter_selector"}, 
    
    {1, "basic_mode_enable"},
    {1, "ppp_mode_enable"},
    {1, "terminal_mode_enable"},
    {4, "reserved2"},
    {1, "direct"},
    
    {0, ""}
  };

fiid_template_t tmpl_get_serial_conf_param_connmode_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {1, "basic_mode_enable"},
    {1, "ppp_mode_enable"},
    {1, "terminal_mode_enable"},
    {4, "reserved2"},
    {1, "direct"},

    {0, ""}
  };

fiid_template_t tmpl_set_serial_conf_param_pageblackout_rq =
  {
    {8, "cmd"},

    {4, "channel_number"},
    {4, "reserved"},
    
    {8, "parameter_selector"}, 
    
    {8, "page_blackout_interval"},

    {0, ""}
  };

fiid_template_t tmpl_get_serial_conf_param_pageblackout_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {8, "page_blackout_interval"},

    {0, ""}
  };

fiid_template_t tmpl_set_serial_conf_param_retry_rq =
  {
    {8, "cmd"},
    
    {4, "channel_number"},
    {4, "reserved"},
    
    {8, "parameter_selector"}, 
    
    {8, "retry_time"},
    
    {0, ""}
  };

fiid_template_t tmpl_get_serial_conf_param_retry_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {8, "retry_time"},

    {0, ""}
  };

fiid_template_t tmpl_set_serial_conf_param_commbits_rq =
  {
    {8, "cmd"},
    
    {4, "channel_number"},
    {4, "reserved"},
    
    {8, "parameter_selector"}, 
    
    {5, "reserved2"},
    {1, "dtr_hangup"},
    {2, "flow_control"},
    
    {4, "bit_rate"},
    {4, "reserved3"},
    
    {0, ""}
  };
    
fiid_template_t tmpl_get_serial_conf_param_commbits_rs =
  {
    {8, "cmd"},

    {8, "comp_code"},

    {4, "present_revision"},
    {4, "oldest_revision_parameter"},

    {5, "reserved2"},
    {1, "dtr_hangup"},
    {2, "flow_control"},

    {4, "bit_rate"},
    {4, "reserved3"},

    {0, ""}
  };

int 
fill_set_serial_connmode (fiid_obj_t obj_data_rq, 
			  uint8_t channel_number, 
			  uint8_t basic_mode_enable,
			  uint8_t ppp_mode_enable,
			  uint8_t terminal_mode_enable,
			  uint8_t direct)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_connmode_rq, 
		"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_connmode_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_connmode_rq, 
		"parameter_selector", 
		IPMI_SERIAL_PARAM_CONNECTION_MODE);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_connmode_rq, 
		"basic_mode_enable", 
		basic_mode_enable);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_connmode_rq, 
		"ppp_mode_enable", 
		ppp_mode_enable);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_connmode_rq, 
		"terminal_mode_enable", 
		terminal_mode_enable);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_connmode_rq, 
		"direct", 
		direct);
  
  return 0;
}

int8_t 
ipmi_set_serial_connmode (uint8_t channel_number, 
			  uint8_t basic_mode_enable,
			  uint8_t ppp_mode_enable,
			  uint8_t terminal_mode_enable,
			  uint8_t direct,
			  fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_set_serial_conf_param_connmode_rq);
  ERR (fill_set_serial_connmode (obj_data_rq, 
				 channel_number, 
				 basic_mode_enable, 
				 ppp_mode_enable, 
				 terminal_mode_enable, 
				 direct) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_serial_conf_param_connmode_rq, 
			 obj_data_rs, tmpl_set_serial_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_set_serial_page_blackout_interval (fiid_obj_t obj_data_rq, 
					uint8_t channel_number, 
					uint8_t page_blackout_interval)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_pageblackout_rq, 
		"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_pageblackout_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_pageblackout_rq, 
		"parameter_selector", 
		IPMI_SERIAL_PARAM_PAGE_BLACKOUT_INTERVAL);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_pageblackout_rq, 
		"page_blackout_interval", 
		page_blackout_interval);
  
  return 0;
}

int8_t 
ipmi_set_serial_page_blackout_interval (uint8_t channel_number, 
					uint8_t page_blackout_interval, 
					fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_set_serial_conf_param_pageblackout_rq);
  ERR (fill_set_serial_page_blackout_interval (obj_data_rq, 
					       channel_number, 
					       page_blackout_interval) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_serial_conf_param_pageblackout_rq, 
			 obj_data_rs, tmpl_set_serial_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_set_serial_retry_time (fiid_obj_t obj_data_rq, 
			    uint8_t channel_number, 
			    uint8_t retry_time)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_retry_rq, 
		"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_retry_rq, 
		"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_retry_rq, 
		"parameter_selector", 
		IPMI_SERIAL_PARAM_RETRY_TIME);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_retry_rq, 
		"retry_time", 
		retry_time);
  
  return 0;
}	    

int8_t 
ipmi_set_serial_retry_time (uint8_t channel_number, 
			    uint8_t retry_time, 
			    fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_set_serial_conf_param_retry_rq);
  ERR (fill_set_serial_retry_time (obj_data_rq, 
				   channel_number, 
				   retry_time) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_serial_conf_param_retry_rq, 
			 obj_data_rs, tmpl_set_serial_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_set_serial_comm_bits (fiid_obj_t obj_data_rq, 
			   uint8_t channel_number, 
                           uint8_t dtr_hangup,
                           uint8_t flow_control,
                           uint8_t bit_rate)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_commbits_rq, 
		"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_commbits_rq, 
		"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_commbits_rq, 
		"parameter_selector", 
		IPMI_SERIAL_PARAM_COMM_BITS);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_commbits_rq, 
		"dtr_hangup", 
		dtr_hangup);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_commbits_rq, 
		"flow_control", 
		flow_control);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_set_serial_conf_param_commbits_rq, 
		"bit_rate", 
		bit_rate);
  
  return 0;
}

int8_t 
ipmi_set_serial_comm_bits (uint8_t channel_number, 
                           uint8_t dtr_hangup,
                           uint8_t flow_control,
                           uint8_t bit_rate,
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_set_serial_conf_param_commbits_rq);
  ERR (fill_set_serial_comm_bits (obj_data_rq, 
				  channel_number, 
				  dtr_hangup,
				  flow_control,
				  bit_rate) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_set_serial_conf_param_commbits_rq, 
			 obj_data_rs, tmpl_set_serial_conf_param_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_get_serial_conf_param (fiid_obj_t obj_data_rq, 
			    uint8_t parameter_selector, 
			    uint8_t channel_number,
			    uint8_t parameter_type,
			    uint8_t set_selector,
			    uint8_t block_selector)
{
  if (obj_data_rq == NULL
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_serial_conf_param_rq, 
		"cmd", 
		IPMI_CMD_GET_SERIAL_MODEM_CONF_PARAM);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_serial_conf_param_rq, 
		"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_serial_conf_param_rq, 
		"parameter_type", 
		parameter_type);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_serial_conf_param_rq, 
		"parameter_selector", 
		parameter_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_serial_conf_param_rq, 
		"set_selector", 
		set_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_serial_conf_param_rq, 
		"block_selector", 
		block_selector);
  
  return 0;
}

int8_t 
ipmi_get_serial_connmode (uint8_t channel_number,
                          uint8_t parameter_type,
                          uint8_t set_selector,
                          uint8_t block_selector,
                          fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_data_rq, 
				   IPMI_SERIAL_PARAM_CONNECTION_MODE, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_serial_conf_param_rq, 
			 obj_data_rs, tmpl_get_serial_conf_param_connmode_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_get_serial_page_blackout (uint8_t channel_number,
			       uint8_t parameter_type,
			       uint8_t set_selector,
			       uint8_t block_selector,
			       fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_data_rq, 
				   IPMI_SERIAL_PARAM_PAGE_BLACKOUT_INTERVAL, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_serial_conf_param_rq, 
			 obj_data_rs, tmpl_get_serial_conf_param_pageblackout_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_get_serial_retry_time (uint8_t channel_number,
			    uint8_t parameter_type,
			    uint8_t set_selector,
			    uint8_t block_selector,
			    fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_data_rq, 
				   IPMI_SERIAL_PARAM_RETRY_TIME, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_serial_conf_param_rq, 
			 obj_data_rs, tmpl_get_serial_conf_param_retry_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_get_serial_comm_bits (uint8_t channel_number,
			   uint8_t parameter_type,
			   uint8_t set_selector,
			   uint8_t block_selector,
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_calloc (tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_data_rq, 
				   IPMI_SERIAL_PARAM_COMM_BITS, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_TRANSPORT_RQ, 
			 obj_data_rq, tmpl_get_serial_conf_param_rq, 
			 obj_data_rs, tmpl_get_serial_conf_param_commbits_rs);
  free (obj_data_rq);
  return status;
}

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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_connmode_rq);
  ERR (fill_set_serial_connmode (obj_cmd_rq, 
				 channel_number, 
				 basic_mode_enable, 
				 ppp_mode_enable, 
				 terminal_mode_enable, 
				 direct) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_pageblackout_rq);
  ERR (fill_set_serial_page_blackout_interval (obj_cmd_rq, 
					       channel_number, 
					       page_blackout_interval) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_retry_rq);
  ERR (fill_set_serial_retry_time (obj_cmd_rq, 
				   channel_number, 
				   retry_time) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_serial_conf_param_commbits_rq);
  ERR (fill_set_serial_comm_bits (obj_cmd_rq, 
				  channel_number, 
				  dtr_hangup,
				  flow_control,
				  bit_rate) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_cmd_rq, 
				   IPMI_SERIAL_PARAM_CONNECTION_MODE, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_cmd_rq, 
				   IPMI_SERIAL_PARAM_PAGE_BLACKOUT_INTERVAL, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_cmd_rq, 
				   IPMI_SERIAL_PARAM_RETRY_TIME, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
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
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_serial_conf_param_rq);
  ERR (fill_get_serial_conf_param (obj_cmd_rq, 
				   IPMI_SERIAL_PARAM_COMM_BITS, 
				   channel_number, 
				   parameter_type, 
				   set_selector, 
				   block_selector) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_TRANSPORT_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_get_serial_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_serial_conf_param_commbits_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

