/*   ipmi-pef-cmds-udm.c - IPMI UDM Platform Event Filtering Commands 
   
  Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

$Id: ipmi-pef-cmds-udm.c,v 1.1.2.1 2006-02-10 17:00:33 chu11 Exp $  */

#include "freeipmi.h"

int8_t 
ipmi_cmd_set_pef_control2 (ipmi_device_t *dev, 
			   uint8_t enable_pef, 
			   uint8_t enable_pef_event_msgs, 
			   uint8_t enable_startup_delay, 
			   uint8_t enable_alert_startup_delay, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_pef_control_rq);
  ERR (fill_kcs_set_pef_control (enable_pef, 
				 enable_pef_event_msgs,
				 enable_startup_delay, 
				 enable_alert_startup_delay,
                                 obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_pef_control_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_global_action_control2 (ipmi_device_t *dev, 
				     uint8_t enable_alert,
				     uint8_t enable_powerdown, 
				     uint8_t enable_reset,
				     uint8_t enable_powercycle, 
				     uint8_t enable_oem,
				     uint8_t enable_diag_interrupt, 
				     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_global_action_control_rq);
  ERR (fill_kcs_set_global_action_control (enable_alert, 
					   enable_powerdown,
					   enable_reset, 
					   enable_powercycle, 
					   enable_oem, 
					   enable_diag_interrupt,
                                           obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_global_action_control_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_startup_delay2 (ipmi_device_t *dev, 
			     uint8_t startup_delay, 
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_startup_delay_rq);
  ERR (fill_kcs_set_startup_delay (startup_delay, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_startup_delay_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_alert_startup_delay2 (ipmi_device_t *dev, 
				   uint8_t alert_startup_delay, 
				   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_alert_startup_delay_rq);
  ERR (fill_kcs_set_alert_startup_delay (alert_startup_delay,
                                         obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_alert_startup_delay_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_num_event_filters2 (ipmi_device_t *dev, 
				 uint8_t num_event_filters, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_num_event_filters_rq);
  ERR (fill_kcs_set_num_event_filters (num_event_filters,
                                       obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_num_event_filters_rq, 
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_filter_table_entry2 (ipmi_device_t *dev, 
				  const event_filter_table_entry_t *eft_entry, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_event_filter_table_rq);
  ERR (fill_kcs_set_filter_table_entry (eft_entry,
                                        obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_event_filter_table_rq, 
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_filter_table_data1_2 (ipmi_device_t *dev, 
				   uint8_t filter_number,
				   filter_type_t filter_type, 
				   uint8_t enabled, 
				   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_event_filter_data1_rq);
  ERR (fill_kcs_set_filter_table_data1 (filter_number, 
					filter_type, 
					enabled,
                                        obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_event_filter_data1_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_num_alert_policies2 (ipmi_device_t *dev, 
				  uint8_t num_alert_policies, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_pef_conf_param_num_alert_policies_rq);
  ERR (fill_kcs_set_num_alert_policies (num_alert_policies,
                                        obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_pef_conf_param_num_alert_policies_rq,
		 obj_cmd_rs, 
		 tmpl_set_pef_conf_param_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_alert_immediate2 (ipmi_device_t *dev,
			   uint8_t channel_number, 
			   uint8_t destination_selector,
			   uint8_t string_selector, 
			   uint8_t string_enable, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;

  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_alert_immediate_rq);
  ERR (fill_kcs_alert_immediate (channel_number, 
				 destination_selector,
				 string_selector, 
				 string_enable,
                                 obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_alert_immediate_rq,
		 obj_cmd_rs, 
		 tmpl_alert_immediate_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);                            
}

int8_t 
ipmi_cmd_get_pef_alert_string2 (ipmi_device_t *dev,
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_ALERT_STRINGS,
				    parameter_type,
				    set_selector,
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq,
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_alert_strings_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_alert_string_keys2 (ipmi_device_t *dev,
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

  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_ALERT_STRING_KEYS,
				    parameter_type,
				    set_selector,
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_alert_string_keys_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_num_alert_policies2 (ipmi_device_t *dev,
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_NUM_ALERT_POLICY_ENTRIES,
				    parameter_type,
				    set_selector,
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq,
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_num_alert_policies_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_num_alert_strings2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_NUM_ALERT_STRINGS, 
				    parameter_type, 
				    set_selector, 
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_num_alert_strings_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_filter_data1_2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_EVENT_FILTER_TABLE_DATA_1,
				    parameter_type,
				    set_selector,
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq,
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_event_filter_data1_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_control2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_PEF_CONTROL,
				    parameter_type, 
				    set_selector, 
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_pef_control_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_global_action_control2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_ACTION_GLOBAL_CONTROL,
				    parameter_type, 
				    set_selector, 
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_global_action_control_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_startup_delay2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_STARTUP_DELAY,
				    parameter_type, 
				    set_selector, 
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_startup_delay_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_alert_startup_delay2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_ALERT_STARTUP_DELAY,
				    parameter_type, 
				    set_selector, 
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_alert_startup_delay_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_num_event_filters2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_NUM_EVENT_FILTERS,
				    parameter_type, 
				    set_selector, 
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_num_event_filters_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_filter_table_entry2 (ipmi_device_t *dev, 
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
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_conf_param_rq);
  ERR (fill_kcs_get_pef_conf_param (IPMI_PEF_PARAM_EVENT_FILTER_TABLE,
				    parameter_type, 
				    set_selector, 
				    block_selector,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_TRANSPORT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_conf_param_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_conf_param_event_filter_table_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_pef_caps2 (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_pef_caps_rq);
  ERR (fill_kcs_get_pef_caps (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_pef_caps_rq, 
		 obj_cmd_rs, 
		 tmpl_get_pef_caps_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_arm_pef_postpone_timer2 (ipmi_device_t *dev, 
				  uint8_t countdown, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_arm_pef_postpone_timer_rq);
  ERR (fill_kcs_arm_pef_postpone_timer (countdown,
                                        obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_arm_pef_postpone_timer_rq, 
		 obj_cmd_rs, 
		 tmpl_arm_pef_postpone_timer_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}  

int8_t 
ipmi_cmd_set_last_processed_event2 (ipmi_device_t *dev, 
				    which_event_t which, 
				    uint16_t id, 
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_last_processed_event_rq);
  ERR (fill_kcs_set_last_processed_event (which, 
					  id,
                                          obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_last_processed_event_rq, 
		 obj_cmd_rs, 
		 tmpl_set_last_processed_event_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_last_processed_event2 (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_last_processed_event_rq);
  ERR (fill_kcs_get_last_proessed_event (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_last_processed_event_rq, 
		 obj_cmd_rs, 
		 tmpl_get_last_processed_event_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_pet_ack2 (ipmi_device_t *dev, 
		   uint16_t sequence_number, 
		   uint32_t timestamp, 
		   uint8_t source_type, 
		   uint8_t sensor_device, 
		   uint8_t sensor_number, 
		   uint32_t event_data, 
		   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_pet_ack_rq);
  ERR (fill_kcs_pet_ack (sequence_number, 
			 timestamp, 
			 source_type,
			 sensor_device, 
			 sensor_number, 
			 event_data,
                         obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_pet_ack_rq,
		 obj_cmd_rs, 
		 tmpl_pet_ack_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

