/*  

   Copyright (C) 2003-2008 FreeIPMI Core Team
   
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
   the Free Software Foundation, Inc., 51 Franklin Street,
   Fifth Floor, Boston, MA  02110-1301  USA.
   
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

#include "freeipmi/api/ipmi-pef-and-alerting-cmds-api.h"
#include "freeipmi/cmds/ipmi-pef-and-alerting-cmds.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-pef-parameter-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cmd_get_pef_capabilities (ipmi_ctx_t ctx, fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_capabilities_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_capabilities_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_capabilities (obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_arm_pef_postpone_timer (ipmi_ctx_t ctx, 
				 uint8_t pef_postpone_timeout, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_arm_pef_postpone_timer_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_arm_pef_postpone_timer_rq); 

  API_ERR_CLEANUP (!(fill_cmd_arm_pef_postpone_timer (pef_postpone_timeout,
						      obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}  

int8_t
ipmi_cmd_set_pef_configuration_parameters_pef_control (ipmi_ctx_t ctx, 
						       uint8_t pef, 
						       uint8_t pef_event_messages, 
						       uint8_t pef_startup_delay, 
						       uint8_t pef_alert_startup_delay, 
						       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_PEF_EVENT_MESSAGES_VALID(pef_event_messages)
                      && IPMI_PEF_STARTUP_DELAY_VALID(pef_startup_delay)
                      && IPMI_PEF_ALERT_STARTUP_DELAY_VALID(pef_alert_startup_delay)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_control_rq); 

  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_pef_control (pef, 
									    pef_event_messages,
									    pef_startup_delay, 
									    pef_alert_startup_delay,
									    obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (ipmi_ctx_t ctx, 
								     uint8_t alert_action,
								     uint8_t power_down_action, 
								     uint8_t reset_action,
								     uint8_t power_cycle_action, 
								     uint8_t oem_action,
								     uint8_t diagnostic_interrupt, 
								     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_PEF_POWER_DOWN_ACTION_VALID(power_down_action)
                      && IPMI_PEF_RESET_ACTION_VALID(reset_action)
                      && IPMI_PEF_POWER_CYCLE_ACTION_VALID(power_cycle_action)
                      && IPMI_PEF_OEM_ACTION_VALID(oem_action)
                      && IPMI_PEF_DIAGNOSTIC_INTERRUPT_VALID(diagnostic_interrupt)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_action_global_control_rq); 

  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_pef_action_global_control (alert_action, 
											  power_down_action,
											  reset_action, 
											  power_cycle_action, 
											  oem_action, 
											  diagnostic_interrupt,
											  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (ipmi_ctx_t ctx, 
							     uint8_t pef_startup_delay, 
							     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_startup_delay_rq); 

  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_pef_startup_delay (pef_startup_delay,
										  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (ipmi_ctx_t ctx, 
								   uint8_t pef_alert_startup_delay, 
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_pef_alert_startup_delay_rq); 

  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (pef_alert_startup_delay,
											obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_event_filter_table (ipmi_ctx_t ctx, 
							      uint8_t filter_number,
							      uint8_t filter_configuration_type,
							      uint8_t filter_configuration_filter,
							      uint8_t event_filter_action_alert,
							      uint8_t event_filter_action_power_off,
							      uint8_t event_filter_action_reset,
							      uint8_t event_filter_action_power_cycle,
							      uint8_t event_filter_action_oem,
							      uint8_t event_filter_action_diagnostic_interrupt,
							      uint8_t event_filter_action_group_control_operation,
							      uint8_t alert_policy_number_policy_number,
							      uint8_t alert_policy_number_group_control_selector,
							      uint8_t event_severity,
							      uint8_t generator_id_byte1,
							      uint8_t generator_id_byte2,
							      uint8_t sensor_type,
							      uint8_t sensor_number,
							      uint8_t event_trigger,
							      uint16_t event_data1_offset_mask,
							      uint8_t event_data1_AND_mask,
							      uint8_t event_data1_compare1,
							      uint8_t event_data1_compare2,
							      uint8_t event_data2_AND_mask,
							      uint8_t event_data2_compare1,
							      uint8_t event_data2_compare2,
							      uint8_t event_data3_AND_mask,
							      uint8_t event_data3_compare1,
							      uint8_t event_data3_compare2,
							      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_FILTER_CONFIGURATION_FILTER_TYPE_VALID(filter_configuration_type)
                      && IPMI_FILTER_CONFIGURATION_FILTER_VALID(filter_configuration_filter)
                      && IPMI_EVENT_FILTER_ACTION_ALERT_VALID(event_filter_action_alert)
                      && IPMI_EVENT_FILTER_ACTION_POWER_OFF_VALID(event_filter_action_power_off)
                      && IPMI_EVENT_FILTER_ACTION_RESET_VALID(event_filter_action_reset)
                      && IPMI_EVENT_FILTER_ACTION_POWER_CYCLE_VALID(event_filter_action_power_cycle)
                      && IPMI_EVENT_FILTER_ACTION_OEM_VALID(event_filter_action_oem)
                      && IPMI_EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_VALID(event_filter_action_diagnostic_interrupt)
                      && IPMI_EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_VALID(event_filter_action_group_control_operation)
                      && IPMI_EVENT_SEVERITY_VALID(event_severity)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_event_filter_table_rq); 

  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_event_filter_table (filter_number,
										   filter_configuration_type,
										   filter_configuration_filter,
										   event_filter_action_alert,
										   event_filter_action_power_off,
										   event_filter_action_reset,
										   event_filter_action_power_cycle,
										   event_filter_action_oem,
										   event_filter_action_diagnostic_interrupt,
										   event_filter_action_group_control_operation,
										   alert_policy_number_policy_number,
										   alert_policy_number_group_control_selector,
										   event_severity,
										   generator_id_byte1,
										   generator_id_byte2,
										   sensor_type,
										   sensor_number,
										   event_trigger,
										   event_data1_offset_mask,
										   event_data1_AND_mask,
										   event_data1_compare1,
										   event_data1_compare2,
										   event_data2_AND_mask,
										   event_data2_compare1,
										   event_data2_compare2,
										   event_data3_AND_mask,
										   event_data3_compare1,
										   event_data3_compare2,
										   obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_event_filter_table_data1_ (ipmi_ctx_t ctx, 
								     uint8_t filter_number,
								     uint8_t filter_configuration_type,
								     uint8_t filter_configuration_filter,
								     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_FILTER_CONFIGURATION_FILTER_TYPE_VALID(filter_configuration_type)
                      && IPMI_FILTER_CONFIGURATION_FILTER_VALID(filter_configuration_filter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_pef_configuration_parameters_event_filter_table_data1_rq); 

  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_event_filter_table_data1 (filter_number,
											 filter_configuration_type,
											 filter_configuration_filter,
											 obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_alert_policy_table (ipmi_ctx_t ctx, 
							      uint8_t alert_policy_entry_number, 
							      uint8_t policy_type, 
							      uint8_t policy_enabled, 
							      uint8_t policy_number, 
							      uint8_t destination_selector, 
							      uint8_t channel_number, 
							      uint8_t alert_string_set_selector, 
							      uint8_t event_specific_alert_string, 
							      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_ALERT_POLICY_TABLE_POLICY_TYPE_VALID (policy_type) 
                      && IPMI_ALERT_POLICY_ENABLED_DISABLED_VALID (policy_enabled)
                      && IPMI_EVENT_SPECIFIC_ALERT_STRING_VALID (event_specific_alert_string)
                      && fiid_obj_valid (obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, 
		       tmpl_cmd_set_pef_configuration_parameters_alert_policy_table_rq);
  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_alert_policy_table (alert_policy_entry_number, 
										   policy_type, 
										   policy_enabled, 
										   policy_number, 
										   destination_selector, 
										   channel_number, 
										   alert_string_set_selector, 
										   event_specific_alert_string, 
										   obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY (obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_alert_string_keys (ipmi_ctx_t ctx, 
							     uint8_t string_selector, 
							     uint8_t filter_number, 
							     uint8_t set_number_for_string, 
							     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_STRING_SELECTOR_VALID (string_selector)
                      && fiid_obj_valid (obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, 
                       tmpl_cmd_set_pef_configuration_parameters_alert_string_keys_rq);
  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_alert_string_keys (string_selector, 
                                                                                  filter_number, 
                                                                                  set_number_for_string,
                                                                                  obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
                            IPMI_BMC_IPMB_LUN_BMC, 
                            IPMI_NET_FN_SENSOR_EVENT_RQ, 
                            obj_cmd_rq, 
                            obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY (obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_pef_configuration_parameters_alert_strings (ipmi_ctx_t ctx, 
							 uint8_t string_selector, 
							 uint8_t block_selector, 
							 uint8_t *string_data, 
							 uint8_t string_data_len, 
							 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_STRING_SELECTOR_VALID (string_selector)
                      && string_data_len
                      && IPMI_PEF_ALERT_STRINGS_BLOCK_SIZE_VALID (string_data_len)
                      && fiid_obj_valid (obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, 
                       tmpl_cmd_set_pef_configuration_parameters_alert_strings_rq);
  API_ERR_CLEANUP (!(fill_cmd_set_pef_configuration_parameters_alert_strings (string_selector, 
                                                                              block_selector, 
                                                                              string_data, 
                                                                              string_data_len, 
                                                                              obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
                            IPMI_BMC_IPMB_LUN_BMC, 
                            IPMI_NET_FN_SENSOR_EVENT_RQ, 
                            obj_cmd_rq, 
                            obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY (obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_pef_control (ipmi_ctx_t ctx, 
						       uint8_t get_parameter, 
						       uint8_t set_selector, 
						       uint8_t block_selector, 
						       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_pef_control_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_PEF_CONTROL,
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (ipmi_ctx_t ctx, 
								     uint8_t get_parameter, 
								     uint8_t set_selector,
								     uint8_t block_selector, 
								     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_PEF_ACTION_GLOBAL_CONTROL,
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (ipmi_ctx_t ctx, 
							     uint8_t get_parameter, 
							     uint8_t set_selector,
							     uint8_t block_selector, 
							     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_PEF_STARTUP_DELAY,
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (ipmi_ctx_t ctx, 
								   uint8_t get_parameter, 
								   uint8_t set_selector,
								   uint8_t block_selector, 
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_PEF_ALERT_STARTUP_DELAY,
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (ipmi_ctx_t ctx, 
								   uint8_t get_parameter, 
								   uint8_t set_selector,
								   uint8_t block_selector, 
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_NUMBER_OF_EVENT_FILTERS,
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_event_filter_table (ipmi_ctx_t ctx, 
							      uint8_t get_parameter, 
							      uint8_t set_selector,
							      uint8_t block_selector, 
							      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_event_filter_table_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_EVENT_FILTER_TABLE,
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_event_filter_table_data1_ (ipmi_ctx_t ctx, 
								     uint8_t get_parameter, 
								     uint8_t set_selector, 
								     uint8_t block_selector, 
								     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_event_filter_table_data1_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_EVENT_FILTER_TABLE_DATA_1,
								get_parameter,
								set_selector,
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (ipmi_ctx_t ctx,
									  uint8_t get_parameter, 
									  uint8_t set_selector,
									  uint8_t block_selector, 
									  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_NUMBER_OF_ALERT_POLICY_ENTRIES,
								get_parameter,
								set_selector,
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings (ipmi_ctx_t ctx, 
								   uint8_t get_parameter, 
								   uint8_t set_selector, 
								   uint8_t block_selector, 
								   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_NUMBER_OF_ALERT_STRINGS, 
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_alert_string_keys (ipmi_ctx_t ctx,
							     uint8_t get_parameter, 
							     uint8_t set_selector,
							     uint8_t block_selector, 
							     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_alert_string_keys_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_ALERT_STRING_KEYS,
								get_parameter,
								set_selector,
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_alert_string (ipmi_ctx_t ctx,
							uint8_t get_parameter, 
							uint8_t set_selector,
							uint8_t block_selector, 
							fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID(get_parameter)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_alert_strings_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_ALERT_STRINGS,
								get_parameter,
								set_selector,
								block_selector,
								obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_pef_configuration_parameters_alert_policy_table (ipmi_ctx_t ctx, 
							      uint8_t get_parameter, 
							      uint8_t set_selector, 
							      uint8_t block_selector, 
							      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL; 
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_GET_PEF_PARAMETER_VALID (get_parameter)
                      && fiid_obj_valid (obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_alert_policy_table_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_pef_configuration_parameters_rq); 
  
  API_ERR_CLEANUP (!(fill_cmd_get_pef_configuration_parameters (IPMI_PEF_PARAMETER_ALERT_POLICY_TABLE,
								get_parameter, 
								set_selector, 
								block_selector,
								obj_cmd_rq) < 0));
  
  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY (obj_cmd_rq);
  return (rv);
}


int8_t 
ipmi_cmd_set_last_processed_event_id (ipmi_ctx_t ctx, 
				      uint8_t set_record_id_for_last_record,
				      uint16_t record_id,
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_SET_RECORD_ID_FOR_LAST_RECORD_PROCESSED_VALID(set_record_id_for_last_record)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_last_processed_event_id_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_last_processed_event_id_rq); 

  API_ERR_CLEANUP (!(fill_cmd_set_last_processed_event_id (set_record_id_for_last_record,
							   record_id,
							   obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_last_processed_event_id (ipmi_ctx_t ctx, fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_last_processed_event_id_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_last_processed_event_id_rq); 

  API_ERR_CLEANUP (!(fill_cmd_get_last_processed_event_id (obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_alert_immediate (ipmi_ctx_t ctx,
			  uint8_t channel_number,
			  uint8_t destination_selector,
			  uint8_t operation,
			  uint8_t string_selector,
			  uint8_t send_alert_string,
			  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_ALERT_IMMEDIATE_OPERATION_VALID(operation)
                      && IPMI_STRING_SELECTOR_VALID(string_selector)
                      && IPMI_SEND_ALERT_STRING_VALID(send_alert_string)
                      && IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_alert_immediate_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_alert_immediate_rq); 

  API_ERR_CLEANUP (!(fill_cmd_alert_immediate (channel_number,
					       destination_selector,
					       operation,
					       string_selector,
					       send_alert_string,
					       obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_pet_acknowledge (ipmi_ctx_t ctx, 
			  uint16_t sequence_number,
			  uint32_t local_timestamp,
			  uint8_t event_source_type,
			  uint8_t sensor_device,
			  uint8_t sensor_number,
			  uint32_t event_data,          
			  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_pet_acknowledge_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_pet_acknowledge_rq); 

  API_ERR_CLEANUP (!(fill_cmd_pet_acknowledge (sequence_number,
					       local_timestamp,
					       event_source_type,
					       sensor_device,
					       sensor_number,
					       event_data,
					       obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

