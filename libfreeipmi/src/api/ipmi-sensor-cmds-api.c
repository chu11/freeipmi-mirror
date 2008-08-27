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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/api/ipmi-sensor-cmds-api.h"
#include "freeipmi/cmds/ipmi-sensor-cmds.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

/* achu: as of IPMI 2.0 hysteresis_mask reserved for future - write as 0xFF */
int8_t 
ipmi_cmd_set_sensor_hysteresis (ipmi_ctx_t ctx,
                                uint8_t sensor_number,
                                uint8_t hysteresis_mask,
                                uint8_t positive_going_threshold_hysteresis_value,
                                uint8_t negative_going_threshold_hysteresis_value,
                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (hysteresis_mask == IPMI_SENSOR_HYSTERESIS_MASK
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sensor_hysteresis_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sensor_hysteresis_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_sensor_hysteresis (sensor_number,
                                                     hysteresis_mask,
                                                     positive_going_threshold_hysteresis_value,
                                                     negative_going_threshold_hysteresis_value,
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

/* achu: as of IPMI 2.0 hysteresis_mask reserved for future - write as 0xFF */
int8_t 
ipmi_cmd_get_sensor_hysteresis (ipmi_ctx_t ctx, 
                                uint8_t sensor_number, 
                                uint8_t hysteresis_mask,
                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (hysteresis_mask == IPMI_SENSOR_HYSTERESIS_MASK
                      && fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_hysteresis_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_hysteresis_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_sensor_hysteresis (sensor_number,
                                                     hysteresis_mask,
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
ipmi_cmd_set_sensor_thresholds (ipmi_ctx_t ctx, 
				uint8_t sensor_number, 
                                uint8_t *lower_non_critical_threshold,
                                uint8_t *lower_critical_threshold,
                                uint8_t *lower_non_recoverable_threshold,
                                uint8_t *upper_non_critical_threshold,
                                uint8_t *upper_critical_threshold,
                                uint8_t *upper_non_recoverable_threshold,
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sensor_thresholds_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sensor_thresholds_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_sensor_thresholds (sensor_number,
                                                     lower_non_critical_threshold,
                                                     lower_critical_threshold,
                                                     lower_non_recoverable_threshold,
                                                     upper_non_critical_threshold,
                                                     upper_critical_threshold,
                                                     upper_non_recoverable_threshold,
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
ipmi_cmd_get_sensor_thresholds (ipmi_ctx_t ctx, 
				uint8_t sensor_number, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_thresholds_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_thresholds_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_sensor_thresholds (sensor_number,
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
ipmi_cmd_set_sensor_event_enable (ipmi_ctx_t ctx,
                                  uint8_t sensor_number,
                                  uint8_t event_message_action,
                                  uint8_t scanning_on_this_sensor,
                                  uint8_t all_event_messages,
                                  uint16_t assertion_event_bitmask,
                                  uint16_t deassertion_event_bitmask,
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_SENSOR_EVENT_MESSAGE_ACTION_VALID(event_message_action)
                      && IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_VALID(scanning_on_this_sensor)
                      && IPMI_SENSOR_ALL_EVENT_MESSAGES_VALID(all_event_messages)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sensor_event_enable_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sensor_event_enable_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_sensor_event_enable (sensor_number,
                                                       event_message_action,
                                                       scanning_on_this_sensor,
                                                       all_event_messages,
                                                       assertion_event_bitmask,
                                                       deassertion_event_bitmask,
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
ipmi_cmd_set_sensor_event_enable_threshold (ipmi_ctx_t ctx,
                                            uint8_t sensor_number,
                                            uint8_t event_message_action,
                                            uint8_t scanning_on_this_sensor,
                                            uint8_t all_event_messages,
                                            uint8_t assertion_event_lower_non_critical_going_low,
                                            uint8_t assertion_event_lower_non_critical_going_high,
                                            uint8_t assertion_event_lower_critical_going_low,
                                            uint8_t assertion_event_lower_critical_going_high,
                                            uint8_t assertion_event_lower_non_recoverable_going_low,
                                            uint8_t assertion_event_lower_non_recoverable_going_high,
                                            uint8_t assertion_event_upper_non_critical_going_low,
                                            uint8_t assertion_event_upper_non_critical_going_high,
                                            uint8_t assertion_event_upper_critical_going_low,
                                            uint8_t assertion_event_upper_critical_going_high,
                                            uint8_t assertion_event_upper_non_recoverable_going_low,
                                            uint8_t assertion_event_upper_non_recoverable_going_high,
                                            uint8_t deassertion_event_lower_non_critical_going_low,
                                            uint8_t deassertion_event_lower_non_critical_going_high,
                                            uint8_t deassertion_event_lower_critical_going_low,
                                            uint8_t deassertion_event_lower_critical_going_high,
                                            uint8_t deassertion_event_lower_non_recoverable_going_low,
                                            uint8_t deassertion_event_lower_non_recoverable_going_high ,
                                            uint8_t deassertion_event_upper_non_critical_going_low,
                                            uint8_t deassertion_event_upper_non_critical_going_high,
                                            uint8_t deassertion_event_upper_critical_going_low,
                                            uint8_t deassertion_event_upper_critical_going_high,
                                            uint8_t deassertion_event_upper_non_recoverable_going_low,
                                            uint8_t deassertion_event_upper_non_recoverable_going_high,
                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_SENSOR_EVENT_MESSAGE_ACTION_VALID(event_message_action)
                      && IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_VALID(scanning_on_this_sensor)
                      && IPMI_SENSOR_ALL_EVENT_MESSAGES_VALID(all_event_messages)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_lower_non_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_lower_non_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_lower_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_lower_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_lower_non_recoverable_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_lower_non_recoverable_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_upper_non_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_upper_non_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_upper_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_upper_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_upper_non_recoverable_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_upper_non_recoverable_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_lower_non_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_lower_non_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_lower_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_lower_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_lower_non_recoverable_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_lower_non_recoverable_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_upper_non_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_upper_non_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_upper_critical_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_upper_critical_going_high)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_upper_non_recoverable_going_low)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_upper_non_recoverable_going_high)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sensor_event_enable_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sensor_event_enable_threshold_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_sensor_event_enable_threshold (sensor_number,
                                                                 event_message_action,
                                                                 scanning_on_this_sensor,
                                                                 all_event_messages,
                                                                 assertion_event_lower_non_critical_going_low,
                                                                 assertion_event_lower_non_critical_going_high,
                                                                 assertion_event_lower_critical_going_low,
                                                                 assertion_event_lower_critical_going_high,
                                                                 assertion_event_lower_non_recoverable_going_low,
                                                                 assertion_event_lower_non_recoverable_going_high,
                                                                 assertion_event_upper_non_critical_going_low,
                                                                 assertion_event_upper_non_critical_going_high,
                                                                 assertion_event_upper_critical_going_low,
                                                                 assertion_event_upper_critical_going_high,
                                                                 assertion_event_upper_non_recoverable_going_low,
                                                                 assertion_event_upper_non_recoverable_going_high,
                                                                 deassertion_event_lower_non_critical_going_low,
                                                                 deassertion_event_lower_non_critical_going_high,
                                                                 deassertion_event_lower_critical_going_low,
                                                                 deassertion_event_lower_critical_going_high,
                                                                 deassertion_event_lower_non_recoverable_going_low,
                                                                 deassertion_event_lower_non_recoverable_going_high ,
                                                                 deassertion_event_upper_non_critical_going_low,
                                                                 deassertion_event_upper_non_critical_going_high,
                                                                 deassertion_event_upper_critical_going_low,
                                                                 deassertion_event_upper_critical_going_high,
                                                                 deassertion_event_upper_non_recoverable_going_low,
                                                                 deassertion_event_upper_non_recoverable_going_high,
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
ipmi_cmd_set_sensor_event_enable_discrete (ipmi_ctx_t ctx,
                                           uint8_t sensor_number,
                                           uint8_t event_message_action,
                                           uint8_t scanning_on_this_sensor,
                                           uint8_t all_event_messages,
                                           uint8_t assertion_event_state_bit_0,
                                           uint8_t assertion_event_state_bit_1,
                                           uint8_t assertion_event_state_bit_2,
                                           uint8_t assertion_event_state_bit_3,
                                           uint8_t assertion_event_state_bit_4,
                                           uint8_t assertion_event_state_bit_5,
                                           uint8_t assertion_event_state_bit_6,
                                           uint8_t assertion_event_state_bit_7,
                                           uint8_t assertion_event_state_bit_8,
                                           uint8_t assertion_event_state_bit_9,
                                           uint8_t assertion_event_state_bit_10,
                                           uint8_t assertion_event_state_bit_11,
                                           uint8_t assertion_event_state_bit_12,
                                           uint8_t assertion_event_state_bit_13,
                                           uint8_t assertion_event_state_bit_14,
                                           uint8_t deassertion_event_state_bit_0,
                                           uint8_t deassertion_event_state_bit_1,
                                           uint8_t deassertion_event_state_bit_2,
                                           uint8_t deassertion_event_state_bit_3,
                                           uint8_t deassertion_event_state_bit_4,
                                           uint8_t deassertion_event_state_bit_5,
                                           uint8_t deassertion_event_state_bit_6,
                                           uint8_t deassertion_event_state_bit_7,
                                           uint8_t deassertion_event_state_bit_8,
                                           uint8_t deassertion_event_state_bit_9,
                                           uint8_t deassertion_event_state_bit_10,
                                           uint8_t deassertion_event_state_bit_11,
                                           uint8_t deassertion_event_state_bit_12,
                                           uint8_t deassertion_event_state_bit_13,
                                           uint8_t deassertion_event_state_bit_14,
                                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_SENSOR_EVENT_MESSAGE_ACTION_VALID(event_message_action)
                      && IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_VALID(scanning_on_this_sensor)
                      && IPMI_SENSOR_ALL_EVENT_MESSAGES_VALID(all_event_messages)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_0)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_1)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_2)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_3)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_4)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_5)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_6)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_7)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_8)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_9)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_10)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_11)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_12)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_13)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(assertion_event_state_bit_14)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_0)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_1)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_2)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_3)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_4)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_5)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_6)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_7)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_8)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_9)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_10)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_11)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_12)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_13)
                      && IPMI_SENSOR_EVENT_FLAG_VALID(deassertion_event_state_bit_14)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_sensor_event_enable_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_set_sensor_event_enable_discrete_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_set_sensor_event_enable_discrete (sensor_number,
                                                                event_message_action,
                                                                scanning_on_this_sensor,
                                                                all_event_messages,
                                                                assertion_event_state_bit_0,
                                                                assertion_event_state_bit_1,
                                                                assertion_event_state_bit_2,
                                                                assertion_event_state_bit_3,
                                                                assertion_event_state_bit_4,
                                                                assertion_event_state_bit_5,
                                                                assertion_event_state_bit_6,
                                                                assertion_event_state_bit_7,
                                                                assertion_event_state_bit_8,
                                                                assertion_event_state_bit_9,
                                                                assertion_event_state_bit_10,
                                                                assertion_event_state_bit_11,
                                                                assertion_event_state_bit_12,
                                                                assertion_event_state_bit_13,
                                                                assertion_event_state_bit_14,
                                                                deassertion_event_state_bit_0,
                                                                deassertion_event_state_bit_1,
                                                                deassertion_event_state_bit_2,
                                                                deassertion_event_state_bit_3,
                                                                deassertion_event_state_bit_4,
                                                                deassertion_event_state_bit_5,
                                                                deassertion_event_state_bit_6,
                                                                deassertion_event_state_bit_7,
                                                                deassertion_event_state_bit_8,
                                                                deassertion_event_state_bit_9,
                                                                deassertion_event_state_bit_10,
                                                                deassertion_event_state_bit_11,
                                                                deassertion_event_state_bit_12,
                                                                deassertion_event_state_bit_13,
                                                                deassertion_event_state_bit_14,
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
ipmi_cmd_get_sensor_event_enable (ipmi_ctx_t ctx, 
                                  uint8_t sensor_number, 
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_event_enable_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_event_enable_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sensor_event_enable (sensor_number,
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
ipmi_cmd_get_sensor_event_enable_threshold (ipmi_ctx_t ctx, 
                                            uint8_t sensor_number, 
                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_event_enable_threshold_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_event_enable_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sensor_event_enable (sensor_number,
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
ipmi_cmd_get_sensor_event_enable_discrete (ipmi_ctx_t ctx, 
                                           uint8_t sensor_number, 
                                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_event_enable_discrete_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_event_enable_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_sensor_event_enable (sensor_number,
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
ipmi_cmd_get_sensor_reading (ipmi_ctx_t ctx, 
                             uint8_t sensor_number, 
                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_reading_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_reading_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sensor_reading (sensor_number,
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
ipmi_cmd_get_sensor_reading_ipmb (ipmi_ctx_t ctx, 
                                  uint8_t slave_address,
                                  uint8_t lun,
                                  uint8_t sensor_number, 
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_reading_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_reading_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sensor_reading (sensor_number,
						  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_IPMB_CLEANUP (ctx, 
                                 slave_address,
                                 lun, 
                                 IPMI_NET_FN_SENSOR_EVENT_RQ, 
                                 obj_cmd_rq, 
                                 obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_cmd_get_sensor_reading_threshold (ipmi_ctx_t ctx, 
				       uint8_t sensor_number, 
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_reading_threshold_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_reading_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sensor_reading (sensor_number,
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
ipmi_cmd_get_sensor_reading_discrete (ipmi_ctx_t ctx, 
				      uint8_t sensor_number, 
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_reading_discrete_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_reading_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_sensor_reading (sensor_number,
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
