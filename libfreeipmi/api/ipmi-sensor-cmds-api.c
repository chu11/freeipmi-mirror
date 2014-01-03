/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

/* achu: as of IPMI 2.0 hysteresis_mask reserved for future - write as 0xFF */
int
ipmi_cmd_set_sensor_hysteresis (ipmi_ctx_t ctx,
                                uint8_t sensor_number,
                                uint8_t hysteresis_mask,
                                uint8_t positive_going_threshold_hysteresis_value,
                                uint8_t negative_going_threshold_hysteresis_value,
                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_sensor_hysteresis_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_sensor_hysteresis_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_sensor_hysteresis (sensor_number,
                                      hysteresis_mask,
                                      positive_going_threshold_hysteresis_value,
                                      negative_going_threshold_hysteresis_value,
                                      obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

/* achu: as of IPMI 2.0 hysteresis_mask reserved for future - write as 0xFF */
int
ipmi_cmd_get_sensor_hysteresis (ipmi_ctx_t ctx,
                                uint8_t sensor_number,
                                uint8_t hysteresis_mask,
                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_hysteresis_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_hysteresis_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_hysteresis (sensor_number,
                                      hysteresis_mask,
                                      obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_sensor_thresholds (ipmi_ctx_t ctx,
                                uint8_t sensor_number,
                                const uint8_t *lower_non_critical_threshold,
                                const uint8_t *lower_critical_threshold,
                                const uint8_t *lower_non_recoverable_threshold,
                                const uint8_t *upper_non_critical_threshold,
                                const uint8_t *upper_critical_threshold,
                                const uint8_t *upper_non_recoverable_threshold,
                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_sensor_thresholds_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_sensor_thresholds_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_sensor_thresholds (sensor_number,
                                      lower_non_critical_threshold,
                                      lower_critical_threshold,
                                      lower_non_recoverable_threshold,
                                      upper_non_critical_threshold,
                                      upper_critical_threshold,
                                      upper_non_recoverable_threshold,
                                      obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_thresholds (ipmi_ctx_t ctx,
                                uint8_t sensor_number,
                                fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_thresholds_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_thresholds_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_thresholds (sensor_number,
                                      obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
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
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_sensor_event_enable_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_sensor_event_enable_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_sensor_event_enable (sensor_number,
                                        event_message_action,
                                        scanning_on_this_sensor,
                                        all_event_messages,
                                        assertion_event_bitmask,
                                        deassertion_event_bitmask,
                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
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
                                            uint8_t deassertion_event_lower_non_recoverable_going_high,
                                            uint8_t deassertion_event_upper_non_critical_going_low,
                                            uint8_t deassertion_event_upper_non_critical_going_high,
                                            uint8_t deassertion_event_upper_critical_going_low,
                                            uint8_t deassertion_event_upper_critical_going_high,
                                            uint8_t deassertion_event_upper_non_recoverable_going_low,
                                            uint8_t deassertion_event_upper_non_recoverable_going_high,
                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_sensor_event_enable_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_sensor_event_enable_threshold_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_sensor_event_enable_threshold (sensor_number,
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
                                                  deassertion_event_lower_non_recoverable_going_high,
                                                  deassertion_event_upper_non_critical_going_low,
                                                  deassertion_event_upper_non_critical_going_high,
                                                  deassertion_event_upper_critical_going_low,
                                                  deassertion_event_upper_critical_going_high,
                                                  deassertion_event_upper_non_recoverable_going_low,
                                                  deassertion_event_upper_non_recoverable_going_high,
                                                  obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
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
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_sensor_event_enable_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_sensor_event_enable_discrete_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_sensor_event_enable_discrete (sensor_number,
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
                                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_event_enable (ipmi_ctx_t ctx,
                                  uint8_t sensor_number,
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_event_enable_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_event_enable_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_event_enable (sensor_number,
                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_event_enable_threshold (ipmi_ctx_t ctx,
                                            uint8_t sensor_number,
                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_event_enable_threshold_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_event_enable_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_event_enable (sensor_number,
                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_event_enable_discrete (ipmi_ctx_t ctx,
                                           uint8_t sensor_number,
                                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_event_enable_discrete_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_event_enable_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_event_enable (sensor_number,
                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_re_arm_sensor_events (ipmi_ctx_t ctx,
			       uint8_t sensor_number,
			       uint8_t re_arm_all_event_status_from_this_sensor,
			       uint16_t *re_arm_assertion_event,
			       uint16_t *re_arm_deassertion_event,
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_re_arm_sensor_events_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_re_arm_sensor_events_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_re_arm_sensor_events (sensor_number,
				     re_arm_all_event_status_from_this_sensor,
				     re_arm_assertion_event,
				     re_arm_deassertion_event,
				     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_re_arm_sensor_events_ipmb (ipmi_ctx_t ctx,
				    uint8_t channel_number,
				    uint8_t slave_address,
				    uint8_t lun,
				    uint8_t sensor_number,
				    uint8_t re_arm_all_event_status_from_this_sensor,
				    uint16_t *re_arm_assertion_event,
				    uint16_t *re_arm_deassertion_event,
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_re_arm_sensor_events_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_re_arm_sensor_events_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_re_arm_sensor_events (sensor_number,
				     re_arm_all_event_status_from_this_sensor,
				     re_arm_assertion_event,
				     re_arm_deassertion_event,
				     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd_ipmb (ctx,
                         channel_number,
                         slave_address,
                         lun,
			 IPMI_NET_FN_SENSOR_EVENT_RQ,
			 obj_cmd_rq,
			 obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_reading (ipmi_ctx_t ctx,
                             uint8_t sensor_number,
                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_reading_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_reading_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_reading (sensor_number,
                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_reading_ipmb (ipmi_ctx_t ctx,
                                  uint8_t channel_number,
                                  uint8_t slave_address,
                                  uint8_t lun,
                                  uint8_t sensor_number,
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_reading_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_reading_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_reading (sensor_number,
                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd_ipmb (ctx,
                         channel_number,
                         slave_address,
                         lun,
                         IPMI_NET_FN_SENSOR_EVENT_RQ,
                         obj_cmd_rq,
                         obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_reading_threshold (ipmi_ctx_t ctx,
                                       uint8_t sensor_number,
                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_reading_threshold_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_reading_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_reading (sensor_number,
                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_sensor_reading_discrete (ipmi_ctx_t ctx,
                                      uint8_t sensor_number,
                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_sensor_reading_discrete_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_sensor_reading_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_sensor_reading (sensor_number,
                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_sensor_reading_and_event_status (ipmi_ctx_t ctx,
					      uint8_t sensor_number,
					      uint8_t sensor_reading_operation,
					      uint8_t deassertion_bits_operation,
					      uint8_t assertion_bits_operation,
					      uint8_t event_data_bytes_operation,
					      uint8_t sensor_reading,
					      uint16_t assertion_event_bitmask,
					      uint16_t deassertion_event_bitmask,
					      uint8_t event_data1,
					      uint8_t event_data2,
					      uint8_t event_data3,
					      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_sensor_reading_and_event_status_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_sensor_reading_and_event_status_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_sensor_reading_and_event_status (sensor_number,
						    sensor_reading_operation,
						    deassertion_bits_operation,
						    assertion_bits_operation,
						    event_data_bytes_operation,
						    sensor_reading,
						    assertion_event_bitmask,
						    deassertion_event_bitmask,
						    event_data1,
						    event_data2,
						    event_data3,
						    obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_SENSOR_EVENT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}
