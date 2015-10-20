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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/api/ipmi-oem-intel-node-manager-cmds-api.h"
#include "freeipmi/cmds/ipmi-oem-intel-node-manager-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

int
ipmi_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control (ipmi_ctx_t ctx,
                                                                            uint8_t target_channel_number,
                                                                            uint8_t target_slave_address,
                                                                            uint8_t target_lun,
                                                                            uint8_t policy_enable_disable,
                                                                            uint8_t domain_id,
                                                                            uint8_t policy_id,
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
                                 tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control (policy_enable_disable,
                                                                                  domain_id,
                                                                                  policy_id,
                                                                                  obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_set_node_manager_policy (ipmi_ctx_t ctx,
                                                         uint8_t target_channel_number,
                                                         uint8_t target_slave_address,
                                                         uint8_t target_lun,
                                                         uint8_t domain_id,
                                                         uint8_t policy_enabled,
                                                         uint8_t policy_id,
                                                         uint8_t policy_trigger_type,
                                                         uint8_t policy_configuration_action,
							 uint8_t aggressive_cpu_power_correction,
							 uint8_t policy_storage_option,
                                                         uint8_t policy_exception_actions_send_alert,
                                                         uint8_t policy_exception_actions_shutdown_system,
                                                         uint16_t policy_target_limit,
                                                         uint32_t correction_time_limit,
                                                         uint16_t policy_trigger_limit,
                                                         uint16_t statistics_reporting_period,
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
                                 tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_set_node_manager_policy (domain_id,
                                                               policy_enabled,
                                                               policy_id,
                                                               policy_trigger_type,
                                                               policy_configuration_action,
							       aggressive_cpu_power_correction,
							       policy_storage_option,
                                                               policy_exception_actions_send_alert,
                                                               policy_exception_actions_shutdown_system,
                                                               policy_target_limit,
                                                               correction_time_limit,
                                                               policy_trigger_limit,
                                                               statistics_reporting_period,
                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_node_manager_policy (ipmi_ctx_t ctx,
                                                         uint8_t target_channel_number,
                                                         uint8_t target_slave_address,
                                                         uint8_t target_lun,
                                                         uint8_t domain_id,
                                                         uint8_t policy_id,
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
                                 tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_node_manager_policy (domain_id,
                                                               policy_id,
                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds (ipmi_ctx_t ctx,
                                                                   uint8_t target_channel_number,
                                                                   uint8_t target_slave_address,
                                                                   uint8_t target_lun,
                                                                   uint8_t domain_id,
                                                                   uint8_t policy_id,
                                                                   uint16_t *alert_threshold1,
                                                                   uint16_t *alert_threshold2,
                                                                   uint16_t *alert_threshold3,
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
                                 tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds (domain_id,
                                                                         policy_id,
                                                                         alert_threshold1,
                                                                         alert_threshold2,
                                                                         alert_threshold3,
                                                                         obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds (ipmi_ctx_t ctx,
                                                                   uint8_t target_channel_number,
                                                                   uint8_t target_slave_address,
                                                                   uint8_t target_lun,
                                                                   uint8_t domain_id,
                                                                   uint8_t policy_id,
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
                                 tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds (domain_id,
                                                                         policy_id,
                                                                         obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods (ipmi_ctx_t ctx,
                                                                         uint8_t target_channel_number,
                                                                         uint8_t target_slave_address,
                                                                         uint8_t target_lun,
                                                                         uint8_t domain_id,
                                                                         uint8_t policy_id,
                                                                         uint8_t *policy1_suspend_start_time,
                                                                         uint8_t *policy1_suspend_stop_time,
                                                                         uint8_t *policy1_suspend_period_recurrence_monday,
                                                                         uint8_t *policy1_suspend_period_recurrence_tuesday,
                                                                         uint8_t *policy1_suspend_period_recurrence_wednesday,
                                                                         uint8_t *policy1_suspend_period_recurrence_thursday,
                                                                         uint8_t *policy1_suspend_period_recurrence_friday,
                                                                         uint8_t *policy1_suspend_period_recurrence_saturday,
                                                                         uint8_t *policy1_suspend_period_recurrence_sunday,
                                                                         uint8_t *policy2_suspend_start_time,
                                                                         uint8_t *policy2_suspend_stop_time,
                                                                         uint8_t *policy2_suspend_period_recurrence_monday,
                                                                         uint8_t *policy2_suspend_period_recurrence_tuesday,
                                                                         uint8_t *policy2_suspend_period_recurrence_wednesday,
                                                                         uint8_t *policy2_suspend_period_recurrence_thursday,
                                                                         uint8_t *policy2_suspend_period_recurrence_friday,
                                                                         uint8_t *policy2_suspend_period_recurrence_saturday,
                                                                         uint8_t *policy2_suspend_period_recurrence_sunday,
                                                                         uint8_t *policy3_suspend_start_time,
                                                                         uint8_t *policy3_suspend_stop_time,
                                                                         uint8_t *policy3_suspend_period_recurrence_monday,
                                                                         uint8_t *policy3_suspend_period_recurrence_tuesday,
                                                                         uint8_t *policy3_suspend_period_recurrence_wednesday,
                                                                         uint8_t *policy3_suspend_period_recurrence_thursday,
                                                                         uint8_t *policy3_suspend_period_recurrence_friday,
                                                                         uint8_t *policy3_suspend_period_recurrence_saturday,
                                                                         uint8_t *policy3_suspend_period_recurrence_sunday,
                                                                         uint8_t *policy4_suspend_start_time,
                                                                         uint8_t *policy4_suspend_stop_time,
                                                                         uint8_t *policy4_suspend_period_recurrence_monday,
                                                                         uint8_t *policy4_suspend_period_recurrence_tuesday,
                                                                         uint8_t *policy4_suspend_period_recurrence_wednesday,
                                                                         uint8_t *policy4_suspend_period_recurrence_thursday,
                                                                         uint8_t *policy4_suspend_period_recurrence_friday,
                                                                         uint8_t *policy4_suspend_period_recurrence_saturday,
                                                                         uint8_t *policy4_suspend_period_recurrence_sunday,
                                                                         uint8_t *policy5_suspend_start_time,
                                                                         uint8_t *policy5_suspend_stop_time,
                                                                         uint8_t *policy5_suspend_period_recurrence_monday,
                                                                         uint8_t *policy5_suspend_period_recurrence_tuesday,
                                                                         uint8_t *policy5_suspend_period_recurrence_wednesday,
                                                                         uint8_t *policy5_suspend_period_recurrence_thursday,
                                                                         uint8_t *policy5_suspend_period_recurrence_friday,
                                                                         uint8_t *policy5_suspend_period_recurrence_saturday,
                                                                         uint8_t *policy5_suspend_period_recurrence_sunday,
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
                                 tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods (domain_id,
                                                                               policy_id,
                                                                               policy1_suspend_start_time,
                                                                               policy1_suspend_stop_time,
                                                                               policy1_suspend_period_recurrence_monday,
                                                                               policy1_suspend_period_recurrence_tuesday,
                                                                               policy1_suspend_period_recurrence_wednesday,
                                                                               policy1_suspend_period_recurrence_thursday,
                                                                               policy1_suspend_period_recurrence_friday,
                                                                               policy1_suspend_period_recurrence_saturday,
                                                                               policy1_suspend_period_recurrence_sunday,
                                                                               policy2_suspend_start_time,
                                                                               policy2_suspend_stop_time,
                                                                               policy2_suspend_period_recurrence_monday,
                                                                               policy2_suspend_period_recurrence_tuesday,
                                                                               policy2_suspend_period_recurrence_wednesday,
                                                                               policy2_suspend_period_recurrence_thursday,
                                                                               policy2_suspend_period_recurrence_friday,
                                                                               policy2_suspend_period_recurrence_saturday,
                                                                               policy2_suspend_period_recurrence_sunday,
                                                                               policy3_suspend_start_time,
                                                                               policy3_suspend_stop_time,
                                                                               policy3_suspend_period_recurrence_monday,
                                                                               policy3_suspend_period_recurrence_tuesday,
                                                                               policy3_suspend_period_recurrence_wednesday,
                                                                               policy3_suspend_period_recurrence_thursday,
                                                                               policy3_suspend_period_recurrence_friday,
                                                                               policy3_suspend_period_recurrence_saturday,
                                                                               policy3_suspend_period_recurrence_sunday,
                                                                               policy4_suspend_start_time,
                                                                               policy4_suspend_stop_time,
                                                                               policy4_suspend_period_recurrence_monday,
                                                                               policy4_suspend_period_recurrence_tuesday,
                                                                               policy4_suspend_period_recurrence_wednesday,
                                                                               policy4_suspend_period_recurrence_thursday,
                                                                               policy4_suspend_period_recurrence_friday,
                                                                               policy4_suspend_period_recurrence_saturday,
                                                                               policy4_suspend_period_recurrence_sunday,
                                                                               policy5_suspend_start_time,
                                                                               policy5_suspend_stop_time,
                                                                               policy5_suspend_period_recurrence_monday,
                                                                               policy5_suspend_period_recurrence_tuesday,
                                                                               policy5_suspend_period_recurrence_wednesday,
                                                                               policy5_suspend_period_recurrence_thursday,
                                                                               policy5_suspend_period_recurrence_friday,
                                                                               policy5_suspend_period_recurrence_saturday,
                                                                               policy5_suspend_period_recurrence_sunday,
                                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods (ipmi_ctx_t ctx,
                                                                         uint8_t target_channel_number,
                                                                         uint8_t target_slave_address,
                                                                         uint8_t target_lun,
                                                                         uint8_t domain_id,
                                                                         uint8_t policy_id,
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
                                 tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods (domain_id,
                                                                               policy_id,
                                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_reset_node_manager_statistics (ipmi_ctx_t ctx,
                                                               uint8_t target_channel_number,
                                                               uint8_t target_slave_address,
                                                               uint8_t target_lun,
                                                               uint8_t mode,
                                                               uint8_t domain_id,
                                                               uint8_t policy_id,
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
                                 tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_reset_node_manager_statistics (mode,
                                                                     domain_id,
                                                                     policy_id,
                                                                     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_node_manager_statistics (ipmi_ctx_t ctx,
                                                             uint8_t target_channel_number,
                                                             uint8_t target_slave_address,
                                                             uint8_t target_lun,
                                                             uint8_t mode,
                                                             uint8_t domain_id,
                                                             uint8_t policy_id,
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
                                 tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_node_manager_statistics (mode,
                                                                   domain_id,
                                                                   policy_id,
                                                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_node_manager_capabilities (ipmi_ctx_t ctx,
                                                               uint8_t target_channel_number,
                                                               uint8_t target_slave_address,
                                                               uint8_t target_lun,
                                                               uint8_t domain_id,
                                                               uint8_t policy_trigger_type,
                                                               uint8_t policy_type,
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
                                 tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_node_manager_capabilities (domain_id,
                                                                     policy_trigger_type,
                                                                     policy_type,
                                                                     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_node_manager_version (ipmi_ctx_t ctx,
                                                          uint8_t target_channel_number,
                                                          uint8_t target_slave_address,
                                                          uint8_t target_lun,
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
                                 tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_node_manager_version (obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_set_node_manager_power_draw_range (ipmi_ctx_t ctx,
                                                                   uint8_t target_channel_number,
                                                                   uint8_t target_slave_address,
                                                                   uint8_t target_lun,
                                                                   uint8_t domain_id,
                                                                   uint16_t minimum_power_draw,
                                                                   uint16_t maximum_power_draw,
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
                                 tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_set_node_manager_power_draw_range (domain_id,
                                                                         minimum_power_draw,
                                                                         maximum_power_draw,
                                                                         obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_destination (ipmi_ctx_t ctx,
                                                                    uint8_t target_channel_number,
                                                                    uint8_t target_slave_address,
                                                                    uint8_t target_lun,
                                                                    uint8_t channel_number,
                                                                    uint8_t destination_information_operation,
                                                                    uint8_t destination_information,
                                                                    uint8_t alert_string_selector,
                                                                    uint8_t send_alert_string,
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
                                 tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination (channel_number,
                                                                          destination_information_operation,
                                                                          destination_information,
                                                                          alert_string_selector,
                                                                          send_alert_string,
                                                                          obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb (ipmi_ctx_t ctx,
                                                                         uint8_t target_channel_number,
                                                                         uint8_t target_slave_address,
                                                                         uint8_t target_lun,
                                                                         uint8_t channel_number,
                                                                         uint8_t destination_information_operation,
                                                                         uint8_t slave_address,
                                                                         uint8_t alert_string_selector,
                                                                         uint8_t send_alert_string,
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
                                 tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb (channel_number,
                                                                               destination_information_operation,
                                                                               slave_address,
                                                                               alert_string_selector,
                                                                               send_alert_string,
                                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}
  
int
ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan (ipmi_ctx_t ctx,
                                                                        uint8_t target_channel_number,
                                                                        uint8_t target_slave_address,
                                                                        uint8_t target_lun,
                                                                        uint8_t channel_number,
                                                                        uint8_t destination_information_operation,
                                                                        uint8_t destination_selector,
                                                                        uint8_t alert_string_selector,
                                                                        uint8_t send_alert_string,
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
                                 tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_set_node_manager_alert_destination_lan (channel_number,
                                                                              destination_information_operation,
                                                                              destination_selector,
                                                                              alert_string_selector,
                                                                              send_alert_string,
                                                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_node_manager_alert_destination (ipmi_ctx_t ctx,
                                                                    uint8_t target_channel_number,
                                                                    uint8_t target_slave_address,
                                                                    uint8_t target_lun,
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
                                 tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_node_manager_alert_destination (obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_oem_intel_node_manager_get_limiting_policy_id (ipmi_ctx_t ctx,
							uint8_t target_channel_number,
							uint8_t target_slave_address,
							uint8_t target_lun,
							uint8_t domain_id,
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
                                 tmpl_cmd_oem_intel_node_manager_get_limiting_policy_id_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_limiting_policy_id_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_oem_intel_node_manager_get_limiting_policy_id (domain_id,
							      obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (target_channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB
      && target_slave_address == IPMI_SLAVE_ADDRESS_BMC
      && target_lun == IPMI_BMC_IPMB_LUN_BMC)
    {
      if (api_ipmi_cmd (ctx,
                        IPMI_BMC_IPMB_LUN_BMC,
                        IPMI_NET_FN_OEM_GROUP_RQ,
                        obj_cmd_rq,
                        obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }
  else
    {
      if (api_ipmi_cmd_ipmb (ctx,
                             target_channel_number,
                             target_slave_address,
                             target_lun,
                             IPMI_NET_FN_OEM_GROUP_RQ,
                             obj_cmd_rq,
                             obj_cmd_rs) < 0)
        {
          ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}
