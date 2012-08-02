/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-intelnm.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

struct ipmi_oem_intelnm_sdr_callback
{
  ipmi_oem_state_data_t *state_data;
  uint8_t *target_channel_number;
  uint8_t *target_slave_address;
  uint8_t *target_lun;
  int found;
};

static int
_ipmi_oem_intelnm_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
				uint8_t record_type,
				const void *sdr_record,
				unsigned int sdr_record_len,
				void *arg)
{
  struct ipmi_oem_intelnm_sdr_callback *sdr_callback_arg;
  ipmi_oem_state_data_t *state_data;
  int ret;
  int rv = -1;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  sdr_callback_arg = (struct ipmi_oem_intelnm_sdr_callback *)arg;
  state_data = sdr_callback_arg->state_data;
  
  if (record_type != IPMI_SDR_FORMAT_OEM_RECORD)
    return (0);

  if ((ret = ipmi_sdr_oem_parse_intel_node_manager (sdr_ctx,
						    sdr_record,
						    sdr_record_len,
						    sdr_callback_arg->target_slave_address,
						    sdr_callback_arg->target_lun,
						    sdr_callback_arg->target_channel_number,
						    NULL,
						    NULL,
						    NULL,
						    NULL)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_oem_parse_intel_node_manager: %s\n",
		       ipmi_sdr_ctx_errormsg (sdr_ctx));
      goto cleanup;
    }
  
  if (ret)
    sdr_callback_arg->found = 1;
  rv = ret;

 cleanup:
  return (rv);
}

static int
_ipmi_oem_intelnm_node_manager_init (ipmi_oem_state_data_t *state_data,
                                     uint8_t *target_channel_number,
                                     uint8_t *target_slave_address,
                                     uint8_t *target_lun)
{
  struct ipmi_oem_intelnm_sdr_callback sdr_callback_arg;
  int rv = -1;

  assert (state_data);
  assert (target_channel_number);
  assert (target_slave_address);
  assert (target_lun);

  sdr_callback_arg.state_data = state_data;
  sdr_callback_arg.target_channel_number = target_channel_number;
  sdr_callback_arg.target_slave_address = target_slave_address;
  sdr_callback_arg.target_lun = target_lun;
  sdr_callback_arg.found = 0;

  if (sdr_cache_create_and_load (state_data->sdr_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
 				 &state_data->prog_data->args->common_args) < 0)
    goto cleanup;

  if (ipmi_sdr_cache_iterate (state_data->sdr_ctx,
			      _ipmi_oem_intelnm_sdr_callback,
			      &sdr_callback_arg) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_iterate: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if (!sdr_callback_arg.found)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Intel Node Manager not found\n");
      goto cleanup;
    }
  
  /* slave address is stored as 7-bit i2c in SDR, we need the 8 bit
   * version for the communication
   */
  (*target_slave_address) <<= 1;
  
  rv = 0;
 cleanup:
  return (rv);
}

/* returns 1 if found message to output, 0 if not, -1 on error */
static int
_ipmi_oem_intelnm_bad_completion_code (ipmi_oem_state_data_t *state_data,
				       const char *prefix,
				       fiid_obj_t obj_cmd_rs)
{
  uint8_t cmd;
  uint8_t comp_code;
  uint64_t val;
  char *str = NULL;
  int rv = -1;

  assert (state_data);
  assert (obj_cmd_rs);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "cmd",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'cmd': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cmd = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "comp_code",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'comp_code': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  comp_code = val;

  switch (cmd)
    {
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_ENABLE_DISABLE_NODE_MANAGER_POLICY_CONTROL:
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_POLICY:
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_ALERT_THRESHOLDS:
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_POLICY_SUSPEND_PERIODS:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID_STR;
	  break;
	default:
	  rv = 0;
	  goto cleanup;
	}
      break;
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_OR_UNSUPPORTED_POLICY_TRIGGER_TYPE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_OR_UNSUPPORTED_POLICY_TRIGGER_TYPE_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_OR_UNSUPPORTED_POLICY_CONFIGURATION_ACTION:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_OR_UNSUPPORTED_POLICY_CONFIGURATION_ACTION_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_POWER_LIMIT_OUT_OF_RANGE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_POWER_LIMIT_OUT_OF_RANGE_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_CORRECTION_TIME_OUT_OF_RANGE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_CORRECTION_TIME_OUT_OF_RANGE_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_VALUE_OUT_OF_RANGE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_VALUE_OUT_OF_RANGE_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_REQUEST_PARAMETER_NOT_SUPPORTED:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_REQUEST_PARAMETER_NOT_SUPPORTED_STR;
	  break;
	default:
	  rv = 0;
	  goto cleanup;
	}
      break;
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_THRESHOLDS:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_LIMIT_IN_ONE_OF_THRESHOLDS_IS_INVALID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_LIMIT_IN_ONE_OF_THRESHOLDS_IS_INVALID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_NUMBER_OF_POLICY_THRESHOLDS:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_NUMBER_OF_POLICY_THRESHOLDS_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_THRESHOLDS_REQUEST_PARAMETER_NOT_SUPPORTED:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_THRESHOLDS_REQUEST_PARAMETER_NOT_SUPPORTED_STR;
	  break;
	default:
	  rv = 0;
	  goto cleanup;
	}
      break;
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_SUSPEND_PERIODS:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_ONE_OF_PERIODS_IN_THE_TABLE_IS_INCONSISTENT:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_ONE_OF_PERIODS_IN_THE_TABLE_IS_INCONSISTENT_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_NUMBER_OF_POLICY_SUSPEND_PERIODS:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_NUMBER_OF_POLICY_SUSPEND_PERIODS_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_SUSPEND_PERIODS_REQUEST_PARAMETER_NOT_SUPPORTED:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_SUSPEND_PERIODS_REQUEST_PARAMETER_NOT_SUPPORTED_STR;
	  break;
	default:
	  rv = 0;
	  goto cleanup;
	}
      break;
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_RESET_NODE_MANAGER_STATISTICS:
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_STATISTICS:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_MODE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_MODE_STR;
	  break;
	default:
	  rv = 0;
	  goto cleanup;
	}
      break;
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_CAPABILITIES:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_POLICY_TRIGGER_TYPE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_POLICY_TRIGGER_TYPE_STR;
	  break;
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_POLICY_TYPE:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_POLICY_TYPE_STR;
	  break;
	default:
	  rv = 0;
	  goto cleanup;
	}
      break;
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POWER_DRAW_RANGE:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID_STR;
	  break;
	default:
	  rv = 0;
	  goto cleanup;
	}
      break;
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_DESTINATION:
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_ALERT_DESTINATION:
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_VERSION:
    default:
      rv = 0;
      goto cleanup;
    }

  if (prefix)
    pstdout_fprintf (state_data->pstate,
		     stderr,
		     "%s : %s\n",
		     prefix,
		     str);
  else
    pstdout_fprintf (state_data->pstate,
		     stderr,
		     "%s\n",
		     str);
  rv = 1;
 cleanup:
  return (rv);
}

/* Returns 1 if success, 0 if error but don't fail out, -1 on error */
static int
_ipmi_oem_intelnm_get_node_manager_statistics_common (ipmi_oem_state_data_t *state_data,
                                                      uint8_t target_channel_number,
                                                      uint8_t target_slave_address,
                                                      uint8_t target_lun,
                                                      uint8_t mode,
                                                      uint8_t domainid,
                                                      uint8_t policyid,
                                                      uint16_t *current,
                                                      uint16_t *minimum,
                                                      uint16_t *maximum,
                                                      uint16_t *average,
                                                      uint32_t *statistics_reporting_period,
                                                      uint8_t *policy_global_administrative_state,
                                                      uint8_t *policy_operational_state,
                                                      uint8_t *measurements_state,
                                                      uint8_t *policy_activation_state)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (current);
  assert (minimum);
  assert (maximum);
  assert (average);
  assert (statistics_reporting_period);
  assert (policy_global_administrative_state);
  assert (policy_operational_state);
  assert (measurements_state);
  assert (policy_activation_state);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_statistics_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_get_node_manager_statistics (state_data->ipmi_ctx,
                                                                   target_channel_number,
                                                                   target_slave_address,
                                                                   target_lun,
                                                                   mode,
                                                                   domainid,
                                                                   policyid,
                                                                   obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	  && ipmi_check_completion_code (obj_cmd_rs,
					 IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_MODE))
	rv = 0;
      else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;

	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     NULL,
							     obj_cmd_rs)) < 0)
	    goto cleanup;

	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_get_node_manager_statistics: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "current",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'current': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*current) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "minimum",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'minimum': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*minimum) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "maximum",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'maximum': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*maximum) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "average",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'average': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*average) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "statistics_reporting_period",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'statistics_reporting_period': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*statistics_reporting_period) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "policy_global_administrative_state",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'policy_global_administrative_state': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*policy_global_administrative_state) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "policy_operational_state",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'policy_operational_state': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*policy_operational_state) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "measurements_state",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'measurements_state': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*measurements_state) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "policy_activation_state",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'policy_activation_state': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*policy_activation_state) = val;

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_intelnm_get_node_manager_statistics (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  uint8_t policy_trigger_type = 0;
  uint16_t current;
  uint16_t minimum;
  uint16_t maximum;
  uint16_t average;
  uint32_t statistics_reporting_period;
  uint8_t policy_global_administrative_state;
  uint8_t policy_operational_state;
  uint8_t measurements_state;
  uint8_t policy_activation_state;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  
  if (state_data->prog_data->args->oem_options_count)
    {
      int i;
      
      for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
        {
          char *key = NULL;
          char *value = NULL;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
	      uint8_t domainid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &domainid_tmp) < 0)
                goto cleanup;

              if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
                }
              
              domainid = domainid_tmp;
            }
          else if (!strcasecmp (key, "policyid"))
            {
	      uint8_t policyid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &policyid_tmp) < 0)
                goto cleanup;

              policyid = policyid_tmp;
              policyid_specified++;
            }
          else
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid option\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          free (key);
          free (value);
        }
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (policyid_specified)
    {
      if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rs)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_create: %s\n",
                           strerror (errno));
          goto cleanup;
        }

      if (ipmi_cmd_oem_intel_node_manager_get_node_manager_policy (state_data->ipmi_ctx,
                                                                   target_channel_number,
                                                                   target_slave_address,
                                                                   target_lun,
                                                                   domainid,
                                                                   policyid,
                                                                   obj_cmd_rs) < 0)
        {
	  if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	    {
	      int eret;
	      
	      if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
								 NULL,
								 obj_cmd_rs)) < 0)
		goto cleanup;
	      
	      if (!eret)
		goto efallthrough;
	    }
	  else
	    {
	    efallthrough:
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_cmd_oem_intel_node_manager_get_node_manager_policy: %s\n",
			       ipmi_ctx_errormsg (state_data->ipmi_ctx));
	    }
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "policy_trigger_type",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'policy_trigger_type': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
	  goto cleanup;
        }
      policy_trigger_type = val;
    }

  if (!policyid_specified
      || (policyid_specified
	  && policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER))
    {
      uint8_t mode;
      int ret;

      if (!policyid_specified)
        mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_POWER_STATISTICS;
      else
        mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_POWER_STATISTICS;

      if ((ret = _ipmi_oem_intelnm_get_node_manager_statistics_common (state_data,
								       target_channel_number,
								       target_slave_address,
								       target_lun,
								       mode,
								       domainid,
								       policyid,
								       &current,
								       &minimum,
								       &maximum,
								       &average,
								       &statistics_reporting_period,
								       &policy_global_administrative_state,
								       &policy_operational_state,
								       &measurements_state,
								       &policy_activation_state)) < 0)
        goto cleanup;
      
      if (ret)
	{
	  pstdout_printf (state_data->pstate,
			  "Current Power                                 : %u Watts\n",
			  current);
	  
	  pstdout_printf (state_data->pstate,
			  "Minimum Power                                 : %u Watts\n",
			  minimum);
	  
	  pstdout_printf (state_data->pstate,
			  "Maximum Power                                 : %u Watts\n",
			  maximum);
      
	  pstdout_printf (state_data->pstate,
			  "Average Power                                 : %u Watts\n",
			  average);
      
	  pstdout_printf (state_data->pstate,
			  "Power Statistics Reporting Period             : %u seconds\n",
			  statistics_reporting_period);
      
	  /* achu: assume policy outputs only relevant if policy indicated */
      
	  if (policyid_specified)
	    {
	      pstdout_printf (state_data->pstate,
			      "Power Policy Administrative State             : %s\n",
			      (policy_global_administrative_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_GLOBAL_ADMINISTRATIVE_STATE_ENABLED) ? "Enabled" : "Disabled");
	      
	      pstdout_printf (state_data->pstate,
			      "Power Policy Operational State                : %s\n",
			      (policy_operational_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_OPERATIONAL_STATE_ACTIVELY_MONITORING_DEFINED_TRIGGER) ? "Active" : "Suspended");
	      
	      pstdout_printf (state_data->pstate,
			      "Power Policy Activation State                 : %s\n",
			      (policy_activation_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ACTIVATION_STATE_TRIGGERED_AND_ACTIVELY_LIMITING_TARGET) ? "Triggered" : "Not Triggered");
	    }
	  else
	    pstdout_printf (state_data->pstate,
			    "Power Global Administrative State             : %s\n",
			    (policy_global_administrative_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_GLOBAL_ADMINISTRATIVE_STATE_ENABLED) ? "Enabled" : "Disabled");
	  
	  pstdout_printf (state_data->pstate,
			  "Power Measurements State                      : %s\n",
			  (measurements_state == IPMI_OEM_INTEL_NODE_MANAGER_MEASUREMENTS_STATE_IN_PROGRESS) ? "In Progress" : "Suspended");
	}
    }

  if (!policyid_specified
      || (policyid_specified
	  && policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER))
    {
      uint8_t mode;
      int ret;

      if (!policyid_specified)
        mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_INLET_TEMPERATURE_STATISTICS;
      else
        mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_TRIGGER_STATISTICS;
      
      if ((ret = _ipmi_oem_intelnm_get_node_manager_statistics_common (state_data,
								       target_channel_number,
								       target_slave_address,
								       target_lun,
								       mode,
								       domainid,
								       policyid,
								       &current,
								       &minimum,
								       &maximum,
								       &average,
								       &statistics_reporting_period,
								       &policy_global_administrative_state,
								       &policy_operational_state,
								       &measurements_state,
								       &policy_activation_state)) < 0)
        goto cleanup;
      
      if (ret)
	{
	  if (!policyid_specified)
	    pstdout_printf (state_data->pstate, "\n");
	  
	  pstdout_printf (state_data->pstate,
			  "Current Inlet Temperature                     : %u Celsius\n",
			  current);
	  
	  pstdout_printf (state_data->pstate,
			  "Minimum Inlet Temperature                     : %u Celsius\n",
			  minimum);
	  
	  pstdout_printf (state_data->pstate,
			  "Maximum Inlet Temperature                     : %u Celsius\n",
			  maximum);
	  
	  pstdout_printf (state_data->pstate,
			  "Average Inlet Temperature                     : %u Celsius\n",
			  average);
	  
	  pstdout_printf (state_data->pstate,
			  "Inlet Temperature Statistics Reporting Period : %u seconds\n",
			  statistics_reporting_period);
	  
	  /* achu: assume policy outputs only relevant if policy indicated */
	  
	  if (policyid_specified)
	    {
	      pstdout_printf (state_data->pstate,
			      "Inlet Temperature Policy Administrative State : %s\n",
			      (policy_global_administrative_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_GLOBAL_ADMINISTRATIVE_STATE_ENABLED) ? "Enabled" : "Disabled");
	      
	      pstdout_printf (state_data->pstate,
			      "Inlet Temperature Policy Operational State    : %s\n",
			      (policy_operational_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_OPERATIONAL_STATE_ACTIVELY_MONITORING_DEFINED_TRIGGER) ? "Active" : "Suspended");
	      
	      pstdout_printf (state_data->pstate,
			      "Inlet Temperature Policy Activation State     : %s\n",
			      (policy_activation_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ACTIVATION_STATE_TRIGGERED_AND_ACTIVELY_LIMITING_TARGET) ? "Triggered" : "Not Triggered");
	    }
	  else
	    pstdout_printf (state_data->pstate,
			    "Inlet Temperature Global Administrative State : %s\n",
			    (policy_global_administrative_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_GLOBAL_ADMINISTRATIVE_STATE_ENABLED) ? "Enabled" : "Disabled");
	  
	  pstdout_printf (state_data->pstate,
			  "Inlet Temperature Measurements State          : %s\n",
			  (measurements_state == IPMI_OEM_INTEL_NODE_MANAGER_MEASUREMENTS_STATE_IN_PROGRESS) ? "In Progress" : "Suspended");
	}
    }
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv); 
}

int
ipmi_oem_intelnm_reset_node_manager_statistics (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t mode = 0;
  uint8_t domainid = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  int rv = -1;

  assert (state_data);

  if (state_data->prog_data->args->oem_options_count)
    {
      int i;
      
      for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
        {
          char *key = NULL;
          char *value = NULL;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
	      uint8_t domainid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &domainid_tmp) < 0)
                goto cleanup;

              if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
                }
              
              domainid = domainid_tmp;
            }
          else if (!strcasecmp (key, "policyid"))
            {
	      uint8_t policyid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &policyid_tmp) < 0)
                goto cleanup;

              policyid = policyid_tmp;
              policyid_specified++;
            }
          else
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid option\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          free (key);
          free (value);
        }
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_reset_node_manager_statistics_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (!policyid_specified)
    mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_RESET_GLOBAL_STATISTICS;
  else
    mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_PER_POLICY_STATISTICS;
  
  if (ipmi_cmd_oem_intel_node_manager_reset_node_manager_statistics (state_data->ipmi_ctx,
                                                                     target_channel_number,
                                                                     target_slave_address,
                                                                     target_lun,
                                                                     mode,
                                                                     domainid,
                                                                     policyid,
                                                                     obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;

	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     NULL,
							     obj_cmd_rs)) < 0)
	    goto cleanup;

	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_reset_node_manager_statistics: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_ipmi_oem_intelnm_get_node_manager_capabilities_common (ipmi_oem_state_data_t *state_data,
							uint8_t target_channel_number,
							uint8_t target_slave_address,
							uint8_t target_lun,
							uint8_t domain_id,
							uint8_t policy_trigger_type,
							uint8_t policy_type)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t max_concurrent_settings;
  uint16_t max_power_thermal;
  uint16_t min_power_thermal;
  uint32_t min_correction_time;
  uint32_t max_correction_time;
  uint16_t min_statistics_reporting_period;
  uint16_t max_statistics_reporting_period;
  uint8_t limiting_type;
  char *limiting_type_str;
  uint8_t limiting_based_on;
  char *limiting_based_on_str;
  uint64_t val;
  char *policy_trigger_type_str = NULL;
  char *policy_type_str = NULL;
  int rv = -1;

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_VALID (policy_trigger_type));
  assert (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_VALID (policy_type));

  switch (policy_trigger_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER:
      policy_trigger_type_str = "No Policy Trigger";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER:
      policy_trigger_type_str = "Inlet Temperature Limit Policy Trigger";
      break;
    }

  switch (policy_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY:
      policy_type_str = "Power Control Policy";
      break;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rs)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
		  "Capabilities for Domain ID = %u, Policy Trigger Type = %s, Policy Type = %s\n\n",
		  domain_id,
		  policy_trigger_type_str,
		  policy_type_str);

  if (ipmi_cmd_oem_intel_node_manager_get_node_manager_capabilities (state_data->ipmi_ctx,
								     target_channel_number,
								     target_slave_address,
								     target_lun,
								     domain_id,
								     policy_trigger_type,
								     policy_type,
								     obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;
	  
	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     "Error",
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_get_node_manager_capabilities: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs,
		    "max_concurrent_settings",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'max_concurrent_settings': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  max_concurrent_settings = val;
  
  if (FIID_OBJ_GET (obj_cmd_rs,
		    "max_power_thermal",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'max_power_thermal': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  max_power_thermal = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "min_power_thermal",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'min_power_thermal': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  min_power_thermal = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "max_correction_time",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'max_correction_time': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  max_correction_time = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "min_correction_time",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'min_correction_time': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  min_correction_time = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "max_statistics_reporting_period",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'max_statistics_reporting_period': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  max_statistics_reporting_period = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "min_statistics_reporting_period",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'min_statistics_reporting_period': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  min_statistics_reporting_period = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "domain_limiting_scope.limiting_type",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'domain_limiting_scope.limiting_type': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  limiting_type = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "domain_limiting_scope.limiting_based_on",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'domain_limiting_scope.limiting_based_on': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  limiting_based_on = val;

  switch (limiting_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_TYPE_PLATFORM_POWER_LIMITING:
      limiting_type_str = "platform power limiting";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_TYPE_CPU_POWER_LIMITING:
      limiting_type_str = "CPU power limiting";
      break;
    default:
      limiting_type_str = "Unspecified";
      break;
    }

  switch (limiting_based_on)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_BASED_ON_WALL_INPUT_POWER_PSU_INPUT_POWER:
      limiting_based_on_str = "Wall input power / PSU input power";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_BASED_ON_DC_POWER_PSU_OUTPUT_POWER_OR_BLADED_SYSTEM:
      limiting_based_on_str = "DC power - PSU output power or bladed system";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "Max Concurrent Settings         : %u\n",
		  max_concurrent_settings);

  if (policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER)
    {
      pstdout_printf (state_data->pstate,
		      "Max Power                       : %u W\n",
		      max_power_thermal);
      
      pstdout_printf (state_data->pstate,
		      "Min Power                       : %u W\n",
		      min_power_thermal);
    }
  else	/* policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER */
    {
      pstdout_printf (state_data->pstate,
		      "Max Thermal                     : %u C\n",
		      max_power_thermal);
      
      pstdout_printf (state_data->pstate,
		      "Min Thermal                     : %u C\n",
		      min_power_thermal);
    }
  
  pstdout_printf (state_data->pstate,
		  "Max Correction Time             : %u ms\n",
		  max_correction_time);
  
  pstdout_printf (state_data->pstate,
		  "Min Correction Time             : %u ms\n",
		  min_correction_time);
  
  pstdout_printf (state_data->pstate,
		  "Max Statistics Reporting Period : %u s\n",
		  max_statistics_reporting_period);
  
  pstdout_printf (state_data->pstate,
		  "Min Statistics Reporting Period : %u s\n",
		  min_statistics_reporting_period);

  pstdout_printf (state_data->pstate,
		  "Limiting Type                   : %s\n",
		  limiting_type_str);

  pstdout_printf (state_data->pstate,
		  "Limiting Source                 : %s\n",
		  limiting_based_on_str);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv); 
}

int
ipmi_oem_intelnm_get_node_manager_capabilities (ipmi_oem_state_data_t *state_data)
{
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid_defaults[] = {0};
  unsigned int domainid_defaults_len = 1;
  uint8_t policytrigger_defaults[] = {IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER,
				      IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER};
  unsigned int policytrigger_defaults_len = 2;
  uint8_t policytype_defaults[] = {IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY};
  unsigned int policytype_defaults_len = 1;
  uint8_t domainid_input[1];
  uint8_t policytrigger_input[1];
  uint8_t policytype_input[1];
  uint8_t *domainid_array;
  unsigned int domainid_array_len;
  uint8_t *policytrigger_array;
  unsigned int policytrigger_array_len;
  uint8_t *policytype_array;
  unsigned int policytype_array_len;
  unsigned int i, j, k;
  int first_output_flag = 0;
  int rv = -1;

  assert (state_data);
  
  /* assume going to iterate through defaults */
  domainid_array = domainid_defaults;
  domainid_array_len = domainid_defaults_len;

  policytrigger_array = policytrigger_defaults;
  policytrigger_array_len = policytrigger_defaults_len;

  policytype_array = policytype_defaults;
  policytype_array_len = policytype_defaults_len;

  if (state_data->prog_data->args->oem_options_count)
    {
      int i;
      
      for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
        {
          char *key = NULL;
          char *value = NULL;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
	      uint8_t domainid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &domainid_tmp) < 0)
                goto cleanup;

              if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
                }
              
	      domainid_input[0] = domainid_tmp;

	      domainid_array = domainid_input;
	      domainid_array_len = 1;
            }
          else if (!strcasecmp (key, "policytrigger"))
            {
	      if (strcasecmp (value, "none")
		  && strcasecmp (value, "inlet"))
		{
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid policy trigger type\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
		}

	      if (!strcasecmp (value, "none"))
		policytrigger_input[0] = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER;
	      else /* !strcasecmp (value, "inlet") */
		policytrigger_input[0] = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER;

	      policytrigger_array = policytrigger_input;
	      policytrigger_array_len = 1;
            }
          else if (!strcasecmp (key, "policytype"))
            {
	      if (strcasecmp (value, "powercontrol"))
		{
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid policy type\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
		}

	      /* !strcasecmp (value, "powercontrol") */
	      policytype_input[0] = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY;

	      policytype_array = policytype_input;
	      policytype_array_len = 1;
            }
          else
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid option\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          free (key);
          free (value);
        }
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  for (i = 0; i < domainid_array_len; i++)
    {
      for (j = 0; j < policytrigger_array_len; j++)
	{
	  for (k = 0; k < policytype_array_len; k++)
	    {
	      if (first_output_flag)
		pstdout_printf (state_data->pstate, "\n");

	      if (_ipmi_oem_intelnm_get_node_manager_capabilities_common (state_data,
									  target_channel_number,
									  target_slave_address,
									  target_lun,
									  domainid_array[i],
									  policytrigger_array[j],
									  policytype_array[k]) < 0)
		goto cleanup;

	      first_output_flag++;
	    }
	}
    }

  rv = 0;
 cleanup:
  return (rv); 
}

int
ipmi_oem_intelnm_node_manager_policy_control (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = 0;
  int domainid_specified = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  uint8_t policy_enable_disable;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 1); 

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (state_data->prog_data->args->oem_options_count > 1)
    {
      int i;
      
      for (i = 1; i < state_data->prog_data->args->oem_options_count; i++)
        {
          char *key = NULL;
          char *value = NULL;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
	      uint8_t domainid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &domainid_tmp) < 0)
                goto cleanup;

              if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
                }
              
              domainid = domainid_tmp;
	      domainid_specified++;
            }
          else if (!strcasecmp (key, "policyid"))
            {
	      uint8_t policyid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &policyid_tmp) < 0)
                goto cleanup;

              policyid = policyid_tmp;
              policyid_specified++;
	    }
          else
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid option\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          free (key);
          free (value);
        }
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!domainid_specified)
    {
      if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
	policy_enable_disable = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_GLOBAL_ENABLE_NODE_MANAGER_POLICY_CONTROL;
      else
	policy_enable_disable = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_GLOBAL_DISABLE_NODE_MANAGER_POLICY_CONTROL;
    }
  else
    {
      if (!policyid_specified)
	{
	  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
	    policy_enable_disable = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_DOMAIN_ENABLE_NODE_MANAGER_POLITICES;
	  else
	    policy_enable_disable = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_DOMAIN_DISABLE_NODE_MANAGER_POLICIES;
	}
      else
	{
	  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
	    policy_enable_disable = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_POLICY_ENABLE_NODE_MANAGER_POLITICES;
	  else
	    policy_enable_disable = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLE_DISABLE_PER_POLICY_DISABLE_NODE_MANAGER_POLICIES;
	}
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control_rs)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }
  
  if (ipmi_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control (state_data->ipmi_ctx,
										  target_channel_number,
										  target_slave_address,
										  target_lun,
										  policy_enable_disable,
										  domainid,
										  policyid,
										  obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;
	  
	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     NULL,
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_enable_disable_node_manager_policy_control: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
    }
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv); 
}

static void
_ipmi_oem_intelnm_get_node_manager_policy_output_header (ipmi_oem_state_data_t *state_data,
							 uint8_t domain_id,
							 uint8_t policy_id)
{
  assert (state_data);

  pstdout_printf (state_data->pstate,
		  "Capabilities for Domain ID = %u, Policy ID = %u\n\n",
		  domain_id,
		  policy_id);
}

static int
_ipmi_oem_intelnm_get_node_manager_policy_common (ipmi_oem_state_data_t *state_data,
						  uint8_t target_channel_number,
						  uint8_t target_slave_address,
						  uint8_t target_lun,
						  uint8_t domain_id,
						  uint8_t policy_id,
						  int searching_domain_id,
						  int searching_policy_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t policy_enabled;
  uint8_t per_domain_node_manager_policy_control_enabled;
  uint8_t global_node_manager_policy_control_enabled;
  uint8_t policy_trigger_type;
  uint8_t policy_type;
  uint8_t policy_exception_actions_send_alert;
  uint8_t policy_exception_actions_shutdown_system;
  uint16_t power_limit;
  uint32_t correction_time_limit;
  uint16_t policy_trigger_limit;
  uint16_t statistics_reporting_period;
  char *policy_trigger_type_str = NULL;
  char *policy_trigger_type_units_str = NULL;
  char *policy_type_str = NULL;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rs)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_get_node_manager_policy (state_data->ipmi_ctx,
							       target_channel_number,
							       target_slave_address,
							       target_lun,
							       domain_id,
							       policy_id,
							       obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;

	  if (!state_data->prog_data->args->verbose_count)
	    {
	      if (searching_domain_id
		  && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		goto cleanup;
	      
	      if (searching_policy_id
		  && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID) == 1)
		goto cleanup;
	    }

	  if (searching_domain_id
	      || searching_policy_id)
	    _ipmi_oem_intelnm_get_node_manager_policy_output_header (state_data, domain_id, policy_id);

	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     "Error",
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_get_node_manager_policy: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}

      if ((searching_domain_id
	   || searching_policy_id)
	  && state_data->prog_data->args->verbose_count)
	rv = 0; 

      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_enabled",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_enabled': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_enabled = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "per_domain_node_manager_policy_control_enabled",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'per_domain_node_manager_policy_control_enabled': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  per_domain_node_manager_policy_control_enabled = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "global_node_manager_policy_control_enabled",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'global_node_manager_policy_control_enabled': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  global_node_manager_policy_control_enabled = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_trigger_type",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_trigger_type': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_trigger_type = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_type",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_type': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_type = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_exception_actions.send_alert",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_exception_actions.send_alert': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_exception_actions_send_alert = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_exception_actions.shutdown_system",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_exception_actions.shutdown_system': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_exception_actions_shutdown_system = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "power_limit",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'power_limit': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  power_limit = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "correction_time_limit",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'correction_time_limit': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  correction_time_limit = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_trigger_limit",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_trigger_limit': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_trigger_limit = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "statistics_reporting_period",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'statistics_reporting_period': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  statistics_reporting_period = val;

  switch (policy_trigger_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER:
      policy_trigger_type_str = "No Policy Trigger";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER:
      policy_trigger_type_str = "Inlet Temperature Limit Policy Trigger";
      break;
    default:
      policy_trigger_type_str = "Unknown";
      break;
    }

  switch (policy_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY:
      policy_type_str = "Power Control Policy";
      break;
    default:
      policy_type_str = "Unknown";
      break;
    }

  if (searching_domain_id
      || searching_policy_id)
    _ipmi_oem_intelnm_get_node_manager_policy_output_header (state_data, domain_id, policy_id);

  pstdout_printf (state_data->pstate,
		  "Policy                                  : %s\n",
		  (policy_enabled == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED) ? "enabled" : "disabled");
  
  pstdout_printf (state_data->pstate,
		  "Per Domain Node Manager Policy Control  : %s\n",
		  (per_domain_node_manager_policy_control_enabled == IPMI_OEM_INTEL_NODE_MANAGER_PER_DOMAIN_NODE_MANAGER_POLICY_CONTROL_ENABLED) ? "enabled" : "disabled");

  pstdout_printf (state_data->pstate,
		  "Global Node Manager Policy Control      : %s\n",
		  (global_node_manager_policy_control_enabled == IPMI_OEM_INTEL_NODE_MANAGER_GLOBAL_NODE_MANAGER_POLICY_CONTROL_ENABLED) ? "enabled" : "disabled");

  pstdout_printf (state_data->pstate,
		  "Policy Trigger Type                     : %s\n",
		  policy_trigger_type_str);

  pstdout_printf (state_data->pstate,
		  "Policy Type                             : %s\n",
		  policy_type_str);
  
  pstdout_printf (state_data->pstate,
		  "Policy Exception Send Alert Action      : %s\n",
		  (policy_exception_actions_send_alert == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE) ? "enabled" : "disabled");

  pstdout_printf (state_data->pstate,
		  "Policy Exception Shutdown System Action : %s\n",
		  (policy_exception_actions_shutdown_system == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE) ? "enabled" : "disabled");
		  
  pstdout_printf (state_data->pstate,
		  "Power Limit                             : %u W\n",
		  power_limit);

  pstdout_printf (state_data->pstate,
		  "Correction Time Limit                   : %u ms\n",
		  correction_time_limit);

  switch (policy_trigger_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER:
      policy_trigger_type_units_str = "W";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER:
      policy_trigger_type_units_str = "C";
      break;
    default:
      policy_trigger_type_units_str = "?";
      break;
    }
  
  pstdout_printf (state_data->pstate,
		  "Policy Trigger Limit                    : %u %s\n",
		  policy_trigger_limit,
		  policy_trigger_type_units_str);
  
  pstdout_printf (state_data->pstate,
		  "Statistics Reporting Period             : %u s\n",
		  statistics_reporting_period);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv); 
}

int
ipmi_oem_intelnm_get_node_manager_policy (ipmi_oem_state_data_t *state_data)
{
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid_min = 0;
  uint8_t domainid_max = 0;
  uint8_t policyid_min = 0;
  uint8_t policyid_max = 255;
  int domainid_specified = 0;
  int policyid_specified = 0;
  unsigned int i, j;
  int first_output_flag = 0;
  int rv = -1;

  assert (state_data);
  
  if (state_data->prog_data->args->oem_options_count)
    {
      int i;
      
      for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
        {
          char *key = NULL;
          char *value = NULL;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
	      uint8_t domainid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &domainid_tmp) < 0)
                goto cleanup;

              if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
                }
              
	      domainid_min = domainid_tmp;
	      domainid_max = domainid_tmp;
	      domainid_specified++;
		}
          else if (!strcasecmp (key, "policyid"))
            {
	      uint8_t policyid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &policyid_tmp) < 0)
                goto cleanup;
              
	      policyid_min = policyid_tmp;
	      policyid_max = policyid_tmp;
	      policyid_specified++;
            }
          else
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid option\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          free (key);
          free (value);
        }
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  for (i = domainid_min; i <= domainid_max; i++)
    {
      for (j = policyid_min; j <= policyid_max; j++)
	{
	  if (first_output_flag)
	    pstdout_printf (state_data->pstate, "\n");

	  if (_ipmi_oem_intelnm_get_node_manager_policy_common (state_data,
								target_channel_number,
								target_slave_address,
								target_lun,
								i,
								j,
								domainid_specified ? 0 : 1,
								policyid_specified ? 0 : 1) < 0)
	    goto cleanup;
	  
	  first_output_flag++;
	}
    }

  rv = 0;
 cleanup:
  return (rv); 
}

int
ipmi_oem_intelnm_set_node_manager_policy (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = 0;
  int domainid_specified = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  uint8_t policytrigger = 0;
  int policytrigger_specified = 0;
  uint16_t powerlimit = 0;
  int powerlimit_specified = 0;
  uint32_t correctiontimelimit = 0;
  int correctiontimelimit_specified = 0;
  uint16_t policytriggerlimit = 0;
  int policytriggerlimit_specified = 0;
  uint16_t statisticsreportingperiod = 0;
  int statisticsreportingperiod_specified = 0;
  uint8_t policystate = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED;
  uint8_t policyexceptionaction_alert = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE;
  uint8_t policyexceptionaction_shutdown = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 7);

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
				    i,
				    &key,
				    &value) < 0)
	goto cleanup;

      if (!strcasecmp (key, "domainid"))
	{
	  uint8_t domainid_tmp;
	  
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &domainid_tmp) < 0)
	    goto cleanup;
	  
	  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  domainid = domainid_tmp;
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  uint8_t policyid_tmp;
	  
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid_tmp) < 0)
	    goto cleanup;
	  
	  policyid = policyid_tmp;
	  policyid_specified++;
	}
      else if (!strcasecmp (key, "policytrigger"))
	{
	  if (strcasecmp (value, "none")
	      && strcasecmp (value, "inlet"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid policy trigger type\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "none"))
	    policytrigger = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER;
	  else
	    policytrigger = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER;
	  
	  policytrigger_specified++;
	}
      else if (!strcasecmp (key, "powerlimit"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data, i, value, &powerlimit) < 0)
	    goto cleanup;
	  
	  powerlimit_specified++;
	}
      else if (!strcasecmp (key, "correctiontimelimit"))
	{
	  if (ipmi_oem_parse_4_byte_field (state_data, i, value, &correctiontimelimit) < 0)
	    goto cleanup;
	  
	  correctiontimelimit_specified++;
	}
      else if (!strcasecmp (key, "policytriggerlimit"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data, i, value, &policytriggerlimit) < 0)
	    goto cleanup;
	  
	  policytriggerlimit_specified++;
	}
      else if (!strcasecmp (key, "statisticsreportingperiod"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data, i, value, &statisticsreportingperiod) < 0)
	    goto cleanup;
	  
	  statisticsreportingperiod_specified++;
	}
      else if (!strcasecmp (key, "policystate"))
	{
	  if (strcasecmp (value, "enable")
	      && strcasecmp (value, "disable"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid policy state\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "enable"))
	    policystate = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED;
	  else
	    policystate = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_DISABLED;
	  
	}
      else if (!strcasecmp (key, "policyexceptionaction"))
	{
	  if (strcasecmp (value, "alert")
	      && strcasecmp (value, "shutdown"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid policy exception action\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "alert"))
	    policyexceptionaction_alert = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE; 
	  
	  if (!strcasecmp (value, "shutdown"))
	    policyexceptionaction_shutdown = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE; 
	}
      else
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s invalid OEM option argument '%s' : invalid option\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command,
			   state_data->prog_data->args->oem_options[i]);
	  goto cleanup;
	}
      
      free (key);
      free (value);
    }

  if (!domainid_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "domain ID must be specified\n");
      goto cleanup;
    }

  if (!policyid_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy ID must be specified\n");
      goto cleanup;
    }

  if (!policytrigger_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy trigger type must be specified\n");
      goto cleanup;
    }

  if (!powerlimit_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "power limit must be specified\n");
      goto cleanup;
    }

  if (!correctiontimelimit_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "correction time limit must be specified\n");
      goto cleanup;
    }

  if (!policytriggerlimit_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy trigger limit must be specified\n");
      goto cleanup;
    }

  if (!statisticsreportingperiod_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "statistics reporting period must be specified\n");
      goto cleanup;
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_set_node_manager_policy (state_data->ipmi_ctx,
							       target_channel_number,
							       target_slave_address,
							       target_lun,
							       domainid,
							       policystate,
							       policyid,
							       policytrigger,
							       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_ADD_POWER_POLICY,
							       policyexceptionaction_alert,
							       policyexceptionaction_shutdown,
							       powerlimit,
							       correctiontimelimit,
							       policytriggerlimit,
							       statisticsreportingperiod,
							       obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;
	  
	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     NULL,
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_set_node_manager_policy: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_intelnm_remove_node_manager_policy (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = 0;
  int domainid_specified = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
				    i,
				    &key,
				    &value) < 0)
	goto cleanup;
      
      if (!strcasecmp (key, "domainid"))
	{
	  uint8_t domainid_tmp;
	  
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &domainid_tmp) < 0)
	    goto cleanup;
	  
	  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  domainid = domainid_tmp;
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  uint8_t policyid_tmp;
	  
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid_tmp) < 0)
	    goto cleanup;
	  
	  policyid = policyid_tmp;
	  policyid_specified++;
	}
      else
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s invalid OEM option argument '%s' : invalid option\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command,
			   state_data->prog_data->args->oem_options[i]);
	  goto cleanup;
	}
      
      free (key);
      free (value);
    }

  if (!domainid_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "domain ID must be specified\n");
      goto cleanup;
    }

  if (!policyid_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy ID must be specified\n");
      goto cleanup;
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_set_node_manager_policy (state_data->ipmi_ctx,
							       target_channel_number,
							       target_slave_address,
							       target_lun,
							       domainid,
							       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_DISABLED,
							       policyid,
							       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER,
							       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_POLICY_POINTED_BY_POLICY_ID_SHALL_BE_REMOVED,
							       /* all remaining bytes ignored, fill in with anything */
							       0,
							       0,
							       0,
							       0,
							       0,
							       0,
							       obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;
	  
	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     NULL,
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_set_node_manager_policy: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static void
_ipmi_oem_intelnm_get_node_manager_alert_thresholds_output_header (ipmi_oem_state_data_t *state_data,
								   uint8_t domain_id,
								   uint8_t policy_id)
{
  assert (state_data);
  
  pstdout_printf (state_data->pstate,
		  "Thresholds for Domain ID = %u, Policy ID = %u\n\n",
		  domain_id,
		  policy_id);
}

static int
_ipmi_oem_intelnm_get_node_manager_alert_thresholds_common (ipmi_oem_state_data_t *state_data,
							    uint8_t target_channel_number,
							    uint8_t target_slave_address,
							    uint8_t target_lun,
							    uint8_t domain_id,
							    uint8_t policy_id,
							    int searching_domain_id,
							    int searching_policy_id)
{
  fiid_obj_t alert_threshold_obj_cmd_rs = NULL;
  fiid_obj_t policy_obj_cmd_rs = NULL;
  uint8_t number_of_alert_thresholds = 0;
  uint16_t alert_threshold1 = 0;
  uint16_t alert_threshold2 = 0;
  uint16_t alert_threshold3 = 0;
  uint8_t policy_trigger_type = 0;
  char *policy_trigger_type_units_str = NULL;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));

  if (!(alert_threshold_obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds_rs)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds (state_data->ipmi_ctx,
									 target_channel_number,
									 target_slave_address,
									 target_lun,
									 domain_id,
									 policy_id,
									 alert_threshold_obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;

	  if (!state_data->prog_data->args->verbose_count)
	    {
	      if (searching_domain_id
		  && ipmi_check_completion_code (alert_threshold_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		goto cleanup;
	      
	      if (searching_policy_id
		  && ipmi_check_completion_code (alert_threshold_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID) == 1)
		goto cleanup;
	    }

	  if (searching_domain_id
	      || searching_policy_id)
	    _ipmi_oem_intelnm_get_node_manager_alert_thresholds_output_header (state_data, domain_id, policy_id);

	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     "Error",
							     alert_threshold_obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough_alert_thresholds;
	}
      else
	{
	efallthrough_alert_thresholds:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_get_node_manager_alert_thresholds: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      
      if ((searching_domain_id
	   || searching_policy_id)
	  && state_data->prog_data->args->verbose_count)
	rv = 0; 
      
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (alert_threshold_obj_cmd_rs,
		    "number_of_alert_thresholds",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'number_of_alert_thresholds': %s\n",
		       fiid_obj_errormsg (alert_threshold_obj_cmd_rs));
      goto cleanup;
    }
  number_of_alert_thresholds = val;
  
  if (number_of_alert_thresholds)
    {
      if (number_of_alert_thresholds >= 1)
	{
	  if (FIID_OBJ_GET (alert_threshold_obj_cmd_rs,
			    "alert_threshold1",
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: 'alert_threshold1': %s\n",
			       fiid_obj_errormsg (alert_threshold_obj_cmd_rs));
	      goto cleanup;
	    }
	  alert_threshold1 = val;
	}
      
      if (number_of_alert_thresholds >= 2)
	{
	  if (FIID_OBJ_GET (alert_threshold_obj_cmd_rs,
			    "alert_threshold2",
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: 'alert_threshold2': %s\n",
			       fiid_obj_errormsg (alert_threshold_obj_cmd_rs));
	      goto cleanup;
	    }
	  alert_threshold2 = val;
	}
      
      if (number_of_alert_thresholds >= 3)
	{
	  if (FIID_OBJ_GET (alert_threshold_obj_cmd_rs,
			    "alert_threshold3",
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "fiid_obj_get: 'alert_threshold3': %s\n",
			       fiid_obj_errormsg (alert_threshold_obj_cmd_rs));
	      goto cleanup;
	    }
	  alert_threshold3 = val;
	}

      if (!(policy_obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_rs)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_create: %s\n",
                           strerror (errno));
          goto cleanup;
        }
      
      if (ipmi_cmd_oem_intel_node_manager_get_node_manager_policy (state_data->ipmi_ctx,
                                                                   target_channel_number,
                                                                   target_slave_address,
                                                                   target_lun,
                                                                   domain_id,
                                                                   policy_id,
                                                                   policy_obj_cmd_rs) < 0)
        {
	  if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	    {
	      int eret;
	      
	      if (!state_data->prog_data->args->verbose_count)
		{
		  if (searching_domain_id
		      && ipmi_check_completion_code (policy_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		    goto cleanup;
		  
		  if (searching_policy_id
		      && ipmi_check_completion_code (policy_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID) == 1)
		    goto cleanup;
		}
	      
	      if (searching_domain_id
		  || searching_policy_id)
		_ipmi_oem_intelnm_get_node_manager_alert_thresholds_output_header (state_data, domain_id, policy_id);
	      
	      if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
								 "Error",
								 policy_obj_cmd_rs)) < 0)
		goto cleanup;
	      
	      if (!eret)
		goto efallthrough_policy;
	    }
	  else
	    {
	    efallthrough_policy:
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_cmd_oem_intel_node_manager_get_node_manager_policy: %s\n",
			       ipmi_ctx_errormsg (state_data->ipmi_ctx));
	    }
	  
	  if ((searching_domain_id
	       || searching_policy_id)
	      && state_data->prog_data->args->verbose_count)
	    rv = 0; 
	  
	  goto cleanup;
        }
      
      if (FIID_OBJ_GET (policy_obj_cmd_rs,
                        "policy_trigger_type",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FIID_OBJ_GET: 'policy_trigger_type': %s\n",
                           fiid_obj_errormsg (policy_obj_cmd_rs));
	  goto cleanup;
        }
      policy_trigger_type = val;
      
      switch (policy_trigger_type)
	{
	case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER:
	  policy_trigger_type_units_str = "W";
	  break;
	case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER:
	  policy_trigger_type_units_str = "C";
	  break;
	default:
	  policy_trigger_type_units_str = "?";
	  break;
	}
    }
      
  if (searching_domain_id
      || searching_policy_id)
    _ipmi_oem_intelnm_get_node_manager_alert_thresholds_output_header (state_data, domain_id, policy_id);
  
  pstdout_printf (state_data->pstate,
		  "Number of Alert Thresholds : %u\n",
		  number_of_alert_thresholds);

  if (number_of_alert_thresholds >= 1)
    pstdout_printf (state_data->pstate,
		    "Alert Threshold 1          : %u %s\n",
		    alert_threshold1,
		    policy_trigger_type_units_str);
  else
    pstdout_printf (state_data->pstate,
		    "Alert Threshold 1          : N/A\n");

  if (number_of_alert_thresholds >= 2)
    pstdout_printf (state_data->pstate,
		    "Alert Threshold 2          : %u %s\n",
		    alert_threshold2,
		    policy_trigger_type_units_str);
  else
    pstdout_printf (state_data->pstate,
		    "Alert Threshold 2          : N/A\n");

  if (number_of_alert_thresholds >= 3)
    pstdout_printf (state_data->pstate,
		    "Alert Threshold 3          : %u %s\n",
		    alert_threshold3,
		    policy_trigger_type_units_str);
  else
    pstdout_printf (state_data->pstate,
		    "Alert Threshold 3          : N/A\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (alert_threshold_obj_cmd_rs);
  fiid_obj_destroy (policy_obj_cmd_rs);
  return (rv); 
}

int
ipmi_oem_intelnm_get_node_manager_alert_thresholds (ipmi_oem_state_data_t *state_data)
{
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid_min = 0;
  uint8_t domainid_max = 0;
  uint8_t policyid_min = 0;
  uint8_t policyid_max = 255;
  int domainid_specified = 0;
  int policyid_specified = 0;
  unsigned int i, j;
  int first_output_flag = 0;
  int rv = -1;

  assert (state_data);
  
  if (state_data->prog_data->args->oem_options_count)
    {
      int i;
      
      for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
        {
          char *key = NULL;
          char *value = NULL;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
	      uint8_t domainid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &domainid_tmp) < 0)
                goto cleanup;

              if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
                                   state_data->prog_data->args->oem_id,
                                   state_data->prog_data->args->oem_command,
                                   state_data->prog_data->args->oem_options[i]);
                  goto cleanup;
                }
              
	      domainid_min = domainid_tmp;
	      domainid_max = domainid_tmp;
	      domainid_specified++;
	    }
          else if (!strcasecmp (key, "policyid"))
            {
	      uint8_t policyid_tmp;

              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &policyid_tmp) < 0)
                goto cleanup;
              
	      policyid_min = policyid_tmp;
	      policyid_max = policyid_tmp;
	      policyid_specified++;
            }
          else
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid option\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          free (key);
          free (value);
        }
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  for (i = domainid_min; i <= domainid_max; i++)
    {
      for (j = policyid_min; j <= policyid_max; j++)
	{
	  if (first_output_flag)
	    pstdout_printf (state_data->pstate, "\n");
	  
	  if (_ipmi_oem_intelnm_get_node_manager_alert_thresholds_common (state_data,
									  target_channel_number,
									  target_slave_address,
									  target_lun,
									  i,
									  j,
									  domainid_specified ? 0 : 1,
									  policyid_specified ? 0 : 1) < 0)
	    goto cleanup;
	  
	  first_output_flag++;
	}
    }
  
  rv = 0;
 cleanup:
  return (rv); 
}

int
ipmi_oem_intelnm_set_node_manager_alert_thresholds (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = 0;
  int domainid_specified = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  uint16_t threshold1 = 0;
  int threshold1_specified = 0;
  uint16_t threshold2 = 0;
  int threshold2_specified = 0;
  uint16_t threshold3 = 0;
  int threshold3_specified = 0;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 2);

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
				    i,
				    &key,
				    &value) < 0)
	goto cleanup;

      if (!strcasecmp (key, "domainid"))
	{
	  uint8_t domainid_tmp;
	  
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &domainid_tmp) < 0)
	    goto cleanup;
	  
	  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domainid_tmp))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  domainid = domainid_tmp;
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  uint8_t policyid_tmp;
	  
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid_tmp) < 0)
	    goto cleanup;
	  
	  policyid = policyid_tmp;
	  policyid_specified++;
	}
      else if (!strcasecmp (key, "threshold1"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data, i, value, &threshold1) < 0)
	    goto cleanup;
	  
	  threshold1_specified++;
	}
      else if (!strcasecmp (key, "threshold2"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data, i, value, &threshold2) < 0)
	    goto cleanup;
	  
	  threshold2_specified++;
	}
      else if (!strcasecmp (key, "threshold3"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data, i, value, &threshold3) < 0)
	    goto cleanup;
	  
	  threshold3_specified++;
	}
      else
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s invalid OEM option argument '%s' : invalid option\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command,
			   state_data->prog_data->args->oem_options[i]);
	  goto cleanup;
	}
      
      free (key);
      free (value);
    }

  if (!domainid_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "domain ID must be specified\n");
      goto cleanup;
    }

  if (!policyid_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy ID must be specified\n");
      goto cleanup;
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds (state_data->ipmi_ctx,
									 target_channel_number,
									 target_slave_address,
									 target_lun,
									 domainid,
									 policyid,
									 threshold1_specified ? &threshold1 : NULL,
									 threshold2_specified ? &threshold2 : NULL,
									 threshold3_specified ? &threshold3 : NULL,
									 obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;
	  
	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     NULL,
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_set_node_manager_policy: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_intelnm_get_node_manager_version (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t node_manager_version;
  uint8_t ipmi_interface_version;
  uint8_t patch_version;
  uint8_t major_firmware_revision;
  uint8_t minor_firmware_revision;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_version_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_get_node_manager_version (state_data->ipmi_ctx,
                                                                target_channel_number,
                                                                target_slave_address,
                                                                target_lun,
                                                                obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  int eret;
	  
	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     NULL,
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough;
	}
      else
	{
	efallthrough:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_get_node_manager_version: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "node_manager_version",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'node_manager_version': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  node_manager_version = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "ipmi_interface_version",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'ipmi_interface_version': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipmi_interface_version = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "patch_version",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'patch_version': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  patch_version = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "major_firmware_revision",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'major_firmware_revision': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  major_firmware_revision = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "minor_firmware_revision",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'minor_firmware_revision': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  minor_firmware_revision = val;

  if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_1_0)
    pstdout_printf (state_data->pstate,
                    "Node Manager Version   : 1.0\n");
  else if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_1_5)
    pstdout_printf (state_data->pstate,
                    "Node Manager Version   : 1.5\n");
  else
    pstdout_printf (state_data->pstate,
                    "Node Manager Version   : %02Xh\n",
                    node_manager_version);

  if (ipmi_interface_version == IPMI_OEM_INTEL_NODE_MANAGER_IPMI_INTERFACE_VERSION_1_0)
    pstdout_printf (state_data->pstate,
                    "IPMI Interface Version : 1.0\n");
  else
    pstdout_printf (state_data->pstate,
                    "IPMI Interface Version : %02Xh\n",
                    ipmi_interface_version);

  pstdout_printf (state_data->pstate,
                  "Patch Version          : %u\n",
                  patch_version);
  
  /* minor firmware revision is BCD */
  pstdout_printf (state_data->pstate,
                  "Firmware Revision      : %u.%02X\n",
                  major_firmware_revision,
                  minor_firmware_revision);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
