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

	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data, obj_cmd_rs)) < 0)
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
          uint8_t domainid_tmp;
          uint8_t policyid_tmp;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
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
	      
	      if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data, obj_cmd_rs)) < 0)
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
          return (-1);
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
			      "Power Policy Administrative State           : %s\n",
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
          uint8_t domainid_tmp;
          uint8_t policyid_tmp;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

          if (!strcasecmp (key, "domainid"))
            {
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

	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data, obj_cmd_rs)) < 0)
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
	  
	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data, obj_cmd_rs)) < 0)
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
