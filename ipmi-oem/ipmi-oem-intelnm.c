/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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
#include <limits.h>
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

#define IPMI_OEM_INTELNM_STRING_MAX 128

static int
_ipmi_oem_intelnm_parse_domainid (ipmi_oem_state_data_t *state_data,
				  unsigned int option_num,
				  const char *value,
				  uint8_t *value_out)
{
  int rv = -1;
  
  assert (state_data);
  assert (value);
  assert (value_out);
  
  if (!strcasecmp (value, "platform"))
    (*value_out) = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  else if (!strcasecmp (value, "cpu"))
    (*value_out) = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM;
  else if (!strcasecmp (value, "memory"))
    (*value_out) = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MEMORY_SUBSYSTEM;
  else if (!strcasecmp (value, "highpowerio"))
    (*value_out) = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_HIGH_POWER_IO_SUBSYSTEM;
  else
    {
      if (ipmi_oem_parse_1_byte_field (state_data,
				       option_num,
				       value,
				       value_out) < 0)
	goto cleanup;
    }

  if (!IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID ((*value_out)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s' : invalid domain id\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[option_num]);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

/* achu:
 *
 * In Intel NM 2.0 specification, sensor numbers are now fixed and you
 * don't have to search the SDR for them.  We could check version of
 * NM on motherboard to determine if we need to search SDR or not, but
 * for time being we'll stick to the search SDR method b/c it will
 * always work.
 */
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
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_VALUE_OF_AGGRESSIVE_CPU_POWER_CORRECTION_FIELD:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_VALUE_OF_AGGRESSIVE_CPU_POWER_CORRECTION_FIELD_STR;
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
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_SUSPEND_PERIODS_REQUEST_PARAMETER_ILLEGAL:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_SUSPEND_PERIODS_REQUEST_PARAMETER_ILLEGAL_STR;
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
    case IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_LIMITING_POLICY_ID:
      switch (comp_code)
	{
	case IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_NO_POLICY_IS_CURRENTLY_LIMITING:
	  str = IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_NO_POLICY_IS_CURRENTLY_LIMITING_STR;
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

int
ipmi_oem_intelnm_get_node_manager_statistics (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_POWER_STATISTICS;
  int mode_specified = 0;
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  uint8_t policyid = 0;
  int policyid_specified = 0;
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
  unsigned int i;
      
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 1);

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
				    i,
				    &key,
				    &value) < 0)
	goto cleanup;

      if (!strcasecmp (key, "mode"))
	{
	  if (strcasecmp (value, "globalpower")
	      && strcasecmp (value, "globaltemp")
	      && strcasecmp (value, "policypower")
	      && strcasecmp (value, "policytrigger")
	      && strcasecmp (value, "policythrottling")
	      && strcasecmp (value, "hostunhandledrequests")
	      && strcasecmp (value, "hostresponsetime")
	      && strcasecmp (value, "cputhrottling")
	      && strcasecmp (value, "memorythrottling")
	      && strcasecmp (value, "hostcommfailure"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid mode\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }

	  if (!strcasecmp (value, "globalpower"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_POWER_STATISTICS;
	  else if (!strcasecmp (value, "globaltemp"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_INLET_TEMPERATURE_STATISTICS;
	  else if (!strcasecmp (value, "policypower"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_POWER_STATISTICS;
	  else if (!strcasecmp (value, "policytrigger"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_TRIGGER_STATISTICS;
	  else if (!strcasecmp (value, "policythrottling"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_THROTTLING_STATISTICS;
	  else if (!strcasecmp (value, "hostunhandledrequests"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_UNHANDLED_REQUESTS_STATISTICS;
	  else if (!strcasecmp (value, "hostresponsetime"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_RESPONSE_TIME_STATISTICS;
	  else if (!strcasecmp (value, "cputhrottling"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_CPU_THROTTLING_STATISTICS;
	  else if (!strcasecmp (value, "memorythrottling"))
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_MEMORY_THROTTLING_STATISTICS;
	  else /* !strcasecmp (value, "hostcommfailure") */ 
	    mode = IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_COMMUNICATION_FAILURE_STATISTICS;

	  mode_specified++;
	}
      else if (!strcasecmp (key, "domainid"))
	{
	  if (_ipmi_oem_intelnm_parse_domainid (state_data,
						i,
						value,
						&domainid) < 0)
	    goto cleanup;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid) < 0)
	    goto cleanup;

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

  if (!mode_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "mode must be specified\n");
      goto cleanup;
    }

  if ((mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_POWER_STATISTICS
       || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_TRIGGER_STATISTICS
       || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_THROTTLING_STATISTICS)
      && !policyid_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy id must be specified for given mode\n");
      goto cleanup;
    }

  if ((mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_UNHANDLED_REQUESTS_STATISTICS
       || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_RESPONSE_TIME_STATISTICS
       || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_CPU_THROTTLING_STATISTICS
       || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_MEMORY_THROTTLING_STATISTICS
       || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_COMMUNICATION_FAILURE_STATISTICS)
      && domainid != IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "domain id invalid for given mode\n");
      goto cleanup;
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

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
  current = val;
  
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
  minimum = val;

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
  maximum = val;

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
  average = val;

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
  policy_global_administrative_state = val;

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
  policy_operational_state = val;

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
  measurements_state = val;

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
  policy_activation_state = val;
      
  if (mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_POWER_STATISTICS
      || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_POWER_STATISTICS)
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
    }
  else if (mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_INLET_TEMPERATURE_STATISTICS
	   || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_TRIGGER_STATISTICS)
    {
      pstdout_printf (state_data->pstate,
		      "Current Temperature                           : %u Celsius\n",
		      current);
    
      pstdout_printf (state_data->pstate,
		      "Minimum Temperature                           : %u Celsius\n",
		      minimum);
    
      pstdout_printf (state_data->pstate,
		      "Maximum Temperature                           : %u Celsius\n",
		      maximum);
    
      pstdout_printf (state_data->pstate,
		      "Average Temperature                           : %u Celsius\n",
		      average);
    }
  else if (mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_PER_POLICY_THROTTLING_STATISTICS
	   || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_CPU_THROTTLING_STATISTICS
	   || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_MEMORY_THROTTLING_STATISTICS)
    {
      pstdout_printf (state_data->pstate,
		      "Current Throttling Percent                    : %u%%\n",
		      current);
    
      pstdout_printf (state_data->pstate,
		      "Minimum Throttling Percent                    : %u%%\n",
		      minimum);
    
      pstdout_printf (state_data->pstate,
		      "Maximum Throttling Percent                    : %u%%\n",
		      maximum);
    
      pstdout_printf (state_data->pstate,
		      "Average Throttling Percent                    : %u%%\n",
		      average);
    }
  else if (mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_RESPONSE_TIME_STATISTICS
	   || mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_COMMUNICATION_FAILURE_STATISTICS)
    {
      /* Time is in 1/100 increments, multiply by 10 to get milliseconds */

      pstdout_printf (state_data->pstate,
		      "Current Time                                  : %u ms\n",
		      current * 10);
    
      pstdout_printf (state_data->pstate,
		      "Minimum Time                                  : %u ms\n",
		      minimum * 10);
    
      pstdout_printf (state_data->pstate,
		      "Maximum Time                                  : %u ms\n",
		      maximum * 10);
    
      pstdout_printf (state_data->pstate,
		      "Average Time                                  : %u ms\n",
		      average * 10);
    } 
  else /* mode == IPMI_OEM_INTEL_NODE_MANAGER_STATISTICS_MODE_GLOBAL_HOST_UNHANDLED_REQUESTS_STATISTICS */
    {
      pstdout_printf (state_data->pstate,
		      "Current Unhandled Requests                    : %u\n",
		      current);
    
      pstdout_printf (state_data->pstate,
		      "Maximum Unhandled Requests                    : %u\n",
		      maximum);
    }

  pstdout_printf (state_data->pstate,
		  "Statistics Reporting Period                   : %u seconds\n",
		  statistics_reporting_period);
  

  pstdout_printf (state_data->pstate,
		  "Policy/Global Administrative State            : %s\n",
		  (policy_global_administrative_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_GLOBAL_ADMINISTRATIVE_STATE_ENABLED) ? "Enabled" : "Disabled");
  
  if (policyid_specified)
    pstdout_printf (state_data->pstate,
		    "Policy Operational State                      : %s\n",
		    (policy_operational_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_OPERATIONAL_STATE_ACTIVELY_MONITORING_DEFINED_TRIGGER) ? "Active" : "Suspended");

  pstdout_printf (state_data->pstate,
		  "Measurements State                            : %s\n",
		  (measurements_state == IPMI_OEM_INTEL_NODE_MANAGER_MEASUREMENTS_STATE_IN_PROGRESS) ? "In Progress" : "Suspended");

  if (policyid_specified)
    pstdout_printf (state_data->pstate,
		    "Policy Activation State                       : %s\n",
		    (policy_activation_state == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ACTIVATION_STATE_TRIGGERED_AND_ACTIVELY_LIMITING_TARGET) ? "Triggered" : "Not Triggered");
  
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
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  uint8_t policyid = 0;
  int mode_specified = 0;
  int domainid_specified = 0;
  int policyid_specified = 0;
  int rv = -1;

  assert (state_data);

  if (state_data->prog_data->args->oem_options_count)
    {
      unsigned int i;
      
      for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
        {
          char *key = NULL;
          char *value = NULL;

          if (ipmi_oem_parse_key_value (state_data,
                                        i,
                                        &key,
                                        &value) < 0)
            goto cleanup;

	  if (!strcasecmp (key, "mode"))
	    {
	      if (strcasecmp (value, "global")
		  && strcasecmp (value, "policy")
		  && strcasecmp (value, "hostunhandledrequests")
		  && strcasecmp (value, "hostresponsetime")
		  && strcasecmp (value, "cputhrottling")
		  && strcasecmp (value, "memorythrottling")
		  && strcasecmp (value, "hostcommfailure"))
		{
		  pstdout_fprintf (state_data->pstate,
				   stderr,
				   "%s:%s invalid OEM option argument '%s' : invalid mode\n",
				   state_data->prog_data->args->oem_id,
				   state_data->prog_data->args->oem_command,
				   state_data->prog_data->args->oem_options[i]);
		  goto cleanup;
		}

	      if (!strcasecmp (value, "global"))
		mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_RESET_GLOBAL_STATISTICS;
	      else if (!strcasecmp (value, "policy"))
		mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_PER_POLICY_STATISTICS;
	      else if (!strcasecmp (value, "hostunhandledrequests"))
		mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_GLOBAL_HOST_UNHANDLED_REQUESTS_STATISTICS;
	      else if (!strcasecmp (value, "hostresponsetime"))
		mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_HOST_RESPONSE_TIME_STATISTICS;
	      else if (!strcasecmp (value, "cputhrottling"))
		mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_CPU_THROTTLING_STATISTICS;
	      else if (!strcasecmp (value, "memorythrottling"))
		mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_MEMORY_THROTTLING_STATISTICS;
	      else /* !strcasecmp (value, "hostcommfailure") */ 
		mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_HOST_COMMUNICATION_FAILURE_STATISTICS;

	      mode_specified++;
	    }
          else if (!strcasecmp (key, "domainid"))
            {
              if (_ipmi_oem_intelnm_parse_domainid (state_data,
						    i,
						    value,
						    &domainid) < 0)
                goto cleanup;
	      
	      domainid_specified++;
            }
          else if (!strcasecmp (key, "policyid"))
            {
              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &policyid) < 0)
                goto cleanup;

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

  if (!mode_specified)
    {
      if (!policyid_specified)
	mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_RESET_GLOBAL_STATISTICS;
      else
	mode = IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_PER_POLICY_STATISTICS;
    }
  else
    {
      if ((mode == IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_CPU_THROTTLING_STATISTICS
	   || mode == IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_MEMORY_THROTTLING_STATISTICS
	   || mode == IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_HOST_COMMUNICATION_FAILURE_STATISTICS)
	  && domainid != IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s domainid must be \"platform\" (or \"%u\") for indicated reset mode\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command,
			   IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM);
	  goto cleanup;
	}

      if (mode == IPMI_OEM_INTEL_NODE_MANAGER_RESET_MODE_PER_POLICY_STATISTICS
	  && !policyid_specified)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s policyid must be specified for indicated reset mode\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command);
	  goto cleanup;
	}
    }
      
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
_ipmi_oem_intelnm_get_node_manager_version_common (ipmi_oem_state_data_t *state_data,
						   uint8_t target_channel_number,
						   uint8_t target_slave_address,
						   uint8_t target_lun,
						   uint8_t *node_manager_version,
						   uint8_t *ipmi_interface_version,
						   uint8_t *patch_version,
						   uint8_t *major_firmware_revision,
						   uint8_t *minor_firmware_revision)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (state_data);

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

  if (node_manager_version)
    {
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
      (*node_manager_version) = val;
    }

  if (ipmi_interface_version)
    {
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
      (*ipmi_interface_version) = val;
    }

  if (patch_version)
    {
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
      (*patch_version) = val;
    }
     
  if (major_firmware_revision)
    {
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
      (*major_firmware_revision) = val;
    }

  if (minor_firmware_revision)
    {
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
      (*minor_firmware_revision) = val;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static void
_ipmi_oem_intelnm_get_domain_id_str (ipmi_oem_state_data_t *state_data,
				     uint8_t domain_id,
				     char *domain_id_str,
				     unsigned int domain_id_str_len)
{
  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));
  assert (domain_id_str);
  assert (domain_id_str_len);

  switch (domain_id)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM:
      snprintf (domain_id_str,
		domain_id_str_len,
		"Entire Platform");
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM:
      snprintf (domain_id_str,
		domain_id_str_len,
		"CPU Subsystem");
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MEMORY_SUBSYSTEM:
      snprintf (domain_id_str,
		domain_id_str_len,
		"Memory Subsystem");
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_HIGH_POWER_IO_SUBSYSTEM:
      snprintf (domain_id_str,
		domain_id_str_len,
		"High Power I/O Subsystem");
      break;
    }
}

static char *
_ipmi_oem_intelnm_get_policy_trigger_type_str (uint8_t policy_trigger_type)
{
  char *rv = NULL;

  switch (policy_trigger_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER:
      rv = "No Policy Trigger";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER:
      rv = "Inlet Temperature Limit Policy Trigger";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT:
      rv = "Missing Power Reading Timeout";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER:
      rv = "Time After Platform Reset Trigger";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY:
      rv = "Boot Time Policy";
      break;
    }

  return (rv);
}

static void
_ipmi_oem_intelnm_get_node_manager_get_node_manager_capabilities_header (ipmi_oem_state_data_t *state_data,
									 uint8_t domain_id,
									 uint8_t policy_trigger_type,
									 uint8_t policy_type)
{
  char domain_id_str[IPMI_OEM_INTELNM_STRING_MAX + 1];
  char *policy_trigger_type_str = NULL;
  char *policy_type_str = NULL;

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));
  assert (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_VALID (policy_trigger_type));
  assert (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_VALID (policy_type));

  memset (domain_id_str, '\0', IPMI_OEM_INTELNM_STRING_MAX + 1);
  _ipmi_oem_intelnm_get_domain_id_str (state_data,
				       domain_id,
				       domain_id_str,
				       IPMI_OEM_INTELNM_STRING_MAX);

  policy_trigger_type_str = _ipmi_oem_intelnm_get_policy_trigger_type_str (policy_trigger_type);

  switch (policy_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY:
      policy_type_str = "Power Control Policy";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "Capabilities for Domain ID = %s, Policy Trigger Type = %s, Policy Type = %s\n\n",
		  domain_id_str,
		  policy_trigger_type_str,
		  policy_type_str);
}

/* return 1 if output something, 0 if not, -1 on error */
static int
_ipmi_oem_intelnm_get_node_manager_capabilities_common (ipmi_oem_state_data_t *state_data,
							uint8_t target_channel_number,
							uint8_t target_slave_address,
							uint8_t target_lun,
							uint8_t domain_id,
							uint8_t policy_trigger_type,
							uint8_t policy_type,
							int searching_domain_id,
							int searching_policy_trigger_type,
							int searching_policy_type)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t max_concurrent_settings;
  uint16_t max_power_thermal_time_after_reset;
  uint16_t min_power_thermal_time_after_reset;
  uint32_t min_correction_time;
  uint32_t max_correction_time;
  uint16_t min_statistics_reporting_period;
  uint16_t max_statistics_reporting_period;
  uint8_t limiting_domain_id;
  char limiting_domain_id_str[IPMI_OEM_INTELNM_STRING_MAX + 1];
  uint8_t limiting_based_on;
  char *limiting_based_on_str;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));
  assert (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_VALID (policy_trigger_type));
  assert (IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_VALID (policy_type));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_capabilities_rs)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

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
	  
	  if (!state_data->prog_data->args->verbose_count)
            {
              if ((searching_domain_id
		   && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		  || (searching_policy_trigger_type
		      && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_POLICY_TRIGGER_TYPE) == 1)
		  || (searching_policy_type
		      && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_UNKNOWN_POLICY_TYPE) == 1))
		{
		  rv = 0;
		  goto cleanup;
		}
            }

          if (searching_domain_id
              || searching_policy_trigger_type
	      || searching_policy_type)
            _ipmi_oem_intelnm_get_node_manager_get_node_manager_capabilities_header (state_data,
										     domain_id,
										     policy_trigger_type,
										     policy_type);

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

      if ((searching_domain_id
	   || searching_policy_trigger_type
	   || searching_policy_type)
          && state_data->prog_data->args->verbose_count)
        rv = 1;

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
		    "max_power_thermal_time_after_reset",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'max_power_thermal_time_after_reset': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  max_power_thermal_time_after_reset = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "min_power_thermal_time_after_reset",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'min_power_thermal_time_after_reset': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  min_power_thermal_time_after_reset = val;

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
		    "domain_limiting_scope.domain_id",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'domain_limiting_scope.domain_id': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  limiting_domain_id = val;

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

  memset (limiting_domain_id_str, '\0', IPMI_OEM_INTELNM_STRING_MAX + 1);
  if (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (limiting_domain_id))
    _ipmi_oem_intelnm_get_domain_id_str (state_data,
					 limiting_domain_id,
					 limiting_domain_id_str,
					 IPMI_OEM_INTELNM_STRING_MAX);
  else
    snprintf (limiting_domain_id_str,
	      IPMI_OEM_INTELNM_STRING_MAX,
	      "Unspecified");

  switch (limiting_based_on)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_BASED_ON_WALL_INPUT_POWER_PSU_INPUT_POWER:
      limiting_based_on_str = "Wall input power / PSU input power";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_LIMITING_BASED_ON_DC_POWER_PSU_OUTPUT_POWER_OR_BLADED_SYSTEM:
      limiting_based_on_str = "DC power - PSU output power or bladed system";
      break;
    }

  if (searching_domain_id
      || searching_policy_trigger_type
      || searching_policy_type)
    _ipmi_oem_intelnm_get_node_manager_get_node_manager_capabilities_header (state_data,
									     domain_id,
									     policy_trigger_type,
									     policy_type);

  pstdout_printf (state_data->pstate,
		  "Max Concurrent Settings         : %u\n",
		  max_concurrent_settings);

  if (policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER)
    {
      char thermal_str[IPMI_OEM_INTELNM_STRING_MAX + 1];
      
      memset (thermal_str, '\0', IPMI_OEM_INTELNM_STRING_MAX + 1);
      if (!max_power_thermal_time_after_reset)
	snprintf (thermal_str,
		  IPMI_OEM_INTELNM_STRING_MAX,
		  "No Limit");
      else
	snprintf (thermal_str,
		  IPMI_OEM_INTELNM_STRING_MAX,
		  "%u W",
		  max_power_thermal_time_after_reset);

      pstdout_printf (state_data->pstate,
		      "Max Power                       : %s\n",
		      thermal_str);
      
      if (!min_power_thermal_time_after_reset)
	snprintf (thermal_str,
		  IPMI_OEM_INTELNM_STRING_MAX,
		  "No Limit");
      else
	snprintf (thermal_str,
		  IPMI_OEM_INTELNM_STRING_MAX,
		  "%u W",
		  min_power_thermal_time_after_reset);

      pstdout_printf (state_data->pstate,
		      "Min Power                       : %s\n",
		      thermal_str);
    }
  else if (policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER)
    {
      pstdout_printf (state_data->pstate,
		      "Max Thermal                     : %u C\n",
		      max_power_thermal_time_after_reset);
      
      pstdout_printf (state_data->pstate,
		      "Min Thermal                     : %u C\n",
		      min_power_thermal_time_after_reset);
    }
  else if (policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT
	   || policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER)
    {
      pstdout_printf (state_data->pstate,
		      "Max Trigger Time                : %0.1f s\n",
		      ((float)max_power_thermal_time_after_reset / 10));
      
      pstdout_printf (state_data->pstate,
		      "Min Trigger Time                : %0.1f s\n",
		      ((float)min_power_thermal_time_after_reset / 10));
    }
  /* policy_trigger_type == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY not applicable */
  
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
		  "Limiting Domain                 : %s\n",
		  limiting_domain_id_str);

  pstdout_printf (state_data->pstate,
		  "Limiting Source                 : %s\n",
		  limiting_based_on_str);

  rv = 1;
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
  uint8_t domainid_defaults_1_5[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM};
  unsigned int domainid_defaults_1_5_len = 1;
  uint8_t domainid_defaults_2_0[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM,
				     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM,
				     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MEMORY_SUBSYSTEM,
				     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_HIGH_POWER_IO_SUBSYSTEM};
  unsigned int domainid_defaults_2_0_len = 4;
  uint8_t policytrigger_defaults_1_5[] = {IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER,
					  IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER};
  unsigned int policytrigger_defaults_1_5_len = 2;
  uint8_t policytrigger_defaults_2_0[] = {IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER,
					  IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER,
					  IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT,
					  IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER,
					  IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY};
  unsigned int policytrigger_defaults_2_0_len = 5;
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
  int domainid_specified = 0;
  int policytrigger_specified = 0;
  int policytype_specified = 0;
  unsigned int i, j, k;
  int output_newline_flag = 0;
  uint8_t node_manager_version;
  int rv = -1;
  int ret;

  assert (state_data);
  
  policytype_array = policytype_defaults;
  policytype_array_len = policytype_defaults_len;

  if (state_data->prog_data->args->oem_options_count)
    {
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

              if (_ipmi_oem_intelnm_parse_domainid (state_data,
						    i,
						    value,
						    &domainid_tmp) < 0)
                goto cleanup;

	      domainid_input[0] = domainid_tmp;

	      domainid_array = domainid_input;
	      domainid_array_len = 1;
	      domainid_specified++;
            }
          else if (!strcasecmp (key, "policytrigger"))
            {
	      if (strcasecmp (value, "none")
		  && strcasecmp (value, "inlettemperaturelimitpolicytrigger")
		  && strcasecmp (value, "inlet") /* legacy */
		  && strcasecmp (value, "missingpowerreadingtimeout")
		  && strcasecmp (value, "timeafterplatformresettrigger")
		  && strcasecmp (value, "boottimepolicy"))
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
	      else if (!strcasecmp (value, "inlettemperaturelimitpolicytrigger")
		       || !strcasecmp (value, "inlet")) /* legacy */
		policytrigger_input[0] = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER;
	      else if (!strcasecmp (value, "missingpowerreadingtimeout"))
		policytrigger_input[0] = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT;
	      else if (!strcasecmp (value, "timeafterplatformresettrigger"))
		policytrigger_input[0] = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER;
	      else /* !strcasecmp (value, "boottimepolicy") */
		policytrigger_input[0] = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY;

	      policytrigger_array = policytrigger_input;
	      policytrigger_array_len = 1;
	      policytrigger_specified++;
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
	      policytype_specified++;
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

  if (_ipmi_oem_intelnm_get_node_manager_version_common (state_data,
							 target_channel_number,
							 target_slave_address,
							 target_lun,
							 &node_manager_version,
							 NULL,
							 NULL,
							 NULL,
							 NULL) < 0)
    goto cleanup;

  if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_0
      || node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_5)
    {
      if (!domainid_specified)
	{
	  domainid_array = domainid_defaults_2_0;
	  domainid_array_len = domainid_defaults_2_0_len;
	}
      if (!policytrigger_specified)
	{
	  policytrigger_array = policytrigger_defaults_2_0;
	  policytrigger_array_len = policytrigger_defaults_2_0_len;
	}
    }
  else
    {
      if (!domainid_specified)
	{
	  domainid_array = domainid_defaults_1_5;
	  domainid_array_len = domainid_defaults_1_5_len;
	}
      if (!policytrigger_specified)
	{
	  policytrigger_array = policytrigger_defaults_1_5;
	  policytrigger_array_len = policytrigger_defaults_1_5_len;
	}
    }

  for (i = 0; i < domainid_array_len; i++)
    {
      for (j = 0; j < policytrigger_array_len; j++)
	{
	  for (k = 0; k < policytype_array_len; k++)
	    {
	      if (output_newline_flag)
		pstdout_printf (state_data->pstate, "\n");

	      if ((ret = _ipmi_oem_intelnm_get_node_manager_capabilities_common (state_data,
									         target_channel_number,
									         target_slave_address,
									         target_lun,
									         domainid_array[i],
									         policytrigger_array[j],
									         policytype_array[k],
									         domainid_specified ? 0 : 1,
									         policytrigger_specified ? 0 : 1,
									         policytype_specified ? 0 : 1)) < 0)
		goto cleanup;

	      if (ret > 0)
	        output_newline_flag = 1;
	      else
	        output_newline_flag = 0;
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
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
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
      unsigned int i;
      
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
              if (_ipmi_oem_intelnm_parse_domainid (state_data,
						    i,
						    value,
						    &domainid) < 0)
                goto cleanup;
              
	      domainid_specified++;
            }
          else if (!strcasecmp (key, "policyid"))
            {
              if (ipmi_oem_parse_1_byte_field (state_data,
                                               i,
                                               value,
                                               &policyid) < 0)
                goto cleanup;

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
  char domain_id_str[IPMI_OEM_INTELNM_STRING_MAX + 1];

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));

  memset (domain_id_str, '\0', IPMI_OEM_INTELNM_STRING_MAX + 1);
  _ipmi_oem_intelnm_get_domain_id_str (state_data,
				       domain_id,
				       domain_id_str,
				       IPMI_OEM_INTELNM_STRING_MAX);

  pstdout_printf (state_data->pstate,
		  "Capabilities for Domain ID = %s, Policy ID = %u\n\n",
		  domain_id_str,
		  policy_id);
}

/* return 1 if output something, 0 if not, -1 on error */
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
  uint8_t policy_created_and_managed_by_other_management;
  uint8_t policy_trigger_type;
  uint8_t policy_type;
  uint8_t aggressive_cpu_power_correction;
  uint8_t policy_storage_option;
  uint8_t policy_exception_actions_send_alert;
  uint8_t policy_exception_actions_shutdown_system;
  uint16_t policy_target_limit;
  uint32_t correction_time_limit;
  uint16_t policy_trigger_limit;
  uint16_t statistics_reporting_period;
  char *policy_trigger_type_str = NULL;
  char *policy_target_limit_units_str = NULL;
  char *policy_type_str = NULL;
  char *aggressive_cpu_power_correction_str = NULL;
  char *policy_storage_option_str = NULL;
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
	      if ((searching_domain_id
		  && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		  || (searching_policy_id
		      && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID) == 1))
		{
		  rv = 0;
		  goto cleanup;
		}
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
	rv = 1; 

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
		    "policy_created_and_managed_by_other_management",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_created_and_managed_by_other_management': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_created_and_managed_by_other_management = val;

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
		    "aggressive_cpu_power_correction",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'aggressive_cpu_power_correction': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  aggressive_cpu_power_correction = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_storage_option",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_storage_option': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_storage_option = val;

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
		    "policy_target_limit",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_target_limit': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_target_limit = val;

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

  policy_trigger_type_str = _ipmi_oem_intelnm_get_policy_trigger_type_str (policy_trigger_type);
  if (!policy_trigger_type_str)
    policy_trigger_type_str = "Unknown";

  switch (policy_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TYPE_POWER_CONTROL_POLICY:
      policy_type_str = "Power Control Policy";
      break;
    default:
      policy_type_str = "Unknown";
      break;
    }

  switch (aggressive_cpu_power_correction)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_AUTOMATIC:
      aggressive_cpu_power_correction_str = "Automatic";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_NON_AGGRESSIVE:
      aggressive_cpu_power_correction_str = "Non-Aggressive";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_AGGRESSIVE:
      aggressive_cpu_power_correction_str = "Aggressive";
      break;
    default:
      aggressive_cpu_power_correction_str = "Unknown";
      break;
    }

  switch (policy_storage_option)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_STORAGE_PERSISTENT_STORAGE:
      policy_storage_option_str = "Persistent Storage";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_STORAGE_VOLATILE_MEMORY:
      policy_storage_option_str = "Volatile Storage";
      break;
    default:
      policy_storage_option_str = "Unknown";
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
		  "Policy Created/Managed by Other Client  : %s\n",
		  (policy_created_and_managed_by_other_management == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CREATED_AND_MANAGED_BY_OTHER_MANAGEMENT) ? "Yes" : "No");

  pstdout_printf (state_data->pstate,
		  "Policy Trigger Type                     : %s\n",
		  policy_trigger_type_str);

  pstdout_printf (state_data->pstate,
		  "Policy Type                             : %s\n",
		  policy_type_str);

  if (domain_id == IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM
      || domain_id == IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM)
    pstdout_printf (state_data->pstate,
		    "Aggressive CPU Power Correction         : %s\n",
		    aggressive_cpu_power_correction_str);

  pstdout_printf (state_data->pstate,
		  "Policy Storage Option:                  : %s\n",
		  policy_storage_option_str);
  
  pstdout_printf (state_data->pstate,
		  "Policy Exception Send Alert Action      : %s\n",
		  (policy_exception_actions_send_alert == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE) ? "enabled" : "disabled");

  pstdout_printf (state_data->pstate,
		  "Policy Exception Shutdown System Action : %s\n",
		  (policy_exception_actions_shutdown_system == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_ENABLE) ? "enabled" : "disabled");
		  
  switch (policy_trigger_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER:
      policy_target_limit_units_str = " W";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER:
      policy_target_limit_units_str = " W";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT:
      policy_target_limit_units_str = "%%";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER:
      policy_target_limit_units_str = " W";
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY:
      policy_target_limit_units_str = " W";
      break;
    default:
      policy_target_limit_units_str = " ?";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "Policy Target Limit                     : %u%s\n",
		  policy_target_limit,
		  policy_target_limit_units_str);

  pstdout_printf (state_data->pstate,
		  "Correction Time Limit                   : %u ms\n",
		  correction_time_limit);

  switch (policy_trigger_type)
    {
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_NO_POLICY_TRIGGER:
      pstdout_printf (state_data->pstate,
		      "Policy Trigger Limit                    : %u W\n",
		      policy_trigger_limit);
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER:
      pstdout_printf (state_data->pstate,
		      "Policy Trigger Limit                    : %u C\n",
		      policy_trigger_limit);
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT:
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER:
      pstdout_printf (state_data->pstate,
		      "Policy Trigger Limit                    : %0.1f s\n",
		      ((float)policy_trigger_limit / 10));
      break;
    case IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY:
    default:
      break;
    }
  
  pstdout_printf (state_data->pstate,
		  "Statistics Reporting Period             : %u s\n",
		  statistics_reporting_period);

  rv = 1;
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
  uint8_t domainid_defaults_1_5[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM};
  unsigned int domainid_defaults_1_5_len = 1;
  uint8_t domainid_defaults_2_0[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM,
				     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM,
				     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MEMORY_SUBSYSTEM,
				     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_HIGH_POWER_IO_SUBSYSTEM};
  unsigned int domainid_defaults_2_0_len = 4;
  uint8_t domainid_input[1];
  uint8_t *domainid_array;
  unsigned int domainid_array_len;
  uint8_t policyid_min = 0;
  uint8_t policyid_max = 255;
  int domainid_specified = 0;
  int policyid_specified = 0;
  unsigned int i, j;
  int output_newline_flag = 0;
  uint8_t node_manager_version;
  int rv = -1;
  int ret;

  assert (state_data);
  
  if (state_data->prog_data->args->oem_options_count)
    {
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

              if (_ipmi_oem_intelnm_parse_domainid (state_data,
						    i,
						    value,
						    &domainid_tmp) < 0)
                goto cleanup;

	      domainid_input[0] = domainid_tmp;

	      domainid_array = domainid_input;
	      domainid_array_len = 1;
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


  if (!domainid_specified)
    {
      if (_ipmi_oem_intelnm_get_node_manager_version_common (state_data,
							     target_channel_number,
							     target_slave_address,
							     target_lun,
							     &node_manager_version,
							     NULL,
							     NULL,
							     NULL,
							     NULL) < 0)
	goto cleanup;
      
      if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_0
	  || node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_5)
	{
	  domainid_array = domainid_defaults_2_0;
	  domainid_array_len = domainid_defaults_2_0_len;
	}
      else
	{
	  domainid_array = domainid_defaults_1_5;
	  domainid_array_len = domainid_defaults_1_5_len;
	}
    }

  for (i = 0; i < domainid_array_len; i++)
    {
      for (j = policyid_min; j <= policyid_max; j++)
	{
	  if (output_newline_flag)
	    pstdout_printf (state_data->pstate, "\n");

	  if ((ret = _ipmi_oem_intelnm_get_node_manager_policy_common (state_data,
								       target_channel_number,
								       target_slave_address,
								       target_lun,
								       domainid_array[i],
								       j,
								       domainid_specified ? 0 : 1,
								       policyid_specified ? 0 : 1)) < 0)
	    goto cleanup;
	  
	  if (ret > 0)
	    output_newline_flag = 1;
	  else
	    output_newline_flag = 0;
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
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  int domainid_specified = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  uint8_t policytrigger = 0;
  int policytrigger_specified = 0;
  uint16_t policytargetlimit = 0;
  int policytargetlimit_specified = 0;
  uint8_t platformbootingmode = 0;
  int platformbootingmode_specified = 0;
  uint32_t correctiontimelimit = 0;
  int correctiontimelimit_specified = 0;
  uint16_t policytriggerlimit = 0;
  int policytriggerlimit_specified = 0;
  uint16_t statisticsreportingperiod = 0;
  int statisticsreportingperiod_specified = 0;
  uint8_t policystate = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_ENABLED;
  uint8_t aggressivepowercorrection = IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_AUTOMATIC;
  int aggressivepowercorrection_specified = 0;
  uint8_t policystorage = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_STORAGE_PERSISTENT_STORAGE;
  int policystorage_specified = 0;
  uint8_t policyexceptionaction_alert = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE;
  uint8_t policyexceptionaction_shutdown = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE;
  int rv = -1;
  unsigned int i;

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
	  if (_ipmi_oem_intelnm_parse_domainid (state_data,
						i,
						value,
						&domainid) < 0)
	    goto cleanup;
	  
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid) < 0)
	    goto cleanup;
	  
	  policyid_specified++;
	}
      else if (!strcasecmp (key, "policytrigger"))
	{
	  if (strcasecmp (value, "none")
	      && strcasecmp (value, "inlettemperaturelimitpolicytrigger")
	      && strcasecmp (value, "inlet") /* legacy */
	      && strcasecmp (value, "missingpowerreadingtimeout")
	      && strcasecmp (value, "timeafterplatformresettrigger")
	      && strcasecmp (value, "boottimepolicy"))
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
	  else if (!strcasecmp (value, "inlettemperaturelimitpolicytrigger")
		   || !strcasecmp (value, "inlet")) /* legacy */
	    policytrigger = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER;
	  else if (!strcasecmp (value, "missingpowerreadingtimeout"))
	    policytrigger = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT;
	  else if (!strcasecmp (value, "timeafterplatformresettrigger"))
	    policytrigger = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER;
	  else /* !strcasecmp (value, "boottimepolicy") */
	    policytrigger = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY;
	  policytrigger_specified++;
	}
      else if (!strcasecmp (key, "policytargetlimit")
	       || !strcasecmp (key, "powerlimit")) /* legacy */
	{
	  if (ipmi_oem_parse_2_byte_field (state_data, i, value, &policytargetlimit) < 0)
	    goto cleanup;
	  
	  policytargetlimit_specified++;
	}
      else if (!strcasecmp (key, "platformbootingmode"))
	{
	  if (strcasecmp (value, "performance")
	      && strcasecmp (value, "power"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid platformbootingmode\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "performance"))
	    platformbootingmode = IPMI_OEM_INTEL_NODE_MANAGER_PLATFORM_BOOTING_MODE_BOOT_IN_PERFORMANCE_OPTIMIZED_MODE;
	  else /* !strcasecmp (value, "power") */
	    platformbootingmode = IPMI_OEM_INTEL_NODE_MANAGER_PLATFORM_BOOTING_MODE_BOOT_IN_POWER_OPTIMIZED_MODE;
	  platformbootingmode_specified++;
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
      else if (!strcasecmp (key, "policystorage"))
	{
	  if (strcasecmp (value, "persistent")
	      && strcasecmp (value, "volatile"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid policy storage\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "persistent"))
	    policystorage = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_STORAGE_PERSISTENT_STORAGE;
	  else
	    policystorage = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_STORAGE_VOLATILE_MEMORY;

	  policystorage_specified++;
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
      else if (!strcasecmp (key, "aggressivepowercorrection"))
	{
	  if (strcasecmp (value, "automatic")
	      && strcasecmp (value, "notaggressive")
	      && strcasecmp (value, "aggressive"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid aggressive power correction\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }

	  if (!strcasecmp (value, "automatic"))
	    aggressivepowercorrection = IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_AUTOMATIC;
	  else if (!strcasecmp (value, "notaggressive"))
	    aggressivepowercorrection = IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_NON_AGGRESSIVE;
	  else /* !strcasecmp (value, "aggressive") */
	    aggressivepowercorrection = IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_AGGRESSIVE;

	  aggressivepowercorrection_specified++;
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

  if (!policytargetlimit_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy target limit must be specified\n");
      goto cleanup;
    }

  if (!correctiontimelimit_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "correction time limit must be specified\n");
      goto cleanup;
    }

  if (!statisticsreportingperiod_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "statistics reporting period must be specified\n");
      goto cleanup;
    }

  if (!policystorage_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy storage must be specified\n");
      goto cleanup;
    }

  if ((policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_INLET_TEMPERATURE_LIMIT_POLICY_TRIGGER
       || policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT
       || policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER)
      && !policytriggerlimit_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "policy trigger limit must be specified for given policy trigger type\n");
      goto cleanup;
    }

  if ((aggressivepowercorrection == IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_NON_AGGRESSIVE
       || aggressivepowercorrection == IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_AGGRESSIVE)
      && (domainid != IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM
	  && domainid != IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "aggressive power correction cannot be set with given mode\n");
      goto cleanup;
    }

  if (policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT)
    {
      if (policytargetlimit > 100)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "policy target limit out of range\n");
	  goto cleanup;
	} 
    }

  if (policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_MISSING_POWER_READING_TIMEOUT
      || policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_TIME_AFTER_PLATFORM_RESET_TRIGGER)
    {
      /* User specifies in seconds, but we need to convert to 1/10th of seconds */
      
      if (policytriggerlimit >= (USHRT_MAX / 10))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "policy trigger limit out of range\n");
	  goto cleanup;
	} 

      policytriggerlimit *= 10;
    }

  if (policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY)
    {
      if (!platformbootingmode_specified)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "platform booting mode must be specified for given policy trigger type\n");
	  goto cleanup;
	}
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

  if (policytrigger == IPMI_OEM_INTEL_NODE_MANAGER_POLICY_TRIGGER_TYPE_BOOT_TIME_POLICY)
    {
      uint16_t boot_time_policy = 0;

      boot_time_policy |= policytargetlimit;
      boot_time_policy <<= 1;
      boot_time_policy |= platformbootingmode;

      if (ipmi_cmd_oem_intel_node_manager_set_node_manager_policy (state_data->ipmi_ctx,
								   target_channel_number,
								   target_slave_address,
								   target_lun,
								   domainid,
								   policystate,
								   policyid,
								   policytrigger,
								   IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_ADD_POWER_POLICY,
								   aggressivepowercorrection,
								   policystorage,
								   policyexceptionaction_alert,
								   policyexceptionaction_shutdown,
								   boot_time_policy,
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
		goto efallthrough1;
	    }
	  else
	    {
	    efallthrough1:
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_cmd_oem_intel_node_manager_set_node_manager_policy_boot_time_policy: %s\n",
			       ipmi_ctx_errormsg (state_data->ipmi_ctx));
	    }
	  goto cleanup;
	}
    }
  else
    {
      if (ipmi_cmd_oem_intel_node_manager_set_node_manager_policy (state_data->ipmi_ctx,
								   target_channel_number,
								   target_slave_address,
								   target_lun,
								   domainid,
								   policystate,
								   policyid,
								   policytrigger,
								   IPMI_OEM_INTEL_NODE_MANAGER_POLICY_CONFIGURATION_ACTION_ADD_POWER_POLICY,
								   aggressivepowercorrection,
								   policystorage,
								   policyexceptionaction_alert,
								   policyexceptionaction_shutdown,
								   policytargetlimit,
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
		goto efallthrough2;
	    }
	  else
	    {
	    efallthrough2:
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_cmd_oem_intel_node_manager_set_node_manager_policy: %s\n",
			       ipmi_ctx_errormsg (state_data->ipmi_ctx));
	    }
	  goto cleanup;
	}
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
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  int domainid_specified = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  int rv = -1;
  unsigned int i;

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
	  if (_ipmi_oem_intelnm_parse_domainid (state_data,
						i,
						value,
						&domainid) < 0)
	    goto cleanup;
	  
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid) < 0)
	    goto cleanup;
	  
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
							       /* all remaining bytes ignored, fill in with default-ish values */
							       IPMI_OEM_INTEL_NODE_MANAGER_AGGRESSIVE_CPU_POWER_CORRECTION_AUTOMATIC,
							       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_STORAGE_PERSISTENT_STORAGE,
							       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE,
							       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_EXCEPTION_ACTION_DISABLE,
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
  char domain_id_str[IPMI_OEM_INTELNM_STRING_MAX + 1];

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));

  memset (domain_id_str, '\0', IPMI_OEM_INTELNM_STRING_MAX + 1);
  _ipmi_oem_intelnm_get_domain_id_str (state_data,
				       domain_id,
				       domain_id_str,
				       IPMI_OEM_INTELNM_STRING_MAX);
  
  pstdout_printf (state_data->pstate,
		  "Thresholds for Domain ID = %s, Policy ID = %u\n\n",
		  domain_id_str,
		  policy_id);
}

/* return 1 if output something, 0 if not, -1 on error */
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
	      if ((searching_domain_id
		  && ipmi_check_completion_code (alert_threshold_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		  || (searching_policy_id
		      && ipmi_check_completion_code (alert_threshold_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID) == 1))
		{
		  rv = 0;
		  goto cleanup;
		}
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
	rv = 1; 
      
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
  
  /* just in case */
  if (number_of_alert_thresholds > IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLDS_MAX)
    number_of_alert_thresholds = IPMI_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLDS_MAX;

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
		  if ((searching_domain_id
		      && ipmi_check_completion_code (policy_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		      || (searching_policy_id
			  && ipmi_check_completion_code (policy_obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID) == 1))
		    {
		      rv = 0;
		      goto cleanup;
		    }
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
	    rv = 1; 
	  
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

  rv = 1;
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
  uint8_t domainid_defaults_1_5[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM};
  unsigned int domainid_defaults_1_5_len = 1;
  uint8_t domainid_defaults_2_0[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM,
                                     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM,
                                     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MEMORY_SUBSYSTEM,
                                     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_HIGH_POWER_IO_SUBSYSTEM};
  unsigned int domainid_defaults_2_0_len = 4;
  uint8_t domainid_input[1];
  uint8_t *domainid_array;
  unsigned int domainid_array_len;
  uint8_t policyid_min = 0;
  uint8_t policyid_max = 255;
  int domainid_specified = 0;
  int policyid_specified = 0;
  unsigned int i, j;
  int output_newline_flag = 0;
  uint8_t node_manager_version;
  int rv = -1;
  int ret;

  assert (state_data);
  
  if (state_data->prog_data->args->oem_options_count)
    {
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

              if (_ipmi_oem_intelnm_parse_domainid (state_data,
						    i,
						    value,
						    &domainid_tmp) < 0)
                goto cleanup;

	      domainid_input[0] = domainid_tmp;

              domainid_array = domainid_input;
              domainid_array_len = 1;
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

  if (!domainid_specified)
    {
      if (_ipmi_oem_intelnm_get_node_manager_version_common (state_data,
							     target_channel_number,
							     target_slave_address,
							     target_lun,
							     &node_manager_version,
							     NULL,
							     NULL,
							     NULL,
							     NULL) < 0)
	goto cleanup;
      
      if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_0
	  || node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_5)
	{
	  domainid_array = domainid_defaults_2_0;
	  domainid_array_len = domainid_defaults_2_0_len;
	}
      else
	{
	  domainid_array = domainid_defaults_1_5;
	  domainid_array_len = domainid_defaults_1_5_len;
	}
    }

  for (i = 0; i < domainid_array_len; i++)
    {
      for (j = policyid_min; j <= policyid_max; j++)
	{
	  if (output_newline_flag)
	    pstdout_printf (state_data->pstate, "\n");
	  
	  if (_ipmi_oem_intelnm_get_node_manager_alert_thresholds_common (state_data,
									  target_channel_number,
									  target_slave_address,
									  target_lun,
									  domainid_array[i],
									  j,
									  domainid_specified ? 0 : 1,
									  policyid_specified ? 0 : 1) < 0)
	    goto cleanup;
	  
	  if (ret > 0)
	    output_newline_flag = 1;
	  else
	    output_newline_flag = 0;
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
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
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
  unsigned int i;

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
	  if (_ipmi_oem_intelnm_parse_domainid (state_data,
						i,
						value,
						&domainid) < 0)
	    goto cleanup;
	  
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid) < 0)
	    goto cleanup;

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
			   "ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_thresholds: %s\n",
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
_ipmi_oem_intelnm_get_node_manager_policy_suspend_periods_output_header (ipmi_oem_state_data_t *state_data,
									 uint8_t domain_id,
									 uint8_t policy_id)
{
  char domain_id_str[IPMI_OEM_INTELNM_STRING_MAX + 1];

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));

  memset (domain_id_str, '\0', IPMI_OEM_INTELNM_STRING_MAX + 1);
  _ipmi_oem_intelnm_get_domain_id_str (state_data,
				       domain_id,
				       domain_id_str,
				       IPMI_OEM_INTELNM_STRING_MAX);
  
  pstdout_printf (state_data->pstate,
		  "Suspend Periods for Domain ID = %s, Policy ID = %u\n\n",
		  domain_id_str,
		  policy_id);
}

/* return 1 if output something, 0 if not, -1 on error */
static int
_ipmi_oem_intelnm_get_node_manager_policy_suspend_periods_common (ipmi_oem_state_data_t *state_data,
								  uint8_t target_channel_number,
								  uint8_t target_slave_address,
								  uint8_t target_lun,
								  uint8_t domain_id,
								  uint8_t policy_id,
								  int searching_domain_id,
								  int searching_policy_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t number_of_policy_suspend_periods = 0;
  uint8_t policy_suspend_start_time[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_stop_time[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_monday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_tuesday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_wednesday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_thursday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_friday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_saturday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_sunday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint64_t val;
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_VALID (domain_id));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods_rs)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods (state_data->ipmi_ctx,
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
	      if ((searching_domain_id
		  && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_DOMAIN_ID) == 1)
		  || (searching_policy_id
		      && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OEM_INTEL_NODE_MANAGER_INVALID_POLICY_ID) == 1))
		{
		  rv = 0;
		  goto cleanup;
		}
	    }

	  if (searching_domain_id
	      || searching_policy_id)
	    _ipmi_oem_intelnm_get_node_manager_policy_suspend_periods_output_header (state_data, domain_id, policy_id);

	  if ((eret = _ipmi_oem_intelnm_bad_completion_code (state_data,
							     "Error",
							     obj_cmd_rs)) < 0)
	    goto cleanup;
	  
	  if (!eret)
	    goto efallthrough_suspend_periods;
	}
      else
	{
	efallthrough_suspend_periods:
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_oem_intel_node_manager_get_node_manager_policy_suspend_periods: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      
      if ((searching_domain_id
	   || searching_policy_id)
	  && state_data->prog_data->args->verbose_count)
	rv = 1; 
      
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs,
		    "number_of_policy_suspend_periods",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'number_of_policy_suspend_periods': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  number_of_policy_suspend_periods = val;
  
  /* just in case */
  if (number_of_policy_suspend_periods > IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX)
    number_of_policy_suspend_periods = IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX;

  if (number_of_policy_suspend_periods)
    {
      char start_time_field[IPMI_OEM_STR_BUFLEN + 1];
      char stop_time_field[IPMI_OEM_STR_BUFLEN + 1];
      char monday_field[IPMI_OEM_STR_BUFLEN + 1];
      char tuesday_field[IPMI_OEM_STR_BUFLEN + 1];
      char wednesday_field[IPMI_OEM_STR_BUFLEN + 1];
      char thursday_field[IPMI_OEM_STR_BUFLEN + 1];
      char friday_field[IPMI_OEM_STR_BUFLEN + 1];
      char saturday_field[IPMI_OEM_STR_BUFLEN + 1];
      char sunday_field[IPMI_OEM_STR_BUFLEN + 1];

      for (i = 0; i < number_of_policy_suspend_periods; i++)
	{
	  memset (start_time_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (stop_time_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (monday_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (tuesday_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (wednesday_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (thursday_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (friday_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (saturday_field, '\0', IPMI_OEM_STR_BUFLEN + 1);
	  memset (sunday_field, '\0', IPMI_OEM_STR_BUFLEN + 1);

	  snprintf (start_time_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_start_time",
		    i + 1);
	  snprintf (stop_time_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_stop_time",
		    i + 1);
	  snprintf (monday_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_period_recurrence.monday",
		    i + 1);
	  snprintf (tuesday_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_period_recurrence.tuesday",
		    i + 1);
	  snprintf (wednesday_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_period_recurrence.wednesday",
		    i + 1);
	  snprintf (thursday_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_period_recurrence.thursday",
		    i + 1);
	  snprintf (friday_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_period_recurrence.friday",
		    i + 1);
	  snprintf (saturday_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_period_recurrence.saturday",
		    i + 1);
	  snprintf (sunday_field,
		    IPMI_OEM_STR_BUFLEN,
		    "policy%d.suspend_period_recurrence.sunday",
		    i + 1);

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    start_time_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       start_time_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_start_time[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    stop_time_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       stop_time_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_stop_time[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    monday_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       monday_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_period_recurrence_monday[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    tuesday_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       tuesday_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_period_recurrence_tuesday[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    wednesday_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       wednesday_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_period_recurrence_wednesday[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    thursday_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       thursday_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_period_recurrence_thursday[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    friday_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       friday_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_period_recurrence_friday[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    saturday_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       saturday_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_period_recurrence_saturday[i] = val;

	  if (FIID_OBJ_GET (obj_cmd_rs,
			    sunday_field,
			    &val) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "FIID_OBJ_GET: '%s': %s\n",
			       sunday_field,
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }
	  policy_suspend_period_recurrence_sunday[i] = val;
	}
    }
  
  if (searching_domain_id
      || searching_policy_id)
    _ipmi_oem_intelnm_get_node_manager_policy_suspend_periods_output_header (state_data, domain_id, policy_id);
  
  pstdout_printf (state_data->pstate,
		  "Number of Policy Suspend Periods     : %u\n",
		  number_of_policy_suspend_periods);

  for (i = 0; i < IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX; i++)
    {
      if (number_of_policy_suspend_periods >= (i + 1))
	{
	  /* encoded as minutes starting from midnight divided by 6 */
	  if (!IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_START_TIME_VALID (policy_suspend_start_time[i]))
	    pstdout_printf (state_data->pstate,
			    "Policy %d Suspend Start Time          : Invalid\n");
	  else
	    pstdout_printf (state_data->pstate,
			    "Policy %d Suspend Start Time          : %02u:%02u\n",
			    i + 1,
			    (policy_suspend_start_time[i] * 6) / 60,
			    (policy_suspend_start_time[i] * 6) % 60);
	  
	  
	  if (!IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_STOP_TIME_VALID (policy_suspend_stop_time[i]))
	    pstdout_printf (state_data->pstate,
			    "Policy %d Suspend Stop Time           : Invalid\n");
	  else
	    pstdout_printf (state_data->pstate,
			    "Policy %d Suspend Stop Time           : %02u:%02u\n",
			    i + 1,
			    (policy_suspend_stop_time[i] * 6) / 60,
			    (policy_suspend_stop_time[i] * 6) % 60);

	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Monday    : %s\n",
			  i + 1,
			  policy_suspend_period_recurrence_monday[i] == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD ? "Yes" : "No");
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Tuesday   : %s\n",
			  i + 1,
			  policy_suspend_period_recurrence_tuesday[i] == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD ? "Yes" : "No");
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Wednesday : %s\n",
			  i + 1,
			  policy_suspend_period_recurrence_wednesday[i] == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD ? "Yes" : "No");
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Thursday  : %s\n",
			  i + 1,
			  policy_suspend_period_recurrence_thursday[i] == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD ? "Yes" : "No");
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Friday    : %s\n",
			  i + 1,
			  policy_suspend_period_recurrence_friday[i] == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD ? "Yes" : "No");
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Saturday  : %s\n",
			  i + 1,
			  policy_suspend_period_recurrence_saturday[i] == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD ? "Yes" : "No");
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Sunday    : %s\n",
			  i + 1,
			  policy_suspend_period_recurrence_sunday[i] == IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD ? "Yes" : "No");
	}
      else
	{
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Start Time          : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Stop Time           : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Monday    : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Tuesday   : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Wednesday : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Thursday  : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Friday    : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Saturday  : N/A\n",
			  i + 1);
	  pstdout_printf (state_data->pstate,
			  "Policy %d Suspend Repeat on Sunday    : N/A\n",
			  i + 1);
	}
    }

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv); 
}

int
ipmi_oem_intelnm_get_node_manager_policy_suspend_periods (ipmi_oem_state_data_t *state_data)
{
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid_defaults_1_5[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM};
  unsigned int domainid_defaults_1_5_len = 1;
  uint8_t domainid_defaults_2_0[] = {IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM,
                                     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM,
                                     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MEMORY_SUBSYSTEM,
                                     IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_HIGH_POWER_IO_SUBSYSTEM};
  unsigned int domainid_defaults_2_0_len = 4;
  uint8_t domainid_input[1];
  uint8_t *domainid_array;
  unsigned int domainid_array_len;
  uint8_t policyid_min = 0;
  uint8_t policyid_max = 255;
  int domainid_specified = 0;
  int policyid_specified = 0;
  unsigned int i, j;
  int output_newline_flag = 0;
  uint8_t node_manager_version;
  int rv = -1;
  int ret;

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

              if (_ipmi_oem_intelnm_parse_domainid (state_data,
						    i,
						    value,
						    &domainid_tmp) < 0)
                goto cleanup;

	      domainid_input[0] = domainid_tmp;

              domainid_array = domainid_input;
              domainid_array_len = 1;
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

  if (!domainid_specified)
    {
      if (_ipmi_oem_intelnm_get_node_manager_version_common (state_data,
							     target_channel_number,
							     target_slave_address,
							     target_lun,
							     &node_manager_version,
							     NULL,
							     NULL,
							     NULL,
							     NULL) < 0)
	goto cleanup;

      if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_0
	  || node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_5)
	{
	  domainid_array = domainid_defaults_2_0;
	  domainid_array_len = domainid_defaults_2_0_len;
	}
      else
	{
	  domainid_array = domainid_defaults_1_5;
	  domainid_array_len = domainid_defaults_1_5_len;
	}
    }

  for (i = 0; i < domainid_array_len; i++)
    {
      for (j = policyid_min; j <= policyid_max; j++)
	{
	  if (output_newline_flag)
	    pstdout_printf (state_data->pstate, "\n");
	  
	  if ((ret = _ipmi_oem_intelnm_get_node_manager_policy_suspend_periods_common (state_data,
										       target_channel_number,
										       target_slave_address,
										       target_lun,
										       domainid_array[i],
										       j,
										       domainid_specified ? 0 : 1,
										       policyid_specified ? 0 : 1)) < 0)
	    goto cleanup;
	  
	  if (ret > 0)
	    output_newline_flag = 1;
	  else
	    output_newline_flag = 0;
	}
    }
  
  rv = 0;
 cleanup:
  return (rv); 
}

static int
_parse_suspend_period_number (ipmi_oem_state_data_t *state_data,
			      unsigned int option_num,
			      const char *str,
			      unsigned int *suspend_period_number)
{
  unsigned int temp;
  char *ptr = NULL;

  assert (state_data);
  assert (str);
  assert (suspend_period_number);

  errno = 0;

  temp = strtoul (str, &ptr, 10);

  if (errno
      || ptr[0] != '\0'
      || !temp
      || temp > IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM suspend period number '%s' in '%s', max allowed '%u'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
		       str,
                       state_data->prog_data->args->oem_options[option_num],
		       IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX);
      return (-1);
    }

  (*suspend_period_number) = temp;
  return (0);
}

static int
_parse_suspend_period_time (ipmi_oem_state_data_t *state_data,
			    unsigned int option_num,
			    const char *value,
			    uint8_t *suspend_period_time)
{
  char buf[IPMI_OEM_STR_BUFLEN + 1];
  unsigned int hours;
  unsigned int minutes;
  char *minutes_ptr = NULL;
  char *ptr = NULL;

  assert (state_data);
  assert (value);
  assert (suspend_period_time);

  memset (buf, '\0', IPMI_OEM_STR_BUFLEN + 1); 

  strncpy (buf, value, IPMI_OEM_STR_BUFLEN);

  if (!(minutes_ptr = strchr (buf, ':')))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid time specified '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
		       value);
      return (-1);
    }

  (*minutes_ptr) = '\0';
  minutes_ptr++;

  errno = 0;
  hours = strtoul (buf, &ptr, 10);

  if (errno
      || ptr[0] != '\0'
      || hours > 23)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid hours specified in '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
		       value);
      return (-1);
    }

  errno = 0;
  minutes = strtoul (buf, &ptr, 10);

  if (errno
      || ptr[0] != '\0'
      || minutes > 59)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid minutes specified in '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
		       value);
      return (-1);
    }

  /* encoded by dividing minutes since midnight by 6 */
  (*suspend_period_time) = ((hours * 60) + minutes) / 6;
  return (0);
}

int
ipmi_oem_intelnm_set_node_manager_policy_suspend_periods (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  int domainid_specified = 0;
  uint8_t policyid = 0;
  int policyid_specified = 0;
  uint8_t policy_suspend_start_time[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  int policy_suspend_start_time_specified[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_stop_time[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  int policy_suspend_stop_time_specified[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_monday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_tuesday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_wednesday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_thursday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_friday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_saturday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  uint8_t policy_suspend_period_recurrence_sunday[IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX];
  unsigned int suspend_period_number;
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 2);

  /* initialize */
  
  memset (policy_suspend_start_time_specified, '\0', sizeof (int) * IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX);
  memset (policy_suspend_stop_time_specified, '\0', sizeof (int) * IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX);

  for (i = 0; i < IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX; i++)
    {
      policy_suspend_period_recurrence_monday[i] = IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD;
      policy_suspend_period_recurrence_tuesday[i] = IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD;
      policy_suspend_period_recurrence_wednesday[i] = IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD;
      policy_suspend_period_recurrence_thursday[i] = IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD;
      policy_suspend_period_recurrence_friday[i] = IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD;
      policy_suspend_period_recurrence_saturday[i] = IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD;
      policy_suspend_period_recurrence_sunday[i] = IPMI_OEM_INTEL_NODE_MANAGER_DO_NOT_REPEAT_THE_SUSPEND_PERIOD;
    }

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
	  if (_ipmi_oem_intelnm_parse_domainid (state_data,
						i,
						value,
						&domainid) < 0)
	    goto cleanup;
	  
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "policyid"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &policyid) < 0)
	    goto cleanup;
	  
	  policyid_specified++;
	}
      else if (!strncasecmp (key, "suspendperiodstart", 18))
	{
	  if (_parse_suspend_period_number (state_data,
					    i,
					    key + 18,
					    &suspend_period_number) < 0)
	    goto cleanup;

	  if (_parse_suspend_period_time (state_data,
					  i,
					  value,
					  &policy_suspend_start_time[suspend_period_number - 1]) < 0)
	    goto cleanup;

	  policy_suspend_start_time_specified[suspend_period_number - 1]++;
	}
      else if (!strncasecmp (key, "suspendperiodstop", 17))
	{
	  if (_parse_suspend_period_number (state_data,
					    i,
					    key + 17,
					    &suspend_period_number) < 0)
	    goto cleanup;

	  if (_parse_suspend_period_time (state_data,
					  i,
					  value,
					  &policy_suspend_stop_time[suspend_period_number - 1]) < 0)
	    goto cleanup;

	  policy_suspend_stop_time_specified[suspend_period_number - 1]++;
	}
      else if (!strncasecmp (key, "suspendperiodrepeat", 19))
	{
	  if (_parse_suspend_period_number (state_data,
					    i,
					    key + 19,
					    &suspend_period_number) < 0)
	    goto cleanup;

	  if (strcasecmp (value, "monday")
	      && strcasecmp (value, "tuesday")
	      && strcasecmp (value, "wednesday")
	      && strcasecmp (value, "thursday")
	      && strcasecmp (value, "friday")
	      && strcasecmp (value, "saturday")
	      && strcasecmp (value, "sunday"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid day\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "monday"))
	    policy_suspend_period_recurrence_monday[suspend_period_number - 1] = IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD;
	  else if (! strcasecmp (value, "tuesday"))
	    policy_suspend_period_recurrence_tuesday[suspend_period_number - 1] = IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD;
	  else if (! strcasecmp (value, "wednesday"))
	    policy_suspend_period_recurrence_wednesday[suspend_period_number - 1] = IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD;
	  else if (! strcasecmp (value, "thursday"))
	    policy_suspend_period_recurrence_thursday[suspend_period_number - 1] = IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD;
	  else if (! strcasecmp (value, "friday"))
	    policy_suspend_period_recurrence_friday[suspend_period_number - 1] = IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD;
	  else if (! strcasecmp (value, "saturday"))
	    policy_suspend_period_recurrence_saturday[suspend_period_number - 1] = IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD;
	  else /* strcasecmp (value, "sunday") */
	    policy_suspend_period_recurrence_sunday[suspend_period_number - 1] = IPMI_OEM_INTEL_NODE_MANAGER_REPEAT_THE_SUSPEND_PERIOD;
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

  /* various error checks */

  for (i = 0; i < IPMI_OEM_INTEL_NODE_MANAGER_POLICY_SUSPEND_PERIODS_MAX; i++)
    {
      if ((policy_suspend_start_time_specified[i]
	   && !policy_suspend_stop_time_specified[i])
	  || (!policy_suspend_start_time_specified[i]
	      && policy_suspend_stop_time_specified[i]))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s invalid setup, must specify start and stop suspend time for suspend policy '%u'\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command,
			   i + 1);
	  goto cleanup;
	}

      if (policy_suspend_stop_time[i] < policy_suspend_start_time[i])
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s invalid setup, must specify stop time after start time for suspend policy '%u'\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command,
			   i + 1);
	  goto cleanup;
	}
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* We've done error checks already, so we will use policy_suspend_start_time_specified[] as the flag
   * for all determinations if we pass a pointer argument or NULL
   */
  if (ipmi_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods (state_data->ipmi_ctx,
									       target_channel_number,
									       target_slave_address,
									       target_lun,
									       domainid,
									       policyid,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_start_time[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_stop_time[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_period_recurrence_monday[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_period_recurrence_tuesday[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_period_recurrence_wednesday[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_period_recurrence_thursday[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_period_recurrence_friday[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_period_recurrence_saturday[0] : NULL,
									       policy_suspend_start_time_specified[0] ? &policy_suspend_period_recurrence_sunday[0] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_start_time[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_stop_time[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_period_recurrence_monday[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_period_recurrence_tuesday[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_period_recurrence_wednesday[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_period_recurrence_thursday[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_period_recurrence_friday[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_period_recurrence_saturday[1] : NULL,
									       policy_suspend_start_time_specified[1] ? &policy_suspend_period_recurrence_sunday[1] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_start_time[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_stop_time[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_period_recurrence_monday[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_period_recurrence_tuesday[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_period_recurrence_wednesday[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_period_recurrence_thursday[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_period_recurrence_friday[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_period_recurrence_saturday[2] : NULL,
									       policy_suspend_start_time_specified[2] ? &policy_suspend_period_recurrence_sunday[2] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_start_time[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_stop_time[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_period_recurrence_monday[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_period_recurrence_tuesday[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_period_recurrence_wednesday[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_period_recurrence_thursday[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_period_recurrence_friday[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_period_recurrence_saturday[3] : NULL,
									       policy_suspend_start_time_specified[3] ? &policy_suspend_period_recurrence_sunday[3] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_start_time[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_stop_time[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_period_recurrence_monday[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_period_recurrence_tuesday[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_period_recurrence_wednesday[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_period_recurrence_thursday[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_period_recurrence_friday[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_period_recurrence_saturday[4] : NULL,
									       policy_suspend_start_time_specified[4] ? &policy_suspend_period_recurrence_sunday[4] : NULL,
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
			   "ipmi_cmd_oem_intel_node_manager_set_node_manager_policy_suspend_periods: %s\n",
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
ipmi_oem_intelnm_set_node_manager_power_draw_range (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  int domainid_specified = 0;
  uint16_t minpowerdrawrange = 0;
  int minpowerdrawrange_specified = 0; 
  uint16_t maxpowerdrawrange = 0;
  int maxpowerdrawrange_specified = 0; 
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 3);

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
	  if (_ipmi_oem_intelnm_parse_domainid (state_data,
						i,
						value,
						&domainid) < 0)
	    goto cleanup;
	  
	  domainid_specified++;
	}
      else if (!strcasecmp (key, "minpowerdrawrange"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data,
					   i,
					   value,
					   &minpowerdrawrange) < 0)
	    goto cleanup;
	  
	  minpowerdrawrange_specified++;
	}
      else if (!strcasecmp (key, "maxpowerdrawrange"))
	{
	  if (ipmi_oem_parse_2_byte_field (state_data,
					   i,
					   value,
					   &maxpowerdrawrange) < 0)
	    goto cleanup;
	  
	  maxpowerdrawrange_specified++;
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

  if (!minpowerdrawrange_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Min Power Draw Range must be specified\n");
      goto cleanup;
    }

  if (!maxpowerdrawrange_specified)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Max Power Draw Range must be specified\n");
      goto cleanup;
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_power_draw_range_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_set_node_manager_power_draw_range (state_data->ipmi_ctx,
									 target_channel_number,
									 target_slave_address,
									 target_lun,
									 domainid,
									 minpowerdrawrange,
									 maxpowerdrawrange,
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
			   "ipmi_cmd_oem_intel_node_manager_set_node_manager_power_draw_range: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

#if 0
/* can't verify */
int
ipmi_oem_intelnm_get_limiting_policy_id (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t domainid = IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM;
  int domainid_specified = 0;
  uint8_t policy_id;
  uint64_t val;
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 1);

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
	  if (_ipmi_oem_intelnm_parse_domainid (state_data,
						i,
						value,
						&domainid) < 0)
	    goto cleanup;
	  
	  domainid_specified++;
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

  if (domainid != IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_ENTIRE_PLATFORM
      && domainid != IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_CPU_SUBSYSTEM
      && domainid != IPMI_OEM_INTEL_NODE_MANAGER_DOMAIN_ID_MEMORY_SUBSYSTEM)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "domain ID illegal for this command\n");
      goto cleanup;
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_limiting_policy_id_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_get_limiting_policy_id (state_data->ipmi_ctx,
							      target_channel_number,
							      target_slave_address,
							      target_lun,
							      domainid,
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
			   "ipmi_cmd_oem_intel_node_manager_get_limiting_policy_id: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "policy_id",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "FIID_OBJ_GET: 'policy_id': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  policy_id = val;

  pstdout_printf (state_data->pstate,
		  "Limiting Policy ID: %u\n",
		  policy_id);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
#endif

int
ipmi_oem_intelnm_get_node_manager_alert_destination (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t channel_number;
  uint8_t destination_information_operation;
  uint8_t destination_selector;
  uint8_t alert_string_selector;
  uint8_t send_alert_string;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_get_node_manager_alert_destination_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_oem_intel_node_manager_get_node_manager_alert_destination (state_data->ipmi_ctx,
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
			   "ipmi_cmd_oem_intel_node_manager_get_node_manager_alert_destination: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	}
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "channel_number",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'channel_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  channel_number = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "destination_information_operation",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'destination_information_operation': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  destination_information_operation = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "destination_selector",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'destination_selector': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  destination_selector = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "alert_string_selector",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'alert_string_selector': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  alert_string_selector = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "send_alert_string",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'send_alert_string': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  send_alert_string = val;

  pstdout_printf (state_data->pstate,
                  "Channel Number        : %u\n",
		  channel_number);

  pstdout_printf (state_data->pstate,
                  "Alert Receiver        : %s\n",
		  destination_information_operation == IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_REGISTER_ALERT_RECEIVER ? "Registered" : "Unregistered");

  pstdout_printf (state_data->pstate,
                  "Destination Selector  : %u\n",
		  destination_selector);

  pstdout_printf (state_data->pstate,
                  "Alert String Selector : %u\n",
		  alert_string_selector);

  pstdout_printf (state_data->pstate,
                  "Send Alert String     : %s\n",
		  send_alert_string == IPMI_OEM_INTEL_NODE_MANAGER_DONT_SEND_AN_ALERT_STRING ? "No" : "Yes"); 

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_intelnm_set_node_manager_alert_destination (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t channelnumber= 0;
  int channelnumber_specified= 0;
  /* registerX b/c register is a C keyword */
  uint8_t registerX = IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_REGISTER_ALERT_RECEIVER;
  int registerX_specified= 0;
  uint8_t slaveaddress= 0;
  int slaveaddress_specified= 0;
  uint8_t destinationselector= 0;
  int destinationselector_specified= 0;
  uint8_t alertstringselector= 0;
  int alertstringselector_specified= 0;
  uint8_t sendalertstring= 0;
  int sendalertstring_specified= 0;
  int rv = -1;
  int ret;
  unsigned int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count >= 1);

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
				    i,
				    &key,
				    &value) < 0)
	goto cleanup;

      if (!strcasecmp (key, "channelnumber"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &channelnumber) < 0)
	    goto cleanup;

	  if (!IPMI_CHANNEL_NUMBER_VALID (channelnumber))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid channel number\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  channelnumber_specified++;
	}
      else if (!strcasecmp (key, "slaveaddress"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &slaveaddress) < 0)
	    goto cleanup;
	  
	  slaveaddress_specified++;
	}
      else if (!strcasecmp (key, "destinationselector"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &destinationselector) < 0)
	    goto cleanup;
	  
	  destinationselector_specified++;
	}
      else if (!strcasecmp (key, "alertstringselector"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data,
					   i,
					   value,
					   &alertstringselector) < 0)
	    goto cleanup;
	  
	  alertstringselector_specified++;
	}
      else if (!strcasecmp (key, "register"))
	{
	  if (strcasecmp (value, "yes")
	      && strcasecmp (value, "no"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid value - specify yes or not\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "yes"))
	    registerX = IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_REGISTER_ALERT_RECEIVER;
	  else
	    registerX = IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_UNREGISTER_ALERT_RECEIVER;
	  
	  registerX_specified++;
	}
      else if (!strcasecmp (key, "sendalertstring"))
	{
	  if (strcasecmp (value, "yes")
	      && strcasecmp (value, "no"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid value - specify yes or not\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }
	  
	  if (!strcasecmp (value, "yes"))
	    sendalertstring = IPMI_OEM_INTEL_NODE_MANAGER_SEND_ALERT_STRING_IDENTIFIED_BY_STRING_SELECTOR;
	  else
	    sendalertstring = IPMI_OEM_INTEL_NODE_MANAGER_DONT_SEND_AN_ALERT_STRING;
	  
	  sendalertstring_specified++;
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

  if (registerX == IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_REGISTER_ALERT_RECEIVER)
    {
      if (!channelnumber_specified)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "channel number must be specified if registering alert receiver\n");
	  goto cleanup;
	}

      if (!alertstringselector_specified)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "alert string selector must be specified if registering alert receiver\n");
	  goto cleanup;
	}

      if (!sendalertstring_specified)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "send alert string must be specified if registering alert receiver\n");
	  goto cleanup;
	}

      if (!slaveaddress_specified && !destinationselector_specified)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "slave address or destination selector must be specified\n");
	  goto cleanup;
	}

      if (slaveaddress_specified && destinationselector_specified)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "only one of slave address and destination selector can be specified\n");
	  goto cleanup;
	}
    }

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_oem_intel_node_manager_set_node_manager_alert_destination_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (registerX == IPMI_OEM_INTEL_NODE_MANAGER_DESTINATION_INFORMATION_OPERATION_UNREGISTER_ALERT_RECEIVER)
    ret = ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_destination (state_data->ipmi_ctx,
									      target_channel_number,
									      target_slave_address,
									      target_lun,
									      0,
									      registerX,
									      0,
									      0,
									      0,
									      obj_cmd_rs);
  else
    {
      if (destinationselector_specified)
	ret = ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb (state_data->ipmi_ctx,
										       target_channel_number,
										       target_slave_address,
										       target_lun,
										       channelnumber,
										       registerX,
										       destinationselector,
										       alertstringselector,
										       sendalertstring,
										       obj_cmd_rs);
      else
	ret = ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_destination_ipmb (state_data->ipmi_ctx,
										       target_channel_number,
										       target_slave_address,
										       target_lun,
										       channelnumber,
										       registerX,
										       slaveaddress,
										       alertstringselector,
										       sendalertstring,
										       obj_cmd_rs);
    }
  
  if (ret < 0)
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
			   "ipmi_cmd_oem_intel_node_manager_set_node_manager_alert_destination: %s\n",
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
  uint8_t target_channel_number = 0;
  uint8_t target_slave_address = 0;
  uint8_t target_lun = 0;
  uint8_t node_manager_version;
  uint8_t ipmi_interface_version;
  uint8_t patch_version;
  uint8_t major_firmware_revision;
  uint8_t minor_firmware_revision;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_intelnm_node_manager_init (state_data,
                                           &target_channel_number,
                                           &target_slave_address,
                                           &target_lun) < 0)
    goto cleanup;

  if (_ipmi_oem_intelnm_get_node_manager_version_common (state_data,
							 target_channel_number,
							 target_slave_address,
							 target_lun,
							 &node_manager_version,
							 &ipmi_interface_version,
							 &patch_version,
							 &major_firmware_revision,
							 &minor_firmware_revision) < 0)
    goto cleanup;

  if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_1_0)
    pstdout_printf (state_data->pstate,
                    "Node Manager Version   : 1.0\n");
  else if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_1_5)
    pstdout_printf (state_data->pstate,
                    "Node Manager Version   : 1.5\n");
  else if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_0)
    pstdout_printf (state_data->pstate,
                    "Node Manager Version   : 2.0\n");
  else if (node_manager_version == IPMI_OEM_INTEL_NODE_MANAGER_VERSION_2_5)
    pstdout_printf (state_data->pstate,
		    "Node Manager Version   : 2.5\n");
  else
    pstdout_printf (state_data->pstate,
                    "Node Manager Version   : %02Xh\n",
                    node_manager_version);

  if (ipmi_interface_version == IPMI_OEM_INTEL_NODE_MANAGER_IPMI_INTERFACE_VERSION_1_0)
    pstdout_printf (state_data->pstate,
                    "IPMI Interface Version : 1.0\n");
  else if (ipmi_interface_version == IPMI_OEM_INTEL_NODE_MANAGER_IPMI_INTERFACE_VERSION_2_0)
    pstdout_printf (state_data->pstate,
                    "IPMI Interface Version : 2.0\n");
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
  return (rv);
}
