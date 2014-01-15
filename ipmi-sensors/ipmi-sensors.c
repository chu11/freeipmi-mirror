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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-sensors.h"
#include "ipmi-sensors-argp.h"
#include "ipmi-sensors-simple-output.h"
#include "ipmi-sensors-detailed-output.h"
#include "ipmi-sensors-oem-intel-node-manager.h"
#include "ipmi-sensors-output-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"
#include "tool-util-common.h"

#define IPMI_SENSORS_MESSAGE_LENGTH 1024

#define IPMI_SENSORS_TIME_BUFLEN    512

static int
_sdr_repository_info (ipmi_sensors_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t major, minor;
  uint16_t record_count, free_space;
  uint64_t val;
  char timestr[IPMI_SENSORS_TIME_BUFLEN + 1];
  char *str;
  int rv = -1;
  uint8_t allocation_supported = 0;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sdr_repository_info_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sdr_repository_info (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sdr_repository_info: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "sdr_version_major", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sdr_version_major': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  major = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "sdr_version_minor", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sdr_version_minor': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  minor = val;

  pstdout_printf (state_data->pstate,
                  "SDR version                                       : %u.%u\n",
                  major,
                  minor);

  if (FIID_OBJ_GET (obj_cmd_rs, "record_count", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'record_count': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  record_count = val;

  pstdout_printf (state_data->pstate,
                  "SDR record count                                  : %u\n",
                  record_count);

  if (FIID_OBJ_GET (obj_cmd_rs, "free_space", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'free_space': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  free_space = val;

  pstdout_printf (state_data->pstate,
                  "Free space remaining                              : %u bytes\n",
                  free_space);

  if (FIID_OBJ_GET (obj_cmd_rs, "most_recent_addition_timestamp", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'most_recent_addition_timestamp': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (timestr, '\0', IPMI_SENSORS_TIME_BUFLEN + 1);
  
  if (ipmi_timestamp_string ((uint32_t)val,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%m/%d/%Y - %H:%M:%S",
			     timestr,
			     IPMI_SENSORS_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Most recent addition timestamp                    : %s\n",
                  timestr);

  if (FIID_OBJ_GET (obj_cmd_rs, "most_recent_erase_timestamp", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'most_recent_erase_timestamp': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (timestr, '\0', IPMI_SENSORS_TIME_BUFLEN + 1);
  
  if (ipmi_timestamp_string ((uint32_t)val,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%m/%d/%Y - %H:%M:%S",
			     timestr,
			     IPMI_SENSORS_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Most recent erase timestamp                       : %s\n",
                  timestr);

  if (FIID_OBJ_GET (obj_cmd_rs, "get_sdr_repository_allocation_info_command_supported", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'get_sdr_repository_allocation_info_command_supported': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Get SDR Repository Allocation Information Command : %s\n",
                  (val ? "supported" : "unsupported"));

  allocation_supported = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "reserve_sdr_repository_command_supported", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'reserve_sdr_repository_command_supported': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Reserve SDR Repository Command                    : %s\n",
                  (val ? "supported" : "unsupported"));

  if (FIID_OBJ_GET (obj_cmd_rs, "partial_add_sdr_command_supported", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'partial_add_sdr_command_supported': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Partial Add SDR Command                           : %s\n",
                  (val ? "supported" : "unsupported"));

  if (FIID_OBJ_GET (obj_cmd_rs, "delete_sdr_command_supported", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'delete_sdr_command_supported': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Delete SDR Command                                : %s\n",
                  (val ? "supported" : "unsupported"));

  if (FIID_OBJ_GET (obj_cmd_rs, "modal_non_modal_sdr_repository_update_operation_supported", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'modal_non_modal_sdr_repository_update_operation_supported': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  switch (val)
    {
    case IPMI_SDR_MODAL_NON_MODAL_REPOSITORY_UPDATE_OP_UNSPECIFIED:
      str = "unspecified";
      break;
    case IPMI_SDR_NON_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED:
      str = "non-Modal supported";
      break;
    case IPMI_SDR_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED:
      str = "modal supported";
      break;
    case IPMI_SDR_MODAL_NON_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED:
      str = "both supported";
      break;
    default:
      str = "unknown";
    }
  
  pstdout_printf (state_data->pstate,
                  "Modal/non-modal SDR Repository Update operation   : %s\n",
                  str);

  if (FIID_OBJ_GET (obj_cmd_rs, "overflow_flag", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'overflow_flag': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  /* "SDR could not be written due to lack of space in the SDR Repository" */
  pstdout_printf (state_data->pstate,
                  "SDR could not be written due to lack of space     : %s\n",
                  (val ? "Yes" : "No"));

  if (allocation_supported)
    {
      uint16_t number_of_possible_allocation_units;
      uint16_t allocation_unit_size;
      uint16_t number_of_free_allocation_units;
      uint16_t largest_free_block;
      uint8_t maximum_record_size;

      fiid_obj_destroy (obj_cmd_rs);

      if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sdr_repository_allocation_info_rs)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_create: %s\n",
                           strerror (errno));
          goto cleanup;
        }

      if (ipmi_cmd_get_sdr_repository_allocation_info (state_data->ipmi_ctx, obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_get_sdr_repository_allocation_info: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs, "number_of_possible_allocation_units", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'number_of_possible_allocation_units': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      number_of_possible_allocation_units = val;
       
      if (FIID_OBJ_GET (obj_cmd_rs, "allocation_unit_size", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'allocation_unit_size': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      allocation_unit_size = val;

      if (FIID_OBJ_GET (obj_cmd_rs, "number_of_free_allocation_units", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'number_of_free_allocation_units': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      number_of_free_allocation_units = val;

      if (FIID_OBJ_GET (obj_cmd_rs, "largest_free_block", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'largest_free_block': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      largest_free_block = val;

      if (FIID_OBJ_GET (obj_cmd_rs, "maximum_record_size", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'maximum_record_size': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      maximum_record_size = val;

      if (!number_of_possible_allocation_units)
        pstdout_printf (state_data->pstate,
                        "Number of possible allocation units               : unspecified\n");
      else
        pstdout_printf (state_data->pstate,
                        "Number of possible allocation units               : %u\n",
                        number_of_possible_allocation_units);
      
      if (!allocation_unit_size)
        pstdout_printf (state_data->pstate,
                        "Allocation unit size                              : unspecified\n");
      else
        pstdout_printf (state_data->pstate,
                        "Allocation unit size                              : %u bytes\n",
                        allocation_unit_size);
      
      pstdout_printf (state_data->pstate,
                      "Number of free allocation units                   : %u\n",
                      number_of_free_allocation_units);
      
      pstdout_printf (state_data->pstate,
                      "Largest free block                                : %u allocation units\n",
                      largest_free_block);

      pstdout_printf (state_data->pstate,
                      "Maximum record size                               : %u allocation units\n",
                      maximum_record_size);
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_output_setup (ipmi_sensors_state_data_t *state_data)
{
  assert (state_data);

  if (!state_data->prog_data->args->verbose_count)
    return (ipmi_sensors_simple_output_setup (state_data));

  return (0);
}

static int
_calculate_record_ids (ipmi_sensors_state_data_t *state_data,
                       unsigned int output_record_ids[MAX_SENSOR_RECORD_IDS],
                       unsigned int *output_record_ids_length)
{
  uint16_t record_count;
  uint16_t record_id;
  unsigned int i;
  unsigned int j;

  assert (state_data);
  assert (output_record_ids);
  assert (output_record_ids_length);

  memset (output_record_ids, '\0', sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);
  (*output_record_ids_length) = 0;

  if (ipmi_sdr_cache_record_count (state_data->sdr_ctx,
                                   &record_count) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_cache_record_count: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  /* 
   * achu: This code could be done far more concisely, but it got
   * confusing and hard to understand.  We will divide this up into
   * simple chunks, if the user specified neither record_ids and
   * types, record_ids, or types.  record_ids take precedence over
   * types, and exclude_record_ids takes precedence over
   * exclude_types.
   */

  if (!state_data->prog_data->args->record_ids_length
      && !state_data->prog_data->args->sensor_types_length)
    {
      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (state_data->sdr_ctx))
        {
          if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
						 NULL,
						 0,
                                                 &record_id,
						 NULL) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_parse_record_id_and_type: %s\n",
                               ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
              return (-1);
            }

          if (state_data->prog_data->args->exclude_record_ids_length)
            {
              int found_exclude = 0;
              
              for (j = 0; j < state_data->prog_data->args->exclude_record_ids_length; j++)
                {
                  if (record_id == state_data->prog_data->args->exclude_record_ids[j])
                    {
                      found_exclude++;
                      break;
                    }
                }
              
              if (found_exclude)
                continue;
            }

          if (state_data->prog_data->args->exclude_sensor_types_length)
            {
              int flag;

              if ((flag = sensor_type_listed_sdr (state_data->pstate,
                                                  state_data->sdr_ctx,
                                                  state_data->prog_data->args->exclude_sensor_types,
                                                  state_data->prog_data->args->exclude_sensor_types_length)) < 0)
                return (-1);

              if (flag)
                continue;
            }
          
          output_record_ids[(*output_record_ids_length)] = record_id;
          (*output_record_ids_length)++;

	  if ((*output_record_ids_length) >= MAX_SENSOR_RECORD_IDS)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "Too many sensors found on system; limit is %u\n",
			       MAX_SENSOR_RECORD_IDS);
	      return (-1);
	    }
        }
    }
  else if (state_data->prog_data->args->record_ids_length)
    {
      for (i = 0; i < state_data->prog_data->args->record_ids_length; i++)
        {
          if (state_data->prog_data->args->exclude_record_ids_length)
            {
              int found_exclude = 0;
              
              for (j = 0; j < state_data->prog_data->args->exclude_record_ids_length; j++)
                {
                  if (state_data->prog_data->args->record_ids[i] == state_data->prog_data->args->exclude_record_ids[j])
                    {
                      found_exclude++;
                      break;
                    }
                }
              
              if (found_exclude)
                continue;
            }

          if (ipmi_sdr_cache_search_record_id (state_data->sdr_ctx, state_data->prog_data->args->record_ids[i]) < 0)
            {
              if (ipmi_sdr_ctx_errnum (state_data->sdr_ctx) == IPMI_SDR_ERR_NOT_FOUND)
                {
                  pstdout_printf (state_data->pstate,
                                  "Sensor Record ID '%d' not found\n",
                                  state_data->prog_data->args->record_ids[i]);
                  return (-1);
                }
              else
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "ipmi_sdr_cache_search_record_id: %s\n",
                                   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
                  return (-1);
                }
            }

          if (state_data->prog_data->args->exclude_sensor_types_length)
            {
              int flag;
	      
              if ((flag = sensor_type_listed_sdr (state_data->pstate,
                                                  state_data->sdr_ctx,
                                                  state_data->prog_data->args->exclude_sensor_types,
                                                  state_data->prog_data->args->exclude_sensor_types_length)) < 0)
                return (-1);

              if (flag)
                continue;
            }
          
          output_record_ids[(*output_record_ids_length)] = state_data->prog_data->args->record_ids[i];
          (*output_record_ids_length)++;

	  if ((*output_record_ids_length) >= MAX_SENSOR_RECORD_IDS)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "Too many sensors specified; limit is %u\n",
			       MAX_SENSOR_RECORD_IDS);
	      return (-1);
	    }
        }
    }
  else /* state_data->prog_data->args->sensor_types_length */
    {
      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (state_data->sdr_ctx))
        {
          int flag;

          if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
						 NULL,
						 0,
                                                 &record_id,
						 NULL) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_parse_record_id_and_type: %s\n",
                               ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
              return (-1);
            }

          if ((flag = sensor_type_listed_sdr (state_data->pstate,
                                              state_data->sdr_ctx,
                                              state_data->prog_data->args->sensor_types,
                                              state_data->prog_data->args->sensor_types_length)) < 0)
            return (-1);

          if (!flag)
            continue;

          if (state_data->prog_data->args->exclude_record_ids)
            {
              int found_exclude = 0;
              
              for (j = 0; j < state_data->prog_data->args->exclude_record_ids_length; j++)
                {
                  if (record_id == state_data->prog_data->args->exclude_record_ids[j])
                    {
                      found_exclude++;
                      break;
                    }
                }
              
              if (found_exclude)
                continue;
            }

          if (state_data->prog_data->args->exclude_sensor_types_length)
            {
              if ((flag = sensor_type_listed_sdr (state_data->pstate,
                                                  state_data->sdr_ctx,
                                                  state_data->prog_data->args->exclude_sensor_types,
                                                  state_data->prog_data->args->exclude_sensor_types_length)) < 0)
                return (-1);
              
              if (flag)
                continue;
            }
          
          output_record_ids[(*output_record_ids_length)] = record_id;
          (*output_record_ids_length)++;
	  
	  if ((*output_record_ids_length) >= MAX_SENSOR_RECORD_IDS)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "Too many sensors found on system; limit is %u\n",
			       MAX_SENSOR_RECORD_IDS);
	      return (-1);
	    }
        }
    }

  return (0);
}

/* Return 1 if generated message, 0 if not, -1 on error */
static int
_intel_nm_oem_event_message (ipmi_sensors_state_data_t *state_data,
                             uint8_t sensor_reading_raw,
                             char ***event_message_list,
                             unsigned int *event_message_list_len)
{
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_reading_type_code;
  int rv = -1;

  assert (state_data);
  assert (event_message_list);
  assert (event_message_list_len);
  assert (state_data->prog_data->args->interpret_oem_data);
  assert (state_data->intel_node_manager.node_manager_data_found);

  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				  NULL,
				  0,
                                  &sensor_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT
      && sensor_type == IPMI_SENSOR_TYPE_OEM_INTEL_NODE_MANAGER
      && state_data->intel_node_manager.nm_operational_capabilities_sensor_number == sensor_number)
    {
      uint8_t policy_interface_capability;
      uint8_t monitoring_capability;
      uint8_t power_limiting_capability;
      char policy_interface_capability_str[IPMI_SENSORS_MESSAGE_LENGTH + 1];
      char monitoring_capability_str[IPMI_SENSORS_MESSAGE_LENGTH + 1];
      char power_limiting_capability_str[IPMI_SENSORS_MESSAGE_LENGTH + 1];
      char **tmp_event_message_list = NULL;

      memset (policy_interface_capability_str, '\0', IPMI_SENSORS_MESSAGE_LENGTH + 1);
      memset (monitoring_capability_str, '\0', IPMI_SENSORS_MESSAGE_LENGTH + 1);
      memset (power_limiting_capability_str, '\0', IPMI_SENSORS_MESSAGE_LENGTH + 1);

      policy_interface_capability = (sensor_reading_raw & IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POLICY_INTERFACE_CAPABILITY_BITMASK);
      policy_interface_capability >>= IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POLICY_INTERFACE_CAPABILITY_SHIFT;

      monitoring_capability = (sensor_reading_raw & IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_MONITORING_CAPABILITY_BITMASK);
      monitoring_capability >>= IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_MONITORING_CAPABILITY_SHIFT;

      power_limiting_capability = (sensor_reading_raw & IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POWER_LIMITING_CAPABILITY_BITMASK);
      power_limiting_capability >>= IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_EVENT_DATA1_POWER_LIMITING_CAPABILITY_SHIFT;

      snprintf (policy_interface_capability_str,
                IPMI_SENSORS_MESSAGE_LENGTH,
                "Policy Interface Capability %s",
                (policy_interface_capability == IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_AVAILABLE) ? "Available" : "Not Available");

      snprintf (monitoring_capability_str,
                IPMI_SENSORS_MESSAGE_LENGTH,
                "Monitoring Capability %s",
                (monitoring_capability == IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_AVAILABLE) ? "Available" : "Not Available");

      snprintf (power_limiting_capability_str,
                IPMI_SENSORS_MESSAGE_LENGTH,
                "Power Limiting Capability %s",
                (power_limiting_capability == IPMI_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT_AVAILABLE) ? "Available" : "Not Available");
 
      if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * 4)))
        {
          pstdout_perror (state_data->pstate, "malloc");
          goto cleanup;
        }

      if (!(tmp_event_message_list[0] = strdup (policy_interface_capability_str)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          free (tmp_event_message_list);
          goto cleanup;
        }

      if (!(tmp_event_message_list[1] = strdup (monitoring_capability_str)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          free (tmp_event_message_list[0]);
          free (tmp_event_message_list);
          goto cleanup;
        }

      if (!(tmp_event_message_list[2] = strdup (power_limiting_capability_str)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          free (tmp_event_message_list[1]);
          free (tmp_event_message_list[0]);
          free (tmp_event_message_list);
          goto cleanup;
        }

      tmp_event_message_list[3] = NULL;

      (*event_message_list) = tmp_event_message_list;
      (*event_message_list_len) = 3;

      rv = 1;
    }
  else
    rv = 0;

 cleanup:
  return (rv);
}

static int
_get_event_message (ipmi_sensors_state_data_t *state_data,
                    uint16_t sensor_event_bitmask,
                    char ***event_message_list,
                    unsigned int *event_message_list_len)
{
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_reading_type_code;
  uint32_t manufacturer_id = 0;
  uint16_t product_id = 0;
  unsigned int flags = IPMI_GET_EVENT_MESSAGES_FLAGS_DEFAULT;
  int rv = -1;

  assert (state_data);
  assert (event_message_list);
  assert (event_message_list_len);

  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				  NULL,
				  0,
                                  &sensor_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
				    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  flags |= IPMI_GET_EVENT_MESSAGES_FLAGS_SENSOR_READING;

  if (!state_data->prog_data->args->verbose_count)
    flags |= IPMI_GET_EVENT_MESSAGES_FLAGS_SHORT;

  if (state_data->prog_data->args->interpret_oem_data)
    {
      manufacturer_id = state_data->oem_data.manufacturer_id;
      product_id = state_data->oem_data.product_id;
      flags |= IPMI_GET_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA;
    }

  if (state_data->prog_data->args->ignore_unrecognized_events)
    flags |= IPMI_GET_EVENT_MESSAGES_FLAGS_IGNORE_UNRECOGNIZED_EVENTS;

  if (ipmi_get_event_messages (event_reading_type_code,
                               sensor_type,
			       sensor_number,
                               sensor_event_bitmask,
                               manufacturer_id,
                               product_id,
                               event_message_list,
                               event_message_list_len,
                               IPMI_SENSORS_NO_EVENT_STRING,
                               flags) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_event_messages: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_output_sensor (ipmi_sensors_state_data_t *state_data,
                uint8_t sensor_number_base,
                uint8_t shared_sensor_number_offset)
{
  uint8_t sdr_record[IPMI_SDR_MAX_RECORD_LENGTH];
  int sdr_record_len = 0;
  uint8_t sensor_reading_raw = 0;
  double *sensor_reading = NULL;
  uint16_t sensor_event_bitmask = 0;
  char **event_message_list = NULL;
  int event_message_output_type = IPMI_SENSORS_EVENT_NORMAL;
  unsigned int event_message_list_len = 0;
  int rv = -1;

  assert (state_data);

  if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_ctx,
						    sdr_record,
						    IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_record_read: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sensor_read (state_data->sensor_read_ctx,
                        sdr_record,
                        sdr_record_len,
                        shared_sensor_number_offset,
                        &sensor_reading_raw,
                        &sensor_reading,
                        &sensor_event_bitmask) <= 0)
    {
      int errnum = ipmi_sensor_read_ctx_errnum (state_data->sensor_read_ctx);

      if (errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_ANALOG
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_LINEAR)
        {
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Sensor reading cannot be calculated: %s\n",
                             ipmi_sensor_read_ctx_errormsg (state_data->sensor_read_ctx));

          goto get_events;
        }

      if (errnum == IPMI_SENSOR_READ_ERR_SENSOR_IS_SYSTEM_SOFTWARE
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_READING_UNAVAILABLE
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_SCANNING_DISABLED
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_ANALOG
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_LINEAR
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NOT_OWNED_BY_BMC
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_CANNOT_BE_BRIDGED)
        {
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Sensor reading/event bitmask not available: %s\n",
                             ipmi_sensor_read_ctx_errormsg (state_data->sensor_read_ctx));

          if (state_data->prog_data->args->ignore_not_available_sensors)
            {
              rv = 0;
              goto cleanup;
            }

          event_message_output_type = IPMI_SENSORS_EVENT_NA;

          goto output;
        }

      if (errnum == IPMI_SENSOR_READ_ERR_SENSOR_READING_CANNOT_BE_OBTAINED
          || errnum == IPMI_SENSOR_READ_ERR_NODE_BUSY)
        {
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Sensor reading/event_bitmask retrieval error: %s\n",
                             ipmi_sensor_read_ctx_errormsg (state_data->sensor_read_ctx));
          
          event_message_output_type = IPMI_SENSORS_EVENT_UNKNOWN;
          
          goto output;
        }

      if (errnum == IPMI_SENSOR_READ_ERR_INVALID_SDR_RECORD_TYPE)
        {
          /* Fall through to output.  detailed-output will output SDR
           * information if it pleases, simple-output will ignore this
           * SDR type.
           */

          goto output;
        }

      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sensor_read: %s\n",
                       ipmi_sensor_read_ctx_errormsg (state_data->sensor_read_ctx));
      goto cleanup;
    }

 get_events:

  if (!state_data->prog_data->args->output_event_bitmask
      || state_data->prog_data->args->legacy_output)
    {
      int event_msg_generated = 0;

      /* OEM Interpretation
       *
       * Handle Intel Node Manager special case
       */
      if (state_data->prog_data->args->interpret_oem_data
          && ((state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
               && (state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
		   || state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
		   || state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R))
              || (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
                  && (state_data->oem_data.product_id == IPMI_INVENTEC_PRODUCT_ID_5441
                      || state_data->oem_data.product_id == IPMI_INVENTEC_PRODUCT_ID_5442))
              || (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA
                  && state_data->oem_data.product_id == IPMI_QUANTA_PRODUCT_ID_S99Q))
          && state_data->intel_node_manager.node_manager_data_found)
        {
          if ((event_msg_generated = _intel_nm_oem_event_message (state_data,
                                                                  sensor_reading_raw,
                                                                  &event_message_list,
                                                                  &event_message_list_len)) < 0)
            goto cleanup;
        }
      
      if (!event_msg_generated)
        {
          if (_get_event_message (state_data,
                                  sensor_event_bitmask,
                                  &event_message_list,
                                  &event_message_list_len) < 0)
            goto cleanup;
        }
    }

 output:
  if (state_data->prog_data->args->verbose_count)
    rv = ipmi_sensors_detailed_output (state_data,
				       sensor_number_base + shared_sensor_number_offset,
				       sensor_reading,
				       event_message_output_type,
				       sensor_event_bitmask,
				       event_message_list,
				       event_message_list_len);
  else
    rv = ipmi_sensors_simple_output (state_data,
				     sensor_number_base + shared_sensor_number_offset,
				     sensor_reading,
				     event_message_output_type,
				     sensor_event_bitmask,
				     event_message_list,
				     event_message_list_len);
  
 cleanup:
  free (sensor_reading);
  if (event_message_list)
    {
      unsigned int j;
      for (j = 0; j < event_message_list_len; j++)
        free (event_message_list[j]);
      free (event_message_list);
    }
  return (rv);
}

static int
_display_sensors (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args = NULL;
  unsigned int output_record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int output_record_ids_length = 0;
  unsigned int i;
  unsigned int ctx_flags_orig;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->interpret_oem_data)
    {
      if (ipmi_get_oem_data (state_data->pstate,
                             state_data->ipmi_ctx,
                             &state_data->oem_data) < 0)
        goto cleanup;

      /* OEM Interpretation
       *
       * Intel Node Manager
       *
       * For Intel Chips, not just Intel Motherboards.  Confirmed for:
       *
       * Intel S5500WB/Penguin Computing Relion 700
       * Intel S2600JF/Appro 512X
       * Inventec 5441/Dell Xanadu II
       * Inventec 5442/Dell Xanadu III
       * Quanta S99Q/Dell FS12-TY
       * Quanta QSSC-S4R/Appro GB812X-CN (maintains Intel manufacturer ID)
       *
       * Below confirms the Intel Node Manager exists.  We must do
       * this before reading the sensor to determime what type of
       * sensor it is via the sensor number.
       */
      if ((state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
           && (state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
	       || state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
	       || state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R))
          || (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INVENTEC
              && (state_data->oem_data.product_id == IPMI_INVENTEC_PRODUCT_ID_5441
                  || state_data->oem_data.product_id == IPMI_INVENTEC_PRODUCT_ID_5442))
          || (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_QUANTA
              && state_data->oem_data.product_id == IPMI_QUANTA_PRODUCT_ID_S99Q))
        {
          uint16_t record_count;
          int ret;

          if (ipmi_sdr_cache_record_count (state_data->sdr_ctx,
                                           &record_count) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_cache_record_count: %s\n",
                               ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
              goto cleanup;
            }

	  /* achu:
	   *
	   * In Intel NM 2.0 specification, sensor numbers are now fixed and you
	   * don't have to search the SDR for them.  We could check version of
	   * NM on motherboard to determine if we need to search SDR or not, but
	   * for time being we'll stick to the search SDR method b/c it will
	   * always work.
	   */
          for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (state_data->sdr_ctx))
            {
              if ((ret = ipmi_sdr_oem_parse_intel_node_manager (state_data->sdr_ctx,
								NULL,
								0,
								NULL,
								NULL,
								NULL,
								&state_data->intel_node_manager.nm_health_event_sensor_number,
								&state_data->intel_node_manager.nm_exception_event_sensor_number,
								&state_data->intel_node_manager.nm_operational_capabilities_sensor_number,
								&state_data->intel_node_manager.nm_alert_threshold_exceeded_sensor_number)) < 0)
		{
		  pstdout_fprintf (state_data->pstate,
				   stderr,
				   "ipmi_sdr_oem_parse_intel_node_manager: %s\n",
				   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
		  goto cleanup;
		}
              
              if (ret)
                {
                  state_data->intel_node_manager.node_manager_data_found = 1;
                  break;
                }
            }

          if (ipmi_sdr_cache_first (state_data->sdr_ctx) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_cache_first: %s\n",
                               ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
              goto cleanup;
            }
        }

      if (args->output_sensor_state)
	{
	  if (ipmi_interpret_ctx_set_manufacturer_id (state_data->interpret_ctx,
						      state_data->oem_data.manufacturer_id) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_interpret_ctx_set_manufacturer_id: %s\n",
			       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
	      goto cleanup;
	    }

	  if (ipmi_interpret_ctx_set_product_id (state_data->interpret_ctx,
						 state_data->oem_data.product_id) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_interpret_ctx_set_product_id: %s\n",
			       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
	      goto cleanup;
	    }
	}
    }

  if (_output_setup (state_data) < 0)
    goto cleanup;

  memset (output_record_ids, '\0', sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);

  if (_calculate_record_ids (state_data,
                             output_record_ids,
                             &output_record_ids_length) < 0)
    return (-1);

  if (state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_AUTH_CODE)
    {
      if (ipmi_ctx_get_flags (state_data->ipmi_ctx, &ctx_flags_orig) < 0)
        {
          pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_ctx_get_flags: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }
      
      if (ipmi_ctx_set_flags (state_data->ipmi_ctx, ctx_flags_orig | IPMI_FLAGS_IGNORE_AUTHENTICATION_CODE) < 0)
        {
          pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_ctx_set_flags: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }
    }

  for (i = 0; i < output_record_ids_length; i++)
    {
      uint8_t record_type;
      uint8_t sensor_number_base = 0;

      if (ipmi_sdr_cache_search_record_id (state_data->sdr_ctx,
                                           output_record_ids[i]) < 0)
        {
          /* at this point shouldn't have record id not found error */
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_cache_search_record_id: 0x%02X %s\n",
                           output_record_ids[i],
                           ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          goto cleanup;
        }
    
      if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
					     NULL,
					     0,
					     NULL,
					     &record_type) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_record_id_and_type: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  goto cleanup;
	}

      if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
	  || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
	  || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
	{
	  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
					    NULL,
					    0,
					    &sensor_number_base) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_sdr_parse_sensor_number: %s\n",
			       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	      goto cleanup;
	    }
	}

      if (state_data->prog_data->args->shared_sensors)
        {
          uint8_t share_count;
          int i;

          if (record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
	    goto fallthrough;

          if (ipmi_sdr_parse_sensor_record_sharing (state_data->sdr_ctx,
						    NULL,
						    0,
                                                    &share_count,
                                                    NULL,
                                                    NULL,
                                                    NULL) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_parse_sensor_record_sharing: %s\n",
                               ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
              goto cleanup;
            }
          
          if (share_count <= 1)
	    goto fallthrough;

          /* IPMI spec gives the following example:
           *
           * "If the starting sensor number was 10, and the share
           * count was 3, then sensors 10, 11, and 12 would share
           * the record"
           */
          for (i = 0; i < share_count; i++)
            {
              if (_output_sensor (state_data,
                                  sensor_number_base,
                                  i) < 0)
                goto cleanup;
            }
        }
      else
        {
	fallthrough:
          if (_output_sensor (state_data,
                              sensor_number_base,
                              0) < 0)
            goto cleanup;
        }
    }

  if (state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_AUTH_CODE)
    {
      if (ipmi_ctx_set_flags (state_data->ipmi_ctx, ctx_flags_orig) < 0)
        {
          pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_ctx_set_flags: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
run_cmd_args (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args;

  assert (state_data);

  args = state_data->prog_data->args;

  assert (!args->common_args.flush_cache);

  if (args->sdr_info)
    return (_sdr_repository_info (state_data));

  if (sdr_cache_create_and_load (state_data->sdr_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
				 &state_data->prog_data->args->common_args) < 0)
    return (-1);

  if (_display_sensors (state_data) < 0)
    return (-1);

  return (0);
}

static int
_ipmi_sensors (pstdout_state_t pstate,
               const char *hostname,
               void *arg)
{
  ipmi_sensors_state_data_t state_data;
  ipmi_sensors_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;
  unsigned int sensor_read_flags = 0;

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_sensors_prog_data_t *)arg;

  assert (!prog_data->args->list_sensor_types);
  
  if (prog_data->args->common_args.flush_cache)
    {
      if (sdr_cache_flush_cache (pstate,
                                 hostname,
                                 &prog_data->args->common_args) < 0)
        return (EXIT_FAILURE);
      return (EXIT_SUCCESS);
    }

  memset (&state_data, '\0', sizeof (ipmi_sensors_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;
  
  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
					 hostname,
					 &(prog_data->args->common_args),
					 state_data.pstate)))
    goto cleanup;

  if (!(state_data.sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_ctx_create()");
      goto cleanup;
    }

  if (!(state_data.sensor_read_ctx = ipmi_sensor_read_ctx_create (state_data.ipmi_ctx)))
    {
      pstdout_perror (pstate, "ipmi_sensor_read_ctx_create()");
      goto cleanup;
    }

  if (state_data.prog_data->args->bridge_sensors)
    sensor_read_flags |= IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS;
  
  if (state_data.prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_DISCRETE_READING)
    sensor_read_flags |= IPMI_SENSOR_READ_FLAGS_DISCRETE_READING;
  
  if (state_data.prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SCANNING_DISABLED)
    sensor_read_flags |= IPMI_SENSOR_READ_FLAGS_IGNORE_SCANNING_DISABLED;
  
  if (state_data.prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_BMC_OWNER)
    sensor_read_flags |= IPMI_SENSOR_READ_FLAGS_ASSUME_BMC_OWNER;
  
  if (sensor_read_flags)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sensor_read_ctx_set_flags (state_data.sensor_read_ctx, sensor_read_flags) < 0)
	pstdout_fprintf (pstate,
			 stderr,
			 "ipmi_sensor_read_ctx_set_flags: %s\n",
			 ipmi_sensor_read_ctx_strerror (ipmi_sensor_read_ctx_errnum (state_data.sensor_read_ctx)));
    }

  if (prog_data->args->output_sensor_state)
    {
      if (!(state_data.interpret_ctx = ipmi_interpret_ctx_create ()))
        {
          pstdout_perror (pstate, "ipmi_interpret_ctx_create()");
          goto cleanup;
        }

      if (prog_data->args->sensor_state_config_file)
        {
          if (ipmi_interpret_load_sensor_config (state_data.interpret_ctx,
                                                 prog_data->args->sensor_state_config_file) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST)
                pstdout_fprintf (pstate,
                                 stderr,
                                 "sensor state config file '%s' does not exist\n",
                                 prog_data->args->sensor_state_config_file);
              else if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_PARSE)
                pstdout_fprintf (pstate,
                                 stderr,
                                 "sensor state config file '%s' parse error\n",
                                 prog_data->args->sensor_state_config_file);
              else
                pstdout_fprintf (pstate,
                                 stderr,
                                 "ipmi_interpret_load_sensor_config: %s\n",
                                 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_interpret_load_sensor_config (state_data.interpret_ctx, NULL) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_PARSE)
                pstdout_fprintf (pstate,
                                 stderr,
                                 "sensor state config file parse error\n");
              else
                pstdout_fprintf (pstate,
                                 stderr,
                                 "ipmi_interpret_load_sensor_config: %s\n",
                                 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              goto cleanup;
            }
        }

      if (prog_data->args->interpret_oem_data 
	  || prog_data->args->ignore_unrecognized_events)
	{
	  unsigned int flags = 0;

	  if (prog_data->args->interpret_oem_data)
	    flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;

	  if (prog_data->args->ignore_unrecognized_events)
	    flags |= IPMI_INTERPRET_FLAGS_IGNORE_UNRECOGNIZED_EVENTS;

	  if (ipmi_interpret_ctx_set_flags (state_data.interpret_ctx, flags) < 0)
	    {
	      pstdout_fprintf (pstate,
			       stderr,
			       "ipmi_interpret_ctx_set_flags: %s\n",
			       ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
	      goto cleanup;
	    }
	}
    }

  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
  ipmi_sensor_read_ctx_destroy (state_data.sensor_read_ctx);
  ipmi_interpret_ctx_destroy (state_data.interpret_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_sensors_prog_data_t prog_data;
  struct ipmi_sensors_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_sensors_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_sensors_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  /* Special case, just output list, don't do anything else */
  if (prog_data.args->list_sensor_types)
    {
      if (list_sensor_types () < 0)
	return (EXIT_FAILURE);
      
      return (EXIT_SUCCESS);
    }
  
  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
				    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->common_args.quiet_cache = 1;

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _ipmi_sensors,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
