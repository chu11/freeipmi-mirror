/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
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
#include "ipmi-sensors-output-common.h"
#include "ipmi-sensors-util.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_SENSORS_OEM_MESSAGE_LENGTH 1024

static int
_sdr_repository_info (ipmi_sensors_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t major, minor;
  uint16_t record_count, free_space;
  uint64_t val;
  char timestr[512];
  time_t t;
  struct tm tmp;
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

  t = val;
  localtime_r (&t, &tmp);
  strftime (timestr, sizeof (timestr), "%m/%d/%Y - %H:%M:%S", &tmp);
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

  t = val;
  localtime_r (&t, &tmp);
  strftime (timestr, sizeof (timestr), "%m/%d/%Y - %H:%M:%S", &tmp);
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

  if (val == IPMI_SDR_MODAL_NON_MODAL_REPOSITORY_UPDATE_OP_UNSPECIFIED)
    str = "unspecified";
  else if (val == IPMI_SDR_NON_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED)
    str = "non-Modal supported";
  else if (val == IPMI_SDR_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED)
    str = "modal supported";
  else if (val == IPMI_SDR_MODAL_NON_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED)
    str = "both supported";
  else
    str = "unknown";

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
_flush_cache (ipmi_sensors_state_data_t *state_data)
{
  assert (state_data);

  if (sdr_cache_flush_cache (state_data->sdr_cache_ctx,
                             state_data->pstate,
                             state_data->prog_data->args->sdr.quiet_cache,
                             state_data->hostname,
                             state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return (-1);

  return (0);
}

static int
_display_group (ipmi_sensors_state_data_t *state_data)
{
  unsigned int i = 0;

  assert (state_data);

  for (i = IPMI_SENSOR_TYPE_TEMPERATURE; i <= IPMI_SENSOR_TYPE_FRU_STATE; i++)
    {
      if (display_sensor_group_cmdline (state_data->pstate, i) < 0)
        return (-1);
    }
  
  if (display_string_cmdline (state_data->pstate,
                              ipmi_oem_sensor_type) < 0)
    return (-1);

  return (0);
}

static int
_output_setup (ipmi_sensors_state_data_t *state_data)
{
  int rv = -1;

  assert (state_data);

  switch (state_data->prog_data->args->verbose_count)
    {
    case 0:
      rv = ipmi_sensors_simple_output_setup (state_data);
      break;
    default:
      rv = 0;
      break;
    }

  return (rv);
}

static int
_output_sensor (ipmi_sensors_state_data_t *state_data,
                const void *sdr_record,
                unsigned int sdr_record_len)
{
  double *sensor_reading = NULL;
  uint16_t sensor_event_bitmask = 0;
  uint8_t event_reading_type_code;
  int event_reading_type_code_class;
  char **event_message_list = NULL;
  unsigned int event_message_list_len = 0;
  int rv = -1;

  assert (state_data);
  assert (sdr_record);
  assert (sdr_record_len);

  if (ipmi_sensor_read (state_data->sensor_read_ctx,
                        sdr_record,
                        sdr_record_len,
                        &sensor_reading,
                        &sensor_event_bitmask) <= 0)
    {
      int errnum = ipmi_sensor_read_ctx_errnum (state_data->sensor_read_ctx);

      if (errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_ANALOG
          || errnum == IPMI_SENSOR_READ_ERR_SENSOR_NON_LINEAR)
        {
          if (state_data->prog_data->args->common.debug)
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
          if (state_data->prog_data->args->common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Sensor reading/event bitmask not available: %s\n",
                             ipmi_sensor_read_ctx_errormsg (state_data->sensor_read_ctx));

          if (state_data->prog_data->args->ignore_not_available_sensors)
            {
              rv = 0;
              goto cleanup;
            }

          if (get_msg_message_list (state_data,
                                    &event_message_list,
                                    &event_message_list_len,
                                    IPMI_SENSORS_NA_STRING_OUTPUT) < 0)
            goto cleanup;

          goto output;
        }

      if (errnum == IPMI_SENSOR_READ_ERR_SENSOR_READING_CANNOT_BE_OBTAINED
          || errnum == IPMI_SENSOR_READ_ERR_NODE_BUSY
          || errnum == IPMI_SENSOR_READ_ERR_INVALID_SDR_RECORD_TYPE)
        {
          if (state_data->prog_data->args->common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Sensor reading/event_bitmask retrieval error: %s\n",
                             ipmi_sensor_read_ctx_errormsg (state_data->sensor_read_ctx));

          /* output unknown by not setting/creating an
           * event_message_list if sensor can't be read/obtained.  For
           * an invalid SDR type, fall through to output.
           * detailed-output will output SDR information if it
           * pleases, simple-output will ignore this SDR type.
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

  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_parse_ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      goto cleanup;
    }

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      if (get_threshold_message_list (state_data,
                                      &event_message_list,
                                      &event_message_list_len,
                                      sensor_event_bitmask,
                                      IPMI_SENSORS_NO_EVENT_STRING) < 0)
        goto cleanup;
    }
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE)
    {
      if (get_generic_event_message_list (state_data,
                                          &event_message_list,
                                          &event_message_list_len,
                                          event_reading_type_code,
                                          sensor_event_bitmask,
                                          IPMI_SENSORS_NO_EVENT_STRING) < 0)
        goto cleanup;
    }
  else if (event_reading_type_code_class ==  IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
    {
      uint8_t sensor_type;

      if (ipmi_sdr_parse_sensor_type (state_data->sdr_parse_ctx,
                                      sdr_record,
                                      sdr_record_len,
                                      &sensor_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_sensor_type: %s\n",
                           ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
          goto cleanup;
        }

      if (get_sensor_specific_event_message_list (state_data,
                                                  &event_message_list,
                                                  &event_message_list_len,
                                                  sensor_type,
                                                  sensor_event_bitmask,
                                                  IPMI_SENSORS_NO_EVENT_STRING) < 0)
        goto cleanup;
    }
  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM
           && state_data->prog_data->args->interpret_oem_data
           && state_data->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
           && (state_data->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
               || state_data->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
           && event_reading_type_code == 0x70) /* OEM */
    {
      if (get_generic_event_message_list (state_data,
                                          &event_message_list,
                                          &event_message_list_len,
                                          event_reading_type_code,
                                          sensor_event_bitmask,
                                          IPMI_SENSORS_NO_EVENT_STRING) < 0)
        goto cleanup;
    }
  /* OEM Interpretation
   *
   * Supermicro X8DTH
   */
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM
           && state_data->prog_data->args->interpret_oem_data
           && (state_data->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
	       || state_data->manufacturer_id ==  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
	   && state_data->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTH
	   && event_reading_type_code == 0x70) /* OEM */
    {
      char event_buf[IPMI_SENSORS_OEM_MESSAGE_LENGTH + 1];
      char *event_message = NULL;
      char **tmp_event_message_list = NULL;
      uint8_t sensor_type;
      int ret;

      if (ipmi_sdr_parse_sensor_type (state_data->sdr_parse_ctx,
                                      sdr_record,
                                      sdr_record_len,
                                      &sensor_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_sensor_type: %s\n",
                           ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
          goto cleanup;
        }

      memset (event_buf, '\0', IPMI_SENSORS_OEM_MESSAGE_LENGTH + 1);

      if ((ret = ipmi_get_oem_sensor_event_bitmask_message (state_data->manufacturer_id,
							    state_data->product_id,
							    event_reading_type_code,
							    sensor_type,
							    sensor_event_bitmask,
							    event_buf,
							    IPMI_SENSORS_OEM_MESSAGE_LENGTH)) < 0)
	goto oem_normal_output;
      
      if (!ret)
	goto oem_normal_output;

      if (!(event_message = strdup (event_buf)))
	{
	  pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
	}

      if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * 2)))
        {
          pstdout_perror (state_data->pstate, "malloc");
          free (event_message);
          goto cleanup;
        }

      tmp_event_message_list[0] = event_message;
      tmp_event_message_list[1] = NULL;

      event_message_list = tmp_event_message_list;
      event_message_list_len = 1;
    }
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM)
    {
    oem_normal_output:
      {
	char *event_message = NULL;
	char **tmp_event_message_list = NULL;
	
	if (asprintf (&event_message,
		      "OEM Event = %04Xh",
		      sensor_event_bitmask) < 0)
	  {
	    pstdout_perror (state_data->pstate, "asprintf");
	    goto cleanup;
	  }
	
	if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * 2)))
	  {
	    pstdout_perror (state_data->pstate, "malloc");
	    free (event_message);
	    goto cleanup;
	  }
	
	tmp_event_message_list[0] = event_message;
	tmp_event_message_list[1] = NULL;
	
	event_message_list = tmp_event_message_list;
	event_message_list_len = 1;
      }
    }
  /* else unknown event_reading_type_code, output "unknown" ultimately */

 output:
  switch (state_data->prog_data->args->verbose_count)
    {
    case 0:
      rv = ipmi_sensors_simple_output (state_data,
                                       sdr_record,
                                       sdr_record_len,
                                       sensor_reading,
                                       event_message_list,
                                       event_message_list_len);
      break;
    case 1:
    case 2:
    default:
      rv = ipmi_sensors_detailed_output (state_data,
                                         sdr_record,
                                         sdr_record_len,
                                         sensor_reading,
                                         event_message_list,
                                         event_message_list_len);
      break;
    }

 cleanup:
  if (sensor_reading)
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
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
  int sdr_record_len = 0;
  unsigned int output_record_ids[MAX_SENSOR_RECORD_IDS];
  unsigned int output_record_ids_length = 0;
  unsigned int i;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->interpret_oem_data)
    {
      uint64_t val;

      if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_create: %s\n",
                           strerror (errno));
          goto cleanup;
        }
      
      if (ipmi_cmd_get_device_id (state_data->ipmi_ctx, obj_cmd_rs) < 0)
        {
          if (args->common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "ipmi_cmd_get_device_id: %s\n",
                             ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs, "manufacturer_id.id", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'manufacturer_id.id': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      state_data->manufacturer_id = val;

      if (FIID_OBJ_GET (obj_cmd_rs, "product_id", &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'product_id': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      state_data->product_id = val;
    }

  if (_output_setup (state_data) < 0)
    goto cleanup;

  memset (output_record_ids, '\0', sizeof (unsigned int) * MAX_SENSOR_RECORD_IDS);

  if (calculate_record_ids (state_data->pstate,
                            state_data->sdr_cache_ctx,
                            state_data->sdr_parse_ctx,
                            args->groups,
                            args->groups_length,
                            args->exclude_groups,
                            args->exclude_groups_length,
                            args->record_ids,
                            args->record_ids_length,
                            args->exclude_record_ids,
                            args->exclude_record_ids_length,
                            output_record_ids,
                            &output_record_ids_length) < 0)
    return (-1);

  for (i = 0; i < output_record_ids_length; i++)
    {
      if (ipmi_sdr_cache_search_record_id (state_data->sdr_cache_ctx,
                                           output_record_ids[i]) < 0)
        {
          /* at this point shouldn't have record id not found error */
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_cache_search_record_id: %s\n",
                           ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
          goto cleanup;
        }
    
      if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_cache_ctx,
                                                        sdr_record,
                                                        IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_cache_record_read: %s\n",
                           ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
          goto cleanup;
        }

      /* Shouldn't be possible */
      if (!sdr_record_len)
        continue;
      
      if (_output_sensor (state_data,
                          sdr_record,
                          sdr_record_len) < 0)
        goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
run_cmd_args (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->sdr_info)
    return (_sdr_repository_info (state_data));

  if (args->sdr.flush_cache)
    return (_flush_cache (state_data));

  if (args->list_groups)
    return (_display_group (state_data));

  if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 args->sdr.quiet_cache,
                                 args->sdr.sdr_cache_recreate,
                                 state_data->hostname,
                                 args->sdr.sdr_cache_directory) < 0)
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
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_sensors_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmi_sensors_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* Special case, just flush, don't do an IPMI connection */
  /* Special case, just list groups, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache
      && !prog_data->args->list_groups)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                             hostname,
                                             &(prog_data->args->common),
                                             errmsg,
                                             IPMI_OPEN_ERRMSGLEN)))
        {
          pstdout_fprintf (pstate,
                           stderr,
                           "%s\n",
                           errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (!(state_data.sdr_cache_ctx = ipmi_sdr_cache_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_strerror (ipmi_sdr_cache_ctx_errnum (state_data.sdr_cache_ctx)));

      if (hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix (state_data.sdr_cache_ctx,
                                                   hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
                             ipmi_sdr_cache_ctx_strerror (ipmi_sdr_cache_ctx_errnum (state_data.sdr_cache_ctx)));
        }
    }

  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_parse_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!prog_data->args->sdr.flush_cache
      && !prog_data->args->list_groups)
    {
      if (!(state_data.sensor_read_ctx = ipmi_sensor_read_ctx_create (state_data.ipmi_ctx)))
        {
          pstdout_perror (pstate, "ipmi_sensor_read_ctx_create()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (state_data.prog_data->args->bridge_sensors)
        {
          /* Don't error out, if this fails we can still continue */
          if (ipmi_sensor_read_ctx_set_flags (state_data.sensor_read_ctx,
                                              IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sensor_read_ctx_set_flags: %s\n",
                             ipmi_sensor_read_ctx_strerror (ipmi_sensor_read_ctx_errnum (state_data.sensor_read_ctx)));
        }
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy (state_data.sdr_cache_ctx);
  if (state_data.sdr_parse_ctx)
    ipmi_sdr_parse_ctx_destroy (state_data.sdr_parse_ctx);
  if (state_data.sensor_read_ctx)
    ipmi_sensor_read_ctx_destroy (state_data.sensor_read_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_sensors_prog_data_t prog_data;
  struct ipmi_sensors_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_sensors_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_sensors_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common.hostname),
                                    prog_data.args->hostrange.buffer_output,
                                    prog_data.args->hostrange.consolidate_output,
                                    prog_data.args->hostrange.fanout,
                                    prog_data.args->hostrange.eliminate,
                                    prog_data.args->hostrange.always_prefix)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->sdr.quiet_cache = 1;

  if ((rv = pstdout_launch (prog_data.args->common.hostname,
                            _ipmi_sensors,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
