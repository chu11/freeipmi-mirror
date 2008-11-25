/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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

#include "ipmi-sensors.h"
#include "ipmi-sensors-argp.h"
#include "ipmi-sensors-reading.h"
#include "ipmi-sensors-simple-display.h"
#include "ipmi-sensors-verbose-display.h"
#include "ipmi-sensors-very-verbose-display.h"
#include "ipmi-sensors-util.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-fiid-wrappers.h"
#include "tool-hostrange-common.h"
#include "tool-sensor-common.h"
#include "tool-sdr-cache-common.h"

static int 
_sdr_repository_info (ipmi_sensors_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val, val1, val2;
  char str[512];
  time_t t;
  struct tm tmp;
  int rv = -1;

  assert(state_data);
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sdr_repository_info_rs);
  
  if (ipmi_cmd_get_sdr_repository_info (state_data->ipmi_ctx, obj_cmd_rs) != 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_sdr_repository_info: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
 
  _FIID_OBJ_GET (obj_cmd_rs, "sdr_version_major", &val1);
  _FIID_OBJ_GET (obj_cmd_rs, "sdr_version_minor", &val2);
  pstdout_printf (state_data->pstate, 
                  "SDR version:                     %u.%u\n", 
                  val1, 
                  val2);

  _FIID_OBJ_GET (obj_cmd_rs, "record_count", &val);
  pstdout_printf (state_data->pstate, 
                  "SDR record count:                %u\n", 
                  val);

  _FIID_OBJ_GET (obj_cmd_rs, "free_space", &val); 
  pstdout_printf (state_data->pstate, 
                  "Free space remaining:            %u bytes\n", 
                  val);

  _FIID_OBJ_GET (obj_cmd_rs, "most_recent_addition_timestamp", &val);
  t = val;
  localtime_r (&t, &tmp);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", &tmp);
  pstdout_printf (state_data->pstate, 
                  "Most recent addition timestamp:  %s\n", 
                  str);
  
  _FIID_OBJ_GET (obj_cmd_rs, "most_recent_erase_timestamp", &val);
  t = val;
  localtime_r (&t, &tmp);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", &tmp);
  pstdout_printf (state_data->pstate, 
                  "Most recent erase timestamp:     %s\n", 
                  str);
  
  _FIID_OBJ_GET (obj_cmd_rs, "get_sdr_repository_allocation_info_command_supported", &val);
  pstdout_printf (state_data->pstate, 
                  "Get SDR Repository Allocation Information Command supported:         %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "reserve_sdr_repository_command_supported", &val);
  pstdout_printf (state_data->pstate, 
                  "Reserve SDR Repository Command supported:                            %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "partial_add_sdr_command_supported", &val);
  pstdout_printf (state_data->pstate, 
                  "Partial Add SDR Command supported:                                   %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "delete_sdr_command_supported", &val);
  pstdout_printf (state_data->pstate, 
                  "Delete SDR Command supported:                                        %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "modal_non_modal_sdr_repository_update_operation_supported", &val);
  pstdout_printf (state_data->pstate, 
                  "Modal/non-modal SDR Repository Update operation supported:           ");
  switch (val)
    {
    case IPMI_SDR_MODAL_NON_MODAL_REPOSITORY_UPDATE_OP_UNSPECIFIED:
      pstdout_printf (state_data->pstate, "Unspecified\n");
      break;
    case IPMI_SDR_NON_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED:
      pstdout_printf (state_data->pstate, "Non-Modal\n");
      break;
    case IPMI_SDR_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED:
      pstdout_printf (state_data->pstate, "Modal\n");
      break;
    case IPMI_SDR_MODAL_NON_MODAL_REPOSITORY_UPDATE_OP_SUPPORTED:
      pstdout_printf (state_data->pstate, "Both\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "Unknown\n");
    }

  _FIID_OBJ_GET (obj_cmd_rs, "overflow_flag", &val);
  pstdout_printf (state_data->pstate, 
                  "SDR could not be written due to lack of space in the SDR Repository: %s\n", 
                  (val ? "Yes" : "No"));
  
  rv = 0;
cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static int
_flush_cache (ipmi_sensors_state_data_t *state_data)
{
  assert(state_data);

  if (sdr_cache_flush_cache(state_data->ipmi_sdr_cache_ctx,
                            state_data->pstate,
                            state_data->prog_data->args->sdr.quiet_cache,
                            state_data->hostname,
                            state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return -1;
  
  return 0;
}

static int 
_display_group (ipmi_sensors_state_data_t *state_data)
{
  int i = 0;
  char *group = NULL;
  
  assert(state_data);

  for (i = 0; ipmi_sensor_types[i]; i++)
    {
      if (!(group = strdupa (ipmi_sensor_types[i])))
        {
          pstdout_fprintf (state_data->pstate, 
                           stderr, 
                           "strdupa: %s\n", 
                           strerror(errno));
          return (-1);
        }
      str_replace_char (group, ' ', '_');
      str_replace_char (group, '/', '_');
      pstdout_printf (state_data->pstate, "%s\n", group);
    }
  if (!(group = strdupa (ipmi_oem_sensor_type)))
    {
      pstdout_fprintf (state_data->pstate, 
                       stderr, 
                       "strdupa: %s\n", 
                       strerror(errno));
      return (-1);
    }
  str_replace_char (group, ' ', '_');
  str_replace_char (group, '/', '_');
  pstdout_printf (state_data->pstate, "%s\n", group);
  
  return 0;
}

static int
_sensors_group_specified(ipmi_sensors_state_data_t *state_data,
                         uint8_t *sdr_record,
                         unsigned int sdr_record_len)
{
  const char *sdr_group_name = NULL;
  uint8_t record_type;
  uint8_t sensor_type;
  int i;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(state_data->prog_data->args->groups_wanted);

  if (sdr_cache_get_record_id_and_type(state_data->pstate,
                                       sdr_record,
                                       sdr_record_len,
                                       NULL,
                                       &record_type) < 0)
    return -1;
  
  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_RECORD
      || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      if (sdr_cache_get_sensor_type (state_data->pstate,
                                     sdr_record,
                                     sdr_record_len,
                                     &sensor_type) < 0)
        return -1;

      sdr_group_name = sensor_group (sensor_type);
    }

  if (sdr_group_name)
    {
      char sdr_group_name_subst[IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH];

      strcpy(sdr_group_name_subst, sdr_group_name);
      str_replace_char (sdr_group_name_subst, ' ', '_');
      str_replace_char (sdr_group_name_subst, '/', '_');
      
      for (i = 0; i < state_data->prog_data->args->groups_length; i++)
        {
          if ((strcasecmp (sdr_group_name, 
                           state_data->prog_data->args->groups[i]) == 0)
              || (strcasecmp (sdr_group_name_subst, 
                              state_data->prog_data->args->groups[i]) == 0))
            return 1;
        }
    }

  return 0;
}

static int
_output_sensor (ipmi_sensors_state_data_t *state_data,
                uint8_t *sdr_record,
                unsigned int sdr_record_len)
{
  double *reading = NULL;
  char **event_message_list = NULL;
  unsigned int event_message_list_len = 0;
  int verbose_count;
  int ret, rv = -1;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
          
  verbose_count = state_data->prog_data->args->verbose_count;

  if ((ret = sensor_reading(state_data,
                            sdr_record,
                            sdr_record_len,
                            &reading,
                            &event_message_list,
                            &event_message_list_len)) < 0)
    goto cleanup;
          
  switch (verbose_count)
    {
    case 0:
      rv = ipmi_sensors_display_simple (state_data, 
                                        sdr_record,
                                        sdr_record_len,
                                        reading,
                                        event_message_list,
                                        event_message_list_len);
      break;
    case 1:
      rv = ipmi_sensors_display_verbose (state_data, 
                                         sdr_record,
                                         sdr_record_len,
                                         reading,
                                         event_message_list,
                                         event_message_list_len);
      break;
    case 2:
    default:
      rv = ipmi_sensors_display_very_verbose (state_data, 
                                              sdr_record,
                                              sdr_record_len,
                                              reading,
                                              event_message_list,
                                              event_message_list_len);
      break;
    }
  
 cleanup:
  if (reading)
    free(reading);
  if (event_message_list)
    {
      int j;
      for (j = 0; j < event_message_list_len; j++)
        free(event_message_list[j]);
      free(event_message_list);
    }
  return rv;
}

static int 
_display_sensors (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args = NULL;
  int i;
  
  assert(state_data);

  args = state_data->prog_data->args;

  if (args->sensors_wanted)
    {
      for (i = 0; i < state_data->prog_data->args->sensors_length; i++)
        {
          uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
          int sdr_record_len = 0;

          if (ipmi_sdr_cache_search_record_id(state_data->ipmi_sdr_cache_ctx,
                                              state_data->prog_data->args->sensors[i]) < 0)
            {
              if (ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx) == IPMI_SDR_CACHE_CTX_ERR_NOT_FOUND)
                {
                  pstdout_printf(state_data->pstate,
                                 "Sensor Record ID '%d' not found\n",
                                 state_data->prog_data->args->sensors[i]);
                  continue;
                }
              else
                {
                  pstdout_fprintf(state_data->pstate,
                                  stderr,
                                  "ipmi_sdr_cache_search_record_id: %s\n",
                                  ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
                  return -1;
                }
            }
          
          if ((sdr_record_len = ipmi_sdr_cache_record_read(state_data->ipmi_sdr_cache_ctx,
                                                           sdr_record,
                                                           IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              pstdout_fprintf(state_data->pstate,
                              stderr,
                              "ipmi_sdr_cache_record_read: %s\n",
                              ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
              return -1;
            }
          
          /* Shouldn't be possible */
          if (!sdr_record_len)
            continue;

          if (_output_sensor (state_data,
                              sdr_record,
                              sdr_record_len) < 0)
            return -1;
        }
    }
  else
    {
      uint16_t record_count;

      if (ipmi_sdr_cache_record_count(state_data->ipmi_sdr_cache_ctx, &record_count) < 0)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_sdr_cache_record_count: %s\n",
                          ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
          return -1;
        }
      
      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next(state_data->ipmi_sdr_cache_ctx))
        {
          uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
          int sdr_record_len = 0;
          int ret;
          
          if ((sdr_record_len = ipmi_sdr_cache_record_read(state_data->ipmi_sdr_cache_ctx,
                                                           sdr_record,
                                                           IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              pstdout_fprintf(state_data->pstate,
                              stderr,
                              "ipmi_sdr_cache_record_read: %s\n",
                              ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
              return -1;
            }
          
          /* Shouldn't be possible */
          if (!sdr_record_len)
            continue;
          
          if (args->groups_wanted)
            {
              if ((ret = _sensors_group_specified (state_data,
                                                   sdr_record,
                                                   sdr_record_len)) < 0)
                return -1;
            }
          else
            ret = 1;            /* display everything */
          
          if (ret)
            {
              if (_output_sensor (state_data,
                                  sdr_record,
                                  sdr_record_len) < 0)
                return -1;
            }
        }
    }
  
  return 0;
}

int 
run_cmd_args (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args;

  assert(state_data);

  args = state_data->prog_data->args;
  
  if (args->sdr_info)
    return _sdr_repository_info (state_data);
  
  if (args->sdr.flush_cache)
    return _flush_cache (state_data);
  
  if (args->list_groups)
    return _display_group (state_data);
  
  if (sdr_cache_create_and_load (state_data->ipmi_sdr_cache_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 args->sdr.quiet_cache,
                                 args->sdr.sdr_cache_recreate,
                                 state_data->hostname,
                                 args->sdr.sdr_cache_directory) < 0)
    return -1;
  
  if (_display_sensors (state_data) < 0)
    return -1;

  return 0;
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
  memset(&state_data, '\0', sizeof(ipmi_sensors_state_data_t));
  
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* Special case, just flush, don't do an IPMI connection */
  /* Special case, just list groups, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache
      && !prog_data->args->list_groups)
    {
      if (!(state_data.ipmi_ctx = ipmi_open(prog_data->progname,
                                            hostname,
                                            &(prog_data->args->common),
                                            errmsg,
                                            IPMI_OPEN_ERRMSGLEN)))
        {
          pstdout_fprintf(pstate,
                          stderr,
                          "%s\n",
                          errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }      
 
  if (!(state_data.ipmi_sdr_cache_ctx = ipmi_sdr_cache_ctx_create()))
    {
      pstdout_perror (pstate, "ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags(state_data.ipmi_sdr_cache_ctx,
                                       IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data.ipmi_sdr_cache_ctx)));

      if (hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix(state_data.ipmi_sdr_cache_ctx,
                                                  hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
                             ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data.ipmi_sdr_cache_ctx)));
        }
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = 0;
 cleanup:
  if (state_data.ipmi_sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy(state_data.ipmi_sdr_cache_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return exit_code;
}

int 
main (int argc, char **argv)
{
  ipmi_sensors_prog_data_t prog_data;
  struct ipmi_sensors_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;
  
  ipmi_disable_coredump();
  
  memset(&prog_data, '\0', sizeof(ipmi_sensors_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_sensors_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup(&(prog_data.args->common.hostname),
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

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
                           _ipmi_sensors,
                           &prog_data)) < 0)
    {
      fprintf(stderr,
              "pstdout_launch: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
