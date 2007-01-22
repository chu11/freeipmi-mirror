/*
ipmi-sensors.c: IPMI Sensors utility.
Copyright (C) 2006 FreeIPMI Core Team

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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <sys/resource.h>
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
#include <argp.h>
#include <assert.h>

#include "argp-common.h"
#include "ipmi-common.h"
#include "ipmi-sensor-api.h"
#include "ipmi-sdr-api.h"
#include "ipmi-sensors.h"
#include "ipmi-sensors-argp.h"
#include "ipmi-sensors-utils.h"
#include "sensors-simple-display.h"
#include "sensors-verbose-display.h"
#include "sensors-very-verbose-display.h"

#include "freeipmi-portability.h"

int 
display_sdr_repository_info (ipmi_sensors_state_data_t *state_data)
{
  sdr_repository_info_t sdr_repo_info;
  char str[512];
  time_t t;
  struct tm *tmp;

  assert(state_data);
  
  memset (&sdr_repo_info, 0, sizeof (sdr_repository_info_t));
  
  if (get_sdr_repository_info (state_data->dev, &sdr_repo_info) < 0)
    {
      fprintf (stderr, "%s: unable to get SDR Repository information\n", 
	       program_invocation_short_name);
      return (-1);
    }
  
  printf ("SDR version:                     %d.%d\n", 
	  sdr_repo_info.sdr_version_major, 
	  sdr_repo_info.sdr_version_minor);
  printf ("SDR record count:                %d\n", 
	  sdr_repo_info.record_count);
  printf ("Free space remaining:            %d bytes\n", 
	  sdr_repo_info.free_space);
  
  t = sdr_repo_info.most_recent_addition_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  printf ("Most recent addition timestamp:  %s\n", str);
  
  t = sdr_repo_info.most_recent_erase_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  printf ("Most recent erase timestamp:     %s\n", str);
  
  printf ("Get SDR Repository Allocation Information Command supported:         %s\n", 
	  (sdr_repo_info.get_sdr_repository_allocation_info_command_supported ? "Yes" : "No"));
  printf ("Reserve SDR Repository Command supported:                            %s\n", 
	  (sdr_repo_info.reserve_sdr_repository_command_supported ? "Yes" : "No"));
  printf ("Partial Add SDR Command supported:                                   %s\n", 
	  (sdr_repo_info.partial_add_sdr_command_supported ? "Yes" : "No"));
  printf ("Delete SDR Command supported:                                        %s\n", 
	  (sdr_repo_info.delete_sdr_command_supported ? "Yes" : "No"));
  printf ("Modal/non-modal SDR Repository Update operation supported:           ");
  switch (sdr_repo_info.modal_non_modal_sdr_repository_update_operation_supported)
    {
    case 0:
      printf ("Unspecified\n");
      break;
    case 1:
      printf ("Non-Modal\n");
      break;
    case 2:
      printf ("Modal\n");
      break;
    case 3:
      printf ("Both\n");
      break;
    default:
      printf ("Unknown\n");
    }
  printf ("SDR could not be written due to lack of space in the SDR Repository: %s\n", 
	  (sdr_repo_info.overflow_flag ? "Yes" : "No"));
  
  return (0);
}

int 
display_group_list (ipmi_sensors_state_data_t *state_data)
{
  int i = 0;
  char *group = NULL;
  
  assert(state_data);

  for (i = 0; ipmi_sensor_types[i]; i++)
    {
      if (!(group = strdupa (ipmi_sensor_types[i])))
        {
          fprintf (stderr, "strdupa: %s\n", strerror(errno));
          return (-1);
        }
      str_replace_chr (group, ' ', '_');
      printf ("%s\n", group);
    }
  if (!(group = strdupa (ipmi_oem_sensor_type)))
    {
      fprintf (stderr, "strdupa: %s\n", strerror(errno));
      return (-1);
    }
  str_replace_chr (group, ' ', '_');
  printf ("%s\n", group);
  
  return 0;
}

void
cleanup_sdr_cache (ipmi_sensors_state_data_t *state_data)
{
  assert (state_data);

  memset(&(state_data->sdr_info), '\0', sizeof(sdr_repository_info_t));
  if (state_data->sdr_record_list)
    {
      free(state_data->sdr_record_list);
      state_data->sdr_record_list = NULL;
      state_data->sdr_record_count = 0;
    }
}

int 
init_sdr_cache (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args = NULL;
  char *sdr_cache_filename = NULL;
  FILE *fp = NULL;
  int rv = -1;
  
  assert(state_data);

  args = state_data->prog_data->args;

  if ((sdr_cache_filename = get_sdr_cache_filename (args->common.host, 
						    args->sdr_cache_dir)) == NULL)
    {
      perror ("error: get_sdr_cache_filename (): ");
      return (-1);
    }

  if ((fp = fopen (sdr_cache_filename, "r")))
    {
      sdr_repository_info_t l_sdr_info;

      if (load_sdr_cache (fp, 
                          &(state_data->sdr_info),
                          &(state_data->sdr_record_list), 
                          &(state_data->sdr_record_count)) < 0)
        goto cleanup;

      memset (&l_sdr_info, 0, sizeof (sdr_repository_info_t));
      if (get_sdr_repository_info (state_data->dev, &l_sdr_info) == -1)
        goto cleanup;
	  
      if (l_sdr_info.most_recent_addition_timestamp == state_data->sdr_info.most_recent_addition_timestamp && l_sdr_info.most_recent_erase_timestamp == state_data->sdr_info.most_recent_erase_timestamp)
        {
          rv = 0;
          goto cleanup;
        }

      fclose(fp);
      fp = NULL;
    }
  
  if ((fp = fopen (sdr_cache_filename, "w")))
    {
      int rc;
#ifndef NDEBUG
      rc = create_sdr_cache (state_data->dev, 
                             fp, 
                             (args->quiet_cache_wanted) ? 0 : 1,
                             state_data->prog_data->debug_flags);
#else  /* NDEBUG */
      rc = create_sdr_cache (state_data->dev, 
                             fp, 
                             (args->quiet_cache_wanted) ? 0 : 1,
                             0);
#endif /* NDEBUG */
      if (rc < 0)
        goto cleanup;
      fclose (fp);
      fp = NULL;
    }
  
  if ((fp = fopen (sdr_cache_filename, "r")))
    {
      if (load_sdr_cache (fp,
                          &(state_data->sdr_info), 
                          &(state_data->sdr_record_list), 
                          &(state_data->sdr_record_count)) < 0)
        goto cleanup;

      fclose (fp);
      fp = NULL;
    }
  
  rv = 0;
 cleanup:
  if (fp)
    fclose (fp);
  if (sdr_cache_filename)
    free(sdr_cache_filename);
  if (rv < 0)
    cleanup_sdr_cache(state_data);
  return (rv);
}

int 
display_group_sensors (ipmi_sensors_state_data_t *state_data)
{
  int i;
  sdr_record_t *sdr_record;
  sensor_reading_t _sensor_reading;
  sensor_reading_t *sensor_reading;
  char *group;
  int verbose_count;

  assert(state_data);
  assert(state_data->prog_data->args->group);

  group = state_data->prog_data->args->group;
  verbose_count = state_data->prog_data->args->verbose_count;

  sensor_reading = &_sensor_reading;
  
  for (i = 0; i < state_data->sdr_record_count; i++)
    {
      sdr_record = state_data->sdr_record_list + i;
      
      if (sensors_group_cmp (sdr_record, group) == 0)
	{
          memset (&_sensor_reading, 0, sizeof (sensor_reading_t));
          
          if (get_sensor_reading(state_data->dev,
                                 sdr_record,
                                 &_sensor_reading) < 0)
            sensor_reading = NULL;
          else
            sensor_reading = &_sensor_reading;

	  switch (verbose_count)
	    {
	    case 0:
	      if (sensors_display_simple (sdr_record, sensor_reading) < 0)
                return (-1);
	      break;
	    case 1:
	      if (sensors_display_verbose (sdr_record, sensor_reading) < 0)
                return (-1);
	      break;
	    case 2:
	    default:
	      if (sensors_display_very_verbose (sdr_record, sensor_reading) < 0)
                return (-1);
	    }
	}
    }
  
  return 0;
}

int 
display_sensor_list (ipmi_sensors_state_data_t *state_data)
{
  int i;
  sdr_record_t *sdr_record;
  sensor_reading_t _sensor_reading;
  sensor_reading_t *sensor_reading;
  unsigned int *sensors_list;
  unsigned int sensors_list_length;
  int verbose_count;

  assert(state_data);
  assert(state_data->prog_data->args->sensors_list_wanted);
  assert(state_data->prog_data->args->sensors_list);
  assert(state_data->prog_data->args->sensors_list_length);

  sensors_list = state_data->prog_data->args->sensors_list;
  sensors_list_length = state_data->prog_data->args->sensors_list_length;
  verbose_count = state_data->prog_data->args->verbose_count;

  sensor_reading = &_sensor_reading;
  
  for (i = 0; i < state_data->sdr_record_count; i++)
    {
      sdr_record = state_data->sdr_record_list + i;
      
      if (sensors_list_cmp (sdr_record, sensors_list, sensors_list_length) == 0)
	{
          memset (&_sensor_reading, 0, sizeof (sensor_reading_t));

          if (get_sensor_reading(state_data->dev,
                                 sdr_record,
                                 &_sensor_reading) < 0)
            sensor_reading = NULL;
          else
            sensor_reading = &_sensor_reading;

	  switch (verbose_count)
	    {
	    case 0:
	      if (sensors_display_simple (sdr_record, sensor_reading) < 0)
                return (-1);
	      break;
	    case 1:
	      if (sensors_display_verbose (sdr_record, sensor_reading) < 0)
                return (-1);
	      break;
	    case 2:
	    default:
	      if (sensors_display_very_verbose (sdr_record, sensor_reading) < 0)
                return (-1);
	    }
	}
    }
  
  return 0;
}

int 
display_sensors (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args = NULL;
  
  assert(state_data);

  args = state_data->prog_data->args;

  if (args->group)
    {
      if (display_group_sensors (state_data) < 0)
        return (-1);
    }

  if (args->sensors_list_wanted)
    {
      if (display_sensor_list (state_data) < 0)
        return (-1);
    }
  
  if (!args->group && !args->sensors_list_wanted)
    {
      int i;
      sdr_record_t *sdr_record;
      sensor_reading_t _sensor_reading;
      sensor_reading_t *sensor_reading;
      
      sensor_reading = &_sensor_reading;
      
      for (i = 0; i < state_data->sdr_record_count; i++)
	{
	  sdr_record = state_data->sdr_record_list + i;

	  memset (&_sensor_reading, 0, sizeof (sensor_reading_t));

          if (get_sensor_reading(state_data->dev,
                                 sdr_record,
                                 &_sensor_reading) < 0)
            sensor_reading = NULL;
          else
            sensor_reading = &_sensor_reading;

	  switch (args->verbose_count)
	    {
	    case 0:
	      if (sensors_display_simple (sdr_record, sensor_reading) < 0)
                return (-1);
	      break;
	    case 1:
	      if (sensors_display_verbose (sdr_record, sensor_reading) < 0)
                return (-1);
	      break;
	    case 2:
	    default:
	      if (sensors_display_very_verbose (sdr_record, sensor_reading) < 0)
                return (-1);
	    }
	}
    }
  
  return 0;
}

int 
run_cmd_args (ipmi_sensors_state_data_t *state_data)
{
  struct ipmi_sensors_arguments *args;
  int rv = -1;

  assert(state_data);

  args = state_data->prog_data->args;
  
  if (args->sdr_info_wanted)
    return display_sdr_repository_info (state_data);
  
  if (args->flush_cache_wanted)
    {
      int retval;
      if (!args->quiet_cache_wanted)
        printf ("flushing cache... ");
      retval = flush_sdr_cache_file (args->common.host, args->sdr_cache_dir);
      if (!args->quiet_cache_wanted)
        printf ("%s\n", (retval ? "FAILED" : "done"));
      return retval;
    }
  
  if (args->list_groups_wanted)
    return display_group_list (state_data);
  
  if (init_sdr_cache (state_data) < 0)
    {
      fprintf (stderr, "%s: sdr cache initialization failed\n", 
	       program_invocation_short_name);
      goto cleanup;
    }
  
  if (display_sensors (state_data) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  cleanup_sdr_cache (state_data);
  return (rv);
}

static void
_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

static int
_ipmi_sensors (void *arg)
{
  ipmi_sensors_state_data_t state_data;
  ipmi_sensors_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  int exit_code = -1;

  prog_data = (ipmi_sensors_prog_data_t *)arg;
  
  if (prog_data->args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
				       prog_data->args->common.host,
                                       prog_data->args->common.username,
                                       prog_data->args->common.password,
                                       prog_data->args->common.authentication_type, 
                                       prog_data->args->common.privilege_level,
                                       prog_data->args->common.session_timeout, 
                                       prog_data->args->common.retry_timeout, 
				       prog_data->debug_flags)))
	{
	  perror ("ipmi_open_outofband()");
	  exit_code = EXIT_FAILURE;
          goto cleanup;
	}
    }
  else
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", prog_data->progname);
	  exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (prog_data->args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI, 
					prog_data->args->common.disable_auto_probe, 
                                        prog_data->args->common.driver_address, 
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device, 
                                        prog_data->debug_flags)))
	    {
              if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS,
                                            prog_data->args->common.disable_auto_probe,
                                            prog_data->args->common.driver_address,
                                            prog_data->args->common.register_spacing,
                                            prog_data->args->common.driver_device,
                                            prog_data->debug_flags)))
                {
                  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
                                                prog_data->args->common.disable_auto_probe,
                                                prog_data->args->common.driver_address,
                                                prog_data->args->common.register_spacing,
                                                prog_data->args->common.driver_device,
                                                prog_data->debug_flags)))
                    {
                      perror ("ipmi_open_inband()");
                      exit_code = EXIT_FAILURE;
                      goto cleanup;
                    }
                }
            }
	}
      else
	{
	  if (!(dev = ipmi_open_inband (prog_data->args->common.driver_type,
					prog_data->args->common.disable_auto_probe,
					prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
	    {
	      perror ("ipmi_open_inband()");
              exit_code = EXIT_FAILURE;
              goto cleanup;
	    }
	}
    }

  memset(&state_data, '\0', sizeof(ipmi_sensors_state_data_t));
  state_data.dev = dev;
  state_data.prog_data = prog_data;

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = 0;
 cleanup:
  if (dev)
    ipmi_close_device (dev);
  return exit_code;
}

int 
main (int argc, char **argv)
{
  ipmi_sensors_prog_data_t prog_data;
  struct ipmi_sensors_arguments cmd_args;
  int exit_code;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */
  
  _disable_coredump();
  
  prog_data.progname = argv[0];
  ipmi_sensors_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;
   
#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
  
#ifndef NDEBUG
  if (prog_data.args->common.debug)
    prog_data.debug_flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */

  if (setup_sdr_cache_directory () == -1)
    {
      fprintf (stderr, "%s: sdr cache directory setup failed\n", 
	       program_invocation_short_name);
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = _ipmi_sensors(&prog_data);
 cleanup:
  return (exit_code);
}
