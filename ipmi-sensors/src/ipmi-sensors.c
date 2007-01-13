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

#include "argp-common.h"
#include "ipmi-common.h"
#include "ipmi-sensor-api.h"
#include "ipmi-sdr-api.h"
#include "ipmi-sensors-argp.h"
#include "ipmi-sensors-utils.h"
#include "sensors-simple-display.h"
#include "sensors-verbose-display.h"
#include "sensors-very-verbose-display.h"

#include "freeipmi-portability.h"

sdr_repository_info_t sdr_info;
sdr_record_t *sdr_record_list = NULL;
int sdr_record_count = 0;

int 
display_sdr_repository_info (ipmi_device_t dev)
{
  sdr_repository_info_t sdr_repo_info;
  char str[512];
  time_t t;
  struct tm *tmp;
  
  memset (&sdr_repo_info, 0, sizeof (sdr_repository_info_t));
  
  if (get_sdr_repository_info (dev, &sdr_repo_info) != 0)
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
display_group_list ()
{
  int i = 0;
  char *group = NULL;
  
  for (i = 0; ipmi_sensor_types[i]; i++)
    {
      group = strdupa (ipmi_sensor_types[i]);
      str_replace_chr (group, ' ', '_');
      printf ("%s\n", group);
    }
  group = strdupa (ipmi_oem_sensor_type);
  str_replace_chr (group, ' ', '_');
  printf ("%s\n", group);
  
  return 0;
}

int 
init_sdr_cache (ipmi_device_t dev, struct arguments *args)
{
  char *sdr_cache_filename = NULL;
  FILE *fp = NULL;
  int rv = -1;
  
  if ((sdr_cache_filename = get_sdr_cache_filename (args->common.host, 
						    args->sdr_cache_dir)) == NULL)
    {
      perror ("error: get_sdr_cache_filename (): ");
      return (-1);
    }
  if ((fp = fopen (sdr_cache_filename, "r")))
    {
      rv = load_sdr_cache (fp, &sdr_info, &sdr_record_list, &sdr_record_count);
      fclose (fp);
      
      if (rv == 0)
	{
	  sdr_repository_info_t l_sdr_info;
	  
	  memset (&l_sdr_info, 0, sizeof (sdr_repository_info_t));
	  if (get_sdr_repository_info (dev, &l_sdr_info) == -1)
	    {
	      free (sdr_cache_filename);
	      free (sdr_record_list);
	      sdr_record_list = NULL;
	      sdr_record_count = 0;
	      return (-1);
	    }
	  
	  if (l_sdr_info.most_recent_addition_timestamp == sdr_info.most_recent_addition_timestamp && l_sdr_info.most_recent_erase_timestamp == sdr_info.most_recent_erase_timestamp)
	    {
	      free (sdr_cache_filename);
	      return 0;
	    }
	  
	  free (sdr_cache_filename);
	  free (sdr_record_list);
	  sdr_record_list = NULL;
	  sdr_record_count = 0;
	}
    }
  
  if ((fp = fopen (sdr_cache_filename, "w")))
    {
#ifndef NDEBUG
      rv = create_sdr_cache (dev, 
                             fp, 
                             (args->quiet_cache_wanted) ? 0 : 1,
                             args->common.debug);
#else  /* NDEBUG */
      rv = create_sdr_cache (dev, 
                             fp, 
                             (args->quiet_cache_wanted) ? 0 : 1,
                             0);
#endif /* NDEBUG */
      fclose (fp);
      if (rv)
	{
	  return (-1);
	}
    }
  
  if ((fp = fopen (sdr_cache_filename, "r")))
    {
      rv = load_sdr_cache (fp, &sdr_info, &sdr_record_list, &sdr_record_count);
      fclose (fp);
    }
  
  return rv;
}

int 
display_group_sensors (ipmi_device_t dev, 
		       char *group_name, 
		       int verbose)
{
  int i;
  sdr_record_t *sdr_record;
  sensor_reading_t _sensor_reading;
  sensor_reading_t *sensor_reading;
  
  sensor_reading = &_sensor_reading;
  
  for (i = 0; i < sdr_record_count; i++)
    {
      sdr_record = sdr_record_list + i;
      
      if (sensors_group_cmp (sdr_record, group_name) == 0)
	{
          memset (&_sensor_reading, 0, sizeof (sensor_reading_t));
          sensor_reading = (get_sensor_reading (dev, 
                                                sdr_record, 
                                                &_sensor_reading)) ? NULL : &_sensor_reading;

	  switch (verbose)
	    {
	    case 0:
	      sensors_display_simple (sdr_record, sensor_reading);
	      break;
	    case 1:
	      sensors_display_verbose (sdr_record, sensor_reading);
	      break;
	    case 2:
	    default:
	      sensors_display_very_verbose (sdr_record, sensor_reading);
	    }
	}
    }
  
  return 0;
}

int 
display_sensor_list (ipmi_device_t dev, 
		     unsigned int *sensors_list, 
		     unsigned int sensors_list_length, 
		     int verbose)
{
  int i;
  sdr_record_t *sdr_record;
  sensor_reading_t _sensor_reading;
  sensor_reading_t *sensor_reading;
  
  sensor_reading = &_sensor_reading;
  
  for (i = 0; i < sdr_record_count; i++)
    {
      sdr_record = sdr_record_list + i;
      
      if (sensors_list_cmp (sdr_record, sensors_list, sensors_list_length) == 0)
	{
          memset (&_sensor_reading, 0, sizeof (sensor_reading_t));
          sensor_reading = (get_sensor_reading (dev, 
                                                sdr_record, 
                                                &_sensor_reading)) ? NULL : &_sensor_reading;

	  switch (verbose)
	    {
	    case 0:
	      sensors_display_simple (sdr_record, sensor_reading);
	      break;
	    case 1:
	      sensors_display_verbose (sdr_record, sensor_reading);
	      break;
	    case 2:
	    default:
	      sensors_display_very_verbose (sdr_record, sensor_reading);
	    }
	}
    }
  
  return 0;
}

int 
display_sensors (ipmi_device_t dev)
{
  struct arguments *args = NULL;
  
  args = ipmi_sensors_get_arguments ();
  if (args->group)
    {
      display_group_sensors (dev, 
			     args->group, 
			     args->verbose_count);
    }
  if (args->sensors_list_wanted)
    {
      display_sensor_list (dev, 
			   args->sensors_list, 
			   args->sensors_list_length, 
			   args->verbose_count);
    }
  
  if (args->group == NULL && !args->sensors_list_wanted)
    {
      int i;
      sdr_record_t *sdr_record;
      sensor_reading_t _sensor_reading;
      sensor_reading_t *sensor_reading;
      
      sensor_reading = &_sensor_reading;
      
      for (i = 0; i < sdr_record_count; i++)
	{
	  sdr_record = sdr_record_list + i;
	  memset (&_sensor_reading, 0, sizeof (sensor_reading_t));
	  sensor_reading = (get_sensor_reading (dev, 
						sdr_record, 
						&_sensor_reading)) ? NULL : &_sensor_reading;
	  
	  switch (args->verbose_count)
	    {
	    case 0:
	      sensors_display_simple (sdr_record, sensor_reading);
	      break;
	    case 1:
	      sensors_display_verbose (sdr_record, sensor_reading);
	      break;
	    case 2:
	    default:
	      sensors_display_very_verbose (sdr_record, sensor_reading);
	    }
	}
    }
  
  return 0;
}

int 
run_cmd_args (ipmi_device_t dev, struct arguments *args)
{
  int retval = 0;
  
  if (args == NULL)
    return (-1);
  
  if (args->sdr_info_wanted)
    {
      retval = display_sdr_repository_info (dev);
      return retval;
    }
  
  if (args->flush_cache_wanted)
    {
      if (!args->quiet_cache_wanted)
        printf ("flushing cache... ");
      retval = flush_sdr_cache_file (args->common.host, args->sdr_cache_dir);
      if (!args->quiet_cache_wanted)
        printf ("%s\n", (retval ? "FAILED" : "done"));
      return retval;
    }
  
  if (args->list_groups_wanted)
    {
      retval = display_group_list ();
      return retval;
    }
  
  if ((retval = init_sdr_cache (dev, args)))
    {
      fprintf (stderr, "%s: sdr cache initialization failed\n", 
	       program_invocation_short_name);
      return retval;
    }
  
  return display_sensors (dev);
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

int 
main (int argc, char **argv)
{
  struct arguments *args = NULL;
  ipmi_device_t dev = NULL;
  int retval = 0;
  int flags;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */
  
  _disable_coredump();
  
  ipmi_sensors_argp_parse (argc, argv);
  args = ipmi_sensors_get_arguments ();
  
  if (setup_sdr_cache_directory () == -1)
    {
      fprintf (stderr, "%s: sdr cache directory setup failed\n", 
	       program_invocation_short_name);
      exit (EXIT_FAILURE);
    }
  
#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
  
#ifndef NDEBUG
  if (args->common.debug)
    flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */

  if (args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
				       args->common.host,
                                       args->common.username,
                                       args->common.password,
                                       args->common.authentication_type, 
                                       args->common.privilege_level,
                                       args->common.session_timeout, 
                                       args->common.retry_timeout, 
				       flags)))
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", argv[0]);
          exit(EXIT_FAILURE);
        }

      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI, 
					args->common.disable_auto_probe, 
                                        args->common.driver_address, 
                                        args->common.register_spacing,
                                        args->common.driver_device, 
                                        flags)))
	    {
              if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS,
                                            args->common.disable_auto_probe,
                                            args->common.driver_address,
                                            args->common.register_spacing,
                                            args->common.driver_device,
                                            flags)))
                {
                  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
                                                args->common.disable_auto_probe,
                                                args->common.driver_address,
                                                args->common.register_spacing,
                                                args->common.driver_device,
                                                flags)))
                    {
                      perror ("ipmi_open_inband()");
                      return (-1);
                    }
                }
            }
	}
      else
	{
	  if (!(dev = ipmi_open_inband (args->common.driver_type,
					args->common.disable_auto_probe,
					args->common.driver_address,
                                        args->common.register_spacing,
                                        args->common.driver_device,
                                        flags)))
	    {
	      perror ("ipmi_open_inband()");
	      return (-1);
	    }
	}
    }
  
  retval = run_cmd_args (dev, args);
  
  ipmi_close_device (dev);
  
  return (retval);
}
