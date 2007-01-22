/*
ipmi-sel.c: System Event Logger utility.
Copyright (C) 2005 FreeIPMI Core Team

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
#include "ipmi-sdr-api.h"
#include "ipmi-sel-argp.h"
#include "ipmi-sel-wrapper.h"

#include "freeipmi-portability.h"

typedef struct ipmi_sel_prog_data
{
  char *progname;
  struct arguments *args;
  uint32_t debug_flags;
} ipmi_sel_prog_data_t;

typedef struct ipmi_sel_state_data
{
  ipmi_sel_prog_data_t *prog_data;
  ipmi_device_t dev;
  sdr_repository_info_t sdr_info;
  sdr_record_t *sdr_record_list;
  int sdr_record_count;
} ipmi_sel_state_data_t;

void
cleanup_sdr_cache (ipmi_sel_state_data_t *state_data)
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
init_sdr_cache (ipmi_sel_state_data_t *state_data)
{
  struct arguments *args = NULL;
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
display_sel_info (ipmi_sel_state_data_t *state_data)
{
  local_sel_info_t sel_info;
  char str[512];
  time_t t;
  struct tm *tmp;
  
  assert(state_data);

  memset (&sel_info, 0, sizeof (local_sel_info_t));
  
  if (get_sel_info (state_data->dev, &sel_info) != 0)
    {
      fprintf (stderr, "%s: unable to get SEL information\n", 
	       program_invocation_short_name);
      return (-1);
    }
  
  printf ("SEL version:                                      %d.%d\n", 
	  sel_info.sel_version_major, 
	  sel_info.sel_version_minor);
  printf ("Number of log entries:                            %d\n", 
	  sel_info.log_entry_count);
  printf ("Free space remaining:                             %d bytes\n", 
	  sel_info.free_space);
  
  t = sel_info.recent_addition_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  printf ("Recent addition timestamp:                        %s\n", str);
  
  t = sel_info.recent_erase_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  printf ("Recent erase timestamp:                           %s\n", str);
  
  printf ("Get SEL Allocation Information Command supported: %s\n", 
	  (sel_info.get_sel_alloc_info_cmd_support ? "Yes" : "No"));
  printf ("Reserve SEL Command supported:                    %s\n", 
	  (sel_info.reserve_sel_cmd_support ? "Yes" : "No"));
  printf ("Partial Add SEL Entry Command supported:          %s\n", 
	  (sel_info.partial_add_sel_entry_cmd_support ? "Yes" : "No"));
  printf ("Delete SEL Command supported:                     %s\n", 
	  (sel_info.delete_sel_cmd_support ? "Yes" : "No"));
  printf ("Events drop due to lack of space in SEL:          %s\n", 
	  (sel_info.overflow_flag ? "Yes" : "No"));
  
  return (0);
}

int 
display_sel_records (ipmi_sel_state_data_t *state_data)
{
  sel_record_t sel_rec;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  
  assert(state_data);

  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      memset (&sel_rec, 0, sizeof (sel_record_t));
      if (get_sel_record (state_data->dev, 
                          record_id, 
                          &sel_rec, 
                          &next_record_id,
                          state_data->sdr_record_list,
                          state_data->sdr_record_count) < 0)
	{
	  fprintf (stderr, "%s: unable to get SEL record\n", 
		   program_invocation_short_name);
	  return (-1);
	}
      
      printf ("%d", sel_rec.record_id);
      if (sel_rec.timestamp)
	printf (":%s", sel_rec.timestamp);
      if (sel_rec.sensor_info)
	printf (":%s", sel_rec.sensor_info);
      if (sel_rec.event_message)
	printf (":%s", sel_rec.event_message);
      if (sel_rec.event_data2_message)
	printf (":%s", sel_rec.event_data2_message);
      if (sel_rec.event_data3_message)
	printf (":%s", sel_rec.event_data3_message);
      printf ("\n");
      
      if (sel_rec.timestamp) free (sel_rec.timestamp);
      if (sel_rec.sensor_info) free (sel_rec.sensor_info);
      if (sel_rec.event_message) free (sel_rec.event_message);
      if (sel_rec.event_data2_message) free (sel_rec.event_data2_message);
      if (sel_rec.event_data3_message) free (sel_rec.event_data3_message);
    }
  
  return (0);
}

int 
hex_display_sel_records (ipmi_sel_state_data_t *state_data, FILE *stream)
{
  uint8_t record_data[SEL_RECORD_SIZE];
  uint32_t record_data_len = SEL_RECORD_SIZE;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  
  assert(state_data);
  assert(stream);

  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      memset (record_data, 0, record_data_len);
      if (get_sel_record_raw (state_data->dev, 
                              record_id, 
                              record_data, 
                              record_data_len, 
                              &next_record_id) != 0)
	{
	  fprintf (stderr, "%s: unable to get SEL record\n", 
		   program_invocation_short_name);
	  return (-1);
	}
      
      fprintf (stream, 
	       "RID:[%02X][%02X] " 
	       "RT:[%02X] " 
	       "TS:[%02X][%02X][%02X][%02X] " 
	       "GID:[%02X][%02X] " 
	       "ER:[%02X] " 
	       "ST:[%02X] " 
	       "SN:[%02X] " 
	       "EDIR:[%02X] "
	       "ED1: [%02X] "
	       "ED2: [%02X] "
	       "ED3: [%02X]\n",
	       record_data[0], record_data[1], 
	       record_data[2], 
	       record_data[3], record_data[4], record_data[5], record_data[6], 
	       record_data[7], record_data[8], 
	       record_data[9], 
	       record_data[10], 
	       record_data[11], 
	       record_data[12], 
	       record_data[13], 
	       record_data[14], 
	       record_data[15]);
    }
  
  return (0);
}

int 
run_cmd_args (ipmi_sel_state_data_t *state_data)
{
  struct arguments *args;
  int rv = -1;

  assert(state_data);

  args = state_data->prog_data->args;
  
  if (args->info_wanted)
    return display_sel_info (state_data);
  
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

  if (args->hex_dump_wanted)
    {
      int retval = -1;

      if (args->hex_dump_filename)
	{
	  FILE *stream = NULL;
	  
	  if ((stream = fopen (args->hex_dump_filename, "a+")))
	    {
	      retval = hex_display_sel_records (state_data, stream);
	      fclose (stream);
	    }
	  else 
	    {
	      fprintf (stderr, "%s: unable to open hex dump file [%s]\n", 
		       program_invocation_short_name, 
		       args->hex_dump_filename);
	    }
	}
      else 
        retval = hex_display_sel_records (state_data, stdout);
      
      return retval;
    }
  
  if (args->delete_wanted)
    {
      int i;
      
      for (i = 0; i < args->delete_record_list_length; i++)
	{
	  if (delete_sel_entry (state_data->dev, args->delete_record_list[i]) < 0)
            {
              fprintf (stderr, "deletion of record ID %d failed\n", 
                       args->delete_record_list[i]);
              return (-1);
            }
	}
      
      return 0;
    }
  
  if (args->delete_all_wanted)
    return clear_sel_entries (state_data->dev);
  
  if (args->delete_range_wanted)
    {
      int i = 0;
      
      for (i = args->delete_range1; i <= args->delete_range2; i++)
        /* ignore errors - some numbers may not exist */
        delete_sel_entry (state_data->dev, i);
      
      return 0;
    }
  
  /* achu: ipmi-sel does not require the SDR cache, so if this fails, oh well */
  if (init_sdr_cache (state_data) < 0)
    fprintf (stderr, "%s: sdr cache initialization failed\n",
             program_invocation_short_name);

  if (display_sel_records (state_data) < 0)
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
_ipmi_sel (void *arg)
{
  ipmi_sel_state_data_t state_data;
  ipmi_sel_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  int exit_code = -1;

  prog_data = (ipmi_sel_prog_data_t *)arg;

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
                                                prog_data->args->common.driver_address ,
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
  
  memset(&state_data, '\0', sizeof(ipmi_sel_state_data_t));
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
  ipmi_sel_prog_data_t prog_data;
  int exit_code;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */

  _disable_coredump();

  prog_data.progname = argv[0];
  ipmi_sel_argp_parse (argc, argv);
  prog_data.args = ipmi_sel_get_arguments ();

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

  exit_code = _ipmi_sel(&prog_data);
 cleanup:
  return (exit_code);
}
