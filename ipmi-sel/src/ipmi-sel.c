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
#include <argp.h>
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

#include "cmdline-parse-common.h"
#include "freeipmi-portability.h"
#include "tool-common.h"
#include "ipmi-sdr-cache.h"
#include "ipmi-sel.h"
#include "ipmi-sel-argp.h"
#include "ipmi-sel-wrapper.h"
#include "pstdout.h"
#include "hostrange.h"

int 
display_sel_info (ipmi_sel_state_data_t *state_data)
{
  local_sel_info_t sel_info;
  char str[512];
  time_t t;
  struct tm *tmp;
  
  assert(state_data);

  memset (&sel_info, 0, sizeof (local_sel_info_t));
  
  if (get_sel_info (state_data, &sel_info) != 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr, 
                       "%s: unable to get SEL information\n", 
                       state_data->prog_data->progname);
      return (-1);
    }
  
  pstdout_printf (state_data->pstate, 
                  "SEL version:                                      %d.%d\n", 
                  sel_info.sel_version_major, 
                  sel_info.sel_version_minor);
  pstdout_printf (state_data->pstate, 
                  "Number of log entries:                            %d\n", 
                  sel_info.log_entry_count);
  pstdout_printf (state_data->pstate, 
                  "Free space remaining:                             %d bytes\n", 
                  sel_info.free_space);
  
  t = sel_info.recent_addition_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  pstdout_printf (state_data->pstate, 
                  "Recent addition timestamp:                        %s\n", str);
  
  t = sel_info.recent_erase_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  pstdout_printf (state_data->pstate, 
                  "Recent erase timestamp:                           %s\n", str);
  
  pstdout_printf (state_data->pstate, 
                  "Get SEL Allocation Information Command supported: %s\n", 
                  (sel_info.get_sel_alloc_info_cmd_support ? "Yes" : "No"));
  pstdout_printf (state_data->pstate, 
                  "Reserve SEL Command supported:                    %s\n", 
                  (sel_info.reserve_sel_cmd_support ? "Yes" : "No"));
  pstdout_printf (state_data->pstate, 
                  "Partial Add SEL Entry Command supported:          %s\n", 
                  (sel_info.partial_add_sel_entry_cmd_support ? "Yes" : "No"));
  pstdout_printf (state_data->pstate, 
                  "Delete SEL Command supported:                     %s\n", 
                  (sel_info.delete_sel_cmd_support ? "Yes" : "No"));
  pstdout_printf (state_data->pstate, 
                  "Events drop due to lack of space in SEL:          %s\n", 
                  (sel_info.overflow_flag ? "Yes" : "No"));
  
  return (0);
}

int 
display_sel_records (ipmi_sel_state_data_t *state_data)
{
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  
  assert(state_data);

  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      sel_record_t *sel_rec;

      if (!(sel_rec = get_sel_record (state_data, 
                                      record_id, 
                                      &next_record_id)))
        {
          /* If we error on the first record, assume its b/c the sel
           * is empty so we don't exit with an error.
           */
          if (record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY)
            return 0;

          pstdout_fprintf (state_data->pstate, 
                           stderr, 
                           "%s: unable to get SEL record\n", 
                           state_data->prog_data->progname);
          return (-1);
        }
      
      pstdout_printf (state_data->pstate, "%d", sel_rec->record_id);
      if (sel_rec->timestamp)
	pstdout_printf (state_data->pstate, ":%s", sel_rec->timestamp);
      if (sel_rec->sensor_info)
	pstdout_printf (state_data->pstate, ":%s", sel_rec->sensor_info);
      if (sel_rec->event_message)
	pstdout_printf (state_data->pstate, ":%s", sel_rec->event_message);
      if (sel_rec->event_data2_message)
	pstdout_printf (state_data->pstate, ":%s", sel_rec->event_data2_message);
      if (sel_rec->event_data3_message)
	pstdout_printf (state_data->pstate, ":%s", sel_rec->event_data3_message);
      pstdout_printf (state_data->pstate, "\n");

      destroy_sel_record(sel_rec);
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
      if (get_sel_record_raw (state_data, 
                              record_id, 
                              record_data, 
                              record_data_len, 
                              &next_record_id) != 0)
	{
          /* If we error on the first record, assume its b/c the sel
           * is empty so we don't exit with an error.
           */
          if (record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY)
            return 0;
          
	  pstdout_fprintf (state_data->pstate, 
                           stderr, 
                           "%s: unable to get SEL record\n", 
                           state_data->prog_data->progname);
	  return (-1);
	}
      
      pstdout_fprintf (state_data->pstate, 
                       stream, 
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
  struct ipmi_sel_arguments *args;
  char errmsg[IPMI_SDR_CACHE_ERRMSGLEN];
  int rv = -1;

  assert(state_data);

  args = state_data->prog_data->args;
  
  if (args->info_wanted)
    return display_sel_info (state_data);
  
  if (args->sdr.flush_cache_wanted)
    {
      if (!args->sdr.quiet_cache_wanted)
        pstdout_printf (state_data->pstate, "flushing cache... ");
      if (sdr_cache_flush (state_data->sdr_cache_ctx,
                           state_data->hostname, 
                           args->sdr.sdr_cache_dir) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr, 
                           "SDR Cache Flush failed: %s\n",
                           sdr_cache_ctx_strerror(sdr_cache_ctx_errnum(state_data->sdr_cache_ctx)));
          goto cleanup;
        }
      if (!args->sdr.quiet_cache_wanted)
        pstdout_printf (state_data->pstate, "done\n");
      return 0;
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
	      pstdout_fprintf (state_data->pstate, 
                               stderr, 
                               "%s: unable to open hex dump file [%s]\n", 
                               state_data->prog_data->progname,
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
	  if (delete_sel_entry (state_data, args->delete_record_list[i]) < 0)
            {
              pstdout_fprintf (state_data->pstate, 
                               stderr, 
                               "deletion of record ID %d failed\n", 
                               args->delete_record_list[i]);
              return (-1);
            }
	}
      
      return 0;
    }
  
  if (args->delete_all_wanted)
    return clear_sel_entries (state_data);
  
  if (args->delete_range_wanted)
    {
      int i = 0;
      
      for (i = args->delete_range1; i <= args->delete_range2; i++)
        /* ignore errors - some numbers may not exist */
        delete_sel_entry (state_data, i);
      
      return 0;
    }
  
  if (!args->sdr.ignore_sdr_cache_wanted)
    {
      if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                     state_data->dev,
                                     state_data->hostname,
                                     args->sdr.sdr_cache_dir,
                                     (args->sdr.quiet_cache_wanted) ? 0 : 1,
                                     (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP) ? 1 : 0,
                                     &(state_data->sdr_record_list),
                                     &(state_data->sdr_record_count),
                                     errmsg,
                                     IPMI_SDR_CACHE_ERRMSGLEN) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s\n",
                           errmsg);
          goto cleanup;
        }
    }

  if (display_sel_records (state_data) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (state_data->sdr_record_list)
    {
      free(state_data->sdr_record_list);
      state_data->sdr_record_list = NULL;
      state_data->sdr_record_count = 0;
    }
  return (rv);
}

static int
_ipmi_sel (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_sel_state_data_t state_data;
  ipmi_sel_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  char errmsg[IPMI_DEVICE_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_sel_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(ipmi_sel_state_data_t));

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache_wanted)
    {
      if (!(dev = ipmi_device_open(prog_data->progname,
                                   hostname,
                                   &(prog_data->args->common),
                                   errmsg,
                                   IPMI_DEVICE_OPEN_ERRMSGLEN)))
        {
          pstdout_fprintf(pstate,
                          stderr,
                          "%s\n",
                          errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
 
  state_data.dev = dev;
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  if (!(state_data.sdr_cache_ctx = sdr_cache_ctx_create()))
    {
      pstdout_perror (pstate, "sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.sdr_cache_ctx)
    sdr_cache_ctx_destroy(state_data.sdr_cache_ctx);
  if (dev)
    {
      ipmi_close_device (dev);
      ipmi_device_destroy (dev);
    }
  return exit_code;
}

int 
main (int argc, char **argv)
{
  ipmi_sel_prog_data_t prog_data;
  struct ipmi_sel_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump();

  prog_data.progname = argv[0];
  ipmi_sel_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup(&(prog_data.args->common.hostname),
                                   prog_data.args->hostrange.buffer_hostrange_output,
                                   prog_data.args->hostrange.consolidate_hostrange_output,
                                   prog_data.args->hostrange.fanout,
                                   prog_data.args->hostrange.eliminate)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->sdr.quiet_cache_wanted = 1;

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
                           _ipmi_sel,
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
