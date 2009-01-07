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

#include "freeipmi/api/ipmi-sel-cmds-api.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"

#include "ipmi-sel.h"
#include "ipmi-sel-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-fiid-wrappers.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-sdr-cache-common.h"

#define IPMI_SEL_RECORD_SIZE   16
#define IPMI_SEL_OUTPUT_BUFLEN 1024

static int 
_display_sel_info (ipmi_sel_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val, val1, val2;
  char str[512];
  int rv = -1;
  time_t t;
  struct tm tm;
  
  assert(state_data);

  _FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sel_info_rs);

  if (ipmi_cmd_get_sel_info (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_sel_info: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "sel_version_major", &val1);
  
  _FIID_OBJ_GET (obj_cmd_rs, "sel_version_minor", &val2);

  /* achu: ipmi version is BCD encoded, but major/minor are only 4 bits */
  pstdout_printf (state_data->pstate, 
                  "SEL version:                                      %d.%d\n", 
                  val1,
                  val2);

  _FIID_OBJ_GET (obj_cmd_rs, "entries", &val);
  
  pstdout_printf (state_data->pstate, 
                  "Number of log entries:                            %d\n", 
                  val);

  _FIID_OBJ_GET (obj_cmd_rs, "free_space", &val);

  pstdout_printf (state_data->pstate, 
                  "Free space remaining:                             %d bytes\n", 
                  val);
  
  _FIID_OBJ_GET (obj_cmd_rs, "most_recent_addition_timestamp", &val);
  
  t = val;
  localtime_r (&t, &tm);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", &tm);
  pstdout_printf (state_data->pstate, 
                  "Recent addition timestamp:                        %s\n", 
                  str);
  
  _FIID_OBJ_GET (obj_cmd_rs, "most_recent_erase_timestamp", &val);

  t = val;
  localtime_r (&t, &tm);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", &tm);
  pstdout_printf (state_data->pstate, 
                  "Recent erase timestamp:                           %s\n", 
                  str);
  
  _FIID_OBJ_GET (obj_cmd_rs, "get_sel_allocation_info_command_supported", &val);
  
  pstdout_printf (state_data->pstate, 
                  "Get SEL Allocation Information Command supported: %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "reserve_sel_command_supported", &val);
  
  pstdout_printf (state_data->pstate, 
                  "Reserve SEL Command supported:                    %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "partial_add_sel_entry_command_supported", &val);
  
  pstdout_printf (state_data->pstate, 
                  "Partial Add SEL Entry Command supported:          %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "delete_sel_command_supported", &val);
  
  pstdout_printf (state_data->pstate, 
                  "Delete SEL Command supported:                     %s\n", 
                  (val ? "Yes" : "No"));

  _FIID_OBJ_GET (obj_cmd_rs, "overflow_flag", &val);

  pstdout_printf (state_data->pstate, 
                  "Events drop due to lack of space in SEL:          %s\n", 
                  (val ? "Yes" : "No"));
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static int
_flush_cache (ipmi_sel_state_data_t *state_data)
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
_clear_entries (ipmi_sel_state_data_t *state_data)
{
  int rv = -1;

  assert(state_data);

  if (ipmi_sel_parse_clear_sel (state_data->ipmi_sel_parse_ctx) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_sel_parse_clear_sel: %s\n",
                      ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return rv;
}

static int
_delete_entry (ipmi_sel_state_data_t *state_data, 
               uint16_t record_id,
               int ignore_missing_sel_entries)
{
  int rv = -1;

  assert(state_data);
  assert(record_id);

  if (ipmi_sel_parse_delete_sel_entry (state_data->ipmi_sel_parse_ctx,
                                       record_id) < 0)
    {
      if (!(ignore_missing_sel_entries
            && (ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx) == IPMI_SEL_PARSE_CTX_ERR_NOT_FOUND)))
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_sel_parse_delete_sel_entry: %s\n",
                          ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
          goto cleanup;
        }
    }
  
  rv = 0;
 cleanup:
  return rv;
}

static int
_delete_records (ipmi_sel_state_data_t *state_data)
{
  struct ipmi_sel_arguments *args;
  int i;
  
  assert(state_data);
  
  args = state_data->prog_data->args;
  
  for (i = 0; i < args->delete_record_list_length; i++)
    {
      if (_delete_entry (state_data, 
                         args->delete_record_list[i],
                         0) < 0)
        return -1;
    }

  return 0;
}

static int
_delete_range (ipmi_sel_state_data_t *state_data)
{
  struct ipmi_sel_arguments *args;
  int i;
  
  assert(state_data);
  
  args = state_data->prog_data->args;
 
  for (i = args->delete_range1; i <= args->delete_range2; i++)
    {
      if (_delete_entry (state_data, i, 1) < 0)
        return -1;
    }
  
  return 0;
}

static int
_sel_parse_callback(ipmi_sel_parse_ctx_t ctx, void *callback_data)
{
  ipmi_sel_state_data_t *state_data;
  struct ipmi_sel_arguments *args;
  int rv = -1;

  assert(ctx);
  assert(callback_data);

  state_data = (ipmi_sel_state_data_t *)callback_data;
  args = state_data->prog_data->args;

  if (args->hex_dump)
    {
      uint8_t record_data[IPMI_SEL_RECORD_SIZE];
      int record_data_len;

      if ((record_data_len = ipmi_sel_parse_read_record(state_data->ipmi_sel_parse_ctx,
                                                        record_data,
                                                        IPMI_SEL_RECORD_SIZE)) < 0)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_sel_parse_delete_sel_entry: %s\n",
                          ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
          goto cleanup;
        }

      if (record_data_len < IPMI_SEL_RECORD_SIZE)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "Invalid length SEL entry read: %d\n",
                          record_data_len);
          goto out;
        }

      pstdout_printf (state_data->pstate, 
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
  else
    {
      char fmtbuf[IPMI_SEL_OUTPUT_BUFLEN+1];
      char outbuf[IPMI_SEL_OUTPUT_BUFLEN+1];
      char *fmt;
      int outbuf_len;
      unsigned int flags;
      uint8_t record_type;

      if (ipmi_sel_parse_read_record_type(state_data->ipmi_sel_parse_ctx,
                                          &record_type) < 0)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_sel_parse_read_record_type: %s\n",
                          ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
          goto cleanup;
        }

      flags = IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_MONTH_STRING;

      memset(fmt, '\0', IPMI_SEL_OUTPUT_BUFLEN+1);
      if (ipmi_sel_record_type_class(record_type) == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
        {
          uint8_t event_data2_flag;
          uint8_t event_data3_flag;

          if (ipmi_sel_parse_read_event_data1_event_data2_flag(state_data->ipmi_sel_parse_ctx, &event_data2_flag) < 0)
            {
              pstdout_fprintf(state_data->pstate,
                              stderr,
                              "ipmi_sel_parse_read_event_data1_event_data2_flag: %s\n",
                              ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
              goto cleanup;
            }
          if (ipmi_sel_parse_read_event_data1_event_data3_flag(state_data->ipmi_sel_parse_ctx, &event_data3_flag) < 0)
            {
              pstdout_fprintf(state_data->pstate,
                              stderr,
                              "ipmi_sel_parse_read_event_data1_event_data3_flag: %s\n",
                              ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
              goto cleanup;
            }
          strcpy(fmt, "%i:%d %t:%g %s:%e");
          if (event_data2_flag != IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE)
            strcat(fmt, ":%f");
          if (event_data3_flag != IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE)
            strcat(fmt, ":%h");
          fmt = fmtbuf;
        }
      else if (ipmi_sel_record_type_class(record_type) == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
        fmt = "%i:%d %t:%m:%o";
      else if (ipmi_sel_record_type_class(record_type) == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
        fmt = "%i:o";
      else
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "Unknown SEL Record Type: %X\n",
                          record_type);
          goto out;
        }

      memset(outbuf, '\0', IPMI_SEL_OUTPUT_BUFLEN+1);
      if ((outbuf_len = ipmi_sel_parse_read_record_string(state_data->ipmi_sel_parse_ctx,
                                                          fmt,
                                                          outbuf,
                                                          IPMI_SEL_OUTPUT_BUFLEN,
                                                          flags)) < 0)
        {
          if (ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx) == IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY)
            {
              /* maybe a bad SEL entry returned from remote system, don't error out*/
              pstdout_fprintf(state_data->pstate,
                              stderr,
                              "Invalid SEL entry read\n");
              goto out;
            }
          else
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_sel_parse_read_record_string: %s\n",
                            ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
          goto cleanup;
        }

      if (outbuf_len)
        pstdout_printf (state_data->pstate, "%s\n", outbuf);
    }

 out:
  rv = 0;
 cleanup:
  return rv;
}

static int 
_display_sel_records (ipmi_sel_state_data_t *state_data)
{
  struct ipmi_sel_arguments *args;
  ipmi_sdr_cache_ctx_t sdr_cache_ctx = NULL;
  
  assert(state_data);

  args = state_data->prog_data->args;

  if (!args->sdr.ignore_sdr_cache)
    {
      if (sdr_cache_create_and_load (state_data->ipmi_sdr_cache_ctx,
                                     state_data->pstate,
                                     state_data->ipmi_ctx,
                                     args->sdr.quiet_cache,
                                     args->sdr.sdr_cache_recreate,
                                     state_data->hostname,
                                     args->sdr.sdr_cache_directory) < 0)
        return -1;
      sdr_cache_ctx = state_data->ipmi_sdr_cache_ctx;
    }

  if (ipmi_sel_parse(state_data->ipmi_sel_parse_ctx,
                     _sel_parse_callback,
                     state_data) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_sel_parse: %s\n",
                      ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data->ipmi_sel_parse_ctx)));
      return -1;
    }

  return 0;
}

static int 
run_cmd_args (ipmi_sel_state_data_t *state_data)
{
  struct ipmi_sel_arguments *args;

  assert(state_data);

  args = state_data->prog_data->args;
  
  if (args->info)
    return _display_sel_info (state_data);

  if (args->sdr.flush_cache)
    return _flush_cache (state_data);
    
  if (args->delete_all)
    return _clear_entries (state_data);

  if (args->delete)
    return _delete_records (state_data);  
 
  if (args->delete_range)
    return _delete_range (state_data);

  /* else default to displaying records */

  if (_display_sel_records (state_data) < 0)
    return -1;

  return 0;
}

static int
_ipmi_sel (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_sel_state_data_t state_data;
  ipmi_sel_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_sel_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(ipmi_sel_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache)
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

  if (!(state_data.ipmi_sel_parse_ctx = ipmi_sel_parse_ctx_create(state_data.ipmi_ctx,
                                                                   prog_data->args->sdr.ignore_sdr_cache ? NULL : state_data.ipmi_sdr_cache_ctx)))
    {
      pstdout_perror (pstate, "ipmi_sel_parse_ctx_create()");
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

      /* Don't error out, if this fails we can still continue */
      if (ipmi_sel_parse_ctx_set_flags(state_data.ipmi_sel_parse_ctx,
                                       IPMI_SEL_PARSE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sel_parse_ctx_set_flags: %s\n",
                         ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data.ipmi_sel_parse_ctx)));

      if (hostname)
        {
          if (ipmi_sel_parse_ctx_set_debug_prefix(state_data.ipmi_sel_parse_ctx,
                                                  hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sel_parse_ctx_set_debug_prefix: %s\n",
                             ipmi_sel_parse_ctx_strerror(ipmi_sel_parse_ctx_errnum(state_data.ipmi_sel_parse_ctx)));
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
  if (state_data.ipmi_sel_parse_ctx)
    ipmi_sel_parse_ctx_destroy(state_data.ipmi_sel_parse_ctx);
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
  ipmi_sel_prog_data_t prog_data;
  struct ipmi_sel_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump();

  memset(&prog_data, '\0', sizeof(ipmi_sel_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_sel_argp_parse (argc, argv, &cmd_args);
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
