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
#include "ipmi-sel-entry.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-fiid-wrappers.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-sdr-cache-common.h"

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
  struct ipmi_sel_arguments *args;
  fiid_obj_t obj_reserve_sel_rs = NULL;
  fiid_obj_t obj_clear_sel_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(state_data);

  args = state_data->prog_data->args;

  _FIID_OBJ_CREATE(obj_reserve_sel_rs, tmpl_cmd_reserve_sel_rs);
  _FIID_OBJ_CREATE(obj_clear_sel_rs, tmpl_cmd_clear_sel_rs);

  if (ipmi_cmd_reserve_sel (state_data->ipmi_ctx, obj_reserve_sel_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_reserve_sel: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET(obj_reserve_sel_rs, "reservation_id", &val);
  state_data->reservation_id = val;

  if (ipmi_cmd_clear_sel (state_data->ipmi_ctx,
                          state_data->reservation_id,
                          IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE,
                          obj_clear_sel_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_clear_sel: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_reserve_sel_rs);
  _FIID_OBJ_DESTROY(obj_clear_sel_rs);
  return rv;
}

static int
_delete_entry (ipmi_sel_state_data_t *state_data, 
               uint16_t record_id,
               int ignore_missing_sel_entries)
{
  fiid_obj_t obj_reserve_sel_rs = NULL;
  fiid_obj_t obj_delete_sel_entry_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(state_data);
  assert(record_id);

  _FIID_OBJ_CREATE(obj_reserve_sel_rs, tmpl_cmd_reserve_sel_rs);
  _FIID_OBJ_CREATE(obj_delete_sel_entry_rs, tmpl_cmd_delete_sel_entry_rs);

  if (ipmi_cmd_reserve_sel (state_data->ipmi_ctx, obj_reserve_sel_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_reserve_sel: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET(obj_reserve_sel_rs, "reservation_id", &val);
  state_data->reservation_id = val;

  if (ipmi_cmd_delete_sel_entry (state_data->ipmi_ctx,
                                 state_data->reservation_id,
                                 record_id,
                                 obj_delete_sel_entry_rs) < 0)
    {
      if (!(ignore_missing_sel_entries
            && (ipmi_check_completion_code(obj_delete_sel_entry_rs, IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1
                || ipmi_check_completion_code(obj_delete_sel_entry_rs, IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1)))
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_cmd_delete_sel_entry: %s\n",
                          ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          goto cleanup;
        }
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_reserve_sel_rs);
  _FIID_OBJ_DESTROY(obj_delete_sel_entry_rs);
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
_hex_display_sel_records (ipmi_sel_state_data_t *state_data, 
                          FILE *stream)
{
  fiid_obj_t obj_reserve_sel_rs = NULL;
  fiid_obj_t obj_get_sel_entry_rs = NULL;
  uint8_t record_data[IPMI_SEL_RECORD_SIZE];
  uint32_t record_data_len = IPMI_SEL_RECORD_SIZE;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  unsigned int reservation_cancelled = 0;
  uint64_t val;
  int32_t len;
  int rv = -1;

  assert(state_data);
  assert(stream);

  _FIID_OBJ_CREATE (obj_reserve_sel_rs, tmpl_cmd_reserve_sel_rs);
  _FIID_OBJ_CREATE (obj_get_sel_entry_rs, tmpl_cmd_get_sel_entry_rs);

  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      memset (record_data, '\0', record_data_len);

      /*
       *
       * IPMI spec states in section 31.4.1:
       *
       * "A Requester must issue a 'Reserve SEL' command prior to issuing
       * any of the following SEL commands. Note that the 'Reserve SEL'
       * command only needs to be reissued if the reservation is
       * canceled. ... Get SEL Entry command (if 'get' is from an offset
       * other than 00h)".
       *
       * Since we always use an offset of 00h, presumably we should never
       * need reserve the SEL before the get_sel_entry call.
       *
       * However, some machines may need it due to compliance issues.
       * I don't think using a reservation ID all of the time hurts
       * anything, so we'll just use it all of the time. If there's an
       * error along the way, we'll just ignore it.
       */
      
      if (record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY || reservation_cancelled)
        {
          if (ipmi_cmd_reserve_sel (state_data->ipmi_ctx, obj_reserve_sel_rs) < 0)
            {
              if (state_data->prog_data->args->common.debug)
                pstdout_fprintf(state_data->pstate,
                                stderr,
                                "ipmi_cmd_reserve_sel: %s\n",
                                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));

              state_data->reservation_id = 0;
              goto get_sel_entry;
            }

          _FIID_OBJ_GET(obj_reserve_sel_rs, "reservation_id", &val);
          state_data->reservation_id = val;
        }

    get_sel_entry:

      if (ipmi_cmd_get_sel_entry (state_data->ipmi_ctx,
                                  0,
                                  record_id,
                                  0,
                                  IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
                                  obj_get_sel_entry_rs) < 0)
        {
          if (ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code(obj_get_sel_entry_rs,
                                            IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
              if (reservation_cancelled)
                {
                  pstdout_fprintf(state_data->pstate,
                                  stderr,
                                  "Reservation Cancelled multiple times\n");
                  goto cleanup;
                }
              reservation_cancelled++;
              continue;
            }

          /* If the sel is empty, don't bother outputting an error
           * message, it's not a real error.
           */
          if (!(record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
                && ipmi_ctx_errnum(state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
                && ipmi_check_completion_code(obj_get_sel_entry_rs,
                                              IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1))
            pstdout_fprintf(state_data->pstate,
                            stderr,
                            "ipmi_cmd_get_sel_entry: %s\n",
                            ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          goto cleanup;
        }

      _FIID_OBJ_GET (obj_get_sel_entry_rs, "next_record_id", &val);
      next_record_id = val;
      
      _FIID_OBJ_GET_DATA_LEN (len,
                              obj_get_sel_entry_rs,
                              "record_data",
                              record_data,
                              record_data_len);

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
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_reserve_sel_rs);
  _FIID_OBJ_DESTROY(obj_get_sel_entry_rs);
  return (rv);
}

static int
_hex_dump (ipmi_sel_state_data_t *state_data)
{
  struct ipmi_sel_arguments *args;
  int rv = -1;
  
  assert(state_data);

  args = state_data->prog_data->args;

  if (args->hex_dump_filename)
    {
      FILE *stream = NULL;
      
      if ((stream = fopen (args->hex_dump_filename, "a+")))
        {
          rv = _hex_display_sel_records (state_data, stream);
          fclose (stream);
        }
      else 
        pstdout_fprintf (state_data->pstate, 
                         stderr, 
                         "%s: unable to open hex dump file [%s]\n", 
                         state_data->prog_data->progname,
                         args->hex_dump_filename);
    }
  else 
    rv = _hex_display_sel_records (state_data, stdout);
  
  return rv;
}

static int 
_display_sel_records (ipmi_sel_state_data_t *state_data)
{
  struct ipmi_sel_arguments *args;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  
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
    }

  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      uint16_t stored_record_id;
      char *timestamp = NULL;
      char *sensor_info = NULL;
      char *event_message = NULL;
      char *event_data2_message = NULL;
      char *event_data3_message = NULL;
      
      if (ipmi_sel_get_entry (state_data, 
                              record_id, 
                              &next_record_id,
                              &stored_record_id,
                              &timestamp,
                              &sensor_info,
                              &event_message,
                              &event_data2_message,
                              &event_data3_message) < 0)
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
      
      /* We output the record id stored in the SEL record, not the
       * above, b/c the above may not be legit.  For example,
       * IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY is not a valid record id.
       */
      pstdout_printf (state_data->pstate, "%d", stored_record_id);
      if (timestamp)
	pstdout_printf (state_data->pstate, ":%s", timestamp);
      if (sensor_info)
	pstdout_printf (state_data->pstate, ":%s", sensor_info);
      if (event_message)
	pstdout_printf (state_data->pstate, ":%s", event_message);
      if (event_data2_message)
	pstdout_printf (state_data->pstate, ":%s", event_data2_message);
      if (event_data3_message)
	pstdout_printf (state_data->pstate, ":%s", event_data3_message);
      pstdout_printf (state_data->pstate, "\n");

      if (timestamp)
        free(timestamp);
      if (sensor_info)
        free(sensor_info);
      if (event_message)
        free(event_message);
      if (event_data2_message)
        free(event_data2_message);
      if (event_data3_message)
        free(event_data3_message);
    }
  
  return (0);
}

int 
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

  if (args->hex_dump)
    return _hex_dump (state_data);

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
  state_data.reservation_id = 0;

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
