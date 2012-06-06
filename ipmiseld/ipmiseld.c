/*****************************************************************************\
 *  $Id: ipmiseld.c,v 1.17 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <syslog.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmiseld.h"
#include "ipmiseld-argp.h"
#include "ipmiseld-cache.h"
#include "ipmiseld-common.h"
#include "ipmiseld-debug.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "tool-common.h"
#include "tool-daemon-common.h"
#include "tool-event-common.h"
#include "tool-util-common.h"

#define IPMISELD_PIDFILE                IPMISELD_LOCALSTATEDIR "/run/ipmiseld.pid"

#define IPMISELD_FORMAT_BUFLEN          4096

#define IPMISELD_EVENT_OUTPUT_BUFLEN    4096

static int exit_flag = 1;

static int
_ipmi_sel_info_get (ipmiseld_host_data_t *host_data, ipmiseld_sel_info_t *sel_info)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->ipmi_ctx);
  assert (sel_info);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sel_info_rs)))
    {
      err_output ("fiid_obj_create: %s", strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sel_info (host_data->host_poll->ipmi_ctx, obj_cmd_rs) < 0)
    {
      err_output ("ipmi_cmd_get_sel_info: %s",
		  ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "entries", &val) < 0)
    {
      err_output ("fiid_obj_get: 'entries': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->entries = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "free_space", &val) < 0)
    {
      err_output ("fiid_obj_get: 'free_space': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->free_space = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "most_recent_addition_timestamp", &val) < 0)
    {
      err_output ("fiid_obj_get: 'most_recent_addition_timestamp': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->most_recent_addition_timestamp = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "most_recent_erase_timestamp", &val) < 0)
    {
      err_output ("fiid_obj_get: 'most_recent_erase_timestamp': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->most_recent_erase_timestamp = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "delete_sel_command_supported", &val) < 0)
    {
      err_output ("fiid_obj_get: 'delete_sel_command_supported': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
 sel_info->delete_sel_command_supported = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "reserve_sel_command_supported", &val) < 0)
    {
      err_output ("fiid_obj_get: 'reserve_sel_command_supported': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->reserve_sel_command_supported = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "overflow_flag", &val) < 0)
    {
      err_output ("fiid_obj_get: 'overflow_flag': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->overflow_flag = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_sel_last_record_id_callback (ipmi_sel_ctx_t ctx, void *callback_data)
{
  ipmiseld_last_record_id_t *last_record_id;

  assert (ctx);
  assert (callback_data);

  last_record_id = (ipmiseld_last_record_id_t *)callback_data;

  if (ipmi_sel_parse_read_record_id (ctx,
                                     NULL,
                                     0,
				     &(last_record_id->record_id)) < 0)
    {
      err_output ("ipmi_sel_parse_read_record_id: %s",
		  ipmi_sel_ctx_errormsg (ctx));
      return (-1);
    }
  
  last_record_id->loaded = 1;
  return (0);
}

static int
_ipmiseld_last_record_id (ipmiseld_host_data_t *host_data,
			  ipmiseld_last_record_id_t *last_record_id)
{
  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->sel_ctx);
  assert (last_record_id);

  host_data->host_state.last_record_id.loaded = 0;

  if (ipmi_sel_parse (host_data->host_poll->sel_ctx,
		      IPMI_SEL_RECORD_ID_LAST,
		      IPMI_SEL_RECORD_ID_LAST,
		      _sel_last_record_id_callback,
		      last_record_id) < 0)
    {
      err_output ("ipmi_sel_parse: %s", ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
      return (-1);
    }

  return (0);
}

static int
_ipmiseld_calc_percent_full (ipmiseld_host_data_t *host_data,
			     ipmiseld_sel_info_t *sel_info)
{
  unsigned int used_bytes;
  unsigned int total_bytes;
  int percent;

  assert (host_data);
  assert (sel_info);

  used_bytes = (sel_info->entries * IPMI_SEL_RECORD_MAX_RECORD_LENGTH);
  total_bytes = used_bytes + sel_info->free_space;
  percent = (int)(100 * (double)used_bytes/total_bytes); 
  if (percent > 100)
    {
      /* Some rounding errors could occur, we accept small ones */
      if (percent > 105)
	ipmiseld_syslog_host (host_data, "SEL percent calc error: %d", percent);
      percent = 100;
    }
  return (percent);
}

static int
_ipmiseld_host_state_init (ipmiseld_host_data_t *host_data)
{
  int rv = -1;
  int percent;

  assert (host_data);

  if (_ipmiseld_last_record_id (host_data, &(host_data->host_state.last_record_id)) < 0)
    goto cleanup;
  
  /* possible SEL is empty */
  if (!host_data->host_state.last_record_id.loaded)
    {
      host_data->host_state.last_record_id.record_id = 0;
      host_data->host_state.last_record_id.loaded = 1;
    }
  
  if (_ipmi_sel_info_get (host_data, &(host_data->host_state.sel_info)) < 0)
    goto cleanup;

  percent = _ipmiseld_calc_percent_full (host_data, &(host_data->host_state.sel_info));
  host_data->host_state.last_percent_full = percent; 
  
  host_data->host_state.initialized = 1;
  rv = 0;
 cleanup:
  return (rv);
}

/* return (-1), real error */
static int
_sel_parse_err_handle (ipmiseld_host_data_t *host_data, char *func)
{
  assert (host_data);
  assert (func);

  if (ipmi_sel_ctx_errnum (host_data->host_poll->sel_ctx) == IPMI_SEL_ERR_INVALID_SEL_ENTRY)
    {
      /* maybe a bad SEL entry returned from remote system, don't error out */
      if (host_data->prog_data->args->common_args.debug)
        IPMISELD_HOST_DEBUG (("Invalid SEL entry read"));
      return (0);
    }

  err_output ("%s: %s",
	      func,
	      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));

  return (-1);
}

/* returns 0 on success, 1 on success but w/ truncation */
static int
_snprintf (char *buf,
	   unsigned int buflen,
	   unsigned int *wlen,
	   const char *fmt,
	   ...)
{
  va_list ap;
  int ret;

  assert (buf);
  assert (buflen);
  assert (wlen);
  assert (fmt);

  va_start (ap, fmt);
  ret = vsnprintf (buf + *wlen, buflen - *wlen, fmt, ap);
  va_end (ap);
  if (ret >= (buflen - *wlen))
    {
      (*wlen) = buflen;
      return (1);
    }
  (*wlen) += ret;
  return (0);
}

static int
_sel_log_format (ipmiseld_host_data_t *host_data,
		 const char *fmt_str,
		 char *fmtbuf,
		 unsigned int fmtbuf_len)
{
  unsigned int wlen = 0;
  int percent_flag = 0;
  char *ptr;

  assert (host_data);
  assert (fmt_str);

  ptr = (char *)fmt_str;
  while (*ptr)
    {
      if (*ptr == '%')
	{
	  if (percent_flag)
	    {
	      if (_snprintf (fmtbuf, fmtbuf_len, &wlen, "%%"))
                return (0);
              percent_flag = 0;
	    }
	  else
	    percent_flag = 1;
	  goto end_loop;
	}
      else if (percent_flag && *ptr == 'h')
	{
	  if (_snprintf (fmtbuf, fmtbuf_len, &wlen, "%s",
			 host_data->hostname ? host_data->hostname : "localhost"))
            return (0);
	  percent_flag = 0;
	}
      else
	{
	  if (percent_flag)
            {
              if (_snprintf (fmtbuf, fmtbuf_len, &wlen, "%%%c", *ptr))
                return (0);
              percent_flag = 0;
            }
          else
            {
              if (_snprintf (fmtbuf, fmtbuf_len, &wlen, "%c", *ptr))
                return (0);
            }
	}

    end_loop:
      ptr++;
    }

  return (0);
}

static int
_sel_log_output (ipmiseld_host_data_t *host_data, uint8_t record_type)
{
  char fmtbuf[IPMISELD_FORMAT_BUFLEN + 1];
  char outbuf[IPMISELD_EVENT_OUTPUT_BUFLEN + 1];
  int outbuf_len;
  unsigned int flags;
  int record_type_class;
  char *format_str;
  uint16_t record_id;

  assert (host_data);

  memset (fmtbuf, '\0', IPMISELD_FORMAT_BUFLEN + 1);
  memset (outbuf, '\0', IPMISELD_EVENT_OUTPUT_BUFLEN + 1);
   
  if (ipmi_sel_parse_read_record_id (host_data->host_poll->sel_ctx,
                                     NULL,
                                     0,
                                     &record_id) < 0)
    {
      err_output ("ipmi_sel_parse_read_record_id: %s",
		  ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
      return (-1);
    }

  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug)
    IPMISELD_HOST_DEBUG (("SEL Record parsed: Record ID = %u", record_id));

  /* achu:
   *
   * Algorithmically we can "find" the next entry to log several ways,
   * but there are two reasonable ways.
   *
   * 1) Whatever the last record id is, add 1 to it and iterate until
   * you reach the next valid SEL record id.
   *
   * 2) Read the last record id, and use that to get the next record
   * id to log.
   *
   * While '1' will be faster on most systems, there are a number of
   * systems were vendors jump semi-big chunks of record ids on new
   * events (I have no idea why, it makes no sense).  We will
   * implement '2' as the most reasonable average solution.  So when
   * we hit this callback with the already logged last record id, we
   * need to not log it.  '2' is also the safer implementation, in the
   * event there is a bug in the firmware, and we could loop endlessly
   * looking for the next entry to log when there is none.
   */
  if (host_data->host_state.last_record_id.record_id == record_id)
    return (0);

  flags = IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD;
  flags |= IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE;
  flags |= IPMI_SEL_STRING_FLAGS_DATE_MONTH_STRING;
  if (host_data->prog_data->args->verbose_count)
    flags |= IPMI_SEL_STRING_FLAGS_VERBOSE;
  if (host_data->prog_data->args->entity_sensor_names)
    flags |= IPMI_SEL_STRING_FLAGS_ENTITY_SENSOR_NAMES;
  if (host_data->prog_data->args->non_abbreviated_units)
    flags |= IPMI_SEL_STRING_FLAGS_NON_ABBREVIATED_UNITS;
  if (host_data->prog_data->args->interpret_oem_data)
    flags |= IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA;

  record_type_class = ipmi_sel_record_type_class (record_type);
  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    format_str = host_data->prog_data->args->system_event_format_str;
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    format_str = host_data->prog_data->args->oem_timestamped_event_format_str;
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    format_str = host_data->prog_data->args->oem_non_timestamped_event_format_str;
  else
    {
      if (host_data->prog_data->args->verbose_count)
	ipmiseld_syslog_host (host_data,
			      "SEL Event: Unknown SEL Record Type: %Xh",
			      record_type);
      return (0);
    }

  if (_sel_log_format (host_data,
		       format_str,
		       fmtbuf,
		       IPMISELD_FORMAT_BUFLEN) < 0)
    return (-1);
  
  if ((outbuf_len = ipmi_sel_parse_read_record_string (host_data->host_poll->sel_ctx,
						       fmtbuf,
						       NULL,
						       0,
						       outbuf,
						       IPMISELD_EVENT_OUTPUT_BUFLEN,
						       flags)) < 0)
    {
      if (_sel_parse_err_handle (host_data, "ipmi_sel_parse_read_record_string") < 0)
	return (-1);
      return (0);
    }
  
  if (outbuf_len)
    ipmiseld_syslog (host_data, "%s", outbuf);

  host_data->host_state.last_record_id.record_id = record_id; 
  
  return (0);
}

static int
_sel_parse_callback (ipmi_sel_ctx_t ctx, void *callback_data)
{
  ipmiseld_host_data_t *host_data;
  uint8_t record_type;
  int record_type_class;
  int rv = -1;

  assert (ctx);
  assert (callback_data);

  host_data = (ipmiseld_host_data_t *)callback_data;

  if (host_data->prog_data->args->sensor_types_length
      || host_data->prog_data->args->exclude_sensor_types_length)
    {
      uint8_t sensor_type;
      int flag;

      if (ipmi_sel_parse_read_sensor_type (host_data->host_poll->sel_ctx,
                                           NULL,
                                           0,
                                           &sensor_type) < 0)
        {
          if (_sel_parse_err_handle (host_data, "ipmi_sel_parse_read_record_type") < 0)
            goto cleanup;
          goto out;
        }

      if (host_data->prog_data->args->sensor_types_length)
        {
          if ((flag = sensor_type_listed (NULL,
                                          sensor_type,
                                          host_data->prog_data->args->sensor_types,
                                          host_data->prog_data->args->sensor_types_length)) < 0)
            goto cleanup;
          
          if (!flag)
            goto out;
        }

      if (host_data->prog_data->args->exclude_sensor_types_length)
        {
          if ((flag = sensor_type_listed (NULL,
                                          sensor_type,
                                          host_data->prog_data->args->exclude_sensor_types,
                                          host_data->prog_data->args->exclude_sensor_types_length)) < 0)
            goto cleanup;

          if (flag)
            goto out;
        }
    }

  if (ipmi_sel_parse_read_record_type (host_data->host_poll->sel_ctx,
                                       NULL,
                                       0,
                                       &record_type) < 0)
    {
      if (_sel_parse_err_handle (host_data, "ipmi_sel_parse_read_record_type") < 0)
        goto cleanup;
      goto out;
    }

  /* IPMI Workaround
   *
   * HP DL 380 G5
   * Intel S2600JF/Appro 512X
   *
   * Motherboard is reporting invalid SEL Records types (0x00 on HP DL
   * 380 G5, 0x03 on Intel S2600JF/Appro 512X)
   */
  if (host_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT
      && (!IPMI_SEL_RECORD_TYPE_VALID (record_type)))
    record_type = IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (host_data->prog_data->args->system_event_only
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    goto out;

  if (host_data->prog_data->args->oem_event_only
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    goto out;

  if (host_data->prog_data->event_state_filter_mask)
    {
      char sel_record[IPMI_SEL_RECORD_MAX_RECORD_LENGTH];
      int sel_record_len;
      unsigned int event_state = 0;

      if ((sel_record_len = ipmi_sel_parse_read_record (host_data->host_poll->sel_ctx,
							sel_record,
							IPMI_SEL_RECORD_MAX_RECORD_LENGTH)) < 0)
	{
	  if (_sel_parse_err_handle (host_data, "ipmi_sel_parse_read_record_type") < 0)
	    goto cleanup;
	  goto out;
	}
      
      if (ipmi_interpret_sel (host_data->host_poll->interpret_ctx,
			      sel_record,
			      sel_record_len,
			      &event_state) < 0)
	{
	  err_output ("ipmi_interpret_sel: %s",
		      ipmi_interpret_ctx_errormsg (host_data->host_poll->interpret_ctx));
	  goto cleanup;
	}

      if ((host_data->prog_data->event_state_filter_mask & IPMISELD_NOMINAL_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_NOMINAL)
	goto out;

      if ((host_data->prog_data->event_state_filter_mask & IPMISELD_WARNING_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_WARNING)
	goto out;

      if ((host_data->prog_data->event_state_filter_mask & IPMISELD_CRITICAL_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_CRITICAL)
	goto out;

      if ((host_data->prog_data->event_state_filter_mask & IPMISELD_NA_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_UNKNOWN)
	goto out;
    }

  if (_sel_log_output (host_data, record_type) < 0)
    goto cleanup;

 out:
  rv = 0;
 cleanup:
  return (rv);
}

static int
ipmiseld_sel_parse_test_run (ipmiseld_host_data_t *host_data)
{
  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->sel_ctx);

  if (ipmi_sel_parse (host_data->host_poll->sel_ctx,
		      IPMI_SEL_RECORD_ID_FIRST,
		      IPMI_SEL_RECORD_ID_LAST,
		      _sel_parse_callback,
		      host_data) < 0)
    {
      err_output ("ipmi_sel_parse: %s",
		  ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
      return (-1);
    }

  return (0);
}

static void
_dump_sel_info (ipmiseld_host_data_t *host_data,
		ipmiseld_sel_info_t *sel_info,
		const char *prefix)
{
  assert (host_data);
  assert (host_data->prog_data->args->foreground);
  assert (host_data->prog_data->args->common_args.debug);
  assert (sel_info);
  assert (prefix);

  IPMISELD_HOST_DEBUG (("%s: Entries = %u", prefix, sel_info->entries));
  IPMISELD_HOST_DEBUG (("%s: Free Space = %u", prefix, sel_info->free_space));
  IPMISELD_HOST_DEBUG (("%s: Most Recent Addition Timestamp = %u", prefix, sel_info->most_recent_addition_timestamp));
  IPMISELD_HOST_DEBUG (("%s: Most Recent Erase Timestamp = %u", prefix, sel_info->most_recent_erase_timestamp));
  IPMISELD_HOST_DEBUG (("%s: Delete Sel Command Supported = %u", prefix, sel_info->delete_sel_command_supported));
  IPMISELD_HOST_DEBUG (("%s: Reserve Sel Command Supported = %u", prefix, sel_info->reserve_sel_command_supported));
  IPMISELD_HOST_DEBUG (("%s: Overflow Flag = %u", prefix, sel_info->overflow_flag));
} 

static void
_dump_host_state (ipmiseld_host_data_t *host_data,
		  const char *prefix)
{
  assert (host_data);
  assert (host_data->prog_data->args->foreground);
  assert (host_data->prog_data->args->common_args.debug);
  assert (prefix);

  IPMISELD_HOST_DEBUG (("%s: Last Record ID = %u", prefix, host_data->host_state.last_record_id.record_id));
  IPMISELD_HOST_DEBUG (("%s: Last Percent Full = %u", prefix, host_data->host_state.last_percent_full));
  _dump_sel_info (host_data, &(host_data->host_state.sel_info), prefix);
}

static int
ipmiseld_sel_parse_log (ipmiseld_host_data_t *host_data)
{
  ipmiseld_sel_info_t sel_info;
  uint16_t record_id_start;
  int log_entries_flag = 0;
  int do_clear_flag = 0;
  int percent;
  int ret;

  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->sel_ctx);

  /*
    XXX go through error handling here, still ok? fallthrough for save
    SEL info, what if parse fail still do clear, etc.
   */

  if (!host_data->host_state.initialized)
    {
      /* XXX should get from file later */
      if (_ipmiseld_host_state_init (host_data) < 0)
	return (-1);

      if (host_data->prog_data->args->foreground
	  && host_data->prog_data->args->common_args.debug)
	_dump_host_state (host_data, "Initial State");

      return (0);
    }
  
  if (_ipmi_sel_info_get (host_data, &sel_info) < 0)
    return (-1);

  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug)
    {
      _dump_host_state (host_data, "Last State");
      _dump_sel_info (host_data, &sel_info, "Current State");
    }
  
  if (sel_info.most_recent_addition_timestamp < host_data->host_state.sel_info.most_recent_addition_timestamp
      || sel_info.most_recent_erase_timestamp < host_data->host_state.sel_info.most_recent_erase_timestamp)
    {
      /* This shouldn't be possible under normal circumstances, but
       * could occur if the user changes the SEL timestamp or clock.
       * Or perhaps a vendor firmware update or similar action
       * modified the clock.
       *
       * Under this circumstance, we will treat the timestamps has
       * having changed (note that all checks below are for "not equal
       * to" and not "greater than" or "less than".  We just log to
       * note this.
       */
      ipmiseld_syslog_host (host_data, "SEL timestamps modified to earlier time");
    }

  if (sel_info.entries == host_data->host_state.sel_info.entries)
    {
      /* Small chance entry count is the same after a
       * out-of-daemon clear.  Need to do some checks to handle
       * for this
       */
      
      /* Timestamps unchanged - this is the most common/normal case, no new log entries to log. */
      if (sel_info.most_recent_addition_timestamp == host_data->host_state.sel_info.most_recent_addition_timestamp
	  && sel_info.most_recent_erase_timestamp == host_data->host_state.sel_info.most_recent_erase_timestamp)
	{
	  /* nothing to do */
	  ;
	}
      /* If erase timestamp changed but addition timestamp has
       * not.  An out-of-daemon delete/clear occurred, but
       * there are no new entries to log.
       */
      else if (sel_info.most_recent_addition_timestamp == host_data->host_state.sel_info.most_recent_addition_timestamp
	       && sel_info.most_recent_erase_timestamp != host_data->host_state.sel_info.most_recent_erase_timestamp)
	{
	  if (sel_info.delete_sel_command_supported)
	    {
	      /* We don't know if the erase was for some old entries or if it was a clear.
	       * We will look at the last_record_id to take a guess
	       */
	      ipmiseld_last_record_id_t last_record_id;
	      
	      if (_ipmiseld_last_record_id (host_data, &last_record_id) < 0)
		return (-1);
	      
	      /* If new last_record_id has changed, we assume the erase was a clear */
	      if (last_record_id.loaded
		  && last_record_id.record_id != host_data->host_state.last_record_id.record_id)
		host_data->host_state.last_record_id.record_id = 0;
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear.
	       * Reset last_record_id to zero. 
	       */
	      host_data->host_state.last_record_id.record_id = 0;
	    }
	}
      /* An erase and addition occured, must determine the type of action that occurred */ 
      else if (sel_info.most_recent_addition_timestamp != host_data->host_state.sel_info.most_recent_addition_timestamp
	       && sel_info.most_recent_erase_timestamp != host_data->host_state.sel_info.most_recent_erase_timestamp)
	{
	  if (sel_info.delete_sel_command_supported)
	    {
	      /* We don't know if the erase was for some old entries or if it was a clear.
	       * We will look at the last_record_id to take a guess
	       */
	      ipmiseld_last_record_id_t last_record_id;
	      
	      if (_ipmiseld_last_record_id (host_data, &last_record_id) < 0)
		return (-1);
	      
	      /* If new last_record_id is greater, we assume it's some additional entries
	       * and the erase was only deleting some old entries.
	       */
	      if (last_record_id.loaded
		  && last_record_id.record_id > host_data->host_state.last_record_id.record_id)
		{
		  if (host_data->host_state.last_record_id.record_id)
		    record_id_start = host_data->host_state.last_record_id.record_id;
		  else
		    record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	      else
		{
		  /* We assume a clear occurred so start from the beginning */
		  host_data->host_state.last_record_id.record_id = 0;
		  record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear
	       * So log all the new entries if some are available and
	       * reset last_record_id to zero.
	       */
	      host_data->host_state.last_record_id.record_id = 0;
	      if (sel_info.entries)
		{
		  record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	}
      else /* sel_info.most_recent_addition_timestamp != host_data->host_state.sel_info.most_recent_addition_timestamp
	      && sel_info.most_recent_erase_timestamp == host_data->host_state.sel_info.most_recent_erase_timestamp */
	{
	  /* This shouldn't be possible and is likely a bug in the
	   * IPMI firmware (user erased entries but timestamp didn't
	   * update, SEL added entries and updated timestamp but
	   * didn't update entry count, etc.) we'll only save off the
	   * sel info for later.
	   */
	  ipmiseld_syslog_host (host_data, "SEL illegal timestamp situation");
	}
    }
  else if (sel_info.entries > host_data->host_state.sel_info.entries)
    {
      ipmiseld_last_record_id_t last_record_id;
      
      if (_ipmiseld_last_record_id (host_data, &last_record_id) < 0)
	return (-1);
	  
      /* There is a small race chance that the last time we got sel
       * info, a new SEL event occurred after it, but before the call
       * to ipmi_sel_parse().  So we check what the last record id to
       * see if that happened.  If the last record id is the same,
       * then we already logged it.  So no new logging needs to
       * happen.
       */ 
      if (last_record_id.loaded
	  && host_data->host_state.last_record_id.record_id == last_record_id.record_id)
	{
	  /* nothing to do */
	  ;
	}
      else if (sel_info.most_recent_erase_timestamp == host_data->host_state.sel_info.most_recent_erase_timestamp)
	{
	  /* This is the most normal case we should expect, there
	   * are more entries in the SEL than last time we checked
	   * and must log them.
	   */
	  if (host_data->host_state.last_record_id.record_id)
	    record_id_start = host_data->host_state.last_record_id.record_id;
	  else
	    record_id_start = IPMI_SEL_RECORD_ID_FIRST;
	  log_entries_flag++;
	}
      else
	{
	  if (sel_info.delete_sel_command_supported)
	    {
	      /* If new last_record_id is greater, we assume it's some additional entries
	       * and the erase was only deleting some old entries.
	       */
	      if (last_record_id.loaded
		  && last_record_id.record_id > host_data->host_state.last_record_id.record_id)
		{
		  if (host_data->host_state.last_record_id.record_id)
		    record_id_start = host_data->host_state.last_record_id.record_id;
		  else
		    record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	      else
		{
		  /* We assume a clear occurred so start from the beginning */
		  host_data->host_state.last_record_id.record_id = 0;
		  record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear
	       * So log all the new entries if some are available and
	       * reset last_record_id to zero.
	       */
	      host_data->host_state.last_record_id.record_id = 0;
	      if (sel_info.entries)
		{
		  record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	}
    }
  else /* sel_info.entries < host_data->host_state.sel_info.entries) */
    {
      if (sel_info.most_recent_erase_timestamp == host_data->host_state.sel_info.most_recent_erase_timestamp)
	{
	  /* This shouldn't be possible and is likely a bug in the
	   * IPMI firmware.  Log this, but for rest of this chunk of
	   * code, we assume the erase timestamp must have changed.
	   */
	  ipmiseld_syslog_host (host_data, "SEL timestamp error, fewer entries without erase");
	}

      /* if no additional entries, nothing to log */
      if (sel_info.most_recent_addition_timestamp != host_data->host_state.sel_info.most_recent_addition_timestamp)
	{
	  if (sel_info.delete_sel_command_supported)
	    {
	      /* We don't know if the erase was for some old entries or if it was a clear.
	       * We will look at the last_record_id to take a guess
	       */
	      ipmiseld_last_record_id_t last_record_id;
	      
	      if (_ipmiseld_last_record_id (host_data, &last_record_id) < 0)
		return (-1);
	      
	      /* If new last_record_id is greater, we assume it's some additional entries
	       * and the erase was only deleting some old entries.
	       */
	      if (last_record_id.loaded
		  && last_record_id.record_id > host_data->host_state.last_record_id.record_id)
		{
		  if (host_data->host_state.last_record_id.record_id)
		    record_id_start = host_data->host_state.last_record_id.record_id;
		  else
		    record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	      else
		{
		  /* We assume a clear occurred so start from the beginning */
		  host_data->host_state.last_record_id.record_id = 0;
		  record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear
	       * So log all the new entries if some are available and
	       * reset last_record_id to zero.
	       */
	      host_data->host_state.last_record_id.record_id = 0;
	      if (sel_info.entries)
		{
		  record_id_start = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	}
      else
	{
	  if (!sel_info.entries)
	    host_data->host_state.last_record_id.record_id = 0;
	}
    }

  percent = _ipmiseld_calc_percent_full (host_data, &sel_info);

  if (host_data->prog_data->args->warning_threshold)
    {
      if (percent > host_data->prog_data->args->warning_threshold)
	{
	  if (percent > host_data->host_state.last_percent_full)
	    ipmiseld_syslog_host (host_data, "SEL is %d%% full", percent);
	}
    }

  if (!host_data->host_state.sel_info.overflow_flag
      && sel_info.overflow_flag)
    ipmiseld_syslog_host (host_data, "SEL Overflow, events have been dropped due to lack of space in the SEL");
  
  if (host_data->prog_data->args->clear_threshold)
    {
      if (percent > host_data->prog_data->args->clear_threshold)
	{
	  if (sel_info.reserve_sel_command_supported)
	    {
	      /* No error output, we can do it without it */
	      if (ipmi_sel_ctx_register_reservation_id (host_data->host_poll->sel_ctx, NULL) < 0)
		err_output ("ipmi_sel_ctx_register_reservation_id: %s",
			    ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
	      else
		do_clear_flag = 1;
	    }
	  else
	    /* If can't reserve, gotta do the best we can */
	    do_clear_flag = 1;
	}

    }

  host_data->host_state.last_percent_full = percent;

  if (log_entries_flag)
    {
      if (ipmi_sel_parse (host_data->host_poll->sel_ctx,
			  record_id_start,
			  IPMI_SEL_RECORD_ID_LAST,
			  _sel_parse_callback,
			  host_data) < 0)
	{
	  err_output ("ipmi_sel_parse: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
	  /* XXX do clear? do save info? */
	  return (-1);
	}
    }
  
  if (do_clear_flag)
    {
      if (sel_info.reserve_sel_command_supported)
	{
	  if ((ret = ipmi_sel_clear_sel (host_data->host_poll->sel_ctx)) < 0)
	    {
	      if (ipmi_sel_ctx_errnum (host_data->host_poll->sel_ctx) == IPMI_SEL_ERR_RESERVATION_CANCELED)
		{
		  /* XXX do something */
		  /* try again later?? deal with timeouts or something to handle this */
		}
	      else
		err_output ("ipmi_sel_clear_sel: %s",
			    ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
	    }
	}
      else
	{
	  if ((ret = ipmi_sel_clear_sel (host_data->host_poll->sel_ctx)) < 0)
	    err_output ("ipmi_sel_clear_sel: %s",
			ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));

	} 

      if (!ret)
	{
	  ipmiseld_syslog_host (host_data, "SEL cleared");

	  /* XXX -1 on error? */
	  if (_ipmi_sel_info_get (host_data, &sel_info) < 0)
	    return (-1);
	  host_data->host_state.last_record_id.record_id = 0;
	}
    }

  memcpy (&host_data->host_state.sel_info, &sel_info, sizeof (ipmiseld_sel_info_t));
  /* XXX save disk */
  
  return (0);
}

static int
ipmiseld_sel_parse (ipmiseld_host_data_t *host_data)
{
  assert (host_data);

  if (host_data->prog_data->args->test_run)
    return (ipmiseld_sel_parse_test_run (host_data));

  return (ipmiseld_sel_parse_log (host_data));
}

static int
_ipmi_setup (ipmiseld_host_data_t *host_data)
{
  struct common_cmd_args *common_args;
  unsigned int workaround_flags = 0;
  int rv = -1;

  assert (host_data);
  assert (host_data->host_poll);

  common_args = &(host_data->prog_data->args->common_args);

  if (!(host_data->host_poll->ipmi_ctx = ipmi_ctx_create ()))
    {
      err_output ("ipmi_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (host_data->hostname
      && strcasecmp (host_data->hostname, "localhost") != 0
      && strcmp (host_data->hostname, "127.0.0.1") != 0)
    {
      if (common_args->driver_type == IPMI_DEVICE_LAN_2_0)
        {
          parse_get_freeipmi_outofband_2_0_flags (common_args->workaround_flags_outofband_2_0,
                                                  &workaround_flags);
          
          if (ipmi_ctx_open_outofband_2_0 (host_data->host_poll->ipmi_ctx,
                                           host_data->hostname,
                                           common_args->username,
                                           common_args->password,
                                           (common_args->k_g_len) ? common_args->k_g : NULL,
                                           (common_args->k_g_len) ? common_args->k_g_len : 0,
                                           common_args->privilege_level,
                                           common_args->cipher_suite_id,
                                           common_args->session_timeout,
                                           common_args->retransmission_timeout,
                                           workaround_flags,
                                           (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_open_outofband_2_0: %s", ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_ctx_open_outofband (host_data->host_poll->ipmi_ctx,
                                       host_data->hostname,
                                       common_args->username,
                                       common_args->password,
                                       common_args->authentication_type,
                                       common_args->privilege_level,
                                       common_args->session_timeout,
                                       common_args->retransmission_timeout,
                                       workaround_flags,
                                       (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_open_outofband: %s", ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
              goto cleanup;
            }
        }
    }
  else
    {
      if (!ipmi_is_root ())
        {
	  err_output ("%s", ipmi_ctx_strerror (IPMI_ERR_PERMISSION));
          goto cleanup;
        }

      parse_get_freeipmi_inband_flags (common_args->workaround_flags_inband,
                                       &workaround_flags);

      if (common_args->driver_type == IPMI_DEVICE_UNKNOWN)
        {
          int ret;

          if ((ret = ipmi_ctx_find_inband (host_data->host_poll->ipmi_ctx,
                                           NULL,
                                           common_args->disable_auto_probe,
                                           common_args->driver_address,
                                           common_args->register_spacing,
                                           common_args->driver_device,
                                           workaround_flags,
                                           (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT)) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_find_inband: %s", ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
              goto cleanup;
            }

          if (!ret)
            {
	      /* XXX deal w/ specific errors */
              err_output ("could not find inband device");
              goto cleanup;
            }
        }
      else
        {
	  if (ipmi_ctx_open_inband (host_data->host_poll->ipmi_ctx,
                                    common_args->driver_type,
                                    common_args->disable_auto_probe,
                                    common_args->driver_address,
                                    common_args->register_spacing,
                                    common_args->driver_device,
                                    workaround_flags,
                                    (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_open_inband: %s", ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
              goto cleanup;
            }
        }
    }

  if (common_args->target_channel_number_is_set
      || common_args->target_slave_address_is_set)
    {
      if (ipmi_ctx_set_target (host_data->host_poll->ipmi_ctx,
                               common_args->target_channel_number_is_set ? &common_args->target_channel_number : NULL,
                               common_args->target_slave_address_is_set ? &common_args->target_slave_address : NULL) < 0)
        {
	  err_output ("ipmi_ctx_set_target: %s", ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
          goto cleanup;
        } 
    }
  
  rv = 0;
 cleanup:
  if (rv < 0)
    {
      ipmi_ctx_close (host_data->host_poll->ipmi_ctx);
      ipmi_ctx_destroy (host_data->host_poll->ipmi_ctx);
    }
  return (rv);
}

static int
_ipmiseld_poll (ipmiseld_host_data_t *host_data)
{
  ipmiseld_host_poll_t host_poll;
  unsigned int sel_flags = 0;
  unsigned int interpret_flags = 0;
  int exit_code = EXIT_FAILURE;

  assert (host_data);
  assert (!host_data->host_poll);

  memset (&host_poll, '\0', sizeof (ipmiseld_host_poll_t));
  host_data->host_poll = &host_poll;

  if (_ipmi_setup (host_data) < 0)
    goto cleanup;

  if (ipmiseld_sdr_cache_create_and_load (host_data) < 0)
    goto cleanup;
  
  if (!(host_data->host_poll->sel_ctx = ipmi_sel_ctx_create (host_data->host_poll->ipmi_ctx, host_data->host_poll->sdr_ctx)))
    {
      err_output ("ipmi_sel_ctx_create: %s", strerror (errno));
      goto cleanup;
    }
  
  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug > 1)
    sel_flags |= IPMI_SEL_FLAGS_DEBUG_DUMP;
  
  if (host_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT)
    sel_flags |= IPMI_SEL_FLAGS_ASSUME_SYTEM_EVENT_RECORDS;
  
  if (sel_flags)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sel_ctx_set_flags (host_data->host_poll->sel_ctx, sel_flags) < 0)
	err_output ("ipmi_sel_ctx_set_flags: %s",
		    ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
    }

  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug > 1
      && host_data->hostname)
    {
      if (ipmi_sel_ctx_set_debug_prefix (host_data->host_poll->sel_ctx, host_data->hostname) < 0)
        err_output ("ipmi_sel_ctx_set_debug_prefix: %s",
		    ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
    }
  
  if (!(host_data->host_poll->interpret_ctx = ipmi_interpret_ctx_create ()))
    {
      err_output ("ipmi_interpret_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (ipmi_interpret_load_sel_config (host_data->host_poll->interpret_ctx,
				      host_data->prog_data->args->event_state_config_file) < 0)
    {
      /* if default file is missing its ok */
      if (!(!host_data->prog_data->args->event_state_config_file
	    && ipmi_interpret_ctx_errnum (host_data->host_poll->interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST))
	{
	  err_output ("ipmi_interpret_load_sel_config: %s", ipmi_interpret_ctx_errormsg (host_data->host_poll->interpret_ctx));
	  goto cleanup;
        }
    }

  if (host_data->prog_data->args->interpret_oem_data)
    interpret_flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;
  
  if (interpret_flags)
    {
      if (ipmi_interpret_ctx_set_flags (host_data->host_poll->interpret_ctx, interpret_flags) < 0)
	{
	  err_output ("ipmi_interpret_ctx_set_flags: %s",
		      ipmi_interpret_ctx_errormsg (host_data->host_poll->interpret_ctx));
	  goto cleanup;
	}
    }

  if (ipmi_sel_ctx_set_parameter (host_data->host_poll->sel_ctx,
				  IPMI_SEL_PARAMETER_INTERPRET_CONTEXT,
				  &(host_data->host_poll->interpret_ctx)) < 0)
    {
      err_output("ipmi_sel_ctx_set_interpret: %s",
		 ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
      goto cleanup;
    }
  
  if (ipmi_sel_ctx_set_separator (host_data->host_poll->sel_ctx, EVENT_OUTPUT_SEPARATOR) < 0)
    {
      err_output ("ipmi_sel_parse: %s",
		  ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
      return (-1);
    }

  if (host_data->prog_data->args->interpret_oem_data
      || host_data->prog_data->args->output_oem_event_strings)
    {
      if (ipmi_get_oem_data (NULL,
                             host_data->host_poll->ipmi_ctx,
                             &host_data->host_poll->oem_data) < 0)
        return (-1);

      if (ipmi_sel_ctx_set_manufacturer_id (host_data->host_poll->sel_ctx,
                                            host_data->host_poll->oem_data.manufacturer_id) < 0)
        {
          err_output ("ipmi_sel_ctx_set_manufacturer_id: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
          return (-1);
        }
      
      if (ipmi_sel_ctx_set_product_id (host_data->host_poll->sel_ctx,
                                       host_data->host_poll->oem_data.product_id) < 0)
        {
          err_output ("ipmi_sel_ctx_set_product_id: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
          return (-1);
        }

      if (ipmi_sel_ctx_set_ipmi_version (host_data->host_poll->sel_ctx,
                                         host_data->host_poll->oem_data.ipmi_version_major,
                                         host_data->host_poll->oem_data.ipmi_version_minor) < 0)
        {
          err_output ("ipmi_sel_ctx_set_ipmi_version: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
          return (-1);
        }
      
      if (host_data->prog_data->args->interpret_oem_data)
        {
          if (ipmi_interpret_ctx_set_manufacturer_id (host_data->host_poll->interpret_ctx,
                                                      host_data->host_poll->oem_data.manufacturer_id) < 0)
            {
              err_output ("ipmi_interpret_ctx_set_manufacturer_id: %s",
			  ipmi_interpret_ctx_errormsg (host_data->host_poll->interpret_ctx));
              return (-1);
            }
	  
          if (ipmi_interpret_ctx_set_product_id (host_data->host_poll->interpret_ctx,
                                                 host_data->host_poll->oem_data.product_id) < 0)
            {
              err_output ("ipmi_interpret_ctx_set_product_id: %s",
			  ipmi_interpret_ctx_errormsg (host_data->host_poll->interpret_ctx));
              return (-1);
            }
        }
    }

  if (ipmiseld_sel_parse (host_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_interpret_ctx_destroy (host_data->host_poll->interpret_ctx);
  ipmi_sel_ctx_destroy (host_data->host_poll->sel_ctx);
  ipmi_sdr_ctx_destroy (host_data->host_poll->sdr_ctx);
  ipmi_ctx_close (host_data->host_poll->ipmi_ctx);
  ipmi_ctx_destroy (host_data->host_poll->ipmi_ctx);
  host_data->host_poll = NULL;
  return (exit_code);
}

static void
_signal_handler_callback (int sig)
{
  exit_flag = 0;
}

static int
_ipmiseld (ipmiseld_prog_data_t *prog_data)
{
  ipmiseld_host_data_t host_data;

  /* XXX deal w/ hostrange later */
  memset (&host_data, '\0', sizeof (ipmiseld_host_data_t));
  host_data.prog_data = prog_data;
  host_data.hostname = prog_data->args->common_args.hostname;
  host_data.host_poll = NULL;

  if (host_data.prog_data->args->test_run)
    return (_ipmiseld_poll (&host_data));
  else
    {
      while (exit_flag)
	{
	  if (host_data.prog_data->args->foreground
	      && host_data.prog_data->args->common_args.debug)
	    IPMISELD_DEBUG (("Poll %s", host_data.hostname ? host_data.hostname : "localhost"));
	  
	  /* XXX vary timeout based on error? */ 
	  _ipmiseld_poll (&host_data);

	  daemon_sleep (host_data.prog_data->args->poll_interval);
 	}
    }

  return (0);
}

int
main (int argc, char **argv)
{
  ipmiseld_prog_data_t prog_data;
  struct ipmiseld_arguments cmd_args;

  err_init (argv[0]);
  err_set_flags (ERROR_STDERR);

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmiseld_prog_data_t));
  prog_data.progname = argv[0];
  ipmiseld_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if (prog_data.args->event_state_filter_str)
    prog_data.event_state_filter_mask = ipmiseld_event_state_filter_parse (prog_data.args->event_state_filter_str);
  else
    prog_data.event_state_filter_mask = 0;

  if (prog_data.args->log_facility_str)
    prog_data.log_facility = ipmiseld_log_facility_parse (prog_data.args->log_facility_str);
  else
    prog_data.log_facility = LOG_DAEMON;

  if (prog_data.args->log_priority_str)
    prog_data.log_priority = ipmiseld_log_priority_parse (prog_data.args->log_priority_str);
  else
    prog_data.log_priority = LOG_ERR;

  if (!cmd_args.test_run)
    {
      if (!cmd_args.foreground)
	{
	  daemonize_common (IPMISELD_PIDFILE);
	  err_set_flags (ERROR_SYSLOG);
	}
      else
	err_set_flags (ERROR_STDERR);
      
      daemon_signal_handler_setup (_signal_handler_callback);

      /* Call after daemonization, since daemonization closes currently
       * open fds
       */
      openlog (argv[0], LOG_ODELAY | LOG_PID, prog_data.log_facility);
    }
  
  return (_ipmiseld (&prog_data));
}
