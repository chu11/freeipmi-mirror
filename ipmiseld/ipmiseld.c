/*****************************************************************************\
 *  $Id: ipmiseld.c,v 1.17 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.
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
#include <syslog.h>
#include <pthread.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmiseld.h"
#include "ipmiseld-argp.h"
#include "ipmiseld-cache.h"
#include "ipmiseld-common.h"
#include "ipmiseld-debug.h"
#include "ipmiseld-ipmi-communication.h"
#include "ipmiseld-threadpool.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "heap.h"
#include "hostlist.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-daemon-common.h"
#include "tool-event-common.h"
#include "tool-util-common.h"

#define IPMISELD_PIDFILE                IPMISELD_LOCALSTATEDIR "/run/ipmiseld.pid"

#define IPMISELD_FORMAT_BUFLEN          4096

#define IPMISELD_EVENT_OUTPUT_BUFLEN    4096

#define IPMISELD_RETRY_ATTEMPT_MAX      3

static Heap host_data_heap = NULL;
static pthread_mutex_t host_data_heap_lock = PTHREAD_MUTEX_INITIALIZER;

static int exit_flag = 1;

static int
ipmiseld_sel_info_get (ipmiseld_host_data_t *host_data, ipmiseld_sel_info_t *sel_info)
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
      ipmiseld_err_output (host_data, "fiid_obj_create: %s", strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sel_info (host_data->host_poll->ipmi_ctx, obj_cmd_rs) < 0)
    {
      ipmiseld_err_output (host_data, "ipmi_cmd_get_sel_info: %s",
		  ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "entries", &val) < 0)
    {
      ipmiseld_err_output (host_data, "fiid_obj_get: 'entries': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->entries = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "free_space", &val) < 0)
    {
      ipmiseld_err_output (host_data, "fiid_obj_get: 'free_space': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->free_space = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "most_recent_addition_timestamp", &val) < 0)
    {
      ipmiseld_err_output (host_data, "fiid_obj_get: 'most_recent_addition_timestamp': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->most_recent_addition_timestamp = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "most_recent_erase_timestamp", &val) < 0)
    {
      ipmiseld_err_output (host_data, "fiid_obj_get: 'most_recent_erase_timestamp': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->most_recent_erase_timestamp = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "delete_sel_command_supported", &val) < 0)
    {
      ipmiseld_err_output (host_data, "fiid_obj_get: 'delete_sel_command_supported': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
 sel_info->delete_sel_command_supported = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "reserve_sel_command_supported", &val) < 0)
    {
      ipmiseld_err_output (host_data, "fiid_obj_get: 'reserve_sel_command_supported': %s",
		  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sel_info->reserve_sel_command_supported = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "overflow_flag", &val) < 0)
    {
      ipmiseld_err_output (host_data, "fiid_obj_get: 'overflow_flag': %s",
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
ipmiseld_get_last_record_id (ipmiseld_host_data_t *host_data,
			     ipmiseld_last_record_id_t *last_record_id)
{
  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->sel_ctx);
  assert (last_record_id);

  last_record_id->loaded = 0;

  if (ipmi_sel_parse (host_data->host_poll->sel_ctx,
		      IPMI_SEL_RECORD_ID_LAST,
		      IPMI_SEL_RECORD_ID_LAST,
		      _sel_last_record_id_callback,
		      last_record_id) < 0)
    {
      /* A general IPMI error (busy, timeout, etc.) is ok, it happens */
      if (ipmi_sel_ctx_errnum (host_data->host_poll->sel_ctx) != IPMI_SEL_ERR_IPMI_ERROR)
	ipmiseld_err_output (host_data, "ipmi_sel_parse: %s", ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
      return (-1);
    }

  return (0);
}

static unsigned int
ipmiseld_calc_percent_full (ipmiseld_host_data_t *host_data,
			    ipmiseld_sel_info_t *sel_info)
{
  unsigned int used_bytes;
  unsigned int total_bytes;
  unsigned int percent;

  assert (host_data);
  assert (sel_info);

  used_bytes = (sel_info->entries * IPMI_SEL_RECORD_MAX_RECORD_LENGTH);
  total_bytes = used_bytes + sel_info->free_space;
  percent = (int)(100 * (double)used_bytes/total_bytes); 

  if (percent > 100)
    {
      /* Some rounding errors could occur, we accept small ones */
      if (percent > 105)
	{
	  if (host_data->prog_data->args->verbose_count)
	    ipmiseld_syslog_host (host_data, "SEL percent calc error: %u", percent);
	}
      percent = 100;
    }

  return (percent);
}

static int
ipmiseld_host_state_init (ipmiseld_host_data_t *host_data)
{
  unsigned int percent;
  int rv = -1;

  assert (host_data);

  if (ipmiseld_get_last_record_id (host_data, &(host_data->last_host_state.last_record_id)) < 0)
    goto cleanup;
  
  /* possible SEL is empty */
  if (!host_data->last_host_state.last_record_id.loaded)
    {
      host_data->last_host_state.last_record_id.record_id = 0;
      host_data->last_host_state.last_record_id.loaded = 1;
    }
  
  if (ipmiseld_sel_info_get (host_data, &(host_data->last_host_state.sel_info)) < 0)
    goto cleanup;

  percent = ipmiseld_calc_percent_full (host_data, &(host_data->last_host_state.sel_info));
  host_data->last_host_state.last_percent_full = percent; 
  
  host_data->last_host_state.initialized = 1;
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
      if (host_data->prog_data->args->verbose_count)
	ipmiseld_syslog_host (host_data, "Invalid SEL entry read");
      return (0);
    }

  ipmiseld_err_output (host_data, "%s: %s",
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
      ipmiseld_err_output (host_data, "ipmi_sel_parse_read_record_id: %s",
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
  if (host_data->now_host_state.last_record_id.record_id == record_id)
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
    {
      if (host_data->prog_data->args->system_event_format_str)
	format_str = host_data->prog_data->args->system_event_format_str;
      else
	{
	  if (host_data->hostname)
	    format_str = IPMISELD_SYSTEM_EVENT_FORMAT_OUTOFBAND_STR_DEFAULT;
	  else
	    format_str = IPMISELD_SYSTEM_EVENT_FORMAT_STR_DEFAULT;
	}
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      if (host_data->prog_data->args->oem_timestamped_event_format_str)
	format_str = host_data->prog_data->args->oem_timestamped_event_format_str;
      else
	{
	  if (host_data->hostname)
	    format_str = IPMISELD_OEM_TIMESTAMPED_EVENT_FORMAT_OUTOFBAND_STR_DEFAULT;
	  else
	    format_str = IPMISELD_OEM_TIMESTAMPED_EVENT_FORMAT_STR_DEFAULT;
	}
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    {
      if (host_data->prog_data->args->oem_non_timestamped_event_format_str)
	format_str = host_data->prog_data->args->oem_non_timestamped_event_format_str;
      else
	{
	  if (host_data->hostname)
	    format_str = IPMISELD_OEM_NON_TIMESTAMPED_EVENT_FORMAT_OUTOFBAND_STR_DEFAULT;
	  else
	    format_str = IPMISELD_OEM_NON_TIMESTAMPED_EVENT_FORMAT_STR_DEFAULT;
	}
    }
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

  host_data->now_host_state.last_record_id.record_id = record_id; 
  
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
	  ipmiseld_err_output (host_data, "ipmi_interpret_sel: %s",
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
      /* A general IPMI error (busy, timeout, etc.) is ok, it happens */
      if (ipmi_sel_ctx_errnum (host_data->host_poll->sel_ctx) != IPMI_SEL_ERR_IPMI_ERROR)
	ipmiseld_err_output (host_data, "ipmi_sel_parse: %s",
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
		  ipmiseld_host_state_t *host_state,
		  const char *prefix)
{
  assert (host_data);
  assert (host_data->prog_data->args->foreground);
  assert (host_data->prog_data->args->common_args.debug);
  assert (host_state);
  assert (prefix);

  IPMISELD_HOST_DEBUG (("%s: Last Record ID = %u", prefix, host_state->last_record_id.record_id));
  IPMISELD_HOST_DEBUG (("%s: Last Percent Full = %u", prefix, host_state->last_percent_full));
  _dump_sel_info (host_data, &(host_state->sel_info), prefix);
}

/* returns 1 to log events, 0 if not, -1 on error */
static int
ipmiseld_check_sel_info (ipmiseld_host_data_t *host_data, uint16_t *record_id_start)
{
  int log_entries_flag = 0;
  int rv = -1;

  assert (host_data);
  assert (record_id_start);

  if (host_data->now_host_state.sel_info.most_recent_addition_timestamp < host_data->last_host_state.sel_info.most_recent_addition_timestamp
      || host_data->now_host_state.sel_info.most_recent_erase_timestamp < host_data->last_host_state.sel_info.most_recent_erase_timestamp)
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
      if (host_data->prog_data->args->verbose_count)
	ipmiseld_syslog_host (host_data, "SEL timestamps modified to earlier time");
    }

  if (host_data->now_host_state.sel_info.entries == host_data->last_host_state.sel_info.entries)
    {
      /* Small chance entry count is the same after a
       * out-of-daemon clear.  Need to do some checks to handle
       * for this
       */
      
      /* Timestamps unchanged - this is the most common/normal case, no new log entries to log. */
      if (host_data->now_host_state.sel_info.most_recent_addition_timestamp == host_data->last_host_state.sel_info.most_recent_addition_timestamp
	  && host_data->now_host_state.sel_info.most_recent_erase_timestamp == host_data->last_host_state.sel_info.most_recent_erase_timestamp)
	{
	  /* nothing to do except this single copy/save */
	  host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
	}
      /* If erase timestamp changed but addition timestamp has
       * not.  An out-of-daemon delete/clear occurred, but
       * there are no new entries to log.
       */
      else if (host_data->now_host_state.sel_info.most_recent_addition_timestamp == host_data->last_host_state.sel_info.most_recent_addition_timestamp
	       && host_data->now_host_state.sel_info.most_recent_erase_timestamp != host_data->last_host_state.sel_info.most_recent_erase_timestamp)
	{
	  if (host_data->now_host_state.sel_info.delete_sel_command_supported)
	    {
	      /* We don't know if the erase was for some old entries or if it was a clear.
	       * We will look at the last_record_id to take a guess
	       */
	      ipmiseld_last_record_id_t last_record_id;
	      
	      if (ipmiseld_get_last_record_id (host_data, &last_record_id) < 0)
		goto cleanup;
	      
	      /* If new last_record_id has changed or there are no
	       * records, we assume the erase was a clear
	       */
	      if (!last_record_id.loaded
		  || last_record_id.record_id != host_data->last_host_state.last_record_id.record_id)
		host_data->now_host_state.last_record_id.record_id = 0;
	      else
		host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear.
	       * Reset last_record_id to zero. 
	       */
	      host_data->now_host_state.last_record_id.record_id = 0;
	    }
	}
      /* An erase and addition occured, must determine the type of action that occurred */ 
      else if (host_data->now_host_state.sel_info.most_recent_addition_timestamp != host_data->last_host_state.sel_info.most_recent_addition_timestamp
	       && host_data->now_host_state.sel_info.most_recent_erase_timestamp != host_data->last_host_state.sel_info.most_recent_erase_timestamp)
	{
	  if (host_data->now_host_state.sel_info.delete_sel_command_supported)
	    {
	      /* We don't know if the erase was for some old entries or if it was a clear.
	       * We will look at the last_record_id to take a guess
	       */
	      ipmiseld_last_record_id_t last_record_id;
	      
	      if (ipmiseld_get_last_record_id (host_data, &last_record_id) < 0)
		goto cleanup;
	      
	      /* If new last_record_id is greater, we assume it's some additional entries
	       * and the erase was only deleting some old entries.
	       */
	      if (last_record_id.loaded
		  && last_record_id.record_id > host_data->last_host_state.last_record_id.record_id)
		{
		  host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
		  if (host_data->last_host_state.last_record_id.record_id)
		    (*record_id_start) = host_data->last_host_state.last_record_id.record_id;
		  else
		    (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	      else
		{
		  /* We assume a clear occurred so start from the beginning */
		  host_data->now_host_state.last_record_id.record_id = 0;
		  (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear
	       * So log all the new entries if some are available and
	       * reset last_record_id to zero.
	       */
	      host_data->now_host_state.last_record_id.record_id = 0;
	      if (host_data->now_host_state.sel_info.entries)
		{
		  (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	}
      else /* host_data->now_host_state.sel_info.most_recent_addition_timestamp != host_data->last_host_state.sel_info.most_recent_addition_timestamp
	      && host_data->now_host_state.sel_info.most_recent_erase_timestamp == host_data->last_host_state.sel_info.most_recent_erase_timestamp */
	{
	  /* This shouldn't be possible and is likely a bug in the
	   * IPMI firmware (user erased entries but timestamp didn't
	   * update, SEL added entries and updated timestamp but
	   * didn't update entry count, etc.) we'll only save off the
	   * host state for later.
	   */
	  if (host_data->prog_data->args->verbose_count)
	    ipmiseld_syslog_host (host_data, "SEL illegal timestamp situation");
	  host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
	}
    }
  else if (host_data->now_host_state.sel_info.entries > host_data->last_host_state.sel_info.entries)
    {
      ipmiseld_last_record_id_t last_record_id;
      
      if (host_data->now_host_state.sel_info.most_recent_addition_timestamp == host_data->last_host_state.sel_info.most_recent_addition_timestamp)
	{
	  /* This shouldn't be possible and is likely a bug in the
	   * IPMI firmware.  Log this, but for rest of this chunk of
	   * code, we assume the addition timestamp must have changed.
	   */
	  if (host_data->prog_data->args->verbose_count)
	    ipmiseld_syslog_host (host_data, "SEL timestamp error, more entries without addition");
	}

      if (ipmiseld_get_last_record_id (host_data, &last_record_id) < 0)
	goto cleanup;
	  
      /* There is a small race chance that the last time we got sel
       * info, a new SEL event occurred after it, but before the call
       * to ipmi_sel_parse().  So we check what the last record id to
       * see if that happened.  If the last record id is the same,
       * then we already logged it.  So no new logging needs to
       * happen.
       */ 
      if (last_record_id.loaded
	  && host_data->last_host_state.last_record_id.record_id == last_record_id.record_id)
	{
	  /* nothing to do except this single copy/save */
	  host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
	}
      else if (host_data->now_host_state.sel_info.most_recent_erase_timestamp == host_data->last_host_state.sel_info.most_recent_erase_timestamp)
	{
	  /* This is the most normal case we should expect, there
	   * are more entries in the SEL than last time we checked
	   * and must log them.
	   */
	  host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
	  if (host_data->last_host_state.last_record_id.record_id)
	    (*record_id_start) = host_data->last_host_state.last_record_id.record_id;
	  else
	    (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
	  log_entries_flag++;
	}
      else
	{
	  if (host_data->now_host_state.sel_info.delete_sel_command_supported)
	    {
	      /* If new last_record_id is greater, we assume it's some additional entries
	       * and the erase was only deleting some old entries.
	       */
	      if (last_record_id.loaded
		  && last_record_id.record_id > host_data->last_host_state.last_record_id.record_id)
		{
		  host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
		  if (host_data->last_host_state.last_record_id.record_id)
		    (*record_id_start) = host_data->last_host_state.last_record_id.record_id;
		  else
		    (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	      else
		{
		  /* We assume a clear occurred so start from the beginning */
		  host_data->now_host_state.last_record_id.record_id = 0;
		  (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear
	       * So log all the new entries if some are available and
	       * reset last_record_id to zero.
	       */
	      host_data->now_host_state.last_record_id.record_id = 0;
	      if (host_data->now_host_state.sel_info.entries)
		{
		  (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	}
    }
  else /* host_data->now_host_state.sel_info.entries < host_data->host_state.sel_info.entries) */
    {
      if (host_data->now_host_state.sel_info.most_recent_erase_timestamp == host_data->last_host_state.sel_info.most_recent_erase_timestamp)
	{
	  /* This shouldn't be possible and is likely a bug in the
	   * IPMI firmware.  Log this, but for rest of this chunk of
	   * code, we assume the erase timestamp must have changed.
	   */
	  if (host_data->prog_data->args->verbose_count)
	    ipmiseld_syslog_host (host_data, "SEL timestamp error, fewer entries without erase");
	}

      /* if no additional entries, nothing to log */
      if (host_data->now_host_state.sel_info.most_recent_addition_timestamp != host_data->last_host_state.sel_info.most_recent_addition_timestamp)
	{
	  if (host_data->now_host_state.sel_info.delete_sel_command_supported)
	    {
	      /* We don't know if the erase was for some old entries or if it was a clear.
	       * We will look at the last_record_id to take a guess
	       */
	      ipmiseld_last_record_id_t last_record_id;
	      
	      if (ipmiseld_get_last_record_id (host_data, &last_record_id) < 0)
		goto cleanup;
	      
	      /* If new last_record_id is greater, we assume it's some additional entries
	       * and the erase was only deleting some old entries.
	       */
	      if (last_record_id.loaded
		  && last_record_id.record_id > host_data->last_host_state.last_record_id.record_id)
		{
		  host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
		  if (host_data->last_host_state.last_record_id.record_id)
		    (*record_id_start) = host_data->last_host_state.last_record_id.record_id;
		  else
		    (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	      else
		{
		  /* We assume a clear occurred so start from the beginning */
		  host_data->now_host_state.last_record_id.record_id = 0;
		  (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	  else
	    {
	      /* If delete not supported, the erase must have been a clear
	       * So log all the new entries if some are available and
	       * reset last_record_id to zero.
	       */
	      host_data->now_host_state.last_record_id.record_id = 0;
	      if (host_data->now_host_state.sel_info.entries)
		{
		  (*record_id_start) = IPMI_SEL_RECORD_ID_FIRST;
		  log_entries_flag++;
		}
	    }
	}
      else
	{
	  if (!host_data->now_host_state.sel_info.entries)
	    host_data->now_host_state.last_record_id.record_id = 0;
	  else
	    host_data->now_host_state.last_record_id.record_id = host_data->last_host_state.last_record_id.record_id;
	}
    }

  if (log_entries_flag)
    rv = 1;
  else
    rv = 0;

 cleanup:
  return (rv);
}

/* returns 1 if clear should occur, 0 if not, -1 on error */
static int
ipmiseld_check_thresholds (ipmiseld_host_data_t *host_data)
{
  int do_clear_flag = 0;
  unsigned int percent;
  int rv = -1;

  assert (host_data);

  percent = ipmiseld_calc_percent_full (host_data, &(host_data->now_host_state.sel_info));

  if (host_data->prog_data->args->warning_threshold)
    {
      if (percent > host_data->prog_data->args->warning_threshold)
	{
	  if (percent > host_data->last_host_state.last_percent_full)
	    ipmiseld_syslog_host (host_data, "SEL is %d%% full", percent);
	}
    }

  if (!host_data->last_host_state.sel_info.overflow_flag
      && host_data->now_host_state.sel_info.overflow_flag)
    ipmiseld_syslog_host (host_data, "SEL Overflow, events have been dropped due to lack of space in the SEL");
  
  if (host_data->prog_data->args->clear_threshold)
    {
      if (percent > host_data->prog_data->args->clear_threshold)
	do_clear_flag = 1;
    }

  host_data->now_host_state.last_percent_full = percent;

  if (do_clear_flag)
    rv = 1;
  else
    rv = 0;

  return (rv);
}

/* returns 1 if reserve successful, 0 if not, -1 on error */
static int
ipmiseld_sel_reserve (ipmiseld_host_data_t *host_data)
{
  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->sel_ctx);

  if (host_data->now_host_state.sel_info.reserve_sel_command_supported)
    {
      if (ipmi_sel_ctx_register_reservation_id (host_data->host_poll->sel_ctx, NULL) < 0)
	{
	  /* If an IPMI error, we assume just can't do reservation, no biggie */
	  if (ipmi_sel_ctx_errnum (host_data->host_poll->sel_ctx) == IPMI_SEL_ERR_IPMI_ERROR)
	    return (0);
	  
	  ipmiseld_err_output (host_data, "ipmi_sel_ctx_register_reservation_id: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
	  return (-1);
	}

      return (1);
    }

  return (0);
}

static int
ipmiseld_sel_log_entries (ipmiseld_host_data_t *host_data,
			  uint16_t record_id_start)
{
  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->sel_ctx);
  
  if (ipmi_sel_parse (host_data->host_poll->sel_ctx,
		      record_id_start,
		      IPMI_SEL_RECORD_ID_LAST,
		      _sel_parse_callback,
		      host_data) < 0)
    {
      ipmiseld_err_output (host_data, "ipmi_sel_parse: %s", ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
      return (-1);
    }
  
  return (0);
}

static int
ipmiseld_save_state (ipmiseld_host_data_t *host_data)
{
  assert (host_data);

  memcpy (&(host_data->last_host_state),
	  &(host_data->now_host_state),
	  sizeof (ipmiseld_host_state_t));

  /* ignore error, continue on even if it fails */
  ipmiseld_data_cache_store (host_data);

  return (0);
}
  
/* return 1 - retry immediately, return 0 general success, -1 error */
static int
ipmiseld_sel_parse_log (ipmiseld_host_data_t *host_data)
{
  uint16_t record_id_start = 0;
  int log_entries_flag = 0;
  int do_clear_flag = 0;
  int reserve_flag = 0;
  int retry_flag = 0;
  int rv = -1;
  int ret;

  assert (host_data);

  if (host_data->prog_data->args->clear_sel
      && !host_data->clear_sel_done)
    {
      if (ipmi_sel_clear_sel (host_data->host_poll->sel_ctx) < 0)
	{
	  ipmiseld_err_output (host_data, "ipmi_sel_clear_sel: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
	  goto cleanup;
	}
      host_data->clear_sel_done = 1;
    }

  if (!host_data->last_host_state.initialized)
    {
      if ((ret = ipmiseld_data_cache_load (host_data)) < 0)
	{
	  if (host_data->prog_data->args->verbose_count)
	    ipmiseld_syslog_host (host_data, "Failed to load cached previous state, some SEL entries maybe missed");
	}

      if (ret <= 0)
	{
	  if (ipmiseld_host_state_init (host_data) < 0)
	    goto cleanup;

	  if (host_data->prog_data->args->foreground
	      && host_data->prog_data->args->common_args.debug)
	    _dump_host_state (host_data,
			      &(host_data->last_host_state),
			      "Initial State");
	  
	  goto out;
	}
      else
	{
	  if (host_data->prog_data->args->foreground
	      && host_data->prog_data->args->common_args.debug)
	    _dump_host_state (host_data,
			      &(host_data->last_host_state),
			      "Loaded State");
	}
    }
  
  if (ipmiseld_sel_info_get (host_data, &(host_data->now_host_state.sel_info)) < 0)
    goto cleanup;

  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug)
    {
      _dump_host_state (host_data, &(host_data->last_host_state), "Last State");
      _dump_sel_info (host_data, &host_data->now_host_state.sel_info, "Current State");
    }
  
  if ((do_clear_flag = ipmiseld_check_thresholds (host_data)) < 0)
    goto cleanup;

  if ((log_entries_flag = ipmiseld_check_sel_info (host_data, &record_id_start)) < 0)
    goto cleanup;

  if (do_clear_flag)
    {
      if ((reserve_flag = ipmiseld_sel_reserve (host_data)) < 0)
	goto cleanup;
    }

  if (log_entries_flag)
    {
      if (ipmiseld_sel_log_entries (host_data, record_id_start) < 0)
	goto cleanup;
    }
  
  if (do_clear_flag)
    {
      ipmiseld_sel_info_t tmp_sel_info;
      
      if ((ret = ipmi_sel_clear_sel (host_data->host_poll->sel_ctx)) < 0)
	{
	  if (reserve_flag
	      && ipmi_sel_ctx_errnum (host_data->host_poll->sel_ctx) == IPMI_SEL_ERR_RESERVATION_CANCELED)
	    retry_flag++;
	  else
	    {
	      ipmiseld_err_output (host_data, "ipmi_sel_clear_sel: %s",
			  ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
	      goto save_state_out;
	    }
	}
      
      ipmiseld_syslog_host (host_data, "SEL cleared");
	      
      if (ipmiseld_sel_info_get (host_data, &tmp_sel_info) < 0)
	goto save_state_out;
      
      memcpy (&(host_data->now_host_state.sel_info), &tmp_sel_info, sizeof (ipmiseld_sel_info_t));
      host_data->now_host_state.last_record_id.record_id = 0;
    }

 save_state_out:

  host_data->now_host_state.initialized = 1;

  if (ipmiseld_save_state (host_data) < 0)
    goto cleanup;
  
 out:

  if (retry_flag)
    rv = 1;
  else
    rv = 0;

 cleanup:
  return (rv);
}

static int
ipmiseld_sel_parse (ipmiseld_host_data_t *host_data)
{
  unsigned int retry_count = 0;
  int rv;
 
  assert (host_data);

  if (host_data->prog_data->args->test_run)
    return (ipmiseld_sel_parse_test_run (host_data));

  while (retry_count < IPMISELD_RETRY_ATTEMPT_MAX)
    {
      if ((rv = ipmiseld_sel_parse_log (host_data)) < 0)
	break;

      if (!rv)
	break;

      retry_count++;
    }

  return (rv);
}

static int
_ipmiseld_poll (void *arg)
{
  ipmiseld_host_data_t *host_data;
  ipmiseld_host_poll_t host_poll;
  unsigned int sel_flags = 0;
  unsigned int interpret_flags = 0;
  int exit_code = EXIT_FAILURE;

  assert (arg);

  host_data = (ipmiseld_host_data_t *)arg;

  assert (!host_data->host_poll);

  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug)
    IPMISELD_DEBUG (("Poll %s", host_data->hostname ? host_data->hostname : "localhost"));

  memset (&host_poll, '\0', sizeof (ipmiseld_host_poll_t));
  host_data->host_poll = &host_poll;

  if (ipmiseld_ipmi_setup (host_data) < 0)
    goto cleanup;

  if (!host_data->prog_data->args->ignore_sdr)
    {
      if (ipmiseld_sdr_cache_create_and_load (host_data) < 0)
	goto cleanup;
    }
  else
    host_data->host_poll->sdr_ctx = NULL;
  
  if (!(host_data->host_poll->sel_ctx = ipmi_sel_ctx_create (host_data->host_poll->ipmi_ctx, host_data->host_poll->sdr_ctx)))
    {
      ipmiseld_err_output (host_data, "ipmi_sel_ctx_create: %s", strerror (errno));
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
	ipmiseld_err_output (host_data, "ipmi_sel_ctx_set_flags: %s",
		    ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
    }

  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug > 1
      && host_data->hostname)
    {
      if (ipmi_sel_ctx_set_debug_prefix (host_data->host_poll->sel_ctx, host_data->hostname) < 0)
        ipmiseld_err_output (host_data, "ipmi_sel_ctx_set_debug_prefix: %s",
		    ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
    }
  
  if (!(host_data->host_poll->interpret_ctx = ipmi_interpret_ctx_create ()))
    {
      ipmiseld_err_output (host_data, "ipmi_interpret_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (ipmi_interpret_load_sel_config (host_data->host_poll->interpret_ctx,
				      host_data->prog_data->args->event_state_config_file) < 0)
    {
      /* if default file is missing its ok */
      if (!(!host_data->prog_data->args->event_state_config_file
	    && ipmi_interpret_ctx_errnum (host_data->host_poll->interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST))
	{
	  ipmiseld_err_output (host_data, "ipmi_interpret_load_sel_config: %s", ipmi_interpret_ctx_errormsg (host_data->host_poll->interpret_ctx));
	  goto cleanup;
        }
    }

  if (host_data->prog_data->args->interpret_oem_data)
    interpret_flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;
  
  if (interpret_flags)
    {
      if (ipmi_interpret_ctx_set_flags (host_data->host_poll->interpret_ctx, interpret_flags) < 0)
	{
	  ipmiseld_err_output (host_data, "ipmi_interpret_ctx_set_flags: %s",
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
      ipmiseld_err_output (host_data, "ipmi_sel_ctx_set_separator: %s",
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
          ipmiseld_err_output (host_data, "ipmi_sel_ctx_set_manufacturer_id: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
          return (-1);
        }
      
      if (ipmi_sel_ctx_set_product_id (host_data->host_poll->sel_ctx,
                                       host_data->host_poll->oem_data.product_id) < 0)
        {
          ipmiseld_err_output (host_data, "ipmi_sel_ctx_set_product_id: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
          return (-1);
        }

      if (ipmi_sel_ctx_set_ipmi_version (host_data->host_poll->sel_ctx,
                                         host_data->host_poll->oem_data.ipmi_version_major,
                                         host_data->host_poll->oem_data.ipmi_version_minor) < 0)
        {
          ipmiseld_err_output (host_data, "ipmi_sel_ctx_set_ipmi_version: %s",
		      ipmi_sel_ctx_errormsg (host_data->host_poll->sel_ctx));
          return (-1);
        }
      
      if (host_data->prog_data->args->interpret_oem_data)
        {
          if (ipmi_interpret_ctx_set_manufacturer_id (host_data->host_poll->interpret_ctx,
                                                      host_data->host_poll->oem_data.manufacturer_id) < 0)
            {
              ipmiseld_err_output (host_data, "ipmi_interpret_ctx_set_manufacturer_id: %s",
			  ipmi_interpret_ctx_errormsg (host_data->host_poll->interpret_ctx));
              return (-1);
            }
	  
          if (ipmi_interpret_ctx_set_product_id (host_data->host_poll->interpret_ctx,
                                                 host_data->host_poll->oem_data.product_id) < 0)
            {
              ipmiseld_err_output (host_data, "ipmi_interpret_ctx_set_product_id: %s",
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

static int
_ipmiseld_poll_postprocess (void *arg)
{
  ipmiseld_host_data_t *host_data;
  struct timeval tv;
  int rv = -1;

  assert (arg);

  host_data = (ipmiseld_host_data_t *)arg;

  assert (!host_data->host_poll);

  gettimeofday (&tv, NULL);
  host_data->next_poll_time = tv.tv_sec + host_data->prog_data->args->poll_interval;

  pthread_mutex_lock (&host_data_heap_lock);
  
  if (!heap_insert (host_data_heap, host_data))
    {
      pthread_mutex_unlock (&host_data_heap_lock);
      ipmiseld_err_output (host_data, "heap_insert: %s", strerror (errno));
      goto cleanup;
    }

  pthread_mutex_unlock (&host_data_heap_lock);
  rv = 0;
 cleanup:
  return (rv);
}

static void
_signal_handler_callback (int sig)
{
  exit_flag = 0;
}

static void
_free_host_data (void *x)
{
  ipmiseld_host_data_t *host_data;;
  
  assert (x);

  host_data = (ipmiseld_host_data_t *)x;
  free (host_data->hostname);
  free (host_data);
}

static ipmiseld_host_data_t *
_alloc_host_data (ipmiseld_prog_data_t *prog_data, const char *hostname)
{
  ipmiseld_host_data_t *host_data;

  assert (prog_data);

  if (!(host_data = (ipmiseld_host_data_t *) malloc (sizeof (ipmiseld_host_data_t))))
    {
      err_output ("malloc: %s", strerror (errno));
      return (NULL);
    }

  memset (host_data, '\0', sizeof (ipmiseld_host_data_t));
  host_data->prog_data = prog_data;
  if (hostname)
    {
      if (!(host_data->hostname = strdup (hostname)))
	{
	  err_output ("strdup: %s", strerror (errno));
	  free (host_data);
	  return (NULL);
	}
    }
  else
    host_data->hostname = NULL;
  host_data->host_poll = NULL;
  host_data->re_download_sdr_done = 0;
  host_data->clear_sel_done = 0;
  host_data->next_poll_time = 0; /* 0 will first immediate check first time through */
  host_data->last_ipmi_errnum = 0;
  host_data->last_ipmi_errnum_count = 0;

  return (host_data);
}

static int
hostdata_timecmp (void *x, void *y)
{
  ipmiseld_host_data_t *hd1, *hd2;

  assert (x);
  assert (y);

  hd1 = (ipmiseld_host_data_t *)x;
  hd2 = (ipmiseld_host_data_t *)y;

  if (hd1->next_poll_time < hd2->next_poll_time)
    return (1);
  else if (hd1->next_poll_time > hd2->next_poll_time)
    return (-1);
  return (0);
}

static int
_ipmiseld (ipmiseld_prog_data_t *prog_data)
{
  int hosts_count = 0;
  hostlist_t hlist = NULL;
  hostlist_iterator_t hitr = NULL;
  ipmiseld_host_data_t *host_data;
  char *host = NULL;
  int rv = -1;
  int ret;

  assert (prog_data);
  assert (!host_data_heap);

  if (prog_data->args->common_args.hostname)
    {
      if ((hosts_count = pstdout_hostnames_count (prog_data->args->common_args.hostname)) < 0)
        {
	  err_output ("pstdout_hostnames_count: %s", pstdout_strerror (pstdout_errnum));
	  goto cleanup;
        }
      
      if (!hosts_count)
        {
	  err_output ("invalid number of hosts specified");
	  goto cleanup;
        }
    }
  else /* inband communication, hosts_count = 1 */
    hosts_count = 1;

  /* don't need more threads than hosts */
  if (hosts_count < prog_data->args->threadpool_count)
    prog_data->args->threadpool_count = hosts_count;

  if (!(host_data_heap = heap_create (hosts_count,
				      (HeapCmpF)hostdata_timecmp,
				      (HeapDelF)_free_host_data)))
    {
      err_output ("heap_create: %s", strerror (errno));
      goto cleanup;
    }

  if ((ret = pthread_mutex_init (&host_data_heap_lock, NULL)))
    {
      err_output ("pthread_mutex_init: %s", strerror (ret));
      goto cleanup;
    }

  if (hosts_count == 1)
    {
      if (!(host_data = _alloc_host_data (prog_data, prog_data->args->common_args.hostname)))
	goto cleanup;

      if (!heap_insert (host_data_heap, host_data))
	{
	  err_output ("heap_insert: %s", strerror (errno));
	  goto cleanup;
	}
    }
  else
    {
      if (!(hlist = hostlist_create (prog_data->args->common_args.hostname)))
	{
	  err_output ("hostlist_create: %s", strerror (errno));
	  goto cleanup;
	}

      if (!(hitr = hostlist_iterator_create (hlist)))
	{
	  err_output ("hostlist_iterator_create: %s", strerror (errno));
	  goto cleanup;
	}

      while ((host = hostlist_next (hitr)))
	{
	  if (!(host_data = _alloc_host_data (prog_data, host)))
	    goto cleanup;

	  if (!heap_insert (host_data_heap, host_data))
	    {
	      err_output ("heap_insert: %s", strerror (errno));
	      goto cleanup;
	    }

	  free(host);
	}
      host = NULL;
    }

  if (ipmiseld_threadpool_init (prog_data,
				_ipmiseld_poll,
				_ipmiseld_poll_postprocess) < 0)
    goto cleanup;

  if (prog_data->args->test_run)
    {
      while (!heap_is_empty (host_data_heap))
	{
	  if (!(host_data = heap_pop (host_data_heap)))
	    {
	      err_output ("heap_pop: %s", strerror (errno));
	      goto cleanup;
	    }

	  _ipmiseld_poll (host_data);
	}
    }
  else
    {
      while (exit_flag)
	{
	  pthread_mutex_lock (&host_data_heap_lock);

	  host_data = heap_pop (host_data_heap);
	  
	  pthread_mutex_unlock (&host_data_heap_lock);

	  /* empty heap, small chance of this happening, but
	   * everything is processing and previous sleeps didn't sleep
	   * long enough.  So we just need to wait until something
	   * else finishes.
	   *
	   * There's no way to know the exact right amount of time, so
	   * we're going to make an estimate.  What we'll do is
	   * estimate 1/5th the time of a IPMI session timeout.  So in
	   * the event the previous poll fully timed out, we will
	   * interrupt and go through this loop only 5 times.
	   */
	  if (!host_data)
	    {
	      unsigned int waittime;

	      if (prog_data->args->common_args.session_timeout)
		waittime = prog_data->args->common_args.session_timeout;
	      else
		waittime = IPMI_SESSION_TIMEOUT_DEFAULT;

	      /* session timeout is in milliseconds */
	      waittime /= 1000;
	      
	      /* now take a 5th of it  */
	      waittime /= 5;

	      if (!waittime)
		waittime = 1;

	      daemon_sleep (waittime);
	      continue;
	    }
	      
	  if (ipmiseld_threadpool_queue (host_data) < 0)
	    {
	      pthread_mutex_lock (&host_data_heap_lock);

	      if (!heap_insert (host_data_heap, host_data))
		ipmiseld_err_output (host_data, "heap_insert: %s", strerror (errno));

	      pthread_mutex_unlock (&host_data_heap_lock);
	    }

	  pthread_mutex_lock (&host_data_heap_lock);

	  host_data = heap_peek (host_data_heap);
	  
	  pthread_mutex_unlock (&host_data_heap_lock);

	  /* empty heap, everything must be processing, so we'll sleep
	   * for the poll interval, b/c no one should be scheduled
	   * until after this time has passed anyways.
	   */
	  if (!host_data)
	    daemon_sleep (prog_data->args->poll_interval + 1);
	  else
	    {
	      struct timeval tv;

	      gettimeofday (&tv, NULL);
	      
	      /* If next_poll_time == 0, no sleep, its the first time through */
	      if (host_data->next_poll_time
		  && (host_data->next_poll_time > tv.tv_sec)) 
		daemon_sleep ((host_data->next_poll_time - tv.tv_sec) + 1);
	    }
 	}
    }
  
  rv = 0;
 cleanup:
  ipmiseld_threadpool_destroy ();
  heap_destroy (host_data_heap);
  hostlist_iterator_destroy (hitr);
  hostlist_destroy (hlist);
  free (host);
  return (rv);
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
