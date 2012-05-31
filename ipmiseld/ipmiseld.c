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
#include "ipmiseld-common.h"
#include "ipmiseld-sdr.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "tool-common.h"
#include "tool-daemon-common.h"
#include "tool-event-common.h"
#include "tool-util-common.h"

#define IPMISELD_PIDFILE                                      IPMISELD_LOCALSTATEDIR "/run/ipmiseld.pid"

#define IPMISELD_EVENT_OUTPUT_BUFLEN                          4096

#define IPMISELD_SYSTEM_EVENT_FORMAT_STR_DEFAULT              "SEL System Event: %s, %I, %E"

#define IPMISELD_OEM_TIMESTAMPED_EVENT_FORMAT_STR_DEFAULT     "SEL OEM Event: %I, %o"

#define IPMISELD_OEM_NON_TIMESTAMPED_EVENT_FORMAT_STR_DEFAULT "SEL OEM Event: %I, %o"

/* return (-1), real error */
static int
_sel_parse_err_handle (ipmiseld_state_data_t *state_data, char *func)
{
  assert (state_data);
  assert (func);

  if (ipmi_sel_ctx_errnum (state_data->sel_ctx) == IPMI_SEL_ERR_INVALID_SEL_ENTRY)
    {
      /* maybe a bad SEL entry returned from remote system, don't error out */
      if (state_data->prog_data->args->common_args.debug)
        err_debug ("Invalid SEL entry read");
      return (0);
    }

  err_output ("%s: %s",
	      func,
	      ipmi_sel_ctx_errormsg (state_data->sel_ctx));

  return (-1);
}

static int
_normal_output (ipmiseld_state_data_t *state_data, uint8_t record_type)
{
  char outbuf[IPMISELD_EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;
  unsigned int flags;
  int record_type_class;
  char *format_str;

  assert (state_data);

  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
   
  flags = IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD;
  flags |= IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE;
  flags |= IPMI_SEL_STRING_FLAGS_DATE_MONTH_STRING;
  if (state_data->prog_data->args->verbose_count)
    flags |= IPMI_SEL_STRING_FLAGS_VERBOSE;
  if (state_data->prog_data->args->entity_sensor_names)
    flags |= IPMI_SEL_STRING_FLAGS_ENTITY_SENSOR_NAMES;
  if (state_data->prog_data->args->non_abbreviated_units)
    flags |= IPMI_SEL_STRING_FLAGS_NON_ABBREVIATED_UNITS;
  if (state_data->prog_data->args->interpret_oem_data)
    flags |= IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA;

  record_type_class = ipmi_sel_record_type_class (record_type);
  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      if (state_data->prog_data->args->system_event_format_str)
	format_str = state_data->prog_data->args->system_event_format_str;
      else
	format_str = IPMISELD_SYSTEM_EVENT_FORMAT_STR_DEFAULT;
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      if (state_data->prog_data->args->oem_timestamped_event_format_str)
	format_str = state_data->prog_data->args->oem_timestamped_event_format_str;
      else
	format_str = IPMISELD_OEM_TIMESTAMPED_EVENT_FORMAT_STR_DEFAULT;
    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    {
      if (state_data->prog_data->args->oem_non_timestamped_event_format_str)
	format_str = state_data->prog_data->args->oem_non_timestamped_event_format_str;
      else
	format_str = IPMISELD_OEM_NON_TIMESTAMPED_EVENT_FORMAT_STR_DEFAULT;
    }
  else
    {
      if (state_data->prog_data->args->verbose_count)
	ipmiseld_syslog (state_data,
			 "SEL Event: Unknown SEL Record Type: %Xh",
			 record_type);
      return (0);
    }
  
  if ((outbuf_len = ipmi_sel_parse_read_record_string (state_data->sel_ctx,
						       format_str,
						       NULL,
						       0,
						       outbuf,
						       IPMISELD_EVENT_OUTPUT_BUFLEN,
						       flags)) < 0)
    {
      if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_read_record_string") < 0)
	return (-1);
      return (0);
    }
  
  if (outbuf_len)
    ipmiseld_syslog (state_data, "%s", outbuf);
  
  return (0);
}

static int
_sel_parse_callback (ipmi_sel_ctx_t ctx, void *callback_data)
{
  ipmiseld_state_data_t *state_data;
  uint8_t record_type;
  int record_type_class;
  int rv = -1;

  assert (ctx);
  assert (callback_data);

  state_data = (ipmiseld_state_data_t *)callback_data;

  if (state_data->prog_data->args->sensor_types_length
      || state_data->prog_data->args->exclude_sensor_types_length)
    {
      uint8_t sensor_type;
      int flag;

      if (ipmi_sel_parse_read_sensor_type (state_data->sel_ctx,
                                           NULL,
                                           0,
                                           &sensor_type) < 0)
        {
          if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_read_record_type") < 0)
            goto cleanup;
          goto out;
        }

      if (state_data->prog_data->args->sensor_types_length)
        {
          if ((flag = sensor_type_listed (NULL,
                                          sensor_type,
                                          state_data->prog_data->args->sensor_types,
                                          state_data->prog_data->args->sensor_types_length)) < 0)
            goto cleanup;
          
          if (!flag)
            goto out;
        }

      if (state_data->prog_data->args->exclude_sensor_types_length)
        {
          if ((flag = sensor_type_listed (NULL,
                                          sensor_type,
                                          state_data->prog_data->args->exclude_sensor_types,
                                          state_data->prog_data->args->exclude_sensor_types_length)) < 0)
            goto cleanup;

          if (flag)
            goto out;
        }
    }

  if (ipmi_sel_parse_read_record_type (state_data->sel_ctx,
                                       NULL,
                                       0,
                                       &record_type) < 0)
    {
      if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_read_record_type") < 0)
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
  if (state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT
      && (!IPMI_SEL_RECORD_TYPE_VALID (record_type)))
    record_type = IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (state_data->prog_data->args->system_event_only
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    goto out;

  if (state_data->prog_data->args->oem_event_only
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    goto out;

  if (state_data->prog_data->event_state_filter_mask)
    {
      char sel_record[IPMI_SEL_RECORD_MAX_RECORD_LENGTH];
      int sel_record_len;
      unsigned int event_state = 0;

      if ((sel_record_len = ipmi_sel_parse_read_record (state_data->sel_ctx,
							sel_record,
							IPMI_SEL_RECORD_MAX_RECORD_LENGTH)) < 0)
	{
	  if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_read_record_type") < 0)
	    goto cleanup;
	  goto out;
	}
      
      if (ipmi_interpret_sel (state_data->interpret_ctx,
			      sel_record,
			      sel_record_len,
			      &event_state) < 0)
	{
	  err_output ("ipmi_interpret_sel: %s",
		      ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
	  goto cleanup;
	}

      if ((state_data->prog_data->event_state_filter_mask & IPMISELD_NOMINAL_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_NOMINAL)
	goto out;

      if ((state_data->prog_data->event_state_filter_mask & IPMISELD_WARNING_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_WARNING)
	goto out;

      if ((state_data->prog_data->event_state_filter_mask & IPMISELD_CRITICAL_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_CRITICAL)
	goto out;

      if ((state_data->prog_data->event_state_filter_mask & IPMISELD_NA_FILTER)
	  && event_state == IPMI_INTERPRET_STATE_UNKNOWN)
	goto out;
    }

  if (_normal_output (state_data, record_type) < 0)
    goto cleanup;

 out:
  rv = 0;
 cleanup:
  return (rv);
}

static int
run_cmd_args (ipmiseld_state_data_t *state_data)
{
  struct ipmiseld_arguments *args;

  assert (state_data);

  args = state_data->prog_data->args;

  if (ipmi_sel_ctx_set_separator (state_data->sel_ctx, EVENT_OUTPUT_SEPARATOR) < 0)
    {
      err_output ("ipmi_sel_parse: %s",
		  ipmi_sel_ctx_errormsg (state_data->sel_ctx));
      return (-1);
    }

  if (args->interpret_oem_data || args->output_oem_event_strings)
    {
      if (ipmi_get_oem_data (NULL,
                             state_data->ipmi_ctx,
                             &state_data->oem_data) < 0)
        return (-1);

      if (ipmi_sel_ctx_set_manufacturer_id (state_data->sel_ctx,
                                            state_data->oem_data.manufacturer_id) < 0)
        {
          err_output ("ipmi_sel_ctx_set_manufacturer_id: %s",
		      ipmi_sel_ctx_errormsg (state_data->sel_ctx));
          return (-1);
        }
      
      if (ipmi_sel_ctx_set_product_id (state_data->sel_ctx,
                                       state_data->oem_data.product_id) < 0)
        {
          err_output ("ipmi_sel_ctx_set_product_id: %s",
		      ipmi_sel_ctx_errormsg (state_data->sel_ctx));
          return (-1);
        }

      if (ipmi_sel_ctx_set_ipmi_version (state_data->sel_ctx,
                                         state_data->oem_data.ipmi_version_major,
                                         state_data->oem_data.ipmi_version_minor) < 0)
        {
          err_output ("ipmi_sel_ctx_set_ipmi_version: %s",
		      ipmi_sel_ctx_errormsg (state_data->sel_ctx));
          return (-1);
        }
      
      if (args->interpret_oem_data)
        {
          if (ipmi_interpret_ctx_set_manufacturer_id (state_data->interpret_ctx,
                                                      state_data->oem_data.manufacturer_id) < 0)
            {
              err_output ("ipmi_interpret_ctx_set_manufacturer_id: %s",
			  ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
              return (-1);
            }
	  
          if (ipmi_interpret_ctx_set_product_id (state_data->interpret_ctx,
                                                 state_data->oem_data.product_id) < 0)
            {
              err_output ("ipmi_interpret_ctx_set_product_id: %s",
			  ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
              return (-1);
            }
        }
    }

  if (ipmi_sel_parse (state_data->sel_ctx,
		      IPMI_SEL_RECORD_ID_FIRST,
		      IPMI_SEL_RECORD_ID_LAST,
		      _sel_parse_callback,
		      state_data) < 0)
    {
      err_output ("ipmi_sel_parse: %s",
		  ipmi_sel_ctx_errormsg (state_data->sel_ctx));
      return (-1);
    }

  return (0);
}

static int
_ipmi_setup (ipmiseld_state_data_t *state_data)
{
  struct common_cmd_args *common_args;
  unsigned int workaround_flags = 0;
  int rv = -1;

  assert (state_data);

  common_args = &(state_data->prog_data->args->common_args);

  if (!(state_data->ipmi_ctx = ipmi_ctx_create ()))
    {
      err_output ("ipmi_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (common_args->hostname
      && strcasecmp (common_args->hostname, "localhost") != 0
      && strcmp (common_args->hostname, "127.0.0.1") != 0)
    {
      if (common_args->driver_type == IPMI_DEVICE_LAN_2_0)
        {
          parse_get_freeipmi_outofband_2_0_flags (common_args->workaround_flags_outofband_2_0,
                                                  &workaround_flags);
          
          if (ipmi_ctx_open_outofband_2_0 (state_data->ipmi_ctx,
                                           common_args->hostname,
                                           common_args->username,
                                           common_args->password,
                                           (common_args->k_g_len) ? common_args->k_g : NULL,
                                           (common_args->k_g_len) ? common_args->k_g_len : 0,
                                           common_args->privilege_level,
                                           common_args->cipher_suite_id,
                                           common_args->session_timeout,
                                           common_args->retransmission_timeout,
                                           workaround_flags,
                                           (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_open_outofband_2_0: %s", ipmi_ctx_errormsg (state_data->ipmi_ctx));
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_ctx_open_outofband (state_data->ipmi_ctx,
                                       common_args->hostname,
                                       common_args->username,
                                       common_args->password,
                                       common_args->authentication_type,
                                       common_args->privilege_level,
                                       common_args->session_timeout,
                                       common_args->retransmission_timeout,
                                       workaround_flags,
                                       (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_open_outofband: %s", ipmi_ctx_errormsg (state_data->ipmi_ctx));
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

          if ((ret = ipmi_ctx_find_inband (state_data->ipmi_ctx,
                                           NULL,
                                           common_args->disable_auto_probe,
                                           common_args->driver_address,
                                           common_args->register_spacing,
                                           common_args->driver_device,
                                           workaround_flags,
                                           (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT)) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_find_inband: %s", ipmi_ctx_errormsg (state_data->ipmi_ctx));
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
	  if (ipmi_ctx_open_inband (state_data->ipmi_ctx,
                                    common_args->driver_type,
                                    common_args->disable_auto_probe,
                                    common_args->driver_address,
                                    common_args->register_spacing,
                                    common_args->driver_device,
                                    workaround_flags,
                                    (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      /* XXX deal w/ specific errors */
	      err_output ("ipmi_ctx_open_inband: %s", ipmi_ctx_errormsg (state_data->ipmi_ctx));
              goto cleanup;
            }
        }
    }

  if (common_args->target_channel_number_is_set
      || common_args->target_slave_address_is_set)
    {
      if (ipmi_ctx_set_target (state_data->ipmi_ctx,
                               common_args->target_channel_number_is_set ? &common_args->target_channel_number : NULL,
                               common_args->target_slave_address_is_set ? &common_args->target_slave_address : NULL) < 0)
        {
	  err_output ("ipmi_ctx_set_target: %s", ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        } 
    }
  
  rv = 0;
 cleanup:
  if (rv < 0)
    {
      ipmi_ctx_close (state_data->ipmi_ctx);
      ipmi_ctx_destroy (state_data->ipmi_ctx);
    }
  return (rv);
}

static int
_ipmiseld_instance (ipmiseld_prog_data_t *prog_data)
{
  ipmiseld_state_data_t state_data;
  unsigned int sel_flags = 0;
  unsigned int interpret_flags = 0;
  int exit_code = EXIT_FAILURE;

  assert (prog_data);

  memset (&state_data, '\0', sizeof (ipmiseld_state_data_t));
  state_data.prog_data = prog_data;
  /* XXX deal w/ hostrange later */
  state_data.hostname = prog_data->args->common_args.hostname;

  if (_ipmi_setup (&state_data) < 0)
    goto cleanup;

  if (ipmiseld_sdr_cache_create_and_load (&state_data, state_data.hostname) < 0)
    goto cleanup;
  
  if (!(state_data.sel_ctx = ipmi_sel_ctx_create (state_data.ipmi_ctx, state_data.sdr_ctx)))
    {
      err_output ("ipmi_sel_ctx_create: %s", strerror (errno));
      goto cleanup;
    }
  
  if (state_data.prog_data->args->foreground
      && state_data.prog_data->args->common_args.debug)
    sel_flags |= IPMI_SEL_FLAGS_DEBUG_DUMP;
  
  if (state_data.prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT)
    sel_flags |= IPMI_SEL_FLAGS_ASSUME_SYTEM_EVENT_RECORDS;
  
  if (sel_flags)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sel_ctx_set_flags (state_data.sel_ctx, sel_flags) < 0)
	err_output ("ipmi_sel_ctx_set_flags: %s",
		    ipmi_sel_ctx_errormsg (state_data.sel_ctx));
    }

  if (state_data.prog_data->args->foreground
      && state_data.prog_data->args->common_args.debug
      && prog_data->args->common_args.hostname)
    {
      if (ipmi_sel_ctx_set_debug_prefix (state_data.sel_ctx,
                                         prog_data->args->common_args.hostname) < 0)
        err_output ("ipmi_sel_ctx_set_debug_prefix: %s",
		    ipmi_sel_ctx_errormsg (state_data.sel_ctx));
    }
  
  if (!(state_data.interpret_ctx = ipmi_interpret_ctx_create ()))
    {
      err_output ("ipmi_interpret_ctx_create: %s", strerror (errno));
      goto cleanup;
    }


  if (ipmi_interpret_load_sel_config (state_data.interpret_ctx,
				      prog_data->args->event_state_config_file) < 0)
    {
      /* if default file is missing its ok */
      if (!(!prog_data->args->event_state_config_file
	    && ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST))
	{
	  err_output ("ipmi_interpret_load_sel_config: %s", ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
	  goto cleanup;
        }
    }

  if (prog_data->args->interpret_oem_data)
    interpret_flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;
  
  if (interpret_flags)
    {
      if (ipmi_interpret_ctx_set_flags (state_data.interpret_ctx, interpret_flags) < 0)
	{
	  err_output ("ipmi_interpret_ctx_set_flags: %s",
		      ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
	  goto cleanup;
	}
    }

  if (ipmi_sel_ctx_set_parameter (state_data.sel_ctx,
				  IPMI_SEL_PARAMETER_INTERPRET_CONTEXT,
				  &(state_data.interpret_ctx)) < 0)
    {
      err_output("ipmi_sel_ctx_set_interpret: %s",
		 ipmi_sel_ctx_errormsg (state_data.sel_ctx));
      goto cleanup;
    }
  
  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_interpret_ctx_destroy (state_data.interpret_ctx);
  ipmi_sel_ctx_destroy (state_data.sel_ctx);
  ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

static int
_ipmiseld (ipmiseld_prog_data_t *prog_data)
{
  if (prog_data->args->test_run)
    return (_ipmiseld_instance (prog_data));
  else
    {
      while (1)
	{
	  unsigned int timeout;

	  if (_ipmiseld_instance (prog_data) < 0)
	    timeout = prog_data->args->poll_error_interval;
	  else
	    timeout = prog_data->args->poll_interval;

	  daemon_sleep (timeout);
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
      
      daemon_signal_handler_setup (NULL);

      /* Call after daemonization, since daemonization closes currently
       * open fds
       */
      openlog (argv[0], LOG_ODELAY | LOG_PID, prog_data.log_facility);
    }
  
  return (_ipmiseld (&prog_data));
}
