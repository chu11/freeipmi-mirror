/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "tool-event-common.h"
#include "tool-sdr-cache-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

int
event_load_event_state_config_file (pstdout_state_t pstate,
				    ipmi_interpret_ctx_t interpret_ctx,
				    const char *event_state_config_file)
{
  if (event_state_config_file)
    {
      if (ipmi_interpret_load_sel_config (interpret_ctx,
					  event_state_config_file) < 0)
	{
	  if (ipmi_interpret_ctx_errnum (interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST)
	    PSTDOUT_FPRINTF (pstate,
			     stderr,
			     "event state config file '%s' does not exist\n",
			     event_state_config_file);
	  else if (ipmi_interpret_ctx_errnum (interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
	    PSTDOUT_FPRINTF (pstate,
			     stderr,
			     "event state config file '%s' parse error\n",
			     event_state_config_file);
	  else
	    PSTDOUT_FPRINTF (pstate,
			     stderr,
			     "ipmi_interpret_load_sel_config: %s\n",
			     ipmi_interpret_ctx_errormsg (interpret_ctx));
	  return (-1);
	}
    }
  else
    {
      if (ipmi_interpret_load_sel_config (interpret_ctx, NULL) < 0)
	{
	  if (ipmi_interpret_ctx_errnum (interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
	    PSTDOUT_FPRINTF (pstate,
			     stderr,
			     "event state config file parse error\n");
	  else
	    PSTDOUT_FPRINTF (pstate,
			     stderr,
			     "ipmi_interpret_load_sel_config: %s\n",
			     ipmi_interpret_ctx_errormsg (interpret_ctx));
	  return (-1);
	}
    }
  
  return (0);
}

/* return -1 on failout error, 0 on invalid data */
static int
_sel_parse_err_handle (pstdout_state_t pstate,
		       ipmi_sel_ctx_t sel_ctx,
		       int debug,
		       const char *func)
{
  assert (sel_ctx);
  assert (func);
  
  if (ipmi_sel_ctx_errnum (sel_ctx) == IPMI_SEL_ERR_INVALID_SEL_ENTRY)
    {
      /* most likely bad event data from remote system or user input */
      if (debug)
	PSTDOUT_FPRINTF (pstate,
			 stderr,
			 "Invalid data\n");

      return (0);
    }
  
  PSTDOUT_FPRINTF (pstate,
                   stderr,
                   "%s: %s\n",
                   func,
                   ipmi_sel_ctx_errormsg (sel_ctx));
  return (-1);
}

/* return -1 on failout error, 0 on invalid data, 1 otherwise */
static int
_sel_parse_record_string (pstdout_state_t pstate,
			  ipmi_sel_ctx_t sel_ctx,
			  uint8_t *sel_record,
			  unsigned int sel_record_len,
			  int debug,
			  unsigned int flags,
			  char outbuf[EVENT_OUTPUT_BUFLEN + 1],
			  int *outbuf_len,
			  const char *fmt)
{
  assert (sel_ctx);
  assert (outbuf);
  assert (outbuf_len);
  
  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
  if ((*outbuf_len = ipmi_sel_parse_read_record_string (sel_ctx,
							fmt,
							sel_record,
							sel_record_len,
							outbuf,
							EVENT_OUTPUT_BUFLEN,
							flags)) < 0)
    {
      if (_sel_parse_err_handle (pstate,
				 sel_ctx,
				 debug,
				 "ipmi_sel_parse_format_record_string") < 0)
	return (-1);
      return (0);
    }

  return (1);
}

int
event_output_time (pstdout_state_t pstate,
		   ipmi_sel_ctx_t sel_ctx,
		   uint8_t *sel_record,
		   unsigned int sel_record_len,
		   int comma_separated_output,
		   int debug,
		   unsigned int flags)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;
  int ret;

  assert (sel_ctx);

  if ((ret = _sel_parse_record_string (pstate,
				       sel_ctx,
				       sel_record,
				       sel_record_len,
				       debug,
				       flags,
				       outbuf,
				       &outbuf_len,
				       "%t")) < 0)
    return (-1);

  if (!ret)
    return (0);

  if (comma_separated_output)
    {
      if (outbuf_len)
        PSTDOUT_PRINTF (pstate, ",%s", outbuf);
      else
        PSTDOUT_PRINTF (pstate, ",%s", EVENT_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        PSTDOUT_PRINTF (pstate, " | %-8s", outbuf);
      else
        PSTDOUT_PRINTF (pstate, " | %-8s", EVENT_NA_STRING);
    }

  return (1);
}

int
event_output_not_available_time (pstdout_state_t pstate,
				 int comma_separated_output)
{
  if (comma_separated_output)
    PSTDOUT_PRINTF (pstate, ",%s", EVENT_NA_STRING);
  else
    PSTDOUT_PRINTF (pstate, " | %-8s", EVENT_NA_STRING);

  return (1);
}

int
event_output_sensor_name (pstdout_state_t pstate,
			  ipmi_sel_ctx_t sel_ctx,
			  uint8_t *sel_record,
			  unsigned int sel_record_len,
			  struct sensor_column_width *column_width,
			  struct common_cmd_args *common_args,
			  int comma_separated_output,
			  unsigned int flags)
{
  char fmt[EVENT_FMT_BUFLEN + 1];
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;
  int ret;

  assert (sel_ctx);
  assert (column_width || (!column_width && comma_separated_output));
  assert (common_args);

  if ((ret = _sel_parse_record_string (pstate,
				       sel_ctx,
				       sel_record,
				       sel_record_len,
				       common_args->debug,
				       flags,
				       outbuf,
				       &outbuf_len,
				       "%s")) < 0)
    return (-1);
  
  if (!ret)
    return (0);

  memset (fmt, '\0', EVENT_FMT_BUFLEN + 1);
  if (comma_separated_output)
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              ",%%s");
  else
    {
      if (outbuf_len > column_width->sensor_name)
	column_width->sensor_name = outbuf_len;

      snprintf (fmt,
		EVENT_FMT_BUFLEN,
		" | %%-%ds",
		column_width->sensor_name);
    }

  if (outbuf_len)
    PSTDOUT_PRINTF (pstate, fmt, outbuf);
  else
    PSTDOUT_PRINTF (pstate, fmt, EVENT_NA_STRING);

  return (1);
}

int
event_output_not_available_sensor_name (pstdout_state_t pstate,
					struct sensor_column_width *column_width,
					int comma_separated_output)
{
  char fmt[EVENT_FMT_BUFLEN + 1];

  assert (column_width || (!column_width && comma_separated_output));

  memset (fmt, '\0', EVENT_FMT_BUFLEN + 1);
  if (comma_separated_output)
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              ",%%s");
  else
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              " | %%-%ds",
              column_width->sensor_name);

  PSTDOUT_PRINTF (pstate, fmt, EVENT_NA_STRING);

  return (1);
}

int
event_output_sensor_type (pstdout_state_t pstate,
			  ipmi_sel_ctx_t sel_ctx,
			  uint8_t *sel_record,
			  unsigned int sel_record_len,
			  struct sensor_column_width *column_width,
			  int comma_separated_output,
			  int debug,
			  unsigned int flags)
{
  char fmt[EVENT_FMT_BUFLEN + 1];
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;
  int ret;

  assert (sel_ctx);
  assert (column_width || (!column_width && comma_separated_output));

  if ((ret = _sel_parse_record_string (pstate,
				       sel_ctx,
				       sel_record,
				       sel_record_len,
				       debug,
				       flags,
				       outbuf,
				       &outbuf_len,
				       "%T")) < 0)
    return (-1);
  
  if (!ret)
    return (0);

  memset (fmt, '\0', EVENT_FMT_BUFLEN + 1);
  if (comma_separated_output)
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              ",%%s");
  else
    {
      if (outbuf_len > column_width->sensor_type)
	column_width->sensor_type = outbuf_len;

      snprintf (fmt,
		EVENT_FMT_BUFLEN,
		" | %%-%ds",
		column_width->sensor_type);
    }

  if (outbuf_len)
    PSTDOUT_PRINTF (pstate, fmt, outbuf);
  else
    PSTDOUT_PRINTF (pstate, fmt, EVENT_NA_STRING);
  
  return (1);
}

int
event_output_not_available_sensor_type (pstdout_state_t pstate,
					struct sensor_column_width *column_width,
					int comma_separated_output)
{
  char fmt[EVENT_FMT_BUFLEN + 1];

  assert (column_width || (!column_width && comma_separated_output));
  
  memset (fmt, '\0', EVENT_FMT_BUFLEN + 1);
  if (comma_separated_output)
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              ",%%s");
  else
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              " | %%-%ds",
              column_width->sensor_type);
  
  PSTDOUT_PRINTF (pstate, fmt, EVENT_NA_STRING);

  return (1);
}

int
event_output_event_state (pstdout_state_t pstate,
			  ipmi_sel_ctx_t sel_ctx,
			  uint8_t *sel_record,
			  unsigned int sel_record_len,
			  int comma_separated_output,
			  int debug,
			  unsigned int flags)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;
  int ret;

  assert (sel_ctx);
  
  if ((ret = _sel_parse_record_string (pstate,
				       sel_ctx,
				       sel_record,
				       sel_record_len,
				       debug,
				       flags,
				       outbuf,
				       &outbuf_len,
				       "%I")) < 0)
    return (-1);
  
  if (!ret)
    return (0);
  
  if (comma_separated_output)
    {
      if (outbuf_len)
        PSTDOUT_PRINTF (pstate, ",%s", outbuf);
      else
        PSTDOUT_PRINTF (pstate, ",%s", EVENT_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        PSTDOUT_PRINTF (pstate, " | %-8s", outbuf);
      else
        PSTDOUT_PRINTF (pstate, " | %-8s", EVENT_NA_STRING);
    }

  return (1);
}

int
event_output_event_direction (pstdout_state_t pstate,
			      ipmi_sel_ctx_t sel_ctx,
			      uint8_t *sel_record,
			      unsigned int sel_record_len,
			      int comma_separated_output,
			      int debug,
			      unsigned int flags)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;
  int ret;

  assert (sel_ctx);
  
  if ((ret = _sel_parse_record_string (pstate,
				       sel_ctx,
				       sel_record,
				       sel_record_len,
				       debug,
				       flags,
				       outbuf,
				       &outbuf_len,
				       "%k")) < 0)
    return (-1);
  
  if (!ret)
    return (0);

  if (comma_separated_output)
    {
      if (outbuf_len)
        PSTDOUT_PRINTF (pstate, ",%s", outbuf);
      else
        PSTDOUT_PRINTF (pstate, ",%s", EVENT_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        PSTDOUT_PRINTF (pstate, " | %-17s", outbuf);
      else
        PSTDOUT_PRINTF (pstate, " | %-17s", EVENT_NA_STRING);
    }

  return (1);
}

int
event_output_not_available_event_direction (pstdout_state_t pstate,
					    int comma_separated_output)
{
  if (comma_separated_output)
    PSTDOUT_PRINTF (pstate, ",%s", EVENT_NA_STRING);
  else
    PSTDOUT_PRINTF (pstate, " | %-17s", EVENT_NA_STRING);
  
  return (1);
}

int
event_output_event (pstdout_state_t pstate,
		    ipmi_sel_ctx_t sel_ctx,
		    uint8_t *sel_record,
		    unsigned int sel_record_len,
		    int comma_separated_output,
		    int debug,
		    unsigned int flags)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len = 0;
  int ret;
  
  assert (sel_ctx);
  
  if ((ret = _sel_parse_record_string (pstate,
                                       sel_ctx,
                                       sel_record,
                                       sel_record_len,
                                       debug,
                                       flags,
                                       outbuf,
                                       &outbuf_len,
				       "%E")) < 0)
    return (-1);
  
  if (!ret)
    return (0); 
  
  if (comma_separated_output)
    {
      if (outbuf_len)
	PSTDOUT_PRINTF (pstate, ",%s", outbuf);
      else
	PSTDOUT_PRINTF (pstate, ",%s", EVENT_NA_STRING);
    }
  else
    {
      if (outbuf_len)
	PSTDOUT_PRINTF (pstate, " | %s", outbuf);
      else
	PSTDOUT_PRINTF (pstate, " | %s", EVENT_NA_STRING);
    }

  return (1);
}

int
event_output_not_available_event (pstdout_state_t pstate,
				  int comma_separated_output)
{
  if (comma_separated_output)
    PSTDOUT_PRINTF (pstate, ",%s", EVENT_NA_STRING);
  else
    PSTDOUT_PRINTF (pstate, " | %s", EVENT_NA_STRING);

  return (1);
}
