/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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

/* return (-1), real error */
static int
_sel_parse_err_handle (pstdout_state_t pstate,
		       ipmi_sel_parse_ctx_t sel_parse_ctx,
		       uint8_t *sel_record,
		       unsigned int sel_record_len,
		       int debug,
		       const char *func)
{
  assert (sel_parse_ctx);
  assert (func);
  
  if (ipmi_sel_parse_ctx_errnum (sel_parse_ctx) == IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY)
    {
      /* most likely bad event data from remote system or user input */
      if (debug)
	{
	  if (sel_record && sel_record_len)
	    PSTDOUT_FPRINTF (pstate,
			     stderr,
			     "Invalid SEL entry read\n");
	  else
	    PSTDOUT_FPRINTF (pstate,
			     stderr,
			     "Invalid event data specified\n");
	}
      return (0);
    }
  
  PSTDOUT_FPRINTF (pstate,
                   stderr,
                   "%s: %s\n",
                   func,
                   ipmi_sel_parse_ctx_errormsg (sel_parse_ctx));
  return (-1);
}

int
event_output_time (pstdout_state_t pstate,
		   ipmi_sel_parse_ctx_t sel_parse_ctx,
		   uint8_t *sel_record,
		   unsigned int sel_record_len,
		   int comma_separated_output,
		   int debug,
		   unsigned int flags)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;

  assert (sel_parse_ctx);

  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
  if (sel_record && sel_record_len)
    {
      if ((outbuf_len = ipmi_sel_parse_format_record_string (sel_parse_ctx,
							     "%t",
							     sel_record,
							     sel_record_len,
							     outbuf,
							     EVENT_OUTPUT_BUFLEN,
							     flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     sel_record,
				     sel_record_len,
				     debug,
				     "ipmi_sel_parse_format_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }
  else
    {
      if ((outbuf_len = ipmi_sel_parse_read_record_string (sel_parse_ctx,
							   "%t",
							   outbuf,
							   EVENT_OUTPUT_BUFLEN,
							   flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     NULL,
				     0,
				     debug,
				     "ipmi_sel_parse_read_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }

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
			  ipmi_sel_parse_ctx_t sel_parse_ctx,
			  ipmi_sdr_cache_ctx_t sdr_cache_ctx,
			  ipmi_sdr_parse_ctx_t sdr_parse_ctx,
			  uint8_t *sel_record,
			  unsigned int sel_record_len,
			  struct sensor_entity_id_counts *entity_id_counts,
			  struct sensor_column_width *column_width,
			  struct sdr_cmd_args *sdr,
			  int entity_sensor_names,
			  int comma_separated_output,
			  int debug,
			  unsigned int flags)
{
  char fmt[EVENT_FMT_BUFLEN + 1];
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;

  assert (sel_parse_ctx);
  assert (sdr_cache_ctx);
  assert (sdr_parse_ctx);
  assert (column_width);

  if (entity_sensor_names
      && !sdr->ignore_sdr_cache)
    {
      uint8_t sensor_number, generator_id;
      uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
      int sdr_record_len = 0;

      if (sel_record && sel_record_len)
	{
	  if (ipmi_sel_parse_record_generator_id (sel_parse_ctx,
						  sel_record,
						  sel_record_len,
						  &generator_id) < 0)
	    {
	      if (_sel_parse_err_handle (pstate,
					 sel_parse_ctx,
					 sel_record,
					 sel_record_len,
					 debug,
					 "ipmi_sel_parse_record_generator_id") < 0)
		return (-1);
	      return (0);
	    }
	  
	  if (ipmi_sel_parse_record_sensor_number (sel_parse_ctx,
						   sel_record,
						   sel_record_len,
						   &sensor_number) < 0)
	    {
	      if (_sel_parse_err_handle (pstate,
					 sel_parse_ctx,
					 sel_record,
					 sel_record_len,
					 debug,
					 "ipmi_sel_parse_record_sensor_number") < 0)
		return (-1);
	      return (0);
	    }
	}
      else
	{
	  if (ipmi_sel_parse_read_generator_id (sel_parse_ctx,
						&generator_id) < 0)
	    {
	      if (_sel_parse_err_handle (pstate,
					 sel_parse_ctx,
					 NULL,
					 0,
					 debug,
					 "ipmi_sel_parse_read_generator_id") < 0)
		return (-1);
	      return (0);
	    }
	  
	  if (ipmi_sel_parse_read_sensor_number (sel_parse_ctx,
						 &sensor_number) < 0)
	    {
	      if (_sel_parse_err_handle (pstate,
					 sel_parse_ctx,
					 NULL,
					 0,
					 debug,
					 "ipmi_sel_parse_read_sensor_number") < 0)
		return (-1);
	      return (0);
	    }
	}

      /* achu: really shouldn't do this, b/c sel-parse library uses                                              
       * this, but sel-parse lib doesn't iterate over the cache, so                                              
       * it's ok.  If we need to later, we'll open a new sdr_cache                                               
       */
      if (ipmi_sdr_cache_search_sensor_wrapper (sdr_cache_ctx,
                                                sensor_number,
                                                generator_id) < 0)
        {
          if (debug)
            PSTDOUT_FPRINTF (pstate,
                             stderr,
                             "ipmi_sdr_cache_search_sensor: %s\n",
                             ipmi_sdr_cache_ctx_errormsg (sdr_cache_ctx));
          goto normal_sensor_output;
        }

      if ((sdr_record_len = ipmi_sdr_cache_record_read (sdr_cache_ctx,
                                                        sdr_record,
                                                        IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sdr_cache_record_read: %s\n",
                           ipmi_sdr_cache_ctx_errormsg (sdr_cache_ctx));
          return (-1);
        }

      memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
      if (get_entity_sensor_name_string (pstate,
                                         sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         entity_id_counts,
                                         &sensor_number,
                                         outbuf,
                                         EVENT_OUTPUT_BUFLEN) < 0)
        return (-1);
      
      outbuf_len = strlen (outbuf);
      if (!outbuf_len)
        goto normal_sensor_output;
    }
  else
    {
    normal_sensor_output:

      memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
      if (sel_record && sel_record_len)
	{
	  if ((outbuf_len = ipmi_sel_parse_format_record_string (sel_parse_ctx,
								 "%s",
								 sel_record,
								 sel_record_len,
								 outbuf,
								 EVENT_OUTPUT_BUFLEN,
								 flags)) < 0)
	    {
	      if (_sel_parse_err_handle (pstate,
                                         sel_parse_ctx,
                                         sel_record,
					 sel_record_len,
                                         debug,
					 "ipmi_sel_parse_format_record_string") < 0)
		return (-1);
	      return (0);
	    }
	}
      else
	{
	  if ((outbuf_len = ipmi_sel_parse_read_record_string (sel_parse_ctx,
							       "%s",
							       outbuf,
							       EVENT_OUTPUT_BUFLEN,
							       flags)) < 0)
	    {
	      if (_sel_parse_err_handle (pstate,
                                         sel_parse_ctx,
                                         NULL,
                                         0,
                                         debug,
					 "ipmi_sel_parse_read_record_string") < 0)
		return (-1);
	      return (0);
	    }
	}
    }

  if (outbuf_len > column_width->sensor_name)
    column_width->sensor_name = outbuf_len;

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

  assert (column_width);

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
			  ipmi_sel_parse_ctx_t sel_parse_ctx,
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

  assert (sel_parse_ctx);
  assert (column_width);

  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
  if (sel_record && sel_record_len)
    {
      if ((outbuf_len = ipmi_sel_parse_format_record_string (sel_parse_ctx,
							     "%T",
							     sel_record,
							     sel_record_len,
							     outbuf,
							     EVENT_OUTPUT_BUFLEN,
							     flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     sel_record,
				     sel_record_len,
				     debug,
				     "ipmi_sel_parse_format_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }
  else
    {
      if ((outbuf_len = ipmi_sel_parse_read_record_string (sel_parse_ctx,
							   "%T",
							   outbuf,
							   EVENT_OUTPUT_BUFLEN,
							   flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     NULL,
				     0,
				     debug,
				     "ipmi_sel_parse_read_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }
  
  if (outbuf_len > column_width->sensor_type)
    column_width->sensor_type = outbuf_len;
  
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

  assert (column_width);
  
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
event_output_event_direction (pstdout_state_t pstate,
			      ipmi_sel_parse_ctx_t sel_parse_ctx,
			      uint8_t *sel_record,
			      unsigned int sel_record_len,
			      int comma_separated_output,
			      int debug,
			      unsigned int flags)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;

  assert (sel_parse_ctx);
  
  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
  if (sel_record && sel_record_len)
    {
      if ((outbuf_len = ipmi_sel_parse_format_record_string (sel_parse_ctx,
							     "%k",
							     sel_record,
							     sel_record_len,
							     outbuf,
							     EVENT_OUTPUT_BUFLEN,
							     flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     sel_record,
				     sel_record_len,
				     debug,
				     "ipmi_sel_parse_format_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }
  else
    {
      if ((outbuf_len = ipmi_sel_parse_read_record_string (sel_parse_ctx,
							   "%k",
							   outbuf,
							   EVENT_OUTPUT_BUFLEN,
							   flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     NULL,
				     0,
				     debug,
				     "ipmi_sel_parse_read_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }
  
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
