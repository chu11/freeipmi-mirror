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


/* struct sensor_column_width *column_width */

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
