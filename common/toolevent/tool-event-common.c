/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

/* return -1 on failout error, 0 on invalid data */
static int
_sel_parse_err_handle (pstdout_state_t pstate,
		       ipmi_sel_parse_ctx_t sel_parse_ctx,
		       int debug,
		       const char *func)
{
  assert (sel_parse_ctx);
  assert (func);
  
  if (ipmi_sel_parse_ctx_errnum (sel_parse_ctx) == IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY)
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
                   ipmi_sel_parse_ctx_errormsg (sel_parse_ctx));
  return (-1);
}

/* return -1 on failout error, 0 on invalid data, 1 otherwise */
static int
_sel_parse_record_string (pstdout_state_t pstate,
			  ipmi_sel_parse_ctx_t sel_parse_ctx,
			  uint8_t *sel_record,
			  unsigned int sel_record_len,
			  int debug,
			  unsigned int flags,
			  char outbuf[EVENT_OUTPUT_BUFLEN + 1],
			  int *outbuf_len,
			  const char *fmt)
{
  assert (sel_parse_ctx);
  assert (outbuf);
  assert (outbuf_len);
  
  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
  if (sel_record && sel_record_len)
    {
      if ((*outbuf_len = ipmi_sel_parse_format_record_string (sel_parse_ctx,
							      fmt,
							      sel_record,
							      sel_record_len,
							      outbuf,
							      EVENT_OUTPUT_BUFLEN,
							      flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_format_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }
  else
    {
      if ((*outbuf_len = ipmi_sel_parse_read_record_string (sel_parse_ctx,
							    fmt,
							    outbuf,
							    EVENT_OUTPUT_BUFLEN,
							    flags)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_read_record_string") < 0)
	    return (-1);
	  return (0);
	}
    }

  return (1);
}

int
event_data_info (pstdout_state_t pstate,
		 ipmi_sel_parse_ctx_t sel_parse_ctx,
		 uint8_t *sel_record,
		 unsigned int sel_record_len,
		 int debug,
		 uint8_t *generator_id,
		 uint8_t *sensor_type,
		 uint8_t *sensor_number,
		 uint8_t *event_type_code,
		 uint8_t *event_data2_flag,
		 uint8_t *event_data3_flag,
		 uint8_t *event_data2,
		 uint8_t *event_data3)
{
  uint8_t sel_record_buf[IPMI_SEL_RECORD_MAX_RECORD_LENGTH];
  int sel_record_buf_len;
  uint8_t *sel_record_ptr;
  unsigned int sel_record_ptr_len;
  
  assert (sel_parse_ctx);

  if (!sel_record || !sel_record_len)
    {
      if ((sel_record_buf_len = ipmi_sel_parse_read_record (sel_parse_ctx,
							    sel_record_buf,
							    IPMI_SEL_RECORD_MAX_RECORD_LENGTH)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_read_record") < 0)
	    return (-1);
	  return (0);
	}
      
      if (!sel_record_buf_len)
	return (0);
      
      sel_record_ptr = sel_record_buf;
      sel_record_ptr_len = sel_record_buf_len;
    }
  else
    {
      sel_record_ptr = sel_record;
      sel_record_ptr_len = sel_record_len;
    }
  
  if (generator_id)
    {
      if (ipmi_sel_parse_record_generator_id (sel_parse_ctx,
					      sel_record_ptr,
					      sel_record_ptr_len,
					      generator_id) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_generator_id") < 0)
	    return (-1);
	  return (0);
	}
    }
  
  if (sensor_type)
    {
      if (ipmi_sel_parse_record_sensor_type (sel_parse_ctx,
					     sel_record_ptr,
					     sel_record_ptr_len,
					     sensor_type) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_sensor_type") < 0)
	    return (-1);
	  return (0);
	}
    }
  
  if (sensor_number)
    {
      if (ipmi_sel_parse_record_sensor_number (sel_parse_ctx,
					       sel_record_ptr,
					       sel_record_ptr_len,
					       sensor_number) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_sensor_number") < 0)
	    return (-1);
	  return (0);
	}
    }

  if (event_type_code)
    {
      if (ipmi_sel_parse_record_event_type_code (sel_parse_ctx,
						 sel_record_ptr,
						 sel_record_ptr_len,
						 event_type_code) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_event_type_code") < 0)
	    return (-1);
	  return (0);
	}
    }

  if (event_data2_flag)
    {
      if (ipmi_sel_parse_record_event_data1_event_data2_flag (sel_parse_ctx,
							      sel_record_ptr,
							      sel_record_ptr_len,
							      event_data2_flag) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_event_data1_event_data2_flag") < 0)
	    return (-1);
	  return (0);
	}
    }

  if (event_data3_flag)
    {
      if (ipmi_sel_parse_record_event_data1_event_data3_flag (sel_parse_ctx,
							      sel_record_ptr,
							      sel_record_ptr_len,
							      event_data3_flag) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_event_data1_event_data3_flag") < 0)
	    return (-1);
	  return (0);
	}
    }

  if (event_data2)
    {
      if (ipmi_sel_parse_record_event_data2 (sel_parse_ctx,
					     sel_record_ptr,
					     sel_record_ptr_len,
					     event_data2) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_event_data2") < 0)
	    return (-1);
	  return (0);
	}
    }

  if (event_data3)
    {
      if (ipmi_sel_parse_record_event_data3 (sel_parse_ctx,
					     sel_record_ptr,
					     sel_record_ptr_len,
					     event_data3) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_record_event_data3") < 0)
	    return (-1);
	  return (0);
	}
    }

  return (1);
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
  int ret;

  assert (sel_parse_ctx);

  if ((ret = _sel_parse_record_string (pstate,
				       sel_parse_ctx,
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
  int ret;

  assert (sel_parse_ctx);
  assert (sdr_cache_ctx);
  assert (sdr_parse_ctx);
  assert (!entity_sensor_names || (entity_sensor_names && entity_id_counts));
  assert (column_width || (!column_width && comma_separated_output));

  if (entity_sensor_names
      && !sdr->ignore_sdr_cache)
    {
      uint8_t generator_id, sensor_number;
      uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
      int sdr_record_len = 0;

      if ((ret = event_data_info (pstate,
				  sel_parse_ctx,
				  sel_record,
				  sel_record_len,
				  debug,
				  &generator_id,
				  NULL,
				  &sensor_number,
				  NULL,
				  NULL,
				  NULL,
				  NULL,
				  NULL)) < 0)
	return (-1);

      if (!ret)
	return (0);

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

      if ((ret = _sel_parse_record_string (pstate,
					   sel_parse_ctx,
					   sel_record,
					   sel_record_len,
					   debug,
					   flags,
					   outbuf,
					   &outbuf_len,
					   "%s")) < 0)
	return (-1);
      
      if (!ret)
	return (0);
    }

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
  int ret;

  assert (sel_parse_ctx);
  assert (column_width || (!column_width && comma_separated_output));

  if ((ret = _sel_parse_record_string (pstate,
				       sel_parse_ctx,
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
			  ipmi_sel_parse_ctx_t sel_parse_ctx,
			  ipmi_interpret_ctx_t interpret_ctx,
			  uint8_t *sel_record,
			  unsigned int sel_record_len,
			  int comma_separated_output,
			  int debug,
			  unsigned int flags)
{
  uint8_t sel_record_buf[IPMI_SEL_RECORD_MAX_RECORD_LENGTH];
  int sel_record_buf_len;
  uint8_t *sel_record_ptr;
  unsigned int sel_record_ptr_len;
  unsigned int sel_state;
  char *sel_state_str = NULL;
  
  assert (sel_parse_ctx);
  assert (interpret_ctx);

  if (!sel_record || !sel_record_len)
    {
      if ((sel_record_buf_len = ipmi_sel_parse_read_record (sel_parse_ctx,
							    sel_record_buf,
							    IPMI_SEL_RECORD_MAX_RECORD_LENGTH)) < 0)
	{
	  if (_sel_parse_err_handle (pstate,
				     sel_parse_ctx,
				     debug,
				     "ipmi_sel_parse_read_record") < 0)
	    return (-1);
	  return (0);
	}
 
      if (!sel_record_buf_len)
	return (0);

      sel_record_ptr = sel_record_buf;
      sel_record_ptr_len = sel_record_buf_len;
    }
  else
    {
      sel_record_ptr = sel_record;
      sel_record_ptr_len = sel_record_len;
    }

  if (ipmi_interpret_sel (interpret_ctx,
			  sel_record_ptr,
			  sel_record_ptr_len,
                          &sel_state) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_interpret_sel: %s\n",
                       ipmi_interpret_ctx_errormsg (interpret_ctx));
      return (-1);
    }

  if (sel_state == IPMI_INTERPRET_STATE_NOMINAL)
    sel_state_str = "Nominal";
  else if (sel_state == IPMI_INTERPRET_STATE_WARNING)
    sel_state_str = "Warning";
  else if (sel_state == IPMI_INTERPRET_STATE_CRITICAL)
    sel_state_str = "Critical";
  else
    sel_state_str = EVENT_NA_STRING;

  if (comma_separated_output)
    PSTDOUT_PRINTF (pstate, ",%s", sel_state_str);
  else
    PSTDOUT_PRINTF (pstate, " | %-8s", sel_state_str);

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
  int ret;

  assert (sel_parse_ctx);
  
  if ((ret = _sel_parse_record_string (pstate,
				       sel_parse_ctx,
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
		    ipmi_sel_parse_ctx_t sel_parse_ctx,
		    uint8_t *sel_record,
		    unsigned int sel_record_len,
		    struct ipmi_oem_data *oem_data,
		    int interpret_oem_data,
		    int comma_separated_output,
		    int debug,
		    unsigned int flags)
{
  char fmtbuf[EVENT_FMT_BUFLEN+1];
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len = 0;
  char *fmt;
  uint8_t event_type_code;
  uint8_t event_data2_flag;
  uint8_t event_data3_flag;
  int check_for_half_na = 0;
  int ret;

  assert (sel_parse_ctx);

  if ((ret = _sel_parse_record_string (pstate,
                                       sel_parse_ctx,
                                       sel_record,
                                       sel_record_len,
                                       debug,
                                       flags,
                                       outbuf,
                                       &outbuf_len,
				       "%e")) < 0)
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

  if ((ret = event_data_info (pstate,
			      sel_parse_ctx,
			      sel_record,
			      sel_record_len,
			      debug,
			      NULL,
			      NULL,
			      NULL,
			      &event_type_code,
			      &event_data2_flag,
			      &event_data3_flag,
			      NULL,
			      NULL)) < 0)
    return (-1);
  
  if (!ret)
    return (0);

  /* note: previously set sel parse library separator to " ; " 
   * so some places where there could be two outputs
   * would be separated by a semi-colon 
   */

  memset (fmtbuf, '\0', EVENT_FMT_BUFLEN+1);

  /* OEM Interpretation 
   * 
   * Dell Poweredge 2900
   * Dell Poweredge 2950
   * Dell Poweredge R610
   * Dell Poweredge R710
   * 
   * Unique condition, event_data2_flag and event_data3_flag are 
   * listed as "unspecified", so we need to handle this as a
   * special case.
   */
  if (interpret_oem_data
      && oem_data->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (oem_data->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
	  || oem_data->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
	  || oem_data->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
	  || oem_data->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710))
    {
      uint8_t sensor_number;
      uint8_t sensor_type;
      
      if ((ret = event_data_info (pstate,
				  sel_parse_ctx,
				  sel_record,
				  sel_record_len,
				  debug,
				  NULL,
				  &sensor_type,
				  &sensor_number,
				  NULL,
				  NULL,
				  NULL,
				  NULL,
				  NULL)) < 0)
	return (-1);
      
      if (!ret)
	return (0);
      
      if (sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
	  && event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA)
	{
	  strcat (fmtbuf, "%f ; %h");
	  goto output;
	}
    }

  if (event_data2_flag != IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE
      && event_data3_flag != IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE)
    {
      /* will effectively output "%f ; %h" if combined output not 
       * available or reasonable 
       */
      strcat (fmtbuf, "%c");
      check_for_half_na++;
    }
  else if (event_data2_flag != IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE)
    strcat (fmtbuf, "%f");
  else if (event_data3_flag != IPMI_SEL_EVENT_DATA_UNSPECIFIED_BYTE)
    strcat (fmtbuf, "%h");
  else
    goto out;

 output:

  fmt = fmtbuf;

  if ((ret = _sel_parse_record_string (pstate,
                                       sel_parse_ctx,
                                       sel_record,
                                       sel_record_len,
                                       debug,
                                       flags,
                                       outbuf,
                                       &outbuf_len,
				       fmt)) < 0)
    return (-1);
  
  if (!ret)
    return (0); 

  /* If event_data2 and event_data3 flags are valid, it normally
   * shouldn't be possible that we read "N/A". However, it happens, 
   * most notably when the event_data2 and / or event_data3 data are 
   * 0xFF.
   */
  if (!strcasecmp (outbuf, EVENT_NA_STRING))
    outbuf_len = 0;

  /* Special case: 
   *
   * It's possible the SEL record event_data2_flag and 
   * event_data3_flag are bad, and you get a N/A output anyways 
   * (perhaps b/c the event_data2 or event_data3 data is unspecified).
   * If this is the case you can get a strange: "N/A ; text" or "text 
   * ; N/A" instead of just "text". Deal with it appropriately.
   *
   */
  if (outbuf_len && check_for_half_na)
    {
      char *na_ptr;
      char *semicolon_ptr;

      if ((na_ptr = strstr (outbuf, EVENT_NA_STRING))
	  && (semicolon_ptr = strstr (outbuf, EVENT_OUTPUT_SEPARATOR)))
	{
	  memset (fmtbuf, '\0', EVENT_FMT_BUFLEN+1);

	  if (na_ptr < semicolon_ptr)
	    strcat (fmtbuf, "%h");
	  else
	    strcat (fmtbuf, "%f");

	  fmt = fmtbuf;

	  if ((ret = _sel_parse_record_string (pstate,
					       sel_parse_ctx,
					       sel_record,
					       sel_record_len,
					       debug,
					       flags,
					       outbuf,
					       &outbuf_len,
					       fmt)) < 0)
	    return (-1);
	  
	  if (!ret)
	    return (0); 
	}
    }
  
  if (outbuf_len)
    PSTDOUT_PRINTF (pstate, " ; %s", outbuf);
  
 out:
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
