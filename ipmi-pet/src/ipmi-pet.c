/*
 * Copyright (C) 2011 FreeIPMI Core Team
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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-pet.h"
#include "ipmi-pet-argp.h"

#include "freeipmi-portability.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_PET_FMT_BUFLEN      4096

#define IPMI_PET_OUTPUT_BUFLEN   4096

#define IPMI_PET_NA_STRING       "N/A"

#define IPMI_PET_EVENT_SEPARATOR " ; "

#define IPMI_PET_EVENT_SEVERITY_HEADER "Severity"

static int
_ipmi_pet_init (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  int rv = -1;
 
  assert (state_data);

  args = state_data->prog_data->args;

  if (!args->sdr.ignore_sdr_cache)
    {
      struct sensor_entity_id_counts *entity_ptr = NULL;
      
      if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                     NULL,
                                     state_data->ipmi_ctx,
                                     args->sdr.quiet_cache,
                                     args->sdr.sdr_cache_recreate,
                                     state_data->hostname,
                                     args->sdr.sdr_cache_directory) < 0)
        goto cleanup;

      if (args->entity_sensor_names)
	{
	  if (calculate_entity_id_counts (NULL,
					  state_data->sdr_cache_ctx,
					  state_data->sdr_parse_ctx,
					  &(state_data->entity_id_counts)) < 0)
	    goto cleanup;
	  
	  entity_ptr = &(state_data->entity_id_counts);
	}
      
      if (calculate_column_widths (NULL,
				   state_data->sdr_cache_ctx,
				   state_data->sdr_parse_ctx,
				   NULL,
				   0,
				   NULL,
				   0,
				   state_data->prog_data->args->non_abbreviated_units,
				   (entity_ptr) ? 1 : 0, /* shared_sensors */
				   1, /* count_event_only_records */
				   0, /* count_device_locator_records */
				   0, /* count_oem_records */
				   entity_ptr,
				   &(state_data->column_width)) < 0)
	goto cleanup;
    }
  else
    {
      if (calculate_column_widths_ignored_sdr_cache (state_data->prog_data->args->non_abbreviated_units,
						     &(state_data->column_width)) < 0)
	goto cleanup;
    }

  if (args->interpret_oem_data)
    {
      if (!args->manufacturer_id_set
	  && !args->product_id_set)
	{
	  if (ipmi_get_oem_data (NULL,
				 state_data->ipmi_ctx,
				 &state_data->oem_data) < 0)
	    goto cleanup;
	}
      else
	{
	  state_data->oem_data.manufacturer_id = args->manufacturer_id;
	  state_data->oem_data.product_id = args->product_id;
	}

      if (ipmi_sel_parse_ctx_set_manufacturer_id (state_data->sel_parse_ctx,
						  state_data->oem_data.manufacturer_id) < 0)
	{
	  fprintf (stderr,
		   "ipmi_sel_parse_ctx_set_manufacturer_id: %s\n",
		   ipmi_sel_parse_ctx_errormsg (state_data->sel_parse_ctx));
	  goto cleanup;
	}
      
      if (ipmi_sel_parse_ctx_set_product_id (state_data->sel_parse_ctx,
					     state_data->oem_data.product_id) < 0)
	{
	  fprintf (stderr,
		   "ipmi_sel_parse_ctx_set_product_id: %s\n",
		   ipmi_sel_parse_ctx_errormsg (state_data->sel_parse_ctx));
	  goto cleanup;
	}
      
      if (args->output_event_state)
	{
	  if (ipmi_interpret_ctx_set_manufacturer_id (state_data->interpret_ctx,
						      state_data->oem_data.manufacturer_id) < 0)
	    {
	      fprintf (stderr,
		       "ipmi_interpret_ctx_set_manufacturer_id: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
	      goto cleanup;
	    }
	  
	  if (ipmi_interpret_ctx_set_product_id (state_data->interpret_ctx,
						 state_data->oem_data.product_id) < 0)
	    {
	      fprintf (stderr,
		       "ipmi_interpret_ctx_set_product_id: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
	      goto cleanup;
	    }
	}
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_pet_output_headers (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);

  if (!state_data->prog_data->args->no_header_output
      && !state_data->output_headers)
    {
      if (state_data->prog_data->args->comma_separated_output)
        {
          if (state_data->prog_data->args->no_sensor_type_output)
            printf ("Date,Time,%s",
		    SENSORS_HEADER_NAME_STR);
          else
            printf ("Date,Time,%s,%s",
		    SENSORS_HEADER_NAME_STR,
		    SENSORS_HEADER_TYPE_STR);
	  
	  if (state_data->prog_data->args->output_event_severity)
	    printf (",%s", IPMI_PET_EVENT_SEVERITY_HEADER);

          if (state_data->prog_data->args->output_event_state)
            printf (",%s", SENSORS_HEADER_STATE_STR);
	  
          if (state_data->prog_data->args->verbose_count >= 1)
            printf (",Event Direction");
          
          printf (",Event\n");
        }
      else
        {          
	  char fmt[IPMI_PET_FMT_BUFLEN+1];

          memset (fmt, '\0', IPMI_PET_FMT_BUFLEN + 1);

          if (state_data->prog_data->args->no_sensor_type_output)
            {
              snprintf (fmt,
                        IPMI_PET_FMT_BUFLEN,
                        "Date        | Time     | %%-%ds",
                        state_data->column_width.sensor_name);
              
              printf (fmt, SENSORS_HEADER_NAME_STR);
            }
          else
            {
              snprintf (fmt,
                        IPMI_PET_FMT_BUFLEN,
                        "Date        | Time     | %%-%ds | %%-%ds",
                        state_data->column_width.sensor_name,
                        state_data->column_width.sensor_type);
              
              printf (fmt,
		      SENSORS_HEADER_NAME_STR,
		      SENSORS_HEADER_TYPE_STR);
            }
          
	  if (state_data->prog_data->args->output_event_severity)
	    printf (" | %-25s", IPMI_PET_EVENT_SEVERITY_HEADER);

          if (state_data->prog_data->args->output_event_state)
            printf (" | %s   ", SENSORS_HEADER_STATE_STR);
	  
          if (state_data->prog_data->args->verbose_count >= 1)
            printf (" | Event Direction  ");
          
          printf (" | Event\n");
        }

      state_data->output_headers++;
    }

  return (0);
}

/* return (-1), real error */
static int
_sel_parse_err_handle (ipmi_pet_state_data_t *state_data, char *func)
{
  assert (state_data);
  assert (func);

  if (ipmi_sel_parse_ctx_errnum (state_data->sel_parse_ctx) == IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY)
    {
      if (state_data->prog_data->args->common.debug)
        fprintf (stderr,
		 "Invalid PET data input\n");
      return (0);
    }

  fprintf (stderr,
	   "%s: %s\n",
	   func,
	   ipmi_sel_parse_ctx_errormsg (state_data->sel_parse_ctx));
  return (-1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_date_and_time (ipmi_pet_state_data_t *state_data,
			      uint8_t *sel_record,
			      unsigned int sel_record_len,
			      unsigned int flags)
{
  char outbuf[IPMI_PET_OUTPUT_BUFLEN+1];
  int outbuf_len;

  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);

  memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
  if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
							 "%d",
							 sel_record,
							 sel_record_len,
							 outbuf,
							 IPMI_PET_OUTPUT_BUFLEN,
							 flags)) < 0)
    {
      if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
        return (-1);
      return (0);
    }

  if (state_data->prog_data->args->comma_separated_output)
    {
      if (outbuf_len)
        printf ("%s", outbuf);
      else
        printf ("%s", IPMI_PET_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        printf ("%-11s", outbuf);
      else
        printf ("%-11s", IPMI_PET_NA_STRING);
    }

  memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
  if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
							 "%t",
							 sel_record,
                                                         sel_record_len,
							 outbuf,
							 IPMI_PET_OUTPUT_BUFLEN,
							 flags)) < 0)
    {
      if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
        return (-1);
      return (0);
    }

  if (state_data->prog_data->args->comma_separated_output)
    {
      if (outbuf_len)
        printf (",%s", outbuf);
      else
        printf (",%s", IPMI_PET_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        printf (" | %-8s", outbuf);
      else
        printf (" | %-8s", IPMI_PET_NA_STRING);
    }

  return (1);
}


/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_not_available_date_and_time (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);

  if (state_data->prog_data->args->comma_separated_output)
    printf (",%s,%s", IPMI_PET_NA_STRING, IPMI_PET_NA_STRING);
  else
    printf (" | %-11s | %-8s", IPMI_PET_NA_STRING, IPMI_PET_NA_STRING);

  return (1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_sensor_name_and_type (ipmi_pet_state_data_t *state_data,
				     uint8_t *sel_record,
				     unsigned int sel_record_len,
				     unsigned int flags)
{
  char fmt[IPMI_PET_FMT_BUFLEN + 1];
  char outbuf[IPMI_PET_OUTPUT_BUFLEN+1];
  int outbuf_len;

  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);

  if (state_data->prog_data->args->entity_sensor_names
      && !state_data->prog_data->args->sdr.ignore_sdr_cache)
    {
      uint8_t sensor_number, generator_id;
      uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
      int sdr_record_len = 0;
      
      if (ipmi_sel_parse_record_generator_id (state_data->sel_parse_ctx,
					      sel_record,
					      sel_record_len,
					      &generator_id) < 0)
        {
          if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_record_generator_id") < 0)
            return (-1);
          return (0);
        }

      if (ipmi_sel_parse_record_sensor_number (state_data->sel_parse_ctx,
					       sel_record,
					       sel_record_len,
					       &sensor_number) < 0)
        {
          if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_record_sensor_number") < 0)
            return (-1);
          return (0);
        }
      
      /* achu: really shouldn't do this, b/c sel-parse library uses
       * this, but sel-parse lib doesn't iterate over the cache, so
       * it's ok.  If we need to later, we'll open a new sdr_cache
       */
      if (ipmi_sdr_cache_search_sensor (state_data->sdr_cache_ctx,
                                        sensor_number,
                                        generator_id) < 0)
        {
          /* IPMI Workaround (achu)
           *
           * Discovered on Supermicro H8QME with SIMSO daughter card.
           *
           * The slave address is reportedly incorrectly by having the
           * generator_id be shifted over by one.  This is a special
           * "try again" corner case.
           */
          if (ipmi_sdr_cache_ctx_errnum (state_data->sdr_cache_ctx) == IPMI_SDR_CACHE_ERR_NOT_FOUND
              && (generator_id == (IPMI_SLAVE_ADDRESS_BMC << 1)))
            {
              if (!ipmi_sdr_cache_search_sensor (state_data->sdr_cache_ctx,
                                                 sensor_number,
                                                 (generator_id >> 1)))
                goto fall_through;
              /* else fall through to normal error path */
            }
          
          goto normal_sensor_output;
        }

    fall_through:
      
      if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_cache_ctx,
                                                        sdr_record,
                                                        IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
        {
          fprintf (stderr,
		   "ipmi_sdr_cache_record_read: %s\n",
		   ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
          return (-1);
        }

      memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
      if (get_entity_sensor_name_string (NULL,
                                         state_data->sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         &(state_data->entity_id_counts),
                                         &sensor_number,
                                         outbuf,
                                         IPMI_PET_OUTPUT_BUFLEN) < 0)
        return (-1);

      outbuf_len = strlen (outbuf);
      if (!outbuf_len)
        goto normal_sensor_output;
    }
  else
    {
    normal_sensor_output:

      memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
      if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
							     "%s",
							     sel_record,
							     sel_record_len,
							     outbuf,
							     IPMI_PET_OUTPUT_BUFLEN,
							     flags)) < 0)
        {
          if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
            return (-1);
          return (0);
        }
    }

  if (outbuf_len > state_data->column_width.sensor_name)
    state_data->column_width.sensor_name = outbuf_len;

  memset (fmt, '\0', IPMI_PET_FMT_BUFLEN + 1);
  if (state_data->prog_data->args->comma_separated_output)
    snprintf (fmt,
              IPMI_PET_FMT_BUFLEN,
              ",%%s");
  else
    snprintf (fmt,
              IPMI_PET_FMT_BUFLEN,
              " | %%-%ds",
              state_data->column_width.sensor_name);

  if (outbuf_len)
    printf (fmt, outbuf);
  else
    printf (fmt, IPMI_PET_NA_STRING);

  if (!state_data->prog_data->args->no_sensor_type_output)
    {
      memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
      if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
							     "%T",
							     sel_record,
							     sel_record_len,
							     outbuf,
							     IPMI_PET_OUTPUT_BUFLEN,
							     flags)) < 0)
        {
          if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
            return (-1);
          return (0);
        }
      
      if (outbuf_len > state_data->column_width.sensor_type)
        state_data->column_width.sensor_type = outbuf_len;
      
      memset (fmt, '\0', IPMI_PET_FMT_BUFLEN + 1);
      if (state_data->prog_data->args->comma_separated_output)
        snprintf (fmt,
                  IPMI_PET_FMT_BUFLEN,
                  ",%%s");
      else
        snprintf (fmt,
                  IPMI_PET_FMT_BUFLEN,
                  " | %%-%ds",
                  state_data->column_width.sensor_type);
      
      if (outbuf_len)
        printf (fmt, outbuf);
      else
        printf (fmt, IPMI_PET_NA_STRING);
    }

  return (1);
}
                        
/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_event_severity (ipmi_pet_state_data_t *state_data,
			       uint8_t event_severity,
			       unsigned int flags)
{
  char *str = NULL;

  assert (state_data);
  
  switch (event_severity)
    {
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_UNSPECIFIED:
      str = IPMI_PET_NA_STRING;
      break;
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_MONITOR:
      str = "Monitor";
      break;
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_INFORMATION:
      str = "Information";
      break;
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_OK:
      str = "Ok";
      break;
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_NON_CRITICAL_CONDITION:
      str = "Non-critical condition";
      break;
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_CRITICAL_CONDITION:
      str = "Critical condition";
      break;
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION:
      str = "Non-recoverable condition";
      break;
    default:
      str = "Unspecified";
      break;
    }
  
  if (state_data->prog_data->args->comma_separated_output)
    printf (",%s", str);
  else
    printf (" | %-25s", str);

  return (1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_event_state (ipmi_pet_state_data_t *state_data,
			    uint8_t *sel_record,
			    unsigned int sel_record_len,
			    unsigned int flags)
{
  unsigned int sel_state;
  char *sel_state_str = NULL;

  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);
  assert (state_data->prog_data->args->output_event_state);

  if (ipmi_interpret_sel (state_data->interpret_ctx,
                          sel_record,
                          sel_record_len,
                          &sel_state) < 0)
    {
      fprintf (stderr,
	       "ipmi_interpret_sel: %s\n",
	       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
      return (-1);
    }

  if (sel_state == IPMI_INTERPRET_STATE_NOMINAL)
    sel_state_str = "Nominal";
  else if (sel_state == IPMI_INTERPRET_STATE_WARNING)
    sel_state_str = "Warning";
  else if (sel_state == IPMI_INTERPRET_STATE_CRITICAL)
    sel_state_str = "Critical";
  else
    sel_state_str = IPMI_PET_NA_STRING;

  if (state_data->prog_data->args->comma_separated_output)
    printf (",%s", sel_state_str);
  else
    printf (" | %-8s", sel_state_str);

  return (1);
}
                  
/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_event_direction (ipmi_pet_state_data_t *state_data,
				uint8_t *sel_record,
				unsigned int sel_record_len,
				unsigned int flags)
{
  char outbuf[IPMI_PET_OUTPUT_BUFLEN+1];
  int outbuf_len;

  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);
  assert (state_data->prog_data->args->verbose_count >= 1);

  memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
  if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
							 "%k",
							 sel_record,
							 sel_record_len,
							 outbuf,
							 IPMI_PET_OUTPUT_BUFLEN,
							 flags)) < 0)
    {
      if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
        return (-1);
      return (0);
    }

  if (state_data->prog_data->args->comma_separated_output)
    {
      if (outbuf_len)
        printf (",%s", outbuf);
      else
        printf (",%s", IPMI_PET_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        printf (" | %-17s", outbuf);
      else
        printf (" | %-17s", IPMI_PET_NA_STRING);
    }

  return (1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_get_system_event_record_info (ipmi_pet_state_data_t *state_data,
			       uint8_t *sel_record,
			       unsigned int sel_record_len,
                               uint8_t *event_type_code,
                               uint8_t *event_data2_flag,
                               uint8_t *event_data3_flag,
                               uint8_t *event_data2,
                               uint8_t *event_data3)
{
  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);
  assert (event_type_code);
  assert (event_data2_flag);
  assert (event_data3_flag);
  assert (event_data2);
  assert (event_data3);

  if (ipmi_sel_parse_record_event_type_code (state_data->sel_parse_ctx,
					     sel_record,
					     sel_record_len,
					     event_type_code) < 0)
    {
      if (_sel_parse_err_handle (state_data,
                                 "ipmi_sel_parse_record_event_type_code") < 0)
        return (-1);
      return (0);
    }

  if (ipmi_sel_parse_record_event_data1_event_data2_flag (state_data->sel_parse_ctx,
							  sel_record,
							  sel_record_len,
							  event_data2_flag) < 0)
    {
      if (_sel_parse_err_handle (state_data,
                                 "ipmi_sel_parse_record_event_data1_event_data2_flag") < 0)
        return (-1);
      return (0);
    }

  if (ipmi_sel_parse_record_event_data1_event_data3_flag (state_data->sel_parse_ctx,
							  sel_record,
							  sel_record_len,
							  event_data3_flag) < 0)
    {
      if (_sel_parse_err_handle (state_data,
                                 "ipmi_sel_parse_record_event_data1_event_data3_flag") < 0)
        return (-1);
      return (0);
    }

  if (ipmi_sel_parse_record_event_data2 (state_data->sel_parse_ctx,
					 sel_record,
					 sel_record_len,
					 event_data2) < 0)
    {
      if (_sel_parse_err_handle (state_data,
                                 "ipmi_sel_parse_record_event_data2") < 0)
        return (-1);
      return (0);
    }

  if (ipmi_sel_parse_record_event_data3 (state_data->sel_parse_ctx,
					 sel_record,
					 sel_record_len,
					 event_data3) < 0)
    {
      if (_sel_parse_err_handle (state_data,
                                 "ipmi_sel_parse_record_event_data3") < 0)
        return (-1);
      return (0);
    }

  return (1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_event (ipmi_pet_state_data_t *state_data,
		      uint8_t *sel_record,
		      unsigned int sel_record_len,
		      unsigned int flags)
{
  char fmtbuf[IPMI_PET_OUTPUT_BUFLEN+1];
  char outbuf[IPMI_PET_OUTPUT_BUFLEN+1];
  int outbuf_len = 0;
  char *fmt;
  uint8_t event_type_code;
  uint8_t event_data2_flag;
  uint8_t event_data3_flag;
  uint8_t event_data2;
  uint8_t event_data3;
  uint8_t sensor_type;
  uint8_t event_data1_offset;
  int check_for_half_na = 0;
  int ret;

  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);

  memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
  if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
							 "%e",
							 sel_record,
							 sel_record_len,
							 outbuf,
							 IPMI_PET_OUTPUT_BUFLEN,
							 flags)) < 0)
    {
      if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
	return (-1);
      return (0);
    }

  if (state_data->prog_data->args->comma_separated_output)
    {
      if (outbuf_len)
        printf (",%s", outbuf);
      else
        printf (",%s", IPMI_PET_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        printf (" | %s", outbuf);
      else
        printf (" | %s", IPMI_PET_NA_STRING);
    }

  if ((ret = _get_system_event_record_info (state_data,
					    sel_record,
					    sel_record_len,
                                            &event_type_code,
                                            &event_data2_flag,
                                            &event_data3_flag,
                                            &event_data2,
                                            &event_data3)) < 0)
    return (-1);

  if (!ret)
    return (0);

  if (ipmi_sel_parse_record_sensor_type (state_data->sel_parse_ctx,
					 sel_record,
					 sel_record_len,
					 &sensor_type) < 0)
    {
      if (_sel_parse_err_handle (state_data,
                                 "ipmi_sel_parse_record_sensor_type") < 0)
        return (-1);
      return (0);
    }
    
  if (ipmi_sel_parse_record_event_data1_offset_from_event_reading_type_code (state_data->sel_parse_ctx,
									     sel_record,
									     sel_record_len,
									     &event_data1_offset) < 0)
    {
      if (_sel_parse_err_handle (state_data,
                                 "ipmi_sel_parse_record_event_data1_offset_from_event_reading_type_code") < 0)
        return (-1);
      return (0);
    }
  
  /* note: previously set sel parse library separator to " ; "
   * so some places where there could be two outputs
   * would be separated by a semi-colon
   */

  memset (fmtbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);

  if (state_data->prog_data->args->interpret_oem_data)
    {
      uint8_t generator_id;
      uint8_t sensor_number;

      if (ipmi_sel_parse_record_generator_id (state_data->sel_parse_ctx,
					      sel_record,
					      sel_record_len,
					      &generator_id) < 0)
        {
          if (_sel_parse_err_handle (state_data,
                                     "ipmi_sel_parse_record_generator_id") < 0)
            return (-1);
        }

      if (ipmi_sel_parse_record_sensor_number (state_data->sel_parse_ctx,
					       sel_record,
					       sel_record_len,
					       &sensor_number) < 0)
        {
          if (_sel_parse_err_handle (state_data,
                                     "ipmi_sel_parse_record_sensor_number") < 0)
            return (-1);
        }
    
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
      if (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
          && (state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900
              || state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950
              || state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
              || state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
          && sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
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

  memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
  if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
							 fmt,
							 sel_record,
							 sel_record_len,
							 outbuf,
							 IPMI_PET_OUTPUT_BUFLEN,
							 flags)) < 0)
    {
      if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
        return (-1);
      return (0);
    }

  /* If event_data2 and event_data3 flags are valid, it normally
   * shouldn't be possible that we read "N/A".  However, it happens,
   * most notably when the event_data2 and / or event_data3 data are
   * 0xFF.
   */
  if (!strcasecmp (outbuf, IPMI_PET_NA_STRING))
    outbuf_len = 0;

  /* Special case:
   * 
   * It's possible the SEL record event_data2_flag and
   * event_data3_flag are bad, and you get a N/A output anyways
   * (perhaps b/c the event_data2 or event_data3 data is unspecified).
   * If this is the case you can get a strange: "N/A ; text" or "text
   * ; N/A" instead of just "text".  Deal with it appropriately.
   * 
   */
  if (outbuf_len && check_for_half_na)
    {
      char *na_ptr;
      char *semicolon_ptr;

      if ((na_ptr = strstr (outbuf, IPMI_PET_NA_STRING))
          && (semicolon_ptr = strstr (outbuf, IPMI_PET_EVENT_SEPARATOR)))
        {
          memset (fmtbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);

          if (na_ptr < semicolon_ptr)
            strcat (fmtbuf, "%h");
          else
            strcat (fmtbuf, "%f");

          fmt = fmtbuf;
          
          memset (outbuf, '\0', IPMI_PET_OUTPUT_BUFLEN+1);
          if ((outbuf_len = ipmi_sel_parse_format_record_string (state_data->sel_parse_ctx,
								 fmt,
								 sel_record,
								 sel_record_len,
								 outbuf,
								 IPMI_PET_OUTPUT_BUFLEN,
								 flags)) < 0)
            {
              if (_sel_parse_err_handle (state_data, "ipmi_sel_parse_format_record_string") < 0)
                return (-1);
              return (0);
            }
        }
    }

  if (outbuf_len)
    printf (" ; %s", outbuf);

 out:
  return (1);
}
                                       
/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_normal_output_not_available_event (ipmi_pet_state_data_t *state_data, unsigned int flags)
{
  assert (state_data);

  if (state_data->prog_data->args->comma_separated_output)
    printf (",%s", IPMI_PET_NA_STRING);
  else
    printf (" | %s", IPMI_PET_NA_STRING);

  return (1);
}

static int
_ipmi_pet_cmdline (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  uint32_t value;
  uint8_t sensor_type;
  uint8_t event_type;
  uint8_t event_direction;
  uint8_t event_offset;
  uint8_t guid[IPMI_SYSTEM_GUID_LENGTH];
  uint32_t localtimestamp;
  int16_t utcoffset;
  uint8_t event_severity;
  uint8_t sensor_device;
  uint8_t sensor_number;
  uint8_t entity;
  uint8_t entity_instance;
  uint8_t event_data[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_DATA_LENGTH];
  uint32_t manufacturer_id;
  uint16_t system_id;
  uint8_t oem_custom[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_OEM_CUSTOM_FIELDS_LENGTH];
  fiid_obj_t sel_system_event_record = NULL;
  fiid_obj_t sel_system_event_record_event_fields = NULL;
  uint8_t event_offset_test;
  uint8_t sel_record[IPMI_SEL_RECORD_MAX_RECORD_LENGTH];
  unsigned int flags = 0;
  uint64_t val;
  int sel_record_len;
  int rv = -1;
  int ret;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->specific_trap_set);
  assert (state_data->prog_data->args->variable_bindings);
  assert (state_data->prog_data->args->variable_bindings_length);
  
  args = state_data->prog_data->args;

  if (!(args->variable_bindings_length >= IPMI_PLATFORM_EVENT_TRAP_MIN_VARIABLE_BINDINGS_LENGTH
	&& args->variable_bindings_length <= IPMI_PLATFORM_EVENT_TRAP_MAX_VARIABLE_BINDINGS_LENGTH))
    {
      fprintf (stderr,
	       "Invalid number of variable binding bytes\n");
      goto cleanup;
    }

  if (_ipmi_pet_init (state_data) < 0)
    goto cleanup;

  if (_ipmi_pet_output_headers (state_data) < 0)
    goto cleanup;

  value = args->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_SENSOR_TYPE_MASK;
  value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_SENSOR_TYPE_SHIFT;
  sensor_type = value;

  value = args->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_TYPE_MASK;
  value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_TYPE_SHIFT;
  event_type = value;
  
  value = args->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_DIRECTION_MASK;
  value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_DIRECTION_SHIFT;
  event_direction = value;
  
  value = args->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_MASK;
  value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_SHIFT;
  event_offset = value;  
  
  for (i = 0; i < IPMI_SYSTEM_GUID_LENGTH; i++)
    guid[i] = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_GUID_INDEX_START + i];
  
  localtimestamp = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START];
  localtimestamp <<= 8;
  localtimestamp |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START + 1];
  localtimestamp <<= 8;
  localtimestamp |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START + 2];
  localtimestamp <<= 8;
  localtimestamp |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START + 3];

  if (localtimestamp != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_UNSPECIFIED)
    {
      struct tm tm;
      time_t t;

      utcoffset = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_UTC_OFFSET_INDEX_START];
      utcoffset <<= 8;
      utcoffset |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_UTC_OFFSET_INDEX_START + 1];
      
      if (utcoffset != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_UTC_OFFSET_UNSPECIFIED)
	{
	  /* utcoffset is in minutes, multiply by 60 to get seconds */
	  localtimestamp += utcoffset * 60;
	}
      
      /* Posix says individual calls need not clear/set all portions of
       * 'struct tm', thus passing 'struct tm' between functions could
       * have issues.  So we need to memset.
       */
      memset (&tm, '\0', sizeof(struct tm));
      
      /* In FRU, epoch is 0:00 hrs 1/1/98
       *
       * So convert into ansi epoch
       */
      
      tm.tm_year = 98;          /* years since 1900 */
      tm.tm_mon = 0;            /* months since January */
      tm.tm_mday = 1;           /* 1-31 */
      tm.tm_hour = 0;
      tm.tm_min = 0;
      tm.tm_sec = 0;
      tm.tm_isdst = -1;
      
      if ((t = mktime (&tm)) == (time_t)-1)
	{
	  fprintf (stderr, "Invalid timestamp indicated\n");
	  goto cleanup;
	}
      
      localtimestamp += (uint32_t)t;
    }

  event_severity = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_INDEX];

  sensor_device = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SENSOR_DEVICE_INDEX];

  sensor_number = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SENSOR_NUMBER_INDEX];

  entity = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_ENTITY_INDEX];

  entity_instance = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_ENTITY_INSTANCE_INDEX];

  for (i = 0; i < IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_DATA_LENGTH; i++)
    event_data[i] = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_DATA_INDEX_START + i];

  manufacturer_id = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START];
  manufacturer_id <<= 8;
  manufacturer_id |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START + 1];
  manufacturer_id <<= 8;
  manufacturer_id |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START + 2];
  manufacturer_id <<= 8;
  manufacturer_id |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START + 3];
  
  system_id = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SYSTEM_ID_INDEX_START];
  system_id <<= 8;
  system_id |= args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SYSTEM_ID_INDEX_START + 1];

  for (i = 0; i < IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_OEM_CUSTOM_FIELDS_LENGTH; i++)
    oem_custom[i] = args->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_OEM_CUSTOM_FIELDS_INDEX_START + i];

  /* To get very consistent output to ipmi-sel, we will actually stuff
   * the above data into a SEL system event, and use that for
   * outputting information.
   */

  if (!(sel_system_event_record = fiid_obj_create (tmpl_sel_system_event_record)))
    {
      fprintf (stderr,
	       "fiid_obj_create: %s\n",
	       strerror (errno));
      goto cleanup;
    }

  /* Don't care about this field, just set 0 */
  if (fiid_obj_set (sel_system_event_record,
                    "record_id",
                    0) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'record_id': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "record_type",
                    IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'record_type': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "timestamp",
                    localtimestamp) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'timestamp': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  /* Need just the high order bit here */
  if (fiid_obj_set (sel_system_event_record,
                    "generator_id.id_type",
                    (sensor_device >> 7)) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'generator_id.id_type': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "generator_id.id",
                    sensor_device) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'generator_id.id': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  /* Don't care about this field, just set 0 */
  if (fiid_obj_set (sel_system_event_record,
                    "ipmb_device_lun",
                    0) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'ipmb_device_lun': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  /* Don't care about this field, just set 0 */
  if (fiid_obj_set (sel_system_event_record,
                    "reserved",
                    0) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'reserved': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  /* Don't care about this field, just set 0 */
  if (fiid_obj_set (sel_system_event_record,
                    "channel_number",
                    0) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'channel_number': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  /* Don't care about this field, just set 0 */
  if (fiid_obj_set (sel_system_event_record,
                    "channel_number",
                    0) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'channel_number': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_message_format_version",
                    IPMI_V1_5_EVENT_MESSAGE_FORMAT) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_message_format_version': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "sensor_type",
                    sensor_type) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'sensor_type': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "sensor_number",
                    sensor_number) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'sensor_number': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_type_code",
                    event_type) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_type_code': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_dir",
                    event_direction) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_dir': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_data1",
                    event_data[0]) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_data1': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_data2",
                    event_data[1]) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_data2': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_data3",
                    event_data[2]) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_data3': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if ((sel_record_len = fiid_obj_get_all (sel_system_event_record,
					  sel_record,
					  IPMI_SEL_RECORD_MAX_RECORD_LENGTH)) < 0)
    {
      fprintf (stderr,
               "fiid_obj_get_all: %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }
  
  if (sel_record_len != IPMI_SEL_RECORD_MAX_RECORD_LENGTH)
    {
      fprintf (stderr,
               "Invalid length SEL record: %u\n",
	       sel_record_len);
      goto cleanup;
    }

  /* Double check */
  if (event_offset != IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_UNSPECIFIED)
    {
      if (!(sel_system_event_record_event_fields = fiid_obj_create (tmpl_sel_system_event_record_event_fields)))
	{
	  fprintf (stderr,
		   "fiid_obj_create: %s\n",
		   strerror (errno));
	  goto cleanup;
	}
  
      if ((sel_record_len = fiid_obj_set_all (sel_system_event_record_event_fields,
					      sel_record,
					      IPMI_SEL_RECORD_MAX_RECORD_LENGTH)) < 0)
	{
	  fprintf (stderr,
		   "fiid_obj_set_all: %s\n",
		   fiid_obj_errormsg (sel_system_event_record));
	  goto cleanup;
	}
      
      if (FIID_OBJ_GET (sel_system_event_record_event_fields,
			"offset_from_event_reading_type_code",
			&val) < 0)
	{
	  fprintf (stderr,
		   "fiid_obj_get: 'offset_from_event_reading_type_code': %s\n",
		   fiid_obj_errormsg (sel_system_event_record_event_fields));
	  goto cleanup;
	}
      event_offset_test = val;
      
      /* The event offset specified in the specific trap does not match the event_data1 data
       *
       * Not much I can really do, one of them is valid and one isn't.
       * For now, just document bug.
       */
      if (event_offset != event_offset_test)
	{
	  if (state_data->prog_data->args->common.debug)
	    fprintf (stderr,
		     "Invalid PET data input: event_offset and event_data1 inconsistent\n");
	}
    }

  flags = IPMI_SEL_PARSE_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD;
  flags |= IPMI_SEL_PARSE_STRING_FLAGS_OUTPUT_NOT_AVAILABLE;
  flags |= IPMI_SEL_PARSE_STRING_FLAGS_DATE_MONTH_STRING;
  if (state_data->prog_data->args->verbose_count >= 2)
    flags |= IPMI_SEL_PARSE_STRING_FLAGS_VERBOSE;
  if (state_data->prog_data->args->non_abbreviated_units)
    flags |= IPMI_SEL_PARSE_STRING_FLAGS_NON_ABBREVIATED_UNITS;
  if (state_data->prog_data->args->interpret_oem_data)
    flags |= IPMI_SEL_PARSE_STRING_FLAGS_INTERPRET_OEM_DATA;
  
  if (localtimestamp != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_UNSPECIFIED)
    {
      if ((ret = _normal_output_date_and_time (state_data,
					       sel_record,
					       IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
					       flags)) < 0)
	goto cleanup;
    }
  else
    {
      if ((ret = _normal_output_not_available_date_and_time (state_data)) < 0)
	goto cleanup;
    }

  if (!ret)
    goto newline_out;
  
  if ((ret = _normal_output_sensor_name_and_type (state_data,
						  sel_record,
						  IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
						  flags)) < 0)
    goto cleanup;
  
  if (!ret)
    goto newline_out;

  if (state_data->prog_data->args->output_event_severity)
    {
      if ((ret = _normal_output_event_severity (state_data,
						event_severity,
						flags)) < 0)
	goto cleanup;
      
      if (!ret)
	goto newline_out;
    }

  if (state_data->prog_data->args->output_event_state)
    {
      if ((ret = _normal_output_event_state (state_data,
					     sel_record,
					     IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
					     flags)) < 0)
	goto cleanup;
      
      if (!ret)
	goto newline_out;
    }
  
  if (state_data->prog_data->args->verbose_count >= 1)
    {
      if ((ret = _normal_output_event_direction (state_data,
						 sel_record,
						 IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
						 flags)) < 0)
	goto cleanup;
      
      if (!ret)
	goto newline_out;
    }
  
  if (event_offset != IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_UNSPECIFIED)
    {
      if ((ret = _normal_output_event (state_data,
				       sel_record,
				       IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
				       flags)) < 0)
	goto cleanup;
    }
  else
    {
      if ((ret = _normal_output_not_available_event (state_data,
						     flags)) < 0)
	goto cleanup;
    }
  
  if (!ret)
    goto newline_out;
  
 newline_out:
  printf ("\n");
  rv = 0;
 cleanup:
  fiid_obj_destroy (sel_system_event_record);
  fiid_obj_destroy (sel_system_event_record_event_fields);
  return (rv);
}

static int
_flush_cache (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);
  
  if (sdr_cache_flush_cache (state_data->sdr_cache_ctx,
                             NULL,
                             state_data->prog_data->args->sdr.quiet_cache,
                             state_data->hostname,
                             state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return (-1);
  
  return (0);
}

static int
run_cmd_args (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  FILE *infile = NULL;
  int rv = -1;

  assert (state_data);
  
  args = state_data->prog_data->args;
  
  if (args->sdr.flush_cache)
    return (_flush_cache (state_data));
  
  if (args->variable_bindings_length)
    {
      if (_ipmi_pet_cmdline (state_data) < 0)
        goto cleanup;

      return (0);
    }

  if (args->cmd_file)
    {
      if (!(infile = fopen (args->cmd_file, "r")))
        {
          perror ("fopen()");
          goto cleanup;
        }
    }
  else
    infile = stdin;

#if 0
  if (_ipmi_pet_stream (state_data, infile) < 0)
    goto cleanup;
#endif

  rv = 0;
 cleanup:
  if (infile && infile != stdin)
    fclose (infile);
  return (rv);
}

static int
_ipmi_pet (ipmi_pet_prog_data_t *prog_data)
{
  ipmi_pet_state_data_t state_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  memset (&state_data, '\0', sizeof (ipmi_pet_state_data_t));
  state_data.prog_data = prog_data;
  state_data.hostname = prog_data->args->common.hostname;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache
      && (!prog_data->args->sdr.ignore_sdr_cache
	  || (prog_data->args->interpret_oem_data
	      && !prog_data->args->manufacturer_id_set
	      && !prog_data->args->product_id_set)))
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                             prog_data->args->common.hostname,
                                             &(prog_data->args->common),
                                             errmsg,
                                             IPMI_OPEN_ERRMSGLEN)))
        {
          fprintf (stderr,
		   "%s\n",
		   errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (!(state_data.sdr_cache_ctx = ipmi_sdr_cache_ctx_create ()))
    {
      perror ("ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        fprintf (stderr,
		 "ipmi_sdr_cache_ctx_set_flags: %s\n",
		 ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));

      if (prog_data->args->common.hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix (state_data.sdr_cache_ctx,
                                                   prog_data->args->common.hostname) < 0)
            fprintf (stderr,
		     "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
		     ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));
        }
    }

  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      perror ("ipmi_sdr_parse_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  /* Special case, just flush, don't do SEL stuff */
  if (!prog_data->args->sdr.flush_cache)
    {
      if (!(state_data.sel_parse_ctx = ipmi_sel_parse_ctx_create (NULL,
                                                                  prog_data->args->sdr.ignore_sdr_cache ? NULL : state_data.sdr_cache_ctx)))
        {
          perror ("ipmi_sel_parse_ctx_create()");
          goto cleanup;
        }
      
      if (state_data.prog_data->args->common.debug
	  && prog_data->args->common.hostname)
        {
          if (ipmi_sel_parse_ctx_set_debug_prefix (state_data.sel_parse_ctx,
                                                   prog_data->args->common.hostname) < 0)
            fprintf (stderr,
		     "ipmi_sel_parse_ctx_set_debug_prefix: %s\n",
		     ipmi_sel_parse_ctx_errormsg (state_data.sel_parse_ctx));
        }
    }

  if (prog_data->args->output_event_state)
    {
      unsigned int flags = 0;

      if (!(state_data.interpret_ctx = ipmi_interpret_ctx_create ()))
        {
          perror ("ipmi_interpret_ctx_create()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (prog_data->args->event_state_config_file)
        {
          if (ipmi_interpret_load_sel_config (state_data.interpret_ctx,
                                              prog_data->args->event_state_config_file) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST)
                fprintf (stderr,
			 "event state config file '%s' does not exist\n",
			 prog_data->args->event_state_config_file);
              else if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
                fprintf (stderr,
			 "event state config file '%s' parse error\n",
			 prog_data->args->event_state_config_file);
              else
                fprintf (stderr,
			 "ipmi_interpret_load_sel_config: %s\n",
			 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_interpret_load_sel_config (state_data.interpret_ctx, NULL) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
                fprintf (stderr, "event state config file parse error\n");
              else
                fprintf (stderr,
			 "ipmi_interpret_load_sel_config: %s\n",
			 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }

      if (prog_data->args->interpret_oem_data)
        flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;

      if (flags)
        {
          if (ipmi_interpret_ctx_set_flags (state_data.interpret_ctx, flags) < 0)
            {
              fprintf (stderr,
		       "ipmi_interpret_ctx_set_flags: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }
  
  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy (state_data.sdr_cache_ctx);
  if (state_data.sdr_parse_ctx)
    ipmi_sdr_parse_ctx_destroy (state_data.sdr_parse_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_pet_prog_data_t prog_data;
  struct ipmi_pet_arguments cmd_args;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_pet_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_pet_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  return (_ipmi_pet (&prog_data));
}

