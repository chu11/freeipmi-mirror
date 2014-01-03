/*
 * Copyright (C) 2011-2014 FreeIPMI Core Team
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
#include "tool-event-common.h"
#include "tool-hostrange-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"
#include "tool-util-common.h"

#define IPMI_PET_GUID_HEADER            "GUID"
#define IPMI_PET_MANUFACTURER_ID_HEADER "Manufacturer ID"
#define IPMI_PET_SYSTEM_ID_HEADER       "System ID"
#define IPMI_PET_EVENT_SEVERITY_HEADER  "Severity"

struct ipmi_pet_input
{
  uint32_t specific_trap;
  int specific_trap_na_specified;
  uint8_t variable_bindings[IPMI_PLATFORM_EVENT_TRAP_MAX_VARIABLE_BINDINGS_LENGTH];
  unsigned int variable_bindings_length;
};

struct ipmi_pet_trap_data
{
  uint8_t sensor_type;
  int sensor_type_cant_be_determined;
  uint8_t event_type;
  int event_type_cant_be_determined;
  uint8_t event_direction;
  uint8_t event_offset;
  uint8_t guid[IPMI_SYSTEM_GUID_LENGTH];
  uint16_t sequence_number;
  uint32_t localtimestamp_raw;
  uint32_t localtimestamp;
  int16_t utcoffset;
  uint8_t event_source_type;
  uint8_t event_severity;
  uint8_t sensor_device;
  uint8_t sensor_number;
  uint8_t entity;
  uint8_t entity_instance;
  uint8_t event_data[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_DATA_LENGTH];
  uint8_t language_code;
  uint32_t manufacturer_id;
  uint16_t system_id;
  uint8_t oem_custom[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_OEM_CUSTOM_FIELDS_LENGTH];
  unsigned int oem_custom_length;
};

static int
_ipmi_pet_init (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  int rv = -1;
 
  assert (state_data);

  args = state_data->prog_data->args;

  if (!args->common_args.ignore_sdr_cache)
    {
      if (calculate_column_widths (NULL,
				   state_data->sdr_ctx,
				   NULL,
				   0,
				   NULL,
				   0,
				   state_data->prog_data->args->non_abbreviated_units,
				   (args->entity_sensor_names) ? 1 : 0, /* shared_sensors */
				   1, /* count_event_only_records */
				   0, /* count_device_locator_records */
				   0, /* count_oem_records */
				   args->entity_sensor_names,
				   &(state_data->column_width)) < 0)
	goto cleanup;
    }
  else
    {
      if (calculate_column_widths_ignored_sdr_cache (state_data->prog_data->args->non_abbreviated_units,
						     &(state_data->column_width)) < 0)
	goto cleanup;
    }

  if (args->interpret_oem_data
      && !args->common_args.ignore_sdr_cache)
    {
      if (ipmi_get_oem_data (NULL,
			     state_data->ipmi_ctx,
			     &state_data->oem_data) < 0)
	goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_pet_oem_setup (ipmi_pet_state_data_t *state_data, struct ipmi_pet_trap_data *data)
{
  struct ipmi_pet_arguments *args;
  int rv = -1;
 
  assert (state_data);
  assert (data);

  args = state_data->prog_data->args;

  if (args->interpret_oem_data)
    {
      uint32_t manufacturer_id;
      uint16_t product_id;

      /* Three ways to get manufacturer-id/product-id (in order of preference).
       *
       * 1) User input - takes highest priority
       *
       * 2) Trap data - takes priority over IPMI connection because
       * maybe running on alternate machine.  But only use/assume if
       * manufacturer_id/product_id looks ok.
       *
       * 3) IPMI connection
       */

      if (args->manufacturer_id_set
	  && args->product_id_set)
	{
	  manufacturer_id = args->manufacturer_id;
	  product_id = args->product_id;
	}
      else
	{
	  /* achu: I assume vendors that don't support will likely
	   * fill in manufacturer_id with something bogus
	   */
	  if (IPMI_IANA_ENTERPRISE_ID_RECOGNIZED (data->manufacturer_id))
	    {
	      manufacturer_id = data->manufacturer_id;
	      product_id = data->system_id;
	    }
	  else if (!args->common_args.ignore_sdr_cache)
	    {
	      manufacturer_id = state_data->oem_data.manufacturer_id;
	      product_id = state_data->oem_data.product_id;
	    }
	  else
	    {
	      /* Eventually will lead to output of number for
	       * manufacturer id instead of string
	       */
	      manufacturer_id = data->manufacturer_id;
	      product_id = data->system_id;
	    }
	}

      if (ipmi_sel_ctx_set_manufacturer_id (state_data->sel_ctx,
					    manufacturer_id) < 0)
	{
	  fprintf (stderr,
		   "ipmi_sel_ctx_set_manufacturer_id: %s\n",
		   ipmi_sel_ctx_errormsg (state_data->sel_ctx));
	  goto cleanup;
	}
      
      if (ipmi_sel_ctx_set_product_id (state_data->sel_ctx,
				       product_id) < 0)
	{
	  fprintf (stderr,
		   "ipmi_sel_ctx_set_product_id: %s\n",
		   ipmi_sel_ctx_errormsg (state_data->sel_ctx));
	  goto cleanup;
	}
      
      if (args->output_event_state)
	{
	  if (ipmi_interpret_ctx_set_manufacturer_id (state_data->interpret_ctx,
						      manufacturer_id) < 0)
	    {
	      fprintf (stderr,
		       "ipmi_interpret_ctx_set_manufacturer_id: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
	      goto cleanup;
	    }
	  
	  if (ipmi_interpret_ctx_set_product_id (state_data->interpret_ctx,
						 product_id) < 0)
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
_ipmi_pet_parse_trap_data (ipmi_pet_state_data_t *state_data,
			   struct ipmi_pet_input *input,
			   struct ipmi_pet_trap_data *data)
{
  struct ipmi_pet_arguments *args;
  int rv = -1;
  int i;

  assert (state_data);
  assert (input);
  assert (data);

  args = state_data->prog_data->args;
  
  if (!input->specific_trap_na_specified)
    {
      uint32_t value;

      value = input->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_SENSOR_TYPE_MASK;
      value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_SENSOR_TYPE_SHIFT;
      data->sensor_type = value;
      
      value = input->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_TYPE_MASK;
      value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_TYPE_SHIFT;
      data->event_type = value;
  
      value = input->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_DIRECTION_MASK;
      value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_DIRECTION_SHIFT;
      data->event_direction = value;
      
      value = input->specific_trap & IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_MASK;
      value >>= IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_SHIFT;
      data->event_offset = value;  
    }

  for (i = 0; i < IPMI_SYSTEM_GUID_LENGTH; i++)
    data->guid[i] = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_GUID_INDEX_START + i];
  
  data->sequence_number = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SEQUENCE_NUMBER_INDEX_START];
  data->sequence_number <<= 8;
  data->sequence_number |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SEQUENCE_NUMBER_INDEX_START + 1];

  data->localtimestamp_raw = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START];
  data->localtimestamp_raw <<= 8;
  data->localtimestamp_raw |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START + 1];
  data->localtimestamp_raw <<= 8;
  data->localtimestamp_raw |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START + 2];
  data->localtimestamp_raw <<= 8;
  data->localtimestamp_raw |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_INDEX_START + 3];

  data->localtimestamp = data->localtimestamp_raw;

  if (data->localtimestamp != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_UNSPECIFIED)
    {
      struct tm tm;
      time_t t;

      data->utcoffset = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_UTC_OFFSET_INDEX_START];
      data->utcoffset <<= 8;
      data->utcoffset |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_UTC_OFFSET_INDEX_START + 1];
      
      /* utcoffset signed & unspecified 0xffff, cast to remove warnings */
      if ((uint16_t)data->utcoffset != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_UTC_OFFSET_UNSPECIFIED)
	{
	  /* utcoffset is in minutes, multiply by 60 to get seconds */
	  data->localtimestamp += data->utcoffset * 60;
	}
      
      /* Posix says individual calls need not clear/set all portions of
       * 'struct tm', thus passing 'struct tm' between functions could
       * have issues.  So we need to memset.
       */
      memset (&tm, '\0', sizeof(struct tm));
      
      /* In PET, epoch is 0:00 hrs 1/1/98
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
      
      data->localtimestamp += (uint32_t)t;
    }

  data->event_source_type = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SOURCE_TYPE_INDEX];

  data->event_severity = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_INDEX];

  data->sensor_device = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SENSOR_DEVICE_INDEX];

  data->sensor_number = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SENSOR_NUMBER_INDEX];

  data->entity = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_ENTITY_INDEX];

  data->entity_instance = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_ENTITY_INSTANCE_INDEX];

  for (i = 0; i < IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_DATA_LENGTH; i++)
    data->event_data[i] = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_DATA_INDEX_START + i];

  data->language_code =  input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LANGUAGE_CODE_INDEX];

  data->manufacturer_id = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START];
  data->manufacturer_id <<= 8;
  data->manufacturer_id |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START + 1];
  data->manufacturer_id <<= 8;
  data->manufacturer_id |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START + 2];
  data->manufacturer_id <<= 8;
  data->manufacturer_id |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_MANUFACTURER_ID_INDEX_START + 3];
  
  data->system_id = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SYSTEM_ID_INDEX_START];
  data->system_id <<= 8;
  data->system_id |= input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_SYSTEM_ID_INDEX_START + 1];

  for (i = 0;
       (IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_OEM_CUSTOM_FIELDS_INDEX_START + i) < input->variable_bindings_length;
       i++)
    {
      data->oem_custom[i] = input->variable_bindings[IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_OEM_CUSTOM_FIELDS_INDEX_START + i];
      data->oem_custom_length++;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_pet_form_sel_record (ipmi_pet_state_data_t *state_data,
			   struct ipmi_pet_trap_data *data,
			   uint8_t *sel_record,
			   unsigned int sel_record_len)
{
  fiid_obj_t sel_system_event_record = NULL;
  int rv = -1;
  int len;

  assert (state_data);
  assert (data);
  assert (sel_record);
  assert (sel_record_len);

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
                    data->localtimestamp) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'timestamp': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  /* Need just the high order bit here */
  if (fiid_obj_set (sel_system_event_record,
                    "generator_id.id_type",
                    (data->sensor_device >> 7)) < 0)
    {
      fprintf (stderr,
	       "fiid_obj_set: 'generator_id.id_type': %s\n",
	       fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "generator_id.id",
                    data->sensor_device) < 0)
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
                    data->sensor_type_cant_be_determined ? IPMI_SENSOR_TYPE_RESERVED : data->sensor_type) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'sensor_type': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "sensor_number",
                    data->sensor_number) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'sensor_number': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_type_code",
                    data->event_type_cant_be_determined ? IPMI_EVENT_READING_TYPE_CODE_UNSPECIFIED : data->event_type) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_type_code': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_dir",
                    data->event_direction) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_dir': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_data1",
                    data->event_data[0]) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_data1': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_data2",
                    data->event_data[1]) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_data2': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if (fiid_obj_set (sel_system_event_record,
                    "event_data3",
                    data->event_data[2]) < 0)
    {
      fprintf (stderr,
               "fiid_obj_set: 'event_data3': %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }

  if ((len = fiid_obj_get_all (sel_system_event_record,
			       sel_record,
			       sel_record_len)) < 0)
    {
      fprintf (stderr,
               "fiid_obj_get_all: %s\n",
               fiid_obj_errormsg (sel_system_event_record));
      goto cleanup;
    }
  
  if (len != IPMI_SEL_RECORD_MAX_RECORD_LENGTH)
    {
      fprintf (stderr,
               "Invalid length SEL record: %u\n",
	       sel_record_len);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (sel_system_event_record);
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
	  
	  if (state_data->prog_data->args->verbose_count >= 2)
	    printf (",%s,%s,%s",
		    IPMI_PET_GUID_HEADER,
		    IPMI_PET_MANUFACTURER_ID_HEADER,
		    IPMI_PET_SYSTEM_ID_HEADER);

	  if (state_data->prog_data->args->output_event_severity
	      || state_data->prog_data->args->verbose_count >= 1)
	    printf (",%s", IPMI_PET_EVENT_SEVERITY_HEADER);

          if (state_data->prog_data->args->output_event_state)
            printf (",%s", SENSORS_HEADER_STATE_STR);
	  
          if (state_data->prog_data->args->verbose_count >= 1)
            printf (",Event Direction");
          
          printf (",Event\n");
        }
      else
        {          
	  char fmt[EVENT_FMT_BUFLEN+1];

          memset (fmt, '\0', EVENT_FMT_BUFLEN + 1);

          if (state_data->prog_data->args->no_sensor_type_output)
            {
              snprintf (fmt,
                        EVENT_FMT_BUFLEN,
                        "Date        | Time     | %%-%ds",
                        state_data->column_width.sensor_name);
              
              printf (fmt, SENSORS_HEADER_NAME_STR);
            }
          else
            {
              snprintf (fmt,
                        EVENT_FMT_BUFLEN,
                        "Date        | Time     | %%-%ds | %%-%ds",
                        state_data->column_width.sensor_name,
                        state_data->column_width.sensor_type);
              
              printf (fmt,
		      SENSORS_HEADER_NAME_STR,
		      SENSORS_HEADER_TYPE_STR);
            }

	  if (state_data->prog_data->args->verbose_count >= 2)
	    printf (" | %-36s | %-25s | %s",
		    IPMI_PET_GUID_HEADER,
		    IPMI_PET_MANUFACTURER_ID_HEADER,
		    IPMI_PET_SYSTEM_ID_HEADER);

	  if (state_data->prog_data->args->output_event_severity
	      || state_data->prog_data->args->verbose_count >= 1)
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

  if (ipmi_sel_ctx_errnum (state_data->sel_ctx) == IPMI_SEL_ERR_INVALID_SEL_ENTRY)
    {
      if (state_data->prog_data->args->common_args.debug)
        fprintf (stderr,
		 "Invalid PET data input\n");
      return (0);
    }

  fprintf (stderr,
	   "%s: %s\n",
	   func,
	   ipmi_sel_ctx_errormsg (state_data->sel_ctx));
  return (-1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_date (ipmi_pet_state_data_t *state_data,
	      uint8_t *sel_record,
	      unsigned int sel_record_len,
	      unsigned int flags)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int outbuf_len;

  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);

  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN+1);
  if ((outbuf_len = ipmi_sel_parse_read_record_string (state_data->sel_ctx,
						       "%d",
						       sel_record,
						       sel_record_len,
						       outbuf,
						       EVENT_OUTPUT_BUFLEN,
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
        printf ("%s", EVENT_NA_STRING);
    }
  else
    {
      if (outbuf_len)
        printf ("%-11s", outbuf);
      else
        printf ("%-11s", EVENT_NA_STRING);
    }

  return (1);
}


/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_not_available_date (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);

  if (state_data->prog_data->args->comma_separated_output)
    printf ("%s", EVENT_NA_STRING);
  else
    printf ("%-11s", EVENT_NA_STRING);

  return (1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_time (ipmi_pet_state_data_t *state_data,
	      uint8_t *sel_record,
	      unsigned int sel_record_len,
	      unsigned int flags)
{
  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);
  
  return (event_output_time (NULL,
			     state_data->sel_ctx,
			     sel_record,
			     sel_record_len,
			     state_data->prog_data->args->comma_separated_output,
			     state_data->prog_data->args->common_args.debug,
			     flags));
}


/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_not_available_time (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);

  return (event_output_not_available_time (NULL,
					   state_data->prog_data->args->comma_separated_output));
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_sensor_name (ipmi_pet_state_data_t *state_data,
		     uint8_t *sel_record,
		     unsigned int sel_record_len,
		     unsigned int flags)
{
  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);

  return (event_output_sensor_name (NULL,
				    state_data->sel_ctx,
				    sel_record,
				    sel_record_len,
				    &state_data->column_width,
				    &state_data->prog_data->args->common_args,
				    state_data->prog_data->args->comma_separated_output,
				    flags));
}
                        
/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_sensor_type (ipmi_pet_state_data_t *state_data,
		     uint8_t *sel_record,
		     unsigned int sel_record_len,
		     unsigned int flags)
{
  assert (state_data);
  assert (!state_data->prog_data->args->no_sensor_type_output);
  assert (sel_record);
  assert (sel_record_len);
  
  return (event_output_sensor_type (NULL,
				    state_data->sel_ctx,
				    sel_record,
                                    sel_record_len,
                                    &state_data->column_width,
                                    state_data->prog_data->args->comma_separated_output,
                                    state_data->prog_data->args->common_args.debug,
                                    flags));
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_not_available_sensor_type (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->no_sensor_type_output);

  return (event_output_not_available_sensor_type (NULL,
						  &state_data->column_width,
						  state_data->prog_data->args->comma_separated_output));
}


/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_guid_manufacturer_id_system_id (ipmi_pet_state_data_t *state_data,
					struct ipmi_pet_trap_data *data)
{
  char fmt[EVENT_FMT_BUFLEN + 1];
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  int iana_available;

  assert (state_data);
  assert (data);
  assert (state_data->prog_data->args->verbose_count >= 2);
  
  /* Output format for guid from "Wired for Management Specification",
   * Appendex 1 "String Representation of UUIDs" in the above
   * document.  Note that the output is supposed to be output in most
   * significant byte order.
   */

  memset (fmt, '\0', EVENT_FMT_BUFLEN + 1);
  if (state_data->prog_data->args->comma_separated_output)
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              ",%%02x%%02x%%02x%%02x-%%02x%%02x-%%02x%%02x-%%02x%%02x-%%02x%%02x%%02x%%02x%%02x%%02x");
  else
    snprintf (fmt,
              EVENT_FMT_BUFLEN,
              " | %%02x%%02x%%02x%%02x-%%02x%%02x-%%02x%%02x-%%02x%%02x-%%02x%%02x%%02x%%02x%%02x%%02x");
  
  printf (fmt,
	  data->guid[0],  /* time low */
	  data->guid[1],
	  data->guid[2],
	  data->guid[3],
	  data->guid[4],  /* time mid */
	  data->guid[5],
	  data->guid[6],   /* time high and version */
	  data->guid[7],
	  data->guid[8],   /* clock seq high and reserved - comes before clock seq low */
	  data->guid[9],   /* clock seq low */
	  data->guid[10],   /* node */
	  data->guid[11],
	  data->guid[12],
	  data->guid[13],
	  data->guid[14],
	  data->guid[15]);
  
  /* if iana_available == 0 means no string, < 0 means bad manufacturer id
   * either way, output just the number
   */

  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN + 1);
  iana_available = ipmi_iana_enterprise_numbers_string (data->manufacturer_id,
							outbuf,
							EVENT_OUTPUT_BUFLEN);

  memset (fmt, '\0', EVENT_FMT_BUFLEN + 1);

  if (iana_available > 0)
    {
      if (state_data->prog_data->args->comma_separated_output)
	snprintf (fmt,
		  EVENT_FMT_BUFLEN,
		  ",%%s,%%u");
      else
	snprintf (fmt,
		  EVENT_FMT_BUFLEN,
		  " | %%-25s | %%-9u");

      printf (fmt, outbuf, data->system_id);
    }
  else
    {
      if (state_data->prog_data->args->comma_separated_output)
	snprintf (fmt,
		  EVENT_FMT_BUFLEN,
		  ",%%u,%%u");
      else
	snprintf (fmt,
		  EVENT_FMT_BUFLEN,
		  " | %%-25u | %%-9u");

      printf (fmt, data->manufacturer_id, data->system_id);
    }

  return (1);
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_event_severity (ipmi_pet_state_data_t *state_data,
			uint8_t event_severity,
			unsigned int flags)
{
  char *str = NULL;

  assert (state_data);
  assert (state_data->prog_data->args->output_event_severity
	  || state_data->prog_data->args->verbose_count >= 1);

  switch (event_severity)
    {
    case IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_EVENT_SEVERITY_UNSPECIFIED:
      str = EVENT_NA_STRING;
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
_output_event_state (ipmi_pet_state_data_t *state_data,
		     uint8_t *sel_record,
		     unsigned int sel_record_len,
		     unsigned int flags)
{
  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);
  assert (state_data->prog_data->args->output_event_state);

  return (event_output_event_state (NULL,
				    state_data->sel_ctx,
				    sel_record,
				    sel_record_len,
				    state_data->prog_data->args->comma_separated_output,
				    state_data->prog_data->args->common_args.debug,
				    flags));
}
                  
/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_event_direction (ipmi_pet_state_data_t *state_data,
			 uint8_t *sel_record,
			 unsigned int sel_record_len,
			 unsigned int flags)
{
  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);
  assert (state_data->prog_data->args->verbose_count >= 1);

  return (event_output_event_direction (NULL,
					state_data->sel_ctx,
					sel_record,
					sel_record_len,
					state_data->prog_data->args->comma_separated_output,
					state_data->prog_data->args->common_args.debug,
					flags));
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_not_available_event_direction (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->verbose_count >= 1);

  return (event_output_not_available_event_direction (NULL,
						      state_data->prog_data->args->comma_separated_output));
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_event (ipmi_pet_state_data_t *state_data,
	       uint8_t *sel_record,
	       unsigned int sel_record_len,
	       unsigned int flags,
	       struct ipmi_pet_trap_data *data)
{
  assert (state_data);
  assert (sel_record);
  assert (sel_record_len);
  assert (data);

  return (event_output_event (NULL,
			      state_data->sel_ctx,
			      sel_record,
			      sel_record_len,
			      state_data->prog_data->args->comma_separated_output,
			      state_data->prog_data->args->common_args.debug,
			      flags));
}
                                       
/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_not_available_event (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);
  
  return (event_output_not_available_event (NULL,
					    state_data->prog_data->args->comma_separated_output));
}

/* return 1 on success
 * return (0) on non-success, but don't fail
 * return (-1) on error
 */
static int
_output_oem_custom (ipmi_pet_state_data_t *state_data,
		    struct ipmi_pet_trap_data *data)
{
  char outbuf[EVENT_OUTPUT_BUFLEN+1];
  unsigned int outbuflen = 0;
  unsigned int index = 0;
  int rv = -1;

  assert (state_data);
  assert (data);
  assert (state_data->prog_data->args->verbose_count >= 1);

  memset (outbuf, '\0', EVENT_OUTPUT_BUFLEN + 1);

  while (index < data->oem_custom_length
         && data->oem_custom[index] != IPMI_FRU_SENTINEL_VALUE)
    {
      char tmpbuf[EVENT_OUTPUT_BUFLEN+1];
      unsigned int tmpbuflen = EVENT_OUTPUT_BUFLEN;
      uint8_t type_length;
      uint8_t type_code;
      uint8_t number_of_data_bytes;

      memset (tmpbuf, '\0', EVENT_OUTPUT_BUFLEN + 1);

      type_length = data->oem_custom[index];
      type_code = (type_length & IPMI_FRU_TYPE_LENGTH_TYPE_CODE_MASK) >> IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SHIFT;
      number_of_data_bytes = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

      /* Special Case: This shouldn't be a length of 0x01 (see type/length
       * byte format in FRU Information Storage Definition).
       */
      if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_LANGUAGE_CODE
	  && number_of_data_bytes == 0x01)
	{
	  if (state_data->prog_data->args->common_args.debug)
	    fprintf (stderr,
		     "Invalid OEM custom data: length invalid\n");
	  break;
	}

      if ((index + 1 + number_of_data_bytes) > data->oem_custom_length)
	{
	  if (state_data->prog_data->args->common_args.debug)
	    fprintf (stderr,
		     "Invalid OEM custom data: length exceeds input\n");
	  break;
	}

      if (ipmi_fru_type_length_field_to_string (state_data->fru_ctx,
						      &data->oem_custom[index],
						      number_of_data_bytes + 1,
						      data->language_code,
						      tmpbuf,
						      &tmpbuflen) < 0)
	{
	  if (ipmi_fru_ctx_errnum (state_data->fru_ctx) == IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT
	      || ipmi_fru_ctx_errnum (state_data->fru_ctx) == IPMI_FRU_ERR_FRU_LANGUAGE_CODE_NOT_SUPPORTED
	      || ipmi_fru_ctx_errnum (state_data->fru_ctx) == IPMI_FRU_ERR_FRU_INVALID_BCD_ENCODING)
	    {
	      if (state_data->prog_data->args->common_args.debug)
		fprintf (stderr,
			 "Invalid OEM Custom Field: %s\n",
			 ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	      break;
	    }
	  
	  fprintf (stderr,
		   "ipmi_fru_type_length_field_to_string: %s\n",
		   ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	  goto cleanup;
	}

      if (tmpbuflen)
	{
	  if (outbuflen)
	    {
	      if ((outbuflen + 3) > EVENT_OUTPUT_BUFLEN)
		{
		  if (state_data->prog_data->args->common_args.debug)
		    fprintf (stderr, "OEM Custom Overflow\n");
		  break;
		}
	      
	      strcat (outbuf, " ; ");
	      outbuflen += 3;
	    }

	  if ((outbuflen + tmpbuflen) > EVENT_OUTPUT_BUFLEN)
	    {
	      if (state_data->prog_data->args->common_args.debug)
		fprintf (stderr, "OEM Custom Overflow\n");
	      break;
	    }
	  
	  strcat (outbuf, tmpbuf);
	  outbuflen += tmpbuflen;
	}
      
      index += 1;          /* type/length byte */
      index += number_of_data_bytes;
    }

  if (outbuflen)
    printf (" ; %s", outbuf);

  rv = 1;
 cleanup:
  return (rv);
}

static int
_ipmi_pet_process (ipmi_pet_state_data_t *state_data,
		   struct ipmi_pet_input *input)
{
  struct ipmi_pet_arguments *args;
  struct ipmi_pet_trap_data data;
  fiid_obj_t sel_system_event_record_event_fields = NULL;
  uint8_t event_offset_test;
  uint8_t sel_record[IPMI_SEL_RECORD_MAX_RECORD_LENGTH];
  unsigned int flags = 0;
  uint64_t val;
  int sel_record_len;
  int ret;
  int rv = -1;

  assert (state_data);
  assert (input);
  
  memset (&data, '\0', sizeof (struct ipmi_pet_trap_data));

  args = state_data->prog_data->args;

  if (_ipmi_pet_parse_trap_data (state_data, input, &data) < 0)
    goto cleanup;

  /* call after parse trap data */
  if (_ipmi_pet_oem_setup (state_data, &data) < 0)
    goto cleanup;

  if (input->specific_trap_na_specified)
    {
      if (!args->common_args.ignore_sdr_cache)
	{
	  uint8_t record_type;
	  
	  if (ipmi_sdr_cache_search_sensor_wrapper (state_data->sdr_ctx,
						    data.sensor_number,
						    data.sensor_device) < 0)
	    {
	      if (ipmi_sdr_ctx_errnum (state_data->sdr_ctx) != IPMI_SDR_ERR_NOT_FOUND)
		{
		  if (state_data->prog_data->args->common_args.debug)
		    fprintf (stderr,
			     "ipmi_sdr_cache_search_record_id: %s\n",
			     ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
		  goto cleanup;
		}
	      else
		goto cant_be_determined;
	    }
	  
	  if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
                                                 NULL,
                                                 0,
                                                 NULL,
                                                 &record_type) < 0)
            {
              fprintf (stderr,
		       "ipmi_sdr_parse_record_id_and_type: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
              goto cleanup;
            }
	  
	  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
	      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
	      && record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
	    goto cant_be_determined;

	  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
					  NULL,
					  0,
					  &data.sensor_type) < 0)
	    {
	      fprintf (stderr,
		       "ipmi_sdr_parse_sensor_type: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	      goto cleanup;
	    }
	  
	  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
						      NULL,
						      0,
						      &data.event_type) < 0)
	    {
	      fprintf (stderr,
		       "ipmi_sdr_parse_event_reading_type_code: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	      goto cleanup;
	    }
	}
      else
	{
	cant_be_determined:
	  /* Can't determine this stuff */
	  data.event_type_cant_be_determined = 1;
	  data.sensor_type_cant_be_determined = 1;
	}
    }

  /* To get very consistent output to ipmi-sel, we will actually stuff
   * the above data into a SEL system event, and use that for
   * outputting information.
   */

  if (_ipmi_pet_form_sel_record (state_data,
				 &data,
				 sel_record,
				 IPMI_SEL_RECORD_MAX_RECORD_LENGTH) < 0)
    goto cleanup;

  if (input->specific_trap_na_specified
      || data.event_offset != IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_UNSPECIFIED)
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
		   fiid_obj_errormsg (sel_system_event_record_event_fields));
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

      /* determine event_offset from event data1 */
      if (input->specific_trap_na_specified)
	data.event_offset = event_offset_test;
      else
	{
	  /* If the event offset specified in the specific trap does not
	   * match the event_data1 data, not much I can really do, one of them is valid and one isn't.
	   * For now, just document bug.
	   */
	  if (data.event_offset != event_offset_test)
	    {
	      if (state_data->prog_data->args->common_args.debug)
		fprintf (stderr,
			 "Invalid PET data input: event_offset and event_data1 inconsistent\n");
	    }
	}
    }

  if (data.entity != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_ENTITY_UNSPECIFIED
      && !args->common_args.ignore_sdr_cache)
    {
      uint8_t entity_id, entity_instance;
      
      if (ipmi_sdr_cache_search_sensor (state_data->sdr_ctx,
					data.sensor_number,
					data.sensor_device) < 0)
	{
	  if (ipmi_sdr_ctx_errnum (state_data->sdr_ctx) != IPMI_SDR_ERR_NOT_FOUND)
	    {
	      fprintf (stderr,
		       "ipmi_sdr_cache_search_record_id: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	      goto cleanup;
	    }
	  else
	    goto cant_do_entity_id_check;
	}

      if (ipmi_sdr_parse_entity_id_instance_type (state_data->sdr_ctx,
						  NULL,
						  0,
						  &entity_id,
						  &entity_instance,
						  NULL) < 0)
	{
	  fprintf (stderr,
		   "ipmi_sdr_parse_entity_id_instance_type: %s\n",
		   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  goto cleanup;
	}

      if (entity_id != data.entity)
	{
	  if (state_data->prog_data->args->common_args.debug)
	    fprintf (stderr,
		     "Invalid PET data input: entity id inconsistent to SDR data\n");
	}

      if (data.entity_instance != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_ENTITY_INSTANCE_UNSPECIFIED)
	{
	  if (entity_instance != data.entity_instance)
	    {
	      if (state_data->prog_data->args->common_args.debug)
		fprintf (stderr,
			 "Invalid PET data input: entity instance inconsistent to SDR data\n");
	    }
	}
    }

 cant_do_entity_id_check:

  flags = IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD;
  flags |= IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE;
  flags |= IPMI_SEL_STRING_FLAGS_DATE_MONTH_STRING;
  if (state_data->prog_data->args->verbose_count >= 3)
    flags |= IPMI_SEL_STRING_FLAGS_VERBOSE;
  if (state_data->prog_data->args->entity_sensor_names)
    flags |= IPMI_SEL_STRING_FLAGS_ENTITY_SENSOR_NAMES;
  if (state_data->prog_data->args->non_abbreviated_units)
    flags |= IPMI_SEL_STRING_FLAGS_NON_ABBREVIATED_UNITS;
  if (state_data->prog_data->args->interpret_oem_data)
    flags |= IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA;
  
  if (data.localtimestamp != IPMI_PLATFORM_EVENT_TRAP_VARIABLE_BINDINGS_LOCAL_TIMESTAMP_UNSPECIFIED)
    {
      if ((ret = _output_date (state_data,
			       sel_record,
			       IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
			       flags)) < 0)
	goto cleanup;

      if (!ret)
	goto newline_out;

      if ((ret = _output_time (state_data,
			       sel_record,
			       IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
			       flags)) < 0)
	goto cleanup;

      if (!ret)
	goto newline_out;
    }
  else
    {
      if ((ret = _output_not_available_date (state_data)) < 0)
	goto cleanup;

      if (!ret)
	goto newline_out;

      if ((ret = _output_not_available_time (state_data)) < 0)
	goto cleanup;

      if (!ret)
	goto newline_out;
    }

  
  if ((ret = _output_sensor_name (state_data,
				  sel_record,
				  IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
				  flags)) < 0)
    goto cleanup;
  
  if (!ret)
    goto newline_out;

  if (!state_data->prog_data->args->no_sensor_type_output)
    {
      if (data.sensor_type_cant_be_determined)
	{
	  if ((ret = _output_not_available_sensor_type (state_data)) < 0)
	    goto cleanup;	  
	}
      else
	{
	  if ((ret = _output_sensor_type (state_data,
					  sel_record,
					  IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
					  flags)) < 0)
	    goto cleanup;
	}

      if (!ret)
	goto newline_out;
    }

  if (state_data->prog_data->args->verbose_count >= 2)
    {
      if ((ret = _output_guid_manufacturer_id_system_id (state_data, &data)) < 0)
	goto cleanup;

      if (!ret)
	goto newline_out;
    }

  if (state_data->prog_data->args->output_event_severity
      || state_data->prog_data->args->verbose_count >= 1)
    {
      if ((ret = _output_event_severity (state_data,
					 data.event_severity,
					 flags)) < 0)
	goto cleanup;
      
      if (!ret)
	goto newline_out;
    }
  
  if (state_data->prog_data->args->output_event_state)
    {
      if ((ret = _output_event_state (state_data,
				      sel_record,
				      IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
				      flags)) < 0)
	goto cleanup;

      if (!ret)
	goto newline_out;
    }
  
  if (state_data->prog_data->args->verbose_count >= 1)
    {
      if (input->specific_trap_na_specified
	  && args->common_args.ignore_sdr_cache)
	{
	  if ((ret = _output_not_available_event_direction (state_data)) < 0)
	    goto cleanup;
	}
      else
	{
	  if ((ret = _output_event_direction (state_data,
					      sel_record,
					      IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
					      flags)) < 0)
	    goto cleanup;
	}

      if (!ret)
	goto newline_out;
    }
  
  if (data.event_offset != IPMI_PLATFORM_EVENT_TRAP_SPECIFIC_TRAP_EVENT_OFFSET_UNSPECIFIED)
    {
      if (data.event_type_cant_be_determined)
	{
	  if ((ret = _output_not_available_event (state_data)) < 0)
	    goto cleanup;
	}
      else
	{
	  if ((ret = _output_event (state_data,
				    sel_record,
				    IPMI_SEL_RECORD_MAX_RECORD_LENGTH,
				    flags,
				    &data)) < 0)
	    goto cleanup;
	}
    }
  else
    {
      if ((ret = _output_not_available_event (state_data)) < 0)
	goto cleanup;
    }

  if (!ret)
    goto newline_out;

  if (state_data->prog_data->args->verbose_count >= 1)
    {
      if ((ret = _output_oem_custom (state_data, &data)) < 0)
	goto cleanup;

      if (!ret)
	goto newline_out;
    }

 newline_out:
  printf ("\n");
  rv = 0;
 cleanup:
  fiid_obj_destroy (sel_system_event_record_event_fields);
  return (rv);
}

/* returns -1 on fatal error, 0 on non-fatal error, 1 on success */
static int
_ipmi_pet_parse (ipmi_pet_state_data_t *state_data,
		 const char *line,
		 struct ipmi_pet_input *input,
		 unsigned int line_count)
{
  const char delim[] = " \t\f\v\r\n";
  char *str = NULL;
  char *ptr = NULL;
  char *token = NULL;
  int specific_trap_parsed = 0;
  int rv = -1;

  assert (state_data);
  assert (line);
  assert (input);
  assert (line_count);

  if (!(str = (char *) strdup (line)))
    {
      perror ("strdup");
      goto cleanup;
    }
  ptr = str;

  memset (input, '\0', sizeof (struct ipmi_pet_input));

  while (1)
    {
      unsigned int i;
      unsigned long uvalue;
      long value;
      char *endptr = NULL;

      token = strsep (&ptr, delim);
      if (!token)
        break;

      if (!strcmp (token, ""))
        continue;

      if (!specific_trap_parsed)
	{
	  if (!strcasecmp (token, "NA"))
	    {
	      input->specific_trap_na_specified = 1;
	      specific_trap_parsed++;
	      continue;
	    }
	  
	  errno = 0;
	  uvalue = strtoul (token, &endptr, 0);
	  if (errno
	      || endptr[0] != '\0')
	    {
	      fprintf (stderr, "invalid specific trap argument on line %u\n", line_count);
	      rv = 0;
	      goto cleanup;
	    }
	  
	  input->specific_trap = uvalue;
	  specific_trap_parsed++;
	  continue;
	}

      if (strlen (token) >= 2)
	{
	  if (!strncmp (token, "0x", 2))
	    token+=2;
	}

      if (*token == '\0')
	{
	  fprintf (stderr, "invalid variable binding hex byte argument on line %u\n", line_count);
	  rv = 0;
	  goto cleanup;
	}

      for (i = 0; token[i] != '\0'; i++)
	{
	  if (i >= 2)
	    {
	      fprintf (stderr, "invalid variable binding hex byte argument on line %u\n", line_count);
	      rv = 0;
	      goto cleanup;
	    }
            
	  if (!isxdigit (token[i]))
	    {
	      fprintf (stderr, "invalid variable binding hex byte argument on line %u\n", line_count);
	      rv = 0;
	      goto cleanup;
	    }
	}

      if (input->variable_bindings_length < IPMI_PLATFORM_EVENT_TRAP_MAX_VARIABLE_BINDINGS_LENGTH)
	{
	  errno = 0;
	  value = strtol (token, &endptr, 16);
	  if (errno
	      || endptr[0] != '\0')
	    {
	      fprintf (stderr, "invalid variable binding hex byte argument on line %u\n", line_count);
	      rv = 0;
	      goto cleanup;
	    }
	  input->variable_bindings[input->variable_bindings_length++] = (uint8_t) value;
	}
      else
	{
	  fprintf (stderr, "Too many arguments specified on line %u\n", line_count);
	  rv = 0;
	  goto cleanup;
	}
    }

  if (!(input->variable_bindings_length >= IPMI_PLATFORM_EVENT_TRAP_MIN_VARIABLE_BINDINGS_LENGTH
	&& input->variable_bindings_length <= IPMI_PLATFORM_EVENT_TRAP_MAX_VARIABLE_BINDINGS_LENGTH))
    {
      fprintf (stderr,
	       "Invalid number of variable binding bytes on line %u\n",
	       line_count);
      rv = 0;
      goto cleanup;
    }
  
  rv = 1;
 cleanup:
  free (str);
  return (rv);
}

static int
_ipmi_pet_acknowledge (ipmi_pet_state_data_t *state_data, FILE *stream)
{
  struct ipmi_pet_arguments *args;
  struct ipmi_pet_input input;
  struct ipmi_pet_trap_data data;
  uint32_t event_data;
  fiid_obj_t obj_cmd_rs = NULL;
  char *line = NULL;
  unsigned int ctx_flags_orig;
  int rv = -1;

  assert (state_data);
  assert (state_data->ipmi_ctx);
  assert (state_data->prog_data->args->pet_acknowledge);
  assert ((state_data->prog_data->args->variable_bindings_length && !stream)
	  || (!state_data->prog_data->args->variable_bindings_length && stream));

  args = state_data->prog_data->args;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_pet_acknowledge_rs)))
    {
      perror ("fiid_obj_create");
      goto cleanup;
    }

  if (args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_MALFORMED_ACK)
    {
      if (ipmi_ctx_get_flags (state_data->ipmi_ctx, &ctx_flags_orig) < 0)
	{
	  fprintf (stderr,
                   "ipmi_ctx_get_flags: %s\n",
                   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
      
      if (ipmi_ctx_set_flags (state_data->ipmi_ctx, ctx_flags_orig | IPMI_FLAGS_NO_LEGAL_CHECK) < 0)
	{
	  fprintf (stderr,
                   "ipmi_ctx_set_flags: %s\n",
                   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
    }

  if (args->variable_bindings_length)
    {
      if (!(args->variable_bindings_length >= IPMI_PLATFORM_EVENT_TRAP_MIN_VARIABLE_BINDINGS_LENGTH
	    && args->variable_bindings_length <= IPMI_PLATFORM_EVENT_TRAP_MAX_VARIABLE_BINDINGS_LENGTH))
	{
	  fprintf (stderr,
		   "Invalid number of variable binding bytes\n");
	  goto cleanup;
	}
 
      input.specific_trap = args->specific_trap;
      input.specific_trap_na_specified = args->specific_trap_na_specified;
      memcpy (input.variable_bindings,
	      args->variable_bindings,
	      args->variable_bindings_length);
      input.variable_bindings_length = args->variable_bindings_length;

      if (_ipmi_pet_parse_trap_data (state_data,
				     &input,
				     &data) < 0)
	goto cleanup;

      event_data = data.event_data[2];
      event_data <<= 8;
      event_data |= data.event_data[1];
      event_data <<= 8;
      event_data |= data.event_data[0];

      if (ipmi_cmd_pet_acknowledge (state_data->ipmi_ctx,
				    data.sequence_number,
				    data.localtimestamp_raw,
				    data.event_source_type,
				    data.sensor_device,
				    data.sensor_number,
				    event_data,
				    obj_cmd_rs) < 0)
	{
	  fprintf (stderr,
		   "ipmi_cmd_pet_acknowledge: %s\n",
		   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
    }
  else
    {
      size_t n = 0;
      unsigned int line_count = 0;
      int ret;

      while (1)
	{
	  if (getline (&line, &n, stream) < 0)
	    {
	      /* perror ("getline()"); */
	      break;
	    }
	  line_count++;

	  if ((ret = _ipmi_pet_parse (state_data, line, &input, line_count)) < 0)
	    goto cleanup;

	  if (!ret)
	    goto end_loop;
     
	  if (_ipmi_pet_parse_trap_data (state_data,
					 &input,
					 &data) < 0)
	    goto cleanup;
	  
	  event_data = data.event_data[2];
	  event_data <<= 8;
	  event_data |= data.event_data[1];
	  event_data <<= 8;
	  event_data |= data.event_data[0];
	  
	  if (ipmi_cmd_pet_acknowledge (state_data->ipmi_ctx,
					data.sequence_number,
					data.localtimestamp_raw,
					data.event_source_type,
					data.sensor_device,
					data.sensor_number,
					event_data,
					obj_cmd_rs) < 0)
	    {
	      fprintf (stderr,
		       "ipmi_cmd_pet_acknowledge: %s\n",
		       ipmi_ctx_errormsg (state_data->ipmi_ctx));
	      goto cleanup;
	    }
	  
	end_loop:
	  free (line);
	  line = NULL;
	  n = 0;
	}
    }

  if (args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_MALFORMED_ACK)
    {
      if (ipmi_ctx_set_flags (state_data->ipmi_ctx, ctx_flags_orig) < 0)
	{
	  fprintf (stderr,
                   "ipmi_ctx_set_flags: %s\n",
                   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  free (line);
  return (rv);
}

static int
_ipmi_pet_cmdline (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  struct ipmi_pet_input input;
  int rv = -1;

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
  
  input.specific_trap = args->specific_trap;
  input.specific_trap_na_specified = args->specific_trap_na_specified;
  memcpy (input.variable_bindings,
	  args->variable_bindings,
	  args->variable_bindings_length);
  input.variable_bindings_length = args->variable_bindings_length;

  if (_ipmi_pet_process (state_data, &input) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_pet_stream (ipmi_pet_state_data_t *state_data, FILE *stream)
{
  struct ipmi_pet_arguments *args;
  char *line = NULL;
  size_t n = 0;
  unsigned int line_count = 0;
  int rv = -1;
  int ret;

  assert (state_data);
  assert (stream);

  args = state_data->prog_data->args;

  if (_ipmi_pet_init (state_data) < 0)
    goto cleanup;
  
  while (1)
    {
      struct ipmi_pet_input input;

      if (getline (&line, &n, stream) < 0)
        {
          /* perror ("getline()"); */
          break;
        }
      line_count++;

      /* On invalid inputs, we could exit.  However, we assume the
       * user is inputting a large stream of traps, possibly after
       * parsing it from a log or something.  So we just output an
       * error and continue on with trap interpretations when there is
       * an invalid input.
       */
      if ((ret = _ipmi_pet_parse (state_data, line, &input, line_count)) < 0)
	goto cleanup;

      if (!ret)
	goto end_loop;
     
      if (_ipmi_pet_output_headers (state_data) < 0)
	goto cleanup;

      if (_ipmi_pet_process (state_data, &input) < 0)
	goto cleanup;
      
    end_loop:
      free (line);
      line = NULL;
      n = 0;
    }
  
  rv = 0;
 cleanup:
  free (line);
  return (rv);
}

static int
run_cmd_args (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  FILE *infile = NULL;
  int rv = -1;

  assert (state_data);
  
  args = state_data->prog_data->args;
  
  assert (!args->common_args.flush_cache);

  if (args->variable_bindings_length)
    {
      if (args->pet_acknowledge)
	{
	  if (_ipmi_pet_acknowledge (state_data, NULL) < 0)
	    goto cleanup;
	}
      else
	{
	  if (_ipmi_pet_cmdline (state_data) < 0)
	    goto cleanup;
	}

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

  if (args->pet_acknowledge)
    {
      if (_ipmi_pet_acknowledge (state_data, infile) < 0)
	goto cleanup;
    }
  else
    {
      if (_ipmi_pet_stream (state_data, infile) < 0)
	goto cleanup;
    }

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
  int exit_code = EXIT_FAILURE;

  assert (prog_data);

  if (prog_data->args->common_args.flush_cache)
    {
      if (sdr_cache_flush_cache (NULL,
                                 prog_data->args->common_args.hostname,
                                 &prog_data->args->common_args) < 0)
	return (EXIT_FAILURE);
      return (EXIT_SUCCESS);
    }

  memset (&state_data, '\0', sizeof (ipmi_pet_state_data_t));
  state_data.prog_data = prog_data;
  state_data.hostname = prog_data->args->common_args.hostname;

  if (!prog_data->args->common_args.ignore_sdr_cache
      && !prog_data->args->pet_acknowledge)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                             prog_data->args->common_args.hostname,
                                             &(prog_data->args->common_args),
					     NULL)))
	goto cleanup;
    }

  if (prog_data->args->pet_acknowledge)
    {
      if (!(state_data.ipmi_ctx = ipmi_ctx_create ()))
	{
	  perror ("ipmi_ctx_create()");
	  goto cleanup;
	}

      if (ipmi_ctx_open_outofband (state_data.ipmi_ctx,
				   state_data.hostname,
				   NULL,
				   NULL,
				   IPMI_AUTHENTICATION_TYPE_NONE, /* doesn't matter, just anything legal */
				   IPMI_PRIVILEGE_LEVEL_USER, /* doesn't matter, just anything legal */
				   0,
				   0,
				   0,
				   IPMI_FLAGS_NOSESSION | (prog_data->args->common_args.debug ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT)) < 0)
	{
	  if (ipmi_ctx_errnum (state_data.ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID)
	    fprintf (stderr, "%s: %s\n", prog_data->progname, ipmi_ctx_errormsg (state_data.ipmi_ctx));
	  else
	    fprintf (stderr, "ipmi_ctx_open_outofband: %s\n", ipmi_ctx_errormsg (state_data.ipmi_ctx));
	  goto cleanup;
	}
    }

  if (!prog_data->args->common_args.ignore_sdr_cache
       && !prog_data->args->pet_acknowledge)
    {
      if (!(state_data.sdr_ctx = ipmi_sdr_ctx_create ()))
	{
	  perror ("ipmi_sdr_ctx_create()");
	  goto cleanup;
	}
      
      if (!prog_data->args->pet_acknowledge)
        {
          if (sdr_cache_create_and_load (state_data.sdr_ctx,
                                         NULL,
                                         state_data.ipmi_ctx,
                                         state_data.hostname,
                                         &state_data.prog_data->args->common_args) < 0)
            goto cleanup;
        }
    }
  else
    state_data.sdr_ctx = NULL;
  
  if (!(state_data.sel_ctx = ipmi_sel_ctx_create (NULL, state_data.sdr_ctx)))
    {
      perror ("ipmi_sel_ctx_create()");
      goto cleanup;
    }
  
  if (state_data.prog_data->args->common_args.debug
      && prog_data->args->common_args.hostname)
    {
      if (ipmi_sel_ctx_set_debug_prefix (state_data.sel_ctx,
					 prog_data->args->common_args.hostname) < 0)
	fprintf (stderr,
		 "ipmi_sel_ctx_set_debug_prefix: %s\n",
		 ipmi_sel_ctx_errormsg (state_data.sel_ctx));
    }
  
  /* Only for outputting type/length fields */
  if (!(state_data.fru_ctx = ipmi_fru_ctx_create (NULL)))
    {
      perror ("ipmi_fru_ctx_create()");
      goto cleanup;
    }

  if (prog_data->args->output_event_state)
    {
      unsigned int flags = 0;

      if (!(state_data.interpret_ctx = ipmi_interpret_ctx_create ()))
        {
          perror ("ipmi_interpret_ctx_create()");
          goto cleanup;
        }

      if (event_load_event_state_config_file (NULL,
					      state_data.interpret_ctx,
					      prog_data->args->event_state_config_file) < 0)
	goto cleanup;

      if (prog_data->args->interpret_oem_data)
        flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;

      if (flags)
        {
          if (ipmi_interpret_ctx_set_flags (state_data.interpret_ctx, flags) < 0)
            {
              fprintf (stderr,
		       "ipmi_interpret_ctx_set_flags: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              goto cleanup;
            }
        }

      if (ipmi_sel_ctx_set_parameter (state_data.sel_ctx,
				      IPMI_SEL_PARAMETER_INTERPRET_CONTEXT,
				      &(state_data.interpret_ctx)) < 0)
	{
	  fprintf (stderr,
		   "ipmi_sel_ctx_set_interpret: %s\n",
		   ipmi_sel_ctx_errormsg (state_data.sel_ctx));
	  goto cleanup;
	}
    }
  
  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_fru_ctx_destroy (state_data.fru_ctx);
  ipmi_interpret_ctx_destroy (state_data.interpret_ctx);
  ipmi_sel_ctx_destroy (state_data.sel_ctx);
  ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
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

