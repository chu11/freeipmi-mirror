/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-sun.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_ACCESS_ADDRESS_INDEX 5
#define IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_SLAVE_ADDRESS_INDEX  6
#define IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_OEM_INDEX                   14

struct ipmi_oem_sun_get_led_sdr_callback
{
  ipmi_oem_state_data_t *state_data;
  struct sensor_column_width *column_width;
  int header_output_flag;
};

static int
_sun_get_led_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
			   uint8_t record_type,
			   const void *sdr_record,
			   unsigned int sdr_record_len,
			   void *arg)
{
  struct ipmi_oem_sun_get_led_sdr_callback *sdr_callback_arg;
  ipmi_oem_state_data_t *state_data;
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint16_t record_id;
  char fmt[IPMI_OEM_FMT_BUFLEN + 1];
  char device_id_string[IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH + 1];
  char sensor_name_buf[IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1];
  char *sensor_name = NULL;
  uint8_t entity_instance_type;
  uint8_t led_mode;
  char *led_mode_str = NULL;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  sdr_callback_arg = (struct ipmi_oem_sun_get_led_sdr_callback *)arg;
  state_data = sdr_callback_arg->state_data;

  /* Sun OEM
   *
   * From Ipmitool (http://ipmitool.sourceforge.net/)
   *
   * Get Led Request
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x21 - OEM cmd
   * 0x?? - Device Slave Address (in General Device Locator Record)
   *      - Note that the IPMI command requires the entire
   *        byte of the slave address.
   * 0x?? - LED Type (see below [1])
   *      - 0 - ok2rm
   *      - 1 - service
   *      - 2 - activity
   *      - 3 - locate 
   * 0x?? - Controller Address / Device Access Address (in General Device Locator Record)
   *      - 0x20 if the LED is local
   *      - Note that the IPMI command requires the entire
   *        byte of the access address.
   * 0x?? - HW Info (OEM field in General Device Locator Record)
   * 0x?? - Force
   *      - 0 - Go thru controller
   *      - 1 - Directly access device
   *
   * An alternate format is described in the ipmitool comments for Sun
   * Blade Moduler Systems.
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x21 - OEM cmd
   * 0x?? - Device Slave Address (in General Device Locator Record)
   * 0x?? - LED Type
   * 0x?? - Controller Address / Device Access Address (in General Device Locator Record)
   * 0x?? - HW Info (OEM field in General Device Locator Record)
   * 0x?? - Entity ID
   * 0x?? - Entity Instance
   *      - 7 bit version
   * 0x?? - Force
   *      - 0 - Go thru controller
   *      - 1 - Directly access device
   *
   * Get Led Response
   *
   * 0x21 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - LED mode
   *
   * achu notes: 
   *
   * [1] - As far as I can tell, the LED type field is useless.  My
   * assumption is that on older Sun systems, or other motherboards I
   * don't have access to, one can specify an LED type, which allows
   * you to enable/disable a particular LED amongst many.  On my Sun
   * Fire 4140, it appears to do nothing and affect nothing.  I will
   * add in a new option later if it becomes necessary for the user to
   * specify an LED type.  In the meantime, I will copy the code use
   * in ipmitool and set this field to the OEM field.
   */

  if (record_type != IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
    return (0);

  if (ipmi_sdr_parse_entity_id_instance_type (state_data->sdr_ctx,
					      sdr_record,
					      sdr_record_len,
					      NULL,
					      NULL,
					      &entity_instance_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_entity_id_and_instance: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }
      
  /* if it isn't a physical instance, don't continue on */

  if (entity_instance_type == IPMI_SDR_LOGICAL_CONTAINER_ENTITY)
    return (0);
          
  if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
					 sdr_record,
					 sdr_record_len,
					 &record_id,
					 NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_record_id_and_type: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  /* achu: the sun oem commands want the full byte, not just the
   * sub-field, so use indexes instead of sdr-parse lib.
   */

  bytes_rq[0] = IPMI_CMD_OEM_SUN_GET_LED;
  bytes_rq[1] = ((uint8_t *)sdr_record)[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_SLAVE_ADDRESS_INDEX];
  bytes_rq[2] = ((uint8_t *)sdr_record)[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_OEM_INDEX];
  bytes_rq[3] = ((uint8_t *)sdr_record)[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_ACCESS_ADDRESS_INDEX];
  bytes_rq[4] = ((uint8_t *)sdr_record)[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_OEM_INDEX];
  bytes_rq[5] = IPMI_OEM_SUN_LED_FORCE_GO_THRU_CONTROLLER;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
			      0, /* lun */
			      IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
			      bytes_rq, /* data */
			      6, /* num bytes */
			      bytes_rs,
			      IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_raw: %s\n",
		       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      return (-1);
    }
  
  /* achu: there are probably 1 or 2 completion codes that are
   * acceptable to ignore and continue on, but who knows what they
   * are.
   */

  if (ipmi_oem_check_response_and_completion_code (state_data,
						   bytes_rs,
						   rs_len,
						   3,
						   IPMI_CMD_OEM_SUN_GET_LED,
						   IPMI_NET_FN_OEM_GROUP_RS,
						   NULL) < 0)
    return (-1);
  
  if (!sdr_callback_arg->header_output_flag)
    {
      memset (fmt, '\0', IPMI_OEM_FMT_BUFLEN + 1);
      
      snprintf (fmt,
		IPMI_OEM_FMT_BUFLEN,
		"%%-%ds | %%-%ds | LED Mode\n",
		sdr_callback_arg->column_width->record_id,
		sdr_callback_arg->column_width->sensor_name);
      
      pstdout_printf (state_data->pstate,
		      fmt,
		      SENSORS_HEADER_RECORD_ID_STR,
		      SENSORS_HEADER_NAME_STR);
          
      sdr_callback_arg->header_output_flag++;
    }
      
  led_mode = bytes_rs[2];
      
  if (state_data->prog_data->args->verbose_count)
    {
      memset (sensor_name_buf, '\0', IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1);
      
      if (ipmi_sdr_parse_entity_sensor_name (state_data->sdr_ctx,
					     NULL,
					     0,
					     0, /* sensor number */
					     IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS, /* flags */
					     sensor_name_buf,
					     IPMI_SDR_MAX_SENSOR_NAME_LENGTH) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_entity_sensor_name: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  return (-1);
	}
          
      sensor_name = sensor_name_buf;
    }
  else
    {
      memset (device_id_string, '\0', IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH + 1);
      
      if (ipmi_sdr_parse_device_id_string (state_data->sdr_ctx,
					   sdr_record,
					   sdr_record_len,
					   device_id_string,
					   IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_device_id_string: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  return (-1);
	}
      
      sensor_name = device_id_string;
    }
      
  switch (led_mode)
    {
    case IPMI_OEM_SUN_LED_MODE_OFF:
      led_mode_str = "Off";
      break;
    case IPMI_OEM_SUN_LED_MODE_ON:
      led_mode_str = "On";
      break;
    case IPMI_OEM_SUN_LED_MODE_STANDBY:
      led_mode_str = "Standby";
      break;
    case IPMI_OEM_SUN_LED_MODE_SLOW:
      led_mode_str = "Slow";
      break;
    case IPMI_OEM_SUN_LED_MODE_FAST:
      led_mode_str = "Fast";
      break;
    default:
      led_mode_str = "Unknown";
    }
  
  snprintf (fmt,
	    IPMI_OEM_FMT_BUFLEN,
	    "%%-%du | %%-%ds | %s\n",
	    sdr_callback_arg->column_width->record_id,
	    sdr_callback_arg->column_width->sensor_name,
	    led_mode_str);
      
  pstdout_printf (state_data->pstate,
		  fmt,
		  record_id,
		  sensor_name);

  return (0);
}

int
ipmi_oem_sun_get_led (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_sun_get_led_sdr_callback sdr_callback_arg;
  struct sensor_column_width column_width;
  int rv = -1;
  
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (sdr_cache_create_and_load (state_data->sdr_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
 				 &state_data->prog_data->args->common_args) < 0)
    goto cleanup;

  if (calculate_column_widths (state_data->pstate,
                               state_data->sdr_ctx,
                               NULL,
                               0,
                               NULL,
                               0,
                               0, /* non_abbreviated_units */
                               0, /* shared_sensors */
                               0, /* count_event_only_records */
                               1, /* count_device_locator_records */
                               0, /* count_oem_records */
                               state_data->prog_data->args->verbose_count,
                               &column_width) < 0)
    goto cleanup;

  sdr_callback_arg.state_data = state_data;
  sdr_callback_arg.column_width = &column_width;
  sdr_callback_arg.header_output_flag = 0;

  if (ipmi_sdr_cache_iterate (state_data->sdr_ctx,
			      _sun_get_led_sdr_callback,
			      &sdr_callback_arg) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_iterate: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
 
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_sun_set_led (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint16_t record_id;
  uint8_t led_mode;
  long value;
  char *ptr;
  uint8_t sdr_record[IPMI_SDR_MAX_RECORD_LENGTH];
  int sdr_record_len = 0;
  uint8_t record_type;
  uint8_t entity_instance_type;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  errno = 0;
  value = strtol (state_data->prog_data->args->oem_options[0], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || value < 0
      || value < IPMI_SDR_RECORD_ID_FIRST
      || value > IPMI_SDR_RECORD_ID_LAST)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid record_id\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command);
      goto cleanup;
    }

  record_id = value;

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "off")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "on")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "standby")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "slow")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "fast"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "off"))
    led_mode = IPMI_OEM_SUN_LED_MODE_OFF;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "on"))
    led_mode = IPMI_OEM_SUN_LED_MODE_ON;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "standby"))
    led_mode = IPMI_OEM_SUN_LED_MODE_STANDBY;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "slow"))
    led_mode = IPMI_OEM_SUN_LED_MODE_SLOW;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[1], "fast") */
    led_mode = IPMI_OEM_SUN_LED_MODE_FAST;

  if (sdr_cache_create_and_load (state_data->sdr_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
 				 &state_data->prog_data->args->common_args) < 0)
    goto cleanup;

  /* Sun OEM
   *
   * From Ipmitool (http://ipmitool.sourceforge.net/)
   *
   * Set Led Request
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x22 - OEM cmd
   * 0x?? - Device Slave Address (in General Device Locator Record)
   *      - Note that the IPMI command requires the entire
   *        byte of the slave address.
   * 0x?? - LED Type (see below [1])
   *      - 0 - ok2rm
   *      - 1 - service
   *      - 2 - activity
   *      - 3 - locate 
   * 0x?? - Controller Address / Device Access Address (in General Device Locator Record)
   *      - 0x20 if the LED is local
   *      - Note that the IPMI command requires the entire
   *        byte of the access address.
   * 0x?? - HW Info (OEM field in General Device Locator Record)
   * 0x?? - LED Mode
   * 0x?? - Force
   *      - 0 - Go thru controller
   *      - 1 - Directly access device
   * 0x?? - Role
   *      - Ipmitool comments state "Used by BMC for authorization purposes"
   *      - achu: I have no idea what this is for, set to 0 like in code
   *
   * An alternate format is described in the ipmitool comments for Sun
   * Blade Moduler Systems.
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x22 - OEM cmd
   * 0x?? - Device Slave Address (in General Device Locator Record)
   * 0x?? - LED Type
   * 0x?? - Controller Address / Device Access Address (in General Device Locator Record)
   * 0x?? - HW Info (OEM field in General Device Locator Record)
   * 0x?? - LED Mode
   * 0x?? - Entity ID
   * 0x?? - Entity Instance
   *      - 7 bit version
   * 0x?? - Force
   *      - 0 - Go thru controller
   *      - 1 - Directly access device
   * 0x?? - Role
   *      - Ipmitool comments state "Used by BMC for authorization purposes"
   *      - achu: I have no idea what this is for, set to 0 like in code
   *
   * Set Led Response
   *
   * 0x22 - OEM cmd
   * 0x?? - Completion Code
   *
   * achu notes: 
   *
   * [1] - As far as I can tell, the LED type field is useless.  My
   * assumption is that on older Sun systems, or other motherboards I
   * don't have access to, one can specify an LED type, which allows
   * you to enable/disable a particular LED amongst many.  On my Sun
   * Fire 4140, it appears to do nothing and affect nothing.  I will
   * add in a new option later if it becomes necessary for the user to
   * specify an LED type.  In the meantime, I will copy the code use
   * in ipmitool and set this field to the OEM field.
   */
  
  if (ipmi_sdr_cache_search_record_id (state_data->sdr_ctx, record_id) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_cache_search_record_id: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_ctx,
                                                    sdr_record,
                                                    IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_cache_record_read: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if (record_type != IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Record ID points to invalid record type: %Xh\n",
                       record_type);
      goto cleanup;
    }
  
  if (ipmi_sdr_parse_entity_id_instance_type (state_data->sdr_ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              NULL,
                                              NULL,
                                              &entity_instance_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_entity_id_and_instance: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (entity_instance_type != IPMI_SDR_PHYSICAL_ENTITY)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Record ID points to non physical entity\n");
      goto cleanup;
    }
  
  /* achu: the sun oem commands want the full byte, not just the
   * sub-field, so use indexes instead of sdr-parse lib.
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_SUN_SET_LED;
  bytes_rq[1] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_SLAVE_ADDRESS_INDEX];
  bytes_rq[2] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_OEM_INDEX];
  bytes_rq[3] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_ACCESS_ADDRESS_INDEX];
  bytes_rq[4] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_OEM_INDEX];
  bytes_rq[5] = led_mode;
  bytes_rq[6] = IPMI_OEM_SUN_LED_FORCE_GO_THRU_CONTROLLER;
  bytes_rq[7] = 0;              /* see comments above, just set to 0 */
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
      
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_SUN_SET_LED,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;
 
  rv = 0;
 cleanup: 
  return (rv);
}
