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
#include "ipmi-oem-ibm.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

/* IBM OEM SDR LED Record
 *
 * 1-2   - Record ID (standard)
 * 3     - Version (standard)
 * 4     - Record Type (0xC0 - standard)
 * 5     - Record Length (standard)
 * 6-8   - Manufacturer ID (standard)
 * 9     - OEM Sensor Type (oem_data)
 * 10-11 - ?? (oem_data)
 * 12-13 - LED ID (oem_data)
 */
#define IPMI_SDR_RECORD_OEM_IBM_LED_OEM_DATA_MIN_LENGTH    5
#define IPMI_SDR_RECORD_OEM_IBM_SENSOR_TYPE_OEM_DATA_INDEX 0
#define IPMI_SDR_RECORD_OEM_IBM_LED_ID_LS_OEM_DATA_INDEX   3
#define IPMI_SDR_RECORD_OEM_IBM_LED_ID_MS_OEM_DATA_INDEX   4

#define IPMI_SDR_RECORD_OEM_IBM_LED_SENSOR_TYPE      0xED

#define IPMI_OEM_IBM_LED_NAME_COLUMN_SIZE  17
#define IPMI_OEM_IBM_LED_NAME_BUFLEN       17

#define IPMI_OEM_IBM_LED_STATE_COLUMN_SIZE 8

#define IPMI_OEM_IBM_LED_ID_STRING_BUFLEN  64

#define IPMI_OEM_IBM_LED_INFO_BUFLEN       1024

struct ipmi_oem_ibm_find_sensor_sdr_callback
{
  ipmi_oem_state_data_t *state_data;
  uint8_t sensor_number;
  char *id_string;
  unsigned int id_string_len;
  int found;
};

struct ipmi_oem_ibm_get_led_sdr_callback
{
  ipmi_oem_state_data_t *state_data;
  struct sensor_column_width *column_width;
  struct ipmi_oem_data *oem_data;
  int header_output_flag;
};

static int
_get_led_name (ipmi_oem_state_data_t *state_data,
               struct ipmi_oem_data *oem_data,
               uint16_t led_id,
               char *led_name,
               unsigned int led_name_len)
{
  char *led_id_str = NULL;

  assert (state_data);
  assert (oem_data);
  assert (led_name);
  assert (led_name_len);

   if (oem_data->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_IBM
       && oem_data->product_id == IPMI_IBM_PRODUCT_ID_X3455)
     {
       if (led_id == IPMI_OEM_IBM_LED_X3455_LOCATION)
         led_id_str = "Location";
     }
   else if (oem_data->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_IBM
            && oem_data->product_id == IPMI_IBM_PRODUCT_ID_X3755)
     {
       switch (led_id)
	 {
	 case IPMI_OEM_IBM_LED_X3755_CPU:
	   led_id_str = "CPU";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU1:
	   led_id_str = "CPU1";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU2:
	   led_id_str = "CPU2";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU3:
	   led_id_str = "CPU3";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU4:
	   led_id_str = "CPU4";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU1_BOARD:
	   led_id_str = "CPU1_BOARD";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU2_BOARD:
	   led_id_str = "CPU2_BOARD";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU3_BOARD:
	   led_id_str = "CPU3_BOARD";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CPU4_BOARD:
	   led_id_str = "CPU4_BOARD";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_1:
	   led_id_str = "DIMM 1";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_2:
	   led_id_str = "DIMM 2";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_3:
	   led_id_str = "DIMM 3";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_4:
	   led_id_str = "DIMM 4";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_5:
	   led_id_str = "DIMM 5";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_6:
	   led_id_str = "DIMM 6";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_7:
	   led_id_str = "DIMM 7";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_8:
	   led_id_str = "DIMM 8";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_9:
	   led_id_str = "DIMM 9";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_10:
	   led_id_str = "DIMM 10";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_11:
	   led_id_str = "DIMM 11";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_12:
	   led_id_str = "DIMM 12";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_13:
	   led_id_str = "DIMM 13";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_14:
	   led_id_str = "DIMM 14";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_15:
	   led_id_str = "DIMM 15";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_16:
	   led_id_str = "DIMM 16";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_17:
	   led_id_str = "DIMM 17";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_18:
	   led_id_str = "DIMM 18";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_19:
	   led_id_str = "DIMM 19";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_20:
	   led_id_str = "DIMM 20";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_21:
	   led_id_str = "DIMM 21";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_22:
	   led_id_str = "DIMM 22";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_23:
	   led_id_str = "DIMM 23";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_24:
	   led_id_str = "DIMM 24";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_25:
	   led_id_str = "DIMM 25";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_26:
	   led_id_str = "DIMM 26";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_27:
	   led_id_str = "DIMM 27";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_28:
	   led_id_str = "DIMM 28";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_29:
	   led_id_str = "DIMM 29";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_30:
	   led_id_str = "DIMM 30";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_31:
	   led_id_str = "DIMM 31";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DIMM_32:
	   led_id_str = "DIMM 32";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN:
	   led_id_str = "FAN";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_1:
	   led_id_str = "Fan 1";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_2:
	   led_id_str = "Fan 2";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_3:
	   led_id_str = "Fan 3";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_4:
	   led_id_str = "Fan 4";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_5:
	   led_id_str = "Fan 5";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_6:
	   led_id_str = "Fan 6";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_7:
	   led_id_str = "Fan 7";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAN_8:
	   led_id_str = "Fan 8";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_PCI:
	   led_id_str = "PCI";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_PCI_1:
	   led_id_str = "PCI 1";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_PCI_2:
	   led_id_str = "PCI 2";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_PCI_3:
	   led_id_str = "PCI 3";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_PCI_4:
	   led_id_str = "PCI 4";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_PCI_5:
	   led_id_str = "PCI 5";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_PCI_6:
	   led_id_str = "PCI 6";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_SERVERAID_8K_BATT:
	   led_id_str = "ServeRAID 8k Batt";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_SERVERAID_8K_ERR:
	   led_id_str = "ServeRAID 8k Err";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_ALERT:
	   led_id_str = "Alert";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_BK_BLUE:
	   led_id_str = "BK_Blue";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_BOARD:
	   led_id_str = "BOARD";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_CNFG:
	   led_id_str = "CNFG";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_DASD:
	   led_id_str = "DASD";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_FAULT:
	   led_id_str = "FAULT";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_HTX:
	   led_id_str = "HTX";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_INFO:
	   led_id_str = "INFO";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_LOCATION:
	   led_id_str = "Location";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_MEM:
	   led_id_str = "MEM";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_NMI:
	   led_id_str = "NMI";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_OVERSPEC:
	   led_id_str = "OVERSPEC";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_RAID:
	   led_id_str = "RAID";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_SEER:
	   led_id_str = "SEER";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_SP:
	   led_id_str = "SP";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_TEMP:
	   led_id_str = "TEMP";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_VRM:
	   led_id_str = "VRM";
	   break;
	 case IPMI_OEM_IBM_LED_X3755_UNKNOWN1:
	 case IPMI_OEM_IBM_LED_X3755_UNKNOWN2:
	 case IPMI_OEM_IBM_LED_X3755_UNKNOWN3:
	   led_id_str = "UNKNOWN";
	   break;
	 }
     }

   if (led_id_str)
     snprintf (led_name,
               led_name_len,
               "%s",
               led_id_str);
   else
     snprintf (led_name,
               led_name_len,
               "LED = %04Xh",
               led_id);      
   
   return (0);
}

static int
_find_sensor_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
			   uint8_t record_type,
			   const void *sdr_record,
			   unsigned int sdr_record_len,
			   void *arg)
{
  struct ipmi_oem_ibm_find_sensor_sdr_callback *sdr_callback_arg;
  ipmi_oem_state_data_t *state_data;
  uint8_t sdr_sensor_number;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  sdr_callback_arg = (struct ipmi_oem_ibm_find_sensor_sdr_callback *)arg;
  state_data = sdr_callback_arg->state_data;

  /* achu: xCAT only checks for Full records, I'll check compact too though */
  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    return (0);

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    sdr_record,
				    sdr_record_len,
				    &sdr_sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_sensor_number: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  if (sdr_callback_arg->sensor_number == sdr_sensor_number)
    {
      if (ipmi_sdr_parse_id_string (state_data->sdr_ctx,
				    sdr_record,
				    sdr_record_len,
				    sdr_callback_arg->id_string,
				    sdr_callback_arg->id_string_len) < 0)
	return (-1);
      
      sdr_callback_arg->found = 1;
      return (1);
    }

  return (0);
}

static int
_find_sensor (ipmi_oem_state_data_t *state_data,
              uint8_t sensor_number,
              char *id_string,
              unsigned int id_string_len)
{
  struct ipmi_oem_ibm_find_sensor_sdr_callback sdr_callback_arg;
  struct common_cmd_args common_args;
  ipmi_sdr_ctx_t tmp_sdr_ctx = NULL;
  int rv = -1;

  assert (state_data);
  assert (id_string);
  assert (id_string_len);

  /* Make temporary sdr cache to search for sensor
   *
   * Redo loading of SDR cache since this is being called from a loop
   * using the state_data sdr_ctx.
   */
  if (!(tmp_sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      pstdout_perror (state_data->pstate, "ipmi_sdr_ctx_create()");
      goto cleanup;
    }

  sdr_callback_arg.state_data = state_data;
  sdr_callback_arg.sensor_number = sensor_number;
  sdr_callback_arg.id_string = id_string;
  sdr_callback_arg.id_string_len = id_string_len;
  sdr_callback_arg.found = 0;

  /* Should not cause sdr recreation, since this is the second time we're calling it */
  memcpy (&common_args, &state_data->prog_data->args->common_args, sizeof (struct common_cmd_args));
  common_args.quiet_cache = 1;
  common_args.sdr_cache_recreate = 0;

  if (sdr_cache_create_and_load (tmp_sdr_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
				 &common_args) < 0)
    goto cleanup;

  if (ipmi_sdr_cache_iterate (tmp_sdr_ctx,
			      _find_sensor_sdr_callback,
			      &sdr_callback_arg) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_iterate: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (!sdr_callback_arg.found)
    snprintf (id_string,
              id_string_len,
              "Sensor Number = %02Xh",
              sensor_number);
  rv = 0;
 cleanup:
  ipmi_sdr_ctx_destroy (tmp_sdr_ctx);
  return (rv);
}

static int
_get_led_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
		       uint8_t record_type,
		       const void *sdr_record,
		       unsigned int sdr_record_len,
		       void *arg)
{
  struct ipmi_oem_ibm_get_led_sdr_callback *sdr_callback_arg;
  ipmi_oem_state_data_t *state_data;
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t oem_data_buf[IPMI_SDR_MAX_RECORD_LENGTH];
  int oem_data_buf_len;
  uint16_t record_id;
  char fmt[IPMI_OEM_FMT_BUFLEN + 1];
  char led_name[IPMI_OEM_IBM_LED_NAME_BUFLEN + 1];
  char led_pointer_name[IPMI_OEM_IBM_LED_NAME_BUFLEN + 1];
  char id_string[IPMI_OEM_IBM_LED_ID_STRING_BUFLEN + 1];
  char led_info[IPMI_OEM_IBM_LED_INFO_BUFLEN + 1];
  char *led_state_str = NULL;
  uint8_t sensor_type;
  uint8_t led_id_ls;
  uint8_t led_id_ms;
  uint16_t led_id;
  uint8_t led_state;
  uint8_t led_active_type;
  uint16_t led_pointer_id;
  uint8_t sensor_number;
  int available_led;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  sdr_callback_arg = (struct ipmi_oem_ibm_get_led_sdr_callback *)arg;
  state_data = sdr_callback_arg->state_data;

  /* IBM OEM
   *
   * From xCAT (http://xcat.sourceforge.net/)
   *
   * Get Led Request
   *
   * 0x3A - OEM network function (is IPMI_NET_FN_OEM_IBM_LED_RQ)
   * 0xC0 - OEM cmd
   * 0x?? - LED ID (MS Byte)
   * 0x?? - LED ID (LS Byte)
   *
   * Get Led Response
   *
   * 0xC0 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - ??
   * 0x?? - LED Active vs. Inactive
   *      - non-zero = active
   *      - 0 = inactive
   * 0x?? - ??
   * 0x?? - LED Pointer ID (MS Byte)
   * 0x?? - LED Pointer ID (LS Byte) / Sensor Number
   *      - Pointer ID means indicating problem elsewhere
   * 0x?? - LED Active Type
   *      - 1 - Indicates LED Active to indicate LED Pointer ID Active
   *      - 2 - Indicates LED Active due to Sensor w/ Sensor Number
   *      - 3 - User manually activated LED
   *      - 4 - BIOS or Administrator lit LED
   */

  if (record_type != IPMI_SDR_FORMAT_OEM_RECORD)
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

  if ((oem_data_buf_len = ipmi_sdr_parse_oem_data (state_data->sdr_ctx,
						   sdr_record,
						   sdr_record_len,
						   oem_data_buf,
						   IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_oem_data: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }
      
  /* If not enough data, skip it */
  if (oem_data_buf_len < IPMI_SDR_RECORD_OEM_IBM_LED_OEM_DATA_MIN_LENGTH)
    return (0);

  sensor_type = oem_data_buf[IPMI_SDR_RECORD_OEM_IBM_SENSOR_TYPE_OEM_DATA_INDEX];
      
  /* If not LED sensor type, skip it */
  if (sensor_type != IPMI_SDR_RECORD_OEM_IBM_LED_SENSOR_TYPE)
    return (0);

  /* IBM systems use inconsistent endian, guess endian by assuming
   * LED IDs are numerically started at 0
   */
      
  if (oem_data_buf[IPMI_SDR_RECORD_OEM_IBM_LED_ID_LS_OEM_DATA_INDEX] > oem_data_buf[IPMI_SDR_RECORD_OEM_IBM_LED_ID_MS_OEM_DATA_INDEX])
    {
      led_id_ls = oem_data_buf[IPMI_SDR_RECORD_OEM_IBM_LED_ID_LS_OEM_DATA_INDEX];
      led_id_ms = oem_data_buf[IPMI_SDR_RECORD_OEM_IBM_LED_ID_MS_OEM_DATA_INDEX];
    }
  else
    {
      led_id_ls = oem_data_buf[IPMI_SDR_RECORD_OEM_IBM_LED_ID_MS_OEM_DATA_INDEX];
      led_id_ms = oem_data_buf[IPMI_SDR_RECORD_OEM_IBM_LED_ID_LS_OEM_DATA_INDEX];
    }

  led_id = led_id_ls | (led_id_ms << 8);

  bytes_rq[0] = IPMI_CMD_OEM_IBM_GET_LED;
  bytes_rq[1] = led_id_ms;
  bytes_rq[2] = led_id_ls;
      
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
			      0, /* lun */
			      IPMI_NET_FN_OEM_IBM_LED_RQ, /* network function */
			      bytes_rq, /* data */
			      3, /* num bytes */
			      bytes_rs,
			      IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_raw: %s\n",
		       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      return (-1);
    }
      
  /* If get parameter out of range, assume LED ID endian wrong and try again */
  if (rs_len >= 2 && bytes_rs[1] == IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE)
    {
      bytes_rq[0] = IPMI_CMD_OEM_IBM_GET_LED;
      bytes_rq[1] = led_id_ls;
      bytes_rq[2] = led_id_ms;

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_OEM_IBM_LED_RQ, /* network function */
				  bytes_rq, /* data */
				  3, /* num bytes */
				  bytes_rs,
				  IPMI_OEM_MAX_BYTES)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_raw: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  return (-1);
	}
    }

  /* achu: there are probably 1 or 2 completion codes that are
   * acceptable to ignore and continue on, but who knows what they
   * are.
   */

  /* Assume this error code means LED not available */
  if (rs_len >= 2 && bytes_rs[1] == IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE)
    available_led = 0;
  else
    {
      if (ipmi_oem_check_response_and_completion_code (state_data,
						       bytes_rs,
						       rs_len,
						       8,
						       IPMI_CMD_OEM_IBM_GET_LED,
						       IPMI_NET_FN_OEM_IBM_LED_RS,
						       NULL) < 0)
	return (-1);
      
      available_led = 1;
    }

  if (!sdr_callback_arg->header_output_flag)
    {
      memset (fmt, '\0', IPMI_OEM_FMT_BUFLEN + 1);
      
      snprintf (fmt,
		IPMI_OEM_FMT_BUFLEN,
		"%%-%ds | LED               | State    | LED Information\n",
		sdr_callback_arg->column_width->record_id);
      
      pstdout_printf (state_data->pstate,
		      fmt,
		      SENSORS_HEADER_RECORD_ID_STR);
      
      sdr_callback_arg->header_output_flag++;
    }
      
  memset (led_name, '\0', IPMI_OEM_IBM_LED_NAME_BUFLEN + 1);
  memset (led_pointer_name, '\0', IPMI_OEM_IBM_LED_NAME_BUFLEN + 1);
  memset (id_string, '\0', IPMI_OEM_IBM_LED_ID_STRING_BUFLEN + 1);
  memset (led_info, '\0', IPMI_OEM_IBM_LED_INFO_BUFLEN + 1);
      
  if (_get_led_name (state_data,
		     sdr_callback_arg->oem_data,
		     led_id,
		     led_name,
		     IPMI_OEM_IBM_LED_NAME_BUFLEN) < 0)
    return (-1);
      
  if (available_led)
    {
      led_state = bytes_rs[3];
      led_active_type = bytes_rs[7];
      led_pointer_id = (bytes_rs[5] << 8) | bytes_rs[6];
      sensor_number = bytes_rs[6];

      if (led_state == IPMI_OEM_IBM_LED_STATE_INACTIVE)
	led_state_str = "Inactive";
      else
	led_state_str = "Active";
      
      if (led_state != IPMI_OEM_IBM_LED_STATE_INACTIVE)
	{
	  /* Location LED special case */
	  if (!led_id)
	    {
	      snprintf (led_info,
			IPMI_OEM_IBM_LED_INFO_BUFLEN,
			"System Error Condition");
	    }
	  else if (led_active_type == IPMI_OEM_IBM_LED_ACTIVE_BY_LED)
	    {
	      if (_get_led_name (state_data,
				 sdr_callback_arg->oem_data,
				 led_pointer_id,
				 led_pointer_name,
				 IPMI_OEM_IBM_LED_NAME_BUFLEN) < 0)
		return (-1);
	      
	      snprintf (led_info,
			IPMI_OEM_IBM_LED_INFO_BUFLEN,
			"'%s' Active",
			led_pointer_name);
	    }
	  else if (led_active_type == IPMI_OEM_IBM_LED_ACTIVE_BY_SENSOR)
	    {
	      /* achu: sensor numbers may not be unique.  I'm copying
	       * this algorithm from xCAT so I assume it's safe for
	       * IBM machines b/c IBM lays out their SDRs in a fashion
	       * that this search is safe and won't result in an
	       * incorrect output.
	       */
	      if (_find_sensor (state_data,
				sensor_number,
				id_string,
				IPMI_OEM_IBM_LED_ID_STRING_BUFLEN) < 0)
		return (-1);
	      
	      snprintf (led_info,
			IPMI_OEM_IBM_LED_INFO_BUFLEN,
			"Sensor '%s' error",
			id_string);
	    }
	  else if (led_active_type == IPMI_OEM_IBM_LED_ACTIVE_BY_USER)
	    {
	      snprintf (led_info,
			IPMI_OEM_IBM_LED_INFO_BUFLEN,
			"LED Activated by User");
	    }
	  else if (led_active_type == IPMI_OEM_IBM_LED_ACTIVE_BY_BIOS_OR_ADMINISTRATOR)
	    {
	      snprintf (led_info,
			IPMI_OEM_IBM_LED_INFO_BUFLEN,
			"LED Activated by BIOS or Administrator");
	    }
	}
    }
  else
    led_state_str = "N/A";
      
  snprintf (fmt,
	    IPMI_OEM_FMT_BUFLEN,
	    "%%-%du | %%-%ds | %%-%ds | %s\n",
	    sdr_callback_arg->column_width->record_id,
	    IPMI_OEM_IBM_LED_NAME_COLUMN_SIZE,
	    IPMI_OEM_IBM_LED_STATE_COLUMN_SIZE,
	    led_info);
      
  pstdout_printf (state_data->pstate,
		  fmt,
		  record_id,
		  led_name,
		  led_state_str,
		  led_info);

  return (0);
}

int
ipmi_oem_ibm_get_led (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_ibm_get_led_sdr_callback sdr_callback_arg;
  struct sensor_column_width column_width;
  struct ipmi_oem_data oem_data;
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
                               0, /* count_device_locator_records */
                               1, /* count_oem_records */
                               0, /* entity_sensor_names */
                               &column_width) < 0)
    goto cleanup;

  if (ipmi_get_oem_data (state_data->pstate,
                         state_data->ipmi_ctx,
                         &oem_data) < 0)
    goto cleanup;

  sdr_callback_arg.state_data = state_data;
  sdr_callback_arg.column_width = &column_width;
  sdr_callback_arg.oem_data = &oem_data;
  sdr_callback_arg.header_output_flag = 0;

  if (ipmi_sdr_cache_iterate (state_data->sdr_ctx,
			      _get_led_sdr_callback,
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
