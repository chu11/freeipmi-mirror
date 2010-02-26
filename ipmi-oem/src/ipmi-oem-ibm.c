/*
 * Copyright (C) 2008-2010 FreeIPMI Core Team
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

/* IBM OEM SDR LED Record
 *
 * 1-2   - Record ID (standard)
 * 3     - Version (standard)
 * 4     - Record Type (0xC0 - standard)
 * 5     - Record Length (standard)
 * 6-8   - Manufacturer ID (standard)
 * 9-12  - ??
 * 13    - OEM Sensor Type
 * 14-15 - ??
 * 16-17 - LED ID
 * ??    - Dunno
 */
#define IPMI_SDR_RECORD_OEM_IBM_SENSOR_TYPE_INDEX 12
#define IPMI_SDR_RECORD_OEM_IBM_LED_ID_LS_INDEX   15
#define IPMI_SDR_RECORD_OEM_IBM_LED_ID_MS_INDEX   16

#define IPMI_IBM_LED_X3455_LOCATION 0x1

#define IPMI_IBM_LED_X3755_ALERT             0x00D9

#define IPMI_IBM_LED_X3755_CPU               0x0010
#define IPMI_IBM_LED_X3755_CPU1              0x0030
#define IPMI_IBM_LED_X3755_CPU2              0x0031
#define IPMI_IBM_LED_X3755_CPU3              0x0032
#define IPMI_IBM_LED_X3755_CPU4              0x0033

#define IPMI_IBM_LED_X3755_CPU1_BOARD        0x00B8
#define IPMI_IBM_LED_X3755_CPU2_BOARD        0x00B9
#define IPMI_IBM_LED_X3755_CPU3_BOARD        0x00BA
#define IPMI_IBM_LED_X3755_CPU4_BOARD        0x00BB

#define IPMI_IBM_LED_X3755_DIMM_1            0x0060
#define IPMI_IBM_LED_X3755_DIMM_2            0x0061
#define IPMI_IBM_LED_X3755_DIMM_3            0x0062 
#define IPMI_IBM_LED_X3755_DIMM_4            0x0063
#define IPMI_IBM_LED_X3755_DIMM_5            0x0064
#define IPMI_IBM_LED_X3755_DIMM_6            0x0065
#define IPMI_IBM_LED_X3755_DIMM_7            0x0066
#define IPMI_IBM_LED_X3755_DIMM_8            0x0067
#define IPMI_IBM_LED_X3755_DIMM_9            0x0068
#define IPMI_IBM_LED_X3755_DIMM_10           0x0069
#define IPMI_IBM_LED_X3755_DIMM_11           0x006A
#define IPMI_IBM_LED_X3755_DIMM_12           0x006B
#define IPMI_IBM_LED_X3755_DIMM_13           0x006C
#define IPMI_IBM_LED_X3755_DIMM_14           0x006D
#define IPMI_IBM_LED_X3755_DIMM_15           0x006E
#define IPMI_IBM_LED_X3755_DIMM_16           0x006F
#define IPMI_IBM_LED_X3755_DIMM_17           0x00C0
#define IPMI_IBM_LED_X3755_DIMM_18           0x00C1
#define IPMI_IBM_LED_X3755_DIMM_19           0x00C2
#define IPMI_IBM_LED_X3755_DIMM_20           0x00C3
#define IPMI_IBM_LED_X3755_DIMM_21           0x00C4
#define IPMI_IBM_LED_X3755_DIMM_22           0x00C5
#define IPMI_IBM_LED_X3755_DIMM_23           0x00C6
#define IPMI_IBM_LED_X3755_DIMM_24           0x00C7
#define IPMI_IBM_LED_X3755_DIMM_25           0x00C8
#define IPMI_IBM_LED_X3755_DIMM_26           0x00C9
#define IPMI_IBM_LED_X3755_DIMM_27           0x00CA
#define IPMI_IBM_LED_X3755_DIMM_28           0x00CB
#define IPMI_IBM_LED_X3755_DIMM_29           0x00CC
#define IPMI_IBM_LED_X3755_DIMM_30           0x00CD
#define IPMI_IBM_LED_X3755_DIMM_31           0x00CE
#define IPMI_IBM_LED_X3755_DIMM_32           0x00CF

#define IPMI_IBM_LED_X3755_FAN               0x0014
#define IPMI_IBM_LED_X3755_FAN_1             0x0050
#define IPMI_IBM_LED_X3755_FAN_2             0x0051
#define IPMI_IBM_LED_X3755_FAN_3             0x0052
#define IPMI_IBM_LED_X3755_FAN_4             0x0053
#define IPMI_IBM_LED_X3755_FAN_5             0x0054
#define IPMI_IBM_LED_X3755_FAN_6             0x0055
#define IPMI_IBM_LED_X3755_FAN_7             0x0056
#define IPMI_IBM_LED_X3755_FAN_8             0x0057

#define IPMI_IBM_LED_X3755_PCI               0x0020
#define IPMI_IBM_LED_X3755_PCI_1             0x0070
#define IPMI_IBM_LED_X3755_PCI_6             0x0075
#define IPMI_IBM_LED_X3755_PCI_3             0x0072
#define IPMI_IBM_LED_X3755_PCI_5             0x0074
#define IPMI_IBM_LED_X3755_PCI_4             0x0073
#define IPMI_IBM_LED_X3755_PCI_2             0x0071

#define IPMI_IBM_LED_X3755_SERVERAID_8K_BATT 0x00D0
#define IPMI_IBM_LED_X3755_SERVERAID_8K_ERR  0x00D1

#define IPMI_IBM_LED_X3755_BK_BLUE           0x00D8
#define IPMI_IBM_LED_X3755_BOARD             0x000E
#define IPMI_IBM_LED_X3755_CNFG              0x0006
#define IPMI_IBM_LED_X3755_DASD              0x0013
#define IPMI_IBM_LED_X3755_FAULT             0x0000
#define IPMI_IBM_LED_X3755_HTX               0x00B0
#define IPMI_IBM_LED_X3755_INFO              0x0003
#define IPMI_IBM_LED_X3755_LOCATION          0x0001
#define IPMI_IBM_LED_X3755_MEM               0x0015
#define IPMI_IBM_LED_X3755_NMI               0x0019
#define IPMI_IBM_LED_X3755_OVERSPEC          0x001B
#define IPMI_IBM_LED_X3755_RAID              0x000F
#define IPMI_IBM_LED_X3755_SEER              0x000B
#define IPMI_IBM_LED_X3755_SP                0x001E
#define IPMI_IBM_LED_X3755_TEMP              0x001C
#define IPMI_IBM_LED_X3755_VRM               0x0011
 
#define IPMI_IBM_LED_X3755_UNKNOWN1          0x0040
#define IPMI_IBM_LED_X3755_UNKNOWN2          0x0041
#define IPMI_IBM_LED_X3755_UNKNOWN3          0x0047

int
ipmi_oem_ibm_get_led (ipmi_oem_state_data_t *state_data)
{
  struct sensor_column_width column_width;
  uint16_t record_count;
  int rv = -1;
  int header_output_flag = 0;
  int i;
  
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->prog_data->args->sdr.quiet_cache,
                                 state_data->prog_data->args->sdr.sdr_cache_recreate,
                                 state_data->hostname,
                                 state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    goto cleanup;

  if (calculate_column_widths (state_data->pstate,
                               state_data->sdr_cache_ctx,
                               state_data->sdr_parse_ctx,
                               NULL,
                               0,
                               NULL,
                               0,
                               1, /* abbreviated_units */
                               0,
                               0,
                               0,
                               NULL,
                               &column_width) < 0)
    goto cleanup;

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

  if (ipmi_sdr_cache_record_count (state_data->sdr_cache_ctx, &record_count) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_record_count: %s\n",
		       ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
      goto cleanup;
    }

  for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (state_data->sdr_cache_ctx))
    {
      uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
      uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
      int rs_len;
      uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
      int sdr_record_len = 0;
      uint16_t record_id;
      uint8_t record_type;
      char fmt[IPMI_OEM_FMT_BUFLEN + 1];
      char device_id_string[IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING + 1];
      char sensor_name_buf[MAX_ENTITY_ID_SENSOR_NAME_STRING + 1];
      char *sensor_name = NULL;
      uint8_t entity_instance_type;
      uint8_t led_mode;
      char *led_mode_str = NULL;

      if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_cache_ctx,
							sdr_record,
							IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_cache_record_read: %s\n",
			   ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
	  goto cleanup;
	}
      
      if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_parse_ctx,
					     sdr_record,
					     sdr_record_len,
					     &record_id,
					     &record_type) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_record_id_and_type: %s\n",
			   ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
	  goto cleanup;
	}
      
      
      if (record_type != IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
        continue;

      if (ipmi_sdr_parse_entity_id_instance_type (state_data->sdr_parse_ctx,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  NULL,
                                                  NULL,
                                                  &entity_instance_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_entity_id_and_instance: %s\n",
                           ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
          goto cleanup;
        }
      
      /* if it isn't a physical instance, don't continue on */

      if (entity_instance_type == IPMI_SDR_LOGICAL_CONTAINER_ENTITY)
        continue;
          
      /* achu: the sun oem commands want the full byte, not just the
       * sub-field, so use indexes instead of sdr-parse lib.
       */

      bytes_rq[0] = IPMI_CMD_OEM_SUN_GET_LED;
      bytes_rq[1] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_SLAVE_ADDRESS_INDEX];
      bytes_rq[2] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_OEM_INDEX];
      bytes_rq[3] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_DEVICE_ACCESS_ADDRESS_INDEX];
      bytes_rq[4] = sdr_record[IPMI_SDR_RECORD_GENERIC_DEVICE_LOCATOR_OEM_INDEX];
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
          goto cleanup;
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
        goto cleanup;
      
      if (!header_output_flag)
        {
          memset (fmt, '\0', IPMI_OEM_FMT_BUFLEN + 1);
          
          snprintf (fmt,
                    IPMI_OEM_FMT_BUFLEN,
                    "%%-%ds | %%-%ds | LED Mode\n",
                    column_width.record_id,
                    column_width.sensor_name);
          
          pstdout_printf (state_data->pstate,
                          fmt,
                          SENSORS_HEADER_RECORD_ID_STR,
                          SENSORS_HEADER_NAME_STR);
          
          header_output_flag++;
        }
      
      led_mode = bytes_rs[2];
      
      if (state_data->prog_data->args->verbose_count)
        {
          memset (sensor_name_buf, '\0', MAX_ENTITY_ID_SENSOR_NAME_STRING + 1);
          
          if (get_entity_sensor_name_string (state_data->pstate,
                                             state_data->sdr_parse_ctx,
                                             sdr_record,
                                             sdr_record_len,
                                             &entity_id_counts,
                                             NULL,
                                             sensor_name_buf,
                                             MAX_ENTITY_ID_SENSOR_NAME_STRING) < 0)
            goto cleanup;
          
          sensor_name = sensor_name_buf;
        }
      else
        {
          memset (device_id_string, '\0', IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING + 1);
          
          if (ipmi_sdr_parse_device_id_string (state_data->sdr_parse_ctx,
                                               sdr_record,
                                               sdr_record_len,
                                               device_id_string,
                                               IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_parse_device_id_string: %s\n",
                               ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
              goto cleanup;
            }
          
          sensor_name = device_id_string;
        }
      
      if (led_mode == IPMI_OEM_SUN_LED_MODE_OFF)
        led_mode_str = "Off";
      else if (led_mode == IPMI_OEM_SUN_LED_MODE_ON)
        led_mode_str = "On";
      else if (led_mode == IPMI_OEM_SUN_LED_MODE_STANDBY)
        led_mode_str = "Standby";
      else if (led_mode == IPMI_OEM_SUN_LED_MODE_SLOW)
        led_mode_str = "Slow";
      else if (led_mode == IPMI_OEM_SUN_LED_MODE_FAST)
        led_mode_str = "Fast";
      else
        led_mode_str = "Unknown";
      
      snprintf (fmt,
                IPMI_OEM_FMT_BUFLEN,
                "%%-%du | %%-%ds | %s\n",
                column_width.record_id,
                column_width.sensor_name,
                led_mode_str);
      
      pstdout_printf (state_data->pstate,
                      fmt,
                      record_id,
                      sensor_name);
    }
  
  
  rv = 0;
 cleanup:
  return (rv);
}
