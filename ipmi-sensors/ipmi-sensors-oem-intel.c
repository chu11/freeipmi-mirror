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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-sensors.h"
#include "ipmi-sensors-oem-intel.h"
#include "ipmi-sensors-oem-intel-node-manager.h"
#include "ipmi-sensors-output-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sensor-common.h"

static void
_ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (ipmi_sensors_state_data_t *state_data,
								       uint8_t fan_control_profile_support,
								       unsigned int fan_control_profile_number)
{
  assert (state_data);
  assert (state_data->prog_data->args->verbose_count >= 2);
  assert (state_data->prog_data->args->interpret_oem_data);
  assert (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);

  pstdout_printf (state_data->pstate,
		  "Fan Control Profile %u Support: %s\n",
		  fan_control_profile_number,
		  fan_control_profile_support == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_VALID ? "Yes" : "No");
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
ipmi_sensors_oem_intel_output_oem_record (ipmi_sensors_state_data_t *state_data,
					  uint32_t oem_record_manufacturer_id,
					  const uint8_t *oem_data,
					  unsigned int oem_data_len)
{
  int ret;

  assert (state_data);
  assert (oem_data);
  assert (oem_data_len);
  assert (state_data->prog_data->args->verbose_count >= 2);
  assert (state_data->prog_data->args->interpret_oem_data);
  assert (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  
  /*
   * Intel S5500WB/Penguin Computing Relion 700
   * Intel S2600JF/Appro 512X
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_S5500WB
      || state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_S2600JF
      || state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = ipmi_sensors_oem_intel_node_manager_output_oem_record (state_data)) < 0)
	return (-1);

      if (ret)
	return (1);
    }
  
  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard maintains Intel manufacturer ID)
   */
  if (state_data->oem_data.product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if (oem_record_manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL)
	{
	  uint8_t record_subtype;

	  record_subtype = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_SUBTYPE_INDEX];

	  if (record_subtype == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_SUBTYPE
	      && oem_data_len >= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_OEM_DATA_LENGTH)
	    {
	      uint8_t memory_throttling_mode;
	      uint8_t fan_control_profile_support_0;
	      uint8_t fan_control_profile_support_1;
	      uint8_t fan_control_profile_support_2;
	      uint8_t fan_control_profile_support_3;
	      uint8_t fan_control_profile_support_4;
	      uint8_t fan_control_profile_support_5;
	      uint8_t fan_control_profile_support_6;
	      uint8_t fan_control_profile_support_7;
	      uint8_t tempinlet;
	      uint8_t temprise;
	      uint16_t airflow;
	      uint16_t dimmpitch;
	      uint8_t throttle_mode;
	      uint8_t thermal_register_lock;
	      uint8_t hysteresis;
	      uint8_t control_event_mode;
	      char *memory_throttling_mode_str;
	      char *throttle_mode_str;
	      char *hysteresis_str;
	      char *control_event_mode_str;
	      
	      memory_throttling_mode = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_MEMORY_THROTTLING_MODE_INDEX];
	      memory_throttling_mode &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_MEMORY_THROTTLING_MODE_BITMASK;
	      memory_throttling_mode >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_MEMORY_THROTTLING_MODE_SHIFT;

	      fan_control_profile_support_0 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_0 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE0_BITMASK;
	      fan_control_profile_support_0 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE0_SHIFT;

	      fan_control_profile_support_1 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_1 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE1_BITMASK;
	      fan_control_profile_support_1 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE1_SHIFT;

	      fan_control_profile_support_2 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_2 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE2_BITMASK;
	      fan_control_profile_support_2 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE2_SHIFT;

	      fan_control_profile_support_3 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_3 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE3_BITMASK;
	      fan_control_profile_support_3 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE3_SHIFT;

	      fan_control_profile_support_4 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_4 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE4_BITMASK;
	      fan_control_profile_support_4 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE4_SHIFT;

	      fan_control_profile_support_5 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_5 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE5_BITMASK;
	      fan_control_profile_support_5 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE5_SHIFT;

	      fan_control_profile_support_6 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_6 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE6_BITMASK;
	      fan_control_profile_support_6 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE6_SHIFT;

	      fan_control_profile_support_7 = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE_INDEX];
	      fan_control_profile_support_7 &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE7_BITMASK;
	      fan_control_profile_support_7 >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_FAN_PROFILE7_SHIFT;

	      tempinlet = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_TEMPINLET_INDEX];

	      temprise = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_TEMPRISE_INDEX];

	      airflow = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_AIRFLOW_INDEX_START];
	      airflow |= (oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_AIRFLOW_INDEX_START + 1] << 8); 

	      dimmpitch = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_DIMMPITCH_INDEX_START];
	      dimmpitch |= (oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_DIMMPITCH_INDEX_START + 1] << 8); 

	      throttle_mode = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THROTTLE_MODE_INDEX];
	      throttle_mode &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THROTTLE_MODE_BITMASK;
	      throttle_mode >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THROTTLE_MODE_SHIFT;

	      thermal_register_lock = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THERMAL_REGISTER_LOCK_INDEX];
	      thermal_register_lock &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THERMAL_REGISTER_LOCK_BITMASK;
	      thermal_register_lock >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THERMAL_REGISTER_LOCK_SHIFT;

	      hysteresis = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_HYSTERESIS_INDEX];
	      hysteresis &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_HYSTERESIS_BITMASK;
	      hysteresis >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_HYSTERESIS_SHIFT;

	      control_event_mode = oem_data[IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_CONTROL_EVENT_MODE_INDEX];
	      control_event_mode &= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_CONTROL_EVENT_MODE_BITMASK;
	      control_event_mode >>= IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_CONTROL_EVENT_MODE_SHIFT;

	      if (memory_throttling_mode == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_MEMORY_THROTTLING_MODE_NONE_SUPPORTED)
		memory_throttling_mode_str = "None Supported";
	      else if (memory_throttling_mode == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_MEMORY_THROTTLING_MODE_OPEN_LOOP_THROUGHPUT_THROTTLING)
		memory_throttling_mode_str = "Open-loop throughput throttling (OLTT)";
	      else if (memory_throttling_mode == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_MEMORY_THROTTLING_MODE_CLOSE_LOOP_THERMAL_THROTTLING)
		memory_throttling_mode_str = "Close-loop thermal throttling (CLTT)";
	      else
		memory_throttling_mode_str = "Unspecified";

	      pstdout_printf (state_data->pstate,
			      "Memory Throttling Mode: %s\n",
			      memory_throttling_mode_str);

	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_0, 0);
	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_1, 1);
	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_2, 2);
	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_3, 3);
	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_4, 4);
	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_5, 5);
	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_6, 6);
	      _ipmi_sensors_oem_intel_output_oem_record_fan_control_profile_support (state_data, fan_control_profile_support_7, 7);
	  
	      /* Stored in .5 C units */
	      pstdout_printf (state_data->pstate,
			      "Temperature at Chassis Inlet: %u C\n",
			      tempinlet/2);
	  
	      /* Stored in .5 C units */
	      pstdout_printf (state_data->pstate,
			      "Temperature rise from Chassis Inlet to DIMM Local Ambient: %u C\n",
			      temprise/2);

	      /* Stored in mm/sec units */
	      pstdout_printf (state_data->pstate,
			      "Average air flow velocity in DIMM channel: %u m/sec\n", 
			      (float)airflow/1000);

	      /* Stored in 1/1000 in units */
	      pstdout_printf (state_data->pstate,
			      "Pitch between DIMMS: %.2f in\n", 
			      (float)dimmpitch/1000);

	      if (throttle_mode == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THROTTLE_MODE_DISABLED)
		throttle_mode_str = "Disabled";
	      else if (throttle_mode == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THROTTLE_MODE_VTS_ONLY)
		throttle_mode_str = "VTS Only (OLTT)";
	      else if (throttle_mode == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_THROTTLE_MODE_SOFTWARE_MODE)
		throttle_mode_str = "Software Mode";
	      else 
		throttle_mode_str = "EXTTS CLTT";

	      pstdout_printf (state_data->pstate,
			      "Throttle Mode: %s\n",
			      throttle_mode_str);

	      pstdout_printf (state_data->pstate,
			      "Thermal Register Lock: %s\n",
			      thermal_register_lock ? "Enabled" : "Disabled");

	      if (hysteresis == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_HYSTERESIS_DISABLE)
		hysteresis_str = "disable hysteresis";
	      else if (hysteresis == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_HYSTERESIS_1_5C)
		hysteresis_str = "1.5C";
	      else if (hysteresis == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_HYSTERESIS_3C)
		hysteresis_str = "3C";
	      else 
		hysteresis_str = "6C";
	    
	      pstdout_printf (state_data->pstate,
			      "Hysteresis: %s\n",
			      hysteresis_str);

	      if (control_event_mode == IPMI_SDR_OEM_INTEL_QUANTA_QSSC_S4R_THERMAL_PROFILE_DATA_RECORD_CONTROL_EVENT_MODE_ASSERT_NOT_ONLY_CRITICAL)
		control_event_mode_str = "Events asserted above high or low in addition to critical";
	      else 
		control_event_mode_str = "Events asserted only if above critical";

	      pstdout_printf (state_data->pstate,
			      "Control Event Mode: %s\n",
			      control_event_mode_str);

	      return (1);
	    }
	}
    }

  return (0);
}
