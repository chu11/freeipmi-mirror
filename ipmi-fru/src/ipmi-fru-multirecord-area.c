/*****************************************************************************\
 *  $Id: ipmi-fru-multirecord-area.c,v 1.28.4.7 2009-04-17 21:45:20 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-fru.h"
#include "ipmi-fru-multirecord-area.h"
#include "ipmi-fru-util.h"

#include "freeipmi-portability.h"

#if 0

static char *
voltage_str (uint8_t voltage)
{
  if (voltage == IPMI_FRU_VOLTAGE_12V)
    return "12V";
  else if (voltage == IPMI_FRU_VOLTAGE_MINUS12V)
    return "-12V";
  else if (voltage == IPMI_FRU_VOLTAGE_5V)
    return "5V";
  else if (voltage == IPMI_FRU_VOLTAGE_3_3V)
    return "3.3V";
  else
    return "";
}

static int
ipmi_fru_output_power_supply_information (ipmi_fru_state_data_t *state_data,
                                 uint8_t *areabuf,
                                 uint8_t area_length)
{
  unsigned int overall_capacity;
  unsigned int peak_va;
  unsigned int inrush_current;
  unsigned int inrush_interval;
  unsigned int low_end_input_voltage_range_1;
  unsigned int high_end_input_voltage_range_1;
  unsigned int low_end_input_voltage_range_2;
  unsigned int high_end_input_voltage_range_2;
  unsigned int low_end_input_frequency_range;
  unsigned int high_end_input_frequency_range;
  unsigned int ac_dropout_tolerance;
  unsigned int predictive_fail_support;
  unsigned int power_factor_correction;
  unsigned int autoswitch;
  unsigned int hot_swap_support;
  unsigned int tachometer_pulses_per_rotation_predictive_fail_polarity;
  unsigned int peak_capacity;
  unsigned int hold_up_time;
  unsigned int voltage_1;
  unsigned int voltage_2;
  unsigned int total_combined_wattage;
  unsigned int predictive_fail_tachometer_lower_threshold;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Overall Capacity: %u Watts\n",
                  overall_capacity);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Peak VA: %u VA\n",
                  peak_va);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Max Inrush Current: %u Amps\n",
                  inrush_current);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Inrush Interval: %u ms\n",
                  inrush_interval);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Low End Input Voltage 1: %u mV\n",
                  low_end_input_voltage_range_1);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply High End Input Voltage 1: %u mV\n",
                  high_end_input_voltage_range_1);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Low End Input Voltage 2: %u mV\n",
                  low_end_input_voltage_range_2);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply High End Input Voltage 2: %u mV\n",
                  high_end_input_voltage_range_2);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Low End Acceptable Frequencey: %u Hz\n",
                  low_end_input_frequency_range);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply High End Acceptable Frequencey: %u Hz\n",
                  high_end_input_frequency_range);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply A/C Dropout Tolerance: %u ms\n",
                  ac_dropout_tolerance);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Predictive Fail Support: %s\n",
                  (predictive_fail_support) ? "Yes" : "No");
  if (predictive_fail_support)
    {
      if (tachometer_pulses_per_rotation_predictive_fail_polarity)
        {
          if (predictive_fail_tachometer_lower_threshold)
            pstdout_printf (state_data->pstate,
                            "  FRU Power Supply Predictive Fail: Tach output, two pulses per rotation\n");
          else
            pstdout_printf (state_data->pstate,
                            "  FRU Power Supply Predictive Fail: Pass/Fail predictive fail pin (0 = fail)\n");
        }
      else
        {
          if (predictive_fail_tachometer_lower_threshold)
            pstdout_printf (state_data->pstate,
                            "  FRU Power Supply Predictive Fail: Tach output, one pulse per rotation\n");
          else
            pstdout_printf (state_data->pstate,
                            "  FRU Power Supply Predictive Fail: Pass/Fail predictive fail pin (1 = fail)\n");
        }
    }
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Power Factor Correction Supported: %s\n",
                  (power_factor_correction) ? "Yes" : "No");
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply AutoSwitch Supprt: %s\n",
                  (autoswitch) ? "Yes" : "No");
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Hot Swap Support: %s\n",
                  (hot_swap_support) ? "Yes" : "No");

  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Peak Capacity: %u Watts\n",
                  peak_capacity);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Hold Up Time: %u s\n",
                  hold_up_time);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Voltage 1: %s\n",
                  voltage_str (voltage_1));
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Voltage 2: %s\n",
                  voltage_str (voltage_2));
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Total Combined Wattage: %u Watts\n",
                  total_combined_wattage);

  rv = 0;
 cleanup:
  return (rv);
}

static int
ipmi_fru_output_dc_output (ipmi_fru_state_data_t *state_data,
                  uint8_t *areabuf,
                  uint8_t area_length)
{
  unsigned int output_number;
  unsigned int standby;
  unsigned int nominal_voltage;
  unsigned int maximum_negative_voltage_deviation;
  unsigned int maximum_positive_voltage_deviation;
  unsigned int ripple_and_noise_pk_pk;
  unsigned int minimum_current_draw;
  unsigned int maximum_current_draw;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Output Number: %u\n",
                  output_number);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Output on Standy: %s\n",
                  (standby) ? "Yes" : "No");
  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Nominal Voltage: %d mV\n",
                  (int16_t)nominal_voltage);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Maximum Negative Voltage Deviation: %d mV\n",
                  (int16_t)maximum_negative_voltage_deviation);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Maximum Positive Voltage Deviation: %d mV\n",
                  (int16_t)maximum_positive_voltage_deviation);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Ripple and Noise pk-pk: %u mV\n",
                  ripple_and_noise_pk_pk);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Minimum Current Draw: %u mA\n",
                  minimum_current_draw);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Output Maximum Current Draw: %u mA\n",
                  maximum_current_draw);

  rv = 0;
 cleanup:
  return (rv);
}

static int
ipmi_fru_output_dc_load (ipmi_fru_state_data_t *state_data,
                uint8_t *areabuf,
                uint8_t area_length)
{
  unsigned int output_number;
  unsigned int nominal_voltage;
  unsigned int specd_minimum_voltage;
  unsigned int specd_maximum_voltage;
  unsigned int specd_ripple_and_noise_pk_pk;
  unsigned int minimum_current_load;
  unsigned int maximum_current_load;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);


  pstdout_printf (state_data->pstate,
                  "  FRU DC Load Output Number: %u\n",
                  output_number);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Load Nominal Voltage: %d mV\n",
                  (int16_t)nominal_voltage);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Load Spec'd Minimum Voltage: %d mV\n",
                  (int16_t)specd_minimum_voltage);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Load Spec'd Maximum Voltage: %d mV\n",
                  (int16_t)specd_maximum_voltage);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Load Spec'd Ripple and Noise pk-pk: %u mV\n",
                  specd_ripple_and_noise_pk_pk);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Load Minimum Current Load: %u mA\n",
                  minimum_current_load);
  pstdout_printf (state_data->pstate,
                  "  FRU DC Load Maximum Current Load: %u mA\n",
                  maximum_current_load);

  rv = 0;
 cleanup:
  return (rv);
}

static int
ipmi_fru_output_management_access_record (ipmi_fru_state_data_t *state_data,
                                 uint8_t *areabuf,
                                 uint8_t area_length)
{
  uint8_t sub_record_type;
  uint8_t sub_record_data[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  int rv = -1;
  int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (sub_record_data, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);


  if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_MANAGEMENT_URL)
    pstdout_printf (state_data->pstate,
                    "  FRU Management Access System Management URL: %s\n",
                    (char *)sub_record_data);
  else if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_NAME)
    pstdout_printf (state_data->pstate,
                    "  FRU Management Access System Name: %s\n",
                    (char *)sub_record_data);
  else if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_PING_ADDRESS)
    pstdout_printf (state_data->pstate,
                    "  FRU Management Access System Ping Address: %s\n",
                    (char *)sub_record_data);
  else if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_COMPONENT_MANAGEMENT_URL)
    pstdout_printf (state_data->pstate,
                    "  FRU Management Access Component Management URL: %s\n",
                    (char *)sub_record_data);
  else if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_COMPONENT_NAME)
    pstdout_printf (state_data->pstate,
                    "  FRU Management Access Component Name: %s\n",
                    (char *)sub_record_data);
  else if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_COMPONENT_PING_ADDRESS)
    pstdout_printf (state_data->pstate,
                    "  FRU Management Access Component Ping Address: %s\n",
                    (char *)sub_record_data);
  else if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_UNIQUE_ID)
    {
      pstdout_printf (state_data->pstate,
                      "  FRU Management Access System Unique ID:");

      for (i = 0; i < len; i++)
        {
          if (len > 8 && (i % 8) == 0)
            pstdout_printf (state_data->pstate, "\n    ");

          pstdout_printf (state_data->pstate,
                          " %02Xh",
                          sub_record_data[i]);
        }
      pstdout_printf (state_data->pstate, "\n");
    }
  else
    {
      pstdout_printf (state_data->pstate,
                      "  FRU Management Access Record: Unknown Sub Record Type:");
      for (i = 0; i < len; i++)
        {
          if (len > 8 && (i % 8) == 0)
            pstdout_printf (state_data->pstate, "\n    ");

          pstdout_printf (state_data->pstate,
                          " %02Xh",
                          sub_record_data[i]);
        }
      pstdout_printf (state_data->pstate, "\n");
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
ipmi_fru_output_base_compatibility_record (ipmi_fru_state_data_t *state_data,
                                  uint8_t *areabuf,
                                  uint8_t area_length)
{
  uint32_t manufacturer_id;
  unsigned int entity_id_code;
  unsigned int compatibility_base;
  unsigned int compatibility_code_start_value;
  uint8_t code_range_mask[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int code_range_mask_len = IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (code_range_mask, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);



  if (IPMI_IANA_ENTERPRISE_ID_VALID (manufacturer_id)
      && ipmi_iana_enterprise_numbers[manufacturer_id])
    pstdout_printf (state_data->pstate,
                    "  FRU Base Compatibility Manufacturer ID: %s (%Xh)\n",
                    ipmi_iana_enterprise_numbers[manufacturer_id],
                    manufacturer_id);
  else
    pstdout_printf (state_data->pstate,
                    "  FRU Base Compatibility Manufacturer ID: %Xh\n",
                    manufacturer_id);

  pstdout_printf (state_data->pstate,
                  "  FRU Base Compatibility Entity ID: %Xh\n",
                  entity_id_code);
  pstdout_printf (state_data->pstate,
                  "  FRU Base Compatibility Comptability Base: %Xh\n",
                  compatibility_base);
  pstdout_printf (state_data->pstate,
                  "  FRU Base Compatibility Comptability Code Start Value: %Xh\n",
                  compatibility_code_start_value);

  if (code_range_mask_len)
    {
      int i;

      pstdout_printf (state_data->pstate,
                      "  FRU Base Compatibility Code Mask:");

      for (i = 0; i < code_range_mask_len; i++)
        {
          if (code_range_mask_len > 8 && (i % 8) == 0)
            pstdout_printf (state_data->pstate, "\n    ");
          
          pstdout_printf (state_data->pstate,
                          " %02Xh",
                          code_range_mask[i]);
        }
      pstdout_printf (state_data->pstate, "\n");
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
ipmi_fru_output_extended_compatibility_record (ipmi_fru_state_data_t *state_data,
                                      uint8_t *areabuf,
                                      uint8_t area_length)
{
  uint32_t manufacturer_id;
  unsigned int entity_id_code;
  unsigned int compatibility_base;
  unsigned int compatibility_code_start_value;
  uint8_t code_range_mask[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int code_range_mask_len = IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (code_range_mask, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);

  pstdout_printf (state_data->pstate,
                  "  FRU Extended Compatibility Manufacturer ID: %Xh\n",
                  manufacturer_id);

  pstdout_printf (state_data->pstate,
                  "  FRU Extended Compatibility Entity ID: %Xh\n",
                  entity_id_code);
  pstdout_printf (state_data->pstate,
                  "  FRU Extended Compatibility Comptability Base: %Xh\n",
                  compatibility_base);
  pstdout_printf (state_data->pstate,
                  "  FRU Extended Compatibility Comptability Code Start Value: %Xh\n",
                  compatibility_code_start_value);

  if (code_range_mask_len)
    {
      int i;

      pstdout_printf (state_data->pstate,
                      "  FRU Extended Compatibility Code Mask:");

      for (i = 0; i < code_range_mask_len; i++)
        {
          if (code_range_mask_len > 8 && (i % 8) == 0)
            pstdout_printf (state_data->pstate, "\n    ");
          
          pstdout_printf (state_data->pstate,
                          " %02Xh",
                          code_range_mask[i]);
        }
      pstdout_printf (state_data->pstate, "\n");
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
ipmi_fru_output_oem_record (ipmi_fru_state_data_t *state_data,
                   uint8_t *areabuf,
                   uint8_t area_length)
{
  uint32_t manufacturer_id;
  uint8_t oem_data[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int oem_data_len = IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (oem_data, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);


  pstdout_printf (state_data->pstate,
                  "  FRU OEM Manufacturer ID: %Xh\n",
                  manufacturer_id);

  if (oem_data_len)
    {
      int i;

      pstdout_printf (state_data->pstate,
                      "  FRU OEM Code Mask:");

      for (i = 0; i < oem_data_len; i++)
        {
          if (oem_data_len > 8 && (i % 8) == 0)
            pstdout_printf (state_data->pstate, "\n    ");

          pstdout_printf (state_data->pstate,
                          " %02Xh",
                          oem_data[i]);
        }
      pstdout_printf (state_data->pstate, "\n");
    }

  rv = 0;
 cleanup:
  return (rv);
}

#endif
