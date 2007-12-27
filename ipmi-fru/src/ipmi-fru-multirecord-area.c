/*****************************************************************************\
 *  $Id: ipmi-fru-multirecord-area.c,v 1.5.6.1 2007-12-27 04:32:50 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-fru.h"
#include "ipmi-fru-multirecord-area.h"
#include "ipmi-fru-util.h"

#include "tool-fiid-wrappers.h"

static char *
voltage_str(uint8_t voltage)
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

static fru_err_t
output_power_supply_information(ipmi_fru_state_data_t *state_data,
                                uint8_t *frubuf,
                                unsigned int frusize,
                                unsigned int record_offset,
                                uint8_t record_length,
                                uint8_t record_checksum)
{
  fiid_obj_t obj_record = NULL;
  int32_t tmpl_record_length;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint64_t overall_capacity;
  uint64_t peak_va;
  uint64_t inrush_current;
  uint64_t inrush_interval;
  uint64_t low_end_input_voltage_range_1;
  uint64_t high_end_input_voltage_range_1;
  uint64_t low_end_input_voltage_range_2;
  uint64_t high_end_input_voltage_range_2;
  uint64_t low_end_input_frequency_range;
  uint64_t high_end_input_frequency_range;
  uint64_t ac_dropout_tolerance;
  uint64_t predictive_fail_support;
  uint64_t power_factor_correction;
  uint64_t autoswitch;
  uint64_t hot_swap_support;
  uint64_t tachometer_pulses_per_rotation_predictive_fail_polarity;
  uint64_t peak_capacity;
  uint64_t hold_up_time;
  uint64_t voltage_1;
  uint64_t voltage_2;
  uint64_t total_combined_wattage;
  uint64_t predictive_fail_tachometer_lower_threshold;

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(record_offset);
  assert(record_length);

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     record_offset,
                                     record_length,
                                     record_checksum,
                                     "Power Supply Multirecord")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  _FIID_TEMPLATE_LEN_BYTES (tmpl_record_length, tmpl_fru_power_supply_information);

  if (tmpl_record_length != record_length)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU Power Supply Record Length Incorrect: %u\n",
                      record_length);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_record, tmpl_fru_power_supply_information);

  _FIID_OBJ_SET_ALL_LEN(len, 
                        obj_record, 
                        &frubuf[record_offset], 
                        record_length);

  _FIID_OBJ_GET(obj_record,
                "overall_capacity",
                &overall_capacity);
  _FIID_OBJ_GET(obj_record,
                "peak_va",
                &peak_va);
  _FIID_OBJ_GET(obj_record,
                "inrush_current",
                &inrush_current);
  _FIID_OBJ_GET(obj_record,
                "inrush_interval",
                &inrush_interval);
  _FIID_OBJ_GET(obj_record,
                "low_end_input_voltage_range_1",
                &low_end_input_voltage_range_1);
  _FIID_OBJ_GET(obj_record,
                "high_end_input_voltage_range_1",
                &high_end_input_voltage_range_1);
  _FIID_OBJ_GET(obj_record,
                "low_end_input_voltage_range_2",
                &low_end_input_voltage_range_2);
  _FIID_OBJ_GET(obj_record,
                "high_end_input_voltage_range_2",
                &high_end_input_voltage_range_2);
  _FIID_OBJ_GET(obj_record,
                "low_end_input_frequency_range",
                &low_end_input_frequency_range);
  _FIID_OBJ_GET(obj_record,
                "high_end_input_frequency_range",
                &high_end_input_frequency_range);
  _FIID_OBJ_GET(obj_record,
                "ac_dropout_tolerance",
                &ac_dropout_tolerance);
  _FIID_OBJ_GET(obj_record,
                "predictive_fail_support",
                &predictive_fail_support);
  _FIID_OBJ_GET(obj_record,
                "power_factor_correction",
                &power_factor_correction);
  _FIID_OBJ_GET(obj_record,
                "autoswitch",
                &autoswitch);
  _FIID_OBJ_GET(obj_record,
                "hot_swap_support",
                &hot_swap_support);
  _FIID_OBJ_GET(obj_record,
                "tachometer_pulses_per_rotation_predictive_fail_polarity",
                &tachometer_pulses_per_rotation_predictive_fail_polarity);
  _FIID_OBJ_GET(obj_record,
                "peak_capacity",
                &peak_capacity);
  _FIID_OBJ_GET(obj_record,
                "hold_up_time",
                &hold_up_time);
  _FIID_OBJ_GET(obj_record,
                "voltage_1",
                &voltage_1);
  _FIID_OBJ_GET(obj_record,
                "voltage_2",
                &voltage_2);
  _FIID_OBJ_GET(obj_record,
                "total_combined_wattage",
                &total_combined_wattage);
  _FIID_OBJ_GET(obj_record,
                "predictive_fail_tachometer_lower_threshold",
                &predictive_fail_tachometer_lower_threshold); 

  pstdout_printf(state_data->pstate,
                 "  FRU Power Supply Overall Capacity: %u Watts\n",
                 overall_capacity);
  if (state_data->prog_data->args->verbose_count)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Peak VA: %u VA\n",
                     peak_va);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Max Inrush Current: %u Amps\n",
                     inrush_current);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Inrush Interval: %u ms\n",
                     inrush_interval);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Low End Input Voltage 1: %u mV\n",
                     low_end_input_voltage_range_1*10);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply High End Input Voltage 1: %u mV\n",
                     high_end_input_voltage_range_1*10);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Low End Input Voltage 2: %u mV\n",
                     low_end_input_voltage_range_2*10);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply High End Input Voltage 2: %u mV\n",
                     high_end_input_voltage_range_2*10);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Low End Acceptable Frequencey: %u Hz\n",
                     low_end_input_frequency_range);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply High End Acceptable Frequencey: %u Hz\n",
                     high_end_input_frequency_range);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply A/C Dropout Tolerance: %u ms\n",
                     ac_dropout_tolerance);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Predictive Fail Support: %s\n",
                     (predictive_fail_support) ? "Yes" : "No");
      if (predictive_fail_support)
        {
          if (tachometer_pulses_per_rotation_predictive_fail_polarity)
            {
              if (predictive_fail_tachometer_lower_threshold)
                pstdout_printf(state_data->pstate,
                               "  FRU Power Supply Predictive Fail: Tach output, two pulses per rotation\n");             
              else
                pstdout_printf(state_data->pstate,
                               "  FRU Power Supply Predictive Fail: Pass/Fail predictive fail pin (0 = fail)\n");
            }
          else
            {
              if (predictive_fail_tachometer_lower_threshold)
                pstdout_printf(state_data->pstate,
                               "  FRU Power Supply Predictive Fail: Tach output, one pulse per rotation\n");
              else
                pstdout_printf(state_data->pstate,
                               "  FRU Power Supply Predictive Fail: Pass/Fail predictive fail pin (1 = fail)\n");
            }
        }
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Power Factor Correction Supported: %s\n",
                     (power_factor_correction) ? "Yes" : "No");
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply AutoSwitch Supprt: %s\n",
                     (autoswitch) ? "Yes" : "No");
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Hot Swap Support: %s\n",
                     (hot_swap_support) ? "Yes" : "No");
      
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Peak Capacity: %u Watts\n",
                     peak_capacity);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Hold Up Time: %u s\n",
                     hold_up_time);
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Voltage 1: %s\n",
                     voltage_str(voltage_1));
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Voltage 2: %s\n",
                     voltage_str(voltage_2));
      pstdout_printf(state_data->pstate,
                     "  FRU Power Supply Total Combined Wattage: %u Watts\n",
                     total_combined_wattage);
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (obj_record)
    fiid_obj_destroy(obj_record);
  return rv;
}

static fru_err_t
output_dc_output(ipmi_fru_state_data_t *state_data,
                 uint8_t *frubuf,
                 unsigned int frusize,
                 unsigned int record_offset,
                 uint8_t record_length,
                 uint8_t record_checksum)
{
  fiid_obj_t obj_record = NULL;
  int32_t tmpl_record_length;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint64_t output_number;
  uint64_t standby;
  uint64_t nominal_voltage;
  uint64_t maximum_negative_voltage_deviation;
  uint64_t maximum_positive_voltage_deviation;
  uint64_t ripple_and_noise_pk_pk;
  uint64_t minimum_current_draw;
  uint64_t maximum_current_draw;

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(record_offset);
  assert(record_length);

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     record_offset,
                                     record_length,
                                     record_checksum,
                                     "DC Output Multirecord")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  _FIID_TEMPLATE_LEN_BYTES (tmpl_record_length, tmpl_fru_dc_output);

  if (tmpl_record_length != record_length)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU DC Output Record Length Incorrect: %u\n",
                      record_length);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_record, tmpl_fru_dc_output);

  _FIID_OBJ_SET_ALL_LEN(len, 
                        obj_record, 
                        &frubuf[record_offset], 
                        record_length);

  _FIID_OBJ_GET(obj_record,
                "output_number",
                &output_number);
  _FIID_OBJ_GET(obj_record,
                "standby",
                &standby);
  _FIID_OBJ_GET(obj_record,
                "nominal_voltage",
                &nominal_voltage);
  _FIID_OBJ_GET(obj_record,
                "maximum_negative_voltage_deviation",
                &maximum_negative_voltage_deviation);
  _FIID_OBJ_GET(obj_record,
                "maximum_positive_voltage_deviation",
                &maximum_positive_voltage_deviation);
  _FIID_OBJ_GET(obj_record,
                "ripple_and_noise_pk_pk",
                &ripple_and_noise_pk_pk);
  _FIID_OBJ_GET(obj_record,
                "minimum_current_draw",
                &minimum_current_draw);
  _FIID_OBJ_GET(obj_record,
                "maximum_current_draw",
                &maximum_current_draw);

  if (state_data->prog_data->args->verbose_count)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU DC Output Output Number: %u\n",
                     output_number);
      pstdout_printf(state_data->pstate,
                     "  FRU DC Output Output on Standy: %s\n",
                     (standby) ? "Yes" : "No");
    }
  pstdout_printf(state_data->pstate,
                 "  FRU DC Output Nominal Voltage: %d mV\n",
                 (int16_t)nominal_voltage);
  if (state_data->prog_data->args->verbose_count)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU DC Output Maximum Negative Voltage Deviation: %d mV\n",
                     (int16_t)maximum_negative_voltage_deviation);
      pstdout_printf(state_data->pstate,
                     "  FRU DC Output Maximum Positive Voltage Deviation: %d mV\n",
                     (int16_t)maximum_positive_voltage_deviation);
      pstdout_printf(state_data->pstate,
                     "  FRU DC Output Ripple and Noise pk-pk: %u mV\n",
                     ripple_and_noise_pk_pk);
    }
  pstdout_printf(state_data->pstate,
                 "  FRU DC Output Minimum Current Draw: %u mA\n",
                 minimum_current_draw);
  pstdout_printf(state_data->pstate,
                 "  FRU DC Output Maximum Current Draw: %u mA\n",
                 maximum_current_draw);

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (obj_record)
    fiid_obj_destroy(obj_record);
  return rv;
}

static fru_err_t
output_dc_load(ipmi_fru_state_data_t *state_data,
               uint8_t *frubuf,
               unsigned int frusize,
               unsigned int record_offset,
               uint8_t record_length,
               uint8_t record_checksum)
{
  fiid_obj_t obj_record = NULL;
  int32_t tmpl_record_length;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint64_t output_number;
  uint64_t nominal_voltage;
  uint64_t specd_minimum_voltage;
  uint64_t specd_maximum_voltage;
  uint64_t specd_ripple_and_noise_pk_pk;
  uint64_t minimum_current_load;
  uint64_t maximum_current_load;

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(record_offset);
  assert(record_length);

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     record_offset,
                                     record_length,
                                     record_checksum,
                                     "DC Load Multirecord")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  _FIID_TEMPLATE_LEN_BYTES (tmpl_record_length, tmpl_fru_dc_load);

  if (tmpl_record_length != record_length)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU DC Load Record Length Incorrect: %u\n",
                      record_length);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_record, tmpl_fru_dc_load);

  _FIID_OBJ_SET_ALL_LEN(len, 
                        obj_record, 
                        &frubuf[record_offset], 
                        record_length);

  _FIID_OBJ_GET(obj_record,
                "output_number",
                &output_number);
  _FIID_OBJ_GET(obj_record,
                "nominal_voltage",
                &nominal_voltage);
  _FIID_OBJ_GET(obj_record,
                "specd_minimum_voltage",
                &specd_minimum_voltage);
  _FIID_OBJ_GET(obj_record,
                "specd_maximum_voltage",
                &specd_maximum_voltage);
  _FIID_OBJ_GET(obj_record,
                "specd_ripple_and_noise_pk_pk",
                &specd_ripple_and_noise_pk_pk);
  _FIID_OBJ_GET(obj_record,
                "minimum_current_load",
                &minimum_current_load);
  _FIID_OBJ_GET(obj_record,
                "maximum_current_load",
                &maximum_current_load);

  if (state_data->prog_data->args->verbose_count)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU DC Load Output Number: %u\n",
                     output_number);
    }
  pstdout_printf(state_data->pstate,
                 "  FRU DC Load Nominal Voltage: %d mV\n",
                 (int16_t)nominal_voltage);
  if (state_data->prog_data->args->verbose_count)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU DC Load Spec'd Minimum Voltage: %d mV\n",
                     (int16_t)specd_minimum_voltage);
      pstdout_printf(state_data->pstate,
                     "  FRU DC Load Spec'd Maximum Voltage: %d mV\n",
                     (int16_t)specd_maximum_voltage);
      pstdout_printf(state_data->pstate,
                     "  FRU DC Load Spec'd Ripple and Noise pk-pk: %u mV\n",
                     specd_ripple_and_noise_pk_pk);
    }
  pstdout_printf(state_data->pstate,
                 "  FRU DC Load Minimum Current Load: %u mA\n",
                 minimum_current_load);
  pstdout_printf(state_data->pstate,
                 "  FRU DC Load Maximum Current Load: %u mA\n",
                 maximum_current_load);


  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (obj_record)
    fiid_obj_destroy(obj_record);
  return rv;
}

static fru_err_t
output_management_access_record(ipmi_fru_state_data_t *state_data,
                                uint8_t *frubuf,
                                unsigned int frusize,
                                unsigned int record_offset,
                                uint8_t record_length,
                                uint8_t record_checksum)
{
  fiid_obj_t obj_record = NULL;
  int32_t min_tmpl_record_length;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint64_t sub_record_type;
  uint8_t managementaccessbuf[FRU_BUF_LEN+1];
  int i;

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(record_offset);
  assert(record_length);

  memset(managementaccessbuf, '\0', FRU_BUF_LEN+1);

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     record_offset,
                                     record_length,
                                     record_checksum,
                                     "Management Access Multirecord")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  /* Variable Length Record - So need to check min length */
  _FIID_TEMPLATE_FIELD_START_BYTES (min_tmpl_record_length, 
                                    tmpl_fru_management_access_record,
                                    "record");
  
  if (record_length >= min_tmpl_record_length)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU Management Access Record Length Incorrect: %u\n",
                      record_length);
      goto cleanup;
    }
 
  _FIID_OBJ_CREATE(obj_record, tmpl_fru_management_access_record);

  _FIID_OBJ_SET_ALL_LEN(len, 
                        obj_record, 
                        &frubuf[record_offset], 
                        record_length);

  _FIID_OBJ_GET(obj_record,
                "sub_record_type",
                &sub_record_type);

  _FIID_OBJ_GET_DATA(len,
                     obj_record,
                     "record",
                     managementaccessbuf,
                     FRU_BUF_LEN);

  if (!len)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU Management Access Record: No Record Bytes\n");
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (sub_record_type == IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_MANAGEMENT_URL)
    pstdout_printf(state_data->pstate,
                   "  FRU Management Access System Management URL: %s\n",
                   managementaccessbuf);
  else if (IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_NAME)
    pstdout_printf(state_data->pstate,
                   "  FRU Management Access System Name: %s\n",
                   managementaccessbuf);
  else if (IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_PING_ADDRESS)
    {
      if (state_data->prog_data->args->verbose_count)
        {
          pstdout_printf(state_data->pstate,
                         "  FRU Management Access System Ping Address: %s\n",
                         managementaccessbuf);
        }
    }
  else if (IPMI_FRU_SUB_RECORD_TYPE_COMPONENT_MANAGEMENT_URL)
    pstdout_printf(state_data->pstate,
                   "  FRU Management Access Component Management URL: %s\n",
                   managementaccessbuf);
  else if (IPMI_FRU_SUB_RECORD_TYPE_COMPONENT_NAME)
    pstdout_printf(state_data->pstate,
                   "  FRU Management Access Component Name: %s\n",
                   managementaccessbuf);
  else if (IPMI_FRU_SUB_RECORD_TYPE_COMPONENT_PING_ADDRESS)
    {
      if (state_data->prog_data->args->verbose_count)
        {
          pstdout_printf(state_data->pstate,
                         "  FRU Management Access Component Ping Address: %s\n",
                         managementaccessbuf);
        }
    }
  else if (IPMI_FRU_SUB_RECORD_TYPE_SYSTEM_UNIQUE_ID)
    {
      if (state_data->prog_data->args->verbose_count)
        {          
          pstdout_printf(state_data->pstate,
                         "  FRU Management Access System Unique ID:");
          
          for (i = 0; i < len; i++)
            {
              if ((i % 8) == 0)
                pstdout_printf(state_data->pstate, "\n    ");
              
              pstdout_printf(state_data->pstate,
                             " 0x%02X",
                             managementaccessbuf[i]);
            }
          pstdout_printf(state_data->pstate, "\n");
        }
    }
  else
    {
      if (state_data->prog_data->args->verbose_count)
        {
          pstdout_printf(state_data->pstate,
                         "  FRU Management Access Record: Unknown Sub Record Type:\n");
          for (i = 0; i < len; i++)
            {
              if ((i % 8) == 0)
                pstdout_printf(state_data->pstate, "\n    ");
              
              pstdout_printf(state_data->pstate,
                             " 0x%02X",
                             managementaccessbuf[i]);
            }
          pstdout_printf(state_data->pstate, "\n");
        }
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (obj_record)
    fiid_obj_destroy(obj_record);
  return rv;
}

static fru_err_t
output_base_compatibility_record(ipmi_fru_state_data_t *state_data,
                                 uint8_t *frubuf,
                                 unsigned int frusize,
                                 unsigned int record_offset,
                                 uint8_t record_length,
                                 uint8_t record_checksum)
{ 
  fiid_obj_t obj_record = NULL;
  int32_t min_tmpl_record_length;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint64_t manufacturer_id;
  uint64_t entity_id_code;
  uint64_t compatibility_base;
  uint64_t compatibility_code_start_value;
  uint8_t codemaskbuf[FRU_BUF_LEN+1];

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(record_offset);
  assert(record_length);

  memset(codemaskbuf, '\0', FRU_BUF_LEN+1);

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     record_offset,
                                     record_length,
                                     record_checksum,
                                     "Base Compatibility Multirecord")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  /* Variable Length Record - So need to check min length */
  _FIID_TEMPLATE_FIELD_START_BYTES (min_tmpl_record_length, 
                                    tmpl_fru_base_compatibility_record,
                                    "code_range_mask");

  if (record_length >= min_tmpl_record_length)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU Base Compatibility Record Length Incorrect: %u\n",
                      record_length);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_record, tmpl_fru_base_compatibility_record);

  _FIID_OBJ_SET_ALL_LEN(len, 
                        obj_record, 
                        &frubuf[record_offset], 
                        record_length);

  _FIID_OBJ_GET(obj_record,
                "manufacturer_id",
                &manufacturer_id);

  _FIID_OBJ_GET(obj_record,
                "entity_id_code",
                &entity_id_code);

  _FIID_OBJ_GET(obj_record,
                "compatibility_base",
                &compatibility_base);

  _FIID_OBJ_GET(obj_record,
                "compatibility_code_start_value",
                &compatibility_code_start_value);

  _FIID_OBJ_GET_DATA(len,
                     obj_record,
                     "code_range_mask",
                     codemaskbuf,
                     FRU_BUF_LEN);

  pstdout_printf(state_data->pstate,
                 "  FRU Base Compatibility Manufacturer ID: 0x%X\n",
                 manufacturer_id);
  if (state_data->prog_data->args->verbose_count)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU Base Compatibility Entity ID: 0x%X\n",
                     entity_id_code);
      pstdout_printf(state_data->pstate,
                     "  FRU Base Compatibility Comptability Base: 0x%X\n",
                     compatibility_base);
      pstdout_printf(state_data->pstate,
                     "  FRU Base Compatibility Comptability Code Start Value: 0x%X\n",
                     compatibility_code_start_value);
      
      if (len)
        {
          int i;
          
          pstdout_printf(state_data->pstate,
                         "  FRU Base Compatibility Code Mask:");
          
          for (i = 0; i < len; i++)
            {
              if ((i % 8) == 0)
                pstdout_printf(state_data->pstate, "\n    ");
              
              pstdout_printf(state_data->pstate,
                             " 0x%02X",
                             codemaskbuf[i]);
            }
          pstdout_printf(state_data->pstate, "\n");
        }
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (obj_record)
    fiid_obj_destroy(obj_record);
  return rv;
}

static fru_err_t
output_extended_compatibility_record(ipmi_fru_state_data_t *state_data,
                                     uint8_t *frubuf,
                                     unsigned int frusize,
                                     unsigned int record_offset,
                                     uint8_t record_length,
                                     uint8_t record_checksum)
{ 
  fiid_obj_t obj_record = NULL;
  int32_t min_tmpl_record_length;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint64_t manufacturer_id;
  uint64_t entity_id_code;
  uint64_t compatibility_base;
  uint64_t compatibility_code_start_value;
  uint8_t codemaskbuf[FRU_BUF_LEN+1];

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(record_offset);
  assert(record_length);

  memset(codemaskbuf, '\0', FRU_BUF_LEN+1);

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     record_offset,
                                     record_length,
                                     record_checksum,
                                     "Extended Compatibility Multirecord")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  /* Variable Length Record - So need to check min length */
  _FIID_TEMPLATE_FIELD_START_BYTES (min_tmpl_record_length, 
                                    tmpl_fru_extended_compatibility_record,
                                    "code_range_mask");

  if (record_length >= min_tmpl_record_length)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU Extended Compatibility Record Length Incorrect: %u\n",
                      record_length);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_record, tmpl_fru_extended_compatibility_record);

  _FIID_OBJ_SET_ALL_LEN(len, 
                        obj_record, 
                        &frubuf[record_offset], 
                        record_length);

  _FIID_OBJ_GET(obj_record,
                "manufacturer_id",
                &manufacturer_id);

  _FIID_OBJ_GET(obj_record,
                "entity_id_code",
                &entity_id_code);

  _FIID_OBJ_GET(obj_record,
                "compatibility_base",
                &compatibility_base);

  _FIID_OBJ_GET(obj_record,
                "compatibility_code_start_value",
                &compatibility_code_start_value);

  _FIID_OBJ_GET_DATA(len,
                     obj_record,
                     "code_range_mask",
                     codemaskbuf,
                     FRU_BUF_LEN);

  pstdout_printf(state_data->pstate,
                 "  FRU Extended Compatibility Manufacturer ID: 0x%X\n",
                 manufacturer_id);

  if (state_data->prog_data->args->verbose_count)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU Extended Compatibility Entity ID: 0x%X\n",
                     entity_id_code);
      pstdout_printf(state_data->pstate,
                     "  FRU Extended Compatibility Comptability Base: 0x%X\n",
                     compatibility_base);
      pstdout_printf(state_data->pstate,
                     "  FRU Extended Compatibility Comptability Code Start Value: 0x%X\n",
                     compatibility_code_start_value);
      
      if (len)
        {
          int i;
          
          pstdout_printf(state_data->pstate,
                         "  FRU Extended Compatibility Code Mask:");

          for (i = 0; i < len; i++)
            {
              if ((i % 8) == 0)
                pstdout_printf(state_data->pstate, "\n    ");
              
              pstdout_printf(state_data->pstate,
                             " 0x%02X",
                             codemaskbuf[i]);
            }
          pstdout_printf(state_data->pstate, "\n");
        }
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (obj_record)
    fiid_obj_destroy(obj_record);
  return rv;
}

static fru_err_t
output_oem_record(ipmi_fru_state_data_t *state_data,
                  uint8_t *frubuf,
                  unsigned int frusize,
                  unsigned int record_offset,
                  uint8_t record_length,
                  uint8_t record_checksum)
{ 
  fiid_obj_t obj_record = NULL;
  int32_t min_tmpl_record_length;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint64_t manufacturer_id;
  uint8_t oemdatabuf[FRU_BUF_LEN+1];

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(record_offset);
  assert(record_length);

  memset(oemdatabuf, '\0', FRU_BUF_LEN+1);

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     record_offset,
                                     record_length,
                                     record_checksum,
                                     "OEM Multirecord")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  /* Variable Length Record - So need to check min length */
  _FIID_TEMPLATE_FIELD_START_BYTES (min_tmpl_record_length, 
                                    tmpl_fru_oem_record,
                                    "oem_data");

  if (record_length >= min_tmpl_record_length)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU OEM Record Length Incorrect: %u\n",
                      record_length);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_record, tmpl_fru_oem_record);

  _FIID_OBJ_SET_ALL_LEN(len,
                        obj_record, 
                        &frubuf[record_offset], 
                        record_length);

  _FIID_OBJ_GET(obj_record,
                "manufacturer_id",
                &manufacturer_id);

  _FIID_OBJ_GET_DATA(len,
                     obj_record,
                     "oem_data",
                     oemdatabuf,
                     FRU_BUF_LEN);

  pstdout_printf(state_data->pstate,
                 "  FRU OEM Manufacturer ID: 0x%X\n",
                 manufacturer_id);

  if (state_data->prog_data->args->verbose_count)
    {
      if (len)
        {
          int i;
          
          pstdout_printf(state_data->pstate,
                         "  FRU OEM Code Mask:");
          
          for (i = 0; i < len; i++)
            {
              if ((i % 8) == 0)
                pstdout_printf(state_data->pstate, "\n    ");
              
              pstdout_printf(state_data->pstate,
                             " 0x%02X",
                             oemdatabuf[i]);
            }
          pstdout_printf(state_data->pstate, "\n");
        }
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (obj_record)
    fiid_obj_destroy(obj_record);
  return rv;
}

fru_err_t
ipmi_fru_output_multirecord_info_area(ipmi_fru_state_data_t *state_data,
                                      uint8_t *frubuf,
                                      unsigned int frusize,
                                      unsigned int offset)
{
  fiid_obj_t fru_multirecord_header = NULL;
  int32_t record_header_length;
  uint64_t record_type_id;
  uint64_t end_of_list;
  uint64_t record_format_version;
  uint64_t record_length;
  uint64_t record_checksum;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;
  uint32_t multirecord_offset = 0;

  assert(state_data);
  assert(frubuf);
  assert(frusize);
  assert(offset);

  _FIID_TEMPLATE_LEN_BYTES(record_header_length, tmpl_fru_multirecord_area_header);
  
  /* Offset is in multiples of 8 */
  if (frusize < (offset*8 + record_header_length))
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU MultirecordInfo Area size too small\n");
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(fru_multirecord_header, tmpl_fru_multirecord_area_header);

  multirecord_offset = offset*8;
  while (multirecord_offset < frusize)
    {
      _FIID_OBJ_SET_ALL_LEN(len, 
                            fru_multirecord_header, 
                            &frubuf[multirecord_offset], 
                            frusize - multirecord_offset);
      
      _FIID_OBJ_GET (fru_multirecord_header,
                     "record_type_id",
                     &record_type_id);
      _FIID_OBJ_GET (fru_multirecord_header,
                     "record_format_version",
                     &record_format_version);
      _FIID_OBJ_GET (fru_multirecord_header,
                     "end_of_list",
                     &end_of_list);
      _FIID_OBJ_GET (fru_multirecord_header,
                     "record_length",
                     &record_length);
      _FIID_OBJ_GET (fru_multirecord_header,
                     "record_checksum",
                     &record_checksum);

      if (state_data->prog_data->args->verbose_count >= 2)
        {
          pstdout_printf(state_data->pstate, 
                         "  FRU Multirecord Info Area Record Type ID: 0x%02X\n",
                         record_type_id);
          pstdout_printf(state_data->pstate, 
                         "  FRU Multirecord Info Area Record Format Version: 0x%02X\n",
                         record_format_version);
          pstdout_printf(state_data->pstate, 
                         "  FRU Multirecord Info Area End Of List: 0x%02X\n",
                         end_of_list);
          pstdout_printf(state_data->pstate,
                         "  FRU Multirecord Info Area Record Length: %u\n", 
                         record_length);
        }
           
      if (end_of_list)
        break;

      if (record_format_version != IPMI_FRU_MULTIRECORD_INFO_AREA_FORMAT_VERSION)
        {
          pstdout_fprintf(state_data->pstate, 
                          stderr,
                          "  FRU Multirecord Area Format Unknown: 0x%02X\n", 
                          record_format_version);
          continue;
        }

      if (!record_length)
        {
          multirecord_offset += record_header_length;
          continue;
        }
      
      /* Note: Unlike Info Areas, record_length is in bytes */
      if (frusize < (multirecord_offset + record_length))
        {
          pstdout_fprintf(state_data->pstate, 
                          stderr,
                          "  FRU Multirecord Info Area too small\n");
          rv = FRU_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if ((ret = ipmi_fru_dump_hex(state_data,
                                   frubuf,
                                   frusize,
                                   multirecord_offset,
                                   record_header_length + record_length,
                                   "MultiRecord")) != FRU_ERR_SUCCESS)
        {
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          multirecord_offset += record_header_length;
          continue;
        }

      if ((ret = ipmi_fru_check_checksum(state_data,
                                         frubuf,
                                         frusize,
                                         multirecord_offset,
                                         record_header_length,
                                         0,
                                         "Multirecord Record Header")) != FRU_ERR_SUCCESS)
        {
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          multirecord_offset += record_header_length;
          continue;
        }

      multirecord_offset += record_header_length;

      if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_POWER_SUPPLY_INFORMATION)
        {
          ret = output_power_supply_information(state_data,
                                                frubuf,
                                                frusize,
                                                multirecord_offset,
                                                record_length,
                                                record_checksum);
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          /* else continue on */
          pstdout_printf(state_data->pstate, "\n");
        }
      else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_DC_OUTPUT)
        {
          ret = output_dc_output(state_data,
                                 frubuf,
                                 frusize,
                                 multirecord_offset,
                                 record_length,
                                 record_checksum);
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          /* else continue on */
          pstdout_printf(state_data->pstate, "\n");
        }
      else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_DC_LOAD)
        {
          ret = output_dc_load(state_data,
                               frubuf,
                               frusize,
                               multirecord_offset,
                               record_length,
                               record_checksum);
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          /* else continue on */
          pstdout_printf(state_data->pstate, "\n");
        }
      else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_MANAGEMENT_ACCESS_RECORD)
        {
          ret = output_management_access_record(state_data,
                                                frubuf,
                                                frusize,
                                                multirecord_offset,
                                                record_length,
                                                record_checksum);
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          /* else continue on */
          pstdout_printf(state_data->pstate, "\n");
        }
      else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_BASE_COMPATIBILITY_RECORD)
        {
          ret = output_base_compatibility_record(state_data,
                                                 frubuf,
                                                 frusize,
                                                 multirecord_offset,
                                                 record_length,
                                                 record_checksum);
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          /* else continue on */
          pstdout_printf(state_data->pstate, "\n");
        }
      else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_EXTENDED_COMPATIBILITY_RECORD)
        {
          ret = output_extended_compatibility_record(state_data,
                                                     frubuf,
                                                     frusize,
                                                     multirecord_offset,
                                                     record_length,
                                                     record_checksum);
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          /* else continue on */
          pstdout_printf(state_data->pstate, "\n");
        }
      else if (IPMI_FRU_MULTIRECORD_AREA_TYPE_IS_OEM(record_type_id))
        {
          ret = output_oem_record(state_data,
                                  frubuf,
                                  frusize,
                                  multirecord_offset,
                                  record_length,
                                  record_checksum);
          if (ret == FRU_ERR_FATAL_ERROR)
            {
              rv = ret;
              goto cleanup;
            }
          /* else continue on */
          pstdout_printf(state_data->pstate, "\n");
        }
      else
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "  FRU Multirecord Record ID Type Unknown: 0x%02X\n",
                        record_type_id);

      multirecord_offset += record_length;
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (fru_multirecord_header)
    fiid_obj_destroy(fru_multirecord_header);
  return (rv);

}
