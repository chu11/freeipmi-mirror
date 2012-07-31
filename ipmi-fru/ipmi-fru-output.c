/*****************************************************************************\
 *  $Id: ipmi-fru-output.c,v 1.8 2010-03-02 21:09:07 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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

#include <freeipmi/freeipmi.h>

#include "ipmi-fru.h"
#include "ipmi-fru-output.h"
#include "ipmi-fru-oem-wistron.h"

#include "freeipmi-portability.h"

/* haven't seen a motherboard with more than 2-3 so far, 64 should be more than enough */
#define IPMI_FRU_CUSTOM_FIELDS 64

#define IPMI_FRU_STR_BUFLEN    1024

int
_output_field (ipmi_fru_state_data_t *state_data,
               uint8_t language_code,
               ipmi_fru_parse_field_t *field,
               char *str)
{
  char strbuf[IPMI_FRU_PARSE_AREA_STRING_MAX + 1];
  unsigned int strbuflen = IPMI_FRU_PARSE_AREA_STRING_MAX;

  assert (state_data);
  assert (field);
  assert (str);

  if (!field->type_length_field_length)
    return (0);

  memset (strbuf, '\0', IPMI_FRU_PARSE_AREA_STRING_MAX + 1);
      
  if (ipmi_fru_parse_type_length_field_to_string (state_data->fru_parse_ctx,
                                                  field->type_length_field,
                                                  field->type_length_field_length,
                                                  language_code,
                                                  strbuf,
                                                  &strbuflen) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU %s: Error '%s'\n",
                          str,
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_type_length_field_to_string: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  if (strbuflen)
    pstdout_printf (state_data->pstate,
                    "  FRU %s: %s\n",
                    str,
                    strbuf);

  return (0);
}

int
ipmi_fru_output_chassis_info_area (ipmi_fru_state_data_t *state_data,
                                   const void *areabuf,
                                   unsigned int area_length)
{
  uint8_t chassis_type;
  ipmi_fru_parse_field_t chassis_part_number;
  ipmi_fru_parse_field_t chassis_serial_number;
  ipmi_fru_parse_field_t chassis_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  unsigned int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (&chassis_part_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&chassis_serial_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&chassis_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_parse_field_t) * IPMI_FRU_CUSTOM_FIELDS);

  if (ipmi_fru_parse_chassis_info_area (state_data->fru_parse_ctx,
                                        areabuf,
                                        area_length,
                                        &chassis_type,
                                        &chassis_part_number,
                                        &chassis_serial_number,
                                        chassis_custom_fields,
                                        IPMI_FRU_CUSTOM_FIELDS) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Chassis Info Area Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_chassis_info_area: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  if (IPMI_FRU_CHASSIS_TYPE_VALID (chassis_type))
    pstdout_printf (state_data->pstate,
                    "  FRU Chassis Type: %s\n",
                    ipmi_fru_chassis_types[chassis_type]);
  else
    pstdout_printf (state_data->pstate,
                    "  FRU Chassis Type: %s\n",
                    ipmi_fru_chassis_types[IPMI_FRU_CHASSIS_TYPE_UNKNOWN]);

  /* achu: Chassis Info Area has no language code, assume English. */

  if (_output_field (state_data,
                     IPMI_FRU_LANGUAGE_CODE_ENGLISH,
                     &chassis_part_number,
                     "Chassis Part Number") < 0)
    return (-1);

  if (_output_field (state_data,
                     IPMI_FRU_LANGUAGE_CODE_ENGLISH,
                     &chassis_serial_number,
                     "Chassis Serial Number") < 0)
    return (-1);

  for (i = 0; i < IPMI_FRU_CUSTOM_FIELDS; i++)
    {
      if (_output_field (state_data,
                         IPMI_FRU_LANGUAGE_CODE_ENGLISH,
                         &chassis_custom_fields[i],
                         "Chassis Custom Info") < 0)
        return (-1);
    }

  return (0);
}

int
ipmi_fru_output_board_info_area (ipmi_fru_state_data_t *state_data,
                                 const void *areabuf,
                                 unsigned int area_length)
{
  uint8_t language_code;
  uint32_t mfg_date_time;
  ipmi_fru_parse_field_t board_manufacturer;
  ipmi_fru_parse_field_t board_product_name;
  ipmi_fru_parse_field_t board_serial_number;
  ipmi_fru_parse_field_t board_part_number;
  ipmi_fru_parse_field_t board_fru_file_id;
  ipmi_fru_parse_field_t board_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  time_t timetmp;
  struct tm mfg_date_time_tm;
  char mfg_date_time_buf[IPMI_FRU_STR_BUFLEN + 1];
  unsigned int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (&board_manufacturer, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_product_name, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_serial_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_fru_file_id, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&board_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_parse_field_t) * IPMI_FRU_CUSTOM_FIELDS);

  if (ipmi_fru_parse_board_info_area (state_data->fru_parse_ctx,
                                      areabuf,
                                      area_length,
                                      &language_code,
                                      &mfg_date_time,
                                      &board_manufacturer,
                                      &board_product_name,
                                      &board_serial_number,
                                      &board_part_number,
                                      &board_fru_file_id,
                                      board_custom_fields,
                                      IPMI_FRU_CUSTOM_FIELDS) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Board Info Area Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_board_info_area: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  if (state_data->prog_data->args->verbose_count)
    {
      if (IPMI_FRU_LANGUAGE_CODE_VALID (language_code))
        pstdout_printf (state_data->pstate,
                        "  FRU Board Language: %s\n",
                        ipmi_fru_language_codes[language_code]);
      else
        pstdout_printf (state_data->pstate,
                        "  FRU Board Language Code: %02Xh\n",
                        language_code);
    }

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&mfg_date_time_tm, '\0', sizeof (struct tm));

  timetmp = mfg_date_time;
  localtime_r (&timetmp, &mfg_date_time_tm);
  memset (mfg_date_time_buf, '\0', IPMI_FRU_STR_BUFLEN + 1);
  strftime (mfg_date_time_buf, IPMI_FRU_STR_BUFLEN, "%D - %T", &mfg_date_time_tm);

  pstdout_printf (state_data->pstate,
                  "  FRU Board Manufacturing Date/Time: %s\n",
                  mfg_date_time_buf);

  if (_output_field (state_data,
                     language_code,
                     &board_manufacturer,
                     "Board Manufacturer") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &board_product_name,
                     "Board Product Name") < 0)
    return (-1);


  if (_output_field (state_data,
                     language_code,
                     &board_serial_number,
                     "Board Serial Number") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &board_part_number,
                     "Board Part Number") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &board_fru_file_id,
                     "FRU File ID") < 0)
    return (-1);

  for (i = 0; i < IPMI_FRU_CUSTOM_FIELDS; i++)
    {
      if (_output_field (state_data,
                         language_code,
                         &board_custom_fields[i],
                         "Board Custom Info") < 0)
        return (-1);
    }

  return (0);
}

int
ipmi_fru_output_product_info_area (ipmi_fru_state_data_t *state_data,
                                   const void *areabuf,
                                   unsigned int area_length)
{
  uint8_t language_code;
  ipmi_fru_parse_field_t product_manufacturer_name;
  ipmi_fru_parse_field_t product_name;
  ipmi_fru_parse_field_t product_part_model_number;
  ipmi_fru_parse_field_t product_version;
  ipmi_fru_parse_field_t product_serial_number;
  ipmi_fru_parse_field_t product_asset_tag;
  ipmi_fru_parse_field_t product_fru_file_id;
  ipmi_fru_parse_field_t product_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  unsigned int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);
  
  memset (&product_manufacturer_name, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_name, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_part_model_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_version, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_serial_number, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_asset_tag, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_fru_file_id, '\0', sizeof (ipmi_fru_parse_field_t));
  memset (&product_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_parse_field_t) * IPMI_FRU_CUSTOM_FIELDS);
  
  if (ipmi_fru_parse_product_info_area (state_data->fru_parse_ctx,
                                        areabuf,
                                        area_length,
                                        &language_code,
                                        &product_manufacturer_name,
                                        &product_name,
                                        &product_part_model_number,
                                        &product_version,
                                        &product_serial_number,
                                        &product_asset_tag,
                                        &product_fru_file_id,
                                        product_custom_fields,
                                        IPMI_FRU_CUSTOM_FIELDS) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Product Info Area Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_product_info_area: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  if (state_data->prog_data->args->verbose_count)
    {
      if (IPMI_FRU_LANGUAGE_CODE_VALID (language_code))
        pstdout_printf (state_data->pstate,
                        "  FRU Product Language: %s\n",
                        ipmi_fru_language_codes[language_code]);
      else
        pstdout_printf (state_data->pstate,
                        "  FRU Product Language Code: %02Xh\n",
                        language_code);
    }

  if (_output_field (state_data,
                     language_code,
                     &product_manufacturer_name,
                     "Product Manufacturer Name") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &product_name,
                     "Product Name") < 0)
    return (-1);


  if (_output_field (state_data,
                     language_code,
                     &product_part_model_number,
                     "Product Part/Model Number") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &product_version,
                     "Product Version") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &product_serial_number,
                     "Product Serial Number") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &product_asset_tag,
                     "Product Asset Tag") < 0)
    return (-1);

  if (_output_field (state_data,
                     language_code,
                     &product_fru_file_id,
                     "FRU File ID") < 0)
    return (-1);

  for (i = 0; i < IPMI_FRU_CUSTOM_FIELDS; i++)
    {
      if (_output_field (state_data,
                         language_code,
                         &product_custom_fields[i],
                         "Product Custom Info") < 0)
        return (-1);
    }

  return (0);
}

static char *
_voltage_str (uint8_t voltage)
{
  switch (voltage)
    {
    case IPMI_FRU_VOLTAGE_12V:
      return "12V";
    case IPMI_FRU_VOLTAGE_MINUS12V:
      return "-12V";
    case IPMI_FRU_VOLTAGE_5V:
      return "5V";
    case IPMI_FRU_VOLTAGE_3_3V:
      return "3.3V";
    default:
      return "";
    }

  return (NULL);		/* NOT REACHED */
}

int
ipmi_fru_output_power_supply_information (ipmi_fru_state_data_t *state_data,
                                          const void *areabuf,
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

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  if (ipmi_fru_parse_multirecord_power_supply_information (state_data->fru_parse_ctx,
                                                           areabuf,
                                                           area_length,
                                                           &overall_capacity,
                                                           &peak_va,
                                                           &inrush_current,
                                                           &inrush_interval,
                                                           &low_end_input_voltage_range_1,
                                                           &high_end_input_voltage_range_1,
                                                           &low_end_input_voltage_range_2,
                                                           &high_end_input_voltage_range_2,
                                                           &low_end_input_frequency_range,
                                                           &high_end_input_frequency_range,
                                                           &ac_dropout_tolerance,
                                                           &predictive_fail_support,
                                                           &power_factor_correction,
                                                           &autoswitch,
                                                           &hot_swap_support,
                                                           &tachometer_pulses_per_rotation_predictive_fail_polarity,
                                                           &peak_capacity,
                                                           &hold_up_time,
                                                           &voltage_1,
                                                           &voltage_2,
                                                           &total_combined_wattage,
                                                           &predictive_fail_tachometer_lower_threshold) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Power Supply Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_multirecord_power_supply_information: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

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
                  "  FRU Power Supply Low End Acceptable Frequency: %u Hz\n",
                  low_end_input_frequency_range);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply High End Acceptable Frequency: %u Hz\n",
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
                  _voltage_str (voltage_1));
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Voltage 2: %s\n",
                  _voltage_str (voltage_2));
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Total Combined Wattage: %u Watts\n",
                  total_combined_wattage);

  return (0);
}

int
ipmi_fru_output_dc_output (ipmi_fru_state_data_t *state_data,
                           const void *areabuf,
                           uint8_t area_length)
{
  unsigned int output_number;
  unsigned int standby;
  int nominal_voltage;
  int maximum_negative_voltage_deviation;
  int maximum_positive_voltage_deviation;
  unsigned int ripple_and_noise_pk_pk;
  unsigned int minimum_current_draw;
  unsigned int maximum_current_draw;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  if (ipmi_fru_parse_multirecord_dc_output (state_data->fru_parse_ctx,
                                            areabuf,
                                            area_length,
                                            &output_number,
                                            &standby,
                                            &nominal_voltage,
                                            &maximum_negative_voltage_deviation,
                                            &maximum_positive_voltage_deviation,
                                            &ripple_and_noise_pk_pk,
                                            &minimum_current_draw,
                                            &maximum_current_draw) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord DC Output Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_multirecord_dc_output: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

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

  return (0);
}

int
ipmi_fru_output_dc_load (ipmi_fru_state_data_t *state_data,
                         const void *areabuf,
                         uint8_t area_length)
{
  unsigned int output_number;
  unsigned int standby;
  int nominal_voltage;
  int specd_minimum_voltage;
  int specd_maximum_voltage;
  unsigned int specd_ripple_and_noise_pk_pk;
  unsigned int minimum_current_load;
  unsigned int maximum_current_load;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  if (ipmi_fru_parse_multirecord_dc_load (state_data->fru_parse_ctx,
                                          areabuf,
                                          area_length,
                                          &output_number,
                                          &standby,
                                          &nominal_voltage,
                                          &specd_minimum_voltage,
                                          &specd_maximum_voltage,
                                          &specd_ripple_and_noise_pk_pk,
                                          &minimum_current_load,
                                          &maximum_current_load) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord DC Load Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_multirecord_dc_load: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

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

  return (0);
}

int
ipmi_fru_output_management_access_record (ipmi_fru_state_data_t *state_data,
                                          const void *areabuf,
                                          uint8_t area_length)
{
  uint8_t sub_record_type;
  uint8_t sub_record_data[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int sub_record_data_len = IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX;
  unsigned int i;
  
  assert (state_data);
  assert (areabuf);
  assert (area_length);
  
  memset (sub_record_data, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);
  
  if (ipmi_fru_parse_multirecord_management_access_record (state_data->fru_parse_ctx,
                                                           areabuf,
                                                           area_length,
                                                           &sub_record_type,
                                                           sub_record_data,
                                                           &sub_record_data_len) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Management Access Record Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_multirecord_management_access_record: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

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

      for (i = 0; i < sub_record_data_len; i++)
        {
          if (sub_record_data_len > 8 && (i % 8) == 0)
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
      for (i = 0; i < sub_record_data_len; i++)
        {
          if (sub_record_data_len > 8 && (i % 8) == 0)
            pstdout_printf (state_data->pstate, "\n    ");

          pstdout_printf (state_data->pstate,
                          " %02Xh",
                          sub_record_data[i]);
        }
      pstdout_printf (state_data->pstate, "\n");
    }

  return (0);
}

int
ipmi_fru_output_base_compatibility_record (ipmi_fru_state_data_t *state_data,
                                           const void *areabuf,
                                           uint8_t area_length)
{
  uint32_t manufacturer_id;
  unsigned int entity_id_code;
  unsigned int compatibility_base;
  unsigned int compatibility_code_start_value;
  uint8_t code_range_mask[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int code_range_mask_len = IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX;
  char iana_buf[IPMI_FRU_STR_BUFLEN + 1];
  int ret;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (code_range_mask, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);

  if (ipmi_fru_parse_multirecord_base_compatibility_record (state_data->fru_parse_ctx,
                                                            areabuf,
                                                            area_length,
                                                            &manufacturer_id,
                                                            &entity_id_code,
                                                            &compatibility_base,
                                                            &compatibility_code_start_value,
                                                            code_range_mask,
                                                            &code_range_mask_len) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Base Compatibility Record Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_multirecord_base_compatibility_record: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  memset (iana_buf, '\0', IPMI_FRU_STR_BUFLEN + 1);
  
  /* if ret == 0 means no string, < 0 means bad manufacturer id
   * either way, output just the number
   */
  ret = ipmi_iana_enterprise_numbers_string (manufacturer_id,
                                             iana_buf,
                                             IPMI_FRU_STR_BUFLEN);

  if (ret > 0)
    pstdout_printf (state_data->pstate,
                    "  FRU Base Compatibility Manufacturer ID: %s (%Xh)\n",
                    iana_buf,
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
      unsigned int i;

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

  return (0);
}

int
ipmi_fru_output_extended_compatibility_record (ipmi_fru_state_data_t *state_data,
                                               const void *areabuf,
                                               uint8_t area_length)
{
  uint32_t manufacturer_id;
  unsigned int entity_id_code;
  unsigned int compatibility_base;
  unsigned int compatibility_code_start_value;
  uint8_t code_range_mask[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int code_range_mask_len = IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX;
  char iana_buf[IPMI_FRU_STR_BUFLEN + 1];
  int ret;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (code_range_mask, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);

  if (ipmi_fru_parse_multirecord_extended_compatibility_record (state_data->fru_parse_ctx,
                                                                areabuf,
                                                                area_length,
                                                                &manufacturer_id,
                                                                &entity_id_code,
                                                                &compatibility_base,
                                                                &compatibility_code_start_value,
                                                                code_range_mask,
                                                                &code_range_mask_len) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Extended Compatibility Record Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_multirecord_extended_compatibility_record: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }

  memset (iana_buf, '\0', IPMI_FRU_STR_BUFLEN + 1);
  
  /* if ret == 0 means no string, < 0 means bad manufacturer id
   * either way, output just the number
   */
  ret = ipmi_iana_enterprise_numbers_string (manufacturer_id,
                                             iana_buf,
                                             IPMI_FRU_STR_BUFLEN);

  if (ret > 0)
    pstdout_printf (state_data->pstate,
                    "  FRU Extended Compatibility Manufacturer ID: %s (%Xh)\n",
                    iana_buf,
                    manufacturer_id);
  else
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
      unsigned int i;

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

  return (0);
}

int
ipmi_fru_output_oem_record (ipmi_fru_state_data_t *state_data,
                            const void *areabuf,
                            uint8_t area_length)
{
  uint32_t manufacturer_id;
  uint8_t oem_data[IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int oem_data_len = IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX;
  char iana_buf[IPMI_FRU_STR_BUFLEN + 1];
  int ret;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (oem_data, '\0', IPMI_FRU_PARSE_AREA_TYPE_LENGTH_FIELD_MAX + 1);

  if (ipmi_fru_parse_multirecord_oem_record (state_data->fru_parse_ctx,
                                             areabuf,
                                             area_length,
                                             &manufacturer_id,
                                             oem_data,
                                             &oem_data_len) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord OEM Record Error: %s\n",
                          ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_multirecord_oem_record: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      return (-1);
    }
  
  memset (iana_buf, '\0', IPMI_FRU_STR_BUFLEN + 1);
  
  /* if ret == 0 means no string, < 0 means bad manufacturer id
   * either way, output just the number
   */
  ret = ipmi_iana_enterprise_numbers_string (manufacturer_id,
                                             iana_buf,
                                             IPMI_FRU_STR_BUFLEN);
  
  if (ret > 0)
    pstdout_printf (state_data->pstate,
                    "  FRU OEM Manufacturer ID: %s (%Xh)\n",
                    iana_buf,
                    manufacturer_id);
  else
    pstdout_printf (state_data->pstate,
                    "  FRU OEM Manufacturer ID: %Xh\n",
                    manufacturer_id);

  if (state_data->prog_data->args->interpret_oem_data)
    {
      if (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON)
	{
	  if ((ret = ipmi_fru_oem_wistron_oem_record (state_data,
						      manufacturer_id,
						      oem_data,
						      oem_data_len)) < 0)
	    return (-1);

	  if (ret)
	    return (0);
	}
    }

  if (oem_data_len)
    {
      unsigned int i;

      pstdout_printf (state_data->pstate,
                      "  FRU OEM Data:");

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

  return (0);
}
