/*****************************************************************************\
 *  $Id: ipmi-fru-output.c,v 1.8 2010-03-02 21:09:07 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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

#include "ipmi-fru_.h"
#include "ipmi-fru-output.h"
#include "ipmi-fru-oem-wistron.h"
#include "tool-util-common.h"

#include "freeipmi-portability.h"

/* haven't seen a motherboard with more than 2-3 so far, 64 should be more than enough */
#define IPMI_FRU_CUSTOM_FIELDS 64

#define IPMI_FRU_STR_BUFLEN    1024

int
_output_field (ipmi_fru_state_data_t *state_data,
               uint8_t language_code,
               ipmi_fru_field_t *field,
               char *str)
{
  char strbuf[IPMI_FRU_AREA_STRING_MAX + 1];
  unsigned int strbuflen = IPMI_FRU_AREA_STRING_MAX;

  assert (state_data);
  assert (field);
  assert (str);

  if (!field->type_length_field_length)
    return (0);

  memset (strbuf, '\0', IPMI_FRU_AREA_STRING_MAX + 1);
      
  if (ipmi_fru_type_length_field_to_string (state_data->fru_ctx,
					    field->type_length_field,
					    field->type_length_field_length,
					    language_code,
					    strbuf,
					    &strbuflen) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU %s: Error '%s'\n",
                          str,
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_type_length_field_to_string: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
  ipmi_fru_field_t chassis_part_number;
  ipmi_fru_field_t chassis_serial_number;
  ipmi_fru_field_t chassis_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  unsigned int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (&chassis_part_number, '\0', sizeof (ipmi_fru_field_t));
  memset (&chassis_serial_number, '\0', sizeof (ipmi_fru_field_t));
  memset (&chassis_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_field_t) * IPMI_FRU_CUSTOM_FIELDS);

  if (ipmi_fru_chassis_info_area (state_data->fru_ctx,
				  areabuf,
				  area_length,
				  &chassis_type,
				  &chassis_part_number,
				  &chassis_serial_number,
				  chassis_custom_fields,
				  IPMI_FRU_CUSTOM_FIELDS) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Chassis Info Area Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_chassis_info_area: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
  ipmi_fru_field_t board_manufacturer;
  ipmi_fru_field_t board_product_name;
  ipmi_fru_field_t board_serial_number;
  ipmi_fru_field_t board_part_number;
  ipmi_fru_field_t board_fru_file_id;
  ipmi_fru_field_t board_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  char mfg_date_time_buf[IPMI_FRU_STR_BUFLEN + 1];
  unsigned int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (&board_manufacturer, '\0', sizeof (ipmi_fru_field_t));
  memset (&board_product_name, '\0', sizeof (ipmi_fru_field_t));
  memset (&board_serial_number, '\0', sizeof (ipmi_fru_field_t));
  memset (&board_fru_file_id, '\0', sizeof (ipmi_fru_field_t));
  memset (&board_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_field_t) * IPMI_FRU_CUSTOM_FIELDS);

  if (ipmi_fru_board_info_area (state_data->fru_ctx,
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
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Board Info Area Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_board_info_area: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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

  if (mfg_date_time != IPMI_FRU_MFG_DATE_TIME_UNSPECIFIED)
    {
      memset (mfg_date_time_buf, '\0', IPMI_FRU_STR_BUFLEN + 1);

      if (ipmi_timestamp_string (mfg_date_time,
				 state_data->prog_data->args->common_args.utc_offset,
				 get_timestamp_flags (&(state_data->prog_data->args->common_args),
						      IPMI_TIMESTAMP_FLAG_DEFAULT), 
				 "%D - %T",
				 mfg_date_time_buf,
				 IPMI_FRU_STR_BUFLEN) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_timestamp_string: %s\n",
			   strerror (errno));
	  return (-1);
	}
      
      pstdout_printf (state_data->pstate,
		      "  FRU Board Manufacturing Date/Time: %s\n",
		      mfg_date_time_buf);
    }
  else
    pstdout_printf (state_data->pstate,
		    "  FRU Board Manufacturing Date/Time: unspecified\n");
  
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
  ipmi_fru_field_t product_manufacturer_name;
  ipmi_fru_field_t product_name;
  ipmi_fru_field_t product_part_model_number;
  ipmi_fru_field_t product_version;
  ipmi_fru_field_t product_serial_number;
  ipmi_fru_field_t product_asset_tag;
  ipmi_fru_field_t product_fru_file_id;
  ipmi_fru_field_t product_custom_fields[IPMI_FRU_CUSTOM_FIELDS];
  unsigned int i;

  assert (state_data);
  assert (areabuf);
  assert (area_length);
  
  memset (&product_manufacturer_name, '\0', sizeof (ipmi_fru_field_t));
  memset (&product_name, '\0', sizeof (ipmi_fru_field_t));
  memset (&product_part_model_number, '\0', sizeof (ipmi_fru_field_t));
  memset (&product_version, '\0', sizeof (ipmi_fru_field_t));
  memset (&product_serial_number, '\0', sizeof (ipmi_fru_field_t));
  memset (&product_asset_tag, '\0', sizeof (ipmi_fru_field_t));
  memset (&product_fru_file_id, '\0', sizeof (ipmi_fru_field_t));
  memset (&product_custom_fields[0],
          '\0',
          sizeof (ipmi_fru_field_t) * IPMI_FRU_CUSTOM_FIELDS);
  
  if (ipmi_fru_product_info_area (state_data->fru_ctx,
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
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Product Info Area Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_product_info_area: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
                                          unsigned int area_length)
{
  unsigned int overall_capacity;
  unsigned int peak_va;
  unsigned int inrush_current;
  unsigned int inrush_interval;
  int low_end_input_voltage_range_1;
  int high_end_input_voltage_range_1;
  int low_end_input_voltage_range_2;
  int high_end_input_voltage_range_2;
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

  if (ipmi_fru_multirecord_power_supply_information (state_data->fru_ctx,
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
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Power Supply Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_multirecord_power_supply_information: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
                  "  FRU Power Supply Low End Input Voltage 1: %d mV\n",
                  low_end_input_voltage_range_1);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply High End Input Voltage 1: %d mV\n",
                  high_end_input_voltage_range_1);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply Low End Input Voltage 2: %d mV\n",
                  low_end_input_voltage_range_2);
  pstdout_printf (state_data->pstate,
                  "  FRU Power Supply High End Input Voltage 2: %d mV\n",
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

  if (peak_capacity != IPMI_FRU_PEAK_CAPACITY_UNSPECIFIED)
    pstdout_printf (state_data->pstate,
		    "  FRU Power Supply Peak Capacity: %u Watts\n",
		    peak_capacity);
  else
    pstdout_printf (state_data->pstate,
		    "  FRU Power Supply Peak Capacity: unspecified\n",
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
			   unsigned int area_type,
                           const void *areabuf,
                           unsigned int area_length)
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
  assert (area_type == IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_OUTPUT
	  || area_type == IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_DC_OUTPUT);
  assert (areabuf);
  assert (area_length);

  if (area_type == IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_OUTPUT)
    {
      if (ipmi_fru_multirecord_dc_output (state_data->fru_ctx,
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
	  if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
	    {
	      pstdout_printf (state_data->pstate,
			      "  FRU Multirecord DC Output Error: %s\n",
			      ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	      return (0);
	    }
      
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_fru_multirecord_dc_output: %s\n",
			   ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	  return (-1);
	}
    }
  else
    {
      if (ipmi_fru_multirecord_extended_dc_output (state_data->fru_ctx,
						   areabuf,
						   area_length,
						   &output_number,
						   NULL, /* don't need the current_units */
						   &standby,
						   &nominal_voltage,
						   &maximum_negative_voltage_deviation,
						   &maximum_positive_voltage_deviation,
						   &ripple_and_noise_pk_pk,
						   &minimum_current_draw,
						   &maximum_current_draw) < 0)
	{
	  if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
	    {
	      pstdout_printf (state_data->pstate,
			      "  FRU Multirecord Extended DC Output Error: %s\n",
			      ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	      return (0);
	    }
	  
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_fru_multirecord_extended_dc_output: %s\n",
			   ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	  return (-1);
	}
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
			 unsigned int area_type,
                         const void *areabuf,
                         unsigned int area_length)
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
  assert (area_type == IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_LOAD
	  || area_type == IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_DC_LOAD);
  assert (areabuf);
  assert (area_length);

  if (area_type == IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_LOAD)
    {
      if (ipmi_fru_multirecord_dc_load (state_data->fru_ctx,
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
	  if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
	    {
	      pstdout_printf (state_data->pstate,
			      "  FRU Multirecord DC Load Error: %s\n",
			      ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	      return (0);
	    }
	  
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_fru_multirecord_dc_load: %s\n",
			   ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	  return (-1);
	}
    }
  else
    {
      if (ipmi_fru_multirecord_extended_dc_load (state_data->fru_ctx,
						 areabuf,
						 area_length,
						 &output_number,
						 NULL, /* don't need the current_units */
						 &standby,
						 &nominal_voltage,
						 &specd_minimum_voltage,
						 &specd_maximum_voltage,
						 &specd_ripple_and_noise_pk_pk,
						 &minimum_current_load,
						 &maximum_current_load) < 0)
	{
	  if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
	    {
	      pstdout_printf (state_data->pstate,
			      "  FRU Multirecord Extended DC Load Error: %s\n",
			      ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	      return (0);
	    }
	  
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_fru_multirecord_extended_dc_load: %s\n",
			   ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	  return (-1);
	}
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
                                          unsigned int area_length)
{
  uint8_t sub_record_type;
  uint8_t sub_record_data[IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int sub_record_data_len = IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX;
  unsigned int i;
  
  assert (state_data);
  assert (areabuf);
  assert (area_length);
  
  memset (sub_record_data, '\0', IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1);
  
  if (ipmi_fru_multirecord_management_access_record (state_data->fru_ctx,
						     areabuf,
						     area_length,
						     &sub_record_type,
						     sub_record_data,
						     &sub_record_data_len) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Management Access Record Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_multirecord_management_access_record: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
                                           unsigned int area_length)
{
  uint32_t manufacturer_id;
  unsigned int entity_id_code;
  unsigned int compatibility_base;
  unsigned int compatibility_code_start_value;
  uint8_t code_range_mask[IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int code_range_mask_len = IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX;
  char iana_buf[IPMI_FRU_STR_BUFLEN + 1];
  int ret;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (code_range_mask, '\0', IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1);

  if (ipmi_fru_multirecord_base_compatibility_record (state_data->fru_ctx,
						      areabuf,
						      area_length,
						      &manufacturer_id,
						      &entity_id_code,
						      &compatibility_base,
						      &compatibility_code_start_value,
						      code_range_mask,
						      &code_range_mask_len) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Base Compatibility Record Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_multirecord_base_compatibility_record: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
                                               unsigned int area_length)
{
  uint32_t manufacturer_id;
  unsigned int entity_id_code;
  unsigned int compatibility_base;
  unsigned int compatibility_code_start_value;
  uint8_t code_range_mask[IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int code_range_mask_len = IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX;
  char iana_buf[IPMI_FRU_STR_BUFLEN + 1];
  int ret;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (code_range_mask, '\0', IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1);

  if (ipmi_fru_multirecord_extended_compatibility_record (state_data->fru_ctx,
							  areabuf,
							  area_length,
							  &manufacturer_id,
							  &entity_id_code,
							  &compatibility_base,
							  &compatibility_code_start_value,
							  code_range_mask,
							  &code_range_mask_len) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord Extended Compatibility Record Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_multirecord_extended_compatibility_record: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
                            unsigned int area_length)
{
  uint32_t manufacturer_id;
  uint8_t oem_data[IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1];
  unsigned int oem_data_len = IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX;
  char iana_buf[IPMI_FRU_STR_BUFLEN + 1];
  uint8_t record_type_id;
  int ret;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  memset (oem_data, '\0', IPMI_FRU_AREA_TYPE_LENGTH_FIELD_MAX + 1);

  if (ipmi_fru_multirecord_oem_record (state_data->fru_ctx,
				       areabuf,
				       area_length,
				       &manufacturer_id,
				       oem_data,
				       &oem_data_len) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          pstdout_printf (state_data->pstate,
                          "  FRU Multirecord OEM Record Error: %s\n",
                          ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          return (0);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_multirecord_oem_record: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
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
      if (ipmi_fru_read_multirecord_record_type_id (state_data->fru_ctx,
						    &record_type_id) < 0)
	{
	  if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
	    {
	      pstdout_printf (state_data->pstate,
			      "  FRU Multirecord OEM Record Error: %s\n",
			      ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	      return (0);
	    }
	  
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_fru_multirecord_oem_record: %s\n",
			   ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	  return (-1);
	}

      if (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON)
	{
	  if ((ret = ipmi_fru_oem_wistron_oem_record (state_data,
						      record_type_id,
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

static int
_ipmi_fru_output_dimm_ddr3 (ipmi_fru_state_data_t *state_data,
			    const void *areabuf,
			    unsigned int area_length)
{
  fiid_obj_t obj_record = NULL;
  uint8_t total_sdram_capacity;
  char *total_sdram_capacity_str = NULL;
  uint32_t total_sdram_capacity_val = 0; 
  uint8_t total_sdram_capacity_valid;
  uint8_t bank_address_bits;
  char *bank_address_bits_str = NULL;
  uint8_t module_minimum_nominal_voltage_1_5;
  uint8_t module_minimum_nominal_voltage_1_35;
  uint8_t module_minimum_nominal_voltage_1_25;
  uint8_t sdram_device_width;
  char *sdram_device_width_str = NULL;
  uint32_t sdram_device_width_val = 0; 
  uint8_t sdram_device_width_valid;
  uint8_t number_of_ranks;
  char *number_of_ranks_str = NULL;
  uint32_t number_of_ranks_val = 0; 
  uint8_t number_of_ranks_valid;
  uint8_t primary_bus_width;
  char *primary_bus_width_str = NULL;
  uint32_t primary_bus_width_val = 0; 
  uint8_t primary_bus_width_valid;
  uint8_t bus_width_extension;
  char *bus_width_extension_str;
  uint32_t total_memory_capacity;
  char *total_memory_capacity_units_str = NULL;
  uint8_t die_count;
  char *die_count_str;
  uint8_t sdram_device_type;
  char *sdram_device_type_str;
  uint8_t number_of_continuation_codes_module_manufacturer;
  uint8_t last_non_zero_module_manufacturer;
  char *module_manufacturer_str;
  uint8_t module_manufacturing_date_year;
  uint8_t module_manufacturing_date_week;
  uint32_t module_serial_number;
  char module_part_number[IPMI_FRU_STR_BUFLEN + 1];
  int module_part_number_len;
  uint16_t module_revision_code;
  uint8_t number_of_continuation_codes_dram_manufacturer;
  uint8_t last_non_zero_dram_manufacturer;
  char *dram_manufacturer_str;
  uint64_t val;
  int block_len;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);
 
  if (!(obj_record = fiid_obj_create (tmpl_fru_dimm_spd_ddr3_record)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
			areabuf,
			area_length) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set_all: %s\n",
		       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }

  if ((block_len = fiid_template_block_len_bytes (tmpl_fru_dimm_spd_ddr3_record,
						  "spd_bytes_used",
						  "last_non_zero_dram_manufacturer")) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_template_block_len_bytes: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (area_length < block_len)
    {
      pstdout_printf (state_data->pstate, "\n");
      pstdout_printf (state_data->pstate,
                      "  FRU Error: %s\n",
                      ipmi_fru_ctx_strerror (IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID));
      goto out;
    }

  if (FIID_OBJ_GET (obj_record,
		    "total_sdram_capacity",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'total_sdram_capacity': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  total_sdram_capacity = val;
  
  switch (total_sdram_capacity)
    {
    case IPMI_FRU_DIMMSPD_TOTAL_SDRAM_CAPACITY_256_MB:
      total_sdram_capacity_str = "256 Mb";
      total_sdram_capacity_val = 256;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "MB";
      break;
    case IPMI_FRU_DIMMSPD_TOTAL_SDRAM_CAPACITY_512_MB:
      total_sdram_capacity_str = "512 Mb";
      total_sdram_capacity_val = 512;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "MB";
      break;
    case IPMI_FRU_DIMMSPD_TOTAL_SDRAM_CAPACITY_1_GB:
      total_sdram_capacity_str = "1 Gb";
      total_sdram_capacity_val = 1024;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    case IPMI_FRU_DIMMSPD_TOTAL_SDRAM_CAPACITY_2_GB:
      total_sdram_capacity_str = "2 Gb";
      total_sdram_capacity_val = 2048;
      total_sdram_capacity_valid = 1; 
      total_memory_capacity_units_str = "GB";
     break;
    case IPMI_FRU_DIMMSPD_TOTAL_SDRAM_CAPACITY_4_GB:
      total_sdram_capacity_str = "4 Gb";
      total_sdram_capacity_val = 4096;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    case IPMI_FRU_DIMMSPD_TOTAL_SDRAM_CAPACITY_8_GB:
      total_sdram_capacity_str = "8 Gb";
      total_sdram_capacity_val = 8192;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    case IPMI_FRU_DIMMSPD_TOTAL_SDRAM_CAPACITY_16_GB:
      total_sdram_capacity_str = "16 Gb";
      total_sdram_capacity_val = 16384;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    default:
      total_sdram_capacity_str = "Unknown";
      total_sdram_capacity_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Total SDRAM Capacity : %s\n",
		  total_sdram_capacity_str);

  if (FIID_OBJ_GET (obj_record,
		    "bank_address_bits",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bank_address_bits': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  bank_address_bits = val;

  switch (bank_address_bits)
    {
    case IPMI_FRU_DIMMSPD_BANK_ADDRESS_BITS_3:
      bank_address_bits_str = "8 Banks";
      break;
    case IPMI_FRU_DIMMSPD_BANK_ADDRESS_BITS_4:
      bank_address_bits_str = "16 Banks";
      break;
    case IPMI_FRU_DIMMSPD_BANK_ADDRESS_BITS_5:
      bank_address_bits_str = "32 Banks";
      break;
    case IPMI_FRU_DIMMSPD_BANK_ADDRESS_BITS_6:
      bank_address_bits_str = "64 Banks";
      break;
    default:
      bank_address_bits_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Memory Banks : %s\n",
		  bank_address_bits_str);

  if (FIID_OBJ_GET (obj_record,
		    "module_minimum_nominal_voltage.1_5",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_minimum_nominal_voltage.1_5': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_minimum_nominal_voltage_1_5 = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module 1.5 V Nominal Voltage : %s\n",
		  (module_minimum_nominal_voltage_1_5 == IPMI_FRU_DIMMSPD_VOLTAGE_1_5_OPERABLE) ? "Operable" : "Not Operable");

  if (FIID_OBJ_GET (obj_record,
		    "module_minimum_nominal_voltage.1_35",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_minimum_nominal_voltage.1_35': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_minimum_nominal_voltage_1_35 = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module 1.35 V Nominal Voltage : %s\n",
		  (module_minimum_nominal_voltage_1_35 == IPMI_FRU_DIMMSPD_VOLTAGE_1_35_OPERABLE) ? "Operable" : "Not Operable");

  if (FIID_OBJ_GET (obj_record,
		    "module_minimum_nominal_voltage.1_25",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_minimum_nominal_voltage.1_25': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_minimum_nominal_voltage_1_25 = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module 1.25 V Nominal Voltage : %s\n",
		  (module_minimum_nominal_voltage_1_25 == IPMI_FRU_DIMMSPD_VOLTAGE_1_25_OPERABLE) ? "Operable" : "Not Operable");

  if (FIID_OBJ_GET (obj_record,
		    "sdram_device_width",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sdram_device_width': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  sdram_device_width = val;

  switch (sdram_device_width)
    {
    case IPMI_FRU_DIMMSPD_DEVICE_WIDTH_4_BITS:
      sdram_device_width_str = "4 bits";
      sdram_device_width_val = 4;
      sdram_device_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DEVICE_WIDTH_8_BITS:
      sdram_device_width_str = "8 bits";
      sdram_device_width_val = 8;
      sdram_device_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DEVICE_WIDTH_16_BITS:
      sdram_device_width_str = "16 bits";
      sdram_device_width_val = 16;
      sdram_device_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DEVICE_WIDTH_32_BITS:
      sdram_device_width_str = "32 bits";
      sdram_device_width_val = 32;
      sdram_device_width_valid = 1;
      break;
    default:
      sdram_device_width_str = "Unknown";
      sdram_device_width_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU SDRAM Device Width : %s\n",
		  sdram_device_width_str);

  if (FIID_OBJ_GET (obj_record,
		    "number_of_ranks",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_ranks': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  number_of_ranks = val;

  /* achu: Yeah, a lot of unnecessary code here, but gonna keep
   * code style pattern the same as the above.
   */
  switch (number_of_ranks)
    {
    case IPMI_FRU_DIMMSPD_NUMBER_OF_RANKS_1:
      number_of_ranks_str = "1";
      number_of_ranks_val = 1;
      number_of_ranks_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_NUMBER_OF_RANKS_2:
      number_of_ranks_str = "2";
      number_of_ranks_val = 2;
      number_of_ranks_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_NUMBER_OF_RANKS_3:
      number_of_ranks_str = "3";
      number_of_ranks_val = 3;
      number_of_ranks_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_NUMBER_OF_RANKS_4:
      number_of_ranks_str = "4";
      number_of_ranks_val = 4;
      number_of_ranks_valid = 1;
      break;
    default:
      number_of_ranks_str = "Unknown";
      number_of_ranks_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Number of Ranks : %s\n",
		  number_of_ranks_str);

  if (FIID_OBJ_GET (obj_record,
		    "primary_bus_width",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'primary_bus_width': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  primary_bus_width = val;

  switch (primary_bus_width)
    {
    case IPMI_FRU_DIMMSPD_PRIMARY_BUS_WIDTH_8_BITS:
      primary_bus_width_str = "8 bits";
      primary_bus_width_val = 8;
      primary_bus_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_PRIMARY_BUS_WIDTH_16_BITS:
      primary_bus_width_str = "16 bits";
      primary_bus_width_val = 16;
      primary_bus_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_PRIMARY_BUS_WIDTH_32_BITS:
      primary_bus_width_str = "32 bits";
      primary_bus_width_val = 32;
      primary_bus_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_PRIMARY_BUS_WIDTH_64_BITS:
      primary_bus_width_str = "64 bits";
      primary_bus_width_val = 64;
      primary_bus_width_valid = 1;
      break;
    default:
      primary_bus_width_str = "Unknown";
      primary_bus_width_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Primary Bus Width : %s\n",
		  primary_bus_width_str);

  if (FIID_OBJ_GET (obj_record,
		    "bus_width_extension",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bus_width_extension': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  bus_width_extension = val;

  switch (bus_width_extension)
    {
    case IPMI_FRU_DIMMSPD_BUS_WIDTH_EXTENSION_0_BITS:
      bus_width_extension_str = "0 bits";
      break;
    case IPMI_FRU_DIMMSPD_BUS_WIDTH_EXTENSION_8_BITS:
      bus_width_extension_str = "8 bits";
      break;
    default:
      bus_width_extension_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Bus Width Extension : %s\n",
		  bus_width_extension_str);

  if (total_sdram_capacity_valid
      && sdram_device_width_valid
      && number_of_ranks_valid
      && primary_bus_width_valid)
    {
      /* Per JEDEC document, in section "Byte 8"
       *
       * SDRAM CAPCITY / 8 * PRIMARY BUS WIDTH / SDRAM WIDTH * RANKS 
       */
      total_memory_capacity = (total_sdram_capacity_val / 8) * (primary_bus_width_val / sdram_device_width_val) * number_of_ranks_val;

      pstdout_printf (state_data->pstate,
		      "  FRU Total Memory Capacity : %u %s\n",
		      total_memory_capacity,
		      total_memory_capacity_units_str);
    }
  else
    pstdout_printf (state_data->pstate,
		    "  FRU Total Memory Capacity : Unknown\n");

  if (FIID_OBJ_GET (obj_record,
		    "die_count",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'die_count': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  die_count = val;

  switch (die_count)
    {
    case IPMI_FRU_DIMMSPD_DIE_COUNT_NOT_SPECIFIED:
      die_count_str = "Not Specified";
      break;
    case IPMI_FRU_DIMMSPD_DIE_COUNT_SINGLE_DIE:
      die_count_str = "Single die";
      break;
    case IPMI_FRU_DIMMSPD_DIE_COUNT_2_DIE:
      die_count_str = "2 die";
      break;
    case IPMI_FRU_DIMMSPD_DIE_COUNT_4_DIE:
      die_count_str = "4 die";
      break;
    case IPMI_FRU_DIMMSPD_DIE_COUNT_8_DIE:
      die_count_str = "8 die";
      break;
    default:
      die_count_str = "Unknown";
      break;
    } 

  pstdout_printf (state_data->pstate,
		  "  FRU Die Count : %s\n",
		  die_count_str);

  if (FIID_OBJ_GET (obj_record,
		    "sdram_device_type",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sdram_device_type': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  sdram_device_type = val;

  switch (sdram_device_type)
    {
    case IPMI_FRU_DIMMSPD_SDRAM_DEVICE_TYPE_STANDARD_MONOLITHIC_DRAM_DEVICE:
      sdram_device_type_str = "Standard Monolithic DRAM Device";
      break;
    case IPMI_FRU_DIMMSPD_SDRAM_DEVICE_TYPE_NON_STANDARD_DEVICE:
      sdram_device_type_str = "Non-Standard Device";
      break;
    default:
      sdram_device_type_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU SDRAM Device Type : %s\n",
		  sdram_device_type_str);
  
  if (FIID_OBJ_GET (obj_record,
		    "number_of_continuation_codes_module_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_continuation_codes_module_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  number_of_continuation_codes_module_manufacturer = val;

  if (FIID_OBJ_GET (obj_record,
		    "last_non_zero_module_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'last_non_zero_module_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  last_non_zero_module_manufacturer = val;

  module_manufacturer_str = ipmi_jedec_manufacturer_id_search (number_of_continuation_codes_module_manufacturer,
							       last_non_zero_module_manufacturer);
  if (module_manufacturer_str)
    pstdout_printf (state_data->pstate,
		    "  FRU Module Manufacturer : %s\n",
		    module_manufacturer_str);
  else
    pstdout_printf (state_data->pstate,
		    "  FRU Module Manufacturer : Unrecognized\n");

  if (FIID_OBJ_GET (obj_record,
		    "module_manufacturing_date.year",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_manufacturing_date.year': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_manufacturing_date_year = val;

  if (FIID_OBJ_GET (obj_record,
		    "module_manufacturing_date.week",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_manufacturing_date.week': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_manufacturing_date_week = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module Manufacturing Date : Year 20%02X Week %02X\n",
		  module_manufacturing_date_year,
		  module_manufacturing_date_week);

  if (FIID_OBJ_GET (obj_record,
		    "module_serial_number",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_serial_number': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_serial_number = val;

  /* Serial number stored little endian, so output in that order */
  pstdout_printf (state_data->pstate,
		  "  FRU Module Serial Number : %02X%02X%02X%02X\n",
		  module_serial_number & 0x000000FF,
		  (module_serial_number & 0x0000FF00) >> 8,
		  (module_serial_number & 0x00FF0000) >> 16,
		  (module_serial_number & 0xFF000000) >> 24);

  memset (module_part_number, '\0', IPMI_FRU_STR_BUFLEN + 1);

  if ((module_part_number_len = fiid_obj_get_data (obj_record,
						   "module_part_number",
						   module_part_number,
						   IPMI_FRU_STR_BUFLEN)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'module_part_number': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Module Part Number : %s\n",
		  module_part_number);

  if (FIID_OBJ_GET (obj_record,
		    "module_revision_code",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_revision_code': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_revision_code = val;

  /* Revision Code vendor defined, so just output hex  */
  pstdout_printf (state_data->pstate,
		  "  FRU Module Revision Code : 0x%04X\n",
		  module_revision_code);

  if (FIID_OBJ_GET (obj_record,
		    "number_of_continuation_codes_dram_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_continuation_codes_dram_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  number_of_continuation_codes_dram_manufacturer = val;

  if (FIID_OBJ_GET (obj_record,
		    "last_non_zero_dram_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'last_non_zero_dram_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  last_non_zero_dram_manufacturer = val;

  dram_manufacturer_str = ipmi_jedec_manufacturer_id_search (number_of_continuation_codes_dram_manufacturer,
							     last_non_zero_dram_manufacturer);
  if (dram_manufacturer_str)
    pstdout_printf (state_data->pstate,
		    "  FRU DRAM Manufacturer : %s\n",
		    dram_manufacturer_str);
  else
    pstdout_printf (state_data->pstate,
		    "  FRU DRAM Manufacturer : Unrecognized\n");

 out:
  rv = 0;

 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

static int
_ipmi_fru_output_dimm_ddr4 (ipmi_fru_state_data_t *state_data,
			    const void *areabuf,
			    unsigned int area_length)
{
  fiid_obj_t obj_record = NULL;
  uint8_t total_sdram_capacity;
  char *total_sdram_capacity_str = NULL;
  uint32_t total_sdram_capacity_val = 0; 
  uint8_t total_sdram_capacity_valid;
  uint8_t bank_address_bits;
  char *bank_address_bits_str = NULL;
  uint8_t bank_group_bits;
  char *bank_group_bits_str = NULL;
  uint8_t die_count;
  char *die_count_str;
  uint8_t sdram_package_type;
  char *sdram_package_type_str;
  uint8_t module_nominal_voltage_1_2;
  uint8_t module_nominal_voltage_TBD1;
  uint8_t module_nominal_voltage_TBD2;
  uint8_t sdram_device_width;
  char *sdram_device_width_str = NULL;
  uint32_t sdram_device_width_val = 0; 
  uint8_t sdram_device_width_valid;
  uint8_t number_of_package_ranks_per_dimm;
  char *number_of_package_ranks_per_dimm_str = NULL;
  uint32_t number_of_package_ranks_per_dimm_val = 0; 
  uint8_t number_of_package_ranks_per_dimm_valid;
  uint8_t primary_bus_width;
  char *primary_bus_width_str = NULL;
  uint32_t primary_bus_width_val = 0; 
  uint8_t primary_bus_width_valid;
  uint8_t bus_width_extension;
  char *bus_width_extension_str;
  uint32_t total_memory_capacity;
  char *total_memory_capacity_units_str = NULL;
  uint8_t number_of_continuation_codes_module_manufacturer;
  uint8_t last_non_zero_module_manufacturer;
  char *module_manufacturer_str;
  uint8_t module_manufacturing_date_year;
  uint8_t module_manufacturing_date_week;
  uint32_t module_serial_number;
  char module_part_number[IPMI_FRU_STR_BUFLEN + 1];
  int module_part_number_len;
  uint16_t module_revision_code;
  uint8_t number_of_continuation_codes_dram_manufacturer;
  uint8_t last_non_zero_dram_manufacturer;
  char *dram_manufacturer_str;
  uint8_t dram_stepping;
  uint64_t val;
  int block_len;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);
 
  if (!(obj_record = fiid_obj_create (tmpl_fru_dimm_spd_ddr4_record)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
			areabuf,
			area_length) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set_all: %s\n",
		       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }

  if ((block_len = fiid_template_block_len_bytes (tmpl_fru_dimm_spd_ddr4_record,
						  "spd_bytes_used",
						  "dram_stepping")) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_template_block_len_bytes: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (area_length < block_len)
    {
      pstdout_printf (state_data->pstate, "\n");
      pstdout_printf (state_data->pstate,
                      "  FRU Error: %s\n",
                      ipmi_fru_ctx_strerror (IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID));
      goto out;
    }

  if (FIID_OBJ_GET (obj_record,
		    "total_sdram_capacity",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'total_sdram_capacity': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  total_sdram_capacity = val;
  
  switch (total_sdram_capacity)
    {
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_256_MB:
      total_sdram_capacity_str = "256 Mb";
      total_sdram_capacity_val = 256;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "MB";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_512_MB:
      total_sdram_capacity_str = "512 Mb";
      total_sdram_capacity_val = 512;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "MB";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_1_GB:
      total_sdram_capacity_str = "1 Gb";
      total_sdram_capacity_val = 1024;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_2_GB:
      total_sdram_capacity_str = "2 Gb";
      total_sdram_capacity_val = 2048;
      total_sdram_capacity_valid = 1; 
      total_memory_capacity_units_str = "GB";
     break;
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_4_GB:
      total_sdram_capacity_str = "4 Gb";
      total_sdram_capacity_val = 4096;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_8_GB:
      total_sdram_capacity_str = "8 Gb";
      total_sdram_capacity_val = 8192;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_16_GB:
      total_sdram_capacity_str = "16 Gb";
      total_sdram_capacity_val = 16384;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_TOTAL_SDRAM_CAPACITY_32_GB:
      total_sdram_capacity_str = "32 Gb";
      total_sdram_capacity_val = 32768;
      total_sdram_capacity_valid = 1;
      total_memory_capacity_units_str = "GB";
      break;
    default:
      total_sdram_capacity_str = "Unknown";
      total_sdram_capacity_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Total SDRAM Capacity : %s\n",
		  total_sdram_capacity_str);

  if (FIID_OBJ_GET (obj_record,
		    "bank_address_bits",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bank_address_bits': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  bank_address_bits = val;

  switch (bank_address_bits)
    {
    case IPMI_FRU_DIMMSPD_DDR4_BANK_ADDRESS_BITS_2:
      bank_address_bits_str = "4 Banks";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_BANK_ADDRESS_BITS_3:
      bank_address_bits_str = "8 Banks";
      break;
    default:
      bank_address_bits_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Memory Banks : %s\n",
		  bank_address_bits_str);

  if (FIID_OBJ_GET (obj_record,
		    "bank_group_bits",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bank_group_bits': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  bank_group_bits = val;

  switch (bank_group_bits)
    {
    case IPMI_FRU_DIMMSPD_DDR4_BANK_GROUP_BITS_0:
      bank_group_bits_str = "0 bank groups";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_BANK_GROUP_BITS_1:
      bank_group_bits_str = "2 bank groups";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_BANK_GROUP_BITS_2:
      bank_group_bits_str = "4 bank groups";
      break;
    default:
      bank_group_bits_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Memory Bank Groups : %s\n",
		  bank_group_bits_str);

  if (FIID_OBJ_GET (obj_record,
		    "die_count",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'die_count': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  die_count = val;

  switch (die_count)
    {
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_SINGLE_DIE:
      die_count_str = "Single die";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_2_DIE:
      die_count_str = "2 die";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_3_DIE:
      die_count_str = "3 die";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_4_DIE:
      die_count_str = "4 die";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_5_DIE:
      die_count_str = "5 die";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_6_DIE:
      die_count_str = "6 die";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_7_DIE:
      die_count_str = "7 die";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DIE_COUNT_8_DIE:
      die_count_str = "8 die";
      break;
    default:
      die_count_str = "Unknown";
      break;
    } 

  pstdout_printf (state_data->pstate,
		  "  FRU Die Count : %s\n",
		  die_count_str);

  if (FIID_OBJ_GET (obj_record,
		    "sdram_package_type",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sdram_package_type': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  sdram_package_type = val;

  switch (sdram_package_type)
    {
    case IPMI_FRU_DIMMSPD_DDR4_SDRAM_PACKAGE_TYPE_MONOLITHIC_DRAM_DEVICE:
      sdram_package_type_str = "Monolithic DRAM Device";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_SDRAM_PACKAGE_TYPE_NON_MONOLITHIC_DRAM_DEVICE:
      sdram_package_type_str = "Non-Monolithic Device";
      break;
    default:
      sdram_package_type_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU SDRAM Package Type : %s\n",
		  sdram_package_type_str);
  
  if (FIID_OBJ_GET (obj_record,
		    "module_nominal_voltage.dram_vdd.1_2_operable",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_nominal_voltage.dram_vdd.1_2_operable': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_nominal_voltage_1_2 = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module Nominal Voltage 1.2 V : %s\n",
		  (module_nominal_voltage_1_2 == IPMI_FRU_DIMMSPD_DDR4_VDD_1_2_OPERABLE) ? "Operable" : "Not Operable");

  if (FIID_OBJ_GET (obj_record,
		    "module_nominal_voltage.dram_vdd.TBD1_operable",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_nominal_voltage.dram_vdd.TBD1_operable': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_nominal_voltage_TBD1 = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module Nominal Voltage TBD1 V : %s\n",
		  (module_nominal_voltage_TBD1 == IPMI_FRU_DIMMSPD_DDR4_VDD_TBD1_OPERABLE) ? "Operable" : "Not Operable");

  if (FIID_OBJ_GET (obj_record,
		    "module_nominal_voltage.dram_vdd.TBD2_operable",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_nominal_voltage.dram_vdd.TBD2_operable': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_nominal_voltage_TBD2 = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module Nominal Voltage TBD2 V : %s\n",
		  (module_nominal_voltage_TBD2 == IPMI_FRU_DIMMSPD_DDR4_VDD_TBD2_OPERABLE) ? "Operable" : "Not Operable");




  if (FIID_OBJ_GET (obj_record,
		    "sdram_device_width",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sdram_device_width': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  sdram_device_width = val;

  switch (sdram_device_width)
    {
    case IPMI_FRU_DIMMSPD_DDR4_DEVICE_WIDTH_4_BITS:
      sdram_device_width_str = "4 bits";
      sdram_device_width_val = 4;
      sdram_device_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DEVICE_WIDTH_8_BITS:
      sdram_device_width_str = "8 bits";
      sdram_device_width_val = 8;
      sdram_device_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DEVICE_WIDTH_16_BITS:
      sdram_device_width_str = "16 bits";
      sdram_device_width_val = 16;
      sdram_device_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_DEVICE_WIDTH_32_BITS:
      sdram_device_width_str = "32 bits";
      sdram_device_width_val = 32;
      sdram_device_width_valid = 1;
      break;
    default:
      sdram_device_width_str = "Unknown";
      sdram_device_width_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU SDRAM Device Width : %s\n",
		  sdram_device_width_str);

  if (FIID_OBJ_GET (obj_record,
		    "number_of_package_ranks_per_dimm",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_package_ranks_per_dimm': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  number_of_package_ranks_per_dimm = val;

  /* achu: Yeah, a lot of unnecessary code here, but gonna keep
   * code style pattern the same as the above.
   */
  switch (number_of_package_ranks_per_dimm)
    {
    case IPMI_FRU_DIMMSPD_DDR4_NUMBER_OF_PACKAGE_RANKS_1:
      number_of_package_ranks_per_dimm_str = "1";
      number_of_package_ranks_per_dimm_val = 1;
      number_of_package_ranks_per_dimm_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_NUMBER_OF_PACKAGE_RANKS_2:
      number_of_package_ranks_per_dimm_str = "2";
      number_of_package_ranks_per_dimm_val = 2;
      number_of_package_ranks_per_dimm_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_NUMBER_OF_PACKAGE_RANKS_3:
      number_of_package_ranks_per_dimm_str = "3";
      number_of_package_ranks_per_dimm_val = 3;
      number_of_package_ranks_per_dimm_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_NUMBER_OF_PACKAGE_RANKS_4:
      number_of_package_ranks_per_dimm_str = "4";
      number_of_package_ranks_per_dimm_val = 4;
      number_of_package_ranks_per_dimm_valid = 1;
      break;
    default:
      number_of_package_ranks_per_dimm_str = "Unknown";
      number_of_package_ranks_per_dimm_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Number of Package Ranks : %s\n",
		  number_of_package_ranks_per_dimm_str);

  if (FIID_OBJ_GET (obj_record,
		    "primary_bus_width",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'primary_bus_width': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  primary_bus_width = val;

  switch (primary_bus_width)
    {
    case IPMI_FRU_DIMMSPD_DDR4_PRIMARY_BUS_WIDTH_8_BITS:
      primary_bus_width_str = "8 bits";
      primary_bus_width_val = 8;
      primary_bus_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_PRIMARY_BUS_WIDTH_16_BITS:
      primary_bus_width_str = "16 bits";
      primary_bus_width_val = 16;
      primary_bus_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_PRIMARY_BUS_WIDTH_32_BITS:
      primary_bus_width_str = "32 bits";
      primary_bus_width_val = 32;
      primary_bus_width_valid = 1;
      break;
    case IPMI_FRU_DIMMSPD_DDR4_PRIMARY_BUS_WIDTH_64_BITS:
      primary_bus_width_str = "64 bits";
      primary_bus_width_val = 64;
      primary_bus_width_valid = 1;
      break;
    default:
      primary_bus_width_str = "Unknown";
      primary_bus_width_valid = 0;
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Primary Bus Width : %s\n",
		  primary_bus_width_str);

  if (FIID_OBJ_GET (obj_record,
		    "bus_width_extension",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bus_width_extension': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  bus_width_extension = val;

  switch (bus_width_extension)
    {
    case IPMI_FRU_DIMMSPD_DDR4_BUS_WIDTH_EXTENSION_0_BITS:
      bus_width_extension_str = "0 bits";
      break;
    case IPMI_FRU_DIMMSPD_DDR4_BUS_WIDTH_EXTENSION_8_BITS:
      bus_width_extension_str = "8 bits";
      break;
    default:
      bus_width_extension_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Bus Width Extension : %s\n",
		  bus_width_extension_str);

  if (total_sdram_capacity_valid
      && sdram_device_width_valid
      && number_of_package_ranks_per_dimm_valid
      && primary_bus_width_valid)
    {
      /* Per JEDEC document, in section "Byte 8"
       *
       * SDRAM CAPCITY / 8 * PRIMARY BUS WIDTH / SDRAM WIDTH * RANKS 
       */
      total_memory_capacity = (total_sdram_capacity_val / 8) * (primary_bus_width_val / sdram_device_width_val) * number_of_package_ranks_per_dimm_val;

      pstdout_printf (state_data->pstate,
		      "  FRU Total Memory Capacity : %u %s\n",
		      total_memory_capacity,
		      total_memory_capacity_units_str);
    }
  else
    pstdout_printf (state_data->pstate,
		    "  FRU Total Memory Capacity : Unknown\n");

  if (FIID_OBJ_GET (obj_record,
		    "number_of_continuation_codes_module_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_continuation_codes_module_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  number_of_continuation_codes_module_manufacturer = val;

  if (FIID_OBJ_GET (obj_record,
		    "last_non_zero_module_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'last_non_zero_module_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  last_non_zero_module_manufacturer = val;

  module_manufacturer_str = ipmi_jedec_manufacturer_id_search (number_of_continuation_codes_module_manufacturer,
							       last_non_zero_module_manufacturer);
  if (module_manufacturer_str)
    pstdout_printf (state_data->pstate,
		    "  FRU Module Manufacturer : %s\n",
		    module_manufacturer_str);
  else
    pstdout_printf (state_data->pstate,
		    "  FRU Module Manufacturer : Unrecognized\n");

  if (FIID_OBJ_GET (obj_record,
		    "module_manufacturing_date.year",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_manufacturing_date.year': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_manufacturing_date_year = val;

  if (FIID_OBJ_GET (obj_record,
		    "module_manufacturing_date.week",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_manufacturing_date.week': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_manufacturing_date_week = val;

  pstdout_printf (state_data->pstate,
		  "  FRU Module Manufacturing Date : Year 20%02X Week %02X\n",
		  module_manufacturing_date_year,
		  module_manufacturing_date_week);

  if (FIID_OBJ_GET (obj_record,
		    "module_serial_number",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_serial_number': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_serial_number = val;

  /* Serial number stored little endian, so output in that order */
  pstdout_printf (state_data->pstate,
		  "  FRU Module Serial Number : %02X%02X%02X%02X\n",
		  module_serial_number & 0x000000FF,
		  (module_serial_number & 0x0000FF00) >> 8,
		  (module_serial_number & 0x00FF0000) >> 16,
		  (module_serial_number & 0xFF000000) >> 24);

  memset (module_part_number, '\0', IPMI_FRU_STR_BUFLEN + 1);

  if ((module_part_number_len = fiid_obj_get_data (obj_record,
						   "module_part_number",
						   module_part_number,
						   IPMI_FRU_STR_BUFLEN)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'module_part_number': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
		  "  FRU Module Part Number : %s\n",
		  module_part_number);

  if (FIID_OBJ_GET (obj_record,
		    "module_revision_code",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'module_revision_code': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  module_revision_code = val;

  /* Revision Code vendor defined, so just output hex  */
  pstdout_printf (state_data->pstate,
		  "  FRU Module Revision Code : 0x%04X\n",
		  module_revision_code);

  if (FIID_OBJ_GET (obj_record,
		    "number_of_continuation_codes_dram_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_continuation_codes_dram_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  number_of_continuation_codes_dram_manufacturer = val;

  if (FIID_OBJ_GET (obj_record,
		    "last_non_zero_dram_manufacturer",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'last_non_zero_dram_manufacturer': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  last_non_zero_dram_manufacturer = val;

  dram_manufacturer_str = ipmi_jedec_manufacturer_id_search (number_of_continuation_codes_dram_manufacturer,
							     last_non_zero_dram_manufacturer);
  if (dram_manufacturer_str)
    pstdout_printf (state_data->pstate,
		    "  FRU DRAM Manufacturer : %s\n",
		    dram_manufacturer_str);
  else
    pstdout_printf (state_data->pstate,
		    "  FRU DRAM Manufacturer : Unrecognized\n");

  if (FIID_OBJ_GET (obj_record,
		    "dram_stepping",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'dram_stepping': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  dram_stepping = val;

  /* Revision Code vendor defined, so just output hex  */
  pstdout_printf (state_data->pstate,
		  "  FRU Dram Stepping : 0x%02X\n",
		  dram_stepping);

 out:
  rv = 0;

 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}

int
ipmi_fru_output_dimm (ipmi_fru_state_data_t *state_data,
		      const void *areabuf,
		      unsigned int area_length)
{
  fiid_obj_t obj_record = NULL;
  uint8_t dram_device_type;
  char *dram_device_type_str = NULL;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (areabuf);
  assert (area_length);

  if (!(obj_record = fiid_obj_create (tmpl_fru_dimm_spd_ddr_header)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_record,
			areabuf,
			area_length) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set_all: %s\n",
		       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_record,
		    "dram_device_type",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'dram_device_type': %s\n",
                       fiid_obj_errormsg (obj_record));
      goto cleanup;
    }
  dram_device_type = val;

  switch (dram_device_type)
    {
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_STANDARD_FPM_DRAM:
      dram_device_type_str = "Standard FPM DRAM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_EDO:
      dram_device_type_str = "EDO";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_PIPELINED_NIBBLE:
      dram_device_type_str = "Pipelined Nibble";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_SDRAM:
      dram_device_type_str = "SDRAM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_ROM:
      dram_device_type_str = "ROM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR_SGRAM:
      dram_device_type_str = "DDR SGRAM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR_SDRAM:
      dram_device_type_str = "DDR SDRAM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR2_SDRAM:
      dram_device_type_str = "DDR2 SDRAM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR2_SDRAM_FB_DIMM:
      dram_device_type_str = "DDR2 SDRAM FB-DIMM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR2_SDRAM_FB_DIMM_PROBE:
      dram_device_type_str = "DDR2 SDRAM FB-DIMM PROBE";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR3_SDRAM:
      dram_device_type_str = "DDR3 SDRAM";
      break;
    case IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR4_SDRAM:
      dram_device_type_str = "DDR4 SDRAM";
      break;
    default:
      dram_device_type_str = "Unrecognized";
      break;
    }
  
  pstdout_printf (state_data->pstate,
		  "  FRU DRAM Device Type: %s\n",
		  dram_device_type_str);
		    
  if (dram_device_type == IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR3_SDRAM)
    {
      if (_ipmi_fru_output_dimm_ddr3 (state_data, areabuf, area_length) < 0)
	goto cleanup;
    }
  else if (dram_device_type == IPMI_FRU_DIMMSPD_DRAM_DEVICE_TYPE_DDR4_SDRAM)
    {
      if (_ipmi_fru_output_dimm_ddr4 (state_data, areabuf, area_length) < 0)
	goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_record);
  return (rv);
}
