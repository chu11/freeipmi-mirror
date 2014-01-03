/*****************************************************************************\
 *  $Id: ipmi-fru-information-record-format.c,v 1.13 2010-07-28 21:31:25 chu11 Exp $
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include "freeipmi/record-format/ipmi-fru-information-record-format.h"
#include "freeipmi/fiid/fiid.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_fru_common_header =
  {
    { 4, "format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "internal_use_area_starting_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "chassis_info_area_starting_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* named "board area" in spec, assuming that's a typo */
    { 8, "board_info_area_starting_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "product_info_area_starting_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "multirecord_area_starting_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "pad", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_info_area_header =
  {
    { 4, "format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "info_area_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_multirecord_area_header =
  {
    { 8, "record_type_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "record_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "end_of_list", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "header_checksum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_power_supply_information =
  {
    { 12, "overall_capacity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "peak_va", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "inrush_current", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "inrush_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "low_end_input_voltage_range_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "high_end_input_voltage_range_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "low_end_input_voltage_range_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "high_end_input_voltage_range_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "low_end_input_frequency_range", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "high_end_input_frequency_range", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* FRU Revision 1.2 renames this "Input dropout tolerance in ms", keep legacy name */
    { 8, "ac_dropout_tolerance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "predictive_fail_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "power_factor_correction", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "autoswitch", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "hot_swap_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "tachometer_pulses_per_rotation_predictive_fail_polarity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 12, "peak_capacity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "hold_up_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "voltage_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "voltage_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "total_combined_wattage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "predictive_fail_tachometer_lower_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_dc_output =
  {
    { 4, "output_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standby", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "nominal_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* FRU Revision 1.2 renames this "Maximum negative voltage", keep legacy name */
    { 16, "maximum_negative_voltage_deviation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* FRU Revision 1.2 renames this "Maximum positive voltage", keep legacy name */
    { 16, "maximum_positive_voltage_deviation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "ripple_and_noise_pk_pk", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum_current_draw", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_current_draw", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_dc_load =
  {
    { 4, "output_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "nominal_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Spec abbreviates specified to 'spec'd', so we keep it */
    { 16, "specd_minimum_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "specd_maximum_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "specd_ripple_and_noise_pk_pk", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum_current_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_current_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* Note: At most 256*8 bits according to max sub-record types */
fiid_template_t tmpl_fru_management_access_record =
  {
    { 8, "sub_record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2048, "record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_base_compatibility_record =
  {
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_id_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "compatibility_base", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "compatibility_code_start_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4096, "code_range_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_extended_compatibility_record =
  {
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "entity_id_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "compatibility_base", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "compatibility_code_start_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4096, "code_range_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_extended_dc_output =
  {
    { 4, "output_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "current_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standby", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "nominal_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_negative_voltage_deviation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_positive_voltage_deviation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "ripple_and_noise_pk_pk", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum_current_draw", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_current_draw", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_extended_dc_load =
  {
    { 4, "output_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "current_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "nominal_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Spec abbreviates specified to 'spec'd', so we keep it */
    { 16, "specd_minimum_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "specd_maximum_voltage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "specd_ripple_and_noise_pk_pk", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "minimum_current_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "maximum_current_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_oem_record =
  {
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4096, "oem_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };
