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

#include "freeipmi/record-format/ipmi-fru-dimmspd-record-format.h"
#include "freeipmi/fiid/fiid.h"

#include "freeipmi-portability.h"

/* From "Annex K: Serial Presence Detect (SPD) for DDR3 SDRAM Modules"
 * and
 * "Annex L: Serial Presence Detect (SPD) for DDR4 SDRAM Modules"
 * 
 * Released by JEDEC
 */

fiid_template_t tmpl_fru_dimm_spd_ddr_header =
  { 
    /* Byte 0: Number of Bytes Used/ Number of Bytes in SPD Device / CRC Coverage */
    { 4, "spd_bytes_used", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "spd_bytes_total", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "crc_coverage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 1: SPD Revision (X.Y = encoding_level.additions_level) */
    { 4, "additions_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 2: Key Byte / DRAM Device Type */
    { 8, "dram_device_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 3: Key Byte / Module Type */
    { 4, "module_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_dimm_spd_ddr3_record =
  {
    /* Byte 0: Number of Bytes Used/ Number of Bytes in SPD Device / CRC Coverage */
    { 4, "spd_bytes_used", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "spd_bytes_total", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "crc_coverage", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 1: SPD Revision (X.Y = encoding_level.additions_level) */
    { 4, "additions_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 2: Key Byte / DRAM Device Type */
    { 8, "dram_device_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 3: Key Byte / Module Type */
    { 4, "module_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 4: SDRAM Density and Banks */
    /* in megabits */
    { 4, "total_sdram_capacity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    { 3, "bank_address_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 5: SDRAM Addressing */
    { 3, "column_address_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "row_address_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 6: Module Nominal Voltage, VDD */
    { 1, "module_minimum_nominal_voltage.1_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "module_minimum_nominal_voltage.1_35", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "module_minimum_nominal_voltage.1_25", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 7: Module Organization */
    { 3, "sdram_device_width", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "number_of_ranks", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 8: Module Memory Bus Width */
    { 3, "primary_bus_width", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "bus_width_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserve6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 9: Fine Timebase (FTB) Dividend / Divisor */
    { 4, "fine_timebase_divisor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "fine_timebase_dividend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 10: Medium Timebase (MTB) Dividend */
    { 8, "medium_timebase_dividend", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 11: Medium Timebase (MTB) Divisor */
    { 8, "medium_timebase_divisor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 12: SDRAM Minimum Cycle Time (t_ck min) (MTB Units) */
    { 8, "minimum_sdram_cycle_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 14-15: CAS Latencies Supported */
    { 16, "cas_latencies_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 16: Minimum CAS Latency Time (t_aa min) (MTB Units) */
    { 8, "minimum_sdram_cas_latency_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 17: Minimum Write Recovery Time (t_wr min) (MTB Units) */
    { 8, "minimum_write_recovery_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 18: Minimum RAS# to CAS# Delay Time (t_rcd min) */
    { 8, "minimum_ras_to_cas_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 19: Minimum Row Active to Row Active Delay Time (t_rrd min) (MTB Units) */
    { 8, "minimum_row_active_to_row_active_delay", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 20: Minimum Row Precharge Delay Time (t_rp min) (MTB Units) */
    { 8, "minimum_row_precharge_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 21: Upper Nibbles for t_ras and t_rc */
    { 4, "t_ras_msn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "t_rc_msn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 22: Minimum Active to Precharge Delay Time (t_ras min), LSB (MTB Units) */
    { 8, "minimum_active_to_precharge_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 23: Minimum Active to Active/Refresh Delay Time (t_rc min), LSB (MTB Units) */
    { 8, "minimum_active_to_active_refresh_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 24-25: Minimum Refresh Recovery Delay Time (t_rfc min) (MTB Units) */
    { 16, "minimum_refresh_recovery_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 26: Minimum Internal Write to Read Command Delay Time (t_wtr min) (MTB Units) */
    { 8, "internal_write_to_read_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 27: Minimum Internal Read to Precharge Command Delay Time (t_rtp min) (MTB Units) */
    { 8, "internal_read_to_precharge_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 28: Upper Nibble for t_faw */
    { 4, "t_faw_msn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 29: Minimum Four Active Window Delay Time (t_faw min), LSB (MTB Units) */ 
    { 8, "minimum_four_active_window_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 30: SDRAM Optional Features */
    { 1, "rzq_6_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "rzq_7_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "dll_off_mode_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 31: SDRAM Thermal and Refresh Options */
    { 1, "extended_temperature_range", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "extended_temperature_refresh_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "auto_self_refresh", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "on_die_thermal_sensor_readout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "partial_array_self_refresh", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 32: Module Thermal Sensor */
    { 7, "thermal_sensor_accuracy", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "thermal_sensor_incorporated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 33: SDRAM Device Type */
    { 2, "signal_loading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "die_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sdram_device_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 34: Fine Offset for SDRAM Minimum Cycle Time (tCKmin) */
    { 8, "fine_offset_for_sdram_minimum_cycle_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 35: Fine Offset for Minimum CAS Latency Time (tAAmin) */
    { 8, "fine_offset_for_minimum_cas_latency_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 36: Fine Offset for Minimum RAS# to CAS# Delay Time (tRCDmin) */
    { 8, "fine_offset_for_minimum_ras_to_cas_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 37: Minimum Row Precharge Delay Time (tRPmin) */
    { 8, "minimum_row_precharge_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 38: Fine Offset for Minimum Active to Active/Refresh Delay Time (tRCmin) */
    { 8, "fine_offset_for_minimum_active_to_active_refresh_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 39-59: Reserved */
    { 168, "reserved12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 60-116: Module-Specific Section */
    { 456, "module_specific_section", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 117-125: Unique Module ID */
    { 7, "number_of_continuation_codes_module_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "odd_parity_byte_module_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "last_non_zero_module_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "module_manufacturing_location", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "module_manufacturing_date.year", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
    { 8, "module_manufacturing_date.week", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
    { 32, "module_serial_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 126-127: SPD Cyclical Redundancy Code (CRC) */
    { 16, "crc", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 128-145: Module Part Number */
    { 144, "module_part_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* ASCII */
    /* Bytes 146-147: Module Revision Code */
    { 16, "module_revision_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 148-149: DRAM Manufacturer ID Code */
    { 7, "number_of_continuation_codes_dram_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "odd_parity_byte_dram_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "last_non_zero_dram_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 150-175: Manufacturer's Specific Data */
    { 208, "manufacturer_specific_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 176-255: Manufacturer's Specific Data */
    { 640, "open_for_customer_use", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_dimm_spd_ddr4_record =
  {
    /* Byte 0: Number of Serial PD Bytes Written / SPD Device Size / CRC Coverage */
    { 4, "spd_bytes_used", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "spd_bytes_total", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 1: SPD Revision */
    { 4, "additions_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 2: Key Byte / DRAM Device Type */
    { 8, "dram_device_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 3: Key Byte / Module Type */
    { 4, "module_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 4: SDRAM Density and Banks */
    /* in megabits */
    { 4, "total_sdram_capacity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    { 2, "bank_address_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "bank_group_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 5: SDRAM Addressing */
    { 3, "column_address_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "row_address_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 6: SDRAM Package Type */
    { 2, "signal_loading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "die_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sdram_package_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 7: SDRAM Optional Features */
    { 4, "maximum_activate_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "maximum_activate_window", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 8: SDRAM Thermal and Refresh Options */
    { 8, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 9: Other SDRAM Optional Features */
    { 6, "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "post_package_repair", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 10: Reserved */
    { 8, "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 11: Module Nominal Voltage, VDD */
    { 1, "module_nominal_voltage.dram_vdd.1_2_operable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "module_nominal_voltage.dram_vdd.1_2_endurant", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "module_nominal_voltage.dram_vdd.TBD1_operable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "module_nominal_voltage.dram_vdd.TBD1_endurant", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "module_nominal_voltage.dram_vdd.TBD2_operable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "module_nominal_voltage.dram_vdd.TBD2_endurant", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 12: Module Organization */
    { 3, "sdram_device_width", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "number_of_package_ranks_per_dimm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 13: Module Memory Bus Width */
    { 3, "primary_bus_width", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "bus_width_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserve11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 14: Module Thermal Sensor */
    { 7, "reserved12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "thermal_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 15: Extended module type */
    { 4, "extended_base_module_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 16: Reserved */
    { 8, "reserved14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 17: Timebases */
    { 2, "fine_timebase", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "medium_timebase", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved15", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 18: SDRAM Minimum Cycle Time (t_ckavg min) */
    { 8, "minimum_sdram_cycle_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 19: SDRAM Maximum Cycle Time (t_ckavg max) */
    { 8, "maximum_sdram_cycle_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 20-23: CAS Latencies Supported */
    { 32, "cas_latencies_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 24: Minimum CAS Latency Time (t_aa min) */
    { 8, "minimum_sdram_cas_latency_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 25: Minimum RAS to CAS Delay Time (t_rcd min) */
    { 8, "minimum_sdram_ras_to_cas_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 26: Minimum Row Precharge Delay Time (t_rp min) */
    { 8, "minimum_row_precharge_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 27: Upper Nibbles for t_ras min and t_rc min */
    { 4, "t_ras_min_msn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "t_rc_min_msn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 28: Minimum Active to Precharge Delay Time (t_ras min), LSB */
    { 8, "minimum_active_to_precharge_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 29: Minimum Active to Active/Refresh Delay Time (t_rc min), LSB */
    { 8, "minimum_active_to_active_refresh_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 30-31: Minimum Refresh Recovery Delay Time (t_RFC1 min) */
    { 16, "minimum_refresh_recovery_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 32-33: Minimum Refresh Recovery Delay Time (t_RFC2 min) */
    { 16, "minimum_refresh_recovery_delay_time1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 34-35: Minimum Refresh Recovery Delay Time (t_RFC4 min) */
    { 16, "minimum_refrsh_recovery_delay_time4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 36: Minimum Four Activate Window Time (t_FAW min), Most Significant Nibble */
    { 4, "tfaw_msn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved16", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 37: Minimum Four Activate Window Time (t_FAW min), Least Significant Byte */
    { 8, "minimum_four_activate_window_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 38: Minimum Activate to Activate Delay Time (t_RRD_S min), different bank group */
    { 8, "minimum_active_to_active_refresh_time_different_bank_group", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 39: Minimum Activate to Activate Delay Time (t_RRD_S min), same bank group */
    { 8, "minimum_active_to_active_refresh_time_same_bank_group", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 40: Minimum CAS to CAS Delay Time (t_CCD_L min), same bank group */
    { 8, "minimum_cas_to_cas_time_same_bank_group", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 41-59: Reserved */
    { 152, "reserved17", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 60-77: Connector to SDRAM Bit Mapping */
    { 144, "connector_to_sdram_bit_mapping", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 78-116: Reserved */
    { 312, "reserved18", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 117: Fine Offset for Minimum CAS to CAS Delay Time (t_CCD_L min), same bank group */
    { 8, "fine_offset_for_minimum_cas_to_cas_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 118: Fine Offset for Minimum Activate to Activate Delay Time (t_RRD_L min), same bank group */
    { 8, "fine_offset_for_minimum_activate_to_activate_delay_time_same_bank_group", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 119: Fine Offset for Minimum Activate to Activate Delay Time (t_RRD_S min), different bank group */
    { 8, "fine_offset_for_minimum_activate_to_activate_delay_time_different_bank_group", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 120: Fine Offset for Minimum Activate to Activate/Refresh Delay Time (t_RC min) */
    { 8, "fine_offset_for_minimum_activate_to_activate_refresh_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 121: Fine Offset for Minimum Row Precharge Delay Time (t_RP min) */
    { 8, "fine_offset_for_minimum_row_precharge_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 122: Fine Offset for Minimum RAS to CAS Delay Time (t_RCD min) */
    { 8, "fine_offset_for_minimum_ras_to_cas_delay_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 123: Fine Offset for Minimum CAS Latency Time (t_AA min) */
    { 8, "fine_offset_for_minimum_cas_latency_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 124: Fine Offset for SDRAM Maximum Cycle Time (t_CKAVG max) */
    { 8, "fine_offset_for_sdram_maximum_cycle_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 125: Fine Offset for SDRAM Minimum Cycle Time (t_CKAVG min) */
    { 8, "fine_offset_for_sdram_minimum_cycle_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 126-127: CRC for Base Configuration Section */
    { 16, "crc", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 128-255: Module Specific Parameters */
    { 1024, "module_specific_sections", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 256-319: Reserved */
    { 512, "reserved19", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 320-383: Manufacturing Information */
    /* Bytes 320-321: Module Manufacturer's ID Code */
    { 7, "number_of_continuation_codes_module_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "odd_parity_byte_module_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "last_non_zero_module_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 322: Module Manufacturing Location */
    { 8, "module_manufacturing_location", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 323-324: Module Manufacturing Date */
    { 8, "module_manufacturing_date.year", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
    { 8, "module_manufacturing_date.week", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
    /* Bytes 325-328: Module Serial Number */
    { 32, "module_serial_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 329-348: Module Part Number */
    { 160, "module_part_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* ASCII */
    /* Byte 349: Module Revision Code */
    { 16, "module_revision_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 350-351: DRAM Manufacturer's ID Code */
    { 7, "number_of_continuation_codes_dram_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "odd_parity_byte_dram_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "last_non_zero_dram_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 352: DRAM Stepping */
    { 8, "dram_stepping", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 353-381: Module Manufacturer's Specific Data */
    { 232, "module_manufacturer_specific_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 382-383: Reserved */
    { 16, "reserved20", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 384-511: End User Programmable */
    { 1024, "end_user_programmable", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_dimm_spd_ddr4_module_specific_unbuffered_memory_module =
  {
    /* Byte 128: Raw Card Extension, Module Nominal Height */
    { 5, "module_nominal_height", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "raw_card_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 129: Module Maximum Thickness */
    { 4, "module_maximum_thickness_front", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "module_maximum_thickness_back", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 130: Reference Raw Card Used */
    { 5, "reference_raw_card", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reference_raw_card_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reference_raw_card_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 131: Address Mapping from Edge Connector to DRAM */
    { 1, "rank_1_mapping", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 132-253: Reserved */
    { 976, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 254-255: CRC */
    { 16, "crc", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_dimm_spd_ddr4_module_specific_registered_memory_module =
  {
    /* Byte 128: Raw Card Extension, Module Nominal Height */
    { 5, "module_nominal_height", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "raw_card_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 129: Module Maximum Thickness */
    { 4, "module_maximum_thickness_front", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "module_maximum_thickness_back", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 130: Reference Raw Card Used */
    { 5, "reference_raw_card", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reference_raw_card_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reference_raw_card_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 131: DIMM Module Attributes */
    { 2, "number_of_registers_used_on_rdimm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "number_of_rows_of_drams_on_rdimm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 132: RDIMM Thermal Heat Spreader Solution */
    { 7, "heat_spreader_thermal_characteristics", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "heat_spreader_solution", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 133-134: Register Manufacturer ID Code */
    { 7, "number_of_continuation_codes_register_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "odd_parity_byte_register_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "last_non_zero_register_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 135: Register Revision Number */
    { 8, "register_revision_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 136: Address Mapping from Register to DRAM */
    { 1, "rank_1_mapping", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 137: Register Output Drive Strength for Control */
    { 2, "cke", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "odt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "command_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "chip_select", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 138: Register Output Drive Strength for CK */
    { 2, "y0_y2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "y1_y3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 139-253: Reserved */
    { 920, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 254-255: CRC */
    { 16, "crc", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_fru_dimm_spd_ddr4_module_specific_load_reduction_memory_module =
  {
    /* Byte 128: Raw Card Extension, Module Nominal Height */
    { 5, "module_nominal_height", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "raw_card_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 129: Module Maximum Thickness */
    { 4, "module_maximum_thickness_front", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "module_maximum_thickness_back", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 130: Reference Raw Card Used */
    { 5, "reference_raw_card", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reference_raw_card_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reference_raw_card_extension", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 131: DIMM Module Attributes */
    { 2, "number_of_registers_used_on_lrdimm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "number_of_rows_of_drams_on_lrdimm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 132: LRDIMM Thermal Heat Spreader Solution */
    { 7, "heat_spreader_thermal_characteristics", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "heat_spreader_solution", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 133-134: Register and Data Buffer Manufacturer ID Code */
    { 7, "number_of_continuation_codes_memory_buffer_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "odd_parity_byte_memory_buffer_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "last_non_zero_memory_buffer_manufacturer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 135: Register Revision Number */
    { 8, "register_revision_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 136: Address Mapping from Register to DRAM */
    { 1, "odd_rank_mapping", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 137: Register Output Drive Strength for Control and Command/Address */
    { 2, "cke", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "odt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "command_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "chip_select", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 138: Register Output Drive Strength for CK */
    { 2, "y0_y2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "y1_y3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 139: Drive Buffer Revision Number */
    { 8, "drive_buffer_revision_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 140: DRAM VrefDQ for Package Rank 0 */
    { 6, "dram_vrefq_for_package_rank0_drams", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 141: DRAM VrefDQ for Package Rank 1 */
    { 6, "dram_vrefq_for_package_rank1_drams", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 142: DRAM VrefDQ for Package Rank 2 */
    { 6, "dram_vrefq_for_package_rank2_drams", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 143: DRAM VrefDQ for Package Rank 3 */
    { 6, "dram_vrefq_for_package_rank3_drams", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 144: Data Buffer VrefDQ for DRAM Interface */
    { 8, "data_buffer_vrefq_for_dram", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 145: Data Buffer MDQ Drive Strength and RTT for data rate <= 1866 */
    { 3, "dram_interface_mdq_read_termination_strength_lt_1866", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_interface_mdq_drive_strength_lt_1866", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 146: Data Buffer MDQ Drive Strength and RTT for 1866 < data rate <= 2400 */
    { 3, "dram_interface_mdq_read_termination_strength_lt_2400", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_interface_mdq_drive_strength_lt_2400", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 147: Data Buffer MDQ Drive Strength and RTT for 2400 < data rate <= 3200 */
    { 3, "dram_interface_mdq_read_termination_strength_lt_3200", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_interface_mdq_drive_strength_lt_3200", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 148: DRAM Drive Strength (for data rates <= 1866, 1866 < data rate < 2400, and 2400 < data rate <= 3200) */
    { 2, "dram_drive_strength_lt_1866", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "dram_drive_strength_lt_2400", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "dram_drive_strength_lt_3200", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 149: DRAM ODT (RTT_WR, RTT_NOM) for data rate <= 1866 */
    { 3, "dram_odt_strength_rtt_nom_lt_1866", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_odt_strength_rtt_wr_lt_1866", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved15", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 150: DRAM ODT (RTT_WR, RTT_NOM) for 1866 < data rate <= 2400 */
    { 3, "dram_odt_strength_rtt_nom_lt_2400", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_odt_strength_rtt_wr_lt_2400", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved16", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 151: DRAM ODT (RTT_WR, RTT_NOM) for 2400 < data rat e<= 3200 */
    { 3, "dram_odt_strength_rtt_nom_lt_3200", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_odt_strength_rtt_wr_lt_3200", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved17", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 152: DRAM ODT (RTT_PARK) for data rate <= 1866 */
    { 3, "dram_odt_strength_rtt_park_ranks_0_1_lt_1866", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_odt_strength_rtt_park_ranks_2_3_lt_1866", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved18", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 153: DRAM ODT (RTT_PARK) for 1866 < data rate <= 2400 */
    { 3, "dram_odt_strength_rtt_park_ranks_0_1_lt_2400", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_odt_strength_rtt_park_ranks_2_3_lt_2400", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved19", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Byte 153: DRAM ODT (RTT_PARK) for 2400 < data rate <= 3200 */
    { 3, "dram_odt_strength_rtt_park_ranks_0_1_lt_3200", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "dram_odt_strength_rtt_park_ranks_2_3_lt_3200", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved20", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 139-253: Reserved */
    { 792, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Bytes 254-255: CRC */
    { 16, "crc", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };
