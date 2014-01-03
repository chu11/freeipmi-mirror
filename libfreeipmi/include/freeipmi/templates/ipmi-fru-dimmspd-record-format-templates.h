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

#ifndef IPMI_FRU_DIMMSPD_RECORD_FORMAT_TEMPLATES_H
#define IPMI_FRU_DIMMSPD_RECORD_FORMAT_TEMPLATES_H

#ifdef __cplusplus
extern "C" {
#endif

  /* This header file is for documentation only */

#if 0

Please see fiid.h for details concerning the fiid interface.

The following list the configurable fields of individual packet/record
templates in FreeIPMI.  Each field is listed as a list of the
following.

{ bits, "field name", field flag, field flag, ... }

bits - indicates the number of bits in the field

field name - indicates the name of the field, used for getting/setting
             fields in the fiid API.

field flags - flags indicating qualities of the field.  The following
              qualities may exist for each field.

    REQUIRED - field is required for the packet/record
    OPTIONAL - field is optional for the packet/record

    LENGTH-FIXED - field length is fixed at the number of bits listed

    LENGTH-VARIABLE - field length is variable for the number of bits
                      listed

    MAKES-PACKET-SUFFICIENT - indicates field or fields are
                              "sufficient" to make a packet/record valid
  and not malformed, but not necessarily a
                              complete packet/record.

DDR3 SDRAM SPD
--------------

FIID Template: tmpl_fru_dimm_spd_ddr3_record

    /* Byte 0: Number of Bytes Used/ Number of Bytes in SPD Device / CRC Coverage */
    { 4, "spd_bytes_used", REQUIRED, LENGTH-FIXED }
    { 3, "spd_bytes_total", REQUIRED, LENGTH-FIXED }
    { 1, "crc_coverage", REQUIRED, LENGTH-FIXED }
    /* Byte 1: SPD Revision (X.Y = encoding_level.additions_level) */
    { 4, "additions_level", REQUIRED, LENGTH-FIXED }
    { 4, "encoding_level", REQUIRED, LENGTH-FIXED }
    /* Byte 2: Key Byte / DRAM Device Type */
    { 8, "dram_device_type", REQUIRED, LENGTH-FIXED }
    /* Byte 3: Key Byte / Module Type */
    { 4, "module_type", REQUIRED, LENGTH-FIXED }
    { 4, "reserved1", REQUIRED, LENGTH-FIXED }
    /* Byte 4: SDRAM Density and Banks */
    /* in megabits */
    { 4, "total_sdram_capacity", REQUIRED, LENGTH-FIXED } 
    { 3, "bank_address_bits", REQUIRED, LENGTH-FIXED }
    { 1, "reserved2", REQUIRED, LENGTH-FIXED }
    /* Byte 5: SDRAM Addressing */
    { 3, "column_address_bits", REQUIRED, LENGTH-FIXED }
    { 3, "row_address_bits", REQUIRED, LENGTH-FIXED }
    { 2, "reserved3", REQUIRED, LENGTH-FIXED }
    /* Byte 6: Module Nominal Voltage, VDD */
    { 1, "module_minimum_nominal_voltage.1_5", REQUIRED, LENGTH-FIXED }
    { 1, "module_minimum_nominal_voltage.1_35", REQUIRED, LENGTH-FIXED }
    { 1, "module_minimum_nominal_voltage.1_25", REQUIRED, LENGTH-FIXED }
    { 5, "reserved4", REQUIRED, LENGTH-FIXED }
    /* Byte 7: Module Organization */
    { 3, "sdram_device_width", REQUIRED, LENGTH-FIXED }
    { 3, "number_of_ranks", REQUIRED, LENGTH-FIXED }
    { 2, "reserved5", REQUIRED, LENGTH-FIXED }
    /* Byte 8: Module Memory Bus Width */
    { 3, "primary_bus_width", REQUIRED, LENGTH-FIXED }
    { 2, "bus_width_extension", REQUIRED, LENGTH-FIXED }
    { 3, "reserve6", REQUIRED, LENGTH-FIXED }
    /* Byte 9: Fine Timebase (FTB) Dividend / Divisor */
    { 4, "fine_timebase_divisor", REQUIRED, LENGTH-FIXED }
    { 4, "fine_timebase_dividend", REQUIRED, LENGTH-FIXED }
    /* Byte 10: Medium Timebase (MTB) Dividend */
    { 8, "medium_timebase_dividend", REQUIRED, LENGTH-FIXED }
    /* Byte 11: Medium Timebase (MTB) Divisor */
    { 8, "medium_timebase_divisor", REQUIRED, LENGTH-FIXED }
    /* Byte 12: SDRAM Minimum Cycle Time (t_ck min) (MTB Units) */
    { 8, "minimum_sdram_cycle_time", REQUIRED, LENGTH-FIXED }
    { 8, "reserved7", REQUIRED, LENGTH-FIXED }
    /* Byte 14-15: CAS Latencies Supported */
    { 16, "cas_latencies_supported", REQUIRED, LENGTH-FIXED }
    /* Byte 16: Minimum CAS Latency Time (t_aa min) (MTB Units) */
    { 8, "minimum_sdram_cas_latency_time", REQUIRED, LENGTH-FIXED }
    /* Byte 17: Minimum Write Recovery Time (t_wr min) (MTB Units) */
    { 8, "minimum_write_recovery_time", REQUIRED, LENGTH-FIXED }
    /* Byte 18: Minimum RAS# to CAS# Delay Time (t_rcd min) */
    { 8, "minimum_ras_to_cas_delay", REQUIRED, LENGTH-FIXED }
    /* Byte 19: Minimum Row Active to Row Active Delay Time (t_rrd min) (MTB Units) */
    { 8, "minimum_row_active_to_row_active_delay", REQUIRED, LENGTH-FIXED }
    /* Byte 20: Minimum Row Precharge Delay Time (t_rp min) (MTB Units) */
    { 8, "minimum_row_precharge_time", REQUIRED, LENGTH-FIXED }
    /* Byte 21: Upper Nibbles for t_ras and t_rc */
    { 4, "t_ras_msn", REQUIRED, LENGTH-FIXED }
    { 4, "t_rc_msn", REQUIRED, LENGTH-FIXED }
    /* Byte 22: Minimum Active to Precharge Delay Time (t_ras min), LSB (MTB Units) */
    { 8, "minimum_active_to_precharge_time", REQUIRED, LENGTH-FIXED }
    /* Byte 23: Minimum Active to Active/Refresh Delay Time (t_rc min), LSB (MTB Units) */
    { 8, "minimum_active_to_active_refresh_time", REQUIRED, LENGTH-FIXED }
    /* Byte 24-25: Minimum Refresh Recovery Delay Time (t_rfc min) (MTB Units) */
    { 16, "minimum_active_to_active_refresh_time", REQUIRED, LENGTH-FIXED }
    /* Byte 26: Minimum Internal Write to Read Command Delay Time (t_wtr min) (MTB Units) */
    { 8, "internal_write_to_read_delay_time", REQUIRED, LENGTH-FIXED }
    /* Byte 27: Minimum Internal Read to Precharge Command Delay Time (t_rtp min) (MTB Units) */
    { 8, "internal_read_to_precharge_delay_time", REQUIRED, LENGTH-FIXED }
    /* Byte 28: Upper Nibble for t_faw */
    { 4, "t_faw_msn", REQUIRED, LENGTH-FIXED }
    { 4, "reserved8", REQUIRED, LENGTH-FIXED }
    /* Byte 29: Minimum Four Active Window Delay Time (t_faw min), LSB (MTB Units) */ 
    { 8, "minimum_four_active_window_delay_time", REQUIRED, LENGTH-FIXED }
    /* Byte 30: SDRAM Optional Features */
    { 1, "rzq_6_supported", REQUIRED, LENGTH-FIXED }
    { 1, "rzq_7_supported", REQUIRED, LENGTH-FIXED }
    { 5, "reserved9", REQUIRED, LENGTH-FIXED }
    { 1, "dll_off_mode_supported", REQUIRED, LENGTH-FIXED }
    /* Byte 31: SDRAM Thermal and Refresh Options */
    { 1, "extended_temperature_range", REQUIRED, LENGTH-FIXED }
    { 1, "extended_temperature_refresh_rate", REQUIRED, LENGTH-FIXED }
    { 1, "auto_self_refresh", REQUIRED, LENGTH-FIXED }
    { 1, "on_die_thermal_sensor_readout", REQUIRED, LENGTH-FIXED }
    { 3, "reserved10", REQUIRED, LENGTH-FIXED }
    { 1, "partial_array_self_refresh", REQUIRED, LENGTH-FIXED }
    /* Byte 32: Module Thermal Sensor */
    { 7, "thermal_sensor_accuracy", REQUIRED, LENGTH-FIXED }
    { 1, "thermal_sensor_incorporated", REQUIRED, LENGTH-FIXED }
    /* Byte 33: SDRAM Device Type */
    { 2, "signal_loading", REQUIRED, LENGTH-FIXED }
    { 2, "reserved11", REQUIRED, LENGTH-FIXED }
    { 3, "die_count", REQUIRED, LENGTH-FIXED }
    { 1, "sdram_device_type", REQUIRED, LENGTH-FIXED }
    /* Byte 34: Fine Offset for SDRAM Minimum Cycle Time (tCKmin) */
    { 8, "fine_offset_for_sdram_minimum_cycle_time", REQUIRED, LENGTH-FIXED }
    /* Byte 35: Fine Offset for Minimum CAS Latency Time (tAAmin) */
    { 8, "fine_offset_for_minimum_cas_latency_time", REQUIRED, LENGTH-FIXED }
    /* Byte 36: Fine Offset for Minimum RAS# to CAS# Delay Time (tRCDmin) */
    { 8, "fine_offset_for_minimum_ras_to_cas_delay_time", REQUIRED, LENGTH-FIXED }
    /* Byte 37: Minimum Row Precharge Delay Time (tRPmin) */
    { 8, "minimum_row_precharge_delay_time", REQUIRED, LENGTH-FIXED }
    /* Byte 38: Fine Offset for Minimum Active to Active/Refresh Delay Time (tRCmin) */
    { 8, "fine_offset_for_minimum_active_to_active_refresh_delay_time", REQUIRED, LENGTH-FIXED }
    /* Byte 39-59: Reserved */
    { 168, "reserved12", REQUIRED, LENGTH-FIXED }
    /* Byte 60-116: Module-Specific Section */
    { 456, "module_specific_section", REQUIRED, LENGTH-FIXED }
    /* Byte 117-125: Unique Module ID */
    { 7, "number_of_continuation_codes_module_manufacturer", REQUIRED, LENGTH-FIXED }
    { 1, "odd_parity_byte_module_manufacturer", REQUIRED, LENGTH-FIXED }
    { 8, "last_non_zero_module_manufacturer", REQUIRED, LENGTH-FIXED }
    { 8, "module_manufacturing_location", REQUIRED, LENGTH-FIXED }
    { 8, "module_manufacturing_date.year", REQUIRED, LENGTH-FIXED } /* BCD encoded */
    { 8, "module_manufacturing_date.week", REQUIRED, LENGTH-FIXED } /* BCD encoded */
    { 32, "module_serial_number", REQUIRED, LENGTH-FIXED }
    /* Bytes 126-127: SPD Cyclical Redundancy Code (CRC) */
    { 16, "crc", REQUIRED, LENGTH-FIXED }
    /* Bytes 128-145: Module Part Number */
    { 144, "module_part_number", REQUIRED, LENGTH-FIXED } /* ASCII */
    /* Bytes 146-147: Module Revision Code */
    { 16, "module_revision_code", REQUIRED, LENGTH-FIXED }
    /* Bytes 148-149: DRAM Manufacturer ID Code */
    { 7, "number_of_continuation_codes_dram_manufacturer", REQUIRED, LENGTH-FIXED }
    { 1, "odd_parity_byte_dram_manufacturer", REQUIRED, LENGTH-FIXED }
    { 8, "last_non_zero_dram_manufacturer", REQUIRED, LENGTH-FIXED }
    /* Bytes 150-175: Manufacturer's Specific Data */
    { 208, "manufacturer_specific_data", REQUIRED, LENGTH-FIXED }
    /* Bytes 176-255: Manufacturer's Specific Data */
    { 640, "open_for_customer_use", REQUIRED, LENGTH-FIXED }
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif /* IPMI_FRU_DIMMSPD_RECORD_FORMAT_TEMPLATES_H */
