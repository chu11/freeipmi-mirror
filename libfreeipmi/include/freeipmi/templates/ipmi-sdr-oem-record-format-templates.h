/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef _IPMI_SDR_OEM_RECORD_FORMAT_TEMPLATES_H
#define _IPMI_SDR_OEM_RECORD_FORMAT_TEMPLATES_H

#ifdef __cplusplus
extern "C" {
#endif

/* This header file is for documentation only */

#if 0

Format = { bits, "field name", field flags }

FIID_FIELD_REQUIRED - field is required for the payload
FIID_FIELD_OPTIONAL - field is optional for the payload

FIID_FIELD_LENGTH_FIXED - field length is fixed at the number of bits listed
FIID_FIELD_LENGTH_VARIABLE - field length is variable for the number of bits listed

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel Node Manager
 *
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 */

SDR OEM Intel Node Manager Record
---------------------------------

fiid_template_t tmpl_sdr_oem_intel_node_manager_record =
  {
    /*********************
     * SDR Record Header *
     *********************/
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /********************
     * Record Key Bytes *
     ********************/
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "record_subtype", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "version_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "nm_device_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2,  "sensor_owner_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "nm_health_event_sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "nm_exception_event_sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "nm_operational_capabilities_sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "nm_alert_threshold_exceeded_sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_SDR_OEM_RECORD_FORMAT_TEMPLATES_H */
