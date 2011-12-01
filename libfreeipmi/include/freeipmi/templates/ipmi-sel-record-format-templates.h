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

#ifndef _IPMI_SEL_RECORD_FORMAT_TEMPLATES_H
#define _IPMI_SEL_RECORD_FORMAT_TEMPLATES_H

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

SEL Record Header
-----------------

fiid_template_t tmpl_sel_record_header =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

SEL System Event Record
-----------------------

fiid_template_t tmpl_sel_system_event_record =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Generator ID */
    { 1, "generator_id.id_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "generator_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 2, "ipmb_device_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Dir | Event Type */
    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Data */
    { 8, "event_data1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

SEL System Event Record (with Event flags)
------------------------------------------

fiid_template_t tmpl_sel_system_event_record_event_fields =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Generator ID */
    { 1, "generator_id.id_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "generator_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 2, "ipmb_device_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Dir | Event Type */
    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Data 1 */
    { 4, "offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data3_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data2_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Data 2 */
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Data 3 */
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

SEL System Event Record (w/ Previous and Severity Offset fields)
----------------------------------------------------------------

fiid_template_t tmpl_sel_system_event_record_discrete_previous_state_severity =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Generator ID */
    { 1, "generator_id.id_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "generator_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 2, "ipmb_device_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Dir | Event Type */
    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Data 1 */
    { 4, "offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data3_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data2_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Data 2 */
    { 4, "previous_offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "offset_from_severity_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    /* Event Data 3 */
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

SEL Timestamped OEM Record
--------------------------

fiid_template_t tmpl_sel_timestamped_oem_record =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "oem_defined", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

SEL Non-Timestamped OEM Record
------------------------------

fiid_template_t tmpl_sel_non_timestamped_oem_record =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 104, "oem_defined", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_SEL_RECORD_FORMAT_TEMPLATES_H */
