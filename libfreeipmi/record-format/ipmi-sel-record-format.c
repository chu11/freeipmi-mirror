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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/fiid/fiid.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_sel_record_header =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_sel_system_event_record =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "generator_id.id_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "generator_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 2, "ipmb_device_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_data1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

fiid_template_t tmpl_sel_system_event_record_event_fields =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "generator_id.id_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "generator_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 2, "ipmb_device_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 4, "offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data3_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data2_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

fiid_template_t tmpl_sel_system_event_record_discrete_previous_state_severity =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "generator_id.id_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "generator_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 2, "ipmb_device_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 4, "offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data3_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data2_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 4, "previous_offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "offset_from_severity_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

fiid_template_t tmpl_sel_timestamped_oem_record =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "oem_defined", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_sel_non_timestamped_oem_record =
  {
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 104, "oem_defined", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };


