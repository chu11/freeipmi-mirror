/* 
   ipmi-sdr-record-types.c - IPMI SDR record types

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#include "freeipmi.h"

fiid_template_t tmpl_sdr_sensor_record_header = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_full_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    // Sensor owner ID
    {1, "ipmb_software_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "slave_system_software_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor owner LUN
    {2, "sensor_owner_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_owner_lun.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor Number
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    // Record body bytes
    // -----------------
    // Entity ID
    {8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Entity instance
    {7, "entity_instance_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "physical_logical_entity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor initialization
    {1, "sensor_scanning_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "event_generation_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_hysteresis", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_thresholds", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_events", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sensor_init.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor capabilities
    {2, "sensor_event_msg_control_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_threshold_access_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_hysteresis_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sensor_auto_re_arm_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sensor_entity_ignore_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "event_reading_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {16, "assertion_threshold_event_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "deassertion_threshold_event_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "settable_threshold_discrete_reading_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    //Sensor Unit1
    {1, "sensor_unit_percentage_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_unit_modifier_unit_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "sensor_unit_rate_unit_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_unit_analog_data_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "sensor_base_unit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_modifier_unit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Linearization
    {7, "linearization_enum", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "linearization.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // M (2s complement, signed)
    {8, "m_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // M, tolerance
    {6, "tolerance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "m_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // B (2s complement, signed)
    {8, "b_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // B, Accuracy
    {6, "accuracy_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "b_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Accuracy, Accuracy exp
    {2, "accuracy_exp.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "accuracy_exp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "accuracy_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // R exp, B exp (2s complement, signed)
    {4, "b_exponent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "r_exponent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Analog characteristic flags
    {1, "analog_char_flag_nominal_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "analog_char_flag_normal_max", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "analog_char_flag_normal_min", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "analog_char_flag.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Nominal reading
    {8, "nominal_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Normal maximum
    {8, "normal_max", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Normal minimum
    {8, "normal_min", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor maximum reading
    {8, "sensor_max_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor minimum reading
    {8, "sensor_min_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Upper non-recoverable threshold
    {8, "upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Upper critical threshold
    {8, "upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Upper non-critical threshold
    {8, "upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Lower non-recoverable threshold
    {8, "lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Lower critical threshold
    {8, "lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Lower non-critical threshold
    {8, "lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Positive-going Threshold Hysteresis value
    {8, "positive_hysteresis", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Negative-going Threshold Hysteresis value
    {8, "negative_hysteresis", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // 2 bytes reserved
    {16, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // OEM 
    {8, "OEM.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String type/Length code
    {8, "id_string_type_length_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String bytes
    {128, "sensor_id_string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_compact_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    // Sensor owner ID
    {1, "ipmb_software_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "slave_system_software_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor owner LUN
    {2, "sensor_owner_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_owner_lun.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor Number
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    // Record body bytes
    // -----------------
    // Entity ID
    {8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Entity instance
    {7, "entity_instance_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "physical_logical_entity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor initialization
    {1, "sensor_scanning_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "event_generation_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_hysteresis", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_thresholds", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_events", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "init_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sensor_init.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor capabilities
    {2, "sensor_event_msg_control_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_threshold_access_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_hysteresis_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sensor_auto_re_arm_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sensor_entity_ignore_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "event_reading_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {16, "assertion_threshold_event_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "deassertion_threshold_event_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "settable_threshold_discrete_reading_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    //Sensor Unit1
    {1, "sensor_unit_percentage_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_unit_modifier_unit_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "sensor_unit_rate_unit_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_unit_analog_data_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "sensor_base_unit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_modifier_unit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    // Sensor Record Sharing
    {4, "share_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "id_string_instance_modifier_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_record_sharing.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "id_string_instance_modifier_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "entity_instance_sharing", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    // Positive-going Threshold Hysteresis value
    {8, "positive_hysteresis", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Negative-going Threshold Hysteresis value
    {8, "negative_hysteresis", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // 3 bytes reserved
    {24, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // OEM 
    {8, "OEM.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String type/Length code
    {8, "id_string_type_length_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String bytes
    {128, "sensor_id_string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_event_only_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    // Sensor owner ID
    {1, "ipmb_software_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "slave_system_software_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor owner LUN
    {2, "sensor_owner_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "fru_inventory_device_owner_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Sensor Number
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    // Record body bytes
    // -----------------
    // Entity ID
    {8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Entity instance
    {7, "entity_instance_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "physical_logical_entity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "event_reading_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    // Sensor Record Sharing, Sensor Direction
    // ---------------------------------------
    {4, "share_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "id_string_instance_modifier_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "sensor_direction", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "id_string_instance_modifier_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "entity_instance_sharing", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "OEM.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "id_string_type_length_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {128, "sensor_id_string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_entity_association_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    {8, "container_entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "container_entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Flags
    {5, "flags.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sensor_presence_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "record_link", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "contained_entity_list_range_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Contained Entity 1/Range 1
    {8, "entity_id_contained_entity_range_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Contained instance entity 1/range 1
    {8, "instance_id_contained_entity_range_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Body Bytes
    // -----------------
    // Contained Entity 2/Range 2
    {8, "entity_id_contained_entity_range_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Contained instance entity 2/range 2
    {8, "instance_id_contained_entity_range_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Contained Entity 3/Range 3
    {8, "entity_id_contained_entity_range_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Contained instance entity 3/range 3
    {8, "instance_id_contained_entity_range_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Contained Entity 4/Range 4
    {8, "entity_id_contained_entity_range_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Contained instance entity 4/range 4
    {8, "instance_id_contained_entity_range_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_generic_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "direct_access_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Device Slave Address
    {1, "channel_number_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "device_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Access LUN / Bus ID
    {3, "private_bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "lun_master_write_read_command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "channel_number_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Address span
    {3, "address_span", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "address_span.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Reserved
    {8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Device Type
    {8, "device_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Device Type Modifier
    {8, "device_type_modifier", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Entity ID
    {8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Entity Instance
    {8, "entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // OEM Reserved
    {8, "oem_reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Device ID String Type/Length
    {8, "device_id_string_type_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Device ID String
    {128, "device_id_string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_logical_fru_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "controller_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // FRU Device ID/Device Slave Address
    {8, "logical_fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Logical-Physical / Access LUN / Bus ID
    {3, "private_bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "lun_master_write_read_fru_command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "logical_physical_access_lun_bus_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "logical_physical_fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Channel Number
    {4, "channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Body Bytes
    // -----------------
    {8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "device_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "device_type_modifier", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "fru_entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "fru_entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // OEM 
    {8, "OEM.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String type/Length code
    {8, "id_string_type_length_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String bytes
    {128, "device_id_string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_non_intelligent_fru_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "controller_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // FRU Device ID/Device Slave Address
    {1, "non_intelligent_fru_device.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "non_intelligent_fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Logical-Physical / Access LUN / Bus ID
    {3, "private_bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "lun_master_write_read_fru_command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "logical_physical_access_lun_bus_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "logical_physical_fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Channel Number
    {4, "channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Body Bytes
    // -----------------
    {8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "device_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "device_type_modifier", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "fru_entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "fru_entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // OEM 
    {8, "OEM.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String type/Length code
    {8, "id_string_type_length_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String bytes
    {128, "sensor_id_string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_management_controller_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "controller_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Channel number
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Body Bytes
    // -----------------
    // Power State Notification/Global Initialization
    {2, "event_message_generation_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "log_initialization_agent_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "controller_log_initialization_agent_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "power_state_notification.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "static_dynamic_controller_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "acpi_device_power_state_notification_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "acpi_system_power_state_notification_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Device capabilities
    {1, "sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sdr_repository_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "fru_inventory_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "ipmb_event_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "ipmb_event_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "chassis_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // 3 bytes reserved
    {24, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "entity_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "entity_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "OEM.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String type/Length code
    {8, "id_string_type_length_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // ID String bytes
    {128, "device_id_string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_sdr_oem_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    // Record Key bytes
    // ----------------
    {24, "manufacturer_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {440, "oem_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

