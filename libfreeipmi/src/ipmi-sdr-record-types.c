/* 
   ipmi-sdr-record-types.c - IPMI SDR record types

   Copyright (C) 2003 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include "freeipmi.h"
//#include "ipmi-sdr-record-types.h"

fiid_template_t tmpl_sdr_full_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    // Sensor owner ID
    {1, "ipmb_software_id"}, 
    {7, "slave_system_software_id"}, 
    // Sensor owner LUN
    {2, "sensor_owner_lun"}, 
    {2, "sensor_owner_lun.reserved"}, 
    {4, "channel_number"}, 
    // Sensor Number
    {8, "sensor_number"}, 
    
    // Record body bytes
    // -----------------
    // Entity ID
    {8, "entity_id"}, 
    // Entity instance
    {7, "entity_instance_number"}, 
    {1, "physical_logical_entity"}, 
    // Sensor initialization
    {1, "sensor_scanning_flag"}, 
    {1, "event_generation_flag"}, 
    {1, "init_sensor_type"}, 
    {1, "init_hysteresis"}, 
    {1, "init_thresholds"}, 
    {1, "init_events"}, 
    {1, "init_scanning"}, 
    {1, "sensor_init.reserved"}, 
    // Sensor capabilities
    {2, "sensor_event_msg_control_support"}, 
    {2, "sensor_threshold_access_support"}, 
    {2, "sensor_hysteresis_support"}, 
    {1, "sensor_auto_re_arm_support"}, 
    {1, "sensor_entity_ignore_support"}, 
    
    {8, "sensor_type"}, 
    {8, "event_reading_type"}, 
    
    {16, "assertion_threshold_event_mask"}, 
    {16, "deassertion_threshold_event_mask"}, 
    {16, "settable_threshold_discrete_reading_mask"}, 
    //Sensor Unit1
    {1, "sensor_unit_percentage_flag"}, 
    {2, "sensor_unit_modifier_unit_flag"}, 
    {3, "sensor_unit_rate_unit_flag"}, 
    {2, "sensor_unit_analog_data_format"}, 
    
    {8, "sensor_base_unit"}, 
    {8, "sensor_modifier_unit"}, 
    // Linearization
    {7, "linearization_enum"}, 
    {1, "linearization.reserved"}, 
    // M (2s complement, signed)
    {8, "m_ls"}, 
    // M, tolerance
    {6, "tolerance"}, 
    {2, "m_ms"}, 
    // B (2s complement, signed)
    {8, "b_ls"}, 
    // B, Accuracy
    {6, "accuracy_ls"}, 
    {2, "b_ms"}, 
    // Accuracy, Accuracy exp
    {2, "accuracy_exp.reserved"}, 
    {2, "accuracy_exp"}, 
    {4, "accuracy_ms"}, 
    // R exp, B exp (2s complement, signed)
    {4, "b_exponent"}, 
    {4, "r_exponent"}, 
    // Analog characteristic flags
    {1, "analog_char_flag_nominal_reading"}, 
    {1, "analog_char_flag_normal_max"}, 
    {1, "analog_char_flag_normal_min"}, 
    {5, "analog_char_flag.reserved"}, 
    // Nominal reading
    {8, "nominal_reading"}, 
    // Normal maximum
    {8, "normal_max"}, 
    // Normal minimum
    {8, "normal_min"}, 
    // Sensor maximum reading
    {8, "sensor_max_reading"}, 
    // Sensor minimum reading
    {8, "sensor_min_reading"}, 
    // Upper non-recoverable threshold
    {8, "upper_non_recoverable_threshold"}, 
    // Upper critical threshold
    {8, "upper_critical_threshold"}, 
    // Upper non-critical threshold
    {8, "upper_non_critical_threshold"}, 
    // Lower non-recoverable threshold
    {8, "lower_non_recoverable_threshold"}, 
    // Lower critical threshold
    {8, "lower_critical_threshold"}, 
    // Lower non-critical threshold
    {8, "lower_non_critical_threshold"}, 
    // Positive-going Threshold Hysteresis value
    {8, "positive_hysteresis"}, 
    // Negative-going Threshold Hysteresis value
    {8, "negative_hysteresis"}, 
    // 2 bytes reserved
    {16, "reserved"}, 
    // OEM 
    {8, "OEM.reserved"}, 
    // ID String type/Length code
    {8, "id_string_type_length_code"}, 
    // ID String bytes
    {128, "sensor_id_string"}, 
    {0, ""}
  };

fiid_template_t tmpl_sdr_compact_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    // Sensor owner ID
    {1, "ipmb_software_id"}, 
    {7, "slave_system_software_id"}, 
    // Sensor owner LUN
    {2, "sensor_owner_lun"}, 
    {2, "sensor_owner_lun.reserved"}, 
    {4, "channel_number"}, 
    // Sensor Number
    {8, "sensor_number"}, 
    
    // Record body bytes
    // -----------------
    // Entity ID
    {8, "entity_id"}, 
    // Entity instance
    {7, "entity_instance_number"}, 
    {1, "physical_logical_entity"}, 
    // Sensor initialization
    {1, "sensor_scanning_flag"}, 
    {1, "event_generation_flag"}, 
    {1, "init_sensor_type"}, 
    {1, "init_hysteresis"}, 
    {1, "init_thresholds"}, 
    {1, "init_events"}, 
    {1, "init_scanning"}, 
    {1, "sensor_init.reserved"}, 
    // Sensor capabilities
    {2, "sensor_event_msg_control_support"}, 
    {2, "sensor_threshold_access_support"}, 
    {2, "sensor_hysteresis_support"}, 
    {1, "sensor_auto_re_arm_support"}, 
    {1, "sensor_entity_ignore_support"}, 
    
    {8, "sensor_type"}, 
    {8, "event_reading_type"}, 
    
    {16, "assertion_threshold_event_mask"}, 
    {16, "deassertion_threshold_event_mask"}, 
    {16, "settable_threshold_discrete_reading_mask"}, 
    //Sensor Unit1
    {1, "sensor_unit_percentage_flag"}, 
    {2, "sensor_unit_modifier_unit_flag"}, 
    {3, "sensor_unit_rate_unit_flag"}, 
    {2, "sensor_unit_analog_data_format"}, 
    
    {8, "sensor_base_unit"}, 
    {8, "sensor_modifier_unit"}, 
    
    // Sensor Record Sharing
    {4, "share_count"}, 
    {2, "id_string_instance_modifier_type"}, 
    {2, "sensor_record_sharing.reserved"}, 
    {7, "id_string_instance_modifier_offset"}, 
    {1, "entity_instance_sharing"}, 
    
    // Positive-going Threshold Hysteresis value
    {8, "positive_hysteresis"}, 
    // Negative-going Threshold Hysteresis value
    {8, "negative_hysteresis"}, 
    // 3 bytes reserved
    {24, "reserved"}, 
    // OEM 
    {8, "OEM.reserved"}, 
    // ID String type/Length code
    {8, "id_string_type_length_code"}, 
    // ID String bytes
    {120, "sensor_id_string"}, 
    {0, ""}
  };

fiid_template_t tmpl_sdr_entity_association_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    {8, "container_entity_id"}, 
    {8, "container_entity_instance"}, 
    // Flags
    {5, "flags.reserved"}, 
    {1, "sensor_presence_flag"}, 
    {1, "record_link"}, 
    {1, "contained_entity_list_range_flag"}, 
    // Contained Entity 1/Range 1
    {8, "entity_id_contained_entity_range_1"}, 
    // Contained instance entity 1/range 1
    {8, "instance_id_contained_entity_range_1"}, 
    // Record Body Bytes
    // -----------------
    // Contained Entity 2/Range 2
    {8, "entity_id_contained_entity_range_2"}, 
    // Contained instance entity 2/range 2
    {8, "instance_id_contained_entity_range_2"}, 
    // Contained Entity 3/Range 3
    {8, "entity_id_contained_entity_range_3"}, 
    // Contained instance entity 3/range 3
    {8, "instance_id_contained_entity_range_3"}, 
    // Contained Entity 4/Range 4
    {8, "entity_id_contained_entity_range_4"}, 
    // Contained instance entity 4/range 4
    {8, "instance_id_contained_entity_range_4"}, 
    {0, ""}
  };

fiid_template_t tmpl_generic_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved"}, 
    {7, "direct_access_address"}, 
    // Device Slave Address
    {1, "channel_number_ms"}, 
    {7, "device_slave_address"}, 
    // Access LUN / Bus ID
    {3, "private_bus_id"}, 
    {2, "lun_master_write_read_command"}, 
    {3, "channel_number_ls"}, 
    // Address span
    {3, "address_span"}, 
    {5, "address_span.reserved"}, 
    // Reserved
    {8, "reserved"}, 
    // Device Type
    {8, "device_type"}, 
    // Device Type Modifier
    {8, "device_type_modifier"}, 
    // Entity ID
    {8, "entity_id"}, 
    // Entity Instance
    {8, "entity_instance"}, 
    // OEM Reserved
    {8, "oem_reserved"}, 
    // Device ID String Type/Length
    {8, "device_id_string_type_length"}, 
    // Device ID String
    {128, "device_id_string"}, 
    {0, ""}
  };

fiid_template_t tmpl_sdr_logical_fru_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved"}, 
    {7, "controller_slave_address"}, 
    // FRU Device ID/Device Slave Address
    {8, "logical_fru_device"}, 
    // Logical-Physical / Access LUN / Bus ID
    {3, "private_bus_id"}, 
    {2, "lun_master_write_read_fru_command"}, 
    {2, "logical_physical_access_lun_bus_id.reserved"}, 
    {1, "logical_physical_fru_device"}, 
    // Channel Number
    {4, "channel_number.reserved"}, 
    {4, "channel_number"}, 
    // Record Body Bytes
    // -----------------
    {8, "reserved"}, 
    {8, "device_type"}, 
    {8, "device_type_modifier"}, 
    {8, "fru_entity_id"}, 
    {8, "fru_entity_instance"}, 
    // OEM 
    {8, "OEM.reserved"}, 
    // ID String type/Length code
    {8, "id_string_type_length_code"}, 
    // ID String bytes
    {128, "sensor_id_string"}, 
    {0, ""}
  };

fiid_template_t tmpl_sdr_non_intelligent_fru_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved"}, 
    {7, "controller_slave_address"}, 
    // FRU Device ID/Device Slave Address
    {1, "non_intelligent_fru_device.reserved"}, 
    {7, "non_intelligent_fru_device"}, 
    // Logical-Physical / Access LUN / Bus ID
    {3, "private_bus_id"}, 
    {2, "lun_master_write_read_fru_command"}, 
    {2, "logical_physical_access_lun_bus_id.reserved"}, 
    {1, "logical_physical_fru_device"}, 
    // Channel Number
    {4, "channel_number.reserved"}, 
    {4, "channel_number"}, 
    // Record Body Bytes
    // -----------------
    {8, "reserved"}, 
    {8, "device_type"}, 
    {8, "device_type_modifier"}, 
    {8, "fru_entity_id"}, 
    {8, "fru_entity_instance"}, 
    // OEM 
    {8, "OEM.reserved"}, 
    // ID String type/Length code
    {8, "id_string_type_length_code"}, 
    // ID String bytes
    {128, "sensor_id_string"}, 
    {0, ""}
  };

fiid_template_t tmpl_sdr_management_controller_device_locator_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    // Direct Access Address
    {1, "direct_access_address.reserved"}, 
    {7, "controller_slave_address"}, 
    // Channel number
    {4, "channel_number"}, 
    {4, "channel_number.reserved"}, 
    // Record Body Bytes
    // -----------------
    // Power State Notification/Global Initialization
    {2, "event_message_generation_flag"}, 
    {1, "log_initialization_agent_flag"}, 
    {1, "controller_log_initialization_agent_flag"}, 
    {1, "power_state_notification.reserved"}, 
    {1, "static_dynamic_controller_flag"}, 
    {1, "acpi_device_power_state_notification_flag"}, 
    {1, "acpi_system_power_state_notification_flag"}, 
    // Device capabilities
    {1, "sensor_device"}, 
    {1, "sdr_repository_device"}, 
    {1, "sel_device"}, 
    {1, "fru_inventory_device"}, 
    {1, "ipmb_event_receiver"}, 
    {1, "ipmb_event_generator"}, 
    {1, "bridge"}, 
    {1, "chassis_device"}, 
    // 3 bytes reserved
    {24, "reserved"}, 
    {8, "entity_id"}, 
    {8, "entity_instance"}, 
    {8, "OEM.reserved"}, 
    // ID String type/Length code
    {8, "id_string_type_length_code"}, 
    // ID String bytes
    {128, "sensor_id_string"}, 
    {0, ""}
  };

fiid_template_t tmpl_sdr_oem_sensor_record = 
  {
    // Sensor record header
    // --------------------
    {16, "record_id"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {8,  "record_type"}, 
    {8,  "record_length"}, 
    // Record Key bytes
    // ----------------
    {24, "manufacturer_id"}, 
    {440, "oem_data"}, 
    {0, ""}
  };

