/* 
   ipmi-sensor-cmds.c - IPMI Sensor commands

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

/* Intel - Not Implemented */
fiid_template_t tmpl_get_dev_sdr_info_rq =
  {
    {8, "cmd"},
    {1, "oper.get_sdr_sensor_count"},
    {7, "oper.reserved1"},
    {0, ""}
  };

fiid_template_t tmpl_get_dev_sdr_info_rs =
  {
    {8,  "cmd"},
    {8,  "comp_code"}, 
    {8,  "get_sdr_sensor_count"}, 
    {1,  "flags.device_lun0"},
    {1,  "flags.device_lun1"},
    {1,  "flags.device_lun2"},
    {1,  "flags.device_lun3"},
    {3,  "flags.reserved1"}, 
    {1,  "flags.sensor_population"}, 
    {32, "sensor_population_change_indicator"}, 
    {0,  ""}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_dev_sdr_rq =
  {
    {8,  "cmd"}, 
    {16, "reservation_id"}, 
    {16, "record_id"}, 
    {8,  "record_offset"},
    {8,  "read_bytes"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_dev_sdr_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "record_id"}, 
    // Here data[read_bytes] will be added on the fly.
    {0,  ""}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_resrve_dev_sdr_repository_rq =
  {
    {8,  "cmd"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_resrve_dev_sdr_repository_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "reservation_id"}, 
    {0,  ""}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_sensor_reading_factors_rq =
  {
    {8,  "cmd"}, 
    {8, "sensor_number"}, 
    {8, "reading_byte"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_reading_factors_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {8, "next_reading_byte"}, 
    {8, "m_ls"}, 
    {6, "tolerance"}, 
    {2, "m_ms"}, 
    {8, "b_ls"}, 
    {6, "accuracy_ls"}, 
    {2, "b_ms"}, 
    {2, "reserved"}, 
    {2, "accuracy_exp"}, 
    {4, "accuracy_ms"}, 
    {4, "b_exponent"}, 
    {4, "r_exponent"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_hysteresis_rq =
  {
    {8,  "cmd"}, 
    {8, "sensor_number"}, 
    {8, "reserved_hysteresis_mask"}, 
    {8, "positive_threshold_hysteresis_value"}, 
    {8, "negative_threshold_hysteresis_value"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_hysteresis_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_hysteresis_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {8, "reserved_hysteresis_mask"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_hysteresis_rs =
  {
    {8,  "cmd"}, 
    {8, "comp_code"}, 
    {8, "positive_threshold_hysteresis_value"}, 
    {8, "negative_threshold_hysteresis_value"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_thresholds_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {1, "set_lower_non_critical_threshold"}, 
    {1, "set_lower_critical_threshold"}, 
    {1, "set_lower_non_recoverable_threshold"}, 
    {1, "set_upper_non_critical_threshold"}, 
    {1, "set_upper_critical_threshold"}, 
    {1, "set_upper_non_recoverable_threshold"}, 
    {2, "reserved"}, 
    {8, "lower_non_critical_threshold"}, 
    {8, "lower_critical_threshold"}, 
    {8, "lower_non_recoverable_threshold"}, 
    {8, "upper_non_critical_threshold"}, 
    {8, "upper_critical_threshold"}, 
    {8, "upper_non_recoverable_threshold"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_thresholds_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_thresholds_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_thresholds_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {1, "readable_lower_non_critical_threshold"}, 
    {1, "readable_lower_critical_threshold"}, 
    {1, "readable_lower_non_recoverable_threshold"}, 
    {1, "readable_upper_non_critical_threshold"}, 
    {1, "readable_upper_critical_threshold"}, 
    {1, "readable_upper_non_recoverable_threshold"}, 
    {2, "reserved"}, 
    {8, "lower_non_critical_threshold"}, 
    {8, "lower_critical_threshold"}, 
    {8, "lower_non_recoverable_threshold"}, 
    {8, "upper_non_critical_threshold"}, 
    {8, "upper_critical_threshold"}, 
    {8, "upper_non_recoverable_threshold"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_threshold_event_enable_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    
    {4, "reserved1"}, 
    {2, "enable_disable_selected_event_messages"}, 
    {1, "enable_disable_sensor_scanning"}, 
    {1, "enable_disable_all_event_messages"}, 
    
    {1, "select_assertion_event_lower_non_critical_low"}, 
    {1, "select_assertion_event_lower_non_critical_high"}, 
    {1, "select_assertion_event_lower_critical_low"}, 
    {1, "select_assertion_event_lower_critical_high"}, 
    {1, "select_assertion_event_lower_non_recoverable_low"}, 
    {1, "select_assertion_event_lower_non_recoverable_high"}, 
    {1, "select_assertion_event_upper_non_critical_low"}, 
    {1, "select_assertion_event_upper_non_critical_high"}, 
    
    {1, "select_assertion_event_upper_critical_low"}, 
    {1, "select_assertion_event_upper_critical_high"}, 
    {1, "select_assertion_event_upper_non_recoverable_low"}, 
    {1, "select_assertion_event_upper_non_recoverable_high"}, 
    {4, "reserved2"}, 
    
    {1, "select_deassertion_event_lower_non_critical_low"}, 
    {1, "select_deassertion_event_lower_non_critical_high"}, 
    {1, "select_deassertion_event_lower_critical_low"}, 
    {1, "select_deassertion_event_lower_critical_high"}, 
    {1, "select_deassertion_event_lower_non_recoverable_low"}, 
    {1, "select_deassertion_event_lower_non_recoverable_high"}, 
    {1, "select_deassertion_event_upper_non_critical_low"}, 
    {1, "select_deassertion_event_upper_non_critical_high"}, 
    
    {1, "select_deassertion_event_upper_critical_low"}, 
    {1, "select_deassertion_event_upper_critical_high"}, 
    {1, "select_deassertion_event_upper_non_recoverable_low"}, 
    {1, "select_deassertion_event_upper_non_recoverable_high"}, 
    {4, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_threshold_event_enable_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_discrete_event_enable_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    
    {4, "reserved1"}, 
    {2, "enable_disable_selected_event_messages"}, 
    {1, "enable_disable_sensor_scanning"}, 
    {1, "enable_disable_all_event_messages"}, 
    
    {1, "select_assertion_event_state_bit_0"}, 
    {1, "select_assertion_event_state_bit_1"}, 
    {1, "select_assertion_event_state_bit_2"}, 
    {1, "select_assertion_event_state_bit_3"}, 
    {1, "select_assertion_event_state_bit_4"}, 
    {1, "select_assertion_event_state_bit_5"}, 
    {1, "select_assertion_event_state_bit_6"}, 
    {1, "select_assertion_event_state_bit_7"}, 
    
    {1, "select_assertion_event_state_bit_8"}, 
    {1, "select_assertion_event_state_bit_9"}, 
    {1, "select_assertion_event_state_bit_10"}, 
    {1, "select_assertion_event_state_bit_11"}, 
    {1, "select_assertion_event_state_bit_12"}, 
    {1, "select_assertion_event_state_bit_13"}, 
    {1, "select_assertion_event_state_bit_14"}, 
    {1, "reserved2"}, 
    
    {1, "select_deassertion_event_state_bit_0"}, 
    {1, "select_deassertion_event_state_bit_1"}, 
    {1, "select_deassertion_event_state_bit_2"}, 
    {1, "select_deassertion_event_state_bit_3"}, 
    {1, "select_deassertion_event_state_bit_4"}, 
    {1, "select_deassertion_event_state_bit_5"}, 
    {1, "select_deassertion_event_state_bit_6"}, 
    {1, "select_deassertion_event_state_bit_7"}, 
    
    {1, "select_deassertion_event_state_bit_8"}, 
    {1, "select_deassertion_event_state_bit_9"}, 
    {1, "select_deassertion_event_state_bit_10"}, 
    {1, "select_deassertion_event_state_bit_11"}, 
    {1, "select_deassertion_event_state_bit_12"}, 
    {1, "select_deassertion_event_state_bit_13"}, 
    {1, "select_deassertion_event_state_bit_14"}, 
    {1, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_discrete_event_enable_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_threshold_event_enable_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_threshold_event_enable_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    
    {6, "reserved1"}, 
    {1, "status_sensor_scanning"}, 
    {1, "status_all_event_messages"}, 
    
    {1, "status_assertion_event_lower_non_critical_low"}, 
    {1, "status_assertion_event_lower_non_critical_high"}, 
    {1, "status_assertion_event_lower_critical_low"}, 
    {1, "status_assertion_event_lower_critical_high"}, 
    {1, "status_assertion_event_lower_non_recoverable_low"}, 
    {1, "status_assertion_event_lower_non_recoverable_high"}, 
    {1, "status_assertion_event_upper_non_critical_low"}, 
    {1, "status_assertion_event_upper_non_critical_high"}, 
    
    {1, "status_assertion_event_upper_critical_low"}, 
    {1, "status_assertion_event_upper_critical_high"}, 
    {1, "status_assertion_event_upper_non_recoverable_low"}, 
    {1, "status_assertion_event_upper_non_recoverable_high"}, 
    {4, "reserved2"}, 
    
    {1, "status_deassertion_event_lower_non_critical_low"}, 
    {1, "status_deassertion_event_lower_non_critical_high"}, 
    {1, "status_deassertion_event_lower_critical_low"}, 
    {1, "status_deassertion_event_lower_critical_high"}, 
    {1, "status_deassertion_event_lower_non_recoverable_low"}, 
    {1, "status_deassertion_event_lower_non_recoverable_high"}, 
    {1, "status_deassertion_event_upper_non_critical_low"}, 
    {1, "status_deassertion_event_upper_non_critical_high"}, 
    
    {1, "status_deassertion_event_upper_critical_low"}, 
    {1, "status_deassertion_event_upper_critical_high"}, 
    {1, "status_deassertion_event_upper_non_recoverable_low"}, 
    {1, "status_deassertion_event_upper_non_recoverable_high"}, 
    {4, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_discrete_event_enable_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_discrete_event_enable_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    
    {6, "reserved1"}, 
    {1, "status_sensor_scanning"}, 
    {1, "status_all_event_messages"}, 
    
    {1, "status_assertion_event_state_bit_0"}, 
    {1, "status_assertion_event_state_bit_1"}, 
    {1, "status_assertion_event_state_bit_2"}, 
    {1, "status_assertion_event_state_bit_3"}, 
    {1, "status_assertion_event_state_bit_4"}, 
    {1, "status_assertion_event_state_bit_5"}, 
    {1, "status_assertion_event_state_bit_6"}, 
    {1, "status_assertion_event_state_bit_7"}, 
    
    {1, "status_assertion_event_state_bit_8"}, 
    {1, "status_assertion_event_state_bit_9"}, 
    {1, "status_assertion_event_state_bit_10"}, 
    {1, "status_assertion_event_state_bit_11"}, 
    {1, "status_assertion_event_state_bit_12"}, 
    {1, "status_assertion_event_state_bit_13"}, 
    {1, "status_assertion_event_state_bit_14"}, 
    {1, "reserved2"}, 
    
    {1, "status_deassertion_event_state_bit_0"}, 
    {1, "status_deassertion_event_state_bit_1"}, 
    {1, "status_deassertion_event_state_bit_2"}, 
    {1, "status_deassertion_event_state_bit_3"}, 
    {1, "status_deassertion_event_state_bit_4"}, 
    {1, "status_deassertion_event_state_bit_5"}, 
    {1, "status_deassertion_event_state_bit_6"}, 
    {1, "status_deassertion_event_state_bit_7"}, 
    
    {1, "status_deassertion_event_state_bit_8"}, 
    {1, "status_deassertion_event_state_bit_9"}, 
    {1, "status_deassertion_event_state_bit_10"}, 
    {1, "status_deassertion_event_state_bit_11"}, 
    {1, "status_deassertion_event_state_bit_12"}, 
    {1, "status_deassertion_event_state_bit_13"}, 
    {1, "status_deassertion_event_state_bit_14"}, 
    {1, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_re_arm_sensor_threshold_events_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    
    {7, "reserved1"}, 
    {1, "re_arm_all_event_status"}, 
    
    {1, "re_arm_assertion_event_lower_non_critical_low"}, 
    {1, "re_arm_assertion_event_lower_non_critical_high"}, 
    {1, "re_arm_assertion_event_lower_critical_low"}, 
    {1, "re_arm_assertion_event_lower_critical_high"}, 
    {1, "re_arm_assertion_event_lower_non_recoverable_low"}, 
    {1, "re_arm_assertion_event_lower_non_recoverable_high"}, 
    {1, "re_arm_assertion_event_upper_non_critical_low"}, 
    {1, "re_arm_assertion_event_upper_non_critical_high"}, 
    
    {1, "re_arm_assertion_event_upper_critical_low"}, 
    {1, "re_arm_assertion_event_upper_critical_high"}, 
    {1, "re_arm_assertion_event_upper_non_recoverable_low"}, 
    {1, "re_arm_assertion_event_upper_non_recoverable_high"}, 
    {4, "reserved2"}, 
    
    {1, "re_arm_deassertion_event_lower_non_critical_low"}, 
    {1, "re_arm_deassertion_event_lower_non_critical_high"}, 
    {1, "re_arm_deassertion_event_lower_critical_low"}, 
    {1, "re_arm_deassertion_event_lower_critical_high"}, 
    {1, "re_arm_deassertion_event_lower_non_recoverable_low"}, 
    {1, "re_arm_deassertion_event_lower_non_recoverable_high"}, 
    {1, "re_arm_deassertion_event_upper_non_critical_low"}, 
    {1, "re_arm_deassertion_event_upper_non_critical_high"}, 
    
    {1, "re_arm_deassertion_event_upper_critical_low"}, 
    {1, "re_arm_deassertion_event_upper_critical_high"}, 
    {1, "re_arm_deassertion_event_upper_non_recoverable_low"}, 
    {1, "re_arm_deassertion_event_upper_non_recoverable_high"}, 
    {4, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_re_arm_sensor_threshold_events_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_re_arm_sensor_discrete_events_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    
    {7, "reserved1"}, 
    {1, "re_arm_all_event_status"}, 
    
    {1, "re_arm_assertion_event_state_bit_0"}, 
    {1, "re_arm_assertion_event_state_bit_1"}, 
    {1, "re_arm_assertion_event_state_bit_2"}, 
    {1, "re_arm_assertion_event_state_bit_3"}, 
    {1, "re_arm_assertion_event_state_bit_4"}, 
    {1, "re_arm_assertion_event_state_bit_5"}, 
    {1, "re_arm_assertion_event_state_bit_6"}, 
    {1, "re_arm_assertion_event_state_bit_7"}, 
    
    {1, "re_arm_assertion_event_state_bit_8"}, 
    {1, "re_arm_assertion_event_state_bit_9"}, 
    {1, "re_arm_assertion_event_state_bit_10"}, 
    {1, "re_arm_assertion_event_state_bit_11"}, 
    {1, "re_arm_assertion_event_state_bit_12"}, 
    {1, "re_arm_assertion_event_state_bit_13"}, 
    {1, "re_arm_assertion_event_state_bit_14"}, 
    {1, "reserved2"}, 
    
    {1, "re_arm_deassertion_event_state_bit_0"}, 
    {1, "re_arm_deassertion_event_state_bit_1"}, 
    {1, "re_arm_deassertion_event_state_bit_2"}, 
    {1, "re_arm_deassertion_event_state_bit_3"}, 
    {1, "re_arm_deassertion_event_state_bit_4"}, 
    {1, "re_arm_deassertion_event_state_bit_5"}, 
    {1, "re_arm_deassertion_event_state_bit_6"}, 
    {1, "re_arm_deassertion_event_state_bit_7"}, 
    
    {1, "re_arm_deassertion_event_state_bit_8"}, 
    {1, "re_arm_deassertion_event_state_bit_9"}, 
    {1, "re_arm_deassertion_event_state_bit_10"}, 
    {1, "re_arm_deassertion_event_state_bit_11"}, 
    {1, "re_arm_deassertion_event_state_bit_12"}, 
    {1, "re_arm_deassertion_event_state_bit_13"}, 
    {1, "re_arm_deassertion_event_state_bit_14"}, 
    {1, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_re_arm_sensor_discrete_events_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_threshold_event_status_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_threshold_event_status_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    
    {5, "reserved1"}, 
    {1, "status_reading_availability"}, 
    {1, "status_sensor_scanning"}, 
    {1, "status_all_event_messages"}, 
    
    {1, "status_assertion_event_lower_non_critical_low"}, 
    {1, "status_assertion_event_lower_non_critical_high"}, 
    {1, "status_assertion_event_lower_critical_low"}, 
    {1, "status_assertion_event_lower_critical_high"}, 
    {1, "status_assertion_event_lower_non_recoverable_low"}, 
    {1, "status_assertion_event_lower_non_recoverable_high"}, 
    {1, "status_assertion_event_upper_non_critical_low"}, 
    {1, "status_assertion_event_upper_non_critical_high"}, 
    
    {1, "status_assertion_event_upper_critical_low"}, 
    {1, "status_assertion_event_upper_critical_high"}, 
    {1, "status_assertion_event_upper_non_recoverable_low"}, 
    {1, "status_assertion_event_upper_non_recoverable_high"}, 
    {4, "reserved2"}, 
    
    {1, "status_deassertion_event_lower_non_critical_low"}, 
    {1, "status_deassertion_event_lower_non_critical_high"}, 
    {1, "status_deassertion_event_lower_critical_low"}, 
    {1, "status_deassertion_event_lower_critical_high"}, 
    {1, "status_deassertion_event_lower_non_recoverable_low"}, 
    {1, "status_deassertion_event_lower_non_recoverable_high"}, 
    {1, "status_deassertion_event_upper_non_critical_low"}, 
    {1, "status_deassertion_event_upper_non_critical_high"}, 
    
    {1, "status_deassertion_event_upper_critical_low"}, 
    {1, "status_deassertion_event_upper_critical_high"}, 
    {1, "status_deassertion_event_upper_non_recoverable_low"}, 
    {1, "status_deassertion_event_upper_non_recoverable_high"}, 
    {4, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_discrete_event_status_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_discrete_event_status_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    
    {5, "reserved1"}, 
    {1, "status_reading_availability"}, 
    {1, "status_sensor_scanning"}, 
    {1, "status_all_event_messages"}, 
    
    {1, "status_assertion_event_state_bit_0"}, 
    {1, "status_assertion_event_state_bit_1"}, 
    {1, "status_assertion_event_state_bit_2"}, 
    {1, "status_assertion_event_state_bit_3"}, 
    {1, "status_assertion_event_state_bit_4"}, 
    {1, "status_assertion_event_state_bit_5"}, 
    {1, "status_assertion_event_state_bit_6"}, 
    {1, "status_assertion_event_state_bit_7"}, 
    
    {1, "status_assertion_event_state_bit_8"}, 
    {1, "status_assertion_event_state_bit_9"}, 
    {1, "status_assertion_event_state_bit_10"}, 
    {1, "status_assertion_event_state_bit_11"}, 
    {1, "status_assertion_event_state_bit_12"}, 
    {1, "status_assertion_event_state_bit_13"}, 
    {1, "status_assertion_event_state_bit_14"}, 
    {1, "reserved2"}, 
    
    {1, "status_deassertion_event_state_bit_0"}, 
    {1, "status_deassertion_event_state_bit_1"}, 
    {1, "status_deassertion_event_state_bit_2"}, 
    {1, "status_deassertion_event_state_bit_3"}, 
    {1, "status_deassertion_event_state_bit_4"}, 
    {1, "status_deassertion_event_state_bit_5"}, 
    {1, "status_deassertion_event_state_bit_6"}, 
    {1, "status_deassertion_event_state_bit_7"}, 
    
    {1, "status_deassertion_event_state_bit_8"}, 
    {1, "status_deassertion_event_state_bit_9"}, 
    {1, "status_deassertion_event_state_bit_10"}, 
    {1, "status_deassertion_event_state_bit_11"}, 
    {1, "status_deassertion_event_state_bit_12"}, 
    {1, "status_deassertion_event_state_bit_13"}, 
    {1, "status_deassertion_event_state_bit_14"}, 
    {1, "reserved3"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_threshold_reading_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_threshold_reading_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    
    {8, "sensor_reading"}, 
    
    {5, "reserved1"}, 
    {1, "status_reading_availability"}, 
    {1, "status_sensor_scanning"}, 
    {1, "status_all_event_messages"}, 
    
    {1, "status_comparison_lower_non_critical_threshold"},
    {1, "status_comparison_lower_critical_threshold"},
    {1, "status_comparison_lower_non_recoverable_threshold"},
    {1, "status_comparison_upper_non_critical_threshold"},
    {1, "status_comparison_upper_critical_threshold"},
    {1, "status_comparison_upper_non_recoverable_threshold"},
    {2, "reserved2"}, 
    
    /* optional byte */
    {8, "ignore"}, 
    
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_discrete_reading_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_discrete_reading_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    
    {8, "sensor_reading"}, 
    
    {5, "reserved1"}, 
    {1, "status_reading_availability"}, 
    {1, "status_sensor_scanning"}, 
    {1, "status_all_event_messages"}, 
    
    {1, "state_0_asserted"},
    {1, "state_1_asserted"},
    {1, "state_2_asserted"},
    {1, "state_3_asserted"},
    {1, "state_4_asserted"},
    {1, "state_5_asserted"},
    {1, "state_6_asserted"},
    {1, "state_7_asserted"},
    
    {1, "state_8_asserted"},
    {1, "state_9_asserted"},
    {1, "state_10_asserted"},
    {1, "state_11_asserted"},
    {1, "state_12_asserted"},
    {1, "state_13_asserted"},
    {1, "state_14_asserted"},
    {1, "reserved2"}, 
    
    {0,  ""}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_set_sensor_type_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {8, "sensor_type"}, 
    {7, "event_reading_type_code"}, 
    {1, "reserved"}, 
    {0,  ""}
  };

fiid_template_t tmpl_set_sensor_type_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {0,  ""}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_sensor_type_rq =
  {
    {8, "cmd"}, 
    {8, "sensor_number"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sensor_type_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {8, "sensor_type"}, 
    {7, "event_reading_type_code"}, 
    {1, "reserved"}, 
    {0,  ""}
  };

int8_t 
fill_kcs_get_threshold_reading (fiid_obj_t obj_data_rq, u_int8_t sensor_number)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sensor_threshold_reading_rq, 
		"cmd", 
		IPMI_CMD_GET_SENSOR_READING); 
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sensor_threshold_reading_rq, 
		"sensor_number", 
		sensor_number);
  
  return 0;
}

int8_t 
ipmi_kcs_get_threshold_reading (u_int8_t sensor_number, 
				fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_sensor_threshold_reading_rq);
  fill_kcs_get_threshold_reading (obj_data_rq, sensor_number);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_SENSOR_EVENT_RQ, 
			 obj_data_rq, tmpl_get_sensor_threshold_reading_rq, 
			 obj_data_rs, tmpl_get_sensor_threshold_reading_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_discrete_reading (fiid_obj_t obj_data_rq, u_int8_t sensor_number)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sensor_discrete_reading_rq, 
		"cmd", 
		IPMI_CMD_GET_SENSOR_READING); 
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sensor_discrete_reading_rq, 
		"sensor_number", 
		sensor_number);
  
  return 0;
}

int8_t 
ipmi_kcs_get_discrete_reading (u_int8_t sensor_number, 
			       fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_sensor_discrete_reading_rq);
  fill_kcs_get_discrete_reading (obj_data_rq, sensor_number);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_SENSOR_EVENT_RQ, 
			 obj_data_rq, tmpl_get_sensor_discrete_reading_rq, 
			 obj_data_rs, tmpl_get_sensor_discrete_reading_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_sensor_thresholds (fiid_obj_t obj_data_rq, u_int8_t sensor_number)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sensor_thresholds_rq, 
		"cmd", 
		IPMI_CMD_GET_SENSOR_THRESHOLDS); 
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sensor_thresholds_rq, 
		"sensor_number", 
		sensor_number);
  
  return 0;
}

int8_t 
ipmi_kcs_get_sensor_thresholds (u_int8_t sensor_number, 
				fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_sensor_thresholds_rq);
  fill_kcs_get_sensor_thresholds (obj_data_rq, sensor_number);
  status = ipmi_kcs_cmd (IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_SENSOR_EVENT_RQ, 
			 obj_data_rq, tmpl_get_sensor_thresholds_rq, 
			 obj_data_rs, tmpl_get_sensor_thresholds_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
ipmi_cmd_get_threshold_reading2 (ipmi_device_t *dev, 
				 u_int8_t sensor_number, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sensor_threshold_reading_rq);
  ERR (fill_kcs_get_threshold_reading (obj_cmd_rq, 
				       sensor_number) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_SENSOR_EVENT_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_get_sensor_threshold_reading_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sensor_threshold_reading_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_discrete_reading2 (ipmi_device_t *dev, 
				u_int8_t sensor_number, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sensor_discrete_reading_rq);
  ERR (fill_kcs_get_discrete_reading (obj_cmd_rq, 
				      sensor_number) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_SENSOR_EVENT_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_get_sensor_discrete_reading_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sensor_discrete_reading_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_sensor_thresholds2 (ipmi_device_t *dev, 
				 u_int8_t sensor_number, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sensor_thresholds_rq);
  ERR (fill_kcs_get_sensor_thresholds (obj_cmd_rq, 
				       sensor_number) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_SENSOR_EVENT_RQ;
  ERR (ipmi_cmd (dev, 
		 obj_cmd_rq, 
		 tmpl_get_sensor_thresholds_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sensor_thresholds_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

