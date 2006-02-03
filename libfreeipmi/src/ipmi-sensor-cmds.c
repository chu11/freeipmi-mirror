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
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "oper.get_sdr_sensor_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7, "oper.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_dev_sdr_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "get_sdr_sensor_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "flags.device_lun0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "flags.device_lun1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "flags.device_lun2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "flags.device_lun3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "flags.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "flags.sensor_population", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {32, "sensor_population_change_indicator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_dev_sdr_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "record_offset", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "read_bytes", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_dev_sdr_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4096, "requested_bytes"},
    {0,  "", 0}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_resrve_dev_sdr_repository_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_resrve_dev_sdr_repository_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_sensor_reading_factors_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "reading_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_reading_factors_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "next_reading_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "m_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "tolerance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "m_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "b_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "accuracy_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "b_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "accuracy_exp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "accuracy_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "b_exponent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "r_exponent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_hysteresis_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "reserved_hysteresis_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "positive_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "negative_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_hysteresis_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_hysteresis_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "reserved_hysteresis_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_hysteresis_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "positive_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "negative_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_thresholds_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "set_lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "set_lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "set_lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "set_upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "set_upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "set_upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_thresholds_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_thresholds_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_thresholds_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "readable_lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "readable_lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "readable_lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "readable_upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "readable_upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "readable_upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_threshold_event_enable_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "enable_disable_selected_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "enable_disable_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "enable_disable_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_assertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_assertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_deassertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_deassertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_threshold_event_enable_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_discrete_event_enable_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "enable_disable_selected_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "enable_disable_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "enable_disable_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_assertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_assertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_assertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_deassertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "select_deassertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "select_deassertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_discrete_event_enable_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_threshold_event_enable_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_threshold_event_enable_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {6, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_discrete_event_enable_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_discrete_event_enable_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {6, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_re_arm_sensor_threshold_events_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {7, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_all_event_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_assertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_assertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_deassertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_deassertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_re_arm_sensor_threshold_events_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_re_arm_sensor_discrete_events_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {7, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_all_event_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_assertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_assertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_assertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_deassertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "re_arm_deassertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "re_arm_deassertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_re_arm_sensor_discrete_events_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_threshold_event_status_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_threshold_event_status_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_reading_availability", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_lower_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_lower_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_upper_critical_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_critical_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_recoverable_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_upper_non_recoverable_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_discrete_event_status_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_discrete_event_status_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_reading_availability", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_assertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_assertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_state_bit_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_deassertion_event_state_bit_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_deassertion_event_state_bit_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_threshold_reading_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_threshold_reading_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_reading_availability", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "status_comparison_lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "status_comparison_lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "status_comparison_lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "status_comparison_upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "status_comparison_upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "status_comparison_upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    /* optional byte */
    {8, "ignore", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_discrete_reading_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_discrete_reading_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_reading_availability", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "state_0_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_1_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_2_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_3_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_4_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_5_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_6_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_7_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {1, "state_8_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_9_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_10_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_11_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_12_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_13_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "state_14_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {0,  "", 0}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_set_sensor_type_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_set_sensor_type_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

/* Intel - Not Implemented */
fiid_template_t tmpl_get_sensor_type_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sensor_type_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

int8_t 
fill_kcs_get_threshold_reading (fiid_obj_t obj_data_rq, uint8_t sensor_number)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sensor_threshold_reading_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SENSOR_READING); 
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"sensor_number", 
		sensor_number);
  
  return 0;
}

int8_t 
fill_kcs_get_discrete_reading (fiid_obj_t obj_data_rq, uint8_t sensor_number)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sensor_discrete_reading_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SENSOR_READING); 
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"sensor_number", 
		sensor_number);
  
  return 0;
}

int8_t 
fill_kcs_get_sensor_thresholds (fiid_obj_t obj_data_rq, uint8_t sensor_number)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sensor_thresholds_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SENSOR_THRESHOLDS); 
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"sensor_number", 
		sensor_number);
  
  return 0;
}

int8_t 
ipmi_cmd_get_threshold_reading2 (ipmi_device_t *dev, 
				 uint8_t sensor_number, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sensor_threshold_reading_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_sensor_threshold_reading_rq)))
    goto cleanup;
  
  if (fill_kcs_get_threshold_reading (obj_cmd_rq, 
                                      sensor_number) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_SENSOR_EVENT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_discrete_reading2 (ipmi_device_t *dev, 
				uint8_t sensor_number, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sensor_discrete_reading_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_sensor_discrete_reading_rq)))
    goto cleanup;

  if (fill_kcs_get_discrete_reading (obj_cmd_rq, 
                                     sensor_number) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_SENSOR_EVENT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sensor_thresholds2 (ipmi_device_t *dev, 
				 uint8_t sensor_number, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_sensor_thresholds_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_sensor_thresholds_rq)))
    goto cleanup;

  if (fill_kcs_get_sensor_thresholds (obj_cmd_rq, 
                                      sensor_number) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_SENSOR_EVENT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  if (ipmi_comp_test (obj_cmd_rs) != 1)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

