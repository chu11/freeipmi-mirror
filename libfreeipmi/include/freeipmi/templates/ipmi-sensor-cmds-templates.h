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

#ifndef _IPMI_SENSOR_CMDS_TEMPLATES_H
#define _IPMI_SENSOR_CMDS_TEMPLATES_H

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

FIID_FIELD_MAKES_PACKET_SUFFICIENT - indicates field or fields are "sufficient" to make a valid packet

Get Device SDR Info Request
---------------------------

fiid_template_t tmpl_cmd_get_device_sdr_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Device SDR Info Response
----------------------------

fiid_template_t tmpl_cmd_get_device_sdr_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "flags.device_lun_0_has_sensors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "flags.device_lun_1_has_sensors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "flags.device_lun_2_has_sensors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "flags.device_lun_3_has_sensors", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "flags.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "flags.sensor_population", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sensor_population_change_indicator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Device SDR Request
----------------------

fiid_template_t tmpl_cmd_get_device_sdr_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_into_record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Device SDR Response
-----------------------

fiid_template_t tmpl_cmd_get_device_sdr_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4096, "requested_bytes", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

Get Sensor Reading Factors Request
----------------------------------

fiid_template_t tmpl_cmd_get_sensor_reading_factors_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reading_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Reading Factors Response
-----------------------------------

fiid_template_t tmpl_cmd_get_sensor_reading_factors_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "next_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "m_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "tolerance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "m_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "b_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "accuracy_ls", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "b_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "accuracy_exp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "accuracy_ms", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "b_exponent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "r_exponent", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Hysteresis Request
-----------------------------

fiid_template_t tmpl_cmd_set_sensor_hysteresis_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "hysteresis_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "positive_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "negative_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Hysteresis Response
------------------------------

fiid_template_t tmpl_cmd_set_sensor_hysteresis_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Sensor Hysteresis Request
-----------------------------

fiid_template_t tmpl_cmd_get_sensor_hysteresis_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "hysteresis_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Hysteresis Response
------------------------------

fiid_template_t tmpl_cmd_get_sensor_hysteresis_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "positive_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "negative_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Thresholds Request
-----------------------------

fiid_template_t tmpl_cmd_set_sensor_thresholds_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Thresholds Response
------------------------------

fiid_template_t tmpl_cmd_set_sensor_thresholds_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Sensor Thresholds Request
-----------------------------

fiid_template_t tmpl_cmd_get_sensor_thresholds_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Thresholds Response
------------------------------

fiid_template_t tmpl_cmd_get_sensor_thresholds_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "readable_thresholds.lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "readable_thresholds.lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "readable_thresholds.lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "readable_thresholds.upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "readable_thresholds.upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "readable_thresholds.upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Event Enable Request
-------------------------------

fiid_template_t tmpl_cmd_set_sensor_event_enable_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_message_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "scanning_on_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "assertion_event_bitmask", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "deassertion_event_bitmask", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Event Enable (Threshold) Request
-------------------------------------------

fiid_template_t tmpl_cmd_set_sensor_event_enable_threshold_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_message_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "scanning_on_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_lower_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_upper_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_lower_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_upper_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

Set Sensor Event Enable (Discrete) Request
------------------------------------------

fiid_template_t tmpl_cmd_set_sensor_event_enable_discrete_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_message_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "scanning_on_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_state_bit_0", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_state_bit_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_state_bit_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_state_bit_0", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_state_bit_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_state_bit_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

Set Sensor Event Enable Response
--------------------------------

fiid_template_t tmpl_cmd_set_sensor_event_enable_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Sensor Event Enable Request
-------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_enable_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Event Enable Response
--------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_enable_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "scanning_on_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "assertion_event_bitmask", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "deassertion_event_bitmask", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Event Enable (Threshold) Response
--------------------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_enable_threshold_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},

    { 6, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "scanning_on_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_for_lower_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_lower_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_lower_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_lower_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_lower_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_lower_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_upper_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_upper_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_for_upper_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_upper_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_upper_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_for_upper_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_for_lower_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_lower_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_lower_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_lower_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_lower_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_lower_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_upper_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_upper_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_for_upper_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_upper_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_upper_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_for_upper_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

Get Sensor Event Enable (Discrete) Response
-------------------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_enable_discrete_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},

    { 6, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "scanning_on_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_message_for_state_bit_0", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_message_for_state_bit_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_message_for_state_bit_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_message_for_state_bit_0", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_message_for_state_bit_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_message_for_state_bit_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

Re-arm Sensor Events Request
----------------------------

fiid_template_t tmpl_cmd_re_arm_sensor_events_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_all_event_status_from_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "re_arm_assertion_event", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "re_arm_deassertion_event", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Re-arm Sensor Events (Threshold) Request
----------------------------------------

fiid_template_t tmpl_cmd_re_arm_sensor_events_threshold_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_all_event_status_from_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_lower_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_lower_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_lower_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_lower_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_lower_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_lower_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_upper_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_upper_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_upper_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_upper_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_upper_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_upper_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_lower_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_lower_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_lower_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_lower_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_lower_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_lower_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_upper_non_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_upper_non_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_upper_critical_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_upper_critical_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_upper_non_recoverable_going_low", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_upper_non_recoverable_going_high", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Re-arm Sensor Events (Discrete) Request
---------------------------------------

fiid_template_t tmpl_cmd_re_arm_sensor_events_discrete_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_all_event_status_from_this_sensor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_0", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_assertion_event_for_state_bit_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_0", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "re_arm_deassertion_event_for_state_bit_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Re-arm Sensor Events Response
-----------------------------

fiid_template_t tmpl_cmd_re_arm_sensor_events_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Sensor Event Status Request
-------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Event Status Response
--------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_status_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Event Status (Threshold) Response
--------------------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_status_threshold_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},

    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_condition_for_lower_non_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_lower_non_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_lower_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_lower_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_lower_non_recoverable_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_lower_non_recoverable_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_upper_non_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_upper_non_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "assertion_event_condition_for_upper_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_upper_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_upper_non_recoverable_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_condition_for_upper_non_recoverable_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_condition_for_lower_non_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_lower_non_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_lower_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_lower_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_lower_non_recoverable_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_lower_non_recoverable_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_upper_non_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_upper_non_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "deassertion_event_condition_for_upper_critical_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_upper_critical_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_upper_non_recoverable_going_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_condition_for_upper_non_recoverable_going_high", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 0, "", 0}
  };

Get Sensor Event Status (Discrete) Response
--------------------------------------------

fiid_template_t tmpl_cmd_get_sensor_event_status_discrete_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_0_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_1_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_2_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_3_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_4_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_5_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_6_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_7_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_8_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_9_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_10_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_11_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_12_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_13_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_14_assertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_0_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_1_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_2_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_3_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_4_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_5_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_6_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_7_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_8_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_9_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_10_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_11_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_12_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_13_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_14_deassertion_event", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Reading Request
--------------------------

fiid_template_t tmpl_cmd_get_sensor_reading_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Reading Response
---------------------------

fiid_template_t tmpl_cmd_get_sensor_reading_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_event_bitmask1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "sensor_event_bitmask2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Reading (Threshold) Response
---------------------------------------

fiid_template_t tmpl_cmd_get_sensor_reading_threshold_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "at_or_below_lower_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "at_or_below_lower_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "at_or_below_lower_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "at_or_above_upper_non_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "at_or_above_upper_critical_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "at_or_above_upper_non_recoverable_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Reading (Discrete) Response
--------------------------------------

fiid_template_t tmpl_cmd_get_sensor_reading_discrete_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_0_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_1_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_2_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_3_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_4_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_5_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_6_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_7_asserted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_8_asserted", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_9_asserted", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_10_asserted", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_11_asserted", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_12_asserted", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_13_asserted", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_14_asserted", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Type Request
-----------------------

fiid_template_t tmpl_cmd_set_sensor_type_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Set Sensor Type Response
------------------------

fiid_template_t tmpl_cmd_set_sensor_type_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

Get Sensor Type Request
-----------------------

fiid_template_t tmpl_cmd_get_sensor_type_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Get Sensor Type Response
------------------------

fiid_template_t tmpl_cmd_get_sensor_type_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_SENSOR_CMDS_TEMPLATES_H */
