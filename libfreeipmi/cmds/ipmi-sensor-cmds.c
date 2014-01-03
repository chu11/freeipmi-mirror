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

#include "freeipmi/cmds/ipmi-sensor-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_device_sdr_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_get_device_sdr_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "offset_into_record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_device_sdr_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4096, "requested_bytes", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_reading_factors_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reading_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_set_sensor_hysteresis_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "hysteresis_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "positive_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "negative_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_hysteresis_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_hysteresis_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "hysteresis_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_hysteresis_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "positive_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "negative_going_threshold_hysteresis_value", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_set_sensor_thresholds_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_thresholds_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_set_sensor_event_enable_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_event_enable_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_re_arm_sensor_events_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_event_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_get_sensor_reading_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

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

fiid_template_t tmpl_cmd_set_sensor_type_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_type_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_type_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sensor_type_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_reading_and_event_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "sensor_reading_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "deassertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "assertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data_bytes_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "assertion_event_bitmask1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "assertion_event_bitmask2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "deassertion_event_bitmask1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "deassertion_event_bitmask2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_reading_and_event_status_threshold_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "sensor_reading_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "deassertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "assertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data_bytes_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_reading_and_event_status_discrete_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "sensor_reading_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "deassertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "assertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data_bytes_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_0_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_1_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_2_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_3_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_4_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_5_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_6_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_7_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_8_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_9_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_10_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_11_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_12_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_13_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_14_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_0_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_1_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_2_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_3_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_4_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_5_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_6_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_7_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_8_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_9_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_10_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_11_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_12_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_13_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_14_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_reading_and_event_status_event_fields_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "sensor_reading_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "deassertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "assertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data_bytes_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "assertion_event_bitmask1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "assertion_event_bitmask2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "deassertion_event_bitmask1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "deassertion_event_bitmask2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data3_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data2_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_reading_and_event_status_threshold_event_fields_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "sensor_reading_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "deassertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "assertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data_bytes_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_lower_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "assertion_event_upper_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_lower_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_critical_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_critical_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_recoverable_going_low_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "deassertion_event_upper_non_recoverable_going_high_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data3_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data2_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_reading_and_event_status_discrete_event_fields_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "sensor_reading_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "deassertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "assertion_bits_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data_bytes_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_0_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_1_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_2_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_3_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_4_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_5_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_6_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_7_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_8_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_9_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_10_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_11_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_12_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_13_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_14_assertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_0_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_1_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_2_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_3_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_4_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_5_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_6_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_7_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_8_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_9_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_10_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_11_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_12_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_13_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "state_14_deassertion_event_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "offset_from_event_reading_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data3_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_data2_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sensor_reading_and_event_status_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

/* achu: as of IPMI 2.0 hysteresis_mask reserved for future - write as 0xFF */
int
fill_cmd_set_sensor_hysteresis (uint8_t sensor_number,
                                uint8_t hysteresis_mask,
                                uint8_t positive_going_threshold_hysteresis_value,
                                uint8_t negative_going_threshold_hysteresis_value,
                                fiid_obj_t obj_cmd_rq)
{
  if (hysteresis_mask != IPMI_SENSOR_HYSTERESIS_MASK
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sensor_hysteresis_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SENSOR_HYSTERESIS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "hysteresis_mask", hysteresis_mask);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "positive_going_threshold_hysteresis_value",
                     positive_going_threshold_hysteresis_value);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "negative_going_threshold_hysteresis_value",
                     negative_going_threshold_hysteresis_value);

  return (0);
}

/* achu: as of IPMI 2.0 hysteresis_mask reserved for future - write as 0xFF */
int
fill_cmd_get_sensor_hysteresis (uint8_t sensor_number,
                                uint8_t hysteresis_mask,
                                fiid_obj_t obj_cmd_rq)
{
  if (hysteresis_mask != IPMI_SENSOR_HYSTERESIS_MASK
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_sensor_hysteresis_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SENSOR_HYSTERESIS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "hysteresis_mask", hysteresis_mask);

  return (0);
}

int
fill_cmd_set_sensor_thresholds (uint8_t sensor_number,
                                const uint8_t *lower_non_critical_threshold,
                                const uint8_t *lower_critical_threshold,
                                const uint8_t *lower_non_recoverable_threshold,
                                const uint8_t *upper_non_critical_threshold,
                                const uint8_t *upper_critical_threshold,
                                const uint8_t *upper_non_recoverable_threshold,
                                fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sensor_thresholds_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SENSOR_THRESHOLDS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  if (lower_non_critical_threshold)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_lower_non_critical_threshold", IPMI_SENSOR_THRESHOLD_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "lower_non_critical_threshold", *lower_non_critical_threshold);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_lower_non_critical_threshold", IPMI_SENSOR_THRESHOLD_NOT_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "lower_non_critical_threshold", 0);
    }

  if (lower_critical_threshold)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_lower_critical_threshold", IPMI_SENSOR_THRESHOLD_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "lower_critical_threshold", *lower_critical_threshold);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_lower_critical_threshold", IPMI_SENSOR_THRESHOLD_NOT_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "lower_critical_threshold", 0);
    }

  if (lower_non_recoverable_threshold)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_lower_non_recoverable_threshold", IPMI_SENSOR_THRESHOLD_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "lower_non_recoverable_threshold", *lower_non_recoverable_threshold);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_lower_non_recoverable_threshold", IPMI_SENSOR_THRESHOLD_NOT_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "lower_non_recoverable_threshold", 0);
    }

  if (upper_non_critical_threshold)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_upper_non_critical_threshold", IPMI_SENSOR_THRESHOLD_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "upper_non_critical_threshold", *upper_non_critical_threshold);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_upper_non_critical_threshold", IPMI_SENSOR_THRESHOLD_NOT_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "upper_non_critical_threshold", 0);
    }

  if (upper_critical_threshold)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_upper_critical_threshold", IPMI_SENSOR_THRESHOLD_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "upper_critical_threshold", *upper_critical_threshold);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_upper_critical_threshold", IPMI_SENSOR_THRESHOLD_NOT_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "upper_critical_threshold", 0);
    }

  if (upper_non_recoverable_threshold)
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_upper_non_recoverable_threshold", IPMI_SENSOR_THRESHOLD_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "upper_non_recoverable_threshold", *upper_non_recoverable_threshold);
    }
  else
    {
      FILL_FIID_OBJ_SET (obj_cmd_rq, "set_upper_non_recoverable_threshold", IPMI_SENSOR_THRESHOLD_NOT_SET);
      FILL_FIID_OBJ_SET (obj_cmd_rq, "upper_non_recoverable_threshold", 0);
    }

  return (0);
}

int
fill_cmd_get_sensor_thresholds (uint8_t sensor_number, fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_sensor_thresholds_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SENSOR_THRESHOLDS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);

  return (0);
}

int
fill_cmd_set_sensor_event_enable (uint8_t sensor_number,
                                  uint8_t event_message_action,
                                  uint8_t scanning_on_this_sensor,
                                  uint8_t all_event_messages,
                                  uint16_t assertion_event_bitmask,
                                  uint16_t deassertion_event_bitmask,
                                  fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SENSOR_EVENT_MESSAGE_ACTION_VALID (event_message_action)
      || !IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_VALID (scanning_on_this_sensor)
      || !IPMI_SENSOR_ALL_EVENT_MESSAGES_VALID (all_event_messages)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sensor_event_enable_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SENSOR_EVENT_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_message_action", event_message_action);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "scanning_on_this_sensor", scanning_on_this_sensor);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "all_event_messages", all_event_messages);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_bitmask", assertion_event_bitmask);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_bitmask", deassertion_event_bitmask);

  return (0);
}

int
fill_cmd_set_sensor_event_enable_threshold (uint8_t sensor_number,
                                            uint8_t event_message_action,
                                            uint8_t scanning_on_this_sensor,
                                            uint8_t all_event_messages,
                                            uint8_t assertion_event_lower_non_critical_going_low,
                                            uint8_t assertion_event_lower_non_critical_going_high,
                                            uint8_t assertion_event_lower_critical_going_low,
                                            uint8_t assertion_event_lower_critical_going_high,
                                            uint8_t assertion_event_lower_non_recoverable_going_low,
                                            uint8_t assertion_event_lower_non_recoverable_going_high,
                                            uint8_t assertion_event_upper_non_critical_going_low,
                                            uint8_t assertion_event_upper_non_critical_going_high,
                                            uint8_t assertion_event_upper_critical_going_low,
                                            uint8_t assertion_event_upper_critical_going_high,
                                            uint8_t assertion_event_upper_non_recoverable_going_low,
                                            uint8_t assertion_event_upper_non_recoverable_going_high,
                                            uint8_t deassertion_event_lower_non_critical_going_low,
                                            uint8_t deassertion_event_lower_non_critical_going_high,
                                            uint8_t deassertion_event_lower_critical_going_low,
                                            uint8_t deassertion_event_lower_critical_going_high,
                                            uint8_t deassertion_event_lower_non_recoverable_going_low,
                                            uint8_t deassertion_event_lower_non_recoverable_going_high,
                                            uint8_t deassertion_event_upper_non_critical_going_low,
                                            uint8_t deassertion_event_upper_non_critical_going_high,
                                            uint8_t deassertion_event_upper_critical_going_low,
                                            uint8_t deassertion_event_upper_critical_going_high,
                                            uint8_t deassertion_event_upper_non_recoverable_going_low,
                                            uint8_t deassertion_event_upper_non_recoverable_going_high,
                                            fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SENSOR_EVENT_MESSAGE_ACTION_VALID (event_message_action)
      || !IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_VALID (scanning_on_this_sensor)
      || !IPMI_SENSOR_ALL_EVENT_MESSAGES_VALID (all_event_messages)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_lower_non_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_lower_non_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_lower_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_lower_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_lower_non_recoverable_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_lower_non_recoverable_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_upper_non_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_upper_non_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_upper_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_upper_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_upper_non_recoverable_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_upper_non_recoverable_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_lower_non_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_lower_non_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_lower_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_lower_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_lower_non_recoverable_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_lower_non_recoverable_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_upper_non_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_upper_non_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_upper_critical_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_upper_critical_going_high)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_upper_non_recoverable_going_low)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_upper_non_recoverable_going_high)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sensor_event_enable_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SENSOR_EVENT_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_message_action", event_message_action);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "scanning_on_this_sensor", scanning_on_this_sensor);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "all_event_messages", all_event_messages);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_lower_non_critical_going_low", assertion_event_lower_non_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_lower_non_critical_going_high", assertion_event_lower_non_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_lower_critical_going_low", assertion_event_lower_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_lower_critical_going_high", assertion_event_lower_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_lower_non_recoverable_going_low", assertion_event_lower_non_recoverable_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_lower_non_recoverable_going_high", assertion_event_lower_non_recoverable_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_upper_non_critical_going_low", assertion_event_upper_non_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_upper_non_critical_going_high", assertion_event_upper_non_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_upper_critical_going_low", assertion_event_upper_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_upper_critical_going_high", assertion_event_upper_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_upper_non_recoverable_going_low", assertion_event_upper_non_recoverable_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_upper_non_recoverable_going_high", assertion_event_upper_non_recoverable_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_lower_non_critical_going_low", deassertion_event_lower_non_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_lower_non_critical_going_high", deassertion_event_lower_non_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_lower_critical_going_low", deassertion_event_lower_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_lower_critical_going_high", deassertion_event_lower_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_lower_non_recoverable_going_low", deassertion_event_lower_non_recoverable_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_lower_non_recoverable_going_high", deassertion_event_lower_non_recoverable_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_upper_non_critical_going_low", deassertion_event_upper_non_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_upper_non_critical_going_high", deassertion_event_upper_non_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_upper_critical_going_low", deassertion_event_upper_critical_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_upper_critical_going_high", deassertion_event_upper_critical_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_upper_non_recoverable_going_low", deassertion_event_upper_non_recoverable_going_low);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_upper_non_recoverable_going_high", deassertion_event_upper_non_recoverable_going_high);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);

  return (0);
}

int
fill_cmd_set_sensor_event_enable_discrete (uint8_t sensor_number,
                                           uint8_t event_message_action,
                                           uint8_t scanning_on_this_sensor,
                                           uint8_t all_event_messages,
                                           uint8_t assertion_event_state_bit_0,
                                           uint8_t assertion_event_state_bit_1,
                                           uint8_t assertion_event_state_bit_2,
                                           uint8_t assertion_event_state_bit_3,
                                           uint8_t assertion_event_state_bit_4,
                                           uint8_t assertion_event_state_bit_5,
                                           uint8_t assertion_event_state_bit_6,
                                           uint8_t assertion_event_state_bit_7,
                                           uint8_t assertion_event_state_bit_8,
                                           uint8_t assertion_event_state_bit_9,
                                           uint8_t assertion_event_state_bit_10,
                                           uint8_t assertion_event_state_bit_11,
                                           uint8_t assertion_event_state_bit_12,
                                           uint8_t assertion_event_state_bit_13,
                                           uint8_t assertion_event_state_bit_14,
                                           uint8_t deassertion_event_state_bit_0,
                                           uint8_t deassertion_event_state_bit_1,
                                           uint8_t deassertion_event_state_bit_2,
                                           uint8_t deassertion_event_state_bit_3,
                                           uint8_t deassertion_event_state_bit_4,
                                           uint8_t deassertion_event_state_bit_5,
                                           uint8_t deassertion_event_state_bit_6,
                                           uint8_t deassertion_event_state_bit_7,
                                           uint8_t deassertion_event_state_bit_8,
                                           uint8_t deassertion_event_state_bit_9,
                                           uint8_t deassertion_event_state_bit_10,
                                           uint8_t deassertion_event_state_bit_11,
                                           uint8_t deassertion_event_state_bit_12,
                                           uint8_t deassertion_event_state_bit_13,
                                           uint8_t deassertion_event_state_bit_14,
                                           fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SENSOR_EVENT_MESSAGE_ACTION_VALID (event_message_action)
      || !IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_VALID (scanning_on_this_sensor)
      || !IPMI_SENSOR_ALL_EVENT_MESSAGES_VALID (all_event_messages)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_0)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_1)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_2)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_3)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_4)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_5)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_6)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_7)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_8)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_9)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_10)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_11)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_12)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_13)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (assertion_event_state_bit_14)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_0)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_1)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_2)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_3)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_4)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_5)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_6)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_7)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_8)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_9)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_10)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_11)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_12)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_13)
      || !IPMI_SENSOR_EVENT_FLAG_VALID (deassertion_event_state_bit_14)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sensor_event_enable_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SENSOR_EVENT_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_message_action", event_message_action);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "scanning_on_this_sensor", scanning_on_this_sensor);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "all_event_messages", all_event_messages);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_0", assertion_event_state_bit_0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_1", assertion_event_state_bit_1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_2", assertion_event_state_bit_2);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_3", assertion_event_state_bit_3);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_4", assertion_event_state_bit_4);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_5", assertion_event_state_bit_5);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_6", assertion_event_state_bit_6);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_7", assertion_event_state_bit_7);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_8", assertion_event_state_bit_8);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_9", assertion_event_state_bit_9);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_10", assertion_event_state_bit_10);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_11", assertion_event_state_bit_11);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_12", assertion_event_state_bit_12);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_13", assertion_event_state_bit_13);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_state_bit_14", assertion_event_state_bit_14);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_0", deassertion_event_state_bit_0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_1", deassertion_event_state_bit_1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_2", deassertion_event_state_bit_2);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_3", deassertion_event_state_bit_3);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_4", deassertion_event_state_bit_4);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_5", deassertion_event_state_bit_5);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_6", deassertion_event_state_bit_6);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_7", deassertion_event_state_bit_7);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_8", deassertion_event_state_bit_8);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_9", deassertion_event_state_bit_9);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_10", deassertion_event_state_bit_10);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_11", deassertion_event_state_bit_11);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_12", deassertion_event_state_bit_12);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_13", deassertion_event_state_bit_13);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_state_bit_14", deassertion_event_state_bit_14);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);

  return (0);
}

int
fill_cmd_get_sensor_event_enable (uint8_t sensor_number, fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_sensor_event_enable_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SENSOR_EVENT_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);

  return (0);
}

int
fill_cmd_re_arm_sensor_events (uint8_t sensor_number,
			       uint8_t re_arm_all_event_status_from_this_sensor,
			       uint16_t *re_arm_assertion_event,
			       uint16_t *re_arm_deassertion_event,
			       fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SENSOR_RE_ARM_ALL_EVENT_STATUS_VALID (re_arm_all_event_status_from_this_sensor)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_re_arm_sensor_events_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  
  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_RE_ARM_SENSOR_EVENTS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "re_arm_all_event_status_from_this_sensor", re_arm_all_event_status_from_this_sensor);
  if (re_arm_assertion_event)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "re_arm_assertion_event", *re_arm_assertion_event);
  if (re_arm_deassertion_event)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "re_arm_deassertion_event", *re_arm_deassertion_event);
  
  return (0);
}

int
fill_cmd_get_sensor_reading (uint8_t sensor_number, fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_sensor_reading_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SENSOR_READING);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);

  return (0);
}

int
fill_cmd_set_sensor_reading_and_event_status (uint8_t sensor_number,
					      uint8_t sensor_reading_operation,
					      uint8_t deassertion_bits_operation,
					      uint8_t assertion_bits_operation,
					      uint8_t event_data_bytes_operation,
					      uint8_t sensor_reading,
					      uint16_t assertion_event_bitmask,
					      uint16_t deassertion_event_bitmask,
					      uint8_t event_data1,
					      uint8_t event_data2,
					      uint8_t event_data3,
					      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SENSOR_READING_OPERATION_VALID (sensor_reading_operation)
      || !IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_VALID (deassertion_bits_operation)
      || !IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_VALID (assertion_bits_operation)
      || !IPMI_EVENT_DATA_BYTES_OPERATION_VALID (event_data_bytes_operation)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sensor_reading_and_event_status_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SENSOR_READING_AND_EVENT_STATUS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_reading_operation", sensor_reading_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_bits_operation", deassertion_bits_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_bits_operation", assertion_bits_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_data_bytes_operation", event_data_bytes_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_reading", sensor_reading);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_bitmask1", assertion_event_bitmask & 0x00FF);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "assertion_event_bitmask2", (assertion_event_bitmask & 0xFF00) >> 8);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_bitmask1", deassertion_event_bitmask & 0x00FF);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "deassertion_event_bitmask2", (deassertion_event_bitmask & 0xFF00) >> 8);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_data1", event_data1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_data2", event_data2);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_data3", event_data3);

  return (0);
}
