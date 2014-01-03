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

#ifndef IPMI_SENSORS_OUTPUT_COMMON_H
#define IPMI_SENSORS_OUTPUT_COMMON_H

#include "ipmi-sensors.h"

#define IPMI_SENSORS_OEM_DATA_LEN       1024

#define IPMI_SENSORS_NA_STRING          "N/A"
#define IPMI_SENSORS_NA_STRING_LEGACY   "NA"
#define IPMI_SENSORS_NONE_STRING        "NONE"
#define IPMIMONITORING_NA_STRING_LEGACY "N/A"

#define IPMI_SENSORS_NA_STRING_OUTPUT                    \
  (state_data->prog_data->args->legacy_output ? IPMI_SENSORS_NA_STRING_LEGACY : IPMI_SENSORS_NA_STRING)

#define IPMI_SENSORS_ASSERTION_EVENT_PREFIX        "Assertion Event Enabled: "
#define IPMI_SENSORS_ASSERTION_EVENT_PREFIX_LEGACY "Assertion Events Enabled: "

#define IPMI_SENSORS_ASSERTION_EVENT_PREFIX_OUTPUT            \
  (state_data->prog_data->args->legacy_output ? IPMI_SENSORS_ASSERTION_EVENT_PREFIX_LEGACY : IPMI_SENSORS_ASSERTION_EVENT_PREFIX)

#define IPMI_SENSORS_DEASSERTION_EVENT_PREFIX        "Deassertion Event Enabled: "
#define IPMI_SENSORS_DEASSERTION_EVENT_PREFIX_LEGACY "Deassertion Events Enabled: "

#define IPMI_SENSORS_DEASSERTION_EVENT_PREFIX_OUTPUT            \
  (state_data->prog_data->args->legacy_output ? IPMI_SENSORS_DEASSERTION_EVENT_PREFIX_LEGACY : IPMI_SENSORS_DEASSERTION_EVENT_PREFIX)

#define IPMI_SENSORS_SENSOR_EVENT_PREFIX        "Sensor Event: "
#define IPMI_SENSORS_SENSOR_EVENT_PREFIX_LEGACY "Sensor Status: "

#define IPMI_SENSORS_SENSOR_EVENT_PREFIX_OUTPUT                \
  (state_data->prog_data->args->legacy_output ? IPMI_SENSORS_SENSOR_EVENT_PREFIX_LEGACY : IPMI_SENSORS_SENSOR_EVENT_PREFIX)

#define IPMI_SENSORS_NO_EVENT_STRING "OK"

#define IPMI_SENSORS_UNITS_BUFLEN    1024

#define IPMI_SENSORS_EVENT_NORMAL  0x0
#define IPMI_SENSORS_EVENT_NA      0x1
#define IPMI_SENSORS_EVENT_UNKNOWN 0x2

#define IPMI_SENSORS_EVENT_VALID(__val) \
  (((__val) == IPMI_SENSORS_EVENT_NORMAL \
    || (__val) == IPMI_SENSORS_EVENT_NA \
    || (__val) == IPMI_SENSORS_EVENT_UNKNOWN) ? 1 : 0)

int ipmi_sensors_output_event_message_list (ipmi_sensors_state_data_t *state_data,
                                            int event_message_output_type,
                                            uint16_t sensor_event_bitmask,
                                            char **event_message_list,
                                            unsigned int event_message_list_len,
                                            char *prefix,
                                            unsigned int each_on_newline);

int ipmi_sensors_get_thresholds (ipmi_sensors_state_data_t *state_data,
                                 double **lower_non_critical_threshold,
                                 double **lower_critical_threshold,
                                 double **lower_non_recoverable_threshold,
                                 double **upper_non_critical_threshold,
                                 double **upper_critical_threshold,
                                 double **upper_non_recoverable_threshold);

int ipmi_sensors_get_sensor_state (ipmi_sensors_state_data_t *state_data,
                                   int event_message_output_type,
                                   uint16_t sensor_event_bitmask,
                                   char **sensor_state_str);

#endif /* IPMI_SENSORS_OUTPUT_COMMON_H */
