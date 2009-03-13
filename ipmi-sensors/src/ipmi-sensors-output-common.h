/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _IPMI_SENSORS_OUTPUT_COMMON_H
#define _IPMI_SENSORS_OUTPUT_COMMON_H

#include "ipmi-sensors.h"

#define IPMI_SENSORS_NA_STRING        "N/A"
#define IPMI_SENSORS_NA_STRING_LEGACY "NA"
#define IPMI_SENSORS_NONE_STRING      "NONE"

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

int ipmi_sensors_output_event_message_list (ipmi_sensors_state_data_t *state_data,
                                            char **event_message_list,
                                            unsigned int event_message_list_len,
                                            char *prefix,
                                            unsigned int each_on_newline);

int ipmi_sensors_get_thresholds (ipmi_sensors_state_data_t *state_data,
                                 uint8_t *sdr_record,
                                 unsigned int sdr_record_len,
                                 double **lower_non_critical_threshold,
                                 double **lower_critical_threshold,
                                 double **lower_non_recoverable_threshold,
                                 double **upper_non_critical_threshold,
                                 double **upper_critical_threshold,
                                 double **upper_non_recoverable_threshold);

#endif
