#ifndef _PEF_CONFIG_MAP_H
#define _PEF_CONFIG_MAP_H

int alert_destination_type_number (const char *source);

char *alert_destination_type_string (uint8_t source);

int alert_gateway_number (const char *source);

char *alert_gateway_string (uint8_t source);

int policy_type_number (const char *source);

char *policy_type_string (uint8_t source);

int filter_type_number (const char *source);

char *filter_type_string (uint8_t source);

int event_severity_number (const char *source);

char *event_severity_string (uint8_t source);

int sensor_type_number (const char *source);

char *sensor_type_string (uint8_t source);

#endif
