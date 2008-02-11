#ifndef _IPMI_SENSORS_CONFIG_UTILS_H
#define _IPMI_SENSORS_CONFIG_UTILS_H

#include "ipmi-sensors-config.h"

config_err_t convert_id_string (ipmi_sensors_config_state_data_t *state_data, 
                                char *id_string);

#endif
