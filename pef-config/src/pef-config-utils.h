#ifndef _PEF_CONFIG_UTILS_H
#define _PEF_CONFIG_UTILS_H

#include <stdint.h>

#include "pef-config.h"

config_err_t get_lan_channel_number (struct pef_config_state_data *state_data, 
                                  uint8_t *channel_number);

config_err_t get_number_of_lan_alert_destinations (struct pef_config_state_data *state_data, 
                                                uint8_t *number_of_lan_alert_destinations);

config_err_t get_number_of_alert_strings (struct pef_config_state_data *state_data, 
                                       uint8_t *number_of_alert_strings);

config_err_t get_number_of_alert_policy_entries (struct pef_config_state_data *state_data, 
                                              uint8_t *number_of_alert_policy_entries);

config_err_t get_number_of_event_filters (struct pef_config_state_data *state_data, 
                                       uint8_t *number_of_event_filters);

#endif
