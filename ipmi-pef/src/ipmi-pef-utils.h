#ifndef _IPMI_PEF_UTILS_H
#define _IPMI_PEF_UTILS_H

#include <stdint.h>

#include "ipmi-pef.h"

pef_err_t get_lan_channel_number (struct ipmi_pef_state_data *state_data, 
                                  int8_t *channel_number);

pef_err_t get_number_of_lan_alert_destinations (struct ipmi_pef_state_data *state_data, 
                                                int8_t *number_of_lan_alert_destinations);

pef_err_t get_number_of_alert_policy_entries (struct ipmi_pef_state_data *state_data, 
                                              int8_t *number_of_alert_policy_entries);

pef_err_t get_number_of_event_filters (struct ipmi_pef_state_data *state_data, 
                                       int8_t *number_of_event_filters);

#endif
