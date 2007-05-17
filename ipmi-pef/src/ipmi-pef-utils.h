#ifndef _IPMI_PEF_UTILS_H
#define _IPMI_PEF_UTILS_H

#include <stdint.h>

#include "ipmi-pef.h"

int get_lan_channel_number (struct ipmi_pef_state_data *state_data, 
                            int8_t *channel_number);

int get_number_of_lan_destinations (struct ipmi_pef_state_data *state_data, 
				    int8_t *number_of_lan_destinations);

int get_number_of_alert_policy_entries (struct ipmi_pef_state_data *state_data, 
					int8_t *num_alert_policy_entries);

int get_number_of_event_filters (struct ipmi_pef_state_data *state_data, 
                                 int8_t *num_event_filters);

#endif
