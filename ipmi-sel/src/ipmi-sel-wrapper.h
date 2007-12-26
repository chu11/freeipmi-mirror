#ifndef _IPMI_SEL_WRAPPER_H
#define _IPMI_SEL_WRAPPER_H

#define SEL_RECORD_SIZE 16

#include "ipmi-sel.h"

int ipmi_sel_record_get (ipmi_sel_state_data_t *state_data,
                         uint16_t record_id, 
                         uint16_t *next_record_id,
                         char **timestamp,
                         char **sensor_info,
                         char **event_message,
                         char **event_data2_message,
                         char **event_data3_message);

#endif
