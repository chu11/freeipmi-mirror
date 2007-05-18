#ifndef _IPMI_PEF_ALERT_DESTINATION_H_
#define _IPMI_PEF_ALERT_DESTINATION_H_

#include "ipmi-pef.h"
#include "ipmi-pef-sections.h"

struct section * ipmi_pef_alert_destination_section_get (ipmi_pef_state_data_t *state_data, int num);

#endif /* _IPMI_PEF_ALERT_DESTINATION_H_ */
