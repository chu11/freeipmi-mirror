#ifndef _IPMI_PEF_ALERT_POLICY_TABLE_H_
#define _IPMI_PEF_ALERT_POLICY_TABLE_H_

#include "ipmi-pef.h"
#include "ipmi-pef-sections.h"

struct section * ipmi_pef_alert_policy_table_section_get (ipmi_pef_state_data_t *state_data, int num);

#endif /* _IPMI_PEF_ALERT_POLICY_TABLE_H_ */
