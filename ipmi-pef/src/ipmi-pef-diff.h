#ifndef _IPMI_PEF_DIFF_H
#define _IPMI_PEF_DIFF_H

#include "ipmi-pef.h"
#include "ipmi-pef-sections.h"

pef_err_t pef_diff (ipmi_pef_state_data_t *state_data);

void report_diff (const char *section,
                  const char *key,
                  const char *input_value,
                  const char *actual_value);

#endif
