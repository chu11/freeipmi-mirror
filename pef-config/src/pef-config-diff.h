#ifndef _PEF_CONFIG_DIFF_H
#define _PEF_CONFIG_DIFF_H

#include "pef-config.h"

#include "config-common.h"

config_err_t pef_diff (pef_config_state_data_t *state_data);

void report_diff (const char *section,
                  const char *key,
                  const char *input_value,
                  const char *actual_value);

#endif
