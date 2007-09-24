#ifndef _PEF_CONFIG_DIFF_H
#define _PEF_CONFIG_DIFF_H

#include "pef-config.h"
#include "pef-config-sections.h"

config_err_t config_diff (struct config_section *sections,
                          struct config_arguments *cmd_args,
                          void *arg);

void report_diff (const char *section,
                  const char *key,
                  const char *input_value,
                  const char *actual_value);

#endif
