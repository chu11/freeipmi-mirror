#ifndef _CONFIG_DIFF_H
#define _CONFIG_DIFF_H

#include "config-common.h"

config_err_t config_diff (struct config_section *sections,
                          struct config_arguments *cmd_args,
                          void *arg);

void report_diff (const char *section,
                  const char *key,
                  const char *input_value,
                  const char *actual_value);

#endif  /* _CONFIG_DIFF_H */
