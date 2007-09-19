#ifndef _CONFIG_DIFF_H_
#define _CONFIG_DIFF_H_

#include <stdio.h>

#include "config-common.h"

config_err_t config_diff(struct config_section *sections,
                         int debug,
                         void *arg);

#endif /* _CONFIG_DIFF_H_ */
