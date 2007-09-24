#ifndef _PEF_CONFIG_COMMIT_H
#define _PEF_CONFIG_COMMIT_H

#include "pef-config.h"

config_err_t pef_commit (struct config_section *sections,
                         struct config_arguments *cmd_args,
                         void *arg);

#endif
