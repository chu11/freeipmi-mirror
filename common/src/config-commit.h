#ifndef _CONFIG_COMMIT_H_
#define _CONFIG_COMMIT_H_

#include "config-common.h"

config_err_t config_commit (struct config_section *sections,
                            struct config_arguments *cmd_args,
                            void *arg);

#endif /* _CONFIG_COMMIT_H_ */
