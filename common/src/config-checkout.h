#ifndef _CONFIG_CHECKOUT_H_
#define _CONFIG_CHECKOUT_H_

#include "config-common.h"

config_err_t config_checkout (struct config_section *sections,
                              struct config_arguments *cmd_args,
                              void *arg);

#endif /* _CONFIG_CHECKOUT_H_ */
