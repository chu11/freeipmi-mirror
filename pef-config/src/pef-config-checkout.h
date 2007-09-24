#ifndef _PEF_CONFIG_CHECKOUT_H
#define _PEF_CONFIG_CHECKOUT_H

#include "pef-config.h"

config_err_t pef_checkout (struct config_section *sections,
                           struct config_arguments *cmd_args,
                           void *arg);

#endif
