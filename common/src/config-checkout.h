#ifndef _CONFIG_CHECKOUT_H_
#define _CONFIG_CHECKOUT_H_

#include "config-common.h"

config_err_t config_checkout_section(struct config_section *section,
                                     struct config_arguments *cmd_args,
                                     int all_keys_if_none_specified,
                                     FILE *fp,
                                     void *arg);

config_err_t config_checkout (struct config_section *sections,
                              struct config_arguments *cmd_args,
                              int all_keys_if_none_specified,
                              FILE *fp,
                              void *arg);

#endif /* _CONFIG_CHECKOUT_H_ */
