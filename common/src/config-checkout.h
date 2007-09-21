#ifndef _CONFIG_CHECKOUT_H_
#define _CONFIG_CHECKOUT_H_

#include <stdio.h>

#include "config-common.h"

config_err_t config_checkout_section(struct config_section *section, 
                                     int all_keys_if_none_specified,
                                     FILE *fp,
                                     int debug,
                                     void *arg);

config_err_t config_checkout_all(struct config_section *sections, 
                                 int all_keys_if_none_specified,
                                 FILE *fp,
                                 int debug,
                                 void *arg);

#endif /* _CONFIG_CHECKOUT_H_ */
