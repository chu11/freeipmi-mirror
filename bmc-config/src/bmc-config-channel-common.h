#ifndef _BMC_IPMI_WRAPPER_H
#define _BMC_IPMI_WRAPPER_H

#include "bmc-config.h"
#include "bmc-config-common.h"

#include "config-common.h"

config_err_t channel_checkout(const char *section_name,
                              struct config_keyvalue *keyvalues,
                              int debug,
                              void *arg);

config_err_t channel_commit(const char *section_name,
                            struct config_keyvalue *keyvalues,
                            int debug,
                            void *arg);

int channel_section_get(bmc_config_state_data_t *state_data,
                        struct config_section *channel_section);

#endif
