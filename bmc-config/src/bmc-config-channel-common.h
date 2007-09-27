#ifndef _BMC_CONFIG_CHANNEL_COMMON_H_
#define _BMC_CONFIG_CHANNEL_COMMON_H_

#include "bmc-config.h"

int bmc_config_channel_common_section_get(bmc_config_state_data_t *state_data,
                                          struct config_section *channel_section);

#endif /* _BMC_CONFIG_CHANNEL_COMMON_H_ */
