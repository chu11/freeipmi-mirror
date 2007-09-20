#ifndef _BMC_IPMI_WRAPPER_H
#define _BMC_IPMI_WRAPPER_H

#include "bmc-config.h"
#include "bmc-config-common.h"

#include "config-common.h"

config_err_t get_lan_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_serial_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_sol_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_number_of_lan_destinations (bmc_config_state_data_t *state_data, uint8_t *number_of_lan_destinations);

#endif
