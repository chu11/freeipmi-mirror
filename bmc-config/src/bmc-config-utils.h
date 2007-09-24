#ifndef _BMC_CONFIG_UTILS_H
#define _BMC_CONFIG_UTILS_H

#include "bmc-config.h"

config_err_t get_lan_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_serial_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_sol_channel_number (bmc_config_state_data_t *state_data, uint8_t *channel_num);
config_err_t get_number_of_users (bmc_config_state_data_t *state_data, uint8_t *number_of_users);

#endif
