#ifndef _PEF_CONFIG_WRAPPER_H
#define _PEF_CONFIG_WRAPPER_H

config_err_t get_bmc_destination_type(pef_config_state_data_t *state_data,
                                   uint8_t destination_selector,
                                   uint8_t *alert_destination_type,
                                   uint8_t *alert_acknowledge,
                                   uint8_t *alert_acknowledge_timeout,
                                   uint8_t *alert_retries);

config_err_t set_bmc_destination_type(pef_config_state_data_t *state_data,
                                   uint8_t destination_selector,
                                   uint8_t alert_destination_type,
                                   uint8_t alert_acknowledge,
                                   uint8_t alert_acknowledge_timeout,
                                   uint8_t alert_retries);

config_err_t get_bmc_destination_addresses(pef_config_state_data_t *state_data,
                                        uint8_t destination_selector,
                                        uint8_t *alert_gateway,
                                        char *alert_ip_address,
                                        unsigned int alert_ip_address_len,
                                        char *alert_mac_address,
                                        unsigned int alert_mac_address_len);

config_err_t set_bmc_destination_addresses(pef_config_state_data_t *state_data,
                                        uint8_t destination_selector,
                                        uint8_t alert_gateway,
                                        char *alert_ip_address,
                                        char *alert_mac_address);
#endif
