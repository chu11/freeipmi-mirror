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

config_err_t get_bmc_pef_conf_alert_policy_table (struct pef_config_state_data *state_data, 
                                               uint8_t alert_policy_entry_number,
                                               uint8_t *policy_type,
                                               uint8_t *policy_enabled,
                                               uint8_t *policy_number,
                                               uint8_t *destination_selector,
                                               uint8_t *channel_number,
                                               uint8_t *alert_string_set_selector,
                                               uint8_t *event_specific_alert_string);

config_err_t set_bmc_pef_conf_alert_policy_table (struct pef_config_state_data *state_data, 
                                               uint8_t alert_policy_entry_number,
                                               uint8_t policy_type,
                                               uint8_t policy_enabled,
                                               uint8_t policy_number,
                                               uint8_t destination_selector,
                                               uint8_t channel_number,
                                               uint8_t alert_string_set_selector,
                                               uint8_t event_specific_alert_string);
#endif
