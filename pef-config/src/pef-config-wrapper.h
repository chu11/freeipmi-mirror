#ifndef _PEF_CONFIG_WRAPPER_H
#define _PEF_CONFIG_WRAPPER_H

pef_err_t get_pef_control (pef_config_state_data_t *state_data,
                           uint8_t *pef,
                           uint8_t *pef_event_messages,
                           uint8_t *pef_startup_delay,
                           uint8_t *pef_alert_startup_delay);

pef_err_t set_pef_control (pef_config_state_data_t *state_data,
                           uint8_t pef,
                           uint8_t pef_event_messages,
                           uint8_t pef_startup_delay,
                           uint8_t pef_alert_startup_delay);

pef_err_t get_pef_action_global_control (pef_config_state_data_t *state_data,
                                         uint8_t *alert_action,
                                         uint8_t *power_down_action,
                                         uint8_t *reset_action,
                                         uint8_t *power_cycle_action,
                                         uint8_t *oem_action,
                                         uint8_t *diagnostic_interrupt);

pef_err_t set_pef_action_global_control (pef_config_state_data_t *state_data,
                                         uint8_t alert_action,
                                         uint8_t power_down_action,
                                         uint8_t reset_action,
                                         uint8_t power_cycle_action,
                                         uint8_t oem_action,
                                         uint8_t diagnostic_interrupt);

pef_err_t get_pef_startup_delay (pef_config_state_data_t *state_data,
                                 uint8_t *pef_startup_delay);

pef_err_t set_pef_startup_delay (pef_config_state_data_t *state_data,
                                 uint8_t pef_startup_delay);

pef_err_t set_pef_alert_startup_delay (pef_config_state_data_t *state_data,
                                       uint8_t pef_alert_startup_delay);

pef_err_t get_pef_alert_startup_delay (pef_config_state_data_t *state_data,
                                       uint8_t *pef_alert_startup_delay);

pef_err_t get_pef_alert_string_keys (pef_config_state_data_t *state_data,
                                     uint8_t string_selector,
                                     uint8_t *event_filter_number,
                                     uint8_t *alert_string_set);
     
pef_err_t set_pef_alert_string_keys (pef_config_state_data_t *state_data,
                                     uint8_t string_selector,
                                     uint8_t event_filter_number,
                                     uint8_t alert_string_set);

pef_err_t get_pef_alert_string (pef_config_state_data_t *state_data,
                                uint8_t string_selector,
                                uint8_t *alert_string,
                                uint32_t alert_string_len);

pef_err_t set_pef_alert_string (pef_config_state_data_t *state_data,
                                uint8_t string_selector,
                                uint8_t *alert_string);
     
pef_err_t get_bmc_community_string (pef_config_state_data_t *state_data,
                                    char *community_string,
                                    uint32_t community_string_len);

pef_err_t set_bmc_community_string (pef_config_state_data_t *state_data,
                                    char *community_string);

pef_err_t get_bmc_destination_type(pef_config_state_data_t *state_data,
                                   uint8_t destination_selector,
                                   uint8_t *alert_destination_type,
                                   uint8_t *alert_acknowledge,
                                   uint8_t *alert_acknowledge_timeout,
                                   uint8_t *alert_retries);

pef_err_t set_bmc_destination_type(pef_config_state_data_t *state_data,
                                   uint8_t destination_selector,
                                   uint8_t alert_destination_type,
                                   uint8_t alert_acknowledge,
                                   uint8_t alert_acknowledge_timeout,
                                   uint8_t alert_retries);

pef_err_t get_bmc_destination_addresses(pef_config_state_data_t *state_data,
                                        uint8_t destination_selector,
                                        uint8_t *alert_gateway,
                                        char *alert_ip_address,
                                        unsigned int alert_ip_address_len,
                                        char *alert_mac_address,
                                        unsigned int alert_mac_address_len);

pef_err_t set_bmc_destination_addresses(pef_config_state_data_t *state_data,
                                        uint8_t destination_selector,
                                        uint8_t alert_gateway,
                                        char *alert_ip_address,
                                        char *alert_mac_address);

pef_err_t get_bmc_pef_conf_alert_policy_table (struct pef_config_state_data *state_data, 
                                               uint8_t alert_policy_entry_number,
                                               uint8_t *policy_type,
                                               uint8_t *policy_enabled,
                                               uint8_t *policy_number,
                                               uint8_t *destination_selector,
                                               uint8_t *channel_number,
                                               uint8_t *alert_string_set_selector,
                                               uint8_t *event_specific_alert_string);

pef_err_t set_bmc_pef_conf_alert_policy_table (struct pef_config_state_data *state_data, 
                                               uint8_t alert_policy_entry_number,
                                               uint8_t policy_type,
                                               uint8_t policy_enabled,
                                               uint8_t policy_number,
                                               uint8_t destination_selector,
                                               uint8_t channel_number,
                                               uint8_t alert_string_set_selector,
                                               uint8_t event_specific_alert_string);

pef_err_t get_bmc_pef_conf_event_filter_table (struct pef_config_state_data *state_data, 
                                               uint8_t filter_number,
                                               uint8_t *filter_type,
                                               uint8_t *enable_filter,
                                               uint8_t *event_filter_action_alert,
                                               uint8_t *event_filter_action_power_off,
                                               uint8_t *event_filter_action_reset,
                                               uint8_t *event_filter_action_power_cycle,
                                               uint8_t *event_filter_action_oem,
                                               uint8_t *event_filter_action_diagnostic_interrupt,
                                               uint8_t *event_filter_action_group_control_operation,
                                               uint8_t *alert_policy_number,
                                               uint8_t *group_control_selector,
                                               uint8_t *event_severity,
                                               uint8_t *generator_id_byte1,
                                               uint8_t *generator_id_byte2,
                                               uint8_t *sensor_type,
                                               uint8_t *sensor_number,
                                               uint8_t *event_trigger,
                                               uint16_t *event_data1_offset_mask,
                                               uint8_t *event_data1_AND_mask,
                                               uint8_t *event_data1_compare1,
                                               uint8_t *event_data1_compare2,
                                               uint8_t *event_data2_AND_mask,
                                               uint8_t *event_data2_compare1,
                                               uint8_t *event_data2_compare2,
                                               uint8_t *event_data3_AND_mask,
                                               uint8_t *event_data3_compare1,
                                               uint8_t *event_data3_compare2);

pef_err_t set_bmc_pef_conf_event_filter_table (struct pef_config_state_data *state_data, 
                                               uint8_t filter_number,
                                               uint8_t filter_type,
                                               uint8_t enable_filter,
                                               uint8_t event_filter_action_alert,
                                               uint8_t event_filter_action_power_off,
                                               uint8_t event_filter_action_reset,
                                               uint8_t event_filter_action_power_cycle,
                                               uint8_t event_filter_action_oem,
                                               uint8_t event_filter_action_diagnostic_interrupt,
                                               uint8_t event_filter_action_group_control_operation,
                                               uint8_t alert_policy_number,
                                               uint8_t group_control_selector,
                                               uint8_t event_severity,
                                               uint8_t generator_id_byte1,
                                               uint8_t generator_id_byte2,
                                               uint8_t sensor_type,
                                               uint8_t sensor_number,
                                               uint8_t event_trigger,
                                               uint16_t event_data1_offset_mask,
                                               uint8_t event_data1_AND_mask,
                                               uint8_t event_data1_compare1,
                                               uint8_t event_data1_compare2,
                                               uint8_t event_data2_AND_mask,
                                               uint8_t event_data2_compare1,
                                               uint8_t event_data2_compare2,
                                               uint8_t event_data3_AND_mask,
                                               uint8_t event_data3_compare1,
                                               uint8_t event_data3_compare2);

#endif
