#ifndef _PEF_CONFIG_VALIDATE_H
#define _PEF_CONFIG_VALIDATE_H

#include "pef-config.h"
#include "pef-config-common.h"
#include "pef-config-sections.h"

pef_validate_t yes_no_validate (pef_config_state_data_t *state_data, 
                                const struct section *sect, 
                                const char *value);

pef_validate_t number_range_three_bits (pef_config_state_data_t *state_data, 
                                        const struct section *sect, 
                                        const char *value);

pef_validate_t number_range_four_bits (pef_config_state_data_t *state_data, 
                                       const struct section *sect, 
                                       const char *value);

pef_validate_t number_range_seven_bits (pef_config_state_data_t *state_data, 
                                        const struct section *sect, 
                                        const char *value);

pef_validate_t number_range_one_byte (pef_config_state_data_t *state_data, 
                                      const struct section *sect, 
                                      const char *value);

pef_validate_t number_range_two_bytes (pef_config_state_data_t *state_data,
                                       const struct section *sect,
                                       const char *value);

pef_validate_t ip_address_validate (pef_config_state_data_t *state_data,
                                    const struct section *sect,
                                    const char *value);

pef_validate_t mac_address_validate (pef_config_state_data_t *state_data,
                                     const struct section *sect,
                                     const char *value);

pef_validate_t alert_destination_type_validate (pef_config_state_data_t *state_data, 
                                                const struct section *sect, 
                                                const char *value);

pef_validate_t alert_gateway_validate (pef_config_state_data_t *state_data, 
                                       const struct section *sect, 
                                       const char *value);

pef_validate_t policy_type_validate (pef_config_state_data_t *state_data,
                                     const struct section *sect,
                                     const char *value);

pef_validate_t filter_type_validate (pef_config_state_data_t *state_data,
                                     const struct section *sect,
                                     const char *value);

pef_validate_t event_severity_validate (pef_config_state_data_t *state_data,
                                        const struct section *sect,
                                        const char *value);

pef_validate_t sensor_type_validate (pef_config_state_data_t *state_data,
                                        const struct section *sect,
                                        const char *value);

#endif
