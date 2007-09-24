#ifndef _PEF_CONFIG_VALIDATE_H
#define _PEF_CONFIG_VALIDATE_H

#include "pef-config.h"

config_validate_t alert_destination_type_validate (const char *section_name, 
                                                   const char *key_name, 
                                                   const char *value);

config_validate_t alert_gateway_validate (const char *section_name, 
                                          const char *key_name, 
                                          const char *value);

config_validate_t policy_type_validate (const char *section_name,
                                        const char *key_name,
                                        const char *value);

config_validate_t filter_type_validate (const char *section_name,
                                        const char *key_name,
                                        const char *value);

config_validate_t event_severity_validate (const char *section_name,
                                           const char *key_name,
                                           const char *value);

config_validate_t sensor_type_validate (const char *section_name,
                                        const char *key_name,
                                        const char *value);

#endif
