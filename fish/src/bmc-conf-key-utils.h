#ifndef _BMC_CONFIG_KEY_UTILS_H
#define _BMC_CONFIG_KEY_UTILS_H

int bmc_config_get_key_value (char *line, char **key, char **value);
int bmc_config_validate_key (char *key);
int bmc_config_validate_value (char *key, char *value);

#endif
