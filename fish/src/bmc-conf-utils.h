#ifndef _BMC_CONF_UTILS_H
#define _BMC_CONF_UTILS_H

int check_user_password (int user_id, char *password);
int edit_key_pair_bmc_config_file (char *filename, char *key, char *value);
int bmc_config_diff_key_pair (char *bmc_filename, char *user_key, char *user_value);
int bmc_config_diff_file (char *bmc_filename, char *user_filename);

/* int check_bmc_config_file_key (char *filename, char *key); */
/* int edit_key_pair_bmc_config_file (char *filename, char *key, char *value); */
/* int diff_key_pair_bmc_config_file (char *filename, char *key, char *value); */
/* int diff_file_bmc_config_file (char *bmc_filename, char *filename); */

#endif
