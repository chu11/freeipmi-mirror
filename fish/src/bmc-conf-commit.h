#ifndef _BMC_CONF_COMMIT_H
#define _BMC_CONF_COMMIT_H

/* #define BMC_LAN_CHANNEL_NUMBER       0x7 */
/* #define BMC_SERIAL_CHANNEL_NUMBER    0x1 */

/* #define SET_SELECTOR      0x0 */
/* #define BLOCK_SELECTOR    0x0 */

int kcs_bmc_lan_set_arp_commit (FILE *fp);
int kcs_lan_set_gratuitous_arp_interval_commit (FILE *fp);
int kcs_lan_set_auth_type_enables_commit (FILE *fp);
int kcs_lan_set_ip_addr_source_commit (FILE *fp);
int kcs_lan_set_ip_addr_commit (FILE *fp);
int kcs_lan_set_gw1_ip_addr_commit (FILE *fp);
int kcs_lan_set_gw2_ip_addr_commit (FILE *fp);
int kcs_lan_set_subnet_mask_commit (FILE *fp);
int kcs_lan_set_mac_addr_commit (FILE *fp);
int kcs_lan_set_gw1_mac_addr_commit (FILE *fp);
int kcs_lan_set_gw2_mac_addr_commit (FILE *fp);
int set_user_name_commit (FILE *fp);
int set_user_password_commit (FILE *fp);
int set_user_access_commit (FILE *fp);
int set_channel_access_commit (FILE *fp);
int set_serial_connmode_commit (FILE *fp);
int set_serial_page_blackout_commit (FILE *fp);
int set_serial_retry_time_commit (FILE *fp);
int set_serial_comm_bits_commit (FILE *fp);
int set_power_restore_policy_commit (FILE *fp);
int bmc_config_commit (char *filename);


#endif
