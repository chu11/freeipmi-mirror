#ifndef _BMC_CONF_CHECKOUT_H
#define _BMC_CONF_CHECKOUT_H

#define PASSWORD_MASK    "******"

#define SET_SELECTOR      0x0
#define BLOCK_SELECTOR    0x0

int kcs_bmc_lan_get_arp_checkout (FILE *fp);
int kcs_lan_get_gratuitous_arp_interval_checkout (FILE *fp);
int kcs_lan_get_auth_type_enables_checkout (FILE *fp);
int kcs_lan_get_ip_addr_source_checkout (FILE *fp);
int kcs_lan_get_ip_addr_checkout (FILE *fp);
int kcs_lan_get_gw1_ip_addr_checkout (FILE *fp);
int kcs_lan_get_gw2_ip_addr_checkout (FILE *fp);
int kcs_lan_get_subnet_mask_checkout (FILE *fp);
int kcs_lan_get_mac_addr_checkout (FILE *fp);
int kcs_lan_get_gw1_mac_addr_checkout (FILE *fp);
int kcs_lan_get_gw2_mac_addr_checkout (FILE *fp);
int get_user_name_checkout (FILE *fp);
int get_user_password_checkout (FILE *fp);
int get_user_access_checkout (FILE *fp);
int get_channel_access_checkout (FILE *fp);
int get_serial_connmode_checkout (FILE *fp);
int get_serial_page_blackout_checkout (FILE *fp);
int get_serial_retry_time_checkout (FILE *fp);
int get_serial_comm_bits_checkout (FILE *fp);
int get_chassis_status_checkout (FILE *fp);
int get_power_restore_policy_checkout (FILE *fp);
int bmc_config_checkout (char *filename);





#endif
