#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <freeipmi.h>
#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include <stdlib.h>

#include "fish.h"
#include "fi-utils.h"
#include "ipmi-wrapper.h"
#include "bmc-conf-checkout.h"

int 
kcs_bmc_lan_get_arp_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_bmc_generated_arp_control_rs));
  
  status = ipmi_lan_get_arp (fi_get_sms_io_base (), 
			     get_lan_channel_number (), 
			     IPMI_LAN_CONF_GET_PARAMETER, 
			     SET_SELECTOR, 
			     BLOCK_SELECTOR, 
			     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_arp: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_arp");
      return (-1);
    }
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_bmc_generated_arp_control_rs, 
		"bmc_generated_gratuitous_arps_flag", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "bmc_generated_gratuitous_arps_flag", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_bmc_generated_arp_control_rs, 
		"bmc_generated_arp_responses_flag", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "bmc_generated_arp_responses_flag", 
	   (int) val);
  
  
  return (0);
}


int 
kcs_lan_get_gratuitous_arp_interval_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_gratuitous_arp_interval_rs));
  
  status = ipmi_lan_get_gratuitous_arp_interval (fi_get_sms_io_base (), 
						 get_lan_channel_number (), 
						 IPMI_LAN_CONF_GET_PARAMETER, 
						 SET_SELECTOR, 
						 BLOCK_SELECTOR, 
						 obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_gratuitous_arp_interval: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_gratuitous_arp_interval");
      return (-1);
    }
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_gratuitous_arp_interval_rs, 
		"gratuitous_arp_interval", 
		&val);
  fprintf (fp, 
	   "###   Gratuitous ARP interval in 500 millisecond increments.\n");
  fprintf (fp, 
	   "%s %u\n\n", 
	   "gratuitous_arp_interval", 
	   (u_int8_t) val);
  
  
  return (0);
}


int 
kcs_lan_get_auth_type_enables_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_auth_type_enables_rs));
  
  status = ipmi_lan_get_auth_type_enables (fi_get_sms_io_base (), 
					   get_lan_channel_number (), 
					   IPMI_LAN_CONF_GET_PARAMETER, 
					   SET_SELECTOR, 
					   BLOCK_SELECTOR, 
					   obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_auth_type_enables: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_auth_type_enables");
      return (-1);
    }
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.none", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "callback_level.max_privilege_auth_type.none", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.md2", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "callback_level.max_privilege_auth_type.md2", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.md5", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "callback_level.max_privilege_auth_type.md5", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.straight_password", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "callback_level.max_privilege_auth_type.straight_password", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.oem_proprietary", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "callback_level.max_privilege_auth_type.oem_proprietary", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.none", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "user_level.max_privilege_auth_type.none", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.md2", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "user_level.max_privilege_auth_type.md2", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.md5", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "user_level.max_privilege_auth_type.md5", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.straight_password", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "user_level.max_privilege_auth_type.straight_password", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.oem_proprietary", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "user_level.max_privilege_auth_type.oem_proprietary", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.none", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "operator_level.max_privilege_auth_type.none", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.md2", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "operator_level.max_privilege_auth_type.md2", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.md5", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "operator_level.max_privilege_auth_type.md5", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.straight_password", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "operator_level.max_privilege_auth_type.straight_password", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.oem_proprietary", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "operator_level.max_privilege_auth_type.oem_proprietary", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.none", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "admin_level.max_privilege_auth_type.none", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.md2", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "admin_level.max_privilege_auth_type.md2", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.md5", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "admin_level.max_privilege_auth_type.md5", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.straight_password", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "admin_level.max_privilege_auth_type.straight_password", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.oem_proprietary", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "admin_level.max_privilege_auth_type.oem_proprietary", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.none", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "oem_level.max_privilege_auth_type.none", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.md2", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "oem_level.max_privilege_auth_type.md2", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.md5", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "oem_level.max_privilege_auth_type.md5", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.straight_password", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "oem_level.max_privilege_auth_type.straight_password", 
	   (int) val);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.oem_proprietary", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "oem_level.max_privilege_auth_type.oem_proprietary", 
	   (int) val);
  
  
  return (0);
}


int 
kcs_lan_get_ip_addr_source_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_ip_addr_source_rs));
  
  status = ipmi_lan_get_ip_addr_source (fi_get_sms_io_base (), 
					get_lan_channel_number (), 
					IPMI_LAN_CONF_GET_PARAMETER, 
					SET_SELECTOR, 
					BLOCK_SELECTOR, 
					obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_ip_addr_source: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_ip_addr_source");
      return (-1);
    }
  
  
  fprintf (fp, "###   values: Unspecified = 0, static address (manually configured) = 1, \n"
	   "###   Address obtained by BMC running DHCP = 2, address loaded by BIOS or system software = 3, \n"
	   "###   Address obtained by BMC running other address assignment protocol = 4\n");
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_ip_addr_source_rs, 
		"ip_addr_source", 
		&val);
  fprintf (fp, 
	   "%s %u\n\n", 
	   "ip_addr_source", 
	   (u_int8_t) val);
  
  
  return (0);
}


int 
kcs_lan_get_ip_addr_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_ip_addr_rs));
  
  status = ipmi_lan_get_ip_addr (fi_get_sms_io_base (), 
				 get_lan_channel_number (), 
				 IPMI_LAN_CONF_GET_PARAMETER, 
				 SET_SELECTOR, 
				 BLOCK_SELECTOR, 
				 obj_data_rs);
 
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_ip_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_ip_addr");
      return (-1);
    }
  
  fprintf (fp, 
	   "%s %u.%u.%u.%u\n\n", 
	   "ip_addr", 
	   obj_data_rs[3], 
	   obj_data_rs[4], 
	   obj_data_rs[5], 
	   obj_data_rs[6]);
  
  
  return (0);
}


int 
kcs_lan_get_gw1_ip_addr_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_gw_ip_addr_rs));
  
  status = ipmi_lan_get_gw1_ip_addr (fi_get_sms_io_base (), 
				     get_lan_channel_number (), 
				     IPMI_LAN_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_gw1_ip_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_gw1_ip_addr");
      return (-1);
    }
  
  fprintf (fp, 
	   "gw1_%s %u.%u.%u.%u\n\n", 
	   "ip_addr", 
	   obj_data_rs[3], 
	   obj_data_rs[4], 
	   obj_data_rs[5], 
	   obj_data_rs[6]);
  
  
  return (0);
}


int 
kcs_lan_get_gw2_ip_addr_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_gw_ip_addr_rs));
  
  status = ipmi_lan_get_gw2_ip_addr (fi_get_sms_io_base (), 
				     get_lan_channel_number (), 
				     IPMI_LAN_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_gw2_ip_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_gw2_ip_addr");
      return (-1);
    }
  
  fprintf (fp, 
	   "gw2_%s %u.%u.%u.%u\n\n", 
	   "ip_addr", 
	   obj_data_rs[3], 
	   obj_data_rs[4], 
	   obj_data_rs[5], 
	   obj_data_rs[6]);
  
  
  return (0);
}


int 
kcs_lan_get_subnet_mask_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_subnet_mask_rs));
  
  status = ipmi_lan_get_subnet_mask (fi_get_sms_io_base (), 
				     get_lan_channel_number (), 
				     IPMI_LAN_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_subnet_mask: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_subnet_mask");
      return (-1);
    }
  
  fprintf (fp, 
	   "%s %u.%u.%u.%u\n\n", 
	   "subnet_mask", 
	   obj_data_rs[3], 
	   obj_data_rs[4], 
	   obj_data_rs[5], 
	   obj_data_rs[6]);
  
  
  return (0);
}


int 
kcs_lan_get_mac_addr_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_mac_addr_rs));
  
  status = ipmi_lan_get_mac_addr (fi_get_sms_io_base (), 
				  get_lan_channel_number (), 
				  IPMI_LAN_CONF_GET_PARAMETER, 
				  SET_SELECTOR, 
				  BLOCK_SELECTOR, 
				  obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_mac_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_mac_addr");
      return (-1);
    }
  
  fprintf (fp, 
	   "%s %02X:%02X:%02X:%02X:%02X:%02X\n\n", 
	   "mac_addr", 
	   obj_data_rs[3], 
	   obj_data_rs[4], 
	   obj_data_rs[5], 
	   obj_data_rs[6], 
	   obj_data_rs[7], 
	   obj_data_rs[8]);
  
  
  return (0);
}


int 
kcs_lan_get_gw1_mac_addr_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_mac_addr_rs));
  
  status = ipmi_lan_get_gw1_mac_addr (fi_get_sms_io_base (), 
				      get_lan_channel_number (), 
				      IPMI_LAN_CONF_GET_PARAMETER, 
				      SET_SELECTOR, 
				      BLOCK_SELECTOR, 
				      obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_gw1_mac_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_gw1_mac_addr");
      return (-1);
    }
  
  fprintf (fp, 
	   "gw1_%s %02X:%02X:%02X:%02X:%02X:%02X\n\n", 
	   "mac_addr", 
	   obj_data_rs[3], 
	   obj_data_rs[4], 
	   obj_data_rs[5], 
	   obj_data_rs[6], 
	   obj_data_rs[7], 
	   obj_data_rs[8]);
  
  
  return (0);
}


int 
kcs_lan_get_gw2_mac_addr_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_mac_addr_rs));
  
  status = ipmi_lan_get_gw2_mac_addr (fi_get_sms_io_base (), 
				      get_lan_channel_number (), 
				      IPMI_LAN_CONF_GET_PARAMETER, 
				      SET_SELECTOR, 
				      BLOCK_SELECTOR, 
				      obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_get_gw2_mac_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_get_gw2_mac_addr");
      return (-1);
    }
  
  fprintf (fp,
	   "gw2_%s %02X:%02X:%02X:%02X:%02X:%02X\n\n",
	   "mac_addr",
	   obj_data_rs[3],
	   obj_data_rs[4],
	   obj_data_rs[5],
	   obj_data_rs[6],
	   obj_data_rs[7],
	   obj_data_rs[8]);
  
  
  return (0);
}


int 
get_user_name_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  int user_id;
  int j;
  
  for (user_id = 1; user_id <= 4; user_id++)
    {
      obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_user_name_rs));
      
      status = ipmi_kcs_get_user_name (fi_get_sms_io_base (), 
				       user_id, 
				       obj_data_rs);
      
      if (user_id == 1)
	{
	  fprintf (fp, "### user1_name should always be NULL\n");
	  fprintf (fp, "# %s%d_name ", "user", user_id);
	}
      else 
	fprintf (fp, "%s%d_name ", "user", user_id);
      
      if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
	{
	  char err_msg[IPMI_ERR_STR_MAX_LEN];
	  ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	  fprintf (stderr, 
		   "error: ipmi_kcs_get_user_name: %d: %s\n", 
		   IPMI_COMP_CODE(obj_data_rs), err_msg);
	}
      else 
	if (status != 0)
	  {
	    perror ("ipmi_kcs_get_user_name");
	  }
	else 
	  {
	    if (user_id == 1)
	      fprintf (fp, "NULL");
	    else 
	      {
		for (j = 0; obj_data_rs[j + 2] && j < 16; j++)
		  fprintf (fp, "%c", obj_data_rs[j + 2]);
	      }
	    fprintf (fp, "\n\n");
	  }
    }
  
  
  return (0);
}


int 
get_user_password_checkout (FILE *fp)
{
  fprintf (fp, 
	   "###   Give password here.  Leave '%s' for password unchanged.\n", 
	   PASSWORD_MASK);
  fprintf (fp, "###   Empty password is acceptable.\n");
  fprintf (fp, "%s_password %s\n", "user1", PASSWORD_MASK);
  fprintf (fp, "%s_password %s\n", "user2", PASSWORD_MASK);
  fprintf (fp, "%s_password %s\n", "user3", PASSWORD_MASK);
  fprintf (fp, "%s_password %s\n", "user4", PASSWORD_MASK);
  fprintf (fp, "\n");
  
  return (0);
}


int 
get_user_access_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  int user_id;
  u_int64_t val;
  
  for (user_id = 1; user_id <= 4; user_id++)
    {
      obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_user_access_rs));
      
      status = ipmi_kcs_get_user_access (fi_get_sms_io_base (), 
					 get_lan_channel_number (), 
					 user_id, 
					 obj_data_rs);
      
      if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
	{
	  char err_msg[IPMI_ERR_STR_MAX_LEN];
	  ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	  fprintf (stderr, 
		       "error: ipmi_kcs_get_user_access: %d: %s\n", 
		   IPMI_COMP_CODE(obj_data_rs), err_msg);
	  status = -1;
	}
      
      fprintf (fp, "###   user%d access \n", user_id);
      
      fprintf (fp, "# %s %d\n", "user_id", user_id);
      fprintf (fp, "###   values: Reserved = 0, Callback = 1, User = 2, Operator = 3, \n"
	           "###           Administrator = 4, OEM proprietary = 5, No access = 15\n");
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_user_access_rs, 
		    "user_privilege_level_limit", 
		    &val);
      fprintf (fp, 
	       "user%d.%s %u\n", user_id, 
	       "user_privilege_level_limit", 
	       (u_int8_t) val);
      
      fprintf (fp, "###    values: Disable = 0, Enable = 1\n");
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_user_access_rs, 
		    "user_flags.enable_ipmi_msgs", 
		    &val);
      fprintf (fp, 
	       "user%d.%s %u\n", user_id, 
	       "user_flags.enable_ipmi_msgs", 
	       (u_int8_t) val);
      
      fprintf (fp, "###    values: Disable = 0, Enable = 1\n");
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_user_access_rs, 
		    "user_flags.enable_link_auth", 
		    &val);
      fprintf (fp, 
	       "user%d.%s %u\n", user_id, 
	       "user_flags.enable_link_auth", 
	       (u_int8_t) val);
      
      fprintf (fp, "###    values: Disable = 0, Enable = 1\n");
      fiid_obj_get (obj_data_rs, 
		    tmpl_get_user_access_rs, 
		    "user_flags.restrict_to_callback", 
		    &val);
      fprintf (fp, 
	       "user%d.%s %u\n", user_id, 
	       "user_flags.restrict_to_callback", 
	       (u_int8_t) val);
      
      fprintf (fp, "\n");
    }
  
  
  return (0);
}


int 
get_channel_access_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_channel_access_rs));
  
  fprintf (fp, "###   channel access: Non-volatile\n");
  status = ipmi_kcs_get_channel_access (fi_get_sms_io_base (), 
					get_lan_channel_number (), 
					IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE, 
					obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_kcs_get_channel_access: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_kcs_get_channel_access");
      return (-1);
    }
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"ipmi_messaging_access_mode", 
		&val);
  fprintf (fp, "###   values: Disabled = 0, Pre-boot = 1, Always available = 2, Shared = 3\n");
  fprintf (fp, 
	   "non_volatile.%s %d\n\n", 
	   "ipmi_messaging_access_mode", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"user_level_authentication", 
		&val);
  fprintf (fp, "###   values: Enable = 0, Disable = 1\n");
  fprintf (fp, 
	   "non_volatile.%s %d\n\n", 
	   "user_level_authentication", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"per_message_authentication", 
		&val);
  fprintf (fp, "###   values: Enable = 0, Disable = 1\n");
  fprintf (fp, 
	   "non_volatile.%s %d\n\n", 
	   "per_message_authentication", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"pef_alerting", 
		&val);
  fprintf (fp, "###   values: Enable = 0, Disable = 1\n");
  fprintf (fp, 
	   "non_volatile.%s %d\n\n", 
	   "pef_alerting", 
	   (int) val);
  
  fprintf (fp, "###   values: Reserved = 0, Callback = 1, User = 2, Operator = 3, \n"
	   "###           Administrator = 4, OEM proprietary = 5\n");
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"channel_privilege_level_limit", 
		&val);
  fprintf (fp, 
	   "non_volatile.%s %u\n\n", 
	   "channel_privilege_level_limit", 
	   (u_int8_t) val);
  
  fprintf (fp, "###   channel access: Volatile\n");
  status = ipmi_kcs_get_channel_access (fi_get_sms_io_base (), 
					get_lan_channel_number (), 
					IPMI_CHANNEL_ACCESS_GET_VOLATILE, 
					obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_kcs_get_channel_access: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_kcs_get_channel_access");
      return (-1);
    }
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"ipmi_messaging_access_mode", 
		&val);
  fprintf (fp, "###   values: Disabled = 0, Pre-boot = 1, Always available = 2, Shared = 3\n");
  fprintf (fp, 
	   "volatile.%s %d\n\n", 
	   "ipmi_messaging_access_mode", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"user_level_authentication", 
		&val);
  fprintf (fp, "###   values: Enable = 0, Disable = 1\n");
  fprintf (fp, 
	   "volatile.%s %d\n\n", 
	   "user_level_authentication", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"per_message_authentication", 
		&val);
  fprintf (fp, "###   values: Enable = 0, Disable = 1\n");
  fprintf (fp, 
	   "volatile.%s %d\n\n", 
	   "per_message_authentication", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"pef_alerting", 
		&val);
  fprintf (fp, "###   values: Enable = 0, Disable = 1\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "volatile.pef_alerting", 
	   (int) val);
  
  fprintf (fp, "###   values: Reserved = 0, Callback = 1, User = 2, Operator = 3, \n"
	   "###           Administrator = 4, OEM proprietary = 5\n");
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"channel_privilege_level_limit", 
		&val);
  fprintf (fp, 
	   "volatile.%s %u\n\n", 
	   "channel_privilege_level_limit", 
	   (u_int8_t) val);
  
  return (0);
}


int 
get_serial_connmode_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_connmode_rs));
  
  status = ipmi_get_serial_connmode (fi_get_sms_io_base (), 
				     get_serial_channel_number (), 
				     IPMI_SERIAL_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_get_serial_connmode: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_get_serial_connmode");
      return (-1);
    }
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"basic_mode_enable", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "basic_mode", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"ppp_mode_enable", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "ppp_mode", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"terminal_mode_enable", 
		&val);
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "terminal_mode", 
	   (int) val);
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"direct", 
		&val);
  fprintf (fp, "###   values: direct connect mode = 1, modem connect mode = 0\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "direct_mode", 
	   (int) val);
  
  
  return (0);
}


int 
get_serial_page_blackout_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_pageblackout_rs));
  
  status = ipmi_get_serial_page_blackout (fi_get_sms_io_base (), 
					  get_serial_channel_number (), 
					  IPMI_SERIAL_CONF_GET_PARAMETER, 
					  SET_SELECTOR, 
					  BLOCK_SELECTOR, 
					  obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_get_serial_page_blackout: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_get_serial_page_blackout");
      return (-1);
    }
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_pageblackout_rs, 
		"page_blackout_interval", 
		&val);
  fprintf (fp, 
	   "###   Page Blackout Interval in minutes.\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "page_blackout_interval", 
	   (int) val);
  
  
  return (0);
}


int 
get_serial_retry_time_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_retry_rs));
  
  status = ipmi_get_serial_retry_time (fi_get_sms_io_base (), 
				       get_serial_channel_number (), 
				       IPMI_SERIAL_CONF_GET_PARAMETER, 
				       SET_SELECTOR, 
				       BLOCK_SELECTOR, 
				       obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_get_serial_retry_time: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_get_serial_retry_time");
      return (-1);
    }
  
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_retry_rs, 
		"retry_time", 
		&val);
  fprintf (fp, 
	   "###   Number of seconds between call (`busy signal') retries.\n");
  fprintf (fp, 
	   "%s %d\n\n", 
	   "retry_time", 
	   (int) val);
  
  
  return (0);
}


int 
get_serial_comm_bits_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_commbits_rs));
  
  status = ipmi_get_serial_comm_bits (fi_get_sms_io_base (), 
				      get_serial_channel_number (), 
				      IPMI_SERIAL_CONF_GET_PARAMETER, 
				      SET_SELECTOR, 
				      BLOCK_SELECTOR, 
				      obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: tmpl_get_serial_conf_param_commbits_rs: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_get_serial_comm_bits");
      return (-1);
    }
  
  
  fprintf (fp, "###   values: enable = 1, disable = 0\n");
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_commbits_rs, 
		"dtr_hangup", 
		&val);
  fprintf (fp, 
	   "%s %d\n\n", 
	   "dtr_hangup", 
	   (int) val);
  
  
  fprintf (fp, "###   values: No flowcontrol = 0, RTS/CTS flow control (a.k.a. hardware handshake) = 1, \n"
	   "###   XON/XOFF flow control = 2\n");
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_commbits_rs, 
		"flow_control", 
		&val);
  fprintf (fp, 
	   "%s %d\n\n", 
	   "flow_control", 
	   (int) val);
  
  
  fprintf (fp, "###   values: 9600 bps = 6, 19.2 kbps = 7, 38.4 kbps = 8, 57.6 kbps = 9, 115.2 kbps = 10\n");
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_commbits_rs, 
		"bit_rate", 
		&val);
  fprintf (fp, 
	   "%s %d\n\n", 
	   "bit_rate", 
	   (int) val);
  
  
  return (0);
}

int 
get_chassis_status_checkout (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_cmd_get_chassis_status_rs));
  
  status = ipmi_get_chassis_status (fi_get_sms_io_base (), 
				    obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_get_chassis_status: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_get_chassis_status");
      return (-1);
    }
  
  fiid_obj_get (obj_data_rs, 
		tmpl_cmd_get_chassis_status_rs, 
		"power_state.power_restore_policy", 
		&val);
  fprintf (fp, "power_restore_policy %d\n", (int) val);
  
  return (0);
}

int 
get_power_restore_policy_checkout (FILE *fp)
{
  fprintf (fp, 
	   "###   Values: \n");
  fprintf (fp, 
	   "###   Chassis always stays powered off after AC/mains is applied, \n"
	   "###   power pushbutton or command required to power on system = 0\n");
  fprintf (fp, 
	   "###   After AC/mains is applied or returns, power is restored to the \n"
	   "###   state that was in effect when AC/mains was removed or lost = 1\n");
  fprintf (fp, 
	   "###   Chassis always powers up after AC/mains is applied or returns = 2\n");
  fprintf (fp, 
	   "###   No change in setting = 3\n");
  
  return (get_chassis_status_checkout (fp));
}

int 
bmc_config_checkout (char *filename)
{
  FILE *fp;
  
  if (filename == NULL || *filename == '\0')
    fp = stdout;
  else 
    if ((fp = fopen (filename, "w")) == NULL)
      return (-1);
  
  fprintf (fp, "## This file is auto-generated by 'bmc-config --checkout --filename=THIS-FILE'\n");
  fprintf (fp, "## You are allowed to edit this configuration file.\n\n");
  
  fprintf (fp, "#############################################################################\n");
  fprintf (fp, "## CAUTION:                                                                ##\n");
  fprintf (fp, "##  - Any lines starting with '#' or empty lines are ignored.              ##\n");
  fprintf (fp, "##  - Do not delete or comment any of the existing configuration options.  ##\n");
  fprintf (fp, "##  - Do not change the sequence of configuration options                  ##\n");
  fprintf (fp, "#############################################################################\n");
  
  if (kcs_bmc_lan_get_arp_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_get_gratuitous_arp_interval_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_get_auth_type_enables_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }

  if (kcs_lan_get_ip_addr_source_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_get_ip_addr_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_get_gw1_ip_addr_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_get_gw2_ip_addr_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_get_subnet_mask_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }

  if (kcs_lan_get_mac_addr_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_get_gw1_mac_addr_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }

  if (kcs_lan_get_gw2_mac_addr_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_user_name_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_user_password_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_user_access_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_channel_access_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_serial_connmode_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_serial_page_blackout_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_serial_retry_time_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_serial_comm_bits_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (get_power_restore_policy_checkout (fp) == -1)
    {
      if (fp != stdout)
	fclose (fp);
      return (-1);
    }
  
  if (fp != stdout)
    fclose (fp);
  
  return 0;
}
