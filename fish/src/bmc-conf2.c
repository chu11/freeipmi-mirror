/* 
   bmc-conf2.c: BMC Config functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "freeipmi.h"
#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#define SET_SELECTOR      0x0
#define BLOCK_SELECTOR    0x0

#include <stdlib.h>

#include "fish.h"
#include "fi-utils.h"
#include "ipmi-wrapper.h"
#include "bmc-conf2.h"

/* struct bmc_user */
/* { */
/*   u_int8_t userid; */
/*   u_int8_t username[IPMI_USER_NAME_MAX_LENGTH]; */
/*   u_int8_t enable_user; */
/*   u_int8_t password[IPMI_USER_PASSWORD_MAX_LENGTH]; */
/*   u_int8_t lan_enable_ipmi_msgs; */
/*   u_int8_t lan_enable_link_auth; */
/*   u_int8_t lan_enable_restrict_to_callback; */
/*   u_int8_t lan_privilege_limit; */
/*   u_int8_t lan_session_limit; */
/*   u_int8_t serial_enable_ipmi_msgs; */
/*   u_int8_t serial_enable_link_auth; */
/*   u_int8_t serial_enable_restrict_to_callback; */
/*   u_int8_t serial_privilege_limit; */
/*   u_int8_t serial_session_limit; */
/* }; */

/* struct bmc_lan_conf */
/* { */
/*   u_int8_t ip_addr_source; */
/*   char     ip_addr[16]; */
/*   char     mac_addr[18]; */
/*   char     subnet_mask[16]; */
/*   char     default_gw_ip_addr[16]; */
/*   char     default_gw_mac_addr[18]; */
/*   char     backup_gw_ip_addr[16]; */
/*   char     backup_gw_mac_addr[18]; */
/* }; */

u_int8_t 
set_bmc_user_access (u_int8_t userid, 
		     u_int8_t channel_number, 
		     u_int8_t enable_ipmi_msgs, 
		     u_int8_t enable_link_auth, 
		     u_int8_t enable_restrict_to_callback, 
		     u_int8_t privilege_limit, 
		     u_int8_t session_limit)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_access_rs));
  status = ipmi_kcs_set_user_access (channel_number, 
				     userid, 
				     enable_restrict_to_callback, 
				     enable_link_auth, 
				     enable_ipmi_msgs, 
				     privilege_limit, 
				     session_limit, 
				     obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_channel_access (u_int8_t channel_number, 
			u_int8_t set_option, 
			u_int8_t access_mode, 
			u_int8_t enable_user_level_auth, 
			u_int8_t enable_per_message_auth, 
			u_int8_t enable_pef_alerting, 
			u_int8_t channel_privilege_limit)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_channel_access_rq));
  status = ipmi_kcs_set_channel_access (channel_number, 
					access_mode, 
					enable_user_level_auth, 
					enable_per_message_auth, 
					enable_pef_alerting, 
					(set_option ? IPMI_CHANNEL_ACCESS_SET_VOLATILE : 
					 IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE), 
					channel_privilege_limit, 
					(set_option ? IPMI_PRIV_LEVEL_LIMIT_SET_VOLATILE : 
					 IPMI_PRIV_LEVEL_LIMIT_SET_NON_VOLATILE), 
					obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_username (u_int8_t userid, u_int8_t *username)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  if (userid == 1)
    return 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_name_rs));
  status = ipmi_kcs_set_user_name (userid, 
				   username, 
				   obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_enable_user (u_int8_t userid, int user_status)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t password[IPMI_USER_PASSWORD_MAX_LENGTH];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_password_rs));
  memset (password, 0, IPMI_USER_PASSWORD_MAX_LENGTH);
  status = ipmi_kcs_set_user_password (userid, 
				       (user_status ? IPMI_PASSWORD_OPERATION_ENABLE_USER :
					IPMI_PASSWORD_OPERATION_DISABLE_USER), 
				       password, 
				       obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_user_password (u_int8_t userid, u_int8_t *password)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_password_rs));
  status = ipmi_kcs_set_user_password (userid, 
				       IPMI_PASSWORD_OPERATION_SET_PASSWORD, 
				       password, 
				       obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_user_lan_channel_access (u_int8_t userid, 
				 u_int8_t lan_enable_ipmi_msgs, 
				 u_int8_t lan_enable_link_auth, 
				 u_int8_t lan_enable_restrict_to_callback, 
				 u_int8_t lan_privilege_limit, 
				 u_int8_t lan_session_limit)
{
  return set_bmc_user_access (userid, 
			      get_lan_channel_number (), 
			      lan_enable_ipmi_msgs, 
			      lan_enable_link_auth, 
			      lan_enable_restrict_to_callback, 
			      lan_privilege_limit, 
			      lan_session_limit);
}

u_int8_t 
set_bmc_user_serial_channel_access (u_int8_t userid, 
				    u_int8_t serial_enable_ipmi_msgs, 
				    u_int8_t serial_enable_link_auth, 
				    u_int8_t serial_enable_restrict_to_callback, 
				    u_int8_t serial_privilege_limit, 
				    u_int8_t serial_session_limit)
{
  return set_bmc_user_access (userid, 
			      get_serial_channel_number (), 
			      serial_enable_ipmi_msgs, 
			      serial_enable_link_auth, 
			      serial_enable_restrict_to_callback, 
			      serial_privilege_limit, 
			      serial_session_limit);
}

u_int8_t 
set_bmc_lan_channel_volatile_access (u_int8_t access_mode, 
				     u_int8_t enable_user_level_auth, 
				     u_int8_t enable_per_message_auth, 
				     u_int8_t enable_pef_alerting, 
				     u_int8_t channel_privilege_limit)
{
  return set_bmc_channel_access (get_lan_channel_number (), 
				 1, 
				 access_mode, 
				 (enable_user_level_auth ? 0 : 1), 
				 (enable_per_message_auth ? 0 : 1), 
				 (enable_pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

u_int8_t 
set_bmc_lan_channel_non_volatile_access (u_int8_t access_mode, 
					 u_int8_t enable_user_level_auth, 
					 u_int8_t enable_per_message_auth, 
					 u_int8_t enable_pef_alerting, 
					 u_int8_t channel_privilege_limit)
{
  return set_bmc_channel_access (get_lan_channel_number (), 
				 0, 
				 access_mode, 
				 (enable_user_level_auth ? 0 : 1), 
				 (enable_per_message_auth ? 0 : 1), 
				 (enable_pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

u_int8_t 
set_bmc_lan_conf_ip_addr_source (u_int8_t ip_addr_source)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  status = ipmi_lan_set_ip_addr_source (get_lan_channel_number (), 
					ip_addr_source, 
					obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_ip_addr (char *ip_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t ip_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  sscanf (ip_addr, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  ip_address = bits_merge (ip_address, 0,  8,  b1);
  ip_address = bits_merge (ip_address, 8,  16, b2);
  ip_address = bits_merge (ip_address, 16, 24, b3);
  ip_address = bits_merge (ip_address, 24, 32, b4);
  
  status = ipmi_lan_set_ip_addr (get_lan_channel_number (), 
				 ip_address, 
				 obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_mac_addr (char *mac_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  unsigned int b1, b2, b3, b4, b5, b6;
  u_int64_t mac_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  sscanf (mac_addr, "%02X:%02X:%02X:%02X:%02X:%02X", &b1, &b2, &b3, &b4, &b5, &b6);
  mac_address = bits_merge (mac_address, 0,  8,  b1);
  mac_address = bits_merge (mac_address, 8,  16, b2);
  mac_address = bits_merge (mac_address, 16, 24, b3);
  mac_address = bits_merge (mac_address, 24, 32, b4);
  mac_address = bits_merge (mac_address, 32, 40, b5);
  mac_address = bits_merge (mac_address, 40, 48, b6);
  
  status = ipmi_lan_set_mac_addr (get_lan_channel_number (), 
				  mac_address, 
				  obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_subnet_mask (char *subnet_mask)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t subnetmask = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  sscanf (subnet_mask, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  subnetmask = bits_merge (subnetmask, 0,  8,  b1);
  subnetmask = bits_merge (subnetmask, 8,  16, b2);
  subnetmask = bits_merge (subnetmask, 16, 24, b3);
  subnetmask = bits_merge (subnetmask, 24, 32, b4);
  
  status = ipmi_lan_set_subnet_mask (get_lan_channel_number (), 
				     subnetmask, 
				     obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_default_gw_ip_addr (char *default_gw_ip_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t ip_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  sscanf (default_gw_ip_addr, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  ip_address = bits_merge (ip_address, 0,  8,  b1);
  ip_address = bits_merge (ip_address, 8,  16, b2);
  ip_address = bits_merge (ip_address, 16, 24, b3);
  ip_address = bits_merge (ip_address, 24, 32, b4);
  
  status = ipmi_lan_set_gw1_ip_addr (get_lan_channel_number (), 
				     ip_address,
				     obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_default_gw_mac_addr (char *default_gw_mac_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  unsigned int b1, b2, b3, b4, b5, b6;
  u_int64_t mac_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  sscanf (default_gw_mac_addr, "%02X:%02X:%02X:%02X:%02X:%02X", 
	  &b1, &b2, &b3, &b4, &b5, &b6);
  mac_address = bits_merge (mac_address, 0,  8,  b1);
  mac_address = bits_merge (mac_address, 8,  16, b2);
  mac_address = bits_merge (mac_address, 16, 24, b3);
  mac_address = bits_merge (mac_address, 24, 32, b4);
  mac_address = bits_merge (mac_address, 32, 40, b5);
  mac_address = bits_merge (mac_address, 40, 48, b6);
  
  status = ipmi_lan_set_gw1_mac_addr (get_lan_channel_number (), 
				      mac_address, 
				      obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_backup_gw_ip_addr (char *backup_gw_ip_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t ip_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  sscanf (backup_gw_ip_addr, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  ip_address = bits_merge (ip_address, 0,  8,  b1);
  ip_address = bits_merge (ip_address, 8,  16, b2);
  ip_address = bits_merge (ip_address, 16, 24, b3);
  ip_address = bits_merge (ip_address, 24, 32, b4);
  
  status = ipmi_lan_set_gw2_ip_addr (get_lan_channel_number (), 
				     ip_address, 
				     obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_backup_gw_mac_addr (char *backup_gw_mac_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  unsigned int b1, b2, b3, b4, b5, b6;
  u_int64_t mac_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  sscanf (backup_gw_mac_addr, "%02X:%02X:%02X:%02X:%02X:%02X", 
	  &b1, &b2, &b3, &b4, &b5, &b6);
  mac_address = bits_merge (mac_address, 0,  8,  b1);
  mac_address = bits_merge (mac_address, 8,  16, b2);
  mac_address = bits_merge (mac_address, 16, 24, b3);
  mac_address = bits_merge (mac_address, 24, 32, b4);
  mac_address = bits_merge (mac_address, 32, 40, b5);
  mac_address = bits_merge (mac_address, 40, 48, b6);
  
  status = ipmi_lan_set_gw2_mac_addr (get_lan_channel_number (), 
				      mac_address, 
				      obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_auth_type_enables (struct bmc_auth_level *bmc_auth_level)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  u_int8_t auth_type_callback_level = 0;
  u_int8_t auth_type_user_level = 0;
  u_int8_t auth_type_operator_level = 0;
  u_int8_t auth_type_admin_level = 0;
  u_int8_t auth_type_oem_level = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  if (bmc_auth_level->callback.type_none)
    auth_type_callback_level = BIT_SET (auth_type_callback_level, 0);
  if (bmc_auth_level->callback.type_md2)
    auth_type_callback_level = BIT_SET (auth_type_callback_level, 1);
  if (bmc_auth_level->callback.type_md5)
    auth_type_callback_level = BIT_SET (auth_type_callback_level, 2);
  if (bmc_auth_level->callback.type_straight_password)
    auth_type_callback_level = BIT_SET (auth_type_callback_level, 4);
  if (bmc_auth_level->callback.type_oem_proprietary)
    auth_type_callback_level = BIT_SET (auth_type_callback_level, 5);
  
  if (bmc_auth_level->user.type_none)
    auth_type_user_level = BIT_SET (auth_type_user_level, 0);
  if (bmc_auth_level->user.type_md2)
    auth_type_user_level = BIT_SET (auth_type_user_level, 1);
  if (bmc_auth_level->user.type_md5)
    auth_type_user_level = BIT_SET (auth_type_user_level, 2);
  if (bmc_auth_level->user.type_straight_password)
    auth_type_user_level = BIT_SET (auth_type_user_level, 4);
  if (bmc_auth_level->user.type_oem_proprietary)
    auth_type_user_level = BIT_SET (auth_type_user_level, 5);
  
  if (bmc_auth_level->operator.type_none)
    auth_type_operator_level = BIT_SET (auth_type_operator_level, 0);
  if (bmc_auth_level->operator.type_md2)
    auth_type_operator_level = BIT_SET (auth_type_operator_level, 1);
  if (bmc_auth_level->operator.type_md5)
    auth_type_operator_level = BIT_SET (auth_type_operator_level, 2);
  if (bmc_auth_level->operator.type_straight_password)
    auth_type_operator_level = BIT_SET (auth_type_operator_level, 4);
  if (bmc_auth_level->operator.type_oem_proprietary)
    auth_type_operator_level = BIT_SET (auth_type_operator_level, 5);
  
  if (bmc_auth_level->admin.type_none)
    auth_type_admin_level = BIT_SET (auth_type_admin_level, 0);
  if (bmc_auth_level->admin.type_md2)
    auth_type_admin_level = BIT_SET (auth_type_admin_level, 1);
  if (bmc_auth_level->admin.type_md5)
    auth_type_admin_level = BIT_SET (auth_type_admin_level, 2);
  if (bmc_auth_level->admin.type_straight_password)
    auth_type_admin_level = BIT_SET (auth_type_admin_level, 4);
  if (bmc_auth_level->admin.type_oem_proprietary)
    auth_type_admin_level = BIT_SET (auth_type_admin_level, 5);
  
  if (bmc_auth_level->oem.type_none)
    auth_type_oem_level = BIT_SET (auth_type_oem_level, 0);
  if (bmc_auth_level->oem.type_md2)
    auth_type_oem_level = BIT_SET (auth_type_oem_level, 1);
  if (bmc_auth_level->oem.type_md5)
    auth_type_oem_level = BIT_SET (auth_type_oem_level, 2);
  if (bmc_auth_level->oem.type_straight_password)
    auth_type_oem_level = BIT_SET (auth_type_oem_level, 4);
  if (bmc_auth_level->oem.type_oem_proprietary)
    auth_type_oem_level = BIT_SET (auth_type_oem_level, 5);
  
  status = ipmi_lan_set_auth_type_enables (get_lan_channel_number (), 
					   auth_type_callback_level,
					   auth_type_user_level,
					   auth_type_operator_level,
					   auth_type_admin_level,
					   auth_type_oem_level,
					   obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_arp_control (u_int8_t enable_gratuitous_arps, 
			      u_int8_t enable_arp_response)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  status = ipmi_lan_set_arp (get_lan_channel_number (), 
			     enable_gratuitous_arps, 
			     enable_arp_response, 
			     obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_lan_conf_gratuitous_arp (u_int8_t gratuitous_arp_interval)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  status = ipmi_lan_set_gratuitous_arp_interval (get_lan_channel_number (), 
						 gratuitous_arp_interval, 
						 obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_serial_channel_volatile_access (u_int8_t access_mode, 
					u_int8_t enable_user_level_auth, 
					u_int8_t enable_per_message_auth, 
					u_int8_t enable_pef_alerting, 
					u_int8_t channel_privilege_limit)
{
  return set_bmc_channel_access (get_serial_channel_number (), 
				 1, 
				 access_mode, 
				 (enable_user_level_auth ? 0 : 1), 
				 (enable_per_message_auth ? 0 : 1), 
				 (enable_pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

u_int8_t 
set_bmc_serial_channel_non_volatile_access (u_int8_t access_mode, 
					    u_int8_t enable_user_level_auth, 
					    u_int8_t enable_per_message_auth, 
					    u_int8_t enable_pef_alerting, 
					    u_int8_t channel_privilege_limit)
{
  return set_bmc_channel_access (get_serial_channel_number (), 
				 0, 
				 access_mode, 
				 (enable_user_level_auth ? 0 : 1), 
				 (enable_per_message_auth ? 0 : 1), 
				 (enable_pef_alerting ? 0 : 1), 
				 channel_privilege_limit);
}

u_int8_t 
set_bmc_serial_conf_conn_mode (u_int8_t enable_basic_mode, 
			       u_int8_t enable_ppp_mode, 
			       u_int8_t enable_terminal_mode, 
			       u_int8_t connect_mode)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  status = ipmi_set_serial_connmode (get_serial_channel_number (), 
				     enable_basic_mode,
				     enable_ppp_mode,
				     enable_terminal_mode,
				     connect_mode,
				     obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_serial_conf_page_blackout_interval (u_int8_t page_blackout_interval)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  status = ipmi_set_serial_page_blackout_interval (get_serial_channel_number (), 
						   page_blackout_interval,
						   obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_serial_conf_call_retry_time (u_int8_t call_retry_time)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  status = ipmi_set_serial_retry_time (get_serial_channel_number (), 
				       call_retry_time,
				       obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_serial_conf_ipmi_msg_comm_settings (u_int8_t dtr_hangup, 
					    u_int8_t flow_control, 
					    u_int8_t bit_rate)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  status = ipmi_set_serial_comm_bits (get_serial_channel_number (), 
				      dtr_hangup,
				      flow_control,
				      bit_rate,
				      obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

u_int8_t 
set_bmc_power_restore_policy (u_int8_t power_restore_policy)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_power_restore_policy_rs));
  status = ipmi_set_power_restore_policy (power_restore_policy,
					  obj_data_rs);
  
  if (status == 0)
    return IPMI_COMP_CODE (obj_data_rs);
  return status;
}

/* u_int8_t  */
/* set_bmc_user_conf (struct bmc_user *bmc_user) */
/* { */
/*   u_int8_t status; */
  
/*   if ((status = set_bmc_username (bmc_user->userid, bmc_user->username)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_enable_user (bmc_user->userid, bmc_user->enable_user)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_user_password (bmc_user->userid, bmc_user->password)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_user_lan_access (bmc_user->userid,  */
/* 					 bmc_user->lan_enable_ipmi_msgs,  */
/* 					 bmc_user->lan_enable_link_auth,  */
/* 					 bmc_user->lan_enable_restrict_to_callback,  */
/* 					 bmc_user->lan_privilege_limit,  */
/* 					 bmc_user->lan_session_limit)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_user_serial_access (bmc_user->userid,  */
/* 					    bmc_user->serial_enable_ipmi_msgs,  */
/* 					    bmc_user->serial_enable_link_auth,  */
/* 					    bmc_user->serial_enable_restrict_to_callback,  */
/* 					    bmc_user->serial_privilege_limit,  */
/* 					    bmc_user->serial_session_limit)) != 0) */
/*     return status; */
  
/*   return 0; */
/* } */

/* u_int8_t  */
/* set_bmc_lan_conf (struct bmc_lan_conf *bmc_lan_conf) */
/* { */
/*   u_int8_t status; */
  
/*   if ((status = set_bmc_lan_conf_ip_addr_source (bmc_lan_conf->ip_addr_source)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_lan_conf_ip_addr (bmc_lan_conf->ip_addr)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_lan_conf_mac_addr (bmc_lan_conf->mac_addr)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_lan_conf_subnet_mask (bmc_lan_conf->subnet_mask)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_lan_conf_default_gw_ip_addr (bmc_lan_conf->default_gw_ip_addr)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_lan_conf_default_gw_mac_addr (bmc_lan_conf->default_gw_mac_addr)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_lan_conf_backup_gw_ip_addr (bmc_lan_conf->backup_gw_ip_addr)) != 0) */
/*     return status; */
  
/*   if ((status = set_bmc_lan_conf_backup_gw_mac_addr (bmc_lan_conf->backup_gw_mac_addr)) != 0) */
/*     return status; */
  
/*   return 0; */
/* } */


u_int8_t 
get_bmc_user_access (u_int8_t userid, 
		     u_int8_t channel_number, 
		     u_int8_t *enable_ipmi_msgs, 
		     u_int8_t *enable_link_auth, 
		     u_int8_t *enable_restrict_to_callback, 
		     u_int8_t *privilege_limit, 
		     u_int8_t *session_limit)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_user_access_rs));
  
  status = ipmi_kcs_get_user_access (channel_number, 
				     userid, 
				     obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_user_access_rs, 
		"user_privilege_level_limit", 
		&val);
  *privilege_limit = (u_int8_t) val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_user_access_rs, 
		"user_flags.enable_ipmi_msgs", 
		&val);
  *enable_ipmi_msgs = (u_int8_t) val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_user_access_rs, 
		"user_flags.enable_link_auth", 
		&val);
  *enable_link_auth = (u_int8_t) val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_user_access_rs, 
		"user_flags.restrict_to_callback", 
		&val);
  *enable_restrict_to_callback = (u_int8_t) val;
  
  *session_limit = 0;
  
  return IPMI_COMP_CODE (obj_data_rs);
}

u_int8_t 
get_bmc_channel_access (u_int8_t channel_number, 
			u_int8_t access_type, 
			u_int8_t *access_mode, 
			u_int8_t *user_level_auth, 
			u_int8_t *per_message_auth, 
			u_int8_t *pef_alerting, 
			u_int8_t *privilege_limit)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_channel_access_rs));
  status = ipmi_kcs_get_channel_access (channel_number, 
					(access_type ? IPMI_CHANNEL_ACCESS_GET_VOLATILE :
					 IPMI_CHANNEL_ACCESS_GET_NON_VOLATILE), 
					obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"ipmi_messaging_access_mode", 
		&val);
  *access_mode = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"user_level_authentication", 
		&val);
  *user_level_auth = (val ? 0 : 1);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"per_message_authentication", 
		&val);
  *per_message_auth = (val ? 0 : 1);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"pef_alerting", 
		&val);
  *pef_alerting = (val ? 0 : 1);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_channel_access_rs, 
		"channel_privilege_level_limit", 
		&val);
  *privilege_limit = val;
  
  return (0);
}

u_int8_t 
get_bmc_chassis_status (u_int8_t *power_restore_policy)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_cmd_get_chassis_status_rs));
  status = ipmi_get_chassis_status (obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_cmd_get_chassis_status_rs, 
		"power_state.power_restore_policy", 
		&val);
  *power_restore_policy = val;
  
  return (0);
}

u_int8_t 
get_bmc_username (u_int8_t userid, u_int8_t *username)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  if (userid == 1)
    {
      strcpy (username, "Anonymous");
      return 0;
    }
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_user_name_rs));
  status = ipmi_kcs_get_user_name (userid, 
				   obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_user_name_rs, 
		     "user_name", 
		     username);
  
  return (0);
}

u_int8_t 
get_bmc_user_lan_channel_access (u_int8_t userid, 
				 u_int8_t *enable_ipmi_msgs, 
				 u_int8_t *enable_link_auth, 
				 u_int8_t *enable_restrict_to_callback, 
				 u_int8_t *privilege_limit, 
				 u_int8_t *session_limit)
{
  return get_bmc_user_access (userid, 
			      get_lan_channel_number (), 
			      enable_ipmi_msgs, 
			      enable_link_auth, 
			      enable_restrict_to_callback, 
			      privilege_limit, 
			      session_limit);
}

u_int8_t 
get_bmc_user_serial_channel_access (u_int8_t userid, 
				    u_int8_t *enable_ipmi_msgs, 
				    u_int8_t *enable_link_auth, 
				    u_int8_t *enable_restrict_to_callback, 
				    u_int8_t *privilege_limit, 
				    u_int8_t *session_limit)
{
  return get_bmc_user_access (userid, 
			      get_serial_channel_number (), 
			      enable_ipmi_msgs, 
			      enable_link_auth, 
			      enable_restrict_to_callback, 
			      privilege_limit, 
			      session_limit);
}

u_int8_t 
get_bmc_lan_channel_volatile_access (u_int8_t *access_mode, 
				     u_int8_t *user_level_auth, 
				     u_int8_t *per_message_auth, 
				     u_int8_t *pef_alerting, 
				     u_int8_t *privilege_limit)
{
  return get_bmc_channel_access (get_lan_channel_number (), 
				 1, 
				 access_mode, 
				 user_level_auth, 
				 per_message_auth, 
				 pef_alerting, 
				 privilege_limit);
}

u_int8_t 
get_bmc_lan_channel_non_volatile_access (u_int8_t *access_mode, 
					 u_int8_t *user_level_auth, 
					 u_int8_t *per_message_auth, 
					 u_int8_t *pef_alerting, 
					 u_int8_t *privilege_limit)
{
  return get_bmc_channel_access (get_lan_channel_number (), 
				 0, 
				 access_mode, 
				 user_level_auth, 
				 per_message_auth, 
				 pef_alerting, 
				 privilege_limit);
}

u_int8_t 
get_bmc_lan_conf_ip_addr_source (u_int8_t *ip_addr_source)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_ip_addr_source_rs));
  status = ipmi_lan_get_ip_addr_source (get_lan_channel_number (), 
					IPMI_LAN_CONF_GET_PARAMETER, 
					SET_SELECTOR, 
					BLOCK_SELECTOR, 
					obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_ip_addr_source_rs, 
		"ip_addr_source", 
		&val);
  *ip_addr_source = val;
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_ip_addr (char *ip_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t ip_addr_bytes[4];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_ip_addr_rs));
  status = ipmi_lan_get_ip_addr (get_lan_channel_number (), 
				 IPMI_LAN_CONF_GET_PARAMETER, 
				 SET_SELECTOR, 
				 BLOCK_SELECTOR, 
				 obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_lan_conf_param_ip_addr_rs, 
		     "ip_addr", 
		     ip_addr_bytes);
  sprintf (ip_addr, 
	   "%u.%u.%u.%u", 
	   ip_addr_bytes[0], 
	   ip_addr_bytes[1], 
	   ip_addr_bytes[2], 
	   ip_addr_bytes[3]);
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_mac_addr (char *mac_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t mac_addr_bytes[6];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_mac_addr_rs));
  status = ipmi_lan_get_mac_addr (get_lan_channel_number (), 
				  IPMI_LAN_CONF_GET_PARAMETER, 
				  SET_SELECTOR, 
				  BLOCK_SELECTOR, 
				  obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_lan_conf_param_mac_addr_rs, 
		     "mac_addr", 
		     mac_addr_bytes);
  sprintf (mac_addr, 
	   "%02X:%02X:%02X:%02X:%02X:%02X", 
	   mac_addr_bytes[0], 
	   mac_addr_bytes[1], 
	   mac_addr_bytes[2], 
	   mac_addr_bytes[3], 
	   mac_addr_bytes[4], 
	   mac_addr_bytes[5]);
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_subnet_mask (char *subnet_mask)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t subnet_mask_bytes[4];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_subnet_mask_rs));
  status = ipmi_lan_get_subnet_mask (get_lan_channel_number (), 
				     IPMI_LAN_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_lan_conf_param_subnet_mask_rs, 
		     "subnet_mask", 
		     subnet_mask_bytes);
  sprintf (subnet_mask, 
	   "%u.%u.%u.%u", 
	   subnet_mask_bytes[0], 
	   subnet_mask_bytes[1], 
	   subnet_mask_bytes[2], 
	   subnet_mask_bytes[3]);
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_default_gw_ip_addr (char *default_gw_ip_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t ip_addr_bytes[4];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_gw_ip_addr_rs));
  status = ipmi_lan_get_gw1_ip_addr (get_lan_channel_number (), 
				     IPMI_LAN_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_lan_conf_param_gw_ip_addr_rs, 
		     "ip_addr", 
		     ip_addr_bytes);
  sprintf (default_gw_ip_addr, 
	   "%u.%u.%u.%u", 
	   ip_addr_bytes[0], 
	   ip_addr_bytes[1], 
	   ip_addr_bytes[2], 
	   ip_addr_bytes[3]);
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_default_gw_mac_addr (char *default_gw_mac_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t mac_addr_bytes[6];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_mac_addr_rs));
  status = ipmi_lan_get_gw1_mac_addr (get_lan_channel_number (), 
				      IPMI_LAN_CONF_GET_PARAMETER, 
				      SET_SELECTOR, 
				      BLOCK_SELECTOR, 
				      obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_lan_conf_param_mac_addr_rs, 
		     "mac_addr", 
		     mac_addr_bytes);
  sprintf (default_gw_mac_addr, 
	   "%02X:%02X:%02X:%02X:%02X:%02X", 
	   mac_addr_bytes[0], 
	   mac_addr_bytes[1], 
	   mac_addr_bytes[2], 
	   mac_addr_bytes[3], 
	   mac_addr_bytes[4], 
	   mac_addr_bytes[5]);
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_backup_gw_ip_addr (char *backup_gw_ip_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t ip_addr_bytes[4];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_gw_ip_addr_rs));
  status = ipmi_lan_get_gw2_ip_addr (get_lan_channel_number (), 
				     IPMI_LAN_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_lan_conf_param_gw_ip_addr_rs, 
		     "ip_addr", 
		     ip_addr_bytes);
  sprintf (backup_gw_ip_addr, 
	   "%u.%u.%u.%u", 
	   ip_addr_bytes[0], 
	   ip_addr_bytes[1], 
	   ip_addr_bytes[2], 
	   ip_addr_bytes[3]);
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_backup_gw_mac_addr (char *backup_gw_mac_addr)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int8_t mac_addr_bytes[6];
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_mac_addr_rs));
  status = ipmi_lan_get_gw2_mac_addr (get_lan_channel_number (), 
				      IPMI_LAN_CONF_GET_PARAMETER, 
				      SET_SELECTOR, 
				      BLOCK_SELECTOR, 
				      obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get_data (obj_data_rs, 
		     tmpl_get_lan_conf_param_mac_addr_rs, 
		     "mac_addr", 
		     mac_addr_bytes);
  sprintf (backup_gw_mac_addr, 
	   "%02X:%02X:%02X:%02X:%02X:%02X", 
	   mac_addr_bytes[0], 
	   mac_addr_bytes[1], 
	   mac_addr_bytes[2], 
	   mac_addr_bytes[3], 
	   mac_addr_bytes[4], 
	   mac_addr_bytes[5]);
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_auth_type_enables (struct bmc_auth_level *bmc_auth_level)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_auth_type_enables_rs));
  
  status = ipmi_lan_get_auth_type_enables (get_lan_channel_number (), 
					   IPMI_LAN_CONF_GET_PARAMETER, 
					   SET_SELECTOR, 
					   BLOCK_SELECTOR, 
					   obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.none", 
		&val);
  bmc_auth_level->callback.type_none = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.md2", 
		&val);
  bmc_auth_level->callback.type_md2 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.md5", 
		&val);
  bmc_auth_level->callback.type_md5 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.straight_password", 
		&val);
  bmc_auth_level->callback.type_straight_password = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_callback_level.oem_proprietary", 
		&val);
  bmc_auth_level->callback.type_oem_proprietary = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.none", 
		&val);
  bmc_auth_level->user.type_none = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.md2", 
		&val);
  bmc_auth_level->user.type_md2 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.md5", 
		&val);
  bmc_auth_level->user.type_md5 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.straight_password", 
		&val);
  bmc_auth_level->user.type_straight_password = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_user_level.oem_proprietary", 
		&val);
  bmc_auth_level->user.type_oem_proprietary = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.none", 
		&val);
  bmc_auth_level->operator.type_none = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.md2", 
		&val);
  bmc_auth_level->operator.type_md2 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.md5", 
		&val);
  bmc_auth_level->operator.type_md5 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.straight_password", 
		&val);
  bmc_auth_level->operator.type_straight_password = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_operator_level.oem_proprietary", 
		&val);
  bmc_auth_level->operator.type_oem_proprietary = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.none", 
		&val);
  bmc_auth_level->admin.type_none = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.md2", 
		&val);
  bmc_auth_level->admin.type_md2 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.md5", 
		&val);
  bmc_auth_level->admin.type_md5 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.straight_password", 
		&val);
  bmc_auth_level->admin.type_straight_password = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_admin_level.oem_proprietary", 
		&val);
  bmc_auth_level->admin.type_oem_proprietary = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.none", 
		&val);
  bmc_auth_level->oem.type_none = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.md2", 
		&val);
  bmc_auth_level->oem.type_md2 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.md5", 
		&val);
  bmc_auth_level->oem.type_md5 = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.straight_password", 
		&val);
  bmc_auth_level->oem.type_straight_password = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_auth_type_enables_rs, 
		"max_privilege_auth_type_oem_level.oem_proprietary", 
		&val);
  bmc_auth_level->oem.type_oem_proprietary = val;
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_arp_control (u_int8_t *enable_gratuitous_arps, 
			      u_int8_t *enable_arp_response)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_bmc_generated_arp_control_rs));
  
  status = ipmi_lan_get_arp (get_lan_channel_number (), 
			     IPMI_LAN_CONF_GET_PARAMETER, 
			     SET_SELECTOR, 
			     BLOCK_SELECTOR, 
			     obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_bmc_generated_arp_control_rs, 
		"bmc_generated_gratuitous_arps_flag", 
		&val);
  *enable_gratuitous_arps = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_bmc_generated_arp_control_rs, 
		"bmc_generated_arp_responses_flag", 
		&val);
  *enable_arp_response = val;
  
  return (0);
}

u_int8_t 
get_bmc_lan_conf_gratuitous_arp (u_int8_t *gratuitous_arp_interval)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_lan_conf_param_gratuitous_arp_interval_rs));
  status = ipmi_lan_get_gratuitous_arp_interval (get_lan_channel_number (), 
						 IPMI_LAN_CONF_GET_PARAMETER, 
						 SET_SELECTOR, 
						 BLOCK_SELECTOR, 
						 obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_lan_conf_param_gratuitous_arp_interval_rs, 
		"gratuitous_arp_interval", 
		&val);
  *gratuitous_arp_interval = val;
  
  return (0);
}

u_int8_t 
get_bmc_serial_channel_volatile_access (u_int8_t *access_mode, 
					u_int8_t *user_level_auth, 
					u_int8_t *per_message_auth, 
					u_int8_t *pef_alerting, 
					u_int8_t *privilege_limit)
{
  return get_bmc_channel_access (get_serial_channel_number (), 
				 1, 
				 access_mode, 
				 user_level_auth, 
				 per_message_auth, 
				 pef_alerting, 
				 privilege_limit);
}

u_int8_t 
get_bmc_serial_channel_non_volatile_access (u_int8_t *access_mode, 
					    u_int8_t *user_level_auth, 
					    u_int8_t *per_message_auth, 
					    u_int8_t *pef_alerting, 
					    u_int8_t *privilege_limit)
{
  return get_bmc_channel_access (get_serial_channel_number (), 
				 0, 
				 access_mode, 
				 user_level_auth, 
				 per_message_auth, 
				 pef_alerting, 
				 privilege_limit);
}

u_int8_t 
get_bmc_serial_conf_conn_mode (u_int8_t *enable_basic_mode, 
			       u_int8_t *enable_ppp_mode, 
			       u_int8_t *enable_terminal_mode, 
			       u_int8_t *connect_mode)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_connmode_rs));
  
  status = ipmi_get_serial_connmode (get_serial_channel_number (), 
				     IPMI_SERIAL_CONF_GET_PARAMETER, 
				     SET_SELECTOR, 
				     BLOCK_SELECTOR, 
				     obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"basic_mode_enable", 
		&val);
  *enable_basic_mode = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"ppp_mode_enable", 
		&val);
  *enable_ppp_mode = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"terminal_mode_enable", 
		&val);
  *enable_terminal_mode = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_connmode_rs, 
		"direct", 
		&val);
  *connect_mode = val;
  
  return (0);
}

u_int8_t 
get_bmc_serial_conf_page_blackout_interval (u_int8_t *page_blackout_interval)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_pageblackout_rs));
  status = ipmi_get_serial_page_blackout (get_serial_channel_number (), 
					  IPMI_SERIAL_CONF_GET_PARAMETER, 
					  SET_SELECTOR, 
					  BLOCK_SELECTOR, 
					  obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_pageblackout_rs, 
		"page_blackout_interval", 
		&val);
  *page_blackout_interval = val;
  
  return (0);
}

u_int8_t 
get_bmc_serial_conf_call_retry_time (u_int8_t *call_retry_time)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_retry_rs));
  status = ipmi_get_serial_retry_time (get_serial_channel_number (), 
				       IPMI_SERIAL_CONF_GET_PARAMETER, 
				       SET_SELECTOR, 
				       BLOCK_SELECTOR, 
				       obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_retry_rs, 
		"retry_time", 
		&val);
  *call_retry_time = val;
  
  return (0);
}

u_int8_t 
get_bmc_serial_conf_ipmi_msg_comm_settings (u_int8_t *dtr_hangup, 
					    u_int8_t *flow_control, 
					    u_int8_t *bit_rate)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  u_int64_t val;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_get_serial_conf_param_commbits_rs));
  
  status = ipmi_get_serial_comm_bits (get_serial_channel_number (), 
				      IPMI_SERIAL_CONF_GET_PARAMETER, 
				      SET_SELECTOR, 
				      BLOCK_SELECTOR, 
				      obj_data_rs);
  
  if (status != 0)
    return status;
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    return IPMI_COMP_CODE (obj_data_rs);
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_commbits_rs, 
		"dtr_hangup", 
		&val);
  *dtr_hangup = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_commbits_rs, 
		"flow_control", 
		&val);
  *flow_control = val;
  
  fiid_obj_get (obj_data_rs, 
		tmpl_get_serial_conf_param_commbits_rs, 
		"bit_rate", 
		&val);
  *bit_rate = val;
  
  return (0);
}

u_int8_t 
get_bmc_power_restore_policy (u_int8_t *power_restore_policy)
{
  return get_bmc_chassis_status (power_restore_policy);
}

/***********************************************************/
u_int8_t 
check_bmc_user_password (u_int8_t userid, u_int8_t *password)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_password_rs));
  status = ipmi_kcs_set_user_password (userid, 
				       IPMI_PASSWORD_OPERATION_TEST_PASSWORD, 
				       password, 
				       obj_data_rs);
  
  if (status == 0)
    {
      if (IPMI_COMP_CODE (obj_data_rs) == IPMI_COMMAND_SUCCESS)
	return 1; /* true */
      
      if (IPMI_COMP_CODE (obj_data_rs) == IPMI_PASSWORD_OPERATION_TEST_FAILED)
	return 0; /* false */
    }
  return status;
}
