#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "freeipmi.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# ifndef HAVE_STRCHR
static char*
strchr (const char* s, int c)
{
  while (*s != '\0')
    if (*s == (char)c) return s;
    else s++;
  return NULL;
}
# endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "fi-utils.h"
#include "bmc-conf-key-utils.h"

const char *bmc_config_key_list[] = 
  {
    "bmc_generated_gratuitous_arps_flag", 
    "bmc_generated_arp_responses_flag", 
    "gratuitous_arp_interval", 
    "callback_level.max_privilege_auth_type.none", 
    "callback_level.max_privilege_auth_type.md2", 
    "callback_level.max_privilege_auth_type.md5", 
    "callback_level.max_privilege_auth_type.straight_password", 
    "callback_level.max_privilege_auth_type.oem_proprietary", 
    "user_level.max_privilege_auth_type.none", 
    "user_level.max_privilege_auth_type.md2", 
    "user_level.max_privilege_auth_type.md5", 
    "user_level.max_privilege_auth_type.straight_password", 
    "user_level.max_privilege_auth_type.oem_proprietary", 
    "operator_level.max_privilege_auth_type.none", 
    "operator_level.max_privilege_auth_type.md2", 
    "operator_level.max_privilege_auth_type.md5", 
    "operator_level.max_privilege_auth_type.straight_password", 
    "operator_level.max_privilege_auth_type.oem_proprietary", 
    "admin_level.max_privilege_auth_type.none", 
    "admin_level.max_privilege_auth_type.md2", 
    "admin_level.max_privilege_auth_type.md5", 
    "admin_level.max_privilege_auth_type.straight_password", 
    "admin_level.max_privilege_auth_type.oem_proprietary", 
    "oem_level.max_privilege_auth_type.none", 
    "oem_level.max_privilege_auth_type.md2", 
    "oem_level.max_privilege_auth_type.md5", 
    "oem_level.max_privilege_auth_type.straight_password", 
    "oem_level.max_privilege_auth_type.oem_proprietary", 
    "ip_addr_source", 
    "ip_addr", 
    "gw1_ip_addr", 
    "gw2_ip_addr", 
    "subnet_mask", 
    "mac_addr", 
    "gw1_mac_addr", 
    "gw2_mac_addr", 
    "user2_name", 
    "user3_name", 
    "user4_name", 
    "user1_password", 
    "user2_password", 
    "user3_password", 
    "user4_password", 
    "user1.user_privilege_level_limit", 
    "user1.user_flags.enable_ipmi_msgs", 
    "user1.user_flags.enable_link_auth", 
    "user1.user_flags.restrict_to_callback", 
    "user2.user_privilege_level_limit", 
    "user2.user_flags.enable_ipmi_msgs", 
    "user2.user_flags.enable_link_auth", 
    "user2.user_flags.restrict_to_callback", 
    "user3.user_privilege_level_limit", 
    "user3.user_flags.enable_ipmi_msgs", 
    "user3.user_flags.enable_link_auth", 
    "user3.user_flags.restrict_to_callback", 
    "user4.user_privilege_level_limit", 
    "user4.user_flags.enable_ipmi_msgs", 
    "user4.user_flags.enable_link_auth", 
    "user4.user_flags.restrict_to_callback", 
    "non_volatile.ipmi_messaging_access_mode", 
    "non_volatile.user_level_authentication", 
    "non_volatile.per_message_authentication", 
    "non_volatile.pef_alerting", 
    "non_volatile.channel_privilege_level_limit", 
    "volatile.ipmi_messaging_access_mode", 
    "volatile.user_level_authentication", 
    "volatile.per_message_authentication", 
    "volatile.pef_alerting", 
    "volatile.channel_privilege_level_limit", 
    "basic_mode", 
    "ppp_mode", 
    "terminal_mode", 
    "direct_mode", 
    "page_blackout_interval", 
    "retry_time", 
    "dtr_hangup", 
    "flow_control", 
    "bit_rate", 
    "power_restore_policy", 
    NULL
  };

int 
bmc_config_get_key_value (char *line, char **key, char **value)
{
  int length = 0;
  char *delim_char_ptr = NULL;
  
  if (!line)
    return (-1);
  
  length = strlen (line);
  
  delim_char_ptr = strchr (line,  ' ');
  if (!delim_char_ptr)
    return (-1);
  
  *key = strndup (line, (delim_char_ptr - line));
  *value = strdup (stripwhite (delim_char_ptr));
  
  return 0;
}

int 
bmc_config_validate_key (char *key)
{
  int i;
  for (i = 0; bmc_config_key_list[i]; i++)
    {
      if (strcmp (bmc_config_key_list[i], key) == 0)
	return true;
    }
  
  return false;
}

int 
bmc_config_validate_value (char *key, char *value)
{
  /* check ip validity */
  if (strcmp (key, "ip_addr") == 0)
    {
      if (is_valid_ip (value))
	return true;
      
      fprintf (stderr, 
	       "error: Invalid IP address [%s]\n", 
	       value);
      return false;
    }
  
  if (strcmp (key, "gw1_ip_addr") == 0)
    {
      if (is_valid_ip (value))
	return true;
      
      fprintf (stderr, 
	       "error: Invalid IP address [%s]\n", 
	       value);
      return false;
    }
  
  if (strcmp (key, "gw2_ip_addr") == 0)
    {
      if (is_valid_ip (value))
	return true;
      
      fprintf (stderr, 
	       "error: Invalid IP address [%s]\n", 
	       value);
      return false;
    }
  
  if (strcmp (key, "subnet_mask") == 0)
    {
      if (is_valid_ip (value))
	return true;
      
      fprintf (stderr, 
	       "error: Invalid subnet mask [%s]\n", 
	       value);
      return false;
    }
  
  /* check mac address */
  if (strcmp (key, "mac_addr") == 0)
    {
      if (is_valid_mac_address (value))
	return true;
      
      fprintf (stderr, 
	       "error: Invalid MAC address [%s]\n", 
	       value);
      return false;
    }
  
  if (strcmp (key, "gw1_mac_addr") == 0)
    {
      if (is_valid_mac_address (value))
	return true;
      
      fprintf (stderr, 
	       "error: Invalid MAC address [%s]\n", 
	       value);
      return false;
    }
  
  if (strcmp (key, "gw2_mac_addr") == 0)
    {
      if (is_valid_mac_address (value))
	return true;
      
      fprintf (stderr, 
	       "error: Invalid MAC address [%s]\n", 
	       value);
      return false;
    }
  
  return true;
}

