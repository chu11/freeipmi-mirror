#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <freeipmi.h>
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

#include <stdlib.h>

#include "fish.h"
#include "fi-utils.h"
#include "ipmi-wrapper.h"
#include "bmc-conf-checkout.h"
#include "bmc-conf-commit.h"

int 
kcs_bmc_lan_set_arp_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  char *line;
  char *value_string;
  
  u_int8_t bmc_generated_gratuitous_arps_flag;
  u_int8_t bmc_generated_arp_responses_flag;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  
  bmc_generated_gratuitous_arps_flag = atoi (value_string);
  free (line);
  free (value_string);
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  bmc_generated_arp_responses_flag = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_lan_set_arp (fi_get_sms_io_base (), 
			     get_lan_channel_number (), 
			     bmc_generated_gratuitous_arps_flag, 
			     bmc_generated_arp_responses_flag, 
			     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_arp: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_arp");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_gratuitous_arp_interval_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  char *line;
  char *value_string;
  u_int8_t gratuitous_arp_interval;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  gratuitous_arp_interval = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_lan_set_gratuitous_arp_interval (fi_get_sms_io_base (), 
						 get_lan_channel_number (), 
						 gratuitous_arp_interval, 
						 obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_gratuitous_arp_interval: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_gratuitous_arp_interval");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_auth_type_enables_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  int n;
  int i;
  int j;
  
  u_int8_t max_privilege_auth_type_callback_level = 0;
  u_int8_t max_privilege_auth_type_user_level = 0;
  u_int8_t max_privilege_auth_type_operator_level = 0;
  u_int8_t max_privilege_auth_type_admin_level = 0;
  u_int8_t max_privilege_auth_type_oem_level = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  for (j = 0; j < 5; j++)
    {
      for (i = 0; i < 6; i++)
	{
	  if (i == 3)
	    continue;
	  
	  line = fi_getline (fp);
/* 	  printf ("%s:%d: __DEBUG__ [%s]\n",  */
/* 		  __FILE__, __LINE__,  */
/* 		  line); */
	  
	  if (line == NULL || *line == '\0')
	    return (-1);
	  if ((value_string = fi_get_value (line)) == NULL)
	    {
	      free (line);
	      return (-1);
	    }
	  n = atoi (value_string);
	  free (line);
	  free (value_string);
	  
	  switch (j)
	    {
	    case 0:
	      if (n == 1)
		max_privilege_auth_type_callback_level = BIT_SET (max_privilege_auth_type_callback_level, i);
	      break;
	    case 1:
	      if (n == 1)
		max_privilege_auth_type_user_level = BIT_SET (max_privilege_auth_type_user_level, i);
	      break;
	    case 2:
	      if (n == 1)
		max_privilege_auth_type_operator_level = BIT_SET (max_privilege_auth_type_operator_level, i);
	      break;
	    case 3:
	      if (n == 1)
		max_privilege_auth_type_admin_level = BIT_SET (max_privilege_auth_type_admin_level, i);
	      break;
	    case 4:
	      if (n == 1)
		max_privilege_auth_type_oem_level = BIT_SET (max_privilege_auth_type_oem_level, i);
	      break;
	    }
	}
    }
  
/*   for (i = 0; i < 6; i++) */
/*     { */
/*       if (i == 3) */
/* 	continue; */
      
/*       line = fi_getline (fp); */
/*       if (line == NULL || *line == '\0') */
/* 	return (-1); */
/*       if ((value_string = fi_get_value (line)) == NULL) */
/* 	{ */
/* 	  free (line); */
/* 	  return (-1); */
/* 	} */
/*       n = atoi (value_string); */
/*       free (line); */
/*       free (value_string); */
      
/*       if (n == 1) */
/* 	max_privilege_auth_type_user_level = BIT_SET (max_privilege_auth_type_user_level, i); */
/*     } */
  
/*   for (i = 0; i < 6; i++) */
/*     { */
/*       if (i == 3) */
/* 	continue; */
      
/*       line = fi_getline (fp); */
/*       if (line == NULL || *line == '\0') */
/* 	return (-1); */
/*       if ((value_string = fi_get_value (line)) == NULL) */
/* 	{ */
/* 	  free (line); */
/* 	  return (-1); */
/* 	} */
/*       n = atoi (value_string); */
/*       free (line); */
/*       free (value_string); */
      
/*       if (n == 1) */
/* 	max_privilege_auth_type_operator_level = BIT_SET (max_privilege_auth_type_operator_level, i); */
/*     } */
  
/*   for (i = 0; i < 6; i++) */
/*     { */
/*       if (i == 3) */
/* 	continue; */
      
/*       line = fi_getline (fp); */
/*       if (line == NULL || *line == '\0') */
/* 	return (-1); */
/*       if ((value_string = fi_get_value (line)) == NULL) */
/* 	{ */
/* 	  free (line); */
/* 	  return (-1); */
/* 	} */
/*       n = atoi (value_string); */
/*       free (line); */
/*       free (value_string); */
      
/*       if (n == 1) */
/* 	max_privilege_auth_type_admin_level = BIT_SET (max_privilege_auth_type_admin_level, i); */
/*     } */
  
/*   for (i = 0; i < 6; i++) */
/*     { */
/*       if (i == 3) */
/* 	continue; */
      
/*       line = fi_getline (fp); */
/*       if (line == NULL || *line == '\0') */
/* 	return (-1); */
/*       if ((value_string = fi_get_value (line)) == NULL) */
/* 	{ */
/* 	  free (line); */
/* 	  return (-1); */
/* 	} */
/*       n = atoi (value_string); */
/*       free (line); */
/*       free (value_string); */
      
/*       if (n == 1) */
/* 	max_privilege_auth_type_oem_level = BIT_SET (max_privilege_auth_type_oem_level, i); */
/*     } */
  
  
  status = ipmi_lan_set_auth_type_enables (fi_get_sms_io_base (), 
					   get_lan_channel_number (), 
					   max_privilege_auth_type_callback_level,
					   max_privilege_auth_type_user_level,
					   max_privilege_auth_type_operator_level,
					   max_privilege_auth_type_admin_level,
					   max_privilege_auth_type_oem_level,
					   obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_auth_type_enables: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_auth_type_enables");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_ip_addr_source_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  u_int8_t ip_addr_source;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  ip_addr_source = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_lan_set_ip_addr_source (fi_get_sms_io_base (), 
					get_lan_channel_number (), 
					ip_addr_source,
					obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_ip_addr_source: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_ip_addr_source");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_ip_addr_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t ip_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  free (line);
  
  /* check ip addr */
  if (!is_valid_ip (value_string))
    {
      fprintf (stderr, 
	       "error: ipmi_lan_set_ip_addr: Invalid IP address [%s]\n", 
	       value_string);
      free (value_string);
      return (-1);
    }
  
  sscanf (value_string, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  free (value_string);
  
  ip_address = bits_merge (ip_address, 0,  8,  b1);
  ip_address = bits_merge (ip_address, 8,  16, b2);
  ip_address = bits_merge (ip_address, 16, 24, b3);
  ip_address = bits_merge (ip_address, 24, 32, b4);
  
  status = ipmi_lan_set_ip_addr (fi_get_sms_io_base (), 
				 get_lan_channel_number (), 
				 ip_address, 
				 obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_ip_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_ip_addr");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_gw1_ip_addr_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;

  char *line;
  char *value_string;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t ip_address = 0;
  
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  free (line);
  
  /* check ip addr */
  if (!is_valid_ip (value_string))
    {
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw1_ip_addr: Invalid IP address [%s]\n", 
	       value_string);
      free (value_string);
      return (-1);
    }
  
  sscanf (value_string, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  free (value_string);
  
  ip_address = bits_merge (ip_address, 0,  8,  b1);
  ip_address = bits_merge (ip_address, 8,  16, b2);
  ip_address = bits_merge (ip_address, 16, 24, b3);
  ip_address = bits_merge (ip_address, 24, 32, b4);
  
  status = ipmi_lan_set_gw1_ip_addr (fi_get_sms_io_base (), 
				     get_lan_channel_number (), 
				     ip_address,
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw1_ip_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_gw1_ip_addr");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_gw2_ip_addr_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;

  char *line;
  char *value_string;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t ip_address = 0;
  
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  free (line);
  
  /* check ip addr */
  if (!is_valid_ip (value_string))
    {
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw2_ip_addr: Invalid IP address [%s]\n", 
	       value_string);
      free (value_string);
      return (-1);
    }
  
  sscanf (value_string, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  free (value_string);
  
  ip_address = bits_merge (ip_address, 0,  8,  b1);
  ip_address = bits_merge (ip_address, 8,  16, b2);
  ip_address = bits_merge (ip_address, 16, 24, b3);
  ip_address = bits_merge (ip_address, 24, 32, b4);
  
  status = ipmi_lan_set_gw2_ip_addr (fi_get_sms_io_base (), 
				     get_lan_channel_number (), 
				     ip_address,
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw2_ip_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_gw2_ip_addr");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_subnet_mask_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  unsigned int b1, b2, b3, b4;
  u_int64_t subnet_mask = 0;
  
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  free (line);
  
  /* check subnet mask */
  if (!is_valid_ip (value_string)) /* it works :) */
    {
      fprintf (stderr, 
	       "error: ipmi_lan_set_subnet_mask: Invalid subnet mask [%s]\n", 
	       value_string);
      free (value_string);
      return (-1);
    }
  
  sscanf (value_string, "%u.%u.%u.%u", &b1, &b2, &b3, &b4);
  free (value_string);
  
  subnet_mask = bits_merge (subnet_mask, 0,  8,  b1);
  subnet_mask = bits_merge (subnet_mask, 8,  16, b2);
  subnet_mask = bits_merge (subnet_mask, 16, 24, b3);
  subnet_mask = bits_merge (subnet_mask, 24, 32, b4);
  
  status = ipmi_lan_set_subnet_mask (fi_get_sms_io_base (), 
				     get_lan_channel_number (), 
				     subnet_mask, 
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_subnet_mask: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_subnet_mask");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_mac_addr_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  unsigned int b1, b2, b3, b4, b5, b6;
  u_int64_t mac_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  free (line);
  
  /* check mac address */
  if (!is_valid_mac_address (value_string))
    {
      fprintf (stderr, 
	       "error: ipmi_lan_set_mac_addr: Invalid MAC address [%s]\n", 
	       value_string);
      free (value_string);
      return (-1);
    }
  
  sscanf (value_string, "%02X:%02X:%02X:%02X:%02X:%02X", &b1, &b2, &b3, &b4, &b5, &b6);
  free (value_string);
  
  mac_address = bits_merge (mac_address, 0,  8,  b1);
  mac_address = bits_merge (mac_address, 8,  16, b2);
  mac_address = bits_merge (mac_address, 16, 24, b3);
  mac_address = bits_merge (mac_address, 24, 32, b4);
  mac_address = bits_merge (mac_address, 32, 40, b5);
  mac_address = bits_merge (mac_address, 40, 48, b6);
  
  status = ipmi_lan_set_mac_addr (fi_get_sms_io_base (), 
				  get_lan_channel_number (), 
				  mac_address, 
				  obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_mac_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_mac_addr");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_gw1_mac_addr_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  unsigned int b1, b2, b3, b4, b5, b6;
  u_int64_t mac_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  free (line);
  
  /* check mac address */
  if (!is_valid_mac_address (value_string))
    {
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw1_mac_addr: Invalid MAC address [%s]\n", 
	       value_string);
      free (value_string);
      return (-1);
    }
  
  sscanf (value_string, "%02X:%02X:%02X:%02X:%02X:%02X", &b1, &b2, &b3, &b4, &b5, &b6);
  free (value_string);
  
  mac_address = bits_merge (mac_address, 0,  8,  b1);
  mac_address = bits_merge (mac_address, 8,  16, b2);
  mac_address = bits_merge (mac_address, 16, 24, b3);
  mac_address = bits_merge (mac_address, 24, 32, b4);
  mac_address = bits_merge (mac_address, 32, 40, b5);
  mac_address = bits_merge (mac_address, 40, 48, b6);
  
  status = ipmi_lan_set_gw1_mac_addr (fi_get_sms_io_base (), 
				      get_lan_channel_number (), 
				      mac_address, 
				      obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw1_mac_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_gw1_mac_addr");
      return (-1);
    }
  
  
  return (0);
}


int 
kcs_lan_set_gw2_mac_addr_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  unsigned int b1, b2, b3, b4, b5, b6;
  u_int64_t mac_address = 0;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_lan_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  free (line);
  
  /* check mac address */
  if (!is_valid_mac_address (value_string))
    {
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw2_mac_addr: Invalid MAC address [%s]\n", 
	       value_string);
      free (value_string);
      return (-1);
    }
  
  sscanf (value_string, "%02X:%02X:%02X:%02X:%02X:%02X", &b1, &b2, &b3, &b4, &b5, &b6);
  free (value_string);
  
  mac_address = bits_merge (mac_address, 0,  8,  b1);
  mac_address = bits_merge (mac_address, 8,  16, b2);
  mac_address = bits_merge (mac_address, 16, 24, b3);
  mac_address = bits_merge (mac_address, 24, 32, b4);
  mac_address = bits_merge (mac_address, 32, 40, b5);
  mac_address = bits_merge (mac_address, 40, 48, b6);
  
  status = ipmi_lan_set_gw2_mac_addr (fi_get_sms_io_base (), 
				      get_lan_channel_number (), 
				      mac_address, 
				      obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_lan_set_gw2_mac_addr: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_lan_set_gw2_mac_addr");
      return (-1);
    }
  
  
  return (0);
}


int 
set_user_name_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  int user_id;
  
  char *line;
  char *value_string;
  
  char *user2name = NULL;
  char *user3name = NULL;
  char *user4name = NULL;
  
  char *username;
  
  char username_data[IPMI_USER_NAME_MAX_LENGTH];
  
  for (user_id = 2; user_id <= 4; user_id++)
    {
      obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_name_rs));
      
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      username = value_string;
      free (line);
      
      switch (user_id)
	{
	case 2:
	  user2name = username;
	  break;
	case 3:
	  user3name = username;
	  if (strcmp (username, user2name) == 0)
	    fprintf (stderr, "warning: duplicate username found for user3_name\n");
	  break;
	case 4:
	  user4name = username;
	  if ((strcmp (username, user2name) == 0) || 
	      (strcmp (username, user3name) == 0))
	    fprintf (stderr, "warning: duplicate username found for user4_name\n");
	  break;
	}
      
      if (strlen (username) > IPMI_USER_NAME_MAX_LENGTH)
	fprintf (stderr, 
		 "warning: first %d characters are taken from username [%s]\n", 
		 IPMI_USER_NAME_MAX_LENGTH, username);
      
      memset (username_data, 0, IPMI_USER_NAME_MAX_LENGTH);
      if (strlen (username) < IPMI_USER_NAME_MAX_LENGTH)
	memcpy (username_data, username, strlen (username));
      else 
	memcpy (username_data, username, IPMI_USER_NAME_MAX_LENGTH);
      
      status = ipmi_kcs_set_user_name (fi_get_sms_io_base (), 
				       user_id, 
				       username_data, 
				       obj_data_rs);
      
      if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
	{
	  char err_msg[IPMI_ERR_STR_MAX_LEN];
	  ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	  fprintf (stderr, 
		   "error: ipmi_kcs_set_user_name: %d: %s\n", 
		   IPMI_COMP_CODE(obj_data_rs), err_msg);
	}
      
      if (status != 0)
	perror ("ipmi_kcs_set_user_name");
    }
  
  if (user2name) 
    free (user2name);
  if (user3name) 
    free (user3name);
  if (user4name) 
    free (user4name);
  
  return (0);
}

int 
set_user_password_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  int user_id;
  
  char *line;
  char *value_string;
  
  char *password;
  char password_data[IPMI_USER_PASSWORD_MAX_LENGTH];
  
  for (user_id = 1; user_id <= 4; user_id++)
    {
      obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_password_rs));
      
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      password = value_string;
      free (line);
      
      if (strcmp (password, PASSWORD_MASK) == 0)
	{
	  free (value_string);
	  continue;
	}
      
      if (strlen (password) > IPMI_USER_PASSWORD_MAX_LENGTH)
	fprintf (stderr, 
		 "warning: first %d characters are taken from user%d_password\n", 
		 IPMI_USER_NAME_MAX_LENGTH, user_id);
      
      memset (password_data, 0, IPMI_USER_PASSWORD_MAX_LENGTH);
      if (strlen (password) < IPMI_USER_PASSWORD_MAX_LENGTH)
	memcpy (password_data, password, strlen (password));
      else 
	memcpy (password_data, password, IPMI_USER_PASSWORD_MAX_LENGTH);
      
      status = ipmi_kcs_set_user_password (fi_get_sms_io_base (), 
					   user_id, 
					   IPMI_PASSWORD_OPERATION_SET_PASSWORD, 
					   password_data, 
					   obj_data_rs);
      
      free (value_string);
      
      if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
	{
	  char err_msg[IPMI_ERR_STR_MAX_LEN];
	  ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	  fprintf (stderr, 
		   "error: ipmi_kcs_set_user_password: %d: %s\n", 
		   IPMI_COMP_CODE(obj_data_rs), err_msg);
	}
      
      if (status != 0)
	perror ("ipmi_kcs_set_user_password");
    }
  
  
  return (0);
}

int 
set_user_access_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  int user_id;
  
  char *line;
  char *value_string;
  
  u_int8_t restrict_to_callback;
  u_int8_t enable_link_auth;
  u_int8_t enable_ipmi_msgs;
  u_int8_t user_privilege_level_limit;
  u_int8_t user_session_number_limit = 0;
  
  for (user_id = 1; user_id <= 4; user_id++)
    {
      obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_user_access_rs));
      
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      user_privilege_level_limit = atoi (value_string);
      free (line);
      free (value_string);
      
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      enable_ipmi_msgs = atoi (value_string);
      free (line);
      free (value_string);
      
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      enable_link_auth = atoi (value_string);
      free (line);
      free (value_string);
      
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      restrict_to_callback = atoi (value_string);
      free (line);
      free (value_string);
      
      status = ipmi_kcs_set_user_access (fi_get_sms_io_base (), 
					 get_lan_channel_number (), 
					 user_id, 
					 restrict_to_callback,
					 enable_link_auth,
					 enable_ipmi_msgs,
					 user_privilege_level_limit,
					 user_session_number_limit,
					 obj_data_rs);
      
      if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
	{
	  char err_msg[IPMI_ERR_STR_MAX_LEN];
	  ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	  fprintf (stderr, 
		   "error: ipmi_kcs_set_user_access: %d: %s\n", 
		   IPMI_COMP_CODE(obj_data_rs), err_msg);
	  status = -1;
	}
    }
  
  
  return (0);
}


int 
set_channel_access_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  u_int8_t ipmi_messaging_access_mode;
  u_int8_t user_level_authentication;
  u_int8_t per_message_authentication;
  u_int8_t pef_alerting;
  u_int8_t channel_privilege_level_limit;
  
  int i;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_channel_access_rq));
  
  for (i = 0; i < 2; i++)
    {
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      ipmi_messaging_access_mode = atoi (value_string);
      free (line);
      free (value_string);
  
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      user_level_authentication = atoi (value_string);
      free (line);
      free (value_string);
  
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      per_message_authentication = atoi (value_string);
      free (line);
      free (value_string);
  
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      pef_alerting = atoi (value_string);
      free (line);
      free (value_string);
  
      line = fi_getline (fp);
      if (line == NULL || *line == '\0')
	return (-1);
      if ((value_string = fi_get_value (line)) == NULL)
	{
	  free (line);
	  return (-1);
	}
      channel_privilege_level_limit = atoi (value_string);
      free (line);
      free (value_string);
  
      status = ipmi_kcs_set_channel_access (fi_get_sms_io_base (), 
					    get_lan_channel_number (), 
					    ipmi_messaging_access_mode, 
					    user_level_authentication, 
					    per_message_authentication, 
					    pef_alerting, 
					    (i ? IPMI_CHANNEL_ACCESS_SET_VOLATILE : 
					     IPMI_CHANNEL_ACCESS_SET_NON_VOLATILE), 
					    channel_privilege_level_limit, 
					    (i ? IPMI_PRIV_LEVEL_LIMIT_SET_VOLATILE : 
					     IPMI_PRIV_LEVEL_LIMIT_SET_NON_VOLATILE), 
					    obj_data_rs);
  
      if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
	{
	  char err_msg[IPMI_ERR_STR_MAX_LEN];
	  ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
	  fprintf (stderr, 
		   "error: ipmi_kcs_set_channel_access: %d: %s\n", 
		   IPMI_COMP_CODE(obj_data_rs), err_msg);
	  status = -1;
	}
    }
  
  return (0);
}


int 
set_serial_connmode_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  u_int8_t basic_mode_enable;
  u_int8_t ppp_mode_enable;
  u_int8_t terminal_mode_enable;
  u_int8_t direct;
  
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  basic_mode_enable = atoi (value_string);
  free (line);
  free (value_string);
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  ppp_mode_enable = atoi (value_string);
  free (line);
  free (value_string);
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  terminal_mode_enable = atoi (value_string);
  free (line);
  free (value_string);
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  direct = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_set_serial_connmode (fi_get_sms_io_base (), 
				     get_serial_channel_number (), 
				     basic_mode_enable,
				     ppp_mode_enable,
				     terminal_mode_enable,
				     direct,
				     obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_set_serial_connmode: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_set_serial_connmode");
      return (-1);
    }
  
  
  return (0);
}


int 
set_serial_page_blackout_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  u_int8_t page_blackout_interval;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  page_blackout_interval = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_set_serial_page_blackout_interval (fi_get_sms_io_base (), 
						   get_serial_channel_number (), 
						   page_blackout_interval,
						   obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_set_serial_page_blackout: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_set_serial_page_blackout");
      return (-1);
    }
  
  
  return (0);
}


int 
set_serial_retry_time_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  u_int8_t retry_time;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  retry_time = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_set_serial_retry_time (fi_get_sms_io_base (), 
				       get_serial_channel_number (), 
				       retry_time,
				       obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_set_serial_retry_time: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_set_serial_retry_time");
      return (-1);
    }
  
  
  return (0);
}


int 
set_serial_comm_bits_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  u_int8_t dtr_hangup;
  u_int8_t flow_control;
  u_int8_t bit_rate;
    
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_serial_conf_param_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  dtr_hangup = atoi (value_string);
  free (line);
  free (value_string);
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  flow_control = atoi (value_string);
  free (line);
  free (value_string);
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  bit_rate = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_set_serial_comm_bits (fi_get_sms_io_base (), 
				      get_serial_channel_number (), 
				      dtr_hangup,
				      flow_control,
				      bit_rate,
				      obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_set_serial_comm_bits: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_set_serial_comm_bits");
      return (-1);
    }
  
  
  return (0);
}


int 
set_power_restore_policy_commit (FILE *fp)
{
  u_int8_t status;
  fiid_obj_t obj_data_rs;
  
  char *line;
  char *value_string;
  
  u_int8_t power_restore_policy;
  
  obj_data_rs = alloca (fiid_obj_len_bytes (tmpl_set_power_restore_policy_rs));
  
  line = fi_getline (fp);
  if (line == NULL || *line == '\0')
    return (-1);
  if ((value_string = fi_get_value (line)) == NULL)
    {
      free (line);
      return (-1);
    }
  power_restore_policy = atoi (value_string);
  free (line);
  free (value_string);
  
  status = ipmi_set_power_restore_policy (fi_get_sms_io_base (), 
					  power_restore_policy,
					  obj_data_rs);
  
  if (IPMI_COMP_CODE (obj_data_rs) != IPMI_COMMAND_SUCCESS)
    {
      char err_msg[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_data_rs, err_msg, IPMI_ERR_STR_MAX_LEN);
      fprintf (stderr, 
	       "error: ipmi_set_power_restore_policy: %d: %s\n", 
	       IPMI_COMP_CODE(obj_data_rs), err_msg);
      return (-1);
    }
  
  if (status != 0)
    {
      perror ("ipmi_set_power_restore_policy");
      return (-1);
    }
  
  
  return (0);
}


int 
bmc_config_commit (char *filename)
{
  FILE *fp;
  
  if (filename == NULL)
    return (-1);
  
  if ((fp = fopen (filename, "r")) == NULL)
    return (-1);
  

  if (kcs_bmc_lan_set_arp_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_set_gratuitous_arp_interval_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_set_auth_type_enables_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }

  if (kcs_lan_set_ip_addr_source_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_set_ip_addr_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_set_gw1_ip_addr_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_set_gw2_ip_addr_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_set_subnet_mask_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }

  if (kcs_lan_set_mac_addr_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (kcs_lan_set_gw1_mac_addr_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }

  if (kcs_lan_set_gw2_mac_addr_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_user_name_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_user_password_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_user_access_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_channel_access_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_serial_connmode_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_serial_page_blackout_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_serial_retry_time_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_serial_comm_bits_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  if (set_power_restore_policy_commit (fp) == -1)
    {
      fclose (fp);
      return (-1);
    }
  
  fclose (fp);
  
  return 0;
}
