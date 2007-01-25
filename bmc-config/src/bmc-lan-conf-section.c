#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"


#define BMC_MAXIPADDRLEN 16

static int
ip_address_source_checkout (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  uint8_t source;
  int ret;

  ret = get_bmc_lan_conf_ip_address_source (args->dev,
					     &source);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (ip_address_source_string (source))))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
ip_address_source_commit (const struct bmc_config_arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  return set_bmc_lan_conf_ip_address_source (args->dev,
					     ip_address_source_number (kv->value));
}

static int
ip_address_source_diff (const struct bmc_config_arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  int ret;

  ret = get_bmc_lan_conf_ip_address_source (args->dev,
					     &get_val);
  if (ret != 0)
    return -1;

  passed_val = ip_address_source_number (kv->value);
  if (passed_val == get_val)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   ip_address_source_string (get_val));
    }
  return ret;
}

static int
ip_address_source_validate (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const char *value)
{
  return (ip_address_source_number (value) >= 0) ? 0 : 1;
}

static int
ip_address_checkout (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  int ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_ip_address (args->dev,
				     (char *)&ip);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
ip_address_commit (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  int ret;

  ret = set_bmc_lan_conf_ip_address (args->dev,
				     kv->value);

  return ret;
}

static int
ip_address_diff (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  int ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_ip_address (args->dev,
				     (char *)&ip);

  if (ret != 0)
    return -1;

  if (same (ip, kv->value))
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   ip);
    }
  return ret;
}

static int
ip_address_validate (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const char *value)
{
  struct in_addr a;
  return inet_aton (value, &a) ? 0 : 1;
}

static int
mac_address_checkout (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  int ret;
  char mac[25];

  ret = get_bmc_lan_conf_mac_address (args->dev,
				      (char *)&mac);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return -1;
    }
  return 0;
}

static int
mac_address_commit (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{

  return set_bmc_lan_conf_mac_address (args->dev,
				       kv->value);
}

static int
mac_address_diff (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  int ret;
  char mac[25];

  ret = get_bmc_lan_conf_mac_address (args->dev,
				      (char *)&mac);
  if (ret != 0)
    return -1;

  if (same (mac, kv->value))
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   mac);
    }
  return ret;
}

static int
mac_address_validate (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const char *value)
{
  unsigned int foo;
  return (sscanf (value, "%02x:%02x:%02x:%02x:%02x:%02x", &foo, &foo,
		  &foo, &foo, &foo, &foo) == 6) ? 0 : 1;
}

static int
subnet_mask_checkout (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  int ret;
  char mask[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_subnet_mask (args->dev,
				      (char *)&mask);
  
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mask)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
subnet_mask_commit (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  int ret;

  ret = set_bmc_lan_conf_subnet_mask (args->dev,
				      kv->value);

  return ret;
}

static int
subnet_mask_diff (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  int ret;
  char mask[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_subnet_mask (args->dev,
				      (char *)&mask);

  if (ret != 0)
    return -1;

  if (same (mask, kv->value))
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   mask);
    }
  return ret;
}

static int
subnet_mask_validate (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const char *value)
{
  struct in_addr a;
  /* TODO: checking valid netmask is not same as checking valid IP */
  return inet_aton (value, &a) ? 0 : 1;
}


static int
default_gateway_address_checkout (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  int ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_default_gateway_address (args->dev,
						  (char *)&ip);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
default_gateway_address_commit (const struct bmc_config_arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  int ret;

  ret = set_bmc_lan_conf_default_gateway_address (args->dev,
						  kv->value);

  return ret;
}

static int
default_gateway_address_diff (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  int ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_default_gateway_address (args->dev,
						  (char *)&ip);

  if (ret != 0)
    return -1;

  if (same (ip, kv->value)) 
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   ip);
    }
  return ret;
}

static int
default_gateway_address_validate (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const char *value)
{
  struct in_addr a;
  return inet_aton (value, &a) ? 0 : 1;
}

static int
default_gateway_mac_address_checkout (const struct bmc_config_arguments *args,
				      const struct section *sect,
				      struct keyvalue *kv)
{
  int ret;
  char mac[25];

  ret = get_bmc_lan_conf_default_gateway_mac_address (args->dev,
						      (char *)&mac);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return -1;
    }
  return 0;
}

static int
default_gateway_mac_address_commit (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{

  return set_bmc_lan_conf_default_gateway_mac_address (args->dev,
						       kv->value);
}

static int
default_gateway_mac_address_diff (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  int ret;
  char mac[25];

  ret = get_bmc_lan_conf_default_gateway_mac_address (args->dev,
						      (char *)&mac);
  if (ret != 0)
    return -1;

  return same (mac, kv->value) ? 0 : 1;
}

static int
default_gateway_mac_address_validate (const struct bmc_config_arguments *args,
				      const struct section *sect,
				      const char *value)
{
  unsigned int foo;
  return (sscanf (value, "%02x:%02x:%02x:%02x:%02x:%02x", &foo, &foo,
		  &foo, &foo, &foo, &foo) == 6) ? 0 : 1;
}

/* backup */


static int
backup_gateway_address_checkout (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  int ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_backup_gateway_address (args->dev,
						 (char *)&ip);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
backup_gateway_address_commit (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  int ret;

  ret = set_bmc_lan_conf_backup_gateway_address (args->dev,
						 kv->value);

  return ret;
}

static int
backup_gateway_address_diff (const struct bmc_config_arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  int ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  ret = get_bmc_lan_conf_backup_gateway_address (args->dev,
						 (char *)&ip);

  if (ret != 0)
    return -1;

  if (same (ip, kv->value))
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   ip);
    }
  return ret;
}

static int
backup_gateway_address_validate (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const char *value)
{
  struct in_addr a;
  return inet_aton (value, &a) ? 0 : 1;
}

static int
backup_gateway_mac_address_checkout (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  int ret;
  char mac[25];

  ret = get_bmc_lan_conf_backup_gateway_mac_address (args->dev,
						     (char *)&mac);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return -1;
    }
  return 0;
}

static int
backup_gateway_mac_address_commit (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{

  return set_bmc_lan_conf_backup_gateway_mac_address (args->dev,
						      kv->value);
}

static int
backup_gateway_mac_address_diff (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  int ret;
  char mac[25];

  ret = get_bmc_lan_conf_backup_gateway_mac_address (args->dev,
						     (char *)&mac);
  if (ret != 0)
    return -1;

  return same (mac, kv->value) ? 0 : 1;
}

static int
backup_gateway_mac_address_validate (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     const char *value)
{
  unsigned int foo;
  return (sscanf (value, "%02x:%02x:%02x:%02x:%02x:%02x", &foo, &foo,
		  &foo, &foo, &foo, &foo) == 6) ? 0 : 1;
}

static int
vlan_id_checkout (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  int ret;
  
  ret = get_bmc_lan_conf_vlan_id (args->dev,
				  &vlan_id,
				  &vlan_id_enable);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", vlan_id);
  return 0;
}

static int
vlan_id_commit (const struct bmc_config_arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  int ret;
  
  ret = get_bmc_lan_conf_vlan_id (args->dev,
				  &vlan_id,
				  &vlan_id_enable);
  if (ret != 0)
    return -1;

  vlan_id = atoi (kv->value);

  ret = set_bmc_lan_conf_vlan_id (args->dev,
				  vlan_id,
				  vlan_id_enable);

  return ret;
}

static int
vlan_id_diff (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  int ret;
  
  ret = get_bmc_lan_conf_vlan_id (args->dev,
				  &vlan_id,
				  &vlan_id_enable);
  if (ret != 0)
    return -1;

  if (vlan_id == atoi (kv->value))
    ret = 0;
  else 
    {
      char num[32];
      sprintf (num, "%d", vlan_id);
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
vlan_id_validate (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const char *value)
{
  char *endptr;
  uint32_t num = strtoul (value, &endptr, 0);

  if (*endptr)
    return -1;
  if (num == UINT_MAX || num == (uint32_t)(INT_MIN))
    return 1;
  return 0;
}

static int
vlan_id_enable_checkout (const struct bmc_config_arguments *args,
			 const struct section *sect,
			 struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  int ret;
  
  ret = get_bmc_lan_conf_vlan_id (args->dev,
				  &vlan_id,
				  &vlan_id_enable);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  if (vlan_id_enable)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }
  return 0;
}

static int
vlan_id_enable_commit (const struct bmc_config_arguments *args,
		       const struct section *sect,
		       const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  int ret;
  
  ret = get_bmc_lan_conf_vlan_id (args->dev,
				  &vlan_id,
				  &vlan_id_enable);
  if (ret != 0)
    return -1;

  vlan_id_enable = same (kv->value, "yes");

  ret = set_bmc_lan_conf_vlan_id (args->dev,
				  vlan_id,
				  vlan_id_enable);

  return ret;
}

static int
vlan_id_enable_diff (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  int ret;
  
  ret = get_bmc_lan_conf_vlan_id (args->dev,
				  &vlan_id,
				  &vlan_id_enable);
  if (ret != 0)
    return -1;

  if (vlan_id_enable == (same (kv->value, "yes")))
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   vlan_id_enable ? "Yes" : "No");
    }
  return ret;
}

static int
vlan_id_enable_validate (const struct bmc_config_arguments *args,
			 const struct section *sect,
			 const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

static int
vlan_priority_checkout (const struct bmc_config_arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  uint8_t priority;
  int ret;

  ret = get_bmc_lan_conf_vlan_priority (args->dev,
					&priority);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", priority);
  return 0;
}

static int
vlan_priority_commit (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  return set_bmc_lan_conf_vlan_priority (args->dev,
					 atoi (kv->value));
}

static int
vlan_priority_diff (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  uint8_t priority;
  int ret;

  ret = get_bmc_lan_conf_vlan_priority (args->dev,
					&priority);
  if (ret != 0)
    return -1;

  if (priority == atoi (kv->value))
    ret = 0;
  else 
    {
      char prio[32];
      ret = 1;
      sprintf (prio, "%d", priority);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   prio);
    }
  return ret;
}

static int
vlan_priority_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{
  char *endptr;
  uint32_t num = strtoul (value, &endptr, 0);

  if (*endptr)
    return -1;
  if (num < 0 || num > 255)
    return 1;
  return 0;
}

struct section *
bmc_lan_conf_section_get (struct bmc_config_arguments *args)
{
  struct section *lan_conf_section = NULL;
  if (!(lan_conf_section = (void *) calloc (1, sizeof (struct section))))
    {
      perror("calloc");
      return NULL;
    }
  if (!(lan_conf_section->section = strdup ("Lan_Conf")))
    {
      perror("strdup");
      return NULL;
    }
  
  add_keyvalue (lan_conf_section,
		"IP_Address_Source",
		"Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
                0,
		ip_address_source_checkout,
		ip_address_source_commit,
		ip_address_source_diff,
		ip_address_source_validate);

  add_keyvalue (lan_conf_section,
		"IP_Address",
		"Give valid IP address",
                0,
		ip_address_checkout,
		ip_address_commit,
		ip_address_diff,
		ip_address_validate);

  add_keyvalue (lan_conf_section,
		"MAC_Address",
		"Give valid MAC address",
                0,
		mac_address_checkout,
		mac_address_commit,
		mac_address_diff,
		mac_address_validate);

  add_keyvalue (lan_conf_section,
		"Subnet_Mask",
		"Give valid Subnet Mask",
                0,
		subnet_mask_checkout,
		subnet_mask_commit,
		subnet_mask_diff,
		subnet_mask_validate);

  add_keyvalue (lan_conf_section,
		"Default_Gateway_IP_Address",
		"Give valid IP address",
                0,
		default_gateway_address_checkout,
		default_gateway_address_commit,
		default_gateway_address_diff,
		default_gateway_address_validate);

  add_keyvalue (lan_conf_section,
		"Default_Gateway_MAC_Address",
		"Give valid MAC address",
                0,
		default_gateway_mac_address_checkout,
		default_gateway_mac_address_commit,
		default_gateway_mac_address_diff,
		default_gateway_mac_address_validate);

  add_keyvalue (lan_conf_section,
		"Backup_Gateway_IP_Address",
		"Give valid IP address",
                0,
		backup_gateway_address_checkout,
		backup_gateway_address_commit,
		backup_gateway_address_diff,
		backup_gateway_address_validate);

  add_keyvalue (lan_conf_section,
		"Backup_Gateway_MAC_Address",
		"Give valid MAC address",
                0,
		backup_gateway_mac_address_checkout,
		backup_gateway_mac_address_commit,
		backup_gateway_mac_address_diff,
		backup_gateway_mac_address_validate);

  add_keyvalue (lan_conf_section,
		"Vlan_id",
		"Give valid number",
                0,
		vlan_id_checkout,
		vlan_id_commit,
		vlan_id_diff,
		vlan_id_validate);

  add_keyvalue (lan_conf_section,
		"Vlan_Id_Enable",
		"Possible values: Yes/No",
                0,
		vlan_id_enable_checkout,
		vlan_id_enable_commit,
		vlan_id_enable_diff,
		vlan_id_enable_validate);

  add_keyvalue (lan_conf_section,
		"Vlan_Priority",
		"Give valid number",
                0,
		vlan_priority_checkout,
		vlan_priority_commit,
		vlan_priority_diff,
		vlan_priority_validate);

  return lan_conf_section;
}
