#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"


#define BMC_MAXIPADDRLEN 16

static bmc_err_t
ip_address_source_checkout (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  uint8_t source;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_ip_address_source (args->dev,
                                                 &source)) != BMC_ERR_SUCCESS) 
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (ip_address_source_string (source))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
ip_address_source_commit (const struct bmc_config_arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  return set_bmc_lan_conf_ip_address_source (args->dev,
					     ip_address_source_number (kv->value));
}

static bmc_diff_t
ip_address_source_diff (const struct bmc_config_arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address_source (args->dev,
                                                &get_val)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = ip_address_source_number (kv->value);
  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   ip_address_source_string (get_val));
    }
  return ret;
}

static bmc_validate_t
ip_address_source_validate (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const char *value)
{
  if (ip_address_source_number (value) >= 0)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
ip_address_checkout (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  bmc_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_ip_address (args->dev,
                                          (char *)&ip)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
ip_address_commit (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  return set_bmc_lan_conf_ip_address (args->dev,
                                      kv->value);
}

static bmc_diff_t
ip_address_diff (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address (args->dev,
                                         (char *)&ip)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   ip);
    }
  return ret;
}

static bmc_validate_t
ip_address_validate (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const char *value)
{
  struct in_addr a;
  
  if (inet_aton (value, &a))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
mac_address_checkout (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  bmc_err_t ret;
  char mac[25];

  if ((ret = get_bmc_lan_conf_mac_address (args->dev,
                                           (char *)&mac)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
mac_address_commit (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_mac_address (args->dev,
				       kv->value);
}

static bmc_diff_t
mac_address_diff (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  char mac[25];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_mac_address (args->dev,
                                          (char *)&mac)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   mac);
    }
  return ret;
}

static bmc_validate_t
mac_address_validate (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const char *value)
{
  unsigned int foo;

  if (sscanf (value, 
              "%02x:%02x:%02x:%02x:%02x:%02x", 
              &foo,
              &foo,
              &foo, 
              &foo,
              &foo, 
              &foo) == 6)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
subnet_mask_checkout (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  bmc_err_t ret;
  char mask[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_subnet_mask (args->dev,
                                           (char *)&mask)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mask)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
subnet_mask_commit (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_subnet_mask (args->dev,
                                       kv->value);
}

static bmc_diff_t
subnet_mask_diff (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  char mask[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_subnet_mask (args->dev,
                                          (char *)&mask)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mask, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   mask);
    }
  return ret;
}

static bmc_validate_t
subnet_mask_validate (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const char *value)
{
  /* TODO: checking valid netmask is not same as checking valid IP */
  struct in_addr a;
  
  if (inet_aton (value, &a))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
default_gateway_address_checkout (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  bmc_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_default_gateway_address (args->dev,
                                                       (char *)&ip)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
default_gateway_address_commit (const struct bmc_config_arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  return set_bmc_lan_conf_default_gateway_address (args->dev,
                                                   kv->value);
}

static bmc_diff_t
default_gateway_address_diff (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_address (args->dev,
                                                      (char *)&ip)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value)) 
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   ip);
    }
  return ret;
}

static bmc_validate_t
default_gateway_address_validate (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const char *value)
{
  struct in_addr a;
  
  if (inet_aton (value, &a))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
default_gateway_mac_address_checkout (const struct bmc_config_arguments *args,
				      const struct section *sect,
				      struct keyvalue *kv)
{
  bmc_err_t ret;
  char mac[25];

  if ((ret = get_bmc_lan_conf_default_gateway_mac_address (args->dev,
                                                           (char *)&mac)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
default_gateway_mac_address_commit (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_default_gateway_mac_address (args->dev,
						       kv->value);
}

static bmc_diff_t
default_gateway_mac_address_diff (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  char mac[25];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_mac_address (args->dev,
                                                          (char *)&mac)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   mac);
    }
  return ret;
}

static bmc_validate_t
default_gateway_mac_address_validate (const struct bmc_config_arguments *args,
				      const struct section *sect,
				      const char *value)
{
  unsigned int foo;

  if (sscanf (value, 
              "%02x:%02x:%02x:%02x:%02x:%02x", 
              &foo,
              &foo,
              &foo, 
              &foo,
              &foo, 
              &foo) == 6)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* backup */


static bmc_err_t
backup_gateway_address_checkout (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  bmc_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_backup_gateway_address (args->dev,
                                                      (char *)&ip)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
backup_gateway_address_commit (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  return set_bmc_lan_conf_backup_gateway_address (args->dev,
                                                  kv->value);
}

static bmc_diff_t
backup_gateway_address_diff (const struct bmc_config_arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_address (args->dev,
                                                     (char *)&ip)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   ip);
    }
  return ret;
}

static bmc_validate_t
backup_gateway_address_validate (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const char *value)
{
  struct in_addr a;
  
  if (inet_aton (value, &a))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
backup_gateway_mac_address_checkout (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  bmc_err_t ret;
  char mac[25];

  if ((ret = get_bmc_lan_conf_backup_gateway_mac_address (args->dev,
                                                          (char *)&mac)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
backup_gateway_mac_address_commit (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_backup_gateway_mac_address (args->dev,
						      kv->value);
}

static bmc_diff_t
backup_gateway_mac_address_diff (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  char mac[25];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_mac_address (args->dev,
                                                         (char *)&mac)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   mac);
    }
  return ret;
}

static bmc_validate_t
backup_gateway_mac_address_validate (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     const char *value)
{
  unsigned int foo;

  if (sscanf (value, 
              "%02x:%02x:%02x:%02x:%02x:%02x", 
              &foo,
              &foo,
              &foo, 
              &foo,
              &foo, 
              &foo) == 6)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
vlan_id_checkout (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (args->dev,
                                       &vlan_id,
                                       &vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", vlan_id) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
vlan_id_commit (const struct bmc_config_arguments *args,
		const struct section *sect,
		const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (args->dev,
                                       &vlan_id,
                                       &vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  vlan_id = atoi (kv->value);

  if ((ret = set_bmc_lan_conf_vlan_id (args->dev,
                                       vlan_id,
                                       vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
vlan_id_diff (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_id (args->dev,
                                      &vlan_id,
                                      &vlan_id_enable)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (vlan_id == atoi (kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      sprintf (num, "%d", vlan_id);
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static bmc_validate_t
vlan_id_validate (const struct bmc_config_arguments *args,
		  const struct section *sect,
		  const char *value)
{
  char *endptr;
  uint32_t num = strtoul (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (num == UINT_MAX || num == (uint32_t)(INT_MIN))
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

static bmc_err_t
vlan_id_enable_checkout (const struct bmc_config_arguments *args,
			 const struct section *sect,
			 struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (args->dev,
                                       &vlan_id,
                                       &vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (vlan_id_enable)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
vlan_id_enable_commit (const struct bmc_config_arguments *args,
		       const struct section *sect,
		       const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (args->dev,
                                       &vlan_id,
                                       &vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  if (ret != 0)
    return -1;

  vlan_id_enable = same (kv->value, "yes");

  if ((ret = set_bmc_lan_conf_vlan_id (args->dev,
                                       vlan_id,
                                       vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
vlan_id_enable_diff (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_bmc_lan_conf_vlan_id (args->dev,
                                      &vlan_id,
                                      &vlan_id_enable)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (vlan_id_enable == (same (kv->value, "yes")))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   vlan_id_enable ? "Yes" : "No");
    }
  return ret;
}

static bmc_validate_t
vlan_id_enable_validate (const struct bmc_config_arguments *args,
			 const struct section *sect,
			 const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

static bmc_err_t
vlan_priority_checkout (const struct bmc_config_arguments *args,
			const struct section *sect,
			struct keyvalue *kv)
{
  uint8_t priority;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_vlan_priority (args->dev,
                                             &priority)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", priority) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
vlan_priority_commit (const struct bmc_config_arguments *args,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  return set_bmc_lan_conf_vlan_priority (args->dev,
					 atoi (kv->value));
}

static bmc_diff_t
vlan_priority_diff (const struct bmc_config_arguments *args,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  uint8_t priority;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_priority (args->dev,
                                            &priority)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (priority == atoi (kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      char prio[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (prio, "%d", priority);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   prio);
    }
  return ret;
}

static bmc_validate_t
vlan_priority_validate (const struct bmc_config_arguments *args,
			const struct section *sect,
			const char *value)
{
  char *endptr;
  uint32_t num = strtoul (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (num < 0 || num > 255)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

struct section *
bmc_lan_conf_section_get (struct bmc_config_arguments *args)
{
  struct section *lan_conf_section = NULL;

  if (!(lan_conf_section = bmc_section_create ("Lan_Conf")))
    goto cleanup;
  
  if (bmc_section_add_keyvalue (lan_conf_section,
				"IP_Address_Source",
				"Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
				0,
				ip_address_source_checkout,
				ip_address_source_commit,
				ip_address_source_diff,
				ip_address_source_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"IP_Address",
				"Give valid IP address",
				0,
				ip_address_checkout,
				ip_address_commit,
				ip_address_diff,
				ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"MAC_Address",
				"Give valid MAC address",
				0,
				mac_address_checkout,
				mac_address_commit,
				mac_address_diff,
				mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Subnet_Mask",
				"Give valid Subnet Mask",
				0,
				subnet_mask_checkout,
				subnet_mask_commit,
				subnet_mask_diff,
				subnet_mask_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Default_Gateway_IP_Address",
				"Give valid IP address",
				0,
				default_gateway_address_checkout,
				default_gateway_address_commit,
				default_gateway_address_diff,
				default_gateway_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Default_Gateway_MAC_Address",
				"Give valid MAC address",
				0,
				default_gateway_mac_address_checkout,
				default_gateway_mac_address_commit,
				default_gateway_mac_address_diff,
				default_gateway_mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Backup_Gateway_IP_Address",
				"Give valid IP address",
				0,
				backup_gateway_address_checkout,
				backup_gateway_address_commit,
				backup_gateway_address_diff,
				backup_gateway_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Backup_Gateway_MAC_Address",
				"Give valid MAC address",
				0,
				backup_gateway_mac_address_checkout,
				backup_gateway_mac_address_commit,
				backup_gateway_mac_address_diff,
				backup_gateway_mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Vlan_id",
				"Give valid number",
				0,
				vlan_id_checkout,
				vlan_id_commit,
				vlan_id_diff,
				vlan_id_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Vlan_Id_Enable",
				"Possible values: Yes/No",
				0,
				vlan_id_enable_checkout,
				vlan_id_enable_commit,
				vlan_id_enable_diff,
				vlan_id_enable_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_section,
				"Vlan_Priority",
				"Give valid number",
				0,
				vlan_priority_checkout,
				vlan_priority_commit,
				vlan_priority_diff,
				vlan_priority_validate) < 0) 
    goto cleanup;

  return lan_conf_section;

 cleanup:
  if (lan_conf_section)
    bmc_section_destroy(lan_conf_section);
  return NULL;
}
