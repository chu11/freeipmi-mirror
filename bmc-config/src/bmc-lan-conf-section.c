#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"
#include "bmc-validate.h"

#define BMC_MAXIPADDRLEN 16

static bmc_err_t
ip_address_source_checkout (bmc_config_state_data_t *state_data,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  uint8_t source;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_ip_address_source (state_data,
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
ip_address_source_commit (bmc_config_state_data_t *state_data,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  return set_bmc_lan_conf_ip_address_source (state_data,
					     ip_address_source_number (kv->value));
}

static bmc_diff_t
ip_address_source_diff (bmc_config_state_data_t *state_data,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address_source (state_data,
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

static bmc_err_t
ip_address_checkout (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  bmc_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_ip_address (state_data,
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
ip_address_commit (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  return set_bmc_lan_conf_ip_address (state_data,
                                      kv->value);
}

static bmc_diff_t
ip_address_diff (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address (state_data,
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

static bmc_err_t
mac_address_checkout (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  bmc_err_t ret;
  char mac[25];

  if ((ret = get_bmc_lan_conf_mac_address (state_data,
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
mac_address_commit (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_mac_address (state_data,
				       kv->value);
}

static bmc_diff_t
mac_address_diff (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  char mac[25];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_mac_address (state_data,
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

static bmc_err_t
subnet_mask_checkout (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      struct keyvalue *kv)
{
  bmc_err_t ret;
  char mask[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_subnet_mask (state_data,
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
subnet_mask_commit (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_subnet_mask (state_data,
                                       kv->value);
}

static bmc_diff_t
subnet_mask_diff (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const struct keyvalue *kv)
{
  char mask[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_subnet_mask (state_data,
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

static bmc_err_t
default_gateway_address_checkout (bmc_config_state_data_t *state_data,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  bmc_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_default_gateway_address (state_data,
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
default_gateway_address_commit (bmc_config_state_data_t *state_data,
				const struct section *sect,
				const struct keyvalue *kv)
{
  return set_bmc_lan_conf_default_gateway_address (state_data,
                                                   kv->value);
}

static bmc_diff_t
default_gateway_address_diff (bmc_config_state_data_t *state_data,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_address (state_data,
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

static bmc_err_t
default_gateway_mac_address_checkout (bmc_config_state_data_t *state_data,
				      const struct section *sect,
				      struct keyvalue *kv)
{
  bmc_err_t ret;
  char mac[25];

  if ((ret = get_bmc_lan_conf_default_gateway_mac_address (state_data,
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
default_gateway_mac_address_commit (bmc_config_state_data_t *state_data,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_default_gateway_mac_address (state_data,
						       kv->value);
}

static bmc_diff_t
default_gateway_mac_address_diff (bmc_config_state_data_t *state_data,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  char mac[25];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_mac_address (state_data,
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

/* backup */

static bmc_err_t
backup_gateway_address_checkout (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  bmc_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_backup_gateway_address (state_data,
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
backup_gateway_address_commit (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  return set_bmc_lan_conf_backup_gateway_address (state_data,
                                                  kv->value);
}

static bmc_diff_t
backup_gateway_address_diff (bmc_config_state_data_t *state_data,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_address (state_data,
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

static bmc_err_t
backup_gateway_mac_address_checkout (bmc_config_state_data_t *state_data,
				     const struct section *sect,
				     struct keyvalue *kv)
{
  bmc_err_t ret;
  char mac[25];

  if ((ret = get_bmc_lan_conf_backup_gateway_mac_address (state_data,
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
backup_gateway_mac_address_commit (bmc_config_state_data_t *state_data,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  return set_bmc_lan_conf_backup_gateway_mac_address (state_data,
						      kv->value);
}

static bmc_diff_t
backup_gateway_mac_address_diff (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  char mac[25];
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_mac_address (state_data,
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

static bmc_err_t
community_string_checkout (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           struct keyvalue *kv)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  bmc_err_t ret;

  if ((ret = get_bmc_community_string (state_data,
                                       community_string,
                                       IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != BMC_ERR_SUCCESS) 
    return ret;
		    
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ((char *)community_string)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
community_string_commit (bmc_config_state_data_t *state_data,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  if (!kv->value)
    return BMC_ERR_FATAL_ERROR;

  return set_bmc_community_string (state_data,
                                   (uint8_t *)kv->value);
}

static bmc_diff_t
community_string_diff (bmc_config_state_data_t *state_data,
                       const struct section *sect,
                       const struct keyvalue *kv)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_community_string (state_data,
                                      community_string,
                                      IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (!kv->value || !same (kv->value, (char *)community_string))
    ret = BMC_DIFF_DIFFERENT;
  else
    ret = BMC_DIFF_SAME;

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 (char *)community_string);
  return ret;
}

static bmc_validate_t
community_string_validate (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const char *value)
{
  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return BMC_VALIDATE_INVALID_VALUE;
  return BMC_VALIDATE_VALID_VALUE;
}

static bmc_err_t
vlan_id_checkout (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
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
vlan_id_commit (bmc_config_state_data_t *state_data,
		const struct section *sect,
		const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  vlan_id = atoi (kv->value);

  if ((ret = set_bmc_lan_conf_vlan_id (state_data,
                                       vlan_id,
                                       vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
vlan_id_diff (bmc_config_state_data_t *state_data,
	      const struct section *sect,
	      const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_id (state_data,
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
vlan_id_validate (bmc_config_state_data_t *state_data,
		  const struct section *sect,
		  const char *value)
{
  char *endptr;
  long int num;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  /* Vlan ids are 12 bits */
  if (num < 0 || num > 4095)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

static bmc_err_t
vlan_id_enable_checkout (bmc_config_state_data_t *state_data,
			 const struct section *sect,
			 struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
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
vlan_id_enable_commit (bmc_config_state_data_t *state_data,
		       const struct section *sect,
		       const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  if (ret != 0)
    return -1;

  vlan_id_enable = same (kv->value, "yes");

  if ((ret = set_bmc_lan_conf_vlan_id (state_data,
                                       vlan_id,
                                       vlan_id_enable)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}

static bmc_diff_t
vlan_id_enable_diff (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     const struct keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_bmc_lan_conf_vlan_id (state_data,
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

static bmc_err_t
vlan_priority_checkout (bmc_config_state_data_t *state_data,
			const struct section *sect,
			struct keyvalue *kv)
{
  uint8_t priority;
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_vlan_priority (state_data,
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
vlan_priority_commit (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  return set_bmc_lan_conf_vlan_priority (state_data,
					 atoi (kv->value));
}

static bmc_diff_t
vlan_priority_diff (bmc_config_state_data_t *state_data,
		    const struct section *sect,
		    const struct keyvalue *kv)
{
  uint8_t priority;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_priority (state_data,
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

struct section *
bmc_lan_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct section *lan_conf_section = NULL;

  if (!(lan_conf_section = bmc_section_create (state_data, "Lan_Conf")))
    goto cleanup;
  
  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"IP_Address_Source",
				"Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
				0,
				ip_address_source_checkout,
				ip_address_source_commit,
				ip_address_source_diff,
				ip_address_source_number_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"IP_Address",
				"Give valid IP address",
				0,
				ip_address_checkout,
				ip_address_commit,
				ip_address_diff,
				ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"MAC_Address",
				"Give valid MAC address",
				0,
				mac_address_checkout,
				mac_address_commit,
				mac_address_diff,
				mac_address_validate) < 0) 
    goto cleanup;

  /* TODO: checking valid netmask is not same as checking valid IP */
  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Subnet_Mask",
				"Give valid Subnet Mask",
				0,
				subnet_mask_checkout,
				subnet_mask_commit,
				subnet_mask_diff,
				ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Default_Gateway_IP_Address",
				"Give valid IP address",
				0,
				default_gateway_address_checkout,
				default_gateway_address_commit,
				default_gateway_address_diff,
				ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Default_Gateway_MAC_Address",
				"Give valid MAC address",
				0,
				default_gateway_mac_address_checkout,
				default_gateway_mac_address_commit,
				default_gateway_mac_address_diff,
				mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Backup_Gateway_IP_Address",
				"Give valid IP address",
				0,
				backup_gateway_address_checkout,
				backup_gateway_address_commit,
				backup_gateway_address_diff,
				ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Backup_Gateway_MAC_Address",
				"Give valid MAC address",
				0,
				backup_gateway_mac_address_checkout,
				backup_gateway_mac_address_commit,
				backup_gateway_mac_address_diff,
				mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Community_String",
				"Give valid string",
				0,
				community_string_checkout,
				community_string_commit,
				community_string_diff,
				community_string_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Vlan_id",
				"Give valid unsigned number",
				0,
				vlan_id_checkout,
				vlan_id_commit,
				vlan_id_diff,
				vlan_id_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Vlan_Id_Enable",
				"Possible values: Yes/No",
				0,
				vlan_id_enable_checkout,
				vlan_id_enable_commit,
				vlan_id_enable_diff,
				yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_section_add_keyvalue (state_data,
                                lan_conf_section,
				"Vlan_Priority",
				"Give valid unsigned number",
				0,
				vlan_priority_checkout,
				vlan_priority_commit,
				vlan_priority_diff,
				number_range_one_byte) < 0) 
    goto cleanup;

  return lan_conf_section;

 cleanup:
  if (lan_conf_section)
    bmc_section_destroy(state_data, lan_conf_section);
  return NULL;
}

