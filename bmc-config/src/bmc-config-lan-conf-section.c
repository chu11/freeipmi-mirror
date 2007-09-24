#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-utils.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

#define BMC_MAXIPADDRLEN 16
#define BMC_MAXMACADDRLEN 24

static config_err_t
ip_address_source_checkout (const struct config_section *section,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t source;
  config_err_t ret;

  if ((ret = get_bmc_lan_conf_ip_address_source (state_data,
                                                 &source)) != CONFIG_ERR_SUCCESS) 
    return ret;

  if (!(kv->value = strdup (ip_address_source_string (source))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
ip_address_source_commit (const struct config_section *section,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_ip_address_source (state_data,
					     ip_address_source_number (kv->value));
}

static config_diff_t
ip_address_source_diff (const struct config_section *section,
			const struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address_source (state_data,
                                                &get_val)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  passed_val = ip_address_source_number (kv->value);
  if (passed_val == get_val)
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   ip_address_source_string (get_val));
    }
  return ret;
}

static config_err_t
ip_address_checkout (const struct config_section *section,
		     struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_ip_address (state_data,
                                          ip,
                                          BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
ip_address_commit (const struct config_section *section,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_ip_address (state_data,
                                      kv->value);
}

static config_diff_t
ip_address_diff (const struct config_section *section,
		 const struct config_keyvalue *kv,
                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char ip[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address (state_data,
                                         ip,
                                         BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   ip);
    }
  return ret;
}

static config_err_t
mac_address_checkout (const struct config_section *section,
		      struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  char mac[BMC_MAXMACADDRLEN+1];

  if ((ret = get_bmc_lan_conf_mac_address (state_data,
                                           mac,
                                           BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
mac_address_commit (const struct config_section *section,
		    const struct config_keyvalue *kv,
                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_mac_address (state_data,
				       kv->value);
}

static config_diff_t
mac_address_diff (const struct config_section *section,
		  const struct config_keyvalue *kv,
                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char mac[BMC_MAXMACADDRLEN+1];
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_mac_address (state_data,
                                          mac,
                                          BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   mac);
    }
  return ret;
}

static config_err_t
subnet_mask_checkout (const struct config_section *section,
		      struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  char mask[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_subnet_mask (state_data,
                                           mask,
                                           BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (mask)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
subnet_mask_commit (const struct config_section *section,
		    const struct config_keyvalue *kv,
                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_subnet_mask (state_data,
                                       kv->value);
}

static config_diff_t
subnet_mask_diff (const struct config_section *section,
		  const struct config_keyvalue *kv,
                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char mask[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_subnet_mask (state_data,
                                          mask,
                                          BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (mask, kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   mask);
    }
  return ret;
}

static config_err_t
default_gateway_address_checkout (const struct config_section *section,
				  struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_default_gateway_address (state_data,
                                                       ip,
                                                       BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
default_gateway_address_commit (const struct config_section *section,
				const struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_default_gateway_address (state_data,
                                                   kv->value);
}

static config_diff_t
default_gateway_address_diff (const struct config_section *section,
			      const struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char ip[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_address (state_data,
                                                      ip,
                                                      BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value)) 
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   ip);
    }
  return ret;
}

static config_err_t
default_gateway_mac_address_checkout (const struct config_section *section,
				      struct config_keyvalue *kv,
                                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  char mac[BMC_MAXMACADDRLEN+1];

  if ((ret = get_bmc_lan_conf_default_gateway_mac_address (state_data,
                                                           mac,
                                                           BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
default_gateway_mac_address_commit (const struct config_section *section,
				    const struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_default_gateway_mac_address (state_data,
						       kv->value);
}

static config_diff_t
default_gateway_mac_address_diff (const struct config_section *section,
				  const struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char mac[BMC_MAXMACADDRLEN+1];
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_mac_address (state_data,
                                                          mac,
                                                          BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   mac);
    }
  return ret;
}

/* backup */

static config_err_t
backup_gateway_address_checkout (const struct config_section *section,
				 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_backup_gateway_address (state_data,
                                                      ip,
                                                      BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (ip)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
backup_gateway_address_commit (const struct config_section *section,
			       const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_backup_gateway_address (state_data,
                                                  kv->value);
}

static config_diff_t
backup_gateway_address_diff (const struct config_section *section,
			     const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char ip[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_address (state_data,
                                                     ip,
                                                     BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   ip);
    }
  return ret;
}

static config_err_t
backup_gateway_mac_address_checkout (const struct config_section *section,
				     struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  config_err_t ret;
  char mac[BMC_MAXMACADDRLEN+1];

  if ((ret = get_bmc_lan_conf_backup_gateway_mac_address (state_data,
                                                          mac,
                                                          BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (mac)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
backup_gateway_mac_address_commit (const struct config_section *section,
                                   const struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_backup_gateway_mac_address (state_data,
						      kv->value);
}

static config_diff_t
backup_gateway_mac_address_diff (const struct config_section *section,
				 const struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  char mac[BMC_MAXMACADDRLEN+1];
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_mac_address (state_data,
                                                         mac,
                                                         BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   mac);
    }
  return ret;
}

static config_err_t
vlan_id_checkout (const struct config_section *section,
		  struct config_keyvalue *kv,
                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value, "%d", vlan_id) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_id_commit (const struct config_section *section,
		const struct config_keyvalue *kv,
                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  vlan_id = atoi (kv->value);

  if ((ret = set_bmc_lan_conf_vlan_id (state_data,
                                       vlan_id,
                                       vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_diff_t
vlan_id_diff (const struct config_section *section,
	      const struct config_keyvalue *kv,
              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_id (state_data,
                                      &vlan_id,
                                      &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (vlan_id == atoi (kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      char num[32];
      sprintf (num, "%d", vlan_id);
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   num);
    }
  return ret;
}

static config_validate_t
vlan_id_validate (const char *section_name,
                  const char *key_name,
		  const char *value)
{
  return config_check_number_range(value, 0, 4095);
}

static config_err_t
vlan_id_enable_checkout (const struct config_section *section,
			 struct config_keyvalue *kv,
                         void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = strdup (vlan_id_enable ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_id_enable_commit (const struct config_section *section,
		       const struct config_keyvalue *kv,
                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (ret != 0)
    return -1;

  vlan_id_enable = same (kv->value, "yes");

  if ((ret = set_bmc_lan_conf_vlan_id (state_data,
                                       vlan_id,
                                       vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_diff_t
vlan_id_enable_diff (const struct config_section *section,
		     const struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t rc;
  config_diff_t ret;
  
  if ((rc = get_bmc_lan_conf_vlan_id (state_data,
                                      &vlan_id,
                                      &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (vlan_id_enable == (same (kv->value, "yes")))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   vlan_id_enable ? "Yes" : "No");
    }
  return ret;
}

static config_err_t
vlan_priority_checkout (const struct config_section *section,
			struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t priority;
  config_err_t ret;

  if ((ret = get_bmc_lan_conf_vlan_priority (state_data,
                                             &priority)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value, "%d", priority) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_priority_commit (const struct config_section *section,
		      const struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_vlan_priority (state_data,
					 atoi (kv->value));
}

static config_diff_t
vlan_priority_diff (const struct config_section *section,
		    const struct config_keyvalue *kv,
                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t priority;
  config_err_t rc;
  config_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_priority (state_data,
                                            &priority)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return CONFIG_DIFF_NON_FATAL_ERROR;
      return CONFIG_DIFF_FATAL_ERROR;
    }

  if (priority == atoi (kv->value))
    ret = CONFIG_DIFF_SAME;
  else 
    {
      char prio[32];
      ret = CONFIG_DIFF_DIFFERENT;
      sprintf (prio, "%d", priority);
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   prio);
    }
  return ret;
}

struct config_section *
bmc_config_lan_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_conf_section = NULL;
  char *section_comment = 
    "In the Lan_Conf section, typical networking configuration is setup.  "
    "Most users will choose to set \"Static\" for the \"IP_Address_Source\" "
    "and set the appropriate \"IP_Address\", \"MAC_Address\", "
    "\"Subnet_Mask\", etc. for the machine.";

  if (!(lan_conf_section = bmc_config_section_create ("Lan_Conf",
                                                      "Lan_Conf",
                                                      section_comment,
                                                      0)))
    goto cleanup;
  
  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "IP_Address_Source",
                                       "Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
                                       0,
                                       ip_address_source_checkout,
                                       ip_address_source_commit,
                                       ip_address_source_diff,
                                       ip_address_source_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "IP_Address",
                                       "Give valid IP address",
                                       0,
                                       ip_address_checkout,
                                       ip_address_commit,
                                       ip_address_diff,
                                       config_ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "MAC_Address",
                                       "Give valid MAC address",
                                       0,
                                       mac_address_checkout,
                                       mac_address_commit,
                                       mac_address_diff,
                                       config_mac_address_validate) < 0) 
    goto cleanup;

  /* TODO: checking valid netmask is not same as checking valid IP */
  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Subnet_Mask",
                                       "Give valid Subnet Mask",
                                       0,
                                       subnet_mask_checkout,
                                       subnet_mask_commit,
                                       subnet_mask_diff,
                                       config_ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Default_Gateway_IP_Address",
                                       "Give valid IP address",
                                       0,
                                       default_gateway_address_checkout,
                                       default_gateway_address_commit,
                                       default_gateway_address_diff,
                                       config_ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Default_Gateway_MAC_Address",
                                       "Give valid MAC address",
                                       0,
                                       default_gateway_mac_address_checkout,
                                       default_gateway_mac_address_commit,
                                       default_gateway_mac_address_diff,
                                       config_mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Backup_Gateway_IP_Address",
                                       "Give valid IP address",
                                       0,
                                       backup_gateway_address_checkout,
                                       backup_gateway_address_commit,
                                       backup_gateway_address_diff,
                                       config_ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Backup_Gateway_MAC_Address",
                                       "Give valid MAC address",
                                       0,
                                       backup_gateway_mac_address_checkout,
                                       backup_gateway_mac_address_commit,
                                       backup_gateway_mac_address_diff,
                                       config_mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Vlan_id",
                                       "Give valid unsigned number",
                                       0,
                                       vlan_id_checkout,
                                       vlan_id_commit,
                                       vlan_id_diff,
                                       vlan_id_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Vlan_Id_Enable",
                                       "Possible values: Yes/No",
                                       0,
                                       vlan_id_enable_checkout,
                                       vlan_id_enable_commit,
                                       vlan_id_enable_diff,
                                       config_yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_section,
                                       "Vlan_Priority",
                                       "Give valid unsigned number",
                                       0,
                                       vlan_priority_checkout,
                                       vlan_priority_commit,
                                       vlan_priority_diff,
                                       config_number_range_one_byte) < 0) 
    goto cleanup;

  return lan_conf_section;

 cleanup:
  if (lan_conf_section)
    bmc_config_section_destroy(lan_conf_section);
  return NULL;
}

