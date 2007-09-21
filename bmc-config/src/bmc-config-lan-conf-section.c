#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-utils.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define BMC_MAXIPADDRLEN 16
#define BMC_MAXMACADDRLEN 24

static config_err_t
ip_address_source_checkout (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    struct config_keyvalue *kv)
{
  uint8_t source;
  config_err_t ret;

  if ((ret = get_bmc_lan_conf_ip_address_source (state_data,
                                                 &source)) != CONFIG_ERR_SUCCESS) 
    return ret;

  if (!(kv->value_output = strdup (ip_address_source_string (source))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
ip_address_source_commit (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_ip_address_source (state_data,
					     ip_address_source_number (kv->value_input));
}

static bmc_diff_t
ip_address_source_diff (bmc_config_state_data_t *state_data,
			const struct config_section *sect,
			const struct config_keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address_source (state_data,
                                                &get_val)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_val = ip_address_source_number (kv->value_input);
  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   ip_address_source_string (get_val));
    }
  return ret;
}

static config_err_t
ip_address_checkout (bmc_config_state_data_t *state_data,
		     const struct config_section *sect,
		     struct config_keyvalue *kv)
{
  config_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_ip_address (state_data,
                                          ip,
                                          BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (ip)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
ip_address_commit (bmc_config_state_data_t *state_data,
		   const struct config_section *sect,
		   const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_ip_address (state_data,
                                      kv->value_input);
}

static bmc_diff_t
ip_address_diff (bmc_config_state_data_t *state_data,
		 const struct config_section *sect,
		 const struct config_keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_ip_address (state_data,
                                         ip,
                                         BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   ip);
    }
  return ret;
}

static config_err_t
mac_address_checkout (bmc_config_state_data_t *state_data,
		      const struct config_section *sect,
		      struct config_keyvalue *kv)
{
  config_err_t ret;
  char mac[BMC_MAXMACADDRLEN+1];

  if ((ret = get_bmc_lan_conf_mac_address (state_data,
                                           mac,
                                           BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (mac)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
mac_address_commit (bmc_config_state_data_t *state_data,
		    const struct config_section *sect,
		    const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_mac_address (state_data,
				       kv->value_input);
}

static bmc_diff_t
mac_address_diff (bmc_config_state_data_t *state_data,
		  const struct config_section *sect,
		  const struct config_keyvalue *kv)
{
  char mac[BMC_MAXMACADDRLEN+1];
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_mac_address (state_data,
                                          mac,
                                          BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   mac);
    }
  return ret;
}

static config_err_t
subnet_mask_checkout (bmc_config_state_data_t *state_data,
		      const struct config_section *sect,
		      struct config_keyvalue *kv)
{
  config_err_t ret;
  char mask[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_subnet_mask (state_data,
                                           mask,
                                           BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (mask)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
subnet_mask_commit (bmc_config_state_data_t *state_data,
		    const struct config_section *sect,
		    const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_subnet_mask (state_data,
                                       kv->value_input);
}

static bmc_diff_t
subnet_mask_diff (bmc_config_state_data_t *state_data,
		  const struct config_section *sect,
		  const struct config_keyvalue *kv)
{
  char mask[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_subnet_mask (state_data,
                                          mask,
                                          BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mask, kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   mask);
    }
  return ret;
}

static config_err_t
default_gateway_address_checkout (bmc_config_state_data_t *state_data,
				  const struct config_section *sect,
				  struct config_keyvalue *kv)
{
  config_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_default_gateway_address (state_data,
                                                       ip,
                                                       BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (!(kv->value_output = strdup (ip)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
default_gateway_address_commit (bmc_config_state_data_t *state_data,
				const struct config_section *sect,
				const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_default_gateway_address (state_data,
                                                   kv->value_input);
}

static bmc_diff_t
default_gateway_address_diff (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      const struct config_keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_address (state_data,
                                                      ip,
                                                      BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value_input)) 
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   ip);
    }
  return ret;
}

static config_err_t
default_gateway_mac_address_checkout (bmc_config_state_data_t *state_data,
				      const struct config_section *sect,
				      struct config_keyvalue *kv)
{
  config_err_t ret;
  char mac[BMC_MAXMACADDRLEN+1];

  if ((ret = get_bmc_lan_conf_default_gateway_mac_address (state_data,
                                                           mac,
                                                           BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (mac)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
default_gateway_mac_address_commit (bmc_config_state_data_t *state_data,
				    const struct config_section *sect,
				    const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_default_gateway_mac_address (state_data,
						       kv->value_input);
}

static bmc_diff_t
default_gateway_mac_address_diff (bmc_config_state_data_t *state_data,
				  const struct config_section *sect,
				  const struct config_keyvalue *kv)
{
  char mac[BMC_MAXMACADDRLEN+1];
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_default_gateway_mac_address (state_data,
                                                          mac,
                                                          BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   mac);
    }
  return ret;
}

/* backup */

static config_err_t
backup_gateway_address_checkout (bmc_config_state_data_t *state_data,
				 const struct config_section *sect,
				 struct config_keyvalue *kv)
{
  config_err_t ret;
  char ip[BMC_MAXIPADDRLEN + 1];

  if ((ret = get_bmc_lan_conf_backup_gateway_address (state_data,
                                                      ip,
                                                      BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (ip)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
backup_gateway_address_commit (bmc_config_state_data_t *state_data,
			       const struct config_section *sect,
			       const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_backup_gateway_address (state_data,
                                                  kv->value_input);
}

static bmc_diff_t
backup_gateway_address_diff (bmc_config_state_data_t *state_data,
			     const struct config_section *sect,
			     const struct config_keyvalue *kv)
{
  char ip[BMC_MAXIPADDRLEN + 1];
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_address (state_data,
                                                     ip,
                                                     BMC_MAXIPADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (ip, kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   ip);
    }
  return ret;
}

static config_err_t
backup_gateway_mac_address_checkout (bmc_config_state_data_t *state_data,
				     const struct config_section *sect,
				     struct config_keyvalue *kv)
{
  config_err_t ret;
  char mac[BMC_MAXMACADDRLEN+1];

  if ((ret = get_bmc_lan_conf_backup_gateway_mac_address (state_data,
                                                          mac,
                                                          BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (mac)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
backup_gateway_mac_address_commit (bmc_config_state_data_t *state_data,
				    const struct config_section *sect,
				    const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_backup_gateway_mac_address (state_data,
						      kv->value_input);
}

static bmc_diff_t
backup_gateway_mac_address_diff (bmc_config_state_data_t *state_data,
				 const struct config_section *sect,
				 const struct config_keyvalue *kv)
{
  char mac[BMC_MAXMACADDRLEN+1];
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_backup_gateway_mac_address (state_data,
                                                         mac,
                                                         BMC_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (same (mac, kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   mac);
    }
  return ret;
}

static config_err_t
vlan_id_checkout (bmc_config_state_data_t *state_data,
		  const struct config_section *sect,
		  struct config_keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", vlan_id) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_id_commit (bmc_config_state_data_t *state_data,
		const struct config_section *sect,
		const struct config_keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  vlan_id = atoi (kv->value_input);

  if ((ret = set_bmc_lan_conf_vlan_id (state_data,
                                       vlan_id,
                                       vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static bmc_diff_t
vlan_id_diff (bmc_config_state_data_t *state_data,
	      const struct config_section *sect,
	      const struct config_keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_id (state_data,
                                      &vlan_id,
                                      &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (vlan_id == atoi (kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      sprintf (num, "%d", vlan_id);
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

static config_err_t
vlan_id_enable_checkout (bmc_config_state_data_t *state_data,
			 const struct config_section *sect,
			 struct config_keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (vlan_id_enable)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_id_enable_commit (bmc_config_state_data_t *state_data,
		       const struct config_section *sect,
		       const struct config_keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_vlan_id (state_data,
                                       &vlan_id,
                                       &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (ret != 0)
    return -1;

  vlan_id_enable = same (kv->value_input, "yes");

  if ((ret = set_bmc_lan_conf_vlan_id (state_data,
                                       vlan_id,
                                       vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static bmc_diff_t
vlan_id_enable_diff (bmc_config_state_data_t *state_data,
		     const struct config_section *sect,
		     const struct config_keyvalue *kv)
{
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_bmc_lan_conf_vlan_id (state_data,
                                      &vlan_id,
                                      &vlan_id_enable)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (vlan_id_enable == (same (kv->value_input, "yes")))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   vlan_id_enable ? "Yes" : "No");
    }
  return ret;
}

static config_err_t
vlan_priority_checkout (bmc_config_state_data_t *state_data,
			const struct config_section *sect,
			struct config_keyvalue *kv)
{
  uint8_t priority;
  config_err_t ret;

  if ((ret = get_bmc_lan_conf_vlan_priority (state_data,
                                             &priority)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", priority) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_priority_commit (bmc_config_state_data_t *state_data,
		      const struct config_section *sect,
		      const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_vlan_priority (state_data,
					 atoi (kv->value_input));
}

static bmc_diff_t
vlan_priority_diff (bmc_config_state_data_t *state_data,
		    const struct config_section *sect,
		    const struct config_keyvalue *kv)
{
  uint8_t priority;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_vlan_priority (state_data,
                                            &priority)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (priority == atoi (kv->value_input))
    ret = BMC_DIFF_SAME;
  else 
    {
      char prio[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (prio, "%d", priority);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   prio);
    }
  return ret;
}

static config_err_t
_lan_conf_checkout(const char *section_name,
                   struct config_keyvalue *keyvalues,
                   int debug,
                   void *arg)
{
  bmc_config_state_data_t *state_data;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;


}

static config_err_t
_lan_conf_commit(const char *section_name,
                 struct config_keyvalue *keyvalues,
                 int debug,
                 void *arg)
{
  bmc_config_state_data_t *state_data;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;
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

  if (!(lan_conf_section = config_section_create ("Lan_Conf",
                                                  "Lan_Conf",
                                                  section_comment,
                                                  0,
                                                  _lan_conf_checkout,
                                                  _lan_conf_commit)))
    goto cleanup;
  
  if (config_section_add_key (lan_conf_section,
                              "IP_Address_Source",
                              "Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
                              0,
                              ip_address_source_number_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "IP_Address",
                              "Give valid IP address",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "MAC_Address",
                              "Give valid MAC address",
                              0,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  /* TODO: checking valid netmask is not same as checking valid IP */
  if (config_section_add_key (lan_conf_section,
                              "Subnet_Mask",
                              "Give valid Subnet Mask",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "Default_Gateway_IP_Address",
                              "Give valid IP address",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "Default_Gateway_MAC_Address",
                              "Give valid MAC address",
                              0,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "Backup_Gateway_IP_Address",
                              "Give valid IP address",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "Backup_Gateway_MAC_Address",
                              "Give valid MAC address",
                              0,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "Vlan_id",
                              "Give valid unsigned number",
                              0,
                              config_number_range_twelve_bits) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "Vlan_Id_Enable",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              "Vlan_Priority",
                              "Give valid unsigned number",
                              0,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  return lan_conf_section;

 cleanup:
  if (lan_conf_section)
    config_section_destroy(lan_conf_section);
  return NULL;
}

