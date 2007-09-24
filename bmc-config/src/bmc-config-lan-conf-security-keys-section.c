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
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

#include "tool-common.h"

static config_err_t
k_r_checkout (const struct config_section *section,
	      struct config_keyvalue *kv,
              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];
  config_err_t ret;

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);

  if ((ret = get_k_r (state_data, 
                      (uint8_t *)k_r, 
                      IPMI_MAX_K_R_LENGTH)) != CONFIG_ERR_SUCCESS)
    return ret;

  k_r[IPMI_MAX_K_R_LENGTH] = '\0';
  if (!(kv->value = strdup ((char *)k_r)))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
k_r_commit (const struct config_section *section,
	    const struct config_keyvalue *kv,
            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_k_r (state_data,
		  (uint8_t *)kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static config_diff_t
k_r_diff (const struct config_section *section,
	  const struct config_keyvalue *kv,
          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];
  config_err_t ret;

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);
  if ((ret = get_k_r (state_data, 
                      k_r, 
                      IPMI_MAX_K_R_LENGTH)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (strcmp (kv->value?kv->value:"", (char *)k_r)) 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   (char *)k_r);
    } 
  else
    ret = CONFIG_DIFF_SAME;

  return ret;
}

static config_validate_t
k_r_validate (const char *section_name,
              const char *key_name,
	      const char *value)
{
  if (strlen (value) <= IPMI_MAX_K_R_LENGTH)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

/* k_g */

static config_err_t
k_g_checkout (const struct config_section *section,
	      struct config_keyvalue *kv,
              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  config_err_t ret;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);
  
  if ((ret = get_k_g (state_data, 
                      k_g, 
                      IPMI_MAX_K_G_LENGTH)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value = (char *)malloc(IPMI_MAX_K_G_LENGTH*2+3)))
    {
      perror("malloc");
      return CONFIG_ERR_FATAL_ERROR;
    }

  if (!format_kg(kv->value, IPMI_MAX_K_G_LENGTH*2+3, (unsigned char *)k_g))
    {
      free(kv->value);
      kv->value = NULL;
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
k_g_commit (const struct config_section *section,
	    const struct config_keyvalue *kv,
            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
  int k_g_len;
  
  memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);
  
  if ((k_g_len = parse_kg(k_g, IPMI_MAX_K_G_LENGTH + 1, kv->value)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return set_k_g (state_data, k_g, k_g_len);
}

static config_diff_t
k_g_diff (const struct config_section *section,
	  const struct config_keyvalue *kv,
          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  uint8_t kv_k_g[IPMI_MAX_K_G_LENGTH+1];
  char k_g_str[IPMI_MAX_K_G_LENGTH*2+3];
  config_err_t ret;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);
  if ((ret = get_k_g (state_data, 
                      k_g, 
                      IPMI_MAX_K_G_LENGTH)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!format_kg(k_g_str, IPMI_MAX_K_G_LENGTH*2+3, k_g))
    return CONFIG_ERR_FATAL_ERROR;

  if (parse_kg(kv_k_g, IPMI_MAX_K_G_LENGTH + 1, kv->value) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  /* a printable k_g key can have two representations, so compare the
     binary keys */
  if (memcmp (kv_k_g, k_g, IPMI_MAX_K_G_LENGTH)) 
    {
      ret = CONFIG_DIFF_DIFFERENT;
      report_diff (section->section_name,
                   kv->key_name,
                   kv->value,
                   k_g_str);
    }
  else
    ret = CONFIG_DIFF_SAME;

  return ret;
}

static config_validate_t
k_g_validate (const char *section_name,
              const char *key_name,
	      const char *value)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];

  if (parse_kg(k_g, IPMI_MAX_K_G_LENGTH + 1, value) < 0)
    return CONFIG_VALIDATE_INVALID_VALUE;
  return CONFIG_VALIDATE_VALID_VALUE;
}

struct config_section *
bmc_config_lan_conf_security_keys_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_conf_security_keys_section = NULL;
  char *section_comment = 
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), a "
    "K_g BMC key may be configurable.  The K_g key is an optional key that "
    "can be set for two key authentication in IPMI 2.0.  It is optionally "
    "configured.  Most users will may to set this to zero (or blank).";

  if (!(lan_conf_security_keys_section = bmc_config_section_create ("Lan_Conf_Security_Keys",
                                                                    "Lan_Conf_Security_Keys",
                                                                    section_comment,
                                                                    0)))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_security_keys_section,
                                       "K_R",
                                       "Give string or blank to clear. Max 20 chars",
                                       0,
                                       k_r_checkout,
                                       k_r_commit,
                                       k_r_diff,
                                       k_r_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (lan_conf_security_keys_section,
                                       "K_G",
                                       "Give string or blank to clear. Max 20 bytes, prefix with 0x to enter hex",
                                       0,
                                       k_g_checkout,
                                       k_g_commit,
                                       k_g_diff,
                                       k_g_validate) < 0)
    goto cleanup;

  return lan_conf_security_keys_section;

 cleanup:
  if (lan_conf_security_keys_section)
    bmc_config_section_destroy(lan_conf_security_keys_section);
  return NULL;
}
