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

static bmc_err_t
k_r_checkout (bmc_config_state_data_t *state_data,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];
  bmc_err_t ret;

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);

  if ((ret = get_k_r (state_data, 
                      (uint8_t *)k_r, 
                      IPMI_MAX_K_R_LENGTH)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  k_r[IPMI_MAX_K_R_LENGTH] = '\0';
  if (!(kv->value = strdup ((char *)k_r)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
k_r_commit (bmc_config_state_data_t *state_data,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_r (state_data,
		  (uint8_t *)kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static bmc_diff_t
k_r_diff (bmc_config_state_data_t *state_data,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];
  bmc_err_t ret;

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);
  if ((ret = get_k_r (state_data, 
                      k_r, 
                      IPMI_MAX_K_R_LENGTH)) != BMC_ERR_SUCCESS)
    return ret;

  if (strcmp (kv->value?kv->value:"", (char *)k_r)) 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   (char *)k_r);
    } 
  else
    ret = BMC_DIFF_SAME;

  return ret;
}

static bmc_validate_t
k_r_validate (bmc_config_state_data_t *state_data,
	      const struct section *sect,
	      const char *value)
{
  if (strlen (value) <= IPMI_MAX_K_R_LENGTH)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* k_g */

static bmc_err_t
k_g_checkout (bmc_config_state_data_t *state_data,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  bmc_err_t ret;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);
  
  if ((ret = get_k_g (state_data, 
                      k_g, 
                      IPMI_MAX_K_G_LENGTH)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = (char *)malloc(IPMI_MAX_K_G_LENGTH*2+3)))
    {
      perror("malloc");
      return BMC_ERR_FATAL_ERROR;
    }

  if (!format_kg(kv->value, IPMI_MAX_K_G_LENGTH*2+3, (unsigned char *)k_g))
    {
      free (kv->value);
      kv->value = NULL;
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
k_g_commit (bmc_config_state_data_t *state_data,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
  int k_g_len;
  
  memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);
  
  if ((k_g_len = parse_kg(k_g, IPMI_MAX_K_G_LENGTH + 1, kv->value)) < 0)
    return BMC_ERR_FATAL_ERROR;
  
  return set_k_g (state_data, k_g, k_g_len);
}

static bmc_diff_t
k_g_diff (bmc_config_state_data_t *state_data,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  uint8_t kv_k_g[IPMI_MAX_K_G_LENGTH+1];
  char k_g_str[IPMI_MAX_K_G_LENGTH*2+3];
  bmc_err_t ret;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);
  if ((ret = get_k_g (state_data, 
                      k_g, 
                      IPMI_MAX_K_G_LENGTH)) != BMC_ERR_SUCCESS)
    return ret;

  if (!format_kg(k_g_str, IPMI_MAX_K_G_LENGTH*2+3, k_g))
    return BMC_ERR_FATAL_ERROR;

  if (parse_kg(kv_k_g, IPMI_MAX_K_G_LENGTH + 1, kv->value) < 0)
    return BMC_ERR_FATAL_ERROR;
  
  /* a printable k_g key can have two representations, so compare the
     binary keys */
  if (memcmp (kv_k_g, k_g, IPMI_MAX_K_G_LENGTH)) 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   k_g_str);
    }
  else
    ret = BMC_DIFF_SAME;

  return ret;
}

static bmc_validate_t
k_g_validate (bmc_config_state_data_t *state_data,
	      const struct section *sect,
	      const char *value)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];

  if (parse_kg(k_g, IPMI_MAX_K_G_LENGTH + 1, value) < 0)
    return BMC_VALIDATE_INVALID_VALUE;
  return BMC_VALIDATE_VALID_VALUE;
}

struct section *
bmc_lan_conf_security_keys_section_get (bmc_config_state_data_t *state_data)
{
  struct section *lan_conf_security_keys_section = NULL;
  char *section_comment = 
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), a "
    "K_g BMC key may be configurable.  The K_g key is an optional key that "
    "can be set for two key authentication in IPMI 2.0.  It is optionally "
    "configured.  Most users will may to set this to zero (or blank).";

  if (!(lan_conf_security_keys_section = bmc_config_section_create (state_data, 
                                                                    "Lan_Conf_Security_Keys",
                                                                    "Lan_Conf_Security_Keys",
                                                                    section_comment,
                                                                    0)))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_security_keys_section,
                                       "K_R",
                                       "Give string or blank to clear. Max 20 chars",
                                       0,
                                       k_r_checkout,
                                       k_r_commit,
                                       k_r_diff,
                                       k_r_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_security_keys_section,
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
    bmc_config_section_destroy(state_data, lan_conf_security_keys_section);
  return NULL;
}
