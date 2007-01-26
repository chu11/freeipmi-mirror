#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"

static bmc_err_t
k_r_checkout (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];
  bmc_err_t ret;

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);

  if ((ret = get_k_r (args->dev, 
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
k_r_commit (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_r (args->dev,
		  (uint8_t *)kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static bmc_diff_t
k_r_diff (const struct bmc_config_arguments *args,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];
  bmc_err_t ret;

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);
  if ((ret = get_k_r (args->dev, 
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
k_r_validate (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const char *value)
{
  if (strlen (value) <= IPMI_MAX_K_R_LENGTH)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* k_g */

static bmc_err_t
k_g_checkout (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH + 1];
  bmc_err_t ret;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);
  
  if ((ret = get_k_g (args->dev, 
                      k_g, 
                      IPMI_MAX_K_G_LENGTH)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  k_g[IPMI_MAX_K_G_LENGTH] = '\0';
  if (!(kv->value = strdup ((char *)k_g)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
k_g_commit (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_g (args->dev,
		  (uint8_t *)kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static bmc_diff_t
k_g_diff (const struct bmc_config_arguments *args,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH + 1];
  bmc_err_t ret;

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);
  if ((ret = get_k_g (args->dev, 
                      k_g, 
                      IPMI_MAX_K_G_LENGTH)) != BMC_ERR_SUCCESS)
    return ret;

  if (strcmp (kv->value ? kv->value : "", (char *)k_g)) 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   (char *)k_g);
    }
  else
    ret = BMC_DIFF_SAME;

  return ret;
}

static bmc_validate_t
k_g_validate (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const char *value)
{
  if (strlen (value) <= IPMI_MAX_K_G_LENGTH)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

struct section *
bmc_lan_conf_security_keys_section_get (struct bmc_config_arguments *args)
{
  struct section *lan_conf_security_keys_section = NULL;

  if (!(lan_conf_security_keys_section = bmc_section_create ("Lan_Conf_Security_Keys")))
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_security_keys_section,
				"K_R",
				"Give string or blank to clear. Max 20 chars",
				0,
				k_r_checkout,
				k_r_commit,
				k_r_diff,
				k_r_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (lan_conf_security_keys_section,
				"K_G",
				"Give string or blank to clear. Max 20 chars",
				0,
				k_g_checkout,
				k_g_commit,
				k_g_diff,
				k_g_validate) < 0)
    goto cleanup;

  return lan_conf_security_keys_section;

 cleanup:
  if (lan_conf_security_keys_section)
    bmc_section_destroy(lan_conf_security_keys_section);
  return NULL;
}
