#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"

static int
k_r_checkout (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  int ret;
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);
  ret = get_k_r (args->dev, (uint8_t *)k_r, IPMI_MAX_K_R_LENGTH);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  k_r[IPMI_MAX_K_R_LENGTH] = '\0';
  if (!(kv->value = strdup ((char *)k_r)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
k_r_commit (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_r (args->dev,
		  (uint8_t *)kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static int
k_r_diff (const struct bmc_config_arguments *args,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  int ret;
  uint8_t k_r[IPMI_MAX_K_R_LENGTH + 1];

  memset (k_r, 0, IPMI_MAX_K_R_LENGTH + 1);
  ret = get_k_r (args->dev, k_r, IPMI_MAX_K_R_LENGTH);

  if (ret != 0)
    return -1;

  if (strcmp (kv->value?kv->value:"", (char *)k_r)) 
    {
      ret = 1;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   (char *)k_r);
    } 
  else
    ret = 0;

  return ret;
}

static int
k_r_validate (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const char *value)
{
  return (value && strlen (value) <= IPMI_MAX_K_R_LENGTH) ? 0 : 1;
}


/* k_g */

static int
k_g_checkout (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  int ret;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH + 1];

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);
  ret = get_k_g (args->dev, k_g, IPMI_MAX_K_G_LENGTH);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  k_g[IPMI_MAX_K_G_LENGTH] = '\0';
  if (!(kv->value = strdup ((char *)k_g)))
    {
      perror("strdup");
      return -1;
    }

  return 0;
}

static int
k_g_commit (const struct bmc_config_arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_g (args->dev,
		  (uint8_t *)kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static int
k_g_diff (const struct bmc_config_arguments *args,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  int ret;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH + 1];

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH + 1);
  ret = get_k_g (args->dev, k_g, IPMI_MAX_K_G_LENGTH);

  if (ret != 0)
    return -1;

  if (strcmp (kv->value?kv->value:"", (char *)k_g)) 
    {
      ret = 1;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   (char *)k_g);
    }
  else
    ret = 0;

  return ret;
}

static int
k_g_validate (const struct bmc_config_arguments *args,
	      const struct section *sect,
	      const char *value)
{
  return (value && strlen (value) <= IPMI_MAX_K_G_LENGTH) ? 0 : 1;
}

struct section *
bmc_lan_conf_security_keys_section_get (struct bmc_config_arguments *args)
{
  struct section *lan_conf_security_keys_section = NULL;

  if (!(lan_conf_security_keys_section = bmc_section_create ("Lan_Conf_Security_Keys")))
    goto cleanup;

  add_keyvalue (lan_conf_security_keys_section,
		"K_R",
		"Give string or blank to clear. Max 20 chars",
                0,
		k_r_checkout,
		k_r_commit,
		k_r_diff,
		k_r_validate);

  add_keyvalue (lan_conf_security_keys_section,
		"K_G",
		"Give string or blank to clear. Max 20 chars",
                0,
		k_g_checkout,
		k_g_commit,
		k_g_diff,
		k_g_validate);

  return lan_conf_security_keys_section;

 cleanup:
  if (lan_conf_security_keys_section)
    bmc_section_destroy(lan_conf_security_keys_section);
  return NULL;
}
