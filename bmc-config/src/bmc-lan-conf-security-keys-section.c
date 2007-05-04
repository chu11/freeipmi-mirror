#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"
#include "bmc-types.h"
#include "ipmi-common.h"

static int
k_r_checkout (const struct arguments *args,
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
      exit(1);
    }

  return 0;
}

static int
k_r_commit (const struct arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_r (args->dev,
		  (uint8_t *)kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static int
k_r_diff (const struct arguments *args,
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
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   (char *)k_r);
    } 
  else
    ret = 0;

  return ret;
}

static int
k_r_validate (const struct arguments *args,
	      const struct section *sect,
	      const char *value)
{
  return (value && strlen (value) <= IPMI_MAX_K_R_LENGTH) ? 0 : 1;
}


/* k_g */

static int
k_g_checkout (const struct arguments *args,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  int ret;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);
  ret = get_k_g (args->dev, k_g, IPMI_MAX_K_G_LENGTH);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = (char *)malloc(IPMI_MAX_K_G_LENGTH*2+3)))
    {
      perror("malloc");
      exit(1);
    }

  if (!format_kg(kv->value, IPMI_MAX_K_G_LENGTH*2+3, (unsigned char *)k_g))
    {
      free (kv->value);
      kv->value = NULL;
      return -1;
    }

  return 0;
}

static int
k_g_commit (const struct arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  
  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);
  
  if (parse_kg(k_g, IPMI_MAX_K_G_LENGTH, kv->value) < 0)
    return -1;

  return set_k_g (args->dev, k_g, IPMI_MAX_K_G_LENGTH);
}

static int
k_g_diff (const struct arguments *args,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  int ret;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  uint8_t kv_k_g[IPMI_MAX_K_G_LENGTH];
  char k_g_str[IPMI_MAX_K_G_LENGTH*2+3];

  memset (k_g, 0, IPMI_MAX_K_G_LENGTH);
  ret = get_k_g (args->dev, k_g, IPMI_MAX_K_G_LENGTH);

  if (ret != 0)
    return -1;

  if (!format_kg(k_g_str, IPMI_MAX_K_G_LENGTH*2+3, k_g))
    return -1;

  if (parse_kg(kv_k_g, IPMI_MAX_K_G_LENGTH, kv->value) < 0)
    return -1;

  /* a printable k_g key can have two representations, so compare the
     binary keys */
  if (memcmp (kv_k_g, k_g, IPMI_MAX_K_G_LENGTH))
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   k_g_str);
    }
  else
    ret = 0;
  
  return ret;
}

static int
k_g_validate (const struct arguments *args,
	      const struct section *sect,
	      const char *value)
{
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  if (parse_kg(k_g, IPMI_MAX_K_G_LENGTH, value) < 0)
    return 1;
  return 0;
}

struct section *
bmc_lan_conf_security_keys_section_get (struct arguments *args)
{
  struct section *lan_conf_security_keys_section = NULL;

  if (!(lan_conf_security_keys_section = (void *) calloc (1, sizeof (struct section))))
    {
      perror("calloc");
      exit(1);
    }
  if (!(lan_conf_security_keys_section->section = strdup ("Lan_Conf_Security_Keys")))
    {
      perror("strdup");
      exit(1);
    }

  add_keyvalue (lan_conf_security_keys_section,
		"K_R",
		"Give string or blank to clear. Max 20 chars, prefix with 0x to enter hex",
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
}
