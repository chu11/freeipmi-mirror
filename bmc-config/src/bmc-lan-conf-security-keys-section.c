#include "bmc-common.h"
#include "bmc-diff.h"
#include "bmc-types.h"
#include "bmc-config-api.h"
#include "bmc-sections.h"

static int
k_r_checkout (const struct arguments *args,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  int ret;
  char k_r[21];

  memset (k_r, 0, 21);
  ret = get_k_r ((ipmi_device_t *) &args->dev,
		 k_r, 20);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  k_r[21] = 0;
  kv->value = strdup (k_r);

  return 0;
}

static int
k_r_commit (const struct arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_r ((ipmi_device_t *) &args->dev,
		  kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static int
k_r_diff (const struct arguments *args,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  int ret;
  char k_r[21];

  memset (k_r, 0, 21);
  ret = get_k_r ((ipmi_device_t *) &args->dev,
		 k_r, 20);

  if (ret != 0)
    return -1;

  if (strcmp (kv->value?kv->value:"", k_r)) {
    ret = 1;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 k_r);
  } else {
    ret = 0;
  }
  return ret;
}

static int
k_r_validate (const struct arguments *args,
	      const struct section *sect,
	      const char *value)
{
  return (value && strlen (value) <= 20) ? 0 : 1;
}


/* k_g */

static int
k_g_checkout (const struct arguments *args,
	      const struct section *sect,
	      struct keyvalue *kv)
{
  int ret;
  char k_g[21];

  memset (k_g, 0, 21);
  ret = get_k_g ((ipmi_device_t *) &args->dev,
		 k_g, 20);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  k_g[21] = 0;
  kv->value = strdup (k_g);

  return 0;
}

static int
k_g_commit (const struct arguments *args,
	    const struct section *sect,
	    const struct keyvalue *kv)
{
  return set_k_g ((ipmi_device_t *) &args->dev,
		  kv->value, 
		  kv->value ? strlen (kv->value): 0);
}

static int
k_g_diff (const struct arguments *args,
	  const struct section *sect,
	  const struct keyvalue *kv)
{
  int ret;
  char k_g[21];

  memset (k_g, 0, 21);
  ret = get_k_g ((ipmi_device_t *) &args->dev,
		 k_g, 20);

  if (ret != 0)
    return -1;

  if (strcmp (kv->value?kv->value:"", k_g)) {
    ret = 1;
    report_diff (sect->section,
		 kv->key,
		 kv->value,
		 k_g);
  } else {
    ret = 0;
  }
  return ret;
}

static int
k_g_validate (const struct arguments *args,
	      const struct section *sect,
	      const char *value)
{
  return (value && strlen (value) <= 20) ? 0 : 1;
}

struct section *
bmc_lan_conf_security_keys_section_get (struct arguments *args)
{
  struct section *lan_conf_security_keys_section = NULL;

  lan_conf_security_keys_section = (void *) calloc (1, sizeof (struct section));
  lan_conf_security_keys_section->section = strdup ("LAN_Conf_Security_Keys");

  add_keyvalue (lan_conf_security_keys_section,
		"K_R",
		"Give string or blank to clear. Max 20 chars",
		k_r_checkout,
		k_r_commit,
		k_r_diff,
		k_r_validate);

  add_keyvalue (lan_conf_security_keys_section,
		"K_G",
		"Give string or blank to clear. Max 20 chars",
		k_g_checkout,
		k_g_commit,
		k_g_diff,
		k_g_validate);

  return lan_conf_security_keys_section;
}
