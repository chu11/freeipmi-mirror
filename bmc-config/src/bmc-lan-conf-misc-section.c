#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-map.h"
#include "bmc-sections.h"
#include "bmc-types.h"


static int
enable_gratuitous_arps_checkout (const struct arguments *args,
				 const struct section *sect,
				 struct keyvalue *kv)
{
  int ret;
  uint8_t enable_arp;
  uint8_t reply_arp;
  ret = get_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (enable_arp)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
enable_gratuitous_arps_commit (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  int ret;
  uint8_t enable_arp;
  uint8_t reply_arp;
  ret = get_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  enable_arp = same (kv->value, "yes");

  return set_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						     enable_arp,
						     reply_arp);
}

static int
enable_gratuitous_arps_diff (const struct arguments *args,
			     const struct section *sect,
			     const struct keyvalue *kv)
{
  int ret;
  uint8_t enable_arp;
  uint8_t reply_arp;
  ret = get_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  return (enable_arp == same (kv->value, "yes")) ? 0 : 1;
}

static int
enable_gratuitous_arps_validate (const struct arguments *args,
				 const struct section *sect,
				 const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* reply */

static int
enable_arp_response_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  int ret;
  uint8_t enable_arp;
  uint8_t reply_arp;
  ret = get_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);
  
  if (reply_arp)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          exit(1);
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          exit(1);
        }
    }
  return 0;
}

static int
enable_arp_response_commit (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  int ret;
  uint8_t enable_arp;
  uint8_t reply_arp;
  ret = get_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  reply_arp = same (kv->value, "yes");

  return set_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						     enable_arp,
						     reply_arp);
}

static int
enable_arp_response_diff (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  int ret;
  uint8_t enable_arp;
  uint8_t reply_arp;
  ret = get_bmc_lan_conf_bmc_generated_arp_control (args->dev,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  return (reply_arp == same (kv->value, "yes")) ? 0 : 1;
}

static int
enable_arp_response_validate (const struct arguments *args,
			      const struct section *sect,
			      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

static int
gratuitous_arp_interval_checkout (const struct arguments *args,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  int ret;
  uint8_t interval;

  ret = get_bmc_lan_conf_gratuitous_arp_interval (args->dev,
						  &interval);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", interval);
  return 0;
}

static int
gratuitous_arp_interval_commit (const struct arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  return set_bmc_lan_conf_gratuitous_arp_interval (args->dev,
						  atoi (kv->value));
}

static int
gratuitous_arp_interval_diff (const struct arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  int ret;
  uint8_t interval;

  ret = get_bmc_lan_conf_gratuitous_arp_interval (args->dev,
						  &interval);
  if (ret != 0)
    return -1;

  return (interval == atoi (kv->value)) ? 0 : 1;
}

static int
gratuitous_arp_interval_validate (const struct arguments *args,
				  const struct section *sect,
				  const char *value)
{
  char *endptr;
  int num = strtol (value, &endptr, 0);
  
  if (*endptr)
    return -1;
  if (num < 0 || num > 255)
    return 1;
  return 0;
}

struct section *
bmc_lan_conf_misc_section_get (struct arguments *args)
{
  struct section *lan_conf_misc_section = NULL;

  if (!(lan_conf_misc_section = (void *) calloc (1, sizeof (struct section))))
    {
      perror("calloc");
      exit(1);
    }
  if (!(lan_conf_misc_section->section = strdup ("Lan_Conf_Misc")))
    {
      perror("strdup");
      exit(1);
    }

  add_keyvalue (lan_conf_misc_section,
		"Enable_Gratuitous_ARPs",
		"Possible values: Yes/No",
                0,
		enable_gratuitous_arps_checkout,
		enable_gratuitous_arps_commit,
		enable_gratuitous_arps_diff,
		enable_gratuitous_arps_validate);

  add_keyvalue (lan_conf_misc_section,
		"Enable_ARP_Response",
		"Possible values: Yes/No",
                0,
		enable_arp_response_checkout,
		enable_arp_response_commit,
		enable_arp_response_diff,
		enable_arp_response_validate);

  add_keyvalue (lan_conf_misc_section,
		"Gratuitous_ARP_Interval",
		"Give a number (x 500ms)",
                0,
		gratuitous_arp_interval_checkout,
		gratuitous_arp_interval_commit,
		gratuitous_arp_interval_diff,
		gratuitous_arp_interval_validate);

  return lan_conf_misc_section;
}
