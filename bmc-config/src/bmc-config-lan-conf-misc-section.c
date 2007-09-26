#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

static config_err_t
enable_gratuitous_arps_checkout (const char *section_name,
				 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t enable_arp;
  uint8_t reply_arp;
  config_err_t ret;

  ret = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  if (!(kv->value_output = strdup (enable_arp ? "Yes" : "No")))
    {
      perror("strdup");
      return -1;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_gratuitous_arps_commit (const char *section_name,
			       const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  int ret;
  uint8_t enable_arp;
  uint8_t reply_arp;
  ret = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  enable_arp = same (kv->value_input, "yes");

  return set_bmc_lan_conf_bmc_generated_arp_control (state_data,
						     enable_arp,
						     reply_arp);
}

static config_err_t
enable_arp_response_checkout (const char *section_name,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t enable_arp;
  uint8_t reply_arp;
  config_err_t ret;

  if ((ret = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
                                                         &enable_arp,
                                                         &reply_arp)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (reply_arp ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_arp_response_commit (const char *section_name,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t enable_arp;
  uint8_t reply_arp;
  config_err_t ret;
  
  if ((ret = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
                                                         &enable_arp,
                                                         &reply_arp)) != CONFIG_ERR_SUCCESS)
    return ret;

  reply_arp = same (kv->value_input, "yes");

  return set_bmc_lan_conf_bmc_generated_arp_control (state_data,
						     enable_arp,
						     reply_arp);
}

static config_err_t
gratuitous_arp_interval_checkout (const char *section_name,
				  struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  config_err_t ret;

  if ((ret = get_bmc_lan_conf_gratuitous_arp_interval (state_data,
                                                       &interval)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", interval) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
gratuitous_arp_interval_commit (const char *section_name,
				const struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_bmc_lan_conf_gratuitous_arp_interval (state_data,
                                                   atoi (kv->value_input));
}

struct config_section *
bmc_config_lan_conf_misc_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_conf_misc_section = NULL;
  char *section_comment = 
    "The following miscellaneous configuration options are optionally "
    "implemented by the vendor.  They may not be available your system and "
    "may not be visible below."
    "\n"
    "If set to \"Yes\", \"Enable_Gratuitous_ARPs\" will inform the BMC to "
    "regularly send out Gratuitous ARPs to allow other machines on a "
    "network resolve the BMC's MAC Address.  Many users will want to set "
    "this to \"Yes\" because it offers the easiest way to support BMC IP "
    "Address resolution.  However, it will increase traffic on your "
    "network.  The \"Gratuitous_ARP_Interval\" can be used to set the "
    "period a Gratuitous ARP is always sent."
    "\n"
    "If set to \"Yes\", \"Enable_ARP_Response\" will inform the BMC to"
    "respond to ARP requests from other machines.";

  if (!(lan_conf_misc_section = config_section_create ("Lan_Conf_Misc",
                                                       "Lan_Conf_Misc",
                                                       section_comment,
                                                       0)))
    goto cleanup;

  if (config_section_add_keyvalue (lan_conf_misc_section,
                                   "Enable_Gratuitous_ARPs",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_gratuitous_arps_checkout,
                                   enable_gratuitous_arps_commit,
                                   config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (lan_conf_misc_section,
                                   "Enable_ARP_Response",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_arp_response_checkout,
                                   enable_arp_response_commit,
                                   config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_keyvalue (lan_conf_misc_section,
                                   "Gratuitous_ARP_Interval",
                                   "Give a number (x 500ms)",
                                   0,
                                   gratuitous_arp_interval_checkout,
                                   gratuitous_arp_interval_commit,
                                   config_number_range_one_byte) < 0)
    goto cleanup;
  return lan_conf_misc_section;

 cleanup:
  if (lan_conf_misc_section)
    config_section_destroy(lan_conf_misc_section);
  return NULL;
}
