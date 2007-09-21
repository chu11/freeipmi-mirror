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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

static config_err_t
enable_gratuitous_arps_checkout (bmc_config_state_data_t *state_data,
				 const struct config_section *sect,
				 struct config_keyvalue *kv)
{
  uint8_t enable_arp;
  uint8_t reply_arp;
  config_err_t ret;

  ret = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
						    &enable_arp,
						    &reply_arp);
  if (ret != 0)
    return -1;

  if (enable_arp)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return -1;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return -1;
        }
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_gratuitous_arps_commit (bmc_config_state_data_t *state_data,
			       const struct config_section *sect,
			       const struct config_keyvalue *kv)
{
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

static bmc_diff_t
enable_gratuitous_arps_diff (bmc_config_state_data_t *state_data,
			     const struct config_section *sect,
			     const struct config_keyvalue *kv)
{
  uint8_t enable_arp;
  uint8_t reply_arp;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
                                                        &enable_arp,
                                                        &reply_arp)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (enable_arp == same (kv->value_input, "yes"))
    ret = BMC_DIFF_SAME;
  else
    {
      ret = BMC_DIFF_DIFFERENT; 
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   (enable_arp) ? "Yes" : "No");
    }
  return ret;
}

/* reply */

static config_err_t
enable_arp_response_checkout (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      struct config_keyvalue *kv)
{
  uint8_t enable_arp;
  uint8_t reply_arp;
  config_err_t ret;

  if ((ret = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
                                                         &enable_arp,
                                                         &reply_arp)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (reply_arp)
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
enable_arp_response_commit (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    const struct config_keyvalue *kv)
{
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

static bmc_diff_t
enable_arp_response_diff (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  uint8_t enable_arp;
  uint8_t reply_arp;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_lan_conf_bmc_generated_arp_control (state_data,
                                                        &enable_arp,
                                                        &reply_arp)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (reply_arp == same (kv->value_input, "yes"))
    ret = BMC_DIFF_SAME;
  else
    {
      ret = BMC_DIFF_DIFFERENT; 
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   (reply_arp) ? "Yes" : "No");
    }

  return ret;
}

static config_err_t
gratuitous_arp_interval_checkout (bmc_config_state_data_t *state_data,
				  const struct config_section *sect,
				  struct config_keyvalue *kv)
{
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
gratuitous_arp_interval_commit (bmc_config_state_data_t *state_data,
				const struct config_section *sect,
				const struct config_keyvalue *kv)
{
  return set_bmc_lan_conf_gratuitous_arp_interval (state_data,
                                                   atoi (kv->value_input));
}

static bmc_diff_t
gratuitous_arp_interval_diff (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      const struct config_keyvalue *kv)
{
  uint8_t interval;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_bmc_lan_conf_gratuitous_arp_interval (state_data,
                                                       &interval)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (interval == atoi (kv->value_input))
    ret = BMC_DIFF_SAME;
  else
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT; 
      sprintf (num, "%d", interval);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

static config_err_t
_lan_conf_misc_checkout(const char *section_name,
                        struct config_keyvalue *keyvalues,
                        int debug,
                        void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      kv = kv->next;
    }
}

static config_err_t
_lan_conf_misc_commit(const char *section_name,
                      struct config_keyvalue *keyvalues,
                      int debug,
                      void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      kv = kv->next;
    }
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
                                                       0,
                                                       _lan_conf_misc_checkout,
                                                       _lan_conf_misc_commit)))
    goto cleanup;

  if (config_section_add_key (lan_conf_misc_section,
                              "Enable_Gratuitous_ARPs",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_misc_section,
                              "Enable_ARP_Response",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_misc_section,
                              "Gratuitous_ARP_Interval",
                              "Give a number (x 500ms)",
                              0,
                              config_number_range_one_byte) < 0)
    goto cleanup;
  return lan_conf_misc_section;

 cleanup:
  if (lan_conf_misc_section)
    config_section_destroy(lan_conf_misc_section);
  return NULL;
}
