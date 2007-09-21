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
#include "bmc-config-utils.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define KEY_NAME_ENABLE_GRATUITOUS_ARPS  "Enable_Gratuitous_ARPs"
#define KEY_NAME_ENABLE_ARP_RESPONSE     "Enable_ARP_Response"
#define KEY_NAME_GRATUITOUS_ARP_INTERVAL "Gratuitous_ARP_Interval"

static config_err_t
_get_lan_conf_bmc_generated_arp_control (bmc_config_state_data_t *state_data,
                                         int debug,
                                         uint8_t *bmc_generated_gratuitous_arps,
                                         uint8_t *bmc_generated_arp_responses)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control (state_data->dev,
                                                                           channel_number,
                                                                           IPMI_GET_LAN_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "bmc_generated_gratuitous_arps", &val) < 0)
    goto cleanup;
  *bmc_generated_gratuitous_arps = val;

  if (fiid_obj_get (obj_cmd_rs, "bmc_generated_arp_responses", &val) < 0)
    goto cleanup;
  *bmc_generated_arp_responses = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_get_lan_conf_gratuitous_arp_interval (bmc_config_state_data_t *state_data,
                                       int debug,
                                       uint8_t *gratuitous_arp_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval (state_data->dev,
                                                                         channel_number,
                                                                         IPMI_GET_LAN_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "gratuitous_arp_interval", &val) < 0)
    goto cleanup;
  *gratuitous_arp_interval = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_lan_conf_bmc_generated_arp_control (bmc_config_state_data_t *state_data,
                                         int debug,
                                         uint8_t bmc_generated_gratuitous_arps,
                                         uint8_t bmc_generated_arp_responses)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (state_data->dev,
                                                                           channel_number,
                                                                           bmc_generated_gratuitous_arps,
                                                                           bmc_generated_arp_responses,
                                                                           obj_cmd_rs) < 0)
    {
       if (debug)
         fprintf(stderr,
                 "ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                 ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
       rv = CONFIG_ERR_NON_FATAL_ERROR;
       goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);

}

static config_err_t
_set_lan_conf_gratuitous_arp_interval (bmc_config_state_data_t *state_data,
                                       int debug,
                                       uint8_t gratuitous_arp_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval (state_data->dev,
                                                                         channel_number,
                                                                         gratuitous_arp_interval,
                                                                         obj_cmd_rs) < 0)
    {
       if (debug)
         fprintf(stderr,
                 "ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval: %s\n",
                 ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_lan_conf_misc_checkout(const char *section_name,
                        struct config_keyvalue *keyvalues,
                        int debug,
                        void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;
  uint8_t bmc_generated_gratuitous_arps, bmc_generated_arp_responses;
  uint8_t gratuitous_arp_interval;
  uint8_t generated_arp_control_checkout = 0;
  uint8_t generated_arp_control_checkout_success = 0;
  uint8_t gratuitous_arp_interval_checkout = 0;
  uint8_t gratuitous_arp_interval_checkout_success = 0;
  char buf[BMC_CONFIG_BUFLEN];

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  /* two passes through the list is minimally slower but makes the
   * code far simpler
   */
  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_GRATUITOUS_ARPS)
          || !strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_ARP_RESPONSE))
        generated_arp_control_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_GRATUITOUS_ARP_INTERVAL))
        gratuitous_arp_interval_checkout++;
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
                    section_name);
        }
      kv = kv->next;
    }
  
  if (generated_arp_control_checkout)
    {
      if ((ret = _get_lan_conf_bmc_generated_arp_control(state_data,
                                                         debug,
                                                         &bmc_generated_gratuitous_arps,
                                                         &bmc_generated_arp_responses)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        generated_arp_control_checkout_success++;
      else
        rv = ret;
    }
  
  if (gratuitous_arp_interval_checkout)
    {
      if ((ret = _get_lan_conf_gratuitous_arp_interval (state_data,
                                                        debug,
                                                        &gratuitous_arp_interval)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        gratuitous_arp_interval_checkout_success++;
      else
        rv = ret;
    }
  
  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);
      
      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_GRATUITOUS_ARPS))
        {
          if (generated_arp_control_checkout_success)
            {
              if (config_section_update_keyvalue(kv,
                                                 NULL,
                                                 bmc_generated_gratuitous_arps ? "Yes" : "No") < 0)
                {
                  if (debug)
                    fprintf(stderr, "config_section_update_keyvalue error\n");
                  return CONFIG_ERR_FATAL_ERROR;
                }
            }
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_ARP_RESPONSE))
        {
          if (generated_arp_control_checkout_success)
            {
              if (config_section_update_keyvalue(kv,
                                                 NULL,
                                                 bmc_generated_arp_responses ? "Yes" : "No") < 0)
                {
                  if (debug)
                    fprintf(stderr, "config_section_update_keyvalue error\n");
                  return CONFIG_ERR_FATAL_ERROR;
                }
            }
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_GRATUITOUS_ARP_INTERVAL))
        {
          if (gratuitous_arp_interval_checkout_success)
            {
              snprintf(buf, BMC_CONFIG_BUFLEN, "%d", gratuitous_arp_interval);
              
              if (config_section_update_keyvalue(kv, 
                                                 NULL,
                                                 buf) < 0)
                {
                  if (debug)
                    fprintf(stderr, "config_section_update_keyvalue error\n");
                  return CONFIG_ERR_FATAL_ERROR;
                }
            }
        }
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
                    section_name);
        }
      
      kv = kv->next;
    }

  return rv;
}

static config_err_t
_lan_conf_misc_commit(const char *section_name,
                      struct config_keyvalue *keyvalues,
                      int debug,
                      void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;
  uint8_t bmc_generated_gratuitous_arps, bmc_generated_arp_responses;
  uint8_t gratuitous_arp_interval;
  uint8_t generated_arp_control_checkout = 0;
  uint8_t generated_arp_control_checkout_success = 0;
  uint8_t generated_arp_control_commit = 0;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  /* two passes through the list is minimally slower but makes the
   * code far simpler
   */
  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_GRATUITOUS_ARPS)
          || !strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_ARP_RESPONSE))
        generated_arp_control_checkout++;
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
                    section_name);
        }
      kv = kv->next;

    }

  if (generated_arp_control_checkout)
    {
      if ((ret = _get_lan_conf_bmc_generated_arp_control(state_data,
                                                         debug,
                                                         &bmc_generated_gratuitous_arps,
                                                         &bmc_generated_arp_responses)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        generated_arp_control_checkout_success++;
      else
        rv = ret;
    }
  
  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_GRATUITOUS_ARPS))
        {
          if (generated_arp_control_checkout_success)
            {
              bmc_generated_gratuitous_arps = same (kv->value_input, "yes");
              generated_arp_control_commit++;
            }
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_ARP_RESPONSE))
        {
          if (generated_arp_control_checkout_success)
            {
              bmc_generated_arp_responses = same (kv->value_input, "yes");
              generated_arp_control_commit++;
            }
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_GRATUITOUS_ARP_INTERVAL))
        {
          /* checked earlier for validity */
          gratuitous_arp_interval = atoi(kv->value_input);
          
          if ((ret = _set_lan_conf_gratuitous_arp_interval(state_data,
                                                           debug,
                                                           gratuitous_arp_interval)) == CONFIG_ERR_FATAL_ERROR)
            return CONFIG_ERR_FATAL_ERROR;
          if (ret != CONFIG_ERR_SUCCESS)
            rv = ret;
        }
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
                    section_name);
        }
      
      kv = kv->next;
    }
      
  if (generated_arp_control_commit)
    {
      if ((ret = _set_lan_conf_bmc_generated_arp_control(state_data,
                                                         debug,
                                                         bmc_generated_gratuitous_arps,
                                                         bmc_generated_arp_responses)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  return rv;
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
                              KEY_NAME_ENABLE_GRATUITOUS_ARPS,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_misc_section,
                              KEY_NAME_ENABLE_ARP_RESPONSE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_misc_section,
                              KEY_NAME_GRATUITOUS_ARP_INTERVAL,
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
