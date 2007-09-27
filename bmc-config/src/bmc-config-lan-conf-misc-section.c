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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

/* convenience struct */
struct bmc_generated_arp_control
{
  uint8_t bmc_generated_gratuitous_arps;
  uint8_t bmc_generated_arp_responses;
};

static config_err_t
_get_bmc_generated_arp_control (bmc_config_state_data_t *state_data,
                                struct bmc_generated_arp_control *ac)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(ac);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "bmc_generated_gratuitous_arps", &val) < 0)
    goto cleanup;
  ac->bmc_generated_gratuitous_arps = val;

  if (Fiid_obj_get (obj_cmd_rs, "bmc_generated_arp_responses", &val) < 0)
    goto cleanup;
  ac->bmc_generated_arp_responses = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_bmc_generated_arp_control (bmc_config_state_data_t *state_data,
                                struct bmc_generated_arp_control *ac)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(ac);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (state_data->dev,
                                                                           channel_number,
                                                                           ac->bmc_generated_gratuitous_arps,
                                                                           ac->bmc_generated_arp_responses,
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);

}

static config_err_t
enable_gratuitous_arps_checkout (const char *section_name,
				 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct bmc_generated_arp_control ac;
  config_err_t ret;

  if ((ret = _get_bmc_generated_arp_control (state_data, &ac)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(kv, ac.bmc_generated_gratuitous_arps ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_gratuitous_arps_commit (const char *section_name,
			       const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct bmc_generated_arp_control ac;
  config_err_t ret;

  if ((ret = _get_bmc_generated_arp_control (state_data, &ac)) != CONFIG_ERR_SUCCESS)
    return ret;

  ac.bmc_generated_gratuitous_arps = same (kv->value_input, "yes");
  return _set_bmc_generated_arp_control (state_data, &ac);
}

static config_err_t
enable_arp_response_checkout (const char *section_name,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct bmc_generated_arp_control ac;
  config_err_t ret;

  if ((ret = _get_bmc_generated_arp_control (state_data, &ac)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(kv, ac.bmc_generated_arp_responses ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_arp_response_commit (const char *section_name,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct bmc_generated_arp_control ac;
  config_err_t ret;
  
  if ((ret = _get_bmc_generated_arp_control (state_data, &ac)) != CONFIG_ERR_SUCCESS)
    return ret;

  ac.bmc_generated_arp_responses = same (kv->value_input, "yes");
  return _set_bmc_generated_arp_control (state_data, &ac);
}

static config_err_t
gratuitous_arp_interval_checkout (const char *section_name,
				  struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint64_t val;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs)))
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "gratuitous_arp_interval", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output_int(kv, (uint8_t)val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
gratuitous_arp_interval_commit (const char *section_name,
				const struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval (state_data->dev,
                                                                         channel_number,
                                                                         atoi (kv->value_input),
                                                                         obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);

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

  if (config_section_add_key (lan_conf_misc_section,
                              "Enable_Gratuitous_ARPs",
                              "Possible values: Yes/No",
                              0,
                              enable_gratuitous_arps_checkout,
                              enable_gratuitous_arps_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_misc_section,
                              "Enable_ARP_Response",
                              "Possible values: Yes/No",
                              0,
                              enable_arp_response_checkout,
                              enable_arp_response_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (lan_conf_misc_section,
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
