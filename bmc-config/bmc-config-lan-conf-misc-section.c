/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

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

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience struct */
struct bmc_generated_arp_control
{
  uint8_t bmc_generated_gratuitous_arps;
  uint8_t bmc_generated_arp_responses;
};

static config_err_t
_get_bmc_generated_arp_control (bmc_config_state_data_t *state_data,
				const char *section_name,
                                struct bmc_generated_arp_control *ac)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (ac);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control (state_data->ipmi_ctx,
                                                                           channel_number,
                                                                           IPMI_GET_LAN_PARAMETER,
                                                                           IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                           IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "bmc_generated_gratuitous_arps", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bmc_generated_gratuitous_arps': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ac->bmc_generated_gratuitous_arps = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "bmc_generated_arp_responses", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bmc_generated_arp_responses': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ac->bmc_generated_arp_responses = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_bmc_generated_arp_control (bmc_config_state_data_t *state_data,
				const char *section_name,
                                struct bmc_generated_arp_control *ac)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (ac);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (state_data->ipmi_ctx,
                                                                           channel_number,
                                                                           ac->bmc_generated_gratuitous_arps,
                                                                           ac->bmc_generated_arp_responses,
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static config_err_t
enable_gratuitous_arps_checkout (const char *section_name,
                                 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data;
  struct bmc_generated_arp_control ac;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_bmc_generated_arp_control (state_data, section_name, &ac)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             ac.bmc_generated_gratuitous_arps ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
enable_gratuitous_arps_commit (const char *section_name,
                               const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data;
  struct bmc_generated_arp_control ac;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_bmc_generated_arp_control (state_data, section_name, &ac)) != CONFIG_ERR_SUCCESS)
    return (ret);

  ac.bmc_generated_gratuitous_arps = same (kv->value_input, "yes");
  return (_set_bmc_generated_arp_control (state_data, section_name, &ac));
}

static config_err_t
enable_arp_response_checkout (const char *section_name,
                              struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data;
  struct bmc_generated_arp_control ac;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_bmc_generated_arp_control (state_data, section_name, &ac)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             ac.bmc_generated_arp_responses ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
enable_arp_response_commit (const char *section_name,
                            const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data;
  struct bmc_generated_arp_control ac;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if ((ret = _get_bmc_generated_arp_control (state_data, section_name, &ac)) != CONFIG_ERR_SUCCESS)
    return (ret);

  ac.bmc_generated_arp_responses = same (kv->value_input, "yes");
  return (_set_bmc_generated_arp_control (state_data, section_name, &ac));
}

static config_err_t
gratuitous_arp_interval_checkout (const char *section_name,
                                  struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t gratuitous_arp_interval;
  uint64_t val;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         IPMI_GET_LAN_PARAMETER,
                                                                         IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                         IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "gratuitous_arp_interval", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'gratuitous_arp_interval': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  gratuitous_arp_interval = val;

  if (config_section_update_keyvalue_output_unsigned_int (state_data->pstate,
                                                          kv,
                                                          gratuitous_arp_interval) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
gratuitous_arp_interval_commit (const char *section_name,
                                const struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert (section_name);
  assert (kv);
  assert (arg);

  state_data = (bmc_config_state_data_t *)arg;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_gratuitous_arp_interval (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         atoi (kv->value_input),
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_gratuitous_arp_interval: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

struct config_section *
bmc_config_lan_conf_misc_section_get (bmc_config_state_data_t *state_data,
				      unsigned int config_flags,
				      int channel_index)
{
  struct config_section *section = NULL;
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
  char *section_name_base_str = "Lan_Conf_Misc";

  assert (state_data);

  if (!(section = config_section_multi_channel_create (state_data->pstate,
						       section_name_base_str,
						       section_comment,
						       NULL,
						       NULL,
						       config_flags,
						       channel_index,
						       state_data->lan_channel_numbers,
						       state_data->lan_channel_numbers_count)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Enable_Gratuitous_ARPs",
                              "Possible values: Yes/No",
                              0,
                              enable_gratuitous_arps_checkout,
                              enable_gratuitous_arps_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Enable_ARP_Response",
                              "Possible values: Yes/No",
                              0,
                              enable_arp_response_checkout,
                              enable_arp_response_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Gratuitous_ARP_Interval",
                              "Possible values: 0-255, in 500ms increments (e.g. 2 = 1000ms)",
                              0,
                              gratuitous_arp_interval_checkout,
                              gratuitous_arp_interval_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}
