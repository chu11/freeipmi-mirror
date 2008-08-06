/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
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
#include "tool-fiid-wrappers.h"

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

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control (state_data->ipmi_ctx,
                                                                           channel_number,
                                                                           IPMI_GET_LAN_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "bmc_generated_gratuitous_arps", &val);
  ac->bmc_generated_gratuitous_arps = val;

  _FIID_OBJ_GET (obj_cmd_rs, "bmc_generated_arp_responses", &val);
  ac->bmc_generated_arp_responses = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
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

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
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
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
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
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            ac.bmc_generated_gratuitous_arps ? "Yes" : "No") < 0)
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
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ac.bmc_generated_arp_responses ? "Yes" : "No") < 0)
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

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         IPMI_GET_LAN_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "gratuitous_arp_interval", &val);

  if (config_section_update_keyvalue_output_int(state_data->pstate,
                                                kv, 
                                                (uint8_t)val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
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

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_lan_set_lan_configuration_parameters_gratuitous_arp_interval (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         atoi (kv->value_input),
                                                                         obj_cmd_rs) < 0)
    {
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
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

  if (!(lan_conf_misc_section = config_section_create (state_data->pstate,
                                                       "Lan_Conf_Misc",
                                                       "Lan_Conf_Misc",
                                                       section_comment,
                                                       0,
                                                       NULL,
                                                       NULL)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_misc_section,
                              "Enable_Gratuitous_ARPs",
                              "Possible values: Yes/No",
                              0,
                              enable_gratuitous_arps_checkout,
                              enable_gratuitous_arps_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_misc_section,
                              "Enable_ARP_Response",
                              "Possible values: Yes/No",
                              0,
                              enable_arp_response_checkout,
                              enable_arp_response_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_misc_section,
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
    config_section_destroy(state_data->pstate, lan_conf_misc_section);
  return NULL;
}
