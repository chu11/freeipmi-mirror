/*
 * Copyright (C) 2007-2013 FreeIPMI Core Team
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

#include "ipmi-pef-config.h"
#include "ipmi-pef-config-map.h"
#include "ipmi-pef-config-utils.h"
#include "ipmi-pef-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define IPMI_PEF_CONFIG_MAXIPADDRLEN 16
#define IPMI_PEF_CONFIG_MAXMACADDRLEN 24

/* convenience structs */
struct destination_type {
  uint8_t alert_destination_type;
  uint8_t alert_acknowledge;
  uint8_t alert_acknowledge_timeout;
  uint8_t alert_retries;
};

struct destination_addresses {
  uint8_t alert_gateway;
  char alert_ip[IPMI_PEF_CONFIG_MAXIPADDRLEN+1];
  char alert_mac[IPMI_PEF_CONFIG_MAXMACADDRLEN+1];
};

static config_err_t
_get_destination_type (ipmi_pef_config_state_data_t *state_data,
                       const char *section_name,
                       struct destination_type *dt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t destination_selector;

  assert (state_data);
  assert (section_name);
  assert (dt);

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_destination_type_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_destination_type (state_data->ipmi_ctx,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  destination_selector,
                                                                  IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_destination_type: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "destination_type", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'destination_type': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  dt->alert_destination_type = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "alert_acknowledge", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'alert_acknowledge': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  dt->alert_acknowledge = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "alert_acknowledge_timeout", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'alert_acknowledge_timeout': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  dt->alert_acknowledge_timeout = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "retries", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'retries': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  dt->alert_retries = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_destination_type (ipmi_pef_config_state_data_t *state_data,
                       const char *section_name,
                       struct destination_type *dt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t destination_selector;

  assert (state_data);
  assert (section_name);
  assert (dt);

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

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

  if (ipmi_cmd_set_lan_configuration_parameters_destination_type(state_data->ipmi_ctx,
                                                                 channel_number,
                                                                 destination_selector,
                                                                 dt->alert_destination_type,
                                                                 dt->alert_acknowledge,
                                                                 dt->alert_acknowledge_timeout,
                                                                 dt->alert_retries,
                                                                 obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_destination_type: %s\n",
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
alert_destination_type_checkout (const char *section_name,
                                 struct config_keyvalue *kv,
                                 void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             alert_destination_type_string (dt.alert_destination_type)) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_destination_type_commit (const char *section_name,
                               const struct config_keyvalue *kv,
                               void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  dt.alert_destination_type = alert_destination_type_number (kv->value_input);

  return (_set_destination_type (state_data,
                                 section_name,
                                 &dt));
}

static config_err_t
alert_acknowledge_checkout (const char *section_name,
                            struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             dt.alert_acknowledge ? "Yes" : "No") < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_acknowledge_commit (const char *section_name,
                          const struct config_keyvalue *kv,
                          void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  dt.alert_acknowledge = same (kv->value_input, "yes");

  return (_set_destination_type (state_data,
                                 section_name,
                                 &dt));
}

static config_err_t
alert_acknowledge_timeout_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output_unsigned_int (state_data->pstate,
                                                          kv,
                                                          dt.alert_acknowledge_timeout) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_acknowledge_timeout_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  dt.alert_acknowledge_timeout = atoi (kv->value_input);

  return (_set_destination_type (state_data,
                                 section_name,
                                 &dt));
}

static config_err_t
alert_retries_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output_unsigned_int (state_data->pstate,
                                                          kv,
                                                          dt.alert_retries) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_retries_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_type dt;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_type (state_data,
                                    section_name,
                                    &dt)) != CONFIG_ERR_SUCCESS)
    return (ret);

  dt.alert_retries = atoi (kv->value_input);

  return (_set_destination_type (state_data,
                                 section_name,
                                 &dt));
}

config_validate_t
alert_retries_validate (const char *section_name,
                        const char *key_name,
                        const char *value,
                        void *arg)
{
  assert (section_name);
  assert (key_name);
  assert (value);

  return (config_check_number_range (value, 0, IPMI_ALERT_RETRIES_MAX));
}

static config_err_t
_get_destination_addresses (ipmi_pef_config_state_data_t *state_data,
                            const char *section_name,
                            struct destination_addresses *da)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  uint8_t alert_ip_address_bytes[4];
  uint8_t alert_mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t destination_selector;

  assert (state_data);
  assert (section_name);
  assert (da);

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_destination_addresses(state_data->ipmi_ctx,
                                                                      channel_number,
                                                                      IPMI_GET_LAN_PARAMETER,
                                                                      destination_selector,
                                                                      IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_destination_addresses: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      if (config_is_config_param_non_fatal_error (state_data->ipmi_ctx,
                                                  obj_cmd_rs,
                                                  &ret))
        rv = ret;

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "gateway_selector", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'gateway_selector': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  da->alert_gateway = val;

  if (fiid_obj_get_data (obj_cmd_rs,
                         "alerting_ip_address",
                         alert_ip_address_bytes,
                         4) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'alerting_ip_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (da->alert_ip, '\0', IPMI_PEF_CONFIG_MAXIPADDRLEN+1);
  snprintf (da->alert_ip,
            IPMI_PEF_CONFIG_MAXIPADDRLEN,
            "%u.%u.%u.%u",
            alert_ip_address_bytes[0],
            alert_ip_address_bytes[1],
            alert_ip_address_bytes[2],
            alert_ip_address_bytes[3]);

  if (fiid_obj_get_data (obj_cmd_rs,
                         "alerting_mac_address",
                         alert_mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'alerting_mac_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (da->alert_mac, '\0', IPMI_PEF_CONFIG_MAXMACADDRLEN+1);
  snprintf (da->alert_mac,
            IPMI_PEF_CONFIG_MAXMACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            alert_mac_address_bytes[0],
            alert_mac_address_bytes[1],
            alert_mac_address_bytes[2],
            alert_mac_address_bytes[3],
            alert_mac_address_bytes[4],
            alert_mac_address_bytes[5]);

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_destination_addresses (ipmi_pef_config_state_data_t *state_data,
                            const char *section_name,
                            struct destination_addresses *da)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t alert_ip_address_val = 0;
  uint64_t alert_mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  uint8_t destination_selector;

  assert (state_data);
  assert (section_name);
  assert (da);

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

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

  if (config_ipv4_address_string2int (state_data->pstate,
                                      da->alert_ip,
                                      &alert_ip_address_val) < 0)
    goto cleanup;

  if (config_mac_address_string2int (state_data->pstate,
                                     da->alert_mac,
                                     &alert_mac_address_val) < 0)
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_destination_addresses (state_data->ipmi_ctx,
                                                                       channel_number,
                                                                       destination_selector,
                                                                       da->alert_gateway,
                                                                       alert_ip_address_val,
                                                                       alert_mac_address_val,
                                                                       obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_destination_addresses: %s\n",
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
alert_gateway_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_addresses da;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_addresses (state_data,
                                         section_name,
                                         &da)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             alert_gateway_string (da.alert_gateway)) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_gateway_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_addresses da;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_addresses (state_data,
                                         section_name,
                                         &da)) != CONFIG_ERR_SUCCESS)
    return (ret);

  da.alert_gateway = alert_gateway_number (kv->value_input);

  return (_set_destination_addresses (state_data,
                                      section_name,
                                      &da));
}

static config_err_t
alert_ip_address_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_addresses da;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_addresses (state_data,
                                         section_name,
                                         &da)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             da.alert_ip) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_ip_address_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_addresses da;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_addresses (state_data,
                                         section_name,
                                         &da)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* length checked earlier during validation */
  strcpy (da.alert_ip, kv->value_input);

  return (_set_destination_addresses (state_data,
                                      section_name,
                                      &da));
}

static config_err_t
alert_mac_address_checkout (const char *section_name,
                            struct config_keyvalue *kv,
                            void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_addresses da;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_addresses (state_data,
                                         section_name,
                                         &da)) != CONFIG_ERR_SUCCESS)
    return (ret);

  if (config_section_update_keyvalue_output (state_data->pstate,
                                             kv,
                                             da.alert_mac) < 0)
    return (CONFIG_ERR_FATAL_ERROR);

  return (CONFIG_ERR_SUCCESS);
}

static config_err_t
alert_mac_address_commit (const char *section_name,
                          const struct config_keyvalue *kv,
                          void *arg)
{
  ipmi_pef_config_state_data_t *state_data;
  struct destination_addresses da;
  config_err_t ret;

  assert (section_name);
  assert (kv);
  assert (arg);
  
  state_data = (ipmi_pef_config_state_data_t *)arg;

  if ((ret = _get_destination_addresses (state_data,
                                         section_name,
                                         &da)) != CONFIG_ERR_SUCCESS)
    return (ret);

  /* length checked earlier during validation */
  strcpy (da.alert_mac, kv->value_input);

  return (_set_destination_addresses (state_data,
                                      section_name,
                                      &da));
}

struct config_section *
ipmi_pef_config_lan_alert_destination_section_get (ipmi_pef_config_state_data_t *state_data,
						   unsigned int num,
						   unsigned int config_flags,
						   int channel_index)
{
  struct config_section *section = NULL;
  char section_name_base[CONFIG_MAX_SECTION_NAME_LEN];

  assert (state_data);

  snprintf (section_name_base, CONFIG_MAX_SECTION_NAME_LEN, "Lan_Alert_Destination_%u", num);

  if (!(section = config_section_multi_channel_create (state_data->pstate,
                                                       section_name_base,
                                                       NULL,
                                                       NULL,
                                                       NULL,
                                                       config_flags,
                                                       channel_index,
                                                       state_data->lan_channel_numbers,
                                                       state_data->lan_channel_numbers_count)))
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_Destination_Type",
                              "Possible values: PET_Trap/OEM1/OEM2",
                              0,
                              alert_destination_type_checkout,
                              alert_destination_type_commit,
                              alert_destination_type_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_Acknowledge",
                              "Possible values: Yes/No",
                              0,
                              alert_acknowledge_checkout,
                              alert_acknowledge_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_Acknowledge_Timeout",
                              "Give valid unsigned number in seconds",
                              0,
                              alert_acknowledge_timeout_checkout,
                              alert_acknowledge_timeout_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_Retries",
                              "Give valid unsigned number",
                              0,
                              alert_retries_checkout,
                              alert_retries_commit,
                              alert_retries_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_Gateway",
                              "Possible values: Default/Backup",
                              0,
                              alert_gateway_checkout,
                              alert_gateway_commit,
                              alert_gateway_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_IP_Address",
                              "Give valid IP address",
                              0,
                              alert_ip_address_checkout,
                              alert_ip_address_commit,
                              config_ip_address_validate) < 0)
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              section,
                              "Alert_MAC_Address",
                              "Give valid MAC address",
                              0,
                              alert_mac_address_checkout,
                              alert_mac_address_commit,
                              config_mac_address_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    config_section_destroy (section);
  return (NULL);
}

