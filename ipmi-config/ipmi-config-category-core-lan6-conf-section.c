/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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
#include <netinet/in.h>
#include <arpa/inet.h>

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#ifdef INET6_ADDRSTRLEN
#define BMC_MAXIPV6ADDRLEN INET6_ADDRSTRLEN
#else
#define BMC_MAXIPV6ADDRLEN 46
#endif
#define BMC_MAXMACADDRLEN 24

struct ipv6_address_data {
  uint8_t source;
  uint8_t enable;               /* not used w/ dynamic */
  uint8_t address[IPMI_IPV6_BYTES];
  uint8_t address_prefix_length;
  uint8_t address_status;
};

struct router_address_configuration_control
{
  uint8_t enable_static_router_address;
  uint8_t enable_dynamic_router_address;
};

static uint8_t
get_address_status_number (const char *string)
{
  assert (string);

  if (same (string, "active"))
    return (IPMI_IPV6_ADDRESS_STATUS_ACTIVE);
  if (same (string, "disabled"))
    return (IPMI_IPV6_ADDRESS_STATUS_DISABLED);
  if (same (string, "pending"))
    return (IPMI_IPV6_ADDRESS_STATUS_PENDING);
  if (same (string, "failed"))
    return (IPMI_IPV6_ADDRESS_STATUS_FAILED);
  if (same (string, "deprecated"))
    return (IPMI_IPV6_ADDRESS_STATUS_DEPRECATED);
  if (same (string, "invalid"))
    return (IPMI_IPV6_ADDRESS_STATUS_INVALID);
  return (-1);
}

static char *
get_address_status_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_IPV6_ADDRESS_STATUS_ACTIVE:
      return "Active";
    case IPMI_IPV6_ADDRESS_STATUS_DISABLED:
      return "Disabled";
    case IPMI_IPV6_ADDRESS_STATUS_PENDING:
      return "Pending";
    case IPMI_IPV6_ADDRESS_STATUS_FAILED:
      return "Failed";
    case IPMI_IPV6_ADDRESS_STATUS_DEPRECATED:
      return "Deprecated";
    case IPMI_IPV6_ADDRESS_STATUS_INVALID:
      return "Invalid";
    }
  return "";
}

static ipmi_config_validate_t
ipv6_address_prefix_length_validate (ipmi_config_state_data_t *state_data,
                                     const char *section_name,
                                     const char *key_name,
                                     const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, IPMI_IPV6_PREFIX_LENGTH_MAX));
}

static ipmi_config_validate_t
ipv6_address_status_validate (ipmi_config_state_data_t *state_data,
                              const char *section_name,
                              const char *key_name,
                              const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (get_address_status_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

static ipmi_config_err_t
_get_number_of_ipv6_addresses (struct ipmi_config_state_data *state_data,
                               const char *section_name,
                               uint8_t *number_of_static_addresses,
                               uint8_t *number_of_dynamic_addresses)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint64_t val;
  uint8_t channel_number;

  assert (state_data);
  assert (number_of_static_addresses);
  assert (number_of_dynamic_addresses);

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_status_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ipv6_status (state_data->ipmi_ctx,
                                                             channel_number,
                                                             IPMI_GET_LAN_PARAMETER,
                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                             IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_status: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "static_address_max", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'static_address_max': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  *number_of_static_addresses = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "dynamic_address_max", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'dynamic_address_max': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  *number_of_dynamic_addresses = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_ipv6_ipv4_support (ipmi_config_state_data_t *state_data,
                        const char *section_name)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (state_data->ipv6_ipv4_support_initialized
      && state_data->ipv6_ipv4_support_channel_number == channel_number)
    goto out;

  state_data->ipv6_ipv4_support_initialized = 0;
  state_data->ipv6_ipv4_support_channel_number = 0;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_ipv4_support_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ipv6_ipv4_support (state_data->ipmi_ctx,
                                                                   channel_number,
                                                                   IPMI_GET_LAN_PARAMETER,
                                                                   IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                   IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED)
        {
          state_data->ipv6_ipv4_support_supports_ipv6_only = 0;
          state_data->ipv6_ipv4_support_supports_ipv6_and_ipv4_simultaneously = 0;
          state_data->ipv6_ipv4_support_supports_ipv6_destination_address_for_lan_alert = 0;
          state_data->ipv6_ipv4_support_channel_number = channel_number;
          state_data->ipv6_ipv4_support_initialized++;
          goto out;
        }

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_ipv4_support: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "supports_ipv6_only", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'supports_ipv6_only': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->ipv6_ipv4_support_supports_ipv6_only = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "supports_ipv6_and_ipv4_simultaneously", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'supports_ipv6_and_ipv4_simultaneously': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->ipv6_ipv4_support_supports_ipv6_and_ipv4_simultaneously = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "supports_ipv6_destination_address_for_lan_alert", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'supports_ipv6_destination_address_for_lan_alert': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->ipv6_ipv4_support_supports_ipv6_destination_address_for_lan_alert = val;

  state_data->ipv6_ipv4_support_channel_number = channel_number;
  state_data->ipv6_ipv4_support_initialized++;
 out:
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_ipv4_support_ipv6_only_checkout (ipmi_config_state_data_t *state_data,
                                      const char *section_name,
                                      struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  ret = _get_ipv6_ipv4_support (state_data,
  				section_name);

  if (ret != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  state_data->ipv6_ipv4_support_supports_ipv6_only ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_ipv4_support_ipv6_and_ipv4_simultaneously_checkout (ipmi_config_state_data_t *state_data,
							 const char *section_name,
							 struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  ret = _get_ipv6_ipv4_support (state_data,
  				section_name);

  if (ret != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  state_data->ipv6_ipv4_support_supports_ipv6_and_ipv4_simultaneously ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_ipv4_support_ipv6_destination_address_for_lan_alert_checkout (ipmi_config_state_data_t *state_data,
								   const char *section_name,
								   struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  ret = _get_ipv6_ipv4_support (state_data,
  				section_name);

  if (ret != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  state_data->ipv6_ipv4_support_supports_ipv6_destination_address_for_lan_alert ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_ipv4_addressing_enables_checkout (ipmi_config_state_data_t *state_data,
                                       const char *section_name,
                                       struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint64_t val;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_ipv4_addressing_enables_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ipv6_ipv4_addressing_enables (state_data->ipmi_ctx,
									      channel_number,
									      IPMI_GET_LAN_PARAMETER,
									      IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
									      IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
									      obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_ipv4_addressing_enables: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "enables",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'enables': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ipv6_ipv4_addressing_enables_string (val)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_ipv4_addressing_enables_commit (ipmi_config_state_data_t *state_data,
				     const char *section_name,
				     const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ipv6_ipv4_addressing_enables (state_data->ipmi_ctx,
                                                                              channel_number,
                                                                              ipv6_ipv4_addressing_enables_number (kv->value_input),
                                                                              obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_ipv6_ipv4_addressing_enables: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_ipv6_static_address (ipmi_config_state_data_t *state_data,
                          const char *section_name,
                          uint8_t set_selector,
                          struct ipv6_address_data *ipv6_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint64_t val;

  assert (state_data);
  assert (section_name);
  assert (ipv6_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_addresses_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ipv6_static_addresses (state_data->ipmi_ctx,
                                                                       channel_number,
                                                                       IPMI_GET_LAN_PARAMETER,
                                                                       set_selector,
                                                                       IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                       obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_static_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "source", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'source': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipv6_data->source = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "enable", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'enable': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipv6_data->enable = val;

  if (fiid_obj_get_data (obj_cmd_rs,
                         "address",
                         ipv6_data->address,
                         IPMI_IPV6_BYTES) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "address_prefix_length", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'address_prefix_length': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipv6_data->address_prefix_length = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "address_status", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'address_status': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipv6_data->address_status = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_ipv6_static_address (ipmi_config_state_data_t *state_data,
                          const char *section_name,
                          uint8_t set_selector,
                          struct ipv6_address_data *ipv6_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (ipv6_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ipv6_static_addresses (state_data->ipmi_ctx,
                                                                       channel_number,
                                                                       set_selector,
                                                                       ipv6_data->source,
                                                                       ipv6_data->enable,
                                                                       ipv6_data->address,
                                                                       ipv6_data->address_prefix_length,
                                                                       ipv6_data->address_status,
                                                                       obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_ipv6_static_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

uint8_t
get_static_address_source_number (const char *string)
{
  assert (string);

  if (same (string, "static"))
    return (IPMI_IPV6_ADDRESS_SOURCE_STATIC);
  return (-1);
}

char *
get_static_address_source_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_IPV6_ADDRESS_SOURCE_STATIC:
      return "Static";
    }
  return "";
}

static ipmi_config_err_t
ipv6_static_address_source_checkout (ipmi_config_state_data_t *state_data,
                                     const char *section_name,
                                     struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Source_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  get_static_address_source_string (ipv6_data.source)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_static_address_source_commit (ipmi_config_state_data_t *state_data,
                                   const char *section_name,
                                   const struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Source_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ipv6_data.source = get_static_address_source_number (kv->value_input);

  return (_set_ipv6_static_address (state_data,
                                    section_name,
                                    atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Source_")),
                                    &ipv6_data));
}

static ipmi_config_validate_t
ipv6_static_address_source_validate (ipmi_config_state_data_t *state_data,
                                      const char *section_name,
                                      const char *key_name,
                                      const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (get_static_address_source_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

static ipmi_config_err_t
ipv6_static_address_enable_checkout (ipmi_config_state_data_t *state_data,
                                     const char *section_name,
                                     struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Enable_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ipv6_data.enable ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_static_address_enable_commit (ipmi_config_state_data_t *state_data,
                                   const char *section_name,
                                   const struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Enable_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ipv6_data.enable = same (kv->value_input, "yes");

  return (_set_ipv6_static_address (state_data,
                                    section_name,
                                    atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Enable_")),
                                    &ipv6_data));
}

static ipmi_config_err_t
ipv6_static_address_checkout (ipmi_config_state_data_t *state_data,
                              const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  char address_str[BMC_MAXIPV6ADDRLEN + 1];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  memset (address_str, '\0', BMC_MAXIPV6ADDRLEN+1);
  if (!inet_ntop (AF_INET6,
                  ipv6_data.address,
                  address_str,
                  BMC_MAXIPV6ADDRLEN))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "inet_ntop: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  /* handle special case? !same (address_str, "::") */

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_static_address_commit (ipmi_config_state_data_t *state_data,
                            const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t ret;
  struct in6_addr addr;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (inet_pton (AF_INET6, kv->value_input, &addr) != 1)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "inet_pton: %s\n",
                         strerror (errno));
      return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }

  memcpy (ipv6_data.address, &addr, IPMI_IPV6_BYTES);

  return (_set_ipv6_static_address (state_data,
                                    section_name,
                                    atoi (kv->key->key_name + strlen ("IPv6_Static_Address_")),
                                    &ipv6_data));
}

static ipmi_config_err_t
ipv6_static_address_prefix_length_checkout (ipmi_config_state_data_t *state_data,
                                            const char *section_name,
                                            struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Prefix_Length_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               ipv6_data.address_prefix_length) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_static_address_prefix_length_commit (ipmi_config_state_data_t *state_data,
                                          const char *section_name,
                                          const struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Prefix_Length_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  ipv6_data.address_prefix_length = strtol (kv->value_input, NULL, 0);

  return (_set_ipv6_static_address (state_data,
                                    section_name,
                                    atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Prefix_Length_")),
                                    &ipv6_data));
}

static ipmi_config_err_t
ipv6_static_address_status_checkout (ipmi_config_state_data_t *state_data,
                                     const char *section_name,
                                     struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_static_address (state_data,
                                       section_name,
                                       atoi (kv->key->key_name + strlen ("IPv6_Static_Address_Status_")),
                                       &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  get_address_status_string (ipv6_data.address_status)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
_get_ipv6_dynamic_address (ipmi_config_state_data_t *state_data,
                           const char *section_name,
                           uint8_t set_selector,
                           struct ipv6_address_data *ipv6_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint64_t val;

  assert (state_data);
  assert (section_name);
  assert (ipv6_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_address_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ipv6_dynamic_address (state_data->ipmi_ctx,
                                                                      channel_number,
                                                                      IPMI_GET_LAN_PARAMETER,
                                                                      set_selector,
                                                                      IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                      obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_dynamic_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "source", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'source': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipv6_data->source = val;

  if (fiid_obj_get_data (obj_cmd_rs,
                         "address",
                         ipv6_data->address,
                         IPMI_IPV6_BYTES) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "address_prefix_length", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'address_prefix_length': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipv6_data->address_prefix_length = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "address_status", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'address_status': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ipv6_data->address_status = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static uint8_t
get_dynamic_address_source_number (const char *string)
{
  assert (string);

  if (same (string, "slaac"))
    return (IPMI_IPV6_ADDRESS_SOURCE_SLAAC);
  if (same (string, "dhcpv6"))
    return (IPMI_IPV6_ADDRESS_SOURCE_DHCPV6);
  return (-1);
}

static char *
get_dynamic_address_source_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_IPV6_ADDRESS_SOURCE_SLAAC:
      return "SLAAC";
    case IPMI_IPV6_ADDRESS_SOURCE_DHCPV6:
      return "DHCPv6";
    }
  return "";
}

static ipmi_config_err_t
ipv6_dynamic_address_source_checkout (ipmi_config_state_data_t *state_data,
                                      const char *section_name,
                                      struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_dynamic_address (state_data,
                                        section_name,
                                        atoi (kv->key->key_name + strlen ("IPv6_Dynamic_Address_Source_")),
                                        &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  get_dynamic_address_source_string (ipv6_data.source)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_validate_t
ipv6_dynamic_address_source_validate (ipmi_config_state_data_t *state_data,
                                      const char *section_name,
                                      const char *key_name,
                                      const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (get_dynamic_address_source_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

static ipmi_config_err_t
ipv6_dynamic_address_checkout (ipmi_config_state_data_t *state_data,
                               const char *section_name,
                               struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  char address_str[BMC_MAXIPV6ADDRLEN + 1];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_dynamic_address (state_data,
                                        section_name,
                                        atoi (kv->key->key_name + strlen ("IPv6_Dynamic_Address_")),
                                        &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  memset (address_str, '\0', BMC_MAXIPV6ADDRLEN+1);
  if (!inet_ntop (AF_INET6,
                  ipv6_data.address,
                  address_str,
                  BMC_MAXIPV6ADDRLEN))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "inet_ntop: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  /* handle special case? !same (address_str, "::") */

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_dynamic_address_prefix_length_checkout (ipmi_config_state_data_t *state_data,
                                             const char *section_name,
                                             struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_dynamic_address (state_data,
                                        section_name,
                                        atoi (kv->key->key_name + strlen ("IPv6_Dynamic_Address_Prefix_Length_")),
                                        &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               ipv6_data.address_prefix_length) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_dynamic_address_status_checkout (ipmi_config_state_data_t *state_data,
                                      const char *section_name,
                                      struct ipmi_config_keyvalue *kv)
{
  struct ipv6_address_data ipv6_data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipv6_dynamic_address (state_data,
                                        section_name,
                                        atoi (kv->key->key_name + strlen ("IPv6_Dynamic_Address_Status_")),
                                        &ipv6_data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  get_address_status_string (ipv6_data.address_status)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
_get_router_address_configuration_control (ipmi_config_state_data_t *state_data,
                                           const char *section_name,
                                           struct router_address_configuration_control *racc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint64_t val;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (racc);

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_router_address_configuration_control_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ipv6_router_address_configuration_control (state_data->ipmi_ctx,
                                                                                           channel_number,
                                                                                           IPMI_GET_LAN_PARAMETER,
                                                                                           IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                           IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                           obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_router_address_configuration_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "enable_static_router_address", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'enable_static_router_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  racc->enable_static_router_address = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "enable_dynamic_router_address", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'enable_dynamic_router_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  racc->enable_dynamic_router_address = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_router_address_configuration_control (ipmi_config_state_data_t *state_data,
                                           const char *section_name,
                                           struct router_address_configuration_control *racc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (racc);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ipv6_router_address_configuration_control (state_data->ipmi_ctx,
                                                                                           channel_number,
                                                                                           racc->enable_static_router_address,
                                                                                           racc->enable_dynamic_router_address,
                                                                                           obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_ipv6_router_address_configuration_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_address_enable_checkout (ipmi_config_state_data_t *state_data,
                                            const char *section_name,
                                            struct ipmi_config_keyvalue *kv)
{
  struct router_address_configuration_control racc;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  ret = _get_router_address_configuration_control (state_data,
                                                   section_name,
                                                   &racc);
  if (ret != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
                                                  racc.enable_static_router_address ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_address_enable_commit (ipmi_config_state_data_t *state_data,
                                          const char *section_name,
                                          const struct ipmi_config_keyvalue *kv)
{
  struct router_address_configuration_control racc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_router_address_configuration_control (state_data,
                                                        section_name,
                                                        &racc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  racc.enable_static_router_address = same (kv->value_input, "yes");

  return (_set_router_address_configuration_control (state_data,
                                                     section_name,
                                                     &racc));
}

static ipmi_config_err_t
ipv6_static_router_address_checkout (ipmi_config_state_data_t *state_data,
                                     const char *section_name,
                                     struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t address[IPMI_IPV6_BYTES];
  char address_str[BMC_MAXIPV6ADDRLEN + 1];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Address_"));

  if (num == 1)
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_ip_address_rs);
  else
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_ip_address_rs);

  if (!obj_cmd_rs)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (num == 1)
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_1_ip_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     IPMI_GET_LAN_PARAMETER,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                     obj_cmd_rs);
  else
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_2_ip_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     IPMI_GET_LAN_PARAMETER,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                     obj_cmd_rs);

  if (tmp < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_X_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "ipv6_router_ip_address",
                         address,
                         IPMI_IPV6_BYTES) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'ipv6_router_ip_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }


  memset (address_str, '\0', BMC_MAXIPV6ADDRLEN+1);
  if (!inet_ntop (AF_INET6,
                  address,
                  address_str,
                  BMC_MAXIPV6ADDRLEN))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "inet_ntop: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  /* handle special case? !same (address_str, "::") */

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_address_commit (ipmi_config_state_data_t *state_data,
                                   const char *section_name,
                                   const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  struct in6_addr addr;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Address_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (inet_pton (AF_INET6, kv->value_input, &addr) != 1)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "inet_pton: %s\n",
                         strerror (errno));
      return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }

  if (num == 1)
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_1_ip_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     (uint8_t *)&addr,
                                                                                     obj_cmd_rs);
  else
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_2_ip_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     (uint8_t *)&addr,
                                                                                     obj_cmd_rs);

  if (tmp)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_X_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_mac_address_checkout (ipmi_config_state_data_t *state_data,
                                         const char *section_name,
                                         struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char router_mac_address_str[BMC_MAXMACADDRLEN+1];
  uint8_t router_mac_address_bytes[6];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Mac_Address_"));

  if (num == 1)
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_mac_address_rs);
  else
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_mac_address_rs);

  if (!obj_cmd_rs)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (num == 1)
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_1_mac_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     IPMI_GET_LAN_PARAMETER,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                     obj_cmd_rs);
  else
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_2_mac_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     IPMI_GET_LAN_PARAMETER,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                     IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                     obj_cmd_rs);

  if (tmp < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_X_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "router_mac_address",
                         router_mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'router_mac_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (router_mac_address_str, '\0', BMC_MAXMACADDRLEN+1);
  snprintf (router_mac_address_str,
            BMC_MAXMACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            router_mac_address_bytes[0],
            router_mac_address_bytes[1],
            router_mac_address_bytes[2],
            router_mac_address_bytes[3],
            router_mac_address_bytes[4],
            router_mac_address_bytes[5]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  router_mac_address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_mac_address_commit (ipmi_config_state_data_t *state_data,
                                       const char *section_name,
                                       const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint64_t mac_address_val = 0;
  uint8_t channel_number;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (mac_address_string2int (state_data,
                              kv->value_input,
                              &mac_address_val) < 0)
    goto cleanup;

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Mac_Address_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (num == 1)
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_1_mac_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     mac_address_val,
                                                                                     obj_cmd_rs);
  else
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_2_mac_address (state_data->ipmi_ctx,
                                                                                     channel_number,
                                                                                     mac_address_val,
                                                                                     obj_cmd_rs);

  if (tmp)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_X_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_prefix_length_checkout (ipmi_config_state_data_t *state_data,
                                           const char *section_name,
                                           struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint64_t val;
  uint8_t prefix_length;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Prefix_Length_"));

  if (num == 1)
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_prefix_length_rs);
  else
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_prefix_length_rs);

  if (!obj_cmd_rs)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (num == 1)
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_1_prefix_length (state_data->ipmi_ctx,
                                                                                        channel_number,
                                                                                        IPMI_GET_LAN_PARAMETER,
                                                                                        IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                        IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                        obj_cmd_rs);
  else
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_2_prefix_length (state_data->ipmi_ctx,
                                                                                        channel_number,
                                                                                        IPMI_GET_LAN_PARAMETER,
                                                                                        IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                        IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                        obj_cmd_rs);

  if (tmp < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_X_prefix_length: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "prefix_length",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'prefix_length': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  prefix_length = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               prefix_length) < 0)

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_prefix_length_commit (ipmi_config_state_data_t *state_data,
                                         const char *section_name,
                                         const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint8_t prefix_length;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Prefix_Length_"));

  prefix_length = atoi (kv->value_input);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (num == 1)
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_1_prefix_length (state_data->ipmi_ctx,
                                                                                        channel_number,
                                                                                        prefix_length,
                                                                                        obj_cmd_rs);
  else
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_2_prefix_length (state_data->ipmi_ctx,
                                                                                        channel_number,
                                                                                        prefix_length,
                                                                                        obj_cmd_rs);

  if (tmp)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_X_prefix_length: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_prefix_value_checkout (ipmi_config_state_data_t *state_data,
                                          const char *section_name,
                                          struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t prefix_value[IPMI_IPV6_BYTES];
  char prefix_value_str[BMC_MAXIPV6ADDRLEN + 1];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Prefix_Value_"));

  if (num == 1)
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_1_prefix_value_rs);
  else
    obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_static_router_2_prefix_value_rs);

  if (!obj_cmd_rs)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (num == 1)
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_1_prefix_value (state_data->ipmi_ctx,
                                                                                       channel_number,
                                                                                       IPMI_GET_LAN_PARAMETER,
                                                                                       IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                       IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                       obj_cmd_rs);
  else
    tmp = ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_2_prefix_value (state_data->ipmi_ctx,
                                                                                       channel_number,
                                                                                       IPMI_GET_LAN_PARAMETER,
                                                                                       IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                       IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                                       obj_cmd_rs);

  if (tmp < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_static_router_X_prefix_value: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "prefix_value",
                         prefix_value,
                         IPMI_IPV6_BYTES) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'prefix_value': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }


  memset (prefix_value_str, '\0', BMC_MAXIPV6ADDRLEN+1);
  if (!inet_ntop (AF_INET6,
                  prefix_value,
                  prefix_value_str,
                  BMC_MAXIPV6ADDRLEN))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "inet_ntop: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  /* handle special case? !same (prefix_value_str, "::") */

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  prefix_value_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_router_prefix_value_commit (ipmi_config_state_data_t *state_data,
                                        const char *section_name,
                                        const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  struct in6_addr addr;
  int num;
  int tmp;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* router 1 or 2 */
  num = atoi (kv->key->key_name + strlen ("IPv6_Static_Router_Prefix_Value_"));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data,
                                     section_name,
                                     &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (inet_pton (AF_INET6, kv->value_input, &addr) != 1)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "inet_pton: %s\n",
                         strerror (errno));
      return (IPMI_CONFIG_ERR_FATAL_ERROR);
    }

  if (num == 1)
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_1_prefix_value (state_data->ipmi_ctx,
                                                                                       channel_number,
                                                                                       (uint8_t *)&addr,
                                                                                       obj_cmd_rs);
  else
    tmp = ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_2_prefix_value (state_data->ipmi_ctx,
                                                                                       channel_number,
                                                                                       (uint8_t *)&addr,
                                                                                       obj_cmd_rs);

  if (tmp)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_ipv6_static_router_X_prefix_value: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

struct ipmi_config_section *
ipmi_config_core_lan6_conf_section_get (ipmi_config_state_data_t *state_data,
                                        unsigned int config_flags,
                                        int channel_index)
{
  struct ipmi_config_section *section = NULL;
  char key_name[IPMI_CONFIG_MAX_KEY_NAME_LEN];
  char *section_comment =
    "In the Lan6_Conf section, typical networking configuration is setup.  "
    "Most users will choose to set an address in  \"IPv6_Static_Address\" "
    "and set the appropriate routing for the machine.";
  char *section_name_base_str = "Lan6_Conf";
  unsigned int verbose_option_config_flags = 0;
  uint8_t number_of_static_addresses = 0;
  uint8_t number_of_dynamic_addresses = 0;
  ipmi_config_err_t ret;
  unsigned int i;

  assert (state_data);

  if (!state_data->prog_data->args->verbose_count)
    verbose_option_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;

  if (!(section = ipmi_config_section_multi_channel_create (state_data,
                                                            section_name_base_str,
                                                            section_comment,
                                                            NULL,
                                                            NULL,
                                                            config_flags,
                                                            channel_index,
                                                            state_data->lan_channel_numbers,
                                                            state_data->lan_channel_numbers_count)))
    goto cleanup;


  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Supports_IPv6_Only",
                                   "READ-ONLY",
                                   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
                                   ipv6_ipv4_support_ipv6_only_checkout,
                                   read_only_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Supports_IPv6_And_IPv4_Simultaneously",
                                   "READ-ONLY",
                                   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
				   ipv6_ipv4_support_ipv6_and_ipv4_simultaneously_checkout,
                                   read_only_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Supports_IPv6_Destination_Address_For_Lan_Alert",
                                   "READ-ONLY",
                                   verbose_option_config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
				   ipv6_ipv4_support_ipv6_destination_address_for_lan_alert_checkout,
                                   read_only_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_IPv4_Addressing_Enables",
                                   "Possible values: IPv4-Only/IPv6-Only/IPv4-and-IPv6",
                                   0,
                                   ipv6_ipv4_addressing_enables_checkout,
                                   ipv6_ipv4_addressing_enables_commit,
                                   ipv6_ipv4_addressing_enables_validate) < 0)
    goto cleanup;

  /* Do not return error if this returns != IPMI_CONFIG_ERR_SUCCESS,
   * as the motherboard may report it doesn't support IPv6 above.  If
   * this fails, we just assume all other IPv6 configuration below
   * isn't available.
   */
  if ((ret = _get_number_of_ipv6_addresses (state_data,
                                            section->section_name,
                                            &number_of_static_addresses,
                                            &number_of_dynamic_addresses)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      if (ret == IPMI_CONFIG_ERR_FATAL_ERROR
          || state_data->prog_data->args->common_args.debug)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Unable to get number of addresses\n");
          return (NULL);
        }
      goto done;
    }

  for (i = 0; i < number_of_static_addresses; i++)
    {
      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Static_Address_Source_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "Possible values: Static",
                                       0,
                                       ipv6_static_address_source_checkout,
                                       ipv6_static_address_source_commit,
                                       ipv6_static_address_source_validate) < 0)
        goto cleanup;

      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Static_Address_Enable_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "Possible values: Yes/No",
                                       0,
                                       ipv6_static_address_enable_checkout,
                                       ipv6_static_address_enable_commit,
                                       yes_no_validate) < 0)
        goto cleanup;

      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Static_Address_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "Give valid IPv6 address",
                                       0,
                                       ipv6_static_address_checkout,
                                       ipv6_static_address_commit,
                                       ipv6_address_validate) < 0)
        goto cleanup;

      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Static_Address_Prefix_Length_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "Give valid prefix length",
                                       0,
                                       ipv6_static_address_prefix_length_checkout,
                                       ipv6_static_address_prefix_length_commit,
                                       ipv6_address_prefix_length_validate) < 0)
        goto cleanup;

      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Static_Address_Status_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "READ-ONLY",
                                       IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
                                       ipv6_static_address_status_checkout,
                                       read_only_commit,
                                       ipv6_address_status_validate) < 0)
        goto cleanup;
    }

  for (i = 0; i < number_of_dynamic_addresses; i++)
    {
      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Dynamic_Address_Source_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "READ-ONLY",
                                       IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
                                       ipv6_dynamic_address_source_checkout,
                                       read_only_commit,
                                       ipv6_dynamic_address_source_validate) < 0)
        goto cleanup;

      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Dynamic_Address_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "READ-ONLY",
                                       IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
                                       ipv6_dynamic_address_checkout,
                                       read_only_commit,
                                       ipv6_address_validate) < 0)
        goto cleanup;

      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Dynamic_Address_Prefix_Length_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "READ-ONLY",
                                       IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
                                       ipv6_dynamic_address_prefix_length_checkout,
                                       read_only_commit,
                                       ipv6_address_prefix_length_validate) < 0)
        goto cleanup;

      snprintf (key_name, IPMI_CONFIG_MAX_KEY_NAME_LEN, "IPv6_Dynamic_Address_Status_%u", i);

      if (ipmi_config_section_add_key (state_data,
                                       section,
                                       key_name,
                                       "READ-ONLY",
                                       IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT | IPMI_CONFIG_READABLE_ONLY,
                                       ipv6_dynamic_address_status_checkout,
                                       read_only_commit,
                                       ipv6_address_status_validate) < 0)
        goto cleanup;
    }

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Address_Enable",
                                   "Possible values: Yes/No",
                                   0,
                                   ipv6_static_router_address_enable_checkout,
                                   ipv6_static_router_address_enable_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Address_1",
                                   "Give valid IPv6 address",
                                   0,
                                   ipv6_static_router_address_checkout,
                                   ipv6_static_router_address_commit,
                                   ipv6_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Mac_Address_1",
                                   "Give valid IPv6 mac address",
                                   0,
                                   ipv6_static_router_mac_address_checkout,
                                   ipv6_static_router_mac_address_commit,
                                   mac_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Prefix_Length_1",
                                   "Give valid IPv6 prefix length",
                                   0,
                                   ipv6_static_router_prefix_length_checkout,
                                   ipv6_static_router_prefix_length_commit,
                                   ipv6_address_prefix_length_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Prefix_Value_1",
                                   "Give valid IPv6 prefix value",
                                   0,
                                   ipv6_static_router_prefix_value_checkout,
                                   ipv6_static_router_prefix_value_commit,
                                   ipv6_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Address_2",
                                   "Give valid IPv6 address",
                                   0,
                                   ipv6_static_router_address_checkout,
                                   ipv6_static_router_address_commit,
                                   ipv6_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Mac_Address_2",
                                   "Give valid IPv6 mac address",
                                   0,
                                   ipv6_static_router_mac_address_checkout,
                                   ipv6_static_router_mac_address_commit,
                                   mac_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Prefix_Length_2",
                                   "Give valid IPv6 prefix length",
                                   0,
                                   ipv6_static_router_prefix_length_checkout,
                                   ipv6_static_router_prefix_length_commit,
                                   ipv6_address_prefix_length_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Router_Prefix_Value_2",
                                   "Give valid IPv6 prefix value",
                                   0,
                                   ipv6_static_router_prefix_value_checkout,
                                   ipv6_static_router_prefix_value_commit,
                                   ipv6_address_validate) < 0)
    goto cleanup;

 done:
  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}
