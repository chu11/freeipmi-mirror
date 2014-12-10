/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define BMC_MAXIPADDRLEN 16
#define BMC_MAXMACADDRLEN 24

/* convenience struct */
struct vlan_id
{
  uint16_t vlan_id;
  uint8_t vlan_id_enable;
};

struct ipv4_header_parameters
{
  uint8_t time_to_live;
  uint8_t flags;
  uint8_t type_of_service;
  uint8_t precedence;
};

static ipmi_config_err_t
ip_address_source_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_source;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address_source (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_ip_address_source: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "ip_address_source", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'ip_address_source': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ip_address_source = val;
  
  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ip_address_source_string (ip_address_source)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ip_address_source_commit (ipmi_config_state_data_t *state_data,
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

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address_source (state_data->ipmi_ctx,
                                                                   channel_number,
                                                                   ip_address_source_number (kv->value_input),
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
                         "ipmi_cmd_set_lan_configuration_parameters_ip_address_source: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ip_address_checkout (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char ip_address_str[BMC_MAXIPADDRLEN + 1];
  uint8_t ip_address_bytes[4];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ip_address_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "ip_address",
                         ip_address_bytes,
                         4) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'ip_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (ip_address_str, '\0', BMC_MAXIPADDRLEN+1);
  snprintf (ip_address_str,
            BMC_MAXIPADDRLEN,
            "%u.%u.%u.%u",
            ip_address_bytes[0],
            ip_address_bytes[1],
            ip_address_bytes[2],
            ip_address_bytes[3]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ip_address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ip_address_commit (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (ipv4_address_string2int (state_data,
                               kv->value_input,
                               &ip_address_val) < 0)
    goto cleanup;

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

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address (state_data->ipmi_ctx,
                                                            channel_number,
                                                            ip_address_val,
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
                         "ipmi_cmd_set_lan_configuration_parameters_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
mac_address_checkout (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[BMC_MAXMACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_mac_address_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "mac_address",
                         mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'mac_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (mac_address_str, '\0', BMC_MAXMACADDRLEN+1);
  snprintf (mac_address_str,
            BMC_MAXMACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            mac_address_bytes[0],
            mac_address_bytes[1],
            mac_address_bytes[2],
            mac_address_bytes[3],
            mac_address_bytes[4],
            mac_address_bytes[5]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  mac_address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static ipmi_config_err_t
mac_address_commit (ipmi_config_state_data_t *state_data,
		    const char *section_name,
                    const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (mac_address_string2int (state_data,
                              kv->value_input,
                              &mac_address_val) < 0)
    goto cleanup;

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

  if (ipmi_cmd_set_lan_configuration_parameters_mac_address (state_data->ipmi_ctx,
                                                             channel_number,
                                                             mac_address_val,
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
                         "ipmi_cmd_set_lan_configuration_parameters_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
subnet_mask_checkout (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char subnet_mask_str[BMC_MAXIPADDRLEN + 1];
  uint8_t subnet_mask_bytes[4];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_subnet_mask (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_subnet_mask: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "subnet_mask",
                         subnet_mask_bytes,
                         4) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'subnet_mask': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (subnet_mask_str, '\0', BMC_MAXIPADDRLEN + 1);
  snprintf (subnet_mask_str,
            BMC_MAXIPADDRLEN,
            "%u.%u.%u.%u",
            subnet_mask_bytes[0],
            subnet_mask_bytes[1],
            subnet_mask_bytes[2],
            subnet_mask_bytes[3]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  subnet_mask_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
subnet_mask_commit (ipmi_config_state_data_t *state_data,
		    const char *section_name,
                    const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t subnet_mask_val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (ipv4_address_string2int (state_data,
                               kv->value_input,
                               &subnet_mask_val) < 0)
    goto cleanup;

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

  if (ipmi_cmd_set_lan_configuration_parameters_subnet_mask (state_data->ipmi_ctx,
                                                             channel_number,
                                                             subnet_mask_val,
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
                         "ipmi_cmd_set_lan_configuration_parameters_subnet_mask: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
default_gateway_address_checkout (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char ip_address_str[BMC_MAXIPADDRLEN + 1];
  uint8_t ip_address_bytes[4];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_address (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_default_gateway_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "ip_address",
                         ip_address_bytes,
                         4) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'ip_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (ip_address_str, '\0', BMC_MAXIPADDRLEN + 1);
  snprintf (ip_address_str,
            BMC_MAXIPADDRLEN,
            "%u.%u.%u.%u",
            ip_address_bytes[0],
            ip_address_bytes[1],
            ip_address_bytes[2],
            ip_address_bytes[3]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ip_address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static ipmi_config_err_t
default_gateway_address_commit (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (ipv4_address_string2int (state_data,
                               kv->value_input,
                               &ip_address_val) < 0)
    goto cleanup;

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

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_address (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         ip_address_val,
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
                         "ipmi_cmd_set_lan_configuration_parameters_default_gateway_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
default_gateway_mac_address_checkout (ipmi_config_state_data_t *state_data,
				      const char *section_name,
                                      struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[BMC_MAXMACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "mac_address",
                         mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'mac_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (mac_address_str, '\0', BMC_MAXMACADDRLEN + 1);
  snprintf (mac_address_str,
            BMC_MAXMACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            mac_address_bytes[0],
            mac_address_bytes[1],
            mac_address_bytes[2],
            mac_address_bytes[3],
            mac_address_bytes[4],
            mac_address_bytes[5]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  mac_address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static ipmi_config_err_t
default_gateway_mac_address_commit (ipmi_config_state_data_t *state_data,
				    const char *section_name,
                                    const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (mac_address_string2int (state_data,
                              kv->value_input,
                              &mac_address_val) < 0)
    goto cleanup;

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

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address (state_data->ipmi_ctx,
                                                                             channel_number,
                                                                             mac_address_val,
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
                         "ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
backup_gateway_address_checkout (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char ip_address_str[BMC_MAXIPADDRLEN + 1];
  uint8_t ip_address_bytes[4];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "ip_address",
                         ip_address_bytes,
                         4) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'ip_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (ip_address_str, '\0', BMC_MAXIPADDRLEN+1);
  snprintf (ip_address_str,
            BMC_MAXIPADDRLEN,
            "%u.%u.%u.%u",
            ip_address_bytes[0],
            ip_address_bytes[1],
            ip_address_bytes[2],
            ip_address_bytes[3]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ip_address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
backup_gateway_address_commit (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (ipv4_address_string2int (state_data,
                               kv->value_input,
                               &ip_address_val) < 0)
    goto cleanup;

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

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address (state_data->ipmi_ctx,
                                                                        channel_number,
                                                                        ip_address_val,
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
                         "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
backup_gateway_mac_address_checkout (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[BMC_MAXMACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "mac_address",
                         mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'mac_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (mac_address_str, '\0', BMC_MAXMACADDRLEN+1);
  snprintf (mac_address_str,
            BMC_MAXMACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            mac_address_bytes[0],
            mac_address_bytes[1],
            mac_address_bytes[2],
            mac_address_bytes[3],
            mac_address_bytes[4],
            mac_address_bytes[5]);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  mac_address_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
backup_gateway_mac_address_commit (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (mac_address_string2int (state_data,
                              kv->value_input,
                              &mac_address_val) < 0)
    goto cleanup;

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

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (state_data->ipmi_ctx,
                                                                            channel_number,
                                                                            mac_address_val,
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
                         "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_vlan_id (ipmi_config_state_data_t *state_data,
              const char *section_name,
              struct vlan_id *vi)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (vi);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_id (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_vlan_id: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "vlan_id", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'vlan_id': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  vi->vlan_id = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "vlan_id_enable", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'vlan_id_enable': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  vi->vlan_id_enable = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_vlan_id (ipmi_config_state_data_t *state_data,
              const char *section_name,
              struct vlan_id *vi)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (vi);

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

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_id (state_data->ipmi_ctx,
                                                         channel_number,
                                                         vi->vlan_id,
                                                         vi->vlan_id_enable,
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
                         "ipmi_cmd_set_lan_configuration_parameters_vlan_id: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
vlan_id_checkout (ipmi_config_state_data_t *state_data,
		  const char *section_name,
                  struct ipmi_config_keyvalue *kv)
{
  struct vlan_id vi;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_vlan_id (state_data, section_name, &vi)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               vi.vlan_id) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
vlan_id_commit (ipmi_config_state_data_t *state_data,
		const char *section_name,
                const struct ipmi_config_keyvalue *kv)
{
  struct vlan_id vi;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_vlan_id (state_data, section_name, &vi)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  vi.vlan_id = atoi (kv->value_input);

  return (_set_vlan_id (state_data, section_name, &vi));
}

static ipmi_config_validate_t
vlan_id_validate (ipmi_config_state_data_t *state_data,
		  const char *section_name,
                  const char *key_name,
                  const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, 4095));
}

static ipmi_config_err_t
vlan_id_enable_checkout (ipmi_config_state_data_t *state_data,
			 const char *section_name,
                         struct ipmi_config_keyvalue *kv)
{
  struct vlan_id vi;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_vlan_id (state_data, section_name, &vi)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  vi.vlan_id_enable ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
vlan_id_enable_commit (ipmi_config_state_data_t *state_data,
		       const char *section_name,
                       const struct ipmi_config_keyvalue *kv)
{
  struct vlan_id vi;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_vlan_id (state_data, section_name, &vi)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  vi.vlan_id_enable = same (kv->value_input, "yes");

  return (_set_vlan_id (state_data, section_name, &vi));
}

static ipmi_config_err_t
vlan_priority_checkout (ipmi_config_state_data_t *state_data,
			const char *section_name,
                        struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t vlan_priority;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_priority (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_vlan_priority: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "vlan_priority", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'vlan_priority': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  vlan_priority = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               val) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
vlan_priority_commit (ipmi_config_state_data_t *state_data,
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

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_priority (state_data->ipmi_ctx,
                                                               channel_number,
                                                               atoi (kv->value_input),
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
                         "ipmi_cmd_set_lan_configuration_parameters_vlan_priority: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_ipv4_header_parameters (ipmi_config_state_data_t *state_data,
                             const char *section_name,
                             struct ipv4_header_parameters *ihp)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (ihp);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv4_header_parameters_rs)))
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
  
  if (ipmi_cmd_get_lan_configuration_parameters_ipv4_header_parameters (state_data->ipmi_ctx,
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
                         "ipmi_cmd_get_lan_configuration_parameters_ipv4_header_parameters: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs, "time_to_live", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'time_to_live': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ihp->time_to_live = val;
  
  if (FIID_OBJ_GET (obj_cmd_rs, "flags", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'flags': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ihp->flags = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "type_of_service", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'type_of_service': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ihp->type_of_service = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "precedence", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'precedence': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ihp->precedence = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_ipv4_header_parameters (ipmi_config_state_data_t *state_data,
                             const char *section_name,
                             struct ipv4_header_parameters *ihp)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (ihp);

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

  if (ipmi_cmd_set_lan_configuration_parameters_ipv4_header_parameters (state_data->ipmi_ctx,
                                                                        channel_number,
                                                                        ihp->time_to_live,
                                                                        ihp->flags,
                                                                        ihp->type_of_service,
                                                                        ihp->precedence,
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
                         "ipmi_cmd_set_lan_configuration_parameters_ipv4_header_parameters: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv4_header_time_to_live_checkout (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_hex (state_data,
                                                      kv,
                                                      ihp.time_to_live) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
ipv4_header_time_to_live_commit (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  ihp.time_to_live = strtol (kv->value_input, NULL, 0);

  return (_set_ipv4_header_parameters (state_data, section_name, &ihp));
}

static ipmi_config_err_t
ipv4_header_flags_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_hex (state_data,
                                                      kv,
                                                      ihp.flags) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
ipv4_header_flags_commit (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  ihp.flags = strtol (kv->value_input, NULL, 0);

  return (_set_ipv4_header_parameters (state_data, section_name, &ihp));
}

static ipmi_config_err_t
ipv4_header_type_of_service_checkout (ipmi_config_state_data_t *state_data,
				      const char *section_name,
                                      struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_hex (state_data,
                                                      kv,
                                                      ihp.type_of_service) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
ipv4_header_type_of_service_commit (ipmi_config_state_data_t *state_data,
				    const char *section_name,
                                    const struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  ihp.type_of_service = strtol (kv->value_input, NULL, 0);

  return (_set_ipv4_header_parameters (state_data, section_name, &ihp));
}

static ipmi_config_err_t
ipv4_header_precedence_checkout (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_hex (state_data,
                                                      kv,
                                                      ihp.precedence) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
ipv4_header_precedence_commit (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const struct ipmi_config_keyvalue *kv)
{
  struct ipv4_header_parameters ihp;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if ((ret = _get_ipv4_header_parameters (state_data, section_name, &ihp)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  /* previously checked for correctness, so no error check */
  ihp.precedence = strtol (kv->value_input, NULL, 0);

  return (_set_ipv4_header_parameters (state_data, section_name, &ihp));
}

static ipmi_config_err_t
primary_rmcp_port_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint16_t primary_rmcp_port_number;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_primary_rmcp_port_number_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_primary_rmcp_port_number (state_data->ipmi_ctx,
                                                                          channel_number,
                                                                          IPMI_GET_SOL_PARAMETER,
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
                         "ipmi_cmd_get_lan_configuration_parameters_primary_rmcp_port_number: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs, "primary_rmcp_port_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'primary_rmcp_port_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  primary_rmcp_port_number = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               primary_rmcp_port_number) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
primary_rmcp_port_commit (ipmi_config_state_data_t *state_data,
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
  
  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_set_lan_configuration_parameters_primary_rmcp_port_number (state_data->ipmi_ctx,
                                                                          channel_number,
                                                                          atoi (kv->value_input),
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
                         "ipmi_cmd_set_lan_configuration_parameters_primary_rmcp_port_number: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }
  
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
secondary_rmcp_port_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint16_t secondary_rmcp_port_number;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  
  assert (state_data);
  assert (section_name);
  assert (kv);
  
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_secondary_rmcp_port_number_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_secondary_rmcp_port_number (state_data->ipmi_ctx,
                                                                            channel_number,
                                                                            IPMI_GET_SOL_PARAMETER,
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
                         "ipmi_cmd_get_lan_configuration_parameters_secondary_rmcp_port_number: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs, "secondary_rmcp_port_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'secondary_rmcp_port_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  secondary_rmcp_port_number = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               secondary_rmcp_port_number) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
secondary_rmcp_port_commit (ipmi_config_state_data_t *state_data,
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
  
  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_set_lan_configuration_parameters_secondary_rmcp_port_number (state_data->ipmi_ctx,
                                                                            channel_number,
                                                                            atoi (kv->value_input),
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
                         "ipmi_cmd_set_lan_configuration_parameters_secondary_rmcp_port_number: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }
  
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

struct ipmi_config_section *
ipmi_config_core_lan_conf_section_get (ipmi_config_state_data_t *state_data,
				       unsigned int config_flags,
				       int channel_index)
{
  struct ipmi_config_section *section = NULL;
  char *section_comment =
    "In the Lan_Conf section, typical networking configuration is setup.  "
    "Most users will choose to set \"Static\" for the \"IP_Address_Source\" "
    "and set the appropriate \"IP_Address\", \"MAC_Address\", "
    "\"Subnet_Mask\", etc. for the machine.";
  char *section_name_base_str = "Lan_Conf";
  unsigned int verbose_option_config_flags = 0;

  assert (state_data);

  /* vlan and ipv4 header parameters not checked out by default */

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
                                   "IP_Address_Source",
                                   "Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
                                   0,
                                   ip_address_source_checkout,
                                   ip_address_source_commit,
                                   ip_address_source_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IP_Address",
                                   "Give valid IP address",
                                   0,
                                   ip_address_checkout,
                                   ip_address_commit,
                                   ip_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "MAC_Address",
                                   "Give valid MAC address",
                                   0,
                                   mac_address_checkout,
                                   mac_address_commit,
                                   mac_address_validate) < 0)
    goto cleanup;

  /* TODO: XXX: checking valid netmask is not same as checking valid IP? */
  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Subnet_Mask",
                                   "Give valid Subnet Mask",
                                   0,
                                   subnet_mask_checkout,
                                   subnet_mask_commit,
                                   ip_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Default_Gateway_IP_Address",
                                   "Give valid IP address",
                                   0,
                                   default_gateway_address_checkout,
                                   default_gateway_address_commit,
                                   ip_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Default_Gateway_MAC_Address",
                                   "Give valid MAC address",
                                   0,
                                   default_gateway_mac_address_checkout,
                                   default_gateway_mac_address_commit,
                                   mac_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Backup_Gateway_IP_Address",
                                   "Give valid IP address",
                                   0,
                                   backup_gateway_address_checkout,
                                   backup_gateway_address_commit,
                                   ip_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Backup_Gateway_MAC_Address",
                                   "Give valid MAC address",
                                   0,
                                   backup_gateway_mac_address_checkout,
                                   backup_gateway_mac_address_commit,
                                   mac_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Vlan_id",
                                   "Give valid unsigned number",
                                   verbose_option_config_flags,
                                   vlan_id_checkout,
                                   vlan_id_commit,
                                   vlan_id_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Vlan_Id_Enable",
                                   "Possible values: Yes/No",
                                   verbose_option_config_flags,
                                   vlan_id_enable_checkout,
                                   vlan_id_enable_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Vlan_Priority",
                                   "Give valid unsigned number",
                                   verbose_option_config_flags,
                                   vlan_priority_checkout,
                                   vlan_priority_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv4_Header_Time_To_Live",
                                   "Give valid hex number",
                                   verbose_option_config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   ipv4_header_time_to_live_checkout,
                                   ipv4_header_time_to_live_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv4_Header_Flags",
                                   "Give valid hex number",
                                   verbose_option_config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   ipv4_header_flags_checkout,
                                   ipv4_header_flags_commit,
                                   number_range_three_bits_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv4_Header_Type_Of_Service",
                                   "Give valid hex number",
                                   verbose_option_config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   ipv4_header_type_of_service_checkout,
                                   ipv4_header_type_of_service_commit,
                                   number_range_four_bits_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv4_Header_Precedence",
                                   "Give valid hex number",
                                   verbose_option_config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   ipv4_header_precedence_checkout,
                                   ipv4_header_precedence_commit,
                                   number_range_three_bits_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Primary_RMCP_Port",
                                   "Give valid number",
                                   verbose_option_config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   primary_rmcp_port_checkout,
                                   primary_rmcp_port_commit,
                                   number_range_two_bytes_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Secondary_RMCP_Port",
                                   "Give valid number",
                                   verbose_option_config_flags | IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   secondary_rmcp_port_checkout,
                                   secondary_rmcp_port_commit,
                                   number_range_two_bytes_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}

