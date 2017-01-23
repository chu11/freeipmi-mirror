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
#include <arpa/inet.h>

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define BMC_MAXIPADDRLEN 16
#define BMC_MAXIPV6ADDRLEN 45
#define BMC_MAXMACADDRLEN 24

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
                       "fiid_obj_get_data: 'supports_ipv6_only': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->ipv6_ipv4_support_supports_ipv6_only = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "supports_ipv6_and_ipv4_simultaneously", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'supports_ipv6_and_ipv4_simultaneously': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->ipv6_ipv4_support_supports_ipv6_and_ipv4_simultaneously = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "supports_ipv6_destination_address_for_lan_alert", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'supports_ipv6_destination_address_for_lan_alert': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  state_data->ipv6_ipv4_support_supports_ipv6_destination_address_for_lan_alert = val;

  state_data->ipv6_ipv4_support_initialized++;
 out:
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_ipv4_support_commit (ipmi_config_state_data_t *state_data,
                          const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  /* Read only parameter */
  return (IPMI_CONFIG_ERR_FATAL_ERROR);
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

  if (fiid_obj_get_data (obj_cmd_rs,
                         "enables",
                         &val,
                         1) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'enables': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ipv6_ipv4_addressing_enables_string(val)) < 0)
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
  /* XXX LJ TODO: we need this.. */
  return (IPMI_CONFIG_ERR_FATAL_ERROR);
}

static ipmi_config_err_t
ipv6_static_addresses_checkout (ipmi_config_state_data_t *state_data,
                                const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char ipv6_address_str[BMC_MAXIPV6ADDRLEN + 1];
  char ipv6_addresses_str[(BMC_MAXIPV6ADDRLEN + 1)*255];
  uint8_t ipv6_static_ip_address_bytes[16];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint8_t address_max;
  uint8_t set;
  uint64_t val;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_status_rs)))
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
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_static_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "static_address_max", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'supports_ipv6_only': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  address_max = val;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  memset (ipv6_addresses_str, '\0', (BMC_MAXIPV6ADDRLEN+1)*255);
  for (set = 0; set < address_max; set++)
    {
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
									   set,
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

      if (fiid_obj_get_data (obj_cmd_rs,
			     "address",
			     ipv6_static_ip_address_bytes,
			     16) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "fiid_obj_get_data: 'address': %s\n",
			   fiid_obj_errormsg (obj_cmd_rs));
	  goto cleanup;
	}

      memset (ipv6_address_str, '\0', BMC_MAXIPV6ADDRLEN+1);
      if (NULL == inet_ntop(AF_INET6, ipv6_static_ip_address_bytes, ipv6_address_str, BMC_MAXIPV6ADDRLEN))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "fiid_obj_get_data: 'address': %s\n",
			   strerror (errno));
	  goto cleanup;

	}
      if (!same (ipv6_address_str, "::"))
	{
	  sprintf(ipv6_addresses_str+strlen(ipv6_addresses_str), "%s ", ipv6_address_str);
	}
      fiid_obj_destroy (obj_cmd_rs);
      obj_cmd_rs = NULL;
  }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ipv6_addresses_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_static_addresses_commit (ipmi_config_state_data_t *state_data,
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

  return IPMI_CONFIG_ERR_SUCCESS; /* XXX TODO lamont -- yet to be implemented. */

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
ipv6_dynamic_addresses_checkout (ipmi_config_state_data_t *state_data,
                                const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char ipv6_address_str[BMC_MAXIPV6ADDRLEN + 1];
  char ipv6_addresses_str[(BMC_MAXIPV6ADDRLEN + 1)*255];
  uint8_t ipv6_dynamic_ip_address_bytes[16];
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;
  uint8_t address_max;
  uint8_t set;
  uint64_t val;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_status_rs)))
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
                         "ipmi_cmd_get_lan_configuration_parameters_ipv6_dynamic_ip_address: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "dynamic_address_max", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'supports_ipv6_only': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  address_max = val;
  fiid_obj_destroy (obj_cmd_rs);
  obj_cmd_rs = NULL;

  memset (ipv6_addresses_str, '\0', (BMC_MAXIPV6ADDRLEN+1)*255);
  for (set = 0; set < address_max; set++)
    {
      if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_ipv6_dynamic_addresses_rs)))
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

      if (ipmi_cmd_get_lan_configuration_parameters_ipv6_dynamic_addresses (state_data->ipmi_ctx,
									   channel_number,
									   IPMI_GET_LAN_PARAMETER,
									   set,
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
			     "ipmi_cmd_get_lan_configuration_parameters_ipv6_dynamic_ip_address: %s\n",
			     ipmi_ctx_errormsg (state_data->ipmi_ctx));

	  goto cleanup;
	}

      if (FIID_OBJ_GET (obj_cmd_rs, "address_status", &val) < 0)
        {
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "fiid_obj_get_data: 'address_status': %s\n",
			   fiid_obj_errormsg (obj_cmd_rs));
	  goto cleanup;
	}

      /* XXX LJ: Need defines for ipv6_dynamic_address_status */
      if (val == 0)
	{
	  if (fiid_obj_get_data (obj_cmd_rs,
				 "address",
				 ipv6_dynamic_ip_address_bytes,
				 16) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "fiid_obj_get_data: 'address': %s\n",
			       fiid_obj_errormsg (obj_cmd_rs));
	      goto cleanup;
	    }

	  memset (ipv6_address_str, '\0', BMC_MAXIPV6ADDRLEN+1);
	  if (NULL == inet_ntop(AF_INET6, ipv6_dynamic_ip_address_bytes, ipv6_address_str, BMC_MAXIPV6ADDRLEN))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "fiid_obj_get_data: 'address': %s\n",
			       strerror (errno));
	      goto cleanup;

	    }
	  if (!same (ipv6_address_str, "::"))
	    {
	      sprintf(ipv6_addresses_str+strlen(ipv6_addresses_str), "%s ", ipv6_address_str);
	    }
	}
      fiid_obj_destroy (obj_cmd_rs);
      obj_cmd_rs = NULL;
  }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  ipv6_addresses_str) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
ipv6_dynamic_addresses_commit (ipmi_config_state_data_t *state_data,
                              const char *section_name,
                              const struct ipmi_config_keyvalue *kv)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (kv);

  /* Dynamic addresses are not writable. */
  return (rv);
}

struct ipmi_config_section *
ipmi_config_core_lan6_conf_section_get (ipmi_config_state_data_t *state_data,
                                        unsigned int config_flags,
                                        int channel_index)
{
  struct ipmi_config_section *section = NULL;
  char *section_comment =
    "In the Lan6_Conf section, typical networking configuration is setup.  "
    "Most users will choose to set \"Static\" for the \"IP_Address_Source\" "
    "and set the appropriate \"IP_Address\", \"MAC_Address\", "
    "\"Subnet_Mask\", etc. for the machine.";
  char *section_name_base_str = "Lan6_Conf";
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
                                   "Supports_IPv6_Only",
                                   "Supports IPv6-only",
                                   0,
                                   ipv6_ipv4_support_ipv6_only_checkout,
                                   ipv6_ipv4_support_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Supports_IPv6_And_IPv4_Simultaneously",
                                   "Supports IPv6 And IPv4 Simultaneously",
                                   0,
				   ipv6_ipv4_support_ipv6_and_ipv4_simultaneously_checkout,
                                   ipv6_ipv4_support_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Supports_IPv6_Destination_Address_For_Lan_Alert",
                                   "Supports IPv6 Destination Address For Lan Alert",
                                   verbose_option_config_flags,
				   ipv6_ipv4_support_ipv6_destination_address_for_lan_alert_checkout,
                                   ipv6_ipv4_support_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_IPv4_Addressing_Enables",
                                   "One of 0 (IPv4-only), 1 (IPv6-only), or 2 (IPv4 and IPv6 simultaneously)",
                                   0,
                                   ipv6_ipv4_addressing_enables_checkout,
                                   ipv6_ipv4_addressing_enables_commit,
                                   ipv6_ipv4_addressing_enables_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Static_Addresses",
                                   "Give valid IPv6 address",
                                   0,
                                   ipv6_static_addresses_checkout,
                                   ipv6_static_addresses_commit,
                                   ipv6_address_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "IPv6_Dynamic_Addresses",
                                   "Give valid IPv6 address",
                                   0,
                                   ipv6_dynamic_addresses_checkout,
                                   ipv6_dynamic_addresses_commit,
                                   ipv6_address_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}

