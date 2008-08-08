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
#include "bmc-config-utils.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-fiid-wrappers.h"

#define BMC_MAXIPADDRLEN 16
#define BMC_MAXMACADDRLEN 24

/* convenience struct */
struct vlan_id
{
  uint8_t vlan_id;
  uint8_t vlan_id_enable;
};

static config_err_t
ip_address_source_checkout (const char *section_name,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address_source (state_data->ipmi_ctx, 
								   channel_number, 
								   IPMI_GET_LAN_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_ip_address_source: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET (obj_cmd_rs, "ip_address_source", &val);

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ip_address_source_string (val)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
ip_address_source_commit (const char *section_name,
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

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address_source (state_data->ipmi_ctx,
                                                                   channel_number,
                                                                   ip_address_source_number (kv->value_input),
                                                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_ip_address_source: %s\n",
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
ip_address_checkout (const char *section_name,
		     struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  char ip_address_str[BMC_MAXIPADDRLEN + 1];
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_ip_address_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address (state_data->ipmi_ctx, 
							    channel_number, 
							    IPMI_GET_LAN_PARAMETER, 
							    SET_SELECTOR, 
							    BLOCK_SELECTOR, 
							    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_ip_address: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET_DATA (obj_cmd_rs, 
                      "ip_address", 
                      ip_address_bytes,
                      4);

  memset(ip_address_str, '\0', BMC_MAXIPADDRLEN+1);
  snprintf(ip_address_str, 
           BMC_MAXIPADDRLEN,
           "%u.%u.%u.%u", 
           ip_address_bytes[0], 
           ip_address_bytes[1], 
           ip_address_bytes[2], 
           ip_address_bytes[3]);

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ip_address_str) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
ip_address_commit (const char *section_name,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (config_ipv4_address_string2int(state_data->pstate,
                                     kv->value_input, 
                                     &ip_address_val) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address (state_data->ipmi_ctx,
                                                            channel_number,
                                                            ip_address_val,
                                                            obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_ip_address: %s\n",
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
mac_address_checkout (const char *section_name,
		      struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[BMC_MAXMACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_mac_address_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (state_data->ipmi_ctx, 
							     channel_number, 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_mac_address: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET_DATA (obj_cmd_rs, 
                      "mac_address", 
                      mac_address_bytes,
                      6);
  
  memset(mac_address_str, '\0', BMC_MAXMACADDRLEN+1);
  snprintf(mac_address_str, 
           BMC_MAXMACADDRLEN,
           "%02X:%02X:%02X:%02X:%02X:%02X", 
           mac_address_bytes[0], 
           mac_address_bytes[1], 
           mac_address_bytes[2], 
           mac_address_bytes[3], 
           mac_address_bytes[4], 
           mac_address_bytes[5]);
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            mac_address_str) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);

}

static config_err_t
mac_address_commit (const char *section_name,
		    const struct config_keyvalue *kv,
                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (config_mac_address_string2int(state_data->pstate,
                                    kv->value_input,
                                    &mac_address_val) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_mac_address (state_data->ipmi_ctx,
                                                             channel_number,
                                                             mac_address_val,
                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_mac_address: %s\n",
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
subnet_mask_checkout (const char *section_name,
		      struct config_keyvalue *kv,
                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  char subnet_mask_str[BMC_MAXIPADDRLEN + 1];
  uint8_t subnet_mask_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_subnet_mask (state_data->ipmi_ctx, 
							     channel_number, 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_subnet_mask: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET_DATA (obj_cmd_rs, 
                      "subnet_mask", 
                      subnet_mask_bytes,
                      4);

  memset(subnet_mask_str, '\0', BMC_MAXIPADDRLEN + 1);
  snprintf (subnet_mask_str, 
            BMC_MAXIPADDRLEN,
            "%u.%u.%u.%u", 
            subnet_mask_bytes[0], 
            subnet_mask_bytes[1], 
            subnet_mask_bytes[2], 
            subnet_mask_bytes[3]);
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            subnet_mask_str) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
subnet_mask_commit (const char *section_name,
		    const struct config_keyvalue *kv,
                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t subnet_mask_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (config_ipv4_address_string2int(state_data->pstate,
                                     kv->value_input, 
                                     &subnet_mask_val) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_subnet_mask (state_data->ipmi_ctx,
                                                             channel_number,
                                                             subnet_mask_val,
                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_subnet_mask: %s\n",
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
default_gateway_address_checkout (const char *section_name,
				  struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  char ip_address_str[BMC_MAXIPADDRLEN + 1];
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_address (state_data->ipmi_ctx, 
									 channel_number, 
									 IPMI_GET_LAN_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_default_gateway_address: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET_DATA (obj_cmd_rs, 
                      "ip_address", 
                      ip_address_bytes,
                      4);
  
  memset(ip_address_str, '\0', BMC_MAXIPADDRLEN + 1);
  snprintf (ip_address_str, 
            BMC_MAXIPADDRLEN,
            "%u.%u.%u.%u", 
            ip_address_bytes[0], 
            ip_address_bytes[1], 
            ip_address_bytes[2], 
            ip_address_bytes[3]);
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ip_address_str) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);

}

static config_err_t
default_gateway_address_commit (const char *section_name,
				const struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (config_ipv4_address_string2int(state_data->pstate,
                                     kv->value_input,
                                     &ip_address_val) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_address (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         ip_address_val,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_default_gateway_address: %s\n",
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
default_gateway_mac_address_checkout (const char *section_name,
				      struct config_keyvalue *kv,
                                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[BMC_MAXMACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs);
  
  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address (state_data->ipmi_ctx, 
									     channel_number, 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET_DATA (obj_cmd_rs, 
                      "mac_address", 
                      mac_address_bytes,
                      6);
  
  memset(mac_address_str, '\0', BMC_MAXMACADDRLEN + 1);
  snprintf (mac_address_str, 
            BMC_MAXMACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X", 
            mac_address_bytes[0], 
            mac_address_bytes[1], 
            mac_address_bytes[2], 
            mac_address_bytes[3], 
            mac_address_bytes[4], 
            mac_address_bytes[5]);
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            mac_address_str) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);

}

static config_err_t
default_gateway_mac_address_commit (const char *section_name,
				    const struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (config_mac_address_string2int(state_data->pstate,
                                    kv->value_input, 
                                    &mac_address_val) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address (state_data->ipmi_ctx,
                                                                             channel_number,
                                                                             mac_address_val,
                                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address: %s\n",
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
backup_gateway_address_checkout (const char *section_name,
				 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  char ip_address_str[BMC_MAXIPADDRLEN + 1];
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address (state_data->ipmi_ctx, 
									channel_number, 
									IPMI_GET_LAN_PARAMETER, 
									SET_SELECTOR, 
									BLOCK_SELECTOR, 
									obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET_DATA (obj_cmd_rs, 
                      "ip_address", 
                      ip_address_bytes,
                      4);
  
  memset(ip_address_str, '\0', BMC_MAXIPADDRLEN+1);
  snprintf (ip_address_str, 
            BMC_MAXIPADDRLEN,
            "%u.%u.%u.%u", 
            ip_address_bytes[0], 
            ip_address_bytes[1], 
            ip_address_bytes[2], 
            ip_address_bytes[3]);
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv,
                                            ip_address_str) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
backup_gateway_address_commit (const char *section_name,
			       const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (config_ipv4_address_string2int(state_data->pstate,
                                     kv->value_input, 
                                     &ip_address_val) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address (state_data->ipmi_ctx,
                                                                        channel_number,
                                                                        ip_address_val,
                                                                        obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address: %s\n",
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
backup_gateway_mac_address_checkout (const char *section_name,
				     struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[BMC_MAXMACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address (state_data->ipmi_ctx, 
									    channel_number, 
									    IPMI_GET_LAN_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET_DATA (obj_cmd_rs, 
                      "mac_address", 
                      mac_address_bytes,
                      6);
  
  memset(mac_address_str, '\0', BMC_MAXMACADDRLEN+1);
  snprintf (mac_address_str, 
            BMC_MAXMACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X", 
            mac_address_bytes[0], 
            mac_address_bytes[1], 
            mac_address_bytes[2], 
            mac_address_bytes[3], 
            mac_address_bytes[4], 
            mac_address_bytes[5]);
  
  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            mac_address_str) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
backup_gateway_mac_address_commit (const char *section_name,
                                   const struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (config_mac_address_string2int(state_data->pstate,
                                    kv->value_input,
                                    &mac_address_val) < 0)
    goto cleanup;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (state_data->ipmi_ctx,
                                                                            channel_number,
                                                                            mac_address_val,
                                                                            obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
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
_get_vlan_id (bmc_config_state_data_t *state_data,
              struct vlan_id *vi)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(vi);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_id (state_data->ipmi_ctx,
                                                         channel_number,
                                                         IPMI_GET_LAN_PARAMETER,
                                                         SET_SELECTOR,
                                                         BLOCK_SELECTOR,
                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_vlan_id: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  _FIID_OBJ_GET (obj_cmd_rs, "vlan_id", &val);
  vi->vlan_id = val;
  
  _FIID_OBJ_GET (obj_cmd_rs, "vlan_id_enable", &val);
  vi->vlan_id_enable = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_vlan_id (bmc_config_state_data_t *state_data,
              struct vlan_id *vi)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(vi);

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
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
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_vlan_id: %s\n",
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
vlan_id_checkout (const char *section_name,
		  struct config_keyvalue *kv,
                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct vlan_id vi;
  config_err_t ret;
  
  if ((ret = _get_vlan_id (state_data, &vi)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output_int(state_data->pstate,
                                                kv,
                                                vi.vlan_id) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_id_commit (const char *section_name,
		const struct config_keyvalue *kv,
                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct vlan_id vi;
  config_err_t ret;
  
  if ((ret = _get_vlan_id (state_data, &vi)) != CONFIG_ERR_SUCCESS)
    return ret;

  vi.vlan_id = atoi (kv->value_input);

  return _set_vlan_id (state_data, &vi);
}

static config_validate_t
vlan_id_validate (const char *section_name,
                  const char *key_name,
		  const char *value,
                  void *arg)
{
  return config_check_number_range(value, 0, 4095);
}

static config_err_t
vlan_id_enable_checkout (const char *section_name,
			 struct config_keyvalue *kv,
                         void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct vlan_id vi;
  config_err_t ret;
  
  if ((ret = _get_vlan_id (state_data, &vi)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(state_data->pstate,
                                            kv, 
                                            vi.vlan_id_enable ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
vlan_id_enable_commit (const char *section_name,
		       const struct config_keyvalue *kv,
                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct vlan_id vi;
  config_err_t ret;
  
  if ((ret = _get_vlan_id (state_data, &vi)) != CONFIG_ERR_SUCCESS)
    return ret;

  vi.vlan_id_enable = same (kv->value_input, "yes");
  
  return _set_vlan_id (state_data, &vi);
}

static config_err_t
vlan_priority_checkout (const char *section_name,
			struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs);

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_priority (state_data->ipmi_ctx,
                                                               channel_number,
                                                               IPMI_GET_LAN_PARAMETER,
                                                               SET_SELECTOR,
                                                               BLOCK_SELECTOR,
                                                               obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_get_lan_configuration_parameters_vlan_priority: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(state_data->ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_rs, "vlan_priority", &val);

  if (config_section_update_keyvalue_output_int(state_data->pstate,
                                                kv,
                                                val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}

static config_err_t
vlan_priority_commit (const char *section_name,
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

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_priority (state_data->ipmi_ctx,
                                                               channel_number,
                                                               atoi (kv->value_input),
                                                               obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "ipmi_cmd_set_lan_configuration_parameters_vlan_priority: %s\n",
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

struct config_section *
bmc_config_lan_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_conf_section = NULL;
  char *section_comment = 
    "In the Lan_Conf section, typical networking configuration is setup.  "
    "Most users will choose to set \"Static\" for the \"IP_Address_Source\" "
    "and set the appropriate \"IP_Address\", \"MAC_Address\", "
    "\"Subnet_Mask\", etc. for the machine.";

  if (!(lan_conf_section = config_section_create (state_data->pstate,
                                                  "Lan_Conf",
                                                  "Lan_Conf",
                                                  section_comment,
                                                  0,
                                                  NULL,
                                                  NULL)))
    goto cleanup;
  
  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "IP_Address_Source",
                              "Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
                              0,
                              ip_address_source_checkout,
                              ip_address_source_commit,
                              ip_address_source_number_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "IP_Address",
                              "Give valid IP address",
                              0,
                              ip_address_checkout,
                              ip_address_commit,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "MAC_Address",
                              "Give valid MAC address",
                              0,
                              mac_address_checkout,
                              mac_address_commit,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  /* TODO: checking valid netmask is not same as checking valid IP */
  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Subnet_Mask",
                              "Give valid Subnet Mask",
                              0,
                              subnet_mask_checkout,
                              subnet_mask_commit,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Default_Gateway_IP_Address",
                              "Give valid IP address",
                              0,
                              default_gateway_address_checkout,
                              default_gateway_address_commit,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Default_Gateway_MAC_Address",
                              "Give valid MAC address",
                              0,
                              default_gateway_mac_address_checkout,
                              default_gateway_mac_address_commit,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Backup_Gateway_IP_Address",
                              "Give valid IP address",
                              0,
                              backup_gateway_address_checkout,
                              backup_gateway_address_commit,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Backup_Gateway_MAC_Address",
                              "Give valid MAC address",
                              0,
                              backup_gateway_mac_address_checkout,
                              backup_gateway_mac_address_commit,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Vlan_id",
                              "Give valid unsigned number",
                              0,
                              vlan_id_checkout,
                              vlan_id_commit,
                              vlan_id_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Vlan_Id_Enable",
                              "Possible values: Yes/No",
                              0,
                              vlan_id_enable_checkout,
                              vlan_id_enable_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (state_data->pstate,
                              lan_conf_section,
                              "Vlan_Priority",
                              "Give valid unsigned number",
                              0,
                              vlan_priority_checkout,
                              vlan_priority_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  return lan_conf_section;

 cleanup:
  if (lan_conf_section)
    config_section_destroy(state_data->pstate, lan_conf_section);
  return NULL;
}

