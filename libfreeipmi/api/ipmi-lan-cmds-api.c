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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "freeipmi/api/ipmi-lan-cmds-api.h"
#include "freeipmi/cmds/ipmi-lan-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-lan-configuration-parameters-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

int
ipmi_cmd_set_lan_configuration_parameters (ipmi_ctx_t ctx,
                                           uint8_t channel_number,
                                           uint8_t parameter_selector,
                                           const void *configuration_parameter_data,
                                           unsigned int configuration_parameter_data_len,
                                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters (channel_number,
                                                 parameter_selector,
                                                 configuration_parameter_data,
                                                 configuration_parameter_data_len,
                                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_set_in_progress (ipmi_ctx_t ctx,
                                                           uint8_t channel_number,
                                                           uint8_t state,
                                                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_set_in_progress_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_set_in_progress (channel_number,
                                                                 state,
                                                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_authentication_type_enables (ipmi_ctx_t ctx,
                                                                       uint8_t channel_number,
                                                                       uint8_t callback_level_none,
                                                                       uint8_t callback_level_md2,
                                                                       uint8_t callback_level_md5,
                                                                       uint8_t callback_level_straight_password,
                                                                       uint8_t callback_level_oem_proprietary,
                                                                       uint8_t user_level_none,
                                                                       uint8_t user_level_md2,
                                                                       uint8_t user_level_md5,
                                                                       uint8_t user_level_straight_password,
                                                                       uint8_t user_level_oem_proprietary,
                                                                       uint8_t operator_level_none,
                                                                       uint8_t operator_level_md2,
                                                                       uint8_t operator_level_md5,
                                                                       uint8_t operator_level_straight_password,
                                                                       uint8_t operator_level_oem_proprietary,
                                                                       uint8_t admin_level_none,
                                                                       uint8_t admin_level_md2,
                                                                       uint8_t admin_level_md5,
                                                                       uint8_t admin_level_straight_password,
                                                                       uint8_t admin_level_oem_proprietary,
                                                                       uint8_t oem_level_none,
                                                                       uint8_t oem_level_md2,
                                                                       uint8_t oem_level_md5,
                                                                       uint8_t oem_level_straight_password,
                                                                       uint8_t oem_level_oem_proprietary,
                                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_authentication_type_enables_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_authentication_type_enables (channel_number,
                                                                             callback_level_none,
                                                                             callback_level_md2,
                                                                             callback_level_md5,
                                                                             callback_level_straight_password,
                                                                             callback_level_oem_proprietary,
                                                                             user_level_none,
                                                                             user_level_md2,
                                                                             user_level_md5,
                                                                             user_level_straight_password,
                                                                             user_level_oem_proprietary,
                                                                             operator_level_none,
                                                                             operator_level_md2,
                                                                             operator_level_md5,
                                                                             operator_level_straight_password,
                                                                             operator_level_oem_proprietary,
                                                                             admin_level_none,
                                                                             admin_level_md2,
                                                                             admin_level_md5,
                                                                             admin_level_straight_password,
                                                                             admin_level_oem_proprietary,
                                                                             oem_level_none,
                                                                             oem_level_md2,
                                                                             oem_level_md5,
                                                                             oem_level_straight_password,
                                                                             oem_level_oem_proprietary,
                                                                             obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_ip_address (ipmi_ctx_t ctx,
                                                      uint8_t channel_number,
                                                      uint32_t ip_address,
                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_ip_address_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_ip_address (channel_number,
                                                            ip_address,
                                                            obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_ip_address_source (ipmi_ctx_t ctx,
                                                             uint8_t channel_number,
                                                             uint8_t ip_address_source,
                                                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_ip_address_source_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_ip_address_source (channel_number,
                                                                   ip_address_source,
                                                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_mac_address (ipmi_ctx_t ctx,
                                                       uint8_t channel_number,
                                                       uint64_t mac_address,
                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_mac_address_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_mac_address (channel_number,
                                                             mac_address,
                                                             obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_subnet_mask (ipmi_ctx_t ctx,
                                                       uint8_t channel_number,
                                                       uint32_t subnet_mask,
                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_subnet_mask_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_subnet_mask (channel_number,
                                                             subnet_mask,
                                                             obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_ipv4_header_parameters (ipmi_ctx_t ctx,
                                                                  uint8_t channel_number,
                                                                  uint8_t time_to_live,
                                                                  uint8_t flags,
                                                                  uint8_t type_of_service,
                                                                  uint8_t precedence,
                                                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_ipv4_header_parameters_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_ipv4_header_parameters (channel_number,
                                                                        time_to_live,
                                                                        flags,
                                                                        type_of_service,
                                                                        precedence,
                                                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_primary_rmcp_port_number (ipmi_ctx_t ctx,
                                                                    uint8_t channel_number,
                                                                    uint16_t primary_rmcp_port_number,
                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_primary_rmcp_port_number_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_primary_rmcp_port_number (channel_number,
                                                                          primary_rmcp_port_number,
                                                                          obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_secondary_rmcp_port_number (ipmi_ctx_t ctx,
                                                                      uint8_t channel_number,
                                                                      uint16_t secondary_rmcp_port_number,
                                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_secondary_rmcp_port_number_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_secondary_rmcp_port_number (channel_number,
                                                                            secondary_rmcp_port_number,
                                                                            obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (ipmi_ctx_t ctx,
                                                                     uint8_t channel_number,
                                                                     uint8_t bmc_generated_gratuitous_arps,
                                                                     uint8_t bmc_generated_arp_responses,
                                                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_bmc_generated_arp_control_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_bmc_generated_arp_control (channel_number,
                                                                           bmc_generated_gratuitous_arps,
                                                                           bmc_generated_arp_responses,
                                                                           obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_gratuitous_arp_interval (ipmi_ctx_t ctx,
                                                                   uint8_t channel_number,
                                                                   uint8_t gratuitous_arp_interval,
                                                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_gratuitous_arp_interval_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_gratuitous_arp_interval (channel_number,
                                                                         gratuitous_arp_interval,
                                                                         obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_default_gateway_address (ipmi_ctx_t ctx,
                                                                   uint8_t channel_number,
                                                                   uint32_t ip_address,
                                                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_ip_address_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_default_gateway_address (channel_number,
                                                                         ip_address,
                                                                         obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address (ipmi_ctx_t ctx,
                                                                       uint8_t channel_number,
                                                                       uint64_t mac_address,
                                                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_mac_address_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_default_gateway_mac_address (channel_number,
                                                                             mac_address,
                                                                             obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address (ipmi_ctx_t ctx,
                                                                  uint8_t channel_number,
                                                                  uint32_t ip_address,
                                                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_ip_address_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_backup_gateway_address (channel_number,
                                                                        ip_address,
                                                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (ipmi_ctx_t ctx,
                                                                      uint8_t channel_number,
                                                                      uint64_t mac_address,
                                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_mac_address_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (channel_number,
                                                                            mac_address,
                                                                            obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_community_string (ipmi_ctx_t ctx,
                                                            uint8_t channel_number,
                                                            const char *community_string,
                                                            unsigned int community_string_len,
                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_community_string_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_community_string (channel_number,
                                                                  community_string,
                                                                  community_string_len,
                                                                  obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_destination_type (ipmi_ctx_t ctx,
                                                            uint8_t channel_number,
                                                            uint8_t destination_selector,
                                                            uint8_t destination_type,
                                                            uint8_t alert_acknowledge,
                                                            uint8_t alert_acknowledge_timeout,
                                                            uint8_t retries,
                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_destination_type_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_destination_type (channel_number,
                                                                  destination_selector,
                                                                  destination_type,
                                                                  alert_acknowledge,
                                                                  alert_acknowledge_timeout,
                                                                  retries,
                                                                  obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_destination_addresses (ipmi_ctx_t ctx,
                                                                 uint8_t channel_number,
                                                                 uint8_t destination_selector,
                                                                 uint8_t gateway_selector,
                                                                 uint32_t alerting_ip_address,
                                                                 uint64_t alerting_mac_address,
                                                                 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_destination_addresses_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_destination_addresses (channel_number,
                                                                       destination_selector,
                                                                       gateway_selector,
                                                                       alerting_ip_address,
                                                                       alerting_mac_address,
                                                                       obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}



int
ipmi_cmd_set_lan_configuration_parameters_vlan_id (ipmi_ctx_t ctx,
                                                   uint8_t channel_number,
                                                   uint16_t vlan_id,
                                                   uint8_t vlan_id_enable,
                                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_vlan_id_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_vlan_id (channel_number,
                                                         vlan_id,
                                                         vlan_id_enable,
                                                         obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_vlan_priority (ipmi_ctx_t ctx,
                                                         uint8_t channel_number,
                                                         uint32_t vlan_priority,
                                                         fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_vlan_priority_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_vlan_priority (channel_number,
                                                               vlan_priority,
                                                               obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (ipmi_ctx_t ctx,
                                                                                            uint8_t channel_number,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_1,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_2,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_3,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_4,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_5,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_6,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_7,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_8,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_9,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_10,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_11,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_12,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_13,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_14,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_15,
                                                                                            uint8_t maximum_privilege_for_cipher_suite_16,
                                                                                            fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (channel_number,
                                                                                                  maximum_privilege_for_cipher_suite_1,
                                                                                                  maximum_privilege_for_cipher_suite_2,
                                                                                                  maximum_privilege_for_cipher_suite_3,
                                                                                                  maximum_privilege_for_cipher_suite_4,
                                                                                                  maximum_privilege_for_cipher_suite_5,
                                                                                                  maximum_privilege_for_cipher_suite_6,
                                                                                                  maximum_privilege_for_cipher_suite_7,
                                                                                                  maximum_privilege_for_cipher_suite_8,
                                                                                                  maximum_privilege_for_cipher_suite_9,
                                                                                                  maximum_privilege_for_cipher_suite_10,
                                                                                                  maximum_privilege_for_cipher_suite_11,
                                                                                                  maximum_privilege_for_cipher_suite_12,
                                                                                                  maximum_privilege_for_cipher_suite_13,
                                                                                                  maximum_privilege_for_cipher_suite_14,
                                                                                                  maximum_privilege_for_cipher_suite_15,
                                                                                                  maximum_privilege_for_cipher_suite_16,
                                                                                                  obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);


}

int
ipmi_cmd_set_lan_configuration_parameters_bad_password_threshold (ipmi_ctx_t ctx,
                                                                  uint8_t channel_number,
                                                                  uint8_t user_disabled_event_message,
                                                                  uint8_t bad_password_threshold_number,
                                                                  uint16_t attempt_count_reset_interval,
                                                                  uint16_t user_lockout_interval,
                                                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_set_lan_configuration_parameters_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_bad_password_threshold_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_set_lan_configuration_parameters_bad_password_threshold (channel_number,
                                                                        user_disabled_event_message,
                                                                        bad_password_threshold_number,
                                                                        attempt_count_reset_interval,
                                                                        user_lockout_interval,
                                                                        obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

static int
_ipmi_cmd_get_lan_configuration_parameters_common (ipmi_ctx_t ctx,
						   uint8_t channel_number,
						   uint8_t get_parameter,
						   uint8_t set_selector,
						   uint8_t block_selector,
						   fiid_obj_t obj_cmd_rs,
						   fiid_field_t *tmpl_cmd_rs_expected,
						   uint8_t parameter_selector)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  assert (tmpl_cmd_rs_expected);

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs, tmpl_cmd_rs_expected) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_lan_configuration_parameters (channel_number,
                                                 get_parameter,
						 parameter_selector,
                                                 set_selector,
                                                 block_selector,
                                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_lan_configuration_parameters (ipmi_ctx_t ctx,
                                           uint8_t channel_number,
                                           uint8_t get_parameter,
                                           uint8_t parameter_selector,
                                           uint8_t set_selector,
                                           uint8_t block_selector,
                                           fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_rs,
							 parameter_selector) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_set_in_progress (ipmi_ctx_t ctx,
                                                           uint8_t channel_number,
                                                           uint8_t get_parameter,
                                                           uint8_t set_selector,
                                                           uint8_t block_selector,
                                                           fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_set_in_progress_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_SET_IN_PROGRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_authentication_type_support (ipmi_ctx_t ctx,
                                                                       uint8_t channel_number,
                                                                       uint8_t get_parameter,
                                                                       uint8_t set_selector,
                                                                       uint8_t block_selector,
                                                                       fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_authentication_type_support_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_AUTHENTICATION_TYPE_SUPPORT) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_authentication_type_enables (ipmi_ctx_t ctx,
                                                                       uint8_t channel_number,
                                                                       uint8_t get_parameter,
                                                                       uint8_t set_selector,
                                                                       uint8_t block_selector,
                                                                       fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_authentication_type_enables_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_AUTHENTICATION_TYPE_ENABLES) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_ip_address (ipmi_ctx_t ctx,
                                                      uint8_t channel_number,
                                                      uint8_t get_parameter,
                                                      uint8_t set_selector,
                                                      uint8_t block_selector,
                                                      fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_ip_address_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_ip_address_source (ipmi_ctx_t ctx,
                                                             uint8_t channel_number,
                                                             uint8_t get_parameter,
                                                             uint8_t set_selector,
                                                             uint8_t block_selector,
                                                             fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_IP_ADDRESS_SOURCE) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_mac_address (ipmi_ctx_t ctx,
                                                       uint8_t channel_number,
                                                       uint8_t get_parameter,
                                                       uint8_t set_selector,
                                                       uint8_t block_selector,
                                                       fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_mac_address_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_MAC_ADDRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_subnet_mask (ipmi_ctx_t ctx,
                                                       uint8_t channel_number,
                                                       uint8_t get_parameter,
                                                       uint8_t set_selector,
                                                       uint8_t block_selector,
                                                       fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_SUBNET_MASK) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_ipv4_header_parameters (ipmi_ctx_t ctx,
                                                                  uint8_t channel_number,
                                                                  uint8_t get_parameter,
                                                                  uint8_t set_selector,
                                                                  uint8_t block_selector,
                                                                  fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_ipv4_header_parameters_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_IPV4_HEADER_PARAMETERS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }
  
  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_primary_rmcp_port_number (ipmi_ctx_t ctx,
								    uint8_t channel_number,
								    uint8_t get_parameter,
								    uint8_t set_selector,
								    uint8_t block_selector,
								    fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_primary_rmcp_port_number_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_PRIMARY_RMCP_PORT_NUMBER) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_secondary_rmcp_port_number (ipmi_ctx_t ctx,
								      uint8_t channel_number,
								      uint8_t get_parameter,
								      uint8_t set_selector,
								      uint8_t block_selector,
								      fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_secondary_rmcp_port_number_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_SECONDARY_RMCP_PORT_NUMBER) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_bmc_generated_arp_control (ipmi_ctx_t ctx,
								     uint8_t channel_number,
								     uint8_t get_parameter,
								     uint8_t set_selector,
								     uint8_t block_selector,
								     fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_bmc_generated_arp_control_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_BMC_GENERATED_ARP_CONTROL) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_gratuitous_arp_interval (ipmi_ctx_t ctx,
								   uint8_t channel_number,
								   uint8_t get_parameter,
								   uint8_t set_selector,
								   uint8_t block_selector,
								   fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_gratuitous_arp_interval_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_GRATUITOUS_ARP_INTERVAL) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_default_gateway_address (ipmi_ctx_t ctx,
								   uint8_t channel_number,
								   uint8_t get_parameter,
								   uint8_t set_selector,
								   uint8_t block_selector,
								   fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY_ADDRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address (ipmi_ctx_t ctx,
								       uint8_t channel_number,
								       uint8_t get_parameter,
								       uint8_t set_selector,
								       uint8_t block_selector,
								       fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_mac_address_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_DEFAULT_GATEWAY_MAC_ADDRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address (ipmi_ctx_t ctx,
								  uint8_t channel_number,
								  uint8_t get_parameter,
								  uint8_t set_selector,
								  uint8_t block_selector,
								  fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY_ADDRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address (ipmi_ctx_t ctx,
								      uint8_t channel_number,
								      uint8_t get_parameter,
								      uint8_t set_selector,
								      uint8_t block_selector,
								      fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_mac_address_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_BACKUP_GATEWAY_MAC_ADDRESS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_community_string (ipmi_ctx_t ctx,
							    uint8_t channel_number,
							    uint8_t get_parameter,
							    uint8_t set_selector,
							    uint8_t block_selector,
							    fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_community_string_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_COMMUNITY_STRING) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_number_of_destinations (ipmi_ctx_t ctx,
								  uint8_t channel_number,
								  uint8_t get_parameter,
								  uint8_t set_selector,
								  uint8_t block_selector,
								  fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_NUMBER_OF_DESTINATIONS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_destination_type (ipmi_ctx_t ctx,
							    uint8_t channel_number,
							    uint8_t get_parameter,
							    uint8_t set_selector,
							    uint8_t block_selector,
							    fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_destination_type_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_TYPE) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_destination_addresses (ipmi_ctx_t ctx,
								 uint8_t channel_number,
								 uint8_t get_parameter,
								 uint8_t set_selector,
								 uint8_t block_selector,
								 fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_DESTINATION_ADDRESSES) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_vlan_id (ipmi_ctx_t ctx,
						   uint8_t channel_number,
						   uint8_t get_parameter,
						   uint8_t set_selector,
						   uint8_t block_selector,
						   fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_VLAN_ID) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_vlan_priority (ipmi_ctx_t ctx,
							 uint8_t channel_number,
							 uint8_t get_parameter,
							 uint8_t set_selector,
							 uint8_t block_selector,
							 fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_VLAN_PRIORITY) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support (ipmi_ctx_t ctx,
											 uint8_t channel_number,
											 uint8_t get_parameter,
											 uint8_t set_selector,
											 uint8_t block_selector,
											 fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entry_support_rs,                                                 IPMI_LAN_CONFIGURATION_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_ENTRY_SUPPORT) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries (ipmi_ctx_t ctx,
										   uint8_t channel_number,
										   uint8_t get_parameter,
										   uint8_t set_selector,
										   uint8_t block_selector,
										   fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_entries_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_ENTRIES) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels (ipmi_ctx_t ctx,
											    uint8_t channel_number,
											    uint8_t get_parameter,
											    uint8_t set_selector,
											    uint8_t block_selector,
											    fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_rmcpplus_messaging_cipher_suite_privilege_levels_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_RMCPPLUS_MESSAGING_CIPHER_SUITE_PRIVILEGE_LEVELS) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_get_lan_configuration_parameters_bad_password_threshold (ipmi_ctx_t ctx,
								  uint8_t channel_number,
								  uint8_t get_parameter,
								  uint8_t set_selector,
								  uint8_t block_selector,
								  fiid_obj_t obj_cmd_rs)
{
  if (_ipmi_cmd_get_lan_configuration_parameters_common (ctx,
							 channel_number,
							 get_parameter,
							 set_selector,
							 block_selector,
							 obj_cmd_rs,
							 tmpl_cmd_get_lan_configuration_parameters_bad_password_threshold_rs,
							 IPMI_LAN_CONFIGURATION_PARAMETER_BAD_PASSWORD_THRESHOLD) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  return (0);
}

int
ipmi_cmd_suspend_bmc_arps (ipmi_ctx_t ctx,
                           uint8_t channel_number,
                           uint8_t gratuitous_arp_suspend,
                           uint8_t arp_response_suspend,
                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_suspend_bmc_arps_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_suspend_bmc_arps_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_suspend_bmc_arps (channel_number,
                                 gratuitous_arp_suspend,
                                 arp_response_suspend,
                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}

int
ipmi_cmd_get_ip_udp_rmcp_statistics (ipmi_ctx_t ctx,
                                     uint8_t channel_number,
                                     uint8_t clear_all_statistics,
                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* remaining parameter checks in fill function */
  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_get_ip_udp_rmcp_statistics_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_get_ip_udp_rmcp_statistics_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_get_ip_udp_rmcp_statistics (channel_number,
                                           clear_all_statistics,
                                           obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_TRANSPORT_RQ,
                    obj_cmd_rq,
                    obj_cmd_rs) < 0)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  return (rv);
}
