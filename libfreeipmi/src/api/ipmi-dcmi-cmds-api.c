/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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
/*****************************************************************************\
 *  $Id: ipmi-dcmi-cmds-api.c,v 1.4 2010-05-17 17:42:45 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2009-2010 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-413270
 *
 *  This file is part of Ipmi-Dcmi, tools and libraries to support the
 *  data center manageability interface (DCMI).  For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Ipmi-Dcmi is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-Dcmi is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-Dcmi.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/api/ipmi-dcmi-cmds-api.h"
#include "freeipmi/cmds/ipmi-dcmi-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-cmd-dcmi-spec.h"
#include "freeipmi/spec/ipmi-comp-code-dcmi-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

int
ipmi_cmd_dcmi_get_dcmi_capability_info (ipmi_ctx_t ctx,
                                        uint8_t parameter_selector,
                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SELECTOR_VALID (parameter_selector)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_dcmi_capability_info_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_dcmi_capability_info (parameter_selector, obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities (ipmi_ctx_t ctx,
                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_SUPPORTED_DCMI_CAPABILITIES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_dcmi_capability_info_mandatory_platform_attributes (ipmi_ctx_t ctx,
                                                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_dcmi_capability_info_mandatory_platform_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_MANDATORY_PLATFORM_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_dcmi_capability_info_optional_platform_attributes (ipmi_ctx_t ctx,
                                                                     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_dcmi_capability_info_optional_platform_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_OPTIONAL_PLATFORM_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_dcmi_capability_info_manageability_access_attributes (ipmi_ctx_t ctx,
                                                                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_dcmi_capability_info_manageability_access_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_MANAGEABILITY_ACCESS_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_dcmi_capability_info_enhanced_system_power_statistics_attributes (ipmi_ctx_t ctx,
                                                                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_dcmi_capability_info_enhanced_system_power_statistics_attributes_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_dcmi_capability_info (IPMI_DCMI_CAPABILITIES_INFO_PARAMETER_ENHANCED_SYSTEM_POWER_STATISTICS_ATTRIBUTES,
                                              obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_asset_tag (ipmi_ctx_t ctx,
                             uint8_t offset_to_read,
                             uint8_t number_of_bytes_to_read,
                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (number_of_bytes_to_read > IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_asset_tag_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_asset_tag_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_asset_tag (offset_to_read,
                                   number_of_bytes_to_read,
                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_set_asset_tag (ipmi_ctx_t ctx,
                             uint8_t offset_to_write,
                             uint8_t number_of_bytes_to_write,
                             const void *data,
                             unsigned int data_len,
                             fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }
  
  if (number_of_bytes_to_write > IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX
      || !data
      || !data_len
      || number_of_bytes_to_write > data_len
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_set_asset_tag_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_set_asset_tag_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_set_asset_tag (offset_to_write,
                                   number_of_bytes_to_write,
                                   data,
                                   data_len,
                                   obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_management_controller_identifier_string (ipmi_ctx_t ctx,
                                                           uint8_t offset_to_read,
                                                           uint8_t number_of_bytes_to_read,
                                                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (number_of_bytes_to_read > IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_management_controller_identifier_string_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_management_controller_identifier_string_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_management_controller_identifier_string (offset_to_read,
                                                                 number_of_bytes_to_read,
                                                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_set_management_controller_identifier_string (ipmi_ctx_t ctx,
                                                           uint8_t offset_to_write,
                                                           uint8_t number_of_bytes_to_write,
                                                           const void *data,
                                                           unsigned int data_len,
                                                           fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;
  
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }
  
  if (number_of_bytes_to_write > IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX
      || !data
      || !data_len
      || number_of_bytes_to_write > data_len
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_set_management_controller_identifier_string_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_set_management_controller_identifier_string_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_set_management_controller_identifier_string (offset_to_write,
                                                                 number_of_bytes_to_write,
                                                                 data,
                                                                 data_len,
                                                                 obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_dcmi_sensor_info (ipmi_ctx_t ctx,
                                    uint8_t sensor_type,
                                    uint8_t entity_id,
                                    uint8_t entity_instance,
                                    uint8_t entity_instance_start,
                                    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* achu: only entity id's listed in the spec, or all possible entity IDs? */
  if (sensor_type != IPMI_SENSOR_TYPE_TEMPERATURE
      || !IPMI_DCMI_ENTITY_ID_VALID(entity_id)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_dcmi_sensor_info_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_sensor_info_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_dcmi_sensor_info (sensor_type,
                                          entity_id,
                                          entity_instance,
                                          entity_instance_start,
                                          obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_power_reading (ipmi_ctx_t ctx,
                                 uint8_t mode,
                                 uint8_t mode_attributes,
                                 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_POWER_READING_MODE_VALID (mode)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_power_reading_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_power_reading_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_power_reading (mode,
                                       mode_attributes,
                                       obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_get_power_limit (ipmi_ctx_t ctx,
                               fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_get_power_limit_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_get_power_limit_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_get_power_limit (obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_set_power_limit (ipmi_ctx_t ctx,
                               uint8_t exception_actions,
                               uint16_t power_limit_requested,
                               uint32_t correction_time_limit,
                               uint16_t management_application_statistics_sampling_period,
                               fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_EXCEPTION_ACTION_VALID (exception_actions)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_set_power_limit_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_set_power_limit_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_set_power_limit (exception_actions,
                                     power_limit_requested,
                                     correction_time_limit,
                                     management_application_statistics_sampling_period,
                                     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
ipmi_cmd_dcmi_activate_deactivate_power_limit (ipmi_ctx_t ctx,
                                               uint8_t power_limit_activation,
                                               fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_DCMI_POWER_LIMIT_ACTIVATION_VALID (power_limit_activation)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rs,
                                 tmpl_cmd_dcmi_activate_deactivate_power_limit_rs) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_cmd_dcmi_activate_deactivate_power_limit_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_cmd_dcmi_activate_deactivate_power_limit (power_limit_activation,
                                                     obj_cmd_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (api_ipmi_cmd (ctx,
                    IPMI_BMC_IPMB_LUN_BMC,
                    IPMI_NET_FN_GROUP_EXTENSION_RQ,
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
