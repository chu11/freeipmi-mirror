/*****************************************************************************\
 *  $Id: ipmi-fru-inventory-device-cmds-api.c,v 1.5 2008-08-12 18:14:43 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
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

#include "freeipmi/api/ipmi-fru-inventory-device-cmds-api.h"
#include "freeipmi/cmds/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cmd_get_fru_inventory_area_info (ipmi_ctx_t ctx, 
                                      uint8_t fru_device_id,
                                      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_fru_inventory_area_info_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_fru_inventory_area_info_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_fru_inventory_area_info (fru_device_id, 
                                                           obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
                            IPMI_BMC_IPMB_LUN_BMC, 
                            IPMI_NET_FN_STORAGE_RQ, 
                            obj_cmd_rq, 
                            obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_read_fru_data (ipmi_ctx_t ctx, 
                        uint8_t fru_device_id,
                        uint16_t fru_inventory_offset_to_read,
                        uint8_t count_to_read,
                        fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_read_fru_data_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_read_fru_data_rq);

  API_ERR_CLEANUP (!(fill_cmd_read_fru_data (fru_device_id, 
                                             fru_inventory_offset_to_read,
                                             count_to_read,
                                             obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
                            IPMI_BMC_IPMB_LUN_BMC, 
                            IPMI_NET_FN_STORAGE_RQ, 
                            obj_cmd_rq, 
                            obj_cmd_rs);
  
  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}
