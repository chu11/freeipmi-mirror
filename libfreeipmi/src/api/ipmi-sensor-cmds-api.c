/* 
   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/api/ipmi-sensor-cmds-api.h"
#include "freeipmi/ipmi-sensor-cmds.h"
#include "freeipmi/ipmi-ipmb-interface.h"
#include "freeipmi/ipmi-netfn-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t
ipmi_cmd_get_sensor_reading_threshold (ipmi_ctx_t ctx, 
				       uint8_t sensor_number, 
				       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_reading_threshold_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_reading_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_sensor_reading (sensor_number,
						  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sensor_reading_discrete (ipmi_ctx_t ctx, 
				      uint8_t sensor_number, 
				      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_reading_discrete_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_reading_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_sensor_reading (sensor_number,
						  obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sensor_thresholds (ipmi_ctx_t ctx, 
				uint8_t sensor_number, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rs));
  
  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sensor_thresholds_rs);

  API_FIID_OBJ_CREATE(obj_cmd_rq, tmpl_cmd_get_sensor_thresholds_rq);

  API_ERR_CLEANUP (!(fill_cmd_get_sensor_thresholds (sensor_number,
						     obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx, 
			    IPMI_BMC_IPMB_LUN_BMC, 
			    IPMI_NET_FN_SENSOR_EVENT_RQ, 
			    obj_cmd_rq, 
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

