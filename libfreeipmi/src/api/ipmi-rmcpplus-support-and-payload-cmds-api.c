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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif	/* !TIME_WITH_SYS_TIME */

#include "freeipmi/api/ipmi-rmcpplus-support-and-payload-cmds-api.h"
#include "freeipmi/cmds/ipmi-rmcpplus-support-and-payload-cmds.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cmd_set_user_payload_access (ipmi_ctx_t ctx, 
                                  uint8_t channel_number,
                                  uint8_t user_id,
                                  uint8_t operation,
                                  uint8_t standard_payload_1,
                                  uint8_t standard_payload_2,
                                  uint8_t standard_payload_3,
                                  uint8_t standard_payload_4,
                                  uint8_t standard_payload_5,
                                  uint8_t standard_payload_6,
                                  uint8_t standard_payload_7,
                                  uint8_t oem_payload_0,
                                  uint8_t oem_payload_1,
                                  uint8_t oem_payload_2,
                                  uint8_t oem_payload_3,
                                  uint8_t oem_payload_4,
                                  uint8_t oem_payload_5,
                                  uint8_t oem_payload_6,
                                  uint8_t oem_payload_7,
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && IPMI_SET_USER_PAYLOAD_OPERATION_VALID(operation)
                      && IPMI_PAYLOAD_ACCESS_VALID(standard_payload_1)
                      && IPMI_PAYLOAD_ACCESS_VALID(standard_payload_2)
                      && IPMI_PAYLOAD_ACCESS_VALID(standard_payload_3)
                      && IPMI_PAYLOAD_ACCESS_VALID(standard_payload_4)
                      && IPMI_PAYLOAD_ACCESS_VALID(standard_payload_5)
                      && IPMI_PAYLOAD_ACCESS_VALID(standard_payload_6)
                      && IPMI_PAYLOAD_ACCESS_VALID(standard_payload_7)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_0)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_1)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_2)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_3)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_4)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_5)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_6)
                      && IPMI_PAYLOAD_ACCESS_VALID(oem_payload_7)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_user_payload_access_rs);

  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_user_payload_access_rq);

  API_ERR_CLEANUP (!(fill_cmd_set_user_payload_access (channel_number,
						       user_id,
						       operation,
						       standard_payload_1,
						       standard_payload_2,
						       standard_payload_3,
						       standard_payload_4,
						       standard_payload_5,
						       standard_payload_6,
						       standard_payload_7,
						       oem_payload_0,
						       oem_payload_1,
						       oem_payload_2,
						       oem_payload_3,
						       oem_payload_4,
						       oem_payload_5,
						       oem_payload_6,
						       oem_payload_7,
						       obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx,
			    IPMI_BMC_IPMB_LUN_BMC,
			    IPMI_NET_FN_APP_RQ,
			    obj_cmd_rq,
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_cmd_get_user_payload_access (ipmi_ctx_t ctx,
                                  uint8_t channel_number,
                                  uint8_t user_id,
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (IPMI_CHANNEL_NUMBER_VALID(channel_number)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_user_payload_access_rs);
  
  API_FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_user_payload_access_rq);
  
  API_ERR_CLEANUP (!(fill_cmd_get_user_payload_access (channel_number,
						       user_id,
						       obj_cmd_rq) < 0));

  API_ERR_IPMI_CMD_CLEANUP (ctx,
			    IPMI_BMC_IPMB_LUN_BMC,
			    IPMI_NET_FN_APP_RQ,
			    obj_cmd_rq,
			    obj_cmd_rs);

  rv = 0;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  return (rv);
}
