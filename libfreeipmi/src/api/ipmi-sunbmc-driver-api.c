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
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/driver/ipmi-sunbmc-driver.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"
#include "ipmi-sunbmc-driver-api.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_sunbmc_raw =
  {
    {8,    "cmd",      FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8192, "raw_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

int8_t 
ipmi_sunbmc_cmd_api (ipmi_ctx_t ctx,
                     fiid_obj_t obj_cmd_rq,
                     fiid_obj_t obj_cmd_rs)
{
  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (fiid_obj_valid(obj_cmd_rq)
                      && fiid_obj_valid(obj_cmd_rs));

  API_FIID_OBJ_PACKET_VALID(obj_cmd_rq);

  API_ERR_SUNBMC (!(ipmi_sunbmc_cmd (ctx->io.inband.sunbmc_ctx,
                                     ctx->lun,
                                     ctx->net_fn,
                                     obj_cmd_rq,
                                     obj_cmd_rs) < 0));

  return (0);
}

int32_t
ipmi_sunbmc_cmd_raw_api (ipmi_ctx_t ctx,
                         uint8_t *buf_rq,
                         size_t buf_rq_len,
                         uint8_t *buf_rs,
                         size_t buf_rs_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int32_t len;
  int32_t rv = -1;

  API_ERR_CTX_CHECK (ctx && ctx->magic == IPMI_CTX_MAGIC);

  API_ERR_PARAMETERS (buf_rq 
                      && buf_rq_len > 0
                      && buf_rs 
                      && buf_rs_len > 0);

  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rq, tmpl_sunbmc_raw);
  API_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_sunbmc_raw);

  API_FIID_OBJ_SET_ALL_CLEANUP(obj_cmd_rq, buf_rq, buf_rq_len);

  API_ERR_SUNBMC_CLEANUP (!(ipmi_sunbmc_cmd (ctx->io.inband.sunbmc_ctx,
                                             ctx->lun,
                                             ctx->net_fn,
                                             obj_cmd_rq,
                                             obj_cmd_rs) < 0));

  API_FIID_OBJ_GET_ALL_LEN_CLEANUP(len, obj_cmd_rs, buf_rs, buf_rs_len);

  rv = len;
 cleanup:
  API_FIID_OBJ_DESTROY(obj_cmd_rq);
  API_FIID_OBJ_DESTROY(obj_cmd_rs);
  return (rv);
}
