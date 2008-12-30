/*****************************************************************************\
 *  $Id: ipmi-sel-parse-util.c,v 1.1.2.1 2008-12-30 17:59:15 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "freeipmi/api/ipmi-sel-cmds-api.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-string.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

int 
ipmi_sel_parse_clear_sel(ipmi_sel_parse_ctx_t ctx)
{
  unsigned int reservation_id_retry_count = 0;
  uint16_t reservation_id;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_clear_sel_rs);

  while (1)
    {
      if (sel_parse_get_reservation_id(ctx, &reservation_id) < 0)
        goto cleanup;

      if (ipmi_cmd_clear_sel (ctx->ipmi_ctx,
                              reservation_id,
                              IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE,
                              obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code(obj_cmd_rs,
                                            IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
              reservation_id_retry_count++;
              
              if (reservation_id_retry_count > IPMI_SEL_PARSE_RESERVATION_ID_RETRY)
                {
                  SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
                  goto cleanup;
                }

              if (sel_parse_get_reservation_id(ctx, &reservation_id) < 0)
                goto cleanup;

              continue;
            }
          else
            {
              SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      break;
    }

  rv = 0;
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}  

int 
ipmi_sel_parse_delete_sel_entry(ipmi_sel_parse_ctx_t ctx, uint16_t record_id)
{
  unsigned int reservation_id_retry_count = 0;
  uint16_t reservation_id;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(record_id > IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
                           && record_id < IPMI_SEL_GET_RECORD_ID_LAST_ENTRY);

  SEL_PARSE_FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_delete_sel_entry_rs);

  while (1)
    {
      if (sel_parse_get_reservation_id(ctx, &reservation_id) < 0)
        goto cleanup;

      if (ipmi_cmd_delete_sel_entry (ctx->ipmi_ctx,
                                     reservation_id,
                                     record_id,
                                     obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
              && (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1
                  || ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
            {
              SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_NOT_FOUND);
              goto cleanup;
            }
          else if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
                   && ipmi_check_completion_code(obj_cmd_rs,
                                                 IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
              reservation_id_retry_count++;
              
              if (reservation_id_retry_count > IPMI_SEL_PARSE_RESERVATION_ID_RETRY)
                {
                  SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
                  goto cleanup;
                }

              if (sel_parse_get_reservation_id(ctx, &reservation_id) < 0)
                goto cleanup;
              
              continue;
            }
          else
            {
              SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      break;
    }

  rv = 0;
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

int
ipmi_sel_parse_format_record_string(ipmi_sel_parse_ctx_t ctx,
                                    char *fmt,
                                    uint8_t *record_buf,
                                    unsigned int record_buflen,
                                    uint8_t *buf,
                                    unsigned int buflen,
                                    unsigned int flags)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(fmt 
                           && record_buf
                           && record_buflen
                           && buf 
                           && buflen
                           && !(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));
  
  if (record_buflen < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      return -1;
    }
  
  return sel_parse_format_record_string(ctx,
                                        fmt,
                                        record_buf,
                                        record_buflen,
                                        buf,
                                        buflen,
                                        flags);
}

int
ipmi_sel_record_type_class(uint8_t record_type)
{
  if (IPMI_SEL_RECORD_TYPE_IS_EVENT(record_type))
    return IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD;

  if (IPMI_SEL_RECORD_TYPE_IS_TIMESTAMPED_OEM(record_type))
    return IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD;

  if (IPMI_SEL_RECORD_TYPE_IS_NON_TIMESTAMPED_OEM(record_type))
    return IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD;

  return IPMI_SEL_RECORD_TYPE_CLASS_UNKNOWN;
}

