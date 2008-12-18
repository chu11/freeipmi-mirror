/*****************************************************************************\
 *  $Id: ipmi-sel-parse.c,v 1.1.2.3 2008-12-18 23:38:34 chu11 Exp $
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
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-defs.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

static char *ipmi_sel_parse_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "sdr cache permission error",
    "sdr cache filesystem error",
    "sdr cache error",
    "no sel entries available",
    "not found",
    "callback error",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sel_parse_ctx_t
ipmi_sel_parse_ctx_create(ipmi_ctx_t ipmi_ctx, ipmi_sdr_cache_ctx_t sdr_cache_ctx)
{
  struct ipmi_sel_parse_ctx *ctx = NULL;

  ERR_CLEANUP((ctx = (ipmi_sel_parse_ctx_t)malloc(sizeof(struct ipmi_sel_parse_ctx))));
  memset(ctx, '\0', sizeof(struct ipmi_sel_parse_ctx));
  ctx->magic = IPMI_SEL_PARSE_MAGIC;
  ctx->flags = IPMI_SEL_PARSE_FLAGS_DEFAULT;
  ctx->debug_prefix = NULL;

  ctx->ipmi_ctx = ipmi_ctx;
  ctx->sdr_cache_ctx = sdr_cache_ctx;

  if (!(ctx->sel_entries = list_create((ListDelF)free)))
    goto cleanup;

  return ctx;

 cleanup:
  if (ctx)
    free(ctx);
  return NULL;
}

void
ipmi_sel_parse_ctx_destroy(ipmi_sel_parse_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_PARSE_MAGIC)
    return;

  ctx->magic = ~IPMI_SEL_PARSE_MAGIC;
  if (ctx->debug_prefix)
    free(ctx->debug_prefix);
  free(ctx);
}

int 
ipmi_sel_parse_ctx_errnum(ipmi_sel_parse_ctx_t ctx)
{
  if (!ctx)
    return IPMI_SEL_PARSE_CTX_ERR_CONTEXT_NULL;
  else if (ctx->magic != IPMI_SEL_PARSE_MAGIC)
    return IPMI_SEL_PARSE_CTX_ERR_CONTEXT_INVALID;
  else
    return ctx->errnum;
}

char *
ipmi_sel_parse_ctx_strerror(int errnum)
{
  if (errnum >= IPMI_SEL_PARSE_CTX_ERR_SUCCESS && errnum <= IPMI_SEL_PARSE_CTX_ERR_ERRNUMRANGE)
    return ipmi_sel_parse_errmsgs[errnum];
  else
    return ipmi_sel_parse_errmsgs[IPMI_SEL_PARSE_CTX_ERR_ERRNUMRANGE];
}

int
ipmi_sel_parse_ctx_get_flags(ipmi_sel_parse_ctx_t ctx, unsigned int *flags)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(flags);

  *flags = ctx->flags;
  return 0;
}

int
ipmi_sel_parse_ctx_set_flags(ipmi_sel_parse_ctx_t ctx, unsigned int flags)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(!(flags & ~IPMI_SEL_PARSE_FLAGS_DEBUG_DUMP));

  ctx->flags = flags;
  return 0;
}

char *
ipmi_sel_parse_ctx_get_debug_prefix(ipmi_sel_parse_ctx_t ctx)
{
  ERR_NULL_RETURN(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  return ctx->debug_prefix;
}

int
ipmi_sel_parse_ctx_set_debug_prefix(ipmi_sel_parse_ctx_t ctx, const char *prefix)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  if (ctx->debug_prefix)
    {
      free(ctx->debug_prefix);
      ctx->debug_prefix = NULL;
    }

  if (prefix)
    SEL_PARSE_ERR_OUT_OF_MEMORY((ctx->debug_prefix = strdup(prefix)));

  return 0;
}

static int
_sel_reservation_id(ipmi_sel_parse_ctx_t ctx,
                    uint16_t *reservation_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(reservation_id);

  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_cmd_reserve_sel_rs);

  if (ipmi_cmd_reserve_sel (ctx->ipmi_ctx, obj_cmd_rs) < 0)
    {
      /* achu:
       *
       * IPMI spec states reservation ID 0000h should be used if Reserve SEL 
       * is not supported.  
       */
      if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND
          && ipmi_check_completion_code(obj_cmd_rs,
                                        IPMI_COMP_CODE_COMMAND_INVALID) == 1)
        {
          *reservation_id = 0;
          goto out;
        }
      
      SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
      goto cleanup;
    }

  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_cmd_rs,
                                 "reservation_id",
                                 &val);
  *reservation_id = val;

 out:
  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

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
      if (_sel_reservation_id(ctx, &reservation_id) < 0)
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

              if (_sel_reservation_id(ctx, &reservation_id) < 0)
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
      if (_sel_reservation_id(ctx, &reservation_id) < 0)
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

              if (_sel_reservation_id(ctx, &reservation_id) < 0)
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
