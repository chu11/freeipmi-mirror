/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

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
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/fru-parse/ipmi-fru-parse.h"
#include "freeipmi/api/ipmi-fru-inventory-device-cmds-api.h"
#include "freeipmi/cmds/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-fru-information-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-fru-parse-common.h"
#include "ipmi-fru-parse-defs.h"
#include "ipmi-fru-parse-trace.h"
#include "ipmi-fru-parse-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

static char *ipmi_fru_parse_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "device id not open",
    "device id already open",
    "no FRU information",
    "common header checksum invalid",
    "out of memory",
    "internal IPMI error",
    "internal system error",
    "internal error",
    "errnum out of range",
    NULL
  };

static void
_init_fru_parsing_data (ipmi_fru_parse_ctx_t ctx)
{
  ctx->device_opened = 0;
  ctx->fru_device_id = 0;
  ctx->fru_inventory_area_size = 0;
}

ipmi_fru_parse_ctx_t
ipmi_fru_parse_ctx_create (ipmi_ctx_t ipmi_ctx)
{
  struct ipmi_fru_parse_ctx *ctx = NULL;

  if (!ipmi_ctx)
    {
      SET_ERRNO (EINVAL);
      return (NULL);
    }
 
  if (!(ctx = (ipmi_fru_parse_ctx_t)malloc (sizeof (struct ipmi_fru_parse_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_fru_parse_ctx));
  ctx->magic = IPMI_FRU_PARSE_CTX_MAGIC;
  ctx->flags = IPMI_FRU_PARSE_FLAGS_DEFAULT;
  ctx->debug_prefix = NULL;
  
  ctx->ipmi_ctx = ipmi_ctx;
  _init_fru_parsing_data (ctx);

  return (ctx);
}

void
ipmi_fru_parse_ctx_destroy (ipmi_fru_parse_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return;
    }

  if (ctx->debug_prefix)
    free (ctx->debug_prefix);

  ctx->magic = ~IPMI_FRU_PARSE_CTX_MAGIC;
  free (ctx);
}

int
ipmi_fru_parse_ctx_errnum (ipmi_fru_parse_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_FRU_PARSE_ERR_CONTEXT_NULL);
  else if (ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    return (IPMI_FRU_PARSE_ERR_CONTEXT_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_fru_parse_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_FRU_PARSE_ERR_SUCCESS && errnum <= IPMI_FRU_PARSE_ERR_ERRNUMRANGE)
    return (ipmi_fru_parse_errmsgs[errnum]);
  else
    return (ipmi_fru_parse_errmsgs[IPMI_FRU_PARSE_ERR_ERRNUMRANGE]);
}

char *
ipmi_fru_parse_ctx_errormsg (ipmi_fru_parse_ctx_t ctx)
{
  return (ipmi_fru_parse_ctx_strerror (ipmi_fru_parse_ctx_errnum (ctx)));
}

int
ipmi_fru_parse_ctx_get_flags (ipmi_fru_parse_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_parse_ctx_set_flags (ipmi_fru_parse_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_FRU_PARSE_FLAGS_MASK)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
  return (0);
}

char *
ipmi_fru_parse_ctx_get_debug_prefix (ipmi_fru_parse_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (NULL);
    }
  
  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
  return (ctx)->debug_prefix;
}

int
ipmi_fru_parse_ctx_set_debug_prefix (ipmi_fru_parse_ctx_t ctx, const char *debug_prefix)
{
  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }
  
  if (ctx->debug_prefix)
    {
      free (ctx->debug_prefix);
      ctx->debug_prefix = NULL;
    }

  if (debug_prefix)
    {
      if (!(ctx->debug_prefix = strdup (debug_prefix)))
        {
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_OUT_OF_MEMORY);
          return (-1);
        }
    }
  
  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_parse_open_device_id (ipmi_fru_parse_ctx_t ctx, uint8_t fru_device_id)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  fiid_obj_t fru_get_inventory_rs = NULL;
  fiid_obj_t fru_common_header = NULL;
  int32_t common_header_len;
  uint64_t format_version;
  int rv = -1;
  int ret;

  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }

  if (fru_device_id == IPMI_FRU_DEVICE_ID_RESERVED)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->device_opened)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_DEVICE_ID_ALREADY_OPEN);
      return (-1);
    }

  ctx->fru_device_id = fru_device_id;

  if (!(fru_get_inventory_rs = fiid_obj_create (tmpl_cmd_get_fru_inventory_area_info_rs)))
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (ipmi_cmd_get_fru_inventory_area_info (ctx->ipmi_ctx,
                                            ctx->fru_device_id,
                                            fru_get_inventory_rs) < 0)
    {
      /* achu: Assume this completion code means we got a FRU SDR
       * entry pointed to a device that doesn't exist on this
       * particular mother board (b/c manufacturers may use the same
       * SDR for multiple motherboards).
       */
      if (ipmi_check_completion_code (fru_get_inventory_rs, IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
        {
          FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_NO_FRU_INFORMATION);
          goto cleanup;
        }

      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_IPMI_ERROR);
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (fru_get_inventory_rs,
                    "fru_inventory_area_size",
                    &ctx->fru_inventory_area_size) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_get_inventory_rs);
      goto cleanup;
    }
  
  /* XXX - make sure initted to insure iterator doesn't return anything */
#if 0
  pstdout_printf (state_data->pstate,
                  "  FRU Inventory Area Size Empty\n");
#endif
  if (!ctx->fru_inventory_area_size)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_NO_FRU_INFORMATION);
      goto cleanup;
    }

  if ((common_header_len = fiid_template_len_bytes (tmpl_fru_common_header)) < 0)
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (ipmi_fru_parse_read_fru_data (ctx,
                                    ctx->fru_device_id,
                                    frubuf,
                                    IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                    0,
                                    common_header_len) < 0)
    goto cleanup;

  if ((ret = ipmi_fru_parse_dump_hex (ctx,
                                      frubuf,
                                      common_header_len,
                                      "Common Header")) < 0)
    goto cleanup;

  if ((ret = ipmi_fru_parse_check_checksum (ctx,
                                            frubuf,
                                            common_header_len,
                                            0)) < 0)
    goto cleanup;

#if 0
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "  FRU %s Checksum Invalid: %02Xh\n",
                   str,
                   checksum);
#endif

  if (!ret)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_COMMON_HEADER_CHECKSUM_INVALID);
      goto cleanup;
    }

  if (!(fru_common_header = fiid_obj_create (tmpl_fru_common_header)))
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (fru_common_header,
                        frubuf,
                        common_header_len) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_common_header,
                    "format_version",
                    &format_version) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_common_header,
                    "internal_use_area_starting_offset",
                    &ctx->internal_use_area_starting_offset) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_common_header,
                    "chassis_info_area_starting_offset",
                    &ctx->chassis_info_area_starting_offset) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_common_header,
                    "board_info_area_starting_offset",
                    &ctx->board_info_area_starting_offset) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_common_header,
                    "product_info_area_starting_offset",
                    &ctx->product_info_area_starting_offset) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_common_header,
                    "multirecord_area_starting_offset",
                    &ctx->multirecord_area_starting_offset) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }



  rv = 0;
  ctx->device_opened = 1;
  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
 cleanup:
  if (rv < 0)
    _init_fru_parsing_data (ctx);
  fiid_obj_destroy (fru_get_inventory_rs);
  fiid_obj_destroy (fru_common_header);
  return (rv); 
}

int
ipmi_fru_parse_close_device_id (ipmi_fru_parse_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->device_opened)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_DEVICE_ID_NOT_OPEN);
      return (-1);
    }

  _init_fru_parsing_data (ctx);

  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_parse_first (ipmi_fru_parse_ctx_t ctx)
{
  return (0);
}

int
ipmi_fru_parse_next (ipmi_fru_parse_ctx_t ctx)
{
  return (0);
}

int
ipmi_fru_parse_read_data_area (ipmi_fru_parse_ctx_t ctx,
                               unsigned int *area_type,
                               uint8_t *buf,
                               unsigned int buflen)
{
  return (0);
}
