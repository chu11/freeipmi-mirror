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
    "chassis info area checksum invalid",
    "board info area checksum invalid",
    "product info area checksum invalid",
    "common header format invalid",
    "chassis info area format invalid",
    "board info area format invalid",
    "product info area format invalid",
    "multirecord area format invalid",
    "FRU information inconsistent",
    "buffer too small to hold result",
    "out of memory",
    "internal IPMI error",
    "internal system error",
    "internal error",
    "errnum out of range",
    NULL
  };

static void
_init_fru_parsing_iterator_data (ipmi_fru_parse_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);

  ctx->chassis_info_area_parsed = 0;
  ctx->board_info_area_parsed = 0;
  ctx->product_info_area_parsed = 0;
  ctx->multirecord_area_parsed = 0;
  ctx->multirecord_area_offset_in_bytes = 0;
}

static void
_init_fru_parsing_data (ipmi_fru_parse_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  
  ctx->fru_device_id = 0;
  ctx->fru_inventory_area_size = 0;
  ctx->chassis_info_area_starting_offset = 0;
  ctx->board_info_area_starting_offset = 0;
  ctx->product_info_area_starting_offset = 0;
  ctx->multirecord_area_starting_offset = 0;
  ctx->device_opened = 0;

  _init_fru_parsing_iterator_data (ctx);
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
  uint8_t format_version;
  uint64_t val;
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
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_get_inventory_rs);
      goto cleanup;
    }
  ctx->fru_inventory_area_size = val;

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
                                    frubuf,
                                    IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                    0,
                                    common_header_len) < 0)
    goto cleanup;

  if (ipmi_fru_parse_dump_hex (ctx,
                               frubuf,
                               common_header_len,
                               "Common Header") < 0)
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
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }
  format_version = val;

  if (FIID_OBJ_GET (fru_common_header,
                    "chassis_info_area_starting_offset",
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }
  ctx->chassis_info_area_starting_offset = val;

  if (FIID_OBJ_GET (fru_common_header,
                    "board_info_area_starting_offset",
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }
  ctx->board_info_area_starting_offset = val;

  if (FIID_OBJ_GET (fru_common_header,
                    "product_info_area_starting_offset",
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }
  ctx->product_info_area_starting_offset = val;

  if (FIID_OBJ_GET (fru_common_header,
                    "multirecord_area_starting_offset",
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_common_header);
      goto cleanup;
    }
  ctx->multirecord_area_starting_offset = val;

  if (format_version != IPMI_FRU_COMMON_HEADER_FORMAT_VERSION)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_COMMON_HEADER_FORMAT_INVALID);
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
  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }

  _init_fru_parsing_iterator_data (ctx);
  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
  return (0);
}

static int
_parse_multirecord_header (ipmi_fru_parse_ctx_t ctx,
                           unsigned int *__multirecord_header_length,
                           unsigned int *record_type_id,
                           unsigned int *record_format_version,
                           unsigned int *end_of_list,
                           unsigned int *record_length,
                           unsigned int *record_checksum)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1]; 
  fiid_obj_t fru_multirecord_header = NULL;
  int32_t multirecord_header_length;
  uint64_t val;
  int ret;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (record_type_id
          || record_format_version
          || end_of_list
          || record_length
          || record_checksum);

  if ((multirecord_header_length = fiid_template_len_bytes (tmpl_fru_multirecord_area_header)) < 0)
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (__multirecord_header_length)
    (*__multirecord_header_length) = multirecord_header_length;
      
  if ((ctx->multirecord_area_offset_in_bytes + multirecord_header_length) > ctx->fru_inventory_area_size)
    {
#if 0
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "  FRU MultirecordInfo Info Area size too small\n");
#endif
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }
  
  if (ipmi_fru_parse_read_fru_data (ctx,
                                    frubuf,
                                    IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                    ctx->multirecord_area_offset_in_bytes,
                                    multirecord_header_length) < 0)
    goto cleanup;
      
  if (ipmi_fru_parse_dump_hex (ctx,
                               frubuf,
                               multirecord_header_length,
                               "MultiRecord Header") < 0)
    goto cleanup;

  if ((ret = ipmi_fru_parse_check_checksum (ctx,
                                            frubuf,
                                            multirecord_header_length,
                                            0)) < 0)
    goto cleanup;

  if (!ret)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_MULTIRECORD_AREA_CHECKSUM_INVALID);
      goto cleanup;
    }
  
  if (!(fru_multirecord_header = fiid_obj_create (tmpl_fru_multirecord_area_header)))
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (fru_multirecord_header,
                        frubuf,
                        multirecord_header_length) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_multirecord_header);
      goto cleanup;
    }
  
  if (record_type_id)
    {
      if (FIID_OBJ_GET (fru_multirecord_header,
                        "record_type_id",
                        &val) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_multirecord_header);
          goto cleanup;
        }
      (*record_type_id) = val;
    }

  if (record_format_version)
    {
      if (FIID_OBJ_GET (fru_multirecord_header,
                        "record_format_version",
                        &val) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_multirecord_header);
          goto cleanup;
        }
      (*record_format_version) = val;
    }

  if (end_of_list)
    {
      if (FIID_OBJ_GET (fru_multirecord_header,
                        "end_of_list",
                        &val) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_multirecord_header);
          goto cleanup;
        }
      (*end_of_list) = val;
    }

  if (record_length)
    {
      if (FIID_OBJ_GET (fru_multirecord_header,
                        "record_length",
                        &val) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_multirecord_header);
          goto cleanup;
        }
      (*record_length) = val;
    }

  if (record_checksum)
    {
      if (FIID_OBJ_GET (fru_multirecord_header,
                        "record_checksum",
                        &val) < 0)
        {
          FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_multirecord_header);
          goto cleanup;
        }
      (*record_checksum) = val;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (fru_multirecord_header);
  return (rv);
}

int
ipmi_fru_parse_next (ipmi_fru_parse_ctx_t ctx)
{
  int rv = 0;

  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
    {
      ctx->chassis_info_area_parsed++;
      rv = 1;
    }
  if (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
    {
      ctx->board_info_area_parsed++;
      rv = 1;
    }
  if (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed)
    {
      ctx->product_info_area_parsed++;
      rv = 1;
    }
  if (ctx->multirecord_area_starting_offset && !ctx->multirecord_area_parsed)
    {
      unsigned int multirecord_header_length;
      unsigned int record_length;
      unsigned int end_of_list;

      /* Special case, user is iterator-ing and wants to skip the first multirecord_area */
      if (!ctx->multirecord_area_offset_in_bytes)
        ctx->multirecord_area_offset_in_bytes = ctx->multirecord_area_starting_offset * 8;    

      if (_parse_multirecord_header (ctx,
                                     &multirecord_header_length,
                                     NULL,
                                     NULL,
                                     &end_of_list,
                                     &record_length,
                                     NULL) < 0)
        return (-1);

      if (end_of_list)
        {
          ctx->multirecord_area_parsed++;
          goto out;
        }
      
      ctx->multirecord_area_offset_in_bytes += multirecord_header_length;
      /* if record_length is 0, that's ok still */
      ctx->multirecord_area_offset_in_bytes += record_length;

      rv = 1;
    }

 out:  
  ctx->errnum = IPMI_FRU_PARSE_ERR_SUCCESS;
  return (rv);
}

static int
_read_info_area_data (ipmi_fru_parse_ctx_t ctx,
                      unsigned int *area_type,
                      unsigned int *area_length,
                      uint8_t *buf,
                      unsigned int buflen)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1]; 
  fiid_obj_t fru_info_area_header = NULL;
  int32_t info_area_header_length;
  unsigned int info_area_length;
  unsigned int info_area_length_bytes;
  uint8_t expected_format_version;
  unsigned int err_code_format_invalid;
  unsigned int err_code_checksum_invalid;
  unsigned int info_area_starting_offset;
  char *headerhdrstr;
  char *areahdrstr;
  unsigned int info_area_type;
  unsigned int offset_in_bytes;
  uint64_t format_version;
  uint64_t val;
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (area_type);
  assert (area_length);
  assert (buf);
  assert (buflen);
  assert ((ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
          || (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
          || (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed));

  if (ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
    {
      expected_format_version = IPMI_FRU_CHASSIS_INFO_AREA_FORMAT_VERSION;
      err_code_format_invalid = IPMI_FRU_PARSE_ERR_CHASSIS_INFO_AREA_FORMAT_INVALID;
      err_code_checksum_invalid = IPMI_FRU_PARSE_ERR_CHASSIS_INFO_AREA_CHECKSUM_INVALID;
      info_area_starting_offset = ctx->chassis_info_area_starting_offset;
      headerhdrstr = "Chassis Info Header";
      areahdrstr = "Chassis Info Area";
      info_area_type = IPMI_FRU_PARSE_AREA_TYPE_CHASSIS_INFO_AREA;
    }
  else if (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
    {
      expected_format_version = IPMI_FRU_BOARD_INFO_AREA_FORMAT_VERSION;
      err_code_format_invalid = IPMI_FRU_PARSE_ERR_BOARD_INFO_AREA_FORMAT_INVALID;
      err_code_checksum_invalid = IPMI_FRU_PARSE_ERR_BOARD_INFO_AREA_CHECKSUM_INVALID;
      info_area_starting_offset = ctx->board_info_area_starting_offset;
      headerhdrstr = "Board Info Header";
      areahdrstr = "Board Info Area";
      info_area_type = IPMI_FRU_PARSE_AREA_TYPE_BOARD_INFO_AREA;
    }
  else /* (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed) */
    {
      expected_format_version = IPMI_FRU_PRODUCT_INFO_AREA_FORMAT_VERSION;
      err_code_format_invalid = IPMI_FRU_PARSE_ERR_PRODUCT_INFO_AREA_FORMAT_INVALID;
      err_code_checksum_invalid = IPMI_FRU_PARSE_ERR_PRODUCT_INFO_AREA_CHECKSUM_INVALID;
      info_area_starting_offset = ctx->product_info_area_starting_offset;
      headerhdrstr = "Product Info Header";
      areahdrstr = "Product Info Area";
      info_area_type = IPMI_FRU_PARSE_AREA_TYPE_PRODUCT_INFO_AREA;
    }

  offset_in_bytes = info_area_starting_offset * 8;

  if ((info_area_header_length = fiid_template_len_bytes (tmpl_fru_info_area_header)) < 0)
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((offset_in_bytes + info_area_header_length) > ctx->fru_inventory_area_size)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }

  if (ipmi_fru_parse_read_fru_data (ctx,
                                    frubuf,
                                    IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                    offset_in_bytes,
                                    info_area_header_length) < 0)
    goto cleanup;

  if (!(fru_info_area_header = fiid_obj_create (tmpl_fru_info_area_header)))
    {
      FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (fru_info_area_header,
                        frubuf,
                        info_area_header_length) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_info_area_header,
                    "format_version",
                    &format_version) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }

  if (format_version != expected_format_version)
    {
#if 0
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "  FRU %s Area Format Unknown: %02Xh\n",
                       str,
                       format_version);
#endif
      FRU_PARSE_SET_ERRNUM (ctx, err_code_format_invalid);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_info_area_header,
                    "info_area_length",
                    &val) < 0)
    {
      FRU_PARSE_FIID_OBJECT_ERROR_TO_FRU_PARSE_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }
  info_area_length = val;
  info_area_length_bytes = info_area_length * 8;

  if (!info_area_length)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  if (ctx->fru_inventory_area_size < (offset_in_bytes + info_area_length_bytes))
    {
#if 0
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "  FRU %s Info Area too small\n",
                       str);
#endif
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }
 
  if (ipmi_fru_parse_read_fru_data (ctx,
                                    frubuf,
                                    IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                    info_area_starting_offset * 8,
                                    info_area_length_bytes) < 0)
    goto cleanup;
  
  if (ipmi_fru_parse_dump_hex (ctx,
                               frubuf,
                               info_area_length_bytes,
                               areahdrstr) < 0)
    goto cleanup;
  
  if ((ret = ipmi_fru_parse_check_checksum (ctx,
                                            frubuf,
                                            info_area_length_bytes,
                                            0)) < 0)
    goto cleanup;
  
  if (!ret)
    {
      FRU_PARSE_SET_ERRNUM (ctx, err_code_checksum_invalid);
      goto cleanup;
    }
  
  if (buflen < info_area_length_bytes)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_OVERFLOW);
      goto cleanup;
    }

  (*area_type) = info_area_type;
  (*area_length) = info_area_length_bytes;
  memcpy (buf, frubuf, info_area_length_bytes);
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (fru_info_area_header);
  return (rv);
}

static int
_read_multirecord_area_data (ipmi_fru_parse_ctx_t ctx,
                             unsigned int *area_type,
                             unsigned int *area_length,
                             uint8_t *buf,
                             unsigned int buflen)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  unsigned int multirecord_header_length = 0;
  unsigned int record_type_id = 0;
  unsigned int record_format_version = 0;
  unsigned int record_length = 0;
  unsigned int record_checksum = 0;
  int ret;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (area_type);
  assert (area_length);
  assert (buf);
  assert (buflen);
  assert (ctx->multirecord_area_starting_offset && !ctx->multirecord_area_parsed);

  if (!ctx->multirecord_area_offset_in_bytes)
    ctx->multirecord_area_offset_in_bytes = ctx->multirecord_area_starting_offset * 8;

  if (_parse_multirecord_header (ctx,
                                 &multirecord_header_length,
                                 &record_type_id,
                                 &record_format_version,
                                 NULL,
                                 &record_length,
                                 &record_checksum) < 0)
    goto cleanup;

  if (record_format_version != IPMI_FRU_MULTIRECORD_AREA_FORMAT_VERSION)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_MULTIRECORD_AREA_FORMAT_INVALID);
      goto cleanup;
    }

  if (!record_length)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  /* Note: Unlike Info Areas, record_length is in bytes */
  if (ctx->fru_inventory_area_size < (ctx->multirecord_area_offset_in_bytes + multirecord_header_length + record_length))
    {
#if 0
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "  FRU Multirecord Info Area too small\n");
#endif
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  if (ipmi_fru_parse_read_fru_data (ctx,
                                    frubuf,
                                    IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                    ctx->multirecord_area_offset_in_bytes + multirecord_header_length,
                                    record_length) < 0)
    goto cleanup;

  if (ipmi_fru_parse_dump_hex (ctx,
                               frubuf,
                               record_length,
                               "MultiRecord") < 0)
    goto cleanup;
  
  if ((ret = ipmi_fru_parse_check_checksum (ctx,
                                            frubuf,
                                            record_length,
                                            record_checksum)) < 0)
    goto cleanup;
  
  if (!ret)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_MULTIRECORD_AREA_CHECKSUM_INVALID);
      goto cleanup;
    }

  if (buflen < record_length)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_OVERFLOW);
      goto cleanup;
    }

  if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_POWER_SUPPLY_INFORMATION)
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_POWER_SUPPLY_INFORMATION;
  else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_DC_OUTPUT)
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_DC_OUTPUT;
  else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_DC_LOAD)
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_DC_LOAD;
  else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_MANAGEMENT_ACCESS_RECORD)
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_MANAGEMENT_ACCESS_RECORD;
  else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_BASE_COMPATIBILITY_RECORD)
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_BASE_COMPATABILITY_RECORD;
  else if (record_type_id == IPMI_FRU_MULTIRECORD_AREA_TYPE_EXTENDED_COMPATIBILITY_RECORD)
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_EXTENDED_COMPATABILITY_RECORD;
  else if (IPMI_FRU_MULTIRECORD_AREA_TYPE_IS_OEM (record_type_id))
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_OEM;
  else
    (*area_type) = IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_UNKNOWN;
    
  (*area_length) = record_length;
  memcpy (buf, frubuf, record_length);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_parse_read_data_area (ipmi_fru_parse_ctx_t ctx,
                               unsigned int *area_type,
                               unsigned int *area_length,
                               uint8_t *buf,
                               unsigned int buflen)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_PARSE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_parse_ctx_errormsg (ctx), ipmi_fru_parse_ctx_errnum (ctx));
      return (-1);
    }

  if (!area_type
      || !area_length
      || !buf
      || !buflen)
    {
      FRU_PARSE_SET_ERRNUM (ctx, IPMI_FRU_PARSE_ERR_PARAMETERS);
      return (-1);
    }

  if ((ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
      || (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
      || (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed))
    {
      if (_read_info_area_data (ctx,
                                area_type,
                                area_length,
                                buf,
                                buflen) < 0)
        goto cleanup;

      goto out;
    }

  if (ctx->multirecord_area_starting_offset && !ctx->multirecord_area_parsed)
    {
      if (_read_multirecord_area_data (ctx,
                                       area_type,
                                       area_length,
                                       buf,
                                       buflen) < 0)
        goto cleanup;

      goto out;
    }

 out:
  rv = 0;
 cleanup:
  return (rv);
}
