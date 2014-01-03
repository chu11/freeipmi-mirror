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
/*****************************************************************************\
 *  $Id: ipmi-fru.c,v 1.17 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

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

#include "freeipmi/fru/ipmi-fru.h"
#include "freeipmi/api/ipmi-fru-inventory-device-cmds-api.h"
#include "freeipmi/cmds/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-fru-information-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/spec/ipmi-fru-language-codes-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-fru-common.h"
#include "ipmi-fru-defs.h"
#include "ipmi-fru-trace.h"
#include "ipmi-fru-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

#define IPMI_FRU_COUNT_TO_READ_BLOCK_SIZE  16

static char *ipmi_fru_errmsgs[] =
  {
    "success",                            /* 0 */
    "context null",                       /* 1 */
    "context invalid",                    /* 2 */
    "invalid parameters",                 /* 3 */
    "device id not open",                 /* 4 */
    "device id already open",             /* 5 */
    "no FRU information",                 /* 6 */
    "FRU area length invalid",            /* 7 */
    "common header checksum invalid",     /* 8 */
    "chassis info area checksum invalid", /* 9 */
    "board info area checksum invalid",   /* 10 */
    "product info area checksum invalid", /* 11 */
    "multirecord area checksum invalid",  /* 12 */
    "common header format invalid",       /* 13 */
    "chassis info area format invalid",   /* 14 */
    "board info area format invalid",     /* 15 */
    "product info area format invalid",   /* 16 */
    "multirecord area format invalid",    /* 17 */
    "FRU information inconsistent",       /* 18 */
    "FRU language code not supported",    /* 19 */
    "FRU invalid BCD encoding",           /* 20 */
    "FRU sentinel value not found",       /* 21 */
    "not available for this record",	  /* 22 */
    "buffer too small to hold result",    /* 23 */
    "out of memory",                      /* 24 */
    "device busy",                        /* 25 */
    "internal IPMI error",                /* 26 */
    "internal system error",              /* 27 */
    "internal error",                     /* 28 */
    "errnum out of range",                /* 29*/
    NULL
  };

static void
_init_fru_parsing_iterator_data (ipmi_fru_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);

  ctx->chassis_info_area_parsed = 0;
  ctx->board_info_area_parsed = 0;
  ctx->product_info_area_parsed = 0;
  ctx->multirecord_area_parsed = 0;
  ctx->multirecord_area_offset_in_bytes = 0;
}

static void
_init_fru_parsing_data (ipmi_fru_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  
  ctx->fru_device_id = 0;
  ctx->fru_inventory_area_size = 0;
  ctx->chassis_info_area_starting_offset = 0;
  ctx->board_info_area_starting_offset = 0;
  ctx->product_info_area_starting_offset = 0;
  ctx->multirecord_area_starting_offset = 0;
  ctx->device_opened = 0;

  _init_fru_parsing_iterator_data (ctx);
}

ipmi_fru_ctx_t
ipmi_fru_ctx_create (ipmi_ctx_t ipmi_ctx)
{
  struct ipmi_fru_ctx *ctx = NULL;

  /* check that ipmi_ctx is open for use if supplied */
  if (ipmi_ctx)
    {
      if (ipmi_ctx_get_target (ipmi_ctx, NULL, NULL) < 0)
	{
	  SET_ERRNO (EINVAL);
	  return (NULL);
	}
    }

  if (!(ctx = (ipmi_fru_ctx_t)malloc (sizeof (struct ipmi_fru_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_fru_ctx));
  ctx->magic = IPMI_FRU_CTX_MAGIC;
  ctx->flags = IPMI_FRU_FLAGS_DEFAULT;
  ctx->manufacturer_id = 0;
  ctx->product_id = 0;
  ctx->debug_prefix = NULL;
  
  ctx->ipmi_ctx = ipmi_ctx;
  _init_fru_parsing_data (ctx);

  return (ctx);
}

void
ipmi_fru_ctx_destroy (ipmi_fru_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return;
    }

  free (ctx->debug_prefix);
  ctx->magic = ~IPMI_FRU_CTX_MAGIC;
  free (ctx);
}

int
ipmi_fru_ctx_errnum (ipmi_fru_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_FRU_ERR_CONTEXT_NULL);
  else if (ctx->magic != IPMI_FRU_CTX_MAGIC)
    return (IPMI_FRU_ERR_CONTEXT_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_fru_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_FRU_ERR_SUCCESS && errnum <= IPMI_FRU_ERR_ERRNUMRANGE)
    return (ipmi_fru_errmsgs[errnum]);
  else
    return (ipmi_fru_errmsgs[IPMI_FRU_ERR_ERRNUMRANGE]);
}

char *
ipmi_fru_ctx_errormsg (ipmi_fru_ctx_t ctx)
{
  return (ipmi_fru_ctx_strerror (ipmi_fru_ctx_errnum (ctx)));
}

int
ipmi_fru_ctx_get_flags (ipmi_fru_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_ctx_set_flags (ipmi_fru_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_FRU_FLAGS_MASK)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_ctx_get_manufacturer_id (ipmi_fru_ctx_t ctx, uint32_t *manufacturer_id)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!manufacturer_id)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  *manufacturer_id = ctx->manufacturer_id;
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_ctx_set_manufacturer_id (ipmi_fru_ctx_t ctx, uint32_t manufacturer_id)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  ctx->manufacturer_id = manufacturer_id;
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_ctx_get_product_id (ipmi_fru_ctx_t ctx, uint16_t *product_id)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!product_id)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  *product_id = ctx->product_id;
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_ctx_set_product_id (ipmi_fru_ctx_t ctx, uint16_t product_id)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  ctx->product_id = product_id;
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

char *
ipmi_fru_ctx_get_debug_prefix (ipmi_fru_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (NULL);
    }
  
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (ctx)->debug_prefix;
}

int
ipmi_fru_ctx_set_debug_prefix (ipmi_fru_ctx_t ctx, const char *debug_prefix)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }
  
  free (ctx->debug_prefix);
  ctx->debug_prefix = NULL;

  if (debug_prefix)
    {
      if (!(ctx->debug_prefix = strdup (debug_prefix)))
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_OUT_OF_MEMORY);
          return (-1);
        }
    }
  
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

static int
_read_fru_data (ipmi_fru_ctx_t ctx,
                void *frubuf,
                unsigned int frubuflen,
                unsigned int offset_in_bytes,
                unsigned int fru_read_bytes)
{
  fiid_obj_t fru_read_data_rs = NULL;
  unsigned int num_bytes_read = 0;
  int len = 0;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (ctx->ipmi_ctx);
  assert (frubuf);
  assert (frubuflen);
  assert (fru_read_bytes <= frubuflen);

  if (!(fru_read_data_rs = fiid_obj_create (tmpl_cmd_read_fru_data_rs)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((offset_in_bytes + fru_read_bytes) > ctx->fru_inventory_area_size)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }

  while (num_bytes_read < fru_read_bytes)
    {
      uint8_t buf[IPMI_FRU_BUF_LEN+1];
      uint8_t count_to_read;
      uint8_t count_returned;
      uint64_t val;

      memset (buf, '\0', IPMI_FRU_BUF_LEN+1);

      if (fiid_obj_clear (fru_read_data_rs) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_read_data_rs);
          goto cleanup;
        }

      if ((fru_read_bytes - num_bytes_read) < IPMI_FRU_COUNT_TO_READ_BLOCK_SIZE)
        count_to_read = fru_read_bytes - num_bytes_read;
      else
        count_to_read = IPMI_FRU_COUNT_TO_READ_BLOCK_SIZE;

      /* XXX: achu: Implement retry mechanism? - see spec on
       * completion code 0x81
       */
      if (ipmi_cmd_read_fru_data (ctx->ipmi_ctx,
                                  ctx->fru_device_id,
                                  offset_in_bytes + num_bytes_read,
                                  count_to_read,
                                  fru_read_data_rs) < 0)
        {
	  /* if first time we've read from this device id, assume the
	   * below completion codes mean that there is no data on this
	   * device.
	   */
	  if (!num_bytes_read
	      && ((ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
		   && ipmi_check_completion_code (fru_read_data_rs, IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1)
		  || (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_MESSAGE_TIMEOUT
		      && ipmi_check_completion_code (fru_read_data_rs, IPMI_COMP_CODE_COMMAND_TIMEOUT) == 1)))
	    {
	      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_NO_FRU_INFORMATION);
	      goto cleanup;
	    }
	  
	  if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	      && ipmi_check_completion_code (fru_read_data_rs, IPMI_COMP_CODE_READ_FRU_DATA_FRU_DEVICE_BUSY) == 1)
	    {
	      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_DEVICE_BUSY);
	      goto cleanup;
	    }

          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_IPMI_ERROR);
          goto cleanup;
        }

      if (FIID_OBJ_GET (fru_read_data_rs,
                        "count_returned",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_read_data_rs);
          goto cleanup;
        }
      count_returned = val;

      if (!count_returned)
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_IPMI_ERROR);
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (fru_read_data_rs,
                                    "requested_data",
                                    buf,
                                    IPMI_FRU_BUF_LEN)) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_read_data_rs);
          goto cleanup;
        }

      if (count_returned != len)
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_IPMI_ERROR);
          goto cleanup;
        }

      memcpy (frubuf + num_bytes_read,
              buf,
              count_returned);
      num_bytes_read += count_returned;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (fru_read_data_rs);
  return (rv);
}

static int
_check_checksum (ipmi_fru_ctx_t ctx,
                 uint8_t *frubuf,
                 unsigned int length_in_bytes,
                 uint8_t checksum_init)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (frubuf);
  assert (length_in_bytes);

  if (!(ctx->flags & IPMI_FRU_FLAGS_SKIP_CHECKSUM_CHECKS))
    {
      uint8_t checksum = checksum_init;
      unsigned int i;

      for (i = 0; i < length_in_bytes; i++)
        checksum += frubuf[i];

      if (checksum)
        return (0);
    }

  return (1);
}

int
ipmi_fru_open_device_id (ipmi_fru_ctx_t ctx, uint8_t fru_device_id)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  fiid_obj_t fru_get_inventory_rs = NULL;
  fiid_obj_t fru_common_header = NULL;
  int common_header_len;
  uint8_t format_version;
  uint64_t val;
  int rv = -1;
  int ret;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->ipmi_ctx)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_IPMI_ERROR);
      return (-1);
    }

  if (fru_device_id == IPMI_FRU_DEVICE_ID_RESERVED)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->device_opened)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_DEVICE_ID_ALREADY_OPEN);
      return (-1);
    }

  ctx->fru_device_id = fru_device_id;

  if (!(fru_get_inventory_rs = fiid_obj_create (tmpl_cmd_get_fru_inventory_area_info_rs)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (ipmi_cmd_get_fru_inventory_area_info (ctx->ipmi_ctx,
                                            ctx->fru_device_id,
                                            fru_get_inventory_rs) < 0)
    {
      if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	{
	  /* achu: Assume this completion code means we got a FRU SDR
	   * entry pointed to a device that doesn't exist on this
	   * particular mother board (b/c manufacturers may use the same
	   * SDR for multiple motherboards).
	   */
	  if (ipmi_check_completion_code (fru_get_inventory_rs, IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
	    {
	      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_NO_FRU_INFORMATION);
	      goto cleanup;
	    }
	}

      if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_MESSAGE_TIMEOUT)
	{
	  FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_DEVICE_BUSY);
	  goto cleanup;
	}

      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_IPMI_ERROR);
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (fru_get_inventory_rs,
                    "fru_inventory_area_size",
                    &val) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_get_inventory_rs);
      goto cleanup;
    }
  ctx->fru_inventory_area_size = val;

  if (!ctx->fru_inventory_area_size)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_NO_FRU_INFORMATION);
      goto cleanup;
    }

  if (!(ctx->flags & IPMI_FRU_FLAGS_READ_RAW))
    {
      if ((common_header_len = fiid_template_len_bytes (tmpl_fru_common_header)) < 0)
	{
	  FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
	  goto cleanup;
	}
  
      if (_read_fru_data (ctx,
			  frubuf,
			  IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
			  0,
			  common_header_len) < 0)
	goto cleanup;
      
      if (fru_dump_hex (ctx,
			frubuf,
			common_header_len,
			"Common Header") < 0)
	goto cleanup;

      if ((ret = _check_checksum (ctx,
				  frubuf,
				  common_header_len,
				  0)) < 0)
	goto cleanup;

      if (!ret)
	{
	  FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_COMMON_HEADER_CHECKSUM_INVALID);
	  goto cleanup;
	}

      if (!(fru_common_header = fiid_obj_create (tmpl_fru_common_header)))
	{
	  FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
	  goto cleanup;
	}

      if (fiid_obj_set_all (fru_common_header,
			    frubuf,
			    common_header_len) < 0)
	{
	  FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_common_header);
	  goto cleanup;
	}

      if (fru_dump_obj (ctx,
			fru_common_header,
			"Common Header") < 0)
	goto cleanup;

      if (FIID_OBJ_GET (fru_common_header,
			"format_version",
			&val) < 0)
	{
	  FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_common_header);
	  goto cleanup;
	}
      format_version = val;
      
      if (FIID_OBJ_GET (fru_common_header,
			"chassis_info_area_starting_offset",
			&val) < 0)
	{
	  FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_common_header);
	  goto cleanup;
	}
      ctx->chassis_info_area_starting_offset = val;
      
      if (FIID_OBJ_GET (fru_common_header,
			"board_info_area_starting_offset",
			&val) < 0)
	{
	  FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_common_header);
	  goto cleanup;
	}
      ctx->board_info_area_starting_offset = val;
      
      if (FIID_OBJ_GET (fru_common_header,
			"product_info_area_starting_offset",
			&val) < 0)
	{
	  FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_common_header);
	  goto cleanup;
	}
      ctx->product_info_area_starting_offset = val;

      if (FIID_OBJ_GET (fru_common_header,
			"multirecord_area_starting_offset",
			&val) < 0)
	{
	  FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_common_header);
	  goto cleanup;
	}
      ctx->multirecord_area_starting_offset = val;

      /* Special corner case, found on Dell Poweredge R710 */
      if (!ctx->chassis_info_area_starting_offset
	  && !ctx->board_info_area_starting_offset
	  && !ctx->product_info_area_starting_offset
	  && !ctx->multirecord_area_starting_offset)
	{
	  FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_NO_FRU_INFORMATION);
	  goto cleanup;
	}

      if (format_version != IPMI_FRU_COMMON_HEADER_FORMAT_VERSION)
	{
	  FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_COMMON_HEADER_FORMAT_INVALID);
	  goto cleanup;
	}
    }

  rv = 0;
  ctx->device_opened = 1;
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
 cleanup:
  if (rv < 0)
    _init_fru_parsing_data (ctx);
  fiid_obj_destroy (fru_get_inventory_rs);
  fiid_obj_destroy (fru_common_header);
  return (rv); 
}

int
ipmi_fru_close_device_id (ipmi_fru_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->device_opened)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_DEVICE_ID_NOT_OPEN);
      return (-1);
    }

  _init_fru_parsing_data (ctx);

  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

int
ipmi_fru_first (ipmi_fru_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  _init_fru_parsing_iterator_data (ctx);
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (0);
}

static int
_parse_multirecord_header (ipmi_fru_ctx_t ctx,
                           unsigned int *__multirecord_header_length,
                           unsigned int *record_type_id,
                           unsigned int *record_format_version,
                           unsigned int *end_of_list,
                           unsigned int *record_length,
                           unsigned int *record_checksum)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1]; 
  fiid_obj_t fru_multirecord_header = NULL;
  int multirecord_header_length;
  uint64_t val;
  int ret;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (ctx->ipmi_ctx);
  assert (record_type_id
          || record_format_version
          || end_of_list
          || record_length
          || record_checksum);

  if ((multirecord_header_length = fiid_template_len_bytes (tmpl_fru_multirecord_area_header)) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (__multirecord_header_length)
    (*__multirecord_header_length) = multirecord_header_length;
      
  if ((ctx->multirecord_area_offset_in_bytes + multirecord_header_length) > ctx->fru_inventory_area_size)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }
  
  if (_read_fru_data (ctx,
                      frubuf,
                      IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                      ctx->multirecord_area_offset_in_bytes,
                      multirecord_header_length) < 0)
    goto cleanup;
      
  if (fru_dump_hex (ctx,
		    frubuf,
		    multirecord_header_length,
		    "MultiRecord Header") < 0)
    goto cleanup;

  if ((ret = _check_checksum (ctx,
                              frubuf,
                              multirecord_header_length,
                              0)) < 0)
    goto cleanup;

  if (!ret)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_MULTIRECORD_AREA_CHECKSUM_INVALID);
      goto cleanup;
    }
  
  if (!(fru_multirecord_header = fiid_obj_create (tmpl_fru_multirecord_area_header)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (fru_multirecord_header,
                        frubuf,
                        multirecord_header_length) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_multirecord_header);
      goto cleanup;
    }
  
  if (fru_dump_obj (ctx,
		    fru_multirecord_header,
		    "MultiRecord Header") < 0)
    goto cleanup;

  if (record_type_id)
    {
      if (FIID_OBJ_GET (fru_multirecord_header,
                        "record_type_id",
                        &val) < 0)
        {
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_multirecord_header);
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
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_multirecord_header);
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
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_multirecord_header);
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
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_multirecord_header);
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
          FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_multirecord_header);
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
ipmi_fru_next (ipmi_fru_ctx_t ctx)
{
  int rv = 0;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
    {
      ctx->chassis_info_area_parsed++;
      rv = 1;
      goto out;
    }
  if (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
    {
      ctx->board_info_area_parsed++;
      rv = 1;
      goto out;
    }
  if (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed)
    {
      ctx->product_info_area_parsed++;
      rv = 1;
      goto out;
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
	  /* achu: end_of_list means this is the last record, possibly
	   * with FRU data, and there are no more *after* this record.
	   * So we should return 1.
	   */
          ctx->multirecord_area_parsed++;
        }
      
      ctx->multirecord_area_offset_in_bytes += multirecord_header_length;
      /* if record_length is 0, that's ok still */
      ctx->multirecord_area_offset_in_bytes += record_length;

      rv = 1;
      goto out;
    }

 out:  
  ctx->errnum = IPMI_FRU_ERR_SUCCESS;
  return (rv);
}

static int
_read_info_area_data (ipmi_fru_ctx_t ctx,
                      unsigned int *area_type,
                      unsigned int *area_length,
                      void *areabuf,
                      unsigned int areabuflen)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1]; 
  fiid_obj_t fru_info_area_header = NULL;
  int info_area_header_length;
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
  uint8_t format_version;
  uint64_t val;
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (ctx->ipmi_ctx);
  assert (area_type);
  assert (area_length);
  assert (areabuf);
  assert (areabuflen);
  assert ((ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
          || (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
          || (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed));

  if (ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
    {
      expected_format_version = IPMI_FRU_CHASSIS_INFO_AREA_FORMAT_VERSION;
      err_code_format_invalid = IPMI_FRU_ERR_CHASSIS_INFO_AREA_FORMAT_INVALID;
      err_code_checksum_invalid = IPMI_FRU_ERR_CHASSIS_INFO_AREA_CHECKSUM_INVALID;
      info_area_starting_offset = ctx->chassis_info_area_starting_offset;
      headerhdrstr = "Chassis Info Header";
      areahdrstr = "Chassis Info Area";
      info_area_type = IPMI_FRU_AREA_TYPE_CHASSIS_INFO_AREA;
    }
  else if (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
    {
      expected_format_version = IPMI_FRU_BOARD_INFO_AREA_FORMAT_VERSION;
      err_code_format_invalid = IPMI_FRU_ERR_BOARD_INFO_AREA_FORMAT_INVALID;
      err_code_checksum_invalid = IPMI_FRU_ERR_BOARD_INFO_AREA_CHECKSUM_INVALID;
      info_area_starting_offset = ctx->board_info_area_starting_offset;
      headerhdrstr = "Board Info Header";
      areahdrstr = "Board Info Area";
      info_area_type = IPMI_FRU_AREA_TYPE_BOARD_INFO_AREA;
    }
  else /* (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed) */
    {
      expected_format_version = IPMI_FRU_PRODUCT_INFO_AREA_FORMAT_VERSION;
      err_code_format_invalid = IPMI_FRU_ERR_PRODUCT_INFO_AREA_FORMAT_INVALID;
      err_code_checksum_invalid = IPMI_FRU_ERR_PRODUCT_INFO_AREA_CHECKSUM_INVALID;
      info_area_starting_offset = ctx->product_info_area_starting_offset;
      headerhdrstr = "Product Info Header";
      areahdrstr = "Product Info Area";
      info_area_type = IPMI_FRU_AREA_TYPE_PRODUCT_INFO_AREA;
    }

  offset_in_bytes = info_area_starting_offset * 8;

  if ((info_area_header_length = fiid_template_len_bytes (tmpl_fru_info_area_header)) < 0)
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((offset_in_bytes + info_area_header_length) > ctx->fru_inventory_area_size)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }

  if (_read_fru_data (ctx,
                      frubuf,
                      IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                      offset_in_bytes,
                      info_area_header_length) < 0)
    goto cleanup;

  if (!(fru_info_area_header = fiid_obj_create (tmpl_fru_info_area_header)))
    {
      FRU_ERRNO_TO_FRU_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (fru_info_area_header,
                        frubuf,
                        info_area_header_length) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }

  if (fru_dump_obj (ctx,
		    fru_info_area_header,
		    headerhdrstr) < 0)
    goto cleanup;

  if (FIID_OBJ_GET (fru_info_area_header,
                    "format_version",
                    &val) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }
  format_version = val;

  if (format_version != expected_format_version)
    {
      FRU_SET_ERRNUM (ctx, err_code_format_invalid);
      goto cleanup;
    }

  if (FIID_OBJ_GET (fru_info_area_header,
                    "info_area_length",
                    &val) < 0)
    {
      FRU_FIID_OBJECT_ERROR_TO_FRU_ERRNUM (ctx, fru_info_area_header);
      goto cleanup;
    }
  info_area_length = val;
  info_area_length_bytes = info_area_length * 8;

  if (!info_area_length)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  if (ctx->fru_inventory_area_size < (offset_in_bytes + info_area_length_bytes))
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }
 
  if (_read_fru_data (ctx,
                      frubuf,
                      IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                      info_area_starting_offset * 8,
                      info_area_length_bytes) < 0)
    goto cleanup;
  
  if (fru_dump_hex (ctx,
		    frubuf,
		    info_area_length_bytes,
		    areahdrstr) < 0)
    goto cleanup;
  
  if ((ret = _check_checksum (ctx,
                              frubuf,
                              info_area_length_bytes,
                              0)) < 0)
    goto cleanup;
  
  if (!ret)
    {
      FRU_SET_ERRNUM (ctx, err_code_checksum_invalid);
      goto cleanup;
    }
  
  if (areabuflen < info_area_length_bytes)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_OVERFLOW);
      goto cleanup;
    }

  if (info_area_header_length > info_area_length_bytes)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  (*area_type) = info_area_type;
  (*area_length) = (info_area_length_bytes - info_area_header_length);
  memcpy (areabuf, (frubuf + info_area_header_length), (*area_length));
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (fru_info_area_header);
  return (rv);
}

static int
_read_multirecord_area_data (ipmi_fru_ctx_t ctx,
                             unsigned int *area_type,
                             unsigned int *area_length,
                             void *areabuf,
                             unsigned int areabuflen)
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
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (ctx->ipmi_ctx);
  assert (area_type);
  assert (area_length);
  assert (areabuf);
  assert (areabuflen);
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
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_MULTIRECORD_AREA_FORMAT_INVALID);
      goto cleanup;
    }

  if (!record_length)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  /* Note: Unlike Info Areas, record_length is in bytes */
  if (ctx->fru_inventory_area_size < (ctx->multirecord_area_offset_in_bytes + multirecord_header_length + record_length))
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID);
      goto cleanup;
    }

  if (_read_fru_data (ctx,
                      frubuf,
                      IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                      ctx->multirecord_area_offset_in_bytes + multirecord_header_length,
                      record_length) < 0)
    goto cleanup;

  if (fru_dump_hex (ctx,
		    frubuf,
		    record_length,
		    "MultiRecord") < 0)
    goto cleanup;
  
  if ((ret = _check_checksum (ctx,
                              frubuf,
                              record_length,
                              record_checksum)) < 0)
    goto cleanup;
  
  if (!ret)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_MULTIRECORD_AREA_CHECKSUM_INVALID);
      goto cleanup;
    }

  if (areabuflen < record_length)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_OVERFLOW);
      goto cleanup;
    }

  switch (record_type_id)
    {
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_POWER_SUPPLY_INFORMATION:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_POWER_SUPPLY_INFORMATION;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_DC_OUTPUT:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_OUTPUT;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_DC_LOAD:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_LOAD;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_MANAGEMENT_ACCESS_RECORD:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_MANAGEMENT_ACCESS_RECORD;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_BASE_COMPATIBILITY_RECORD:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_BASE_COMPATABILITY_RECORD;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_EXTENDED_COMPATIBILITY_RECORD:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_COMPATABILITY_RECORD;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_ASF_FIXED_SMBUS_DEVICE_RECORD:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_ASF_FIXED_SMBUS_DEVICE_RECORD;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_ASF_LEGACY_DEVICE_ALERTS:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_ASF_LEGACY_DEVICE_ALERTS;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_ASF_REMOTE_CONTROL:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_ASF_REMOTE_CONTROL;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_EXTENDED_DC_OUTPUT:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_DC_OUTPUT;
      break;
    case IPMI_FRU_MULTIRECORD_AREA_TYPE_EXTENDED_DC_LOAD:
      (*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_DC_LOAD;
      break;
    default:
      if (IPMI_FRU_MULTIRECORD_AREA_TYPE_IS_OEM (record_type_id))
	(*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_OEM;
      else
	(*area_type) = IPMI_FRU_AREA_TYPE_MULTIRECORD_UNKNOWN;
    }
    
  (*area_length) = record_length;
  memcpy (areabuf, frubuf, record_length);

  rv = 0;
 cleanup:
  return (rv);
}

static int
_read_raw_data (ipmi_fru_ctx_t ctx,
		unsigned int *area_type,
		unsigned int *area_length,
		void *areabuf,
		unsigned int areabuflen)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  unsigned int len = 0;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (ctx->ipmi_ctx);
  assert (area_type);
  assert (area_length);
  assert (areabuf);
  assert (areabuflen);
  assert (ctx->flags & IPMI_FRU_FLAGS_READ_RAW);

  if (areabuflen < ctx->fru_inventory_area_size)
    len = areabuflen;
  else
    len = ctx->fru_inventory_area_size;

  if (_read_fru_data (ctx,
                      frubuf,
                      IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
		      0,
		      len) < 0)
    goto cleanup;

  if (fru_dump_hex (ctx,
		    frubuf,
		    len,
		    "Raw") < 0)
    goto cleanup;
  
  (*area_type) = IPMI_FRU_AREA_TYPE_RAW_DATA;
  (*area_length) = ctx->fru_inventory_area_size;
  memcpy (areabuf, frubuf, len);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_read_data_area (ipmi_fru_ctx_t ctx,
			 unsigned int *area_type,
			 unsigned int *area_length,
			 void *areabuf,
			 unsigned int areabuflen)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->ipmi_ctx)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_IPMI_ERROR);
      return (-1);
    }

  if (!area_type
      || !area_length
      || !areabuf
      || !areabuflen)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if (!(ctx->flags & IPMI_FRU_FLAGS_READ_RAW))
    {
      if ((ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
	  || (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
	  || (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed))
	{
	  if (_read_info_area_data (ctx,
				    area_type,
				    area_length,
				    areabuf,
				    areabuflen) < 0)
	    goto cleanup;
	  
	  goto out;
	}
      
      if (ctx->multirecord_area_starting_offset && !ctx->multirecord_area_parsed)
	{
	  if (_read_multirecord_area_data (ctx,
					   area_type,
					   area_length,
					   areabuf,
					   areabuflen) < 0)
	    goto cleanup;
	  
	  goto out;
	}
    }
  else
    {
      if (_read_raw_data (ctx,
			  area_type,
			  area_length,
			  areabuf,
			  areabuflen) < 0)
	    goto cleanup;

      goto out;
    }

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_read_multirecord_record_type_id (ipmi_fru_ctx_t ctx,
					  uint8_t *record_type_id)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!ctx->ipmi_ctx)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_IPMI_ERROR);
      return (-1);
    }

  if (!record_type_id)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  if ((ctx->chassis_info_area_starting_offset && !ctx->chassis_info_area_parsed)
      || (ctx->board_info_area_starting_offset && !ctx->board_info_area_parsed)
      || (ctx->product_info_area_starting_offset && !ctx->product_info_area_parsed))
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_NOT_AVAILABLE_FOR_THIS_RECORD);
      goto cleanup;
    }

  if (ctx->multirecord_area_starting_offset && !ctx->multirecord_area_parsed)
    {
      unsigned int record_type_id_tmp;

      if (!ctx->multirecord_area_offset_in_bytes)
	ctx->multirecord_area_offset_in_bytes = ctx->multirecord_area_starting_offset * 8;
      
      if (_parse_multirecord_header (ctx,
				     NULL,
				     &record_type_id_tmp,
				     NULL,
				     NULL,
				     NULL,
				     NULL) < 0)
	goto cleanup;

      (*record_type_id) = record_type_id_tmp;

      goto out;
    }

 out:
  rv = 0;
 cleanup:
  return (rv);
}

static int
_get_type_length_bytes (ipmi_fru_ctx_t ctx,
                        const uint8_t *type_length_buf,
                        unsigned int type_length_buflen,
                        uint8_t type_code,
                        uint8_t number_of_data_bytes,
                        uint8_t *databuf,
                        unsigned int databuflen)

{
  unsigned int start_offset = 0;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (type_length_buf);
  assert (type_length_buflen);
  assert (databuf);
  assert (databuflen);

  if (!number_of_data_bytes)
    goto out;

  /* move past type_length field */
  start_offset = 1;

  /* Special Case: This shouldn't be a length of 0x01 (see type/length
   * byte format in FRU Information Storage Definition).
   */
  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_LANGUAGE_CODE
      && number_of_data_bytes == 0x01)
    {
#if 0
      unsigned int bytes_offset = 0;
  
      /* I don't know what to do.  I guess we'll just copy data until
       * we hit the sentinel value and pray for the best.
       */
      while (bytes_offset < databuflen
             && (start_offset + bytes_offset) < type_length_buflen
             && databuf[start_offset] != IPMI_FRU_SENTINEL_VALUE)
        {
          databuf[bytes_offset] = type_length_buf[start_offset + bytes_offset];
          bytes_offset++;
        }

      if (bytes_offset >= databuflen)
        {
          fprintf (stderr, "  FRU Size too small\n");
          goto cleanup;
        }

      if ((start_offset + bytes_offset) >= type_length_buflen)
        {
          fprintf (stderr, "  FRU Missing Sentinel Value\n");
          goto cleanup;
        }
#endif
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT);
      goto cleanup;
    }
  else
    {
      if (type_length_buflen < (start_offset + number_of_data_bytes))
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT);
          goto cleanup;
        }

      if (databuflen < number_of_data_bytes)
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_INTERNAL_ERROR);
          goto cleanup;
        }

      memcpy (databuf, &type_length_buf[start_offset], number_of_data_bytes);
    }

 out:
  rv = 0;
 cleanup:
  return (rv);
}

static int
_sixbitascii_to_ascii (ipmi_fru_ctx_t ctx,
                       uint8_t *databuf,
                       unsigned int databuf_bytes,
                       char *typestr,
                       unsigned int typestrlen,
                       unsigned int *bytes_written)
{
  int rv = -1;
  unsigned int i;
  uint32_t c = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (databuf);
  assert (databuf_bytes);
  assert (typestr);
  assert (typestrlen);
  assert (bytes_written);

  /* six bit ascii packs 4 chars in 3 bytes - see FRU Information Storage Definition */
  if (typestrlen < ((databuf_bytes/3 + 1))*4)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  /* six bit ascii "begins" at ' '. see FRU Information Storage Definition */
  for (i = 0; i < databuf_bytes; i+=3)
    {
      typestr[c] = (databuf[i] & 0x3F) + ' ';
      c++;
      if (databuf_bytes > (i+1))
        {
          typestr[c] = (((databuf[i+1] & 0x0F) << 2) | ((databuf[i] & 0xC0) >> 6)) + ' ';
          c++;
        }
      if (databuf_bytes > (i+2))
        {
          typestr[c] = (((databuf[i+1] & 0xF0) >> 4) | ((databuf[i+2] & 0x03) << 4)) + ' ';
          typestr[c+1] = ((databuf[i+2] & 0xFC) >> 2) + ' ';
          c+=2;
        }
    }

  (*bytes_written) = c;
  rv = 0;
 cleanup:
  return (rv);
}

static int
_bcd_to_ascii (ipmi_fru_ctx_t ctx,
               uint8_t *databuf,
               unsigned int databuf_bytes,
               char *typestr,
               unsigned int typestrlen,
               unsigned int *bytes_written)
{
  int rv = -1;
  unsigned int i;

  assert (ctx);
  assert (ctx->magic == IPMI_FRU_CTX_MAGIC);
  assert (databuf);
  assert (databuf_bytes);
  assert (typestr);
  assert (typestrlen);
  assert (bytes_written);

  if (typestrlen < databuf_bytes)
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  for (i = 0; i < databuf_bytes; i++)
    {
      /* +1/-1 hackery to get around warnings */
      if ((databuf[i] + 1) > IPMI_FRU_BCD_DIGIT_MIN
          && (databuf[i] - 1) < IPMI_FRU_BCD_DIGIT_MAX)
        typestr[i] = '0' + databuf[i];
      else if (databuf[i] == IPMI_FRU_BCD_SPACE)
        typestr[i] = ' ';
      else if (databuf[i] == IPMI_FRU_BCD_DASH)
        typestr[i] = '-';
      else if (databuf[i] == IPMI_FRU_BCD_PERIOD)
        typestr[i] = '.';
      else
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_INVALID_BCD_ENCODING);
          goto cleanup;
        }
    }

  (*bytes_written) = databuf_bytes;
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_fru_type_length_field_to_string (ipmi_fru_ctx_t ctx,
				      const uint8_t *type_length_buf,
				      unsigned int type_length_buflen,
				      uint8_t language_code,
				      char *strbuf,
				      unsigned int *strbuflen)
{
  uint8_t type_length;
  uint8_t databuf[IPMI_FRU_BUF_LEN+1];
  char strtmpbuf[IPMI_FRU_AREA_STRING_MAX+1];
  unsigned int strtmpbuflen = 0;
  uint8_t type_code;
  uint8_t number_of_data_bytes;
  int rv = -1;
  int ret;

  if (!ctx || ctx->magic != IPMI_FRU_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_fru_ctx_errormsg (ctx), ipmi_fru_ctx_errnum (ctx));
      return (-1);
    }

  if (!type_length_buf
      || !type_length_buflen
      || !strbuf
      || !strbuflen
      || !(*strbuflen))
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_PARAMETERS);
      return (-1);
    }

  memset (strbuf, '\0', (*strbuflen));

  memset (databuf, '\0', IPMI_FRU_BUF_LEN+1);
  memset (strtmpbuf, '\0', IPMI_FRU_AREA_STRING_MAX+1);

  if (fru_dump_hex (ctx,
		    type_length_buf,
		    1,
		    "Type/Length Field Header") < 0)
    goto cleanup;

  type_length = type_length_buf[0];
  type_code = (type_length & IPMI_FRU_TYPE_LENGTH_TYPE_CODE_MASK) >> IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SHIFT;
  number_of_data_bytes = type_length & IPMI_FRU_TYPE_LENGTH_NUMBER_OF_DATA_BYTES_MASK;

  if (!number_of_data_bytes)
    {
      (*strbuflen) = 0;
      goto out;
    }

  if (_get_type_length_bytes (ctx,
                              type_length_buf,
                              type_length_buflen,
                              type_code,
                              number_of_data_bytes,
                              databuf,
                              IPMI_FRU_BUF_LEN) < 0)
    goto cleanup;

  if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BINARY)
    {
      /* Ummm - it's binary or unspecified.  I guess we'll output hex */
      unsigned int i;

      /* must be atleast length of 1, b/c we check for number_of_data_bytes above */
      ret = snprintf (strtmpbuf + strtmpbuflen,
                      IPMI_FRU_AREA_STRING_MAX - strtmpbuflen,
                      "%02Xh",
                      databuf[0]);
      strtmpbuflen += ret;
      for (i = 1; i < number_of_data_bytes; i++)
        {
          ret = snprintf (strtmpbuf + strtmpbuflen,
                          IPMI_FRU_AREA_STRING_MAX - strtmpbuflen,
                          " %02Xh",
                          databuf[i]);
          strtmpbuflen += ret;
        }
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_BCD)
    {
      if (_bcd_to_ascii (ctx,
                         databuf,
                         number_of_data_bytes,
                         strtmpbuf,
                         IPMI_FRU_AREA_STRING_MAX,
                         &strtmpbuflen) < 0)
        goto cleanup;
    }
  else if (type_code == IPMI_FRU_TYPE_LENGTH_TYPE_CODE_SIXBIT_ASCII)
    {
      if (_sixbitascii_to_ascii (ctx,
                                 databuf,
                                 number_of_data_bytes,
                                 strtmpbuf,
                                 IPMI_FRU_AREA_STRING_MAX,
                                 &strtmpbuflen) < 0)
        goto cleanup;
    }
  else
    {
      if (language_code != IPMI_FRU_LANGUAGE_CODE_ENGLISH_LEGACY
          && language_code != IPMI_FRU_LANGUAGE_CODE_ENGLISH)
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_FRU_LANGUAGE_CODE_NOT_SUPPORTED);
          goto cleanup;
        }

      if (IPMI_FRU_AREA_STRING_MAX < (number_of_data_bytes + 1))
        {
          FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_INTERNAL_ERROR);
          goto cleanup;
        }

      memcpy (strtmpbuf, databuf, number_of_data_bytes);
      strtmpbuflen += number_of_data_bytes;
    }

  if ((*strbuflen) < (strtmpbuflen + 1))
    {
      FRU_SET_ERRNUM (ctx, IPMI_FRU_ERR_OVERFLOW);
      goto cleanup;
    }
  memset (strbuf, '\0', (*strbuflen));
  memcpy (strbuf, strtmpbuf, strtmpbuflen);
  (*strbuflen) = strtmpbuflen;

 out:
  rv = 0;
 cleanup:
  return (rv);
}
