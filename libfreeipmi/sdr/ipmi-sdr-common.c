/*****************************************************************************\
 *  $Id: ipmi-sdr-common.c,v 1.20 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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

#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/api/ipmi-device-global-cmds-api.h"
#include "freeipmi/api/ipmi-sdr-repository-cmds-api.h"
#include "freeipmi/api/ipmi-sensor-cmds-api.h"
#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/cmds/ipmi-sdr-repository-cmds.h"
#include "freeipmi/cmds/ipmi-sensor-cmds.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

void
sdr_init_ctx (ipmi_sdr_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);

  ctx->operation = IPMI_SDR_OPERATION_UNINITIALIZED;

  ctx->sdr_version = 0;
  ctx->record_count = 0;
  ctx->most_recent_addition_timestamp = 0;
  ctx->most_recent_erase_timestamp = 0;

  ctx->fd = -1;
  ctx->file_size = 0;
  ctx->records_start_offset = 0;
  ctx->records_end_offset = 0;
  ctx->sdr_cache = NULL;
  ctx->current_offset.offset = 0;
  ctx->current_offset.offset_dumped = 0;
  ctx->callback_lock = 0;
  
  ctx->stats_compiled = 0;
  memset (ctx->entity_counts,
	  '\0',
	  sizeof (struct ipmi_sdr_entity_count) * IPMI_MAX_ENTITY_IDS); 

  ctx->operation_device_sdr = 0;
}

static int
_device_sdr_info (ipmi_sdr_ctx_t ctx,
		  ipmi_ctx_t ipmi_ctx,
		  uint8_t *sdr_version,
		  uint16_t *record_count,
		  uint32_t *most_recent_addition_timestamp,
		  uint32_t *most_recent_erase_timestamp)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (ipmi_ctx);
  assert (sdr_version);
  assert (record_count);
  assert (most_recent_addition_timestamp);
  assert (most_recent_erase_timestamp);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_sdr_info_rs)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ipmi_cmd_get_device_sdr_info (ipmi_ctx,
				    IPMI_SENSOR_GET_SDR_COUNT,
				    obj_cmd_rs) < 0)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
      goto cleanup;
    }

  /* No SDR version w/ the workaround or timestamps, so all zero */
  *sdr_version = 0;
  *most_recent_addition_timestamp = 0;
  *most_recent_erase_timestamp = 0;

  *record_count = 0;
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "number",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *record_count = val;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_device_sdr_info_workaround (ipmi_sdr_ctx_t ctx,
			     ipmi_ctx_t ipmi_ctx,
			     uint8_t *sdr_version,
			     uint16_t *record_count,
			     uint32_t *most_recent_addition_timestamp,
			     uint32_t *most_recent_erase_timestamp)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sensor_device = 0;
  uint8_t sdr_repository_device = 0;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (ipmi_ctx);
  assert (sdr_version);
  assert (record_count);
  assert (most_recent_addition_timestamp);
  assert (most_recent_erase_timestamp);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ipmi_cmd_get_device_id (ipmi_ctx, obj_cmd_rs) < 0)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "additional_device_support.sensor_device",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  sensor_device = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "additional_device_support.sdr_repository_device",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  sdr_repository_device = val;

  /* If no normal SDR, try device SDR  */
  if (!sdr_repository_device && sensor_device)
    {
      if (_device_sdr_info (ctx,
			    ipmi_ctx,
			    sdr_version,
			    record_count,
			    most_recent_addition_timestamp,
			    most_recent_erase_timestamp) < 0)
	goto cleanup;

      ctx->operation_device_sdr = 1;
    }

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
sdr_info (ipmi_sdr_ctx_t ctx,
	  ipmi_ctx_t ipmi_ctx,
	  uint8_t *sdr_version,
	  uint16_t *record_count,
	  uint32_t *most_recent_addition_timestamp,
	  uint32_t *most_recent_erase_timestamp)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (ipmi_ctx);
  assert (sdr_version);
  assert (record_count);
  assert (most_recent_addition_timestamp);
  assert (most_recent_erase_timestamp);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sdr_repository_info_rs)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ctx->operation_device_sdr)
    {
      if (_device_sdr_info (ctx,
			    ipmi_ctx,
			    sdr_version,
			    record_count,
			    most_recent_addition_timestamp,
			    most_recent_erase_timestamp) < 0)
	goto cleanup;
    }
  else
    {
      if (ipmi_cmd_get_sdr_repository_info (ipmi_ctx, obj_cmd_rs) < 0)
	{
	  /* IPMI Workaround
	   *
	   * Discovered on Artesyn ATCA-7367
	   *
	   * For some reason, they need "Device SDR" instead of normal.
	   * This is only supposed to be used for Dynamic SDRs.  #confused
	   */
	  if (ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	      && ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INVALID_COMMAND) == 1)
	    {
	      if (!(_device_sdr_info_workaround (ctx,
						 ipmi_ctx,
						 sdr_version,
						 record_count,
						 most_recent_addition_timestamp,
						 most_recent_erase_timestamp) < 0))
		{
		  rv = 0;
		  goto cleanup;
		} 
	    }

	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
	  goto cleanup;
	}
    }

  *sdr_version = 0;
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "sdr_version_minor",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *sdr_version = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "sdr_version_major",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *sdr_version |= ((uint8_t)val << 4);

  *record_count = 0;
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "record_count",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *record_count = val;

  *most_recent_addition_timestamp = 0;
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "most_recent_addition_timestamp",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *most_recent_addition_timestamp = val;

  *most_recent_erase_timestamp = 0;
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "most_recent_erase_timestamp",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *most_recent_erase_timestamp = val;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

const char *
sdr_record_type_str (ipmi_sdr_ctx_t ctx,
		     uint8_t *sdr_record,
		     unsigned int sdr_record_len)
{
  fiid_obj_t obj_sdr_record_header = NULL;
  uint8_t record_type;
  uint64_t val;
  int sdr_record_header_len;
  char *rv = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (sdr_record);
  assert (sdr_record_len);

  if ((sdr_record_header_len = fiid_template_len_bytes (tmpl_sdr_record_header)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (sdr_record_len < sdr_record_header_len)
    goto cleanup;

  if (!(obj_sdr_record_header = fiid_obj_create (tmpl_sdr_record_header)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sdr_record_header,
                        sdr_record,
                        sdr_record_header_len) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_header);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_sdr_record_header,
                    "record_type",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_header);
      goto cleanup;
    }
  record_type = val;

  switch (record_type)
    {
    case IPMI_SDR_FORMAT_FULL_SENSOR_RECORD:
      rv = "SDR Full Sensor Record";
      break;
    case IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD:
      rv = "SDR Compact Sensor Record";
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      rv = "SDR Event Only Record";
      break;
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      rv = "SDR Entity Association Record";
      break;
    case IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD:
      rv = "SDR Device Relative Entity Association Record";
      break;
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      rv = "SDR Generic Device Locator Record";
      break;
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      rv = "SDR FRU Device Locator Record";
      break;
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      rv = "SDR Management Controller Device Locator Record";
      break;
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD:
      rv = "SDR Management Controller Confirmation Record";
      break;
    case IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD:
      rv = "SDR Message Channel Info Record";
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      rv = "SDR OEM Record";
      break;
    default:
      rv = "SDR Unknown Record";
    }

 cleanup:
  fiid_obj_destroy (obj_sdr_record_header);
  return (rv);
}

void
sdr_check_read_status (ipmi_sdr_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    return;

  if (ctx->flags & IPMI_SDR_FLAGS_DEBUG_DUMP
      && !ctx->current_offset.offset_dumped)
    {
      unsigned int record_length;
      const char *record_str;
      
      record_length = (uint8_t)((ctx->sdr_cache + ctx->current_offset.offset)[IPMI_SDR_RECORD_LENGTH_INDEX]);

      if ((record_str = sdr_record_type_str (ctx,
					     ctx->sdr_cache + ctx->current_offset.offset,
					     record_length + IPMI_SDR_RECORD_HEADER_LENGTH)))
        {
          char hdrbuf[IPMI_SDR_CACHE_DEBUG_BUFLEN];

          debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                         DEBUG_UTIL_DIRECTION_NONE,
			 DEBUG_UTIL_FLAGS_DEFAULT,
                         record_str,
                         hdrbuf,
                         IPMI_SDR_CACHE_DEBUG_BUFLEN);

          ipmi_dump_sdr_record (STDERR_FILENO,
                                ctx->debug_prefix,
                                hdrbuf,
                                NULL,
                                ctx->sdr_cache + ctx->current_offset.offset,
                                record_length + IPMI_SDR_RECORD_HEADER_LENGTH);
        }

      ctx->current_offset.offset_dumped = 1;
    }
}
