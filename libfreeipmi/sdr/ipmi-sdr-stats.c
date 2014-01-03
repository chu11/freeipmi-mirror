/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-read.c,v 1.33 2010-02-08 22:09:40 chu11 Exp $
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
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/param.h>
#include <sys/mman.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "freeipmi-portability.h"

static int
_entity_id_add_instance (ipmi_sdr_ctx_t ctx,
			 uint8_t entity_id,
			 uint8_t entity_instance)
{
  struct ipmi_sdr_entity_count *entity;
  unsigned int i;
  int found = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);

  entity = &(ctx->entity_counts[entity_id]);

  for (i = 0; i < entity->entity_instances_count; i++)
    {
      if (entity->entity_instances[i] == entity_instance)
	{
	  found++;
	  break;
	}
    }

  if (!found)
    {
      entity->entity_instances[entity->entity_instances_count] = entity_instance;
      entity->entity_instances_count++;
    }

  return (0);
}

static int
_entity_id_instances_count (ipmi_sdr_ctx_t ctx,
			    uint8_t record_type,
			    const void *sdr_record,
			    unsigned int sdr_record_len)
{
  uint8_t entity_id, entity_instance, entity_instance_type;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (sdr_record);
  assert (sdr_record_len);

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD
      && record_type != IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD
      && record_type != IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    return (0);
  
  if (ipmi_sdr_parse_entity_id_instance_type (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &entity_id,
                                              &entity_instance,
                                              &entity_instance_type) < 0)
    {
      SDR_SET_INTERNAL_ERRNUM (ctx);
      return (-1);
    }

  /* if it's a container entity, not part of our calculations */
  if (entity_instance_type == IPMI_SDR_LOGICAL_CONTAINER_ENTITY)
    return (0);

  if (_entity_id_add_instance (ctx, entity_id, entity_instance) < 0)
    return (-1);
  
  /* special case if sensor sharing is involved */
  if (record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      uint8_t share_count;
      uint8_t entity_instance_sharing;
      
      if (ipmi_sdr_parse_sensor_record_sharing (ctx,
						sdr_record,
						sdr_record_len,
                                                &share_count,
                                                NULL,
                                                NULL,
                                                &entity_instance_sharing) < 0)
        {
	  SDR_SET_INTERNAL_ERRNUM (ctx);
          return (-1);
        }
      
      if (share_count > 1
          && entity_instance_sharing == IPMI_SDR_ENTITY_INSTANCE_INCREMENTS_FOR_EACH_SHARED_RECORD)
        {
	  unsigned int i;
	  
          for (i = 1; i < share_count; i++)
	    {
	      if (_entity_id_add_instance (ctx, entity_id, entity_instance + i) < 0)
		return (-1);
	    }
        }
    }

  return (0);
}

static int
_sdr_stat_callback (ipmi_sdr_ctx_t ctx,
		    uint8_t record_type,
		    const void *sdr_record,
		    unsigned int sdr_record_len,
		    void *data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (sdr_record);
  assert (sdr_record_len);

  if (_entity_id_instances_count (ctx,
				  record_type,
				  sdr_record,
				  sdr_record_len) < 0)
    return (-1);

  return (0);
}

int
ipmi_sdr_stats_compile (ipmi_sdr_ctx_t ctx)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  if (ctx->stats_compiled)
    goto out;

  if (ipmi_sdr_cache_iterate (ctx,
			      _sdr_stat_callback,
			      NULL) < 0)
    goto cleanup;

  ctx->stats_compiled = 1;

 out:
  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  return (rv);
}

int
ipmi_sdr_stats_entity_instance_unique (ipmi_sdr_ctx_t ctx, uint8_t entity_id)
{
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  if (!ctx->stats_compiled)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_STATS_NOT_COMPILED);
      return (-1);
    }

  rv = ctx->entity_counts[entity_id].entity_instances_count;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (rv);
}
