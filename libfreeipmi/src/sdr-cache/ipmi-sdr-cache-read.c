/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-read.c,v 1.5 2007-12-30 05:19:54 chu11 Exp $
 *****************************************************************************
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

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"
#include "freeipmi/debug/ipmi-debug.h"

#include "ipmi-sdr-cache-common.h"
#include "ipmi-sdr-cache-defs.h"

#include "libcommon/ipmi-err-wrappers.h"

int 
ipmi_sdr_cache_open(ipmi_sdr_cache_ctx_t ctx, 
                    ipmi_ctx_t ipmi_ctx,
                    char *filename)
{
  uint8_t sdr_version;
  uint16_t record_count;
  uint32_t most_recent_addition_timestamp, most_recent_erase_timestamp;
  char sdr_version_buf;
  char sdr_cache_magic_buf[4];
  char sdr_cache_version_buf[4];
  char record_count_buf[2];
  char most_recent_addition_timestamp_buf[4];
  char most_recent_erase_timestamp_buf[4];
  struct stat stat_buf;
  
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);
  
  SDR_CACHE_ERR_PARAMETERS(ipmi_ctx
                           && filename);
  
  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
        SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_ALREADY_INITIALIZED);
      else
        SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR);
      return -1;
    }

  SDR_CACHE_ERR_CLEANUP(!(stat(filename, &stat_buf) < 0));

  /* File Size must be atleast magic_buf + file_version_buf +
   * sdr_version_buf + record_count_buf +
   * most_recent_addition_timestamp_buf +
   * most_recent_erase_timestamp-buf in size.
   */
  
  ctx->file_size = stat_buf.st_size;
  if (ctx->file_size < (4 + 4 + 1 + 2 + 4 + 4))
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID);
      goto cleanup;
    }

  SDR_CACHE_ERR_CLEANUP(!((ctx->fd = open(filename, O_RDONLY)) < 0));

  ctx->sdr_cache = mmap(NULL,
                        ctx->file_size,
                        PROT_READ,
                        MAP_PRIVATE,
                        ctx->fd,
                        0);
  if (!ctx->sdr_cache || ctx->sdr_cache == ((void *) -1))
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  
  memcpy(sdr_cache_magic_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;
  memcpy(sdr_cache_version_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;
  memcpy(&sdr_version_buf, ctx->sdr_cache + ctx->records_start_offset, 1);
  ctx->records_start_offset += 1;
  memcpy(record_count_buf, ctx->sdr_cache + ctx->records_start_offset, 2);
  ctx->records_start_offset += 2;
  memcpy(most_recent_addition_timestamp_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;
  memcpy(most_recent_erase_timestamp_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;

  if ((uint8_t)sdr_cache_magic_buf[0] != IPMI_SDR_CACHE_FILE_MAGIC_0
      || (uint8_t)sdr_cache_magic_buf[1] != IPMI_SDR_CACHE_FILE_MAGIC_1
      || (uint8_t)sdr_cache_magic_buf[2] != IPMI_SDR_CACHE_FILE_MAGIC_2
      || (uint8_t)sdr_cache_magic_buf[3] != IPMI_SDR_CACHE_FILE_MAGIC_3)
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID);
      goto cleanup;
    }

  if ((uint8_t)sdr_cache_version_buf[0] != IPMI_SDR_CACHE_FILE_VERSION_0
      || (uint8_t)sdr_cache_version_buf[1] != IPMI_SDR_CACHE_FILE_VERSION_1
      || (uint8_t)sdr_cache_version_buf[2] != IPMI_SDR_CACHE_FILE_VERSION_2
      || (uint8_t)sdr_cache_version_buf[3] != IPMI_SDR_CACHE_FILE_VERSION_3)
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID);
      goto cleanup;
    }

  ctx->sdr_version = (uint8_t)sdr_version_buf;
  ctx->record_count = ((uint16_t)record_count_buf[0] & 0xFF);
  ctx->record_count |= ((uint16_t)record_count_buf[1] & 0xFF) << 8;
  ctx->most_recent_addition_timestamp = ((uint32_t)most_recent_addition_timestamp_buf[0] & 0xFF);
  ctx->most_recent_addition_timestamp |= ((uint32_t)most_recent_addition_timestamp_buf[1] & 0xFF) << 8;
  ctx->most_recent_addition_timestamp |= ((uint32_t)most_recent_addition_timestamp_buf[2] & 0xFF) << 16;
  ctx->most_recent_addition_timestamp |= ((uint32_t)most_recent_addition_timestamp_buf[3] & 0xFF) << 24;
  ctx->most_recent_erase_timestamp = ((uint32_t)most_recent_erase_timestamp_buf[0] & 0xFF);
  ctx->most_recent_erase_timestamp |= ((uint32_t)most_recent_erase_timestamp_buf[1] & 0xFF) << 8;
  ctx->most_recent_erase_timestamp |= ((uint32_t)most_recent_erase_timestamp_buf[2] & 0xFF) << 16;
  ctx->most_recent_erase_timestamp |= ((uint32_t)most_recent_erase_timestamp_buf[3] & 0xFF) << 24;

  if (ipmi_sdr_cache_info(ctx,
                          ipmi_ctx,
                          &sdr_version,
                          &record_count,
                          &most_recent_addition_timestamp,
                          &most_recent_erase_timestamp) < 0)
    goto cleanup; 

  if (ctx->sdr_version != sdr_version
      || ctx->most_recent_addition_timestamp != most_recent_addition_timestamp
      || ctx->most_recent_erase_timestamp != most_recent_erase_timestamp)
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE);
      goto cleanup;
    }

  ctx->current_offset = ctx->records_start_offset;
  ctx->operation = IPMI_SDR_CACHE_OPERATION_READ_CACHE;
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;

 cleanup:
  if (ctx->fd >= 0)
    close(ctx->fd);
  if (ctx->sdr_cache)
    munmap(ctx->sdr_cache, ctx->file_size);
  ipmi_sdr_cache_init_ctx(ctx);
  return -1;
}

int 
ipmi_sdr_cache_sdr_version(ipmi_sdr_cache_ctx_t ctx, uint8_t *sdr_version)
{
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(sdr_version);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *sdr_version = ctx->sdr_version;
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_record_count(ipmi_sdr_cache_ctx_t ctx, uint16_t *record_count)
{
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(record_count);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *record_count = ctx->record_count;
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_most_recent_addition_timestamp(ipmi_sdr_cache_ctx_t ctx, uint32_t *most_recent_addition_timestamp)
{
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(most_recent_addition_timestamp);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *most_recent_addition_timestamp = ctx->most_recent_addition_timestamp;
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_most_recent_erase_timestamp(ipmi_sdr_cache_ctx_t ctx, uint32_t *most_recent_erase_timestamp)
{
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(most_recent_erase_timestamp);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *most_recent_erase_timestamp = ctx->most_recent_erase_timestamp;
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_first(ipmi_sdr_cache_ctx_t ctx)
{
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  ctx->current_offset = ctx->records_start_offset;

  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_next(ipmi_sdr_cache_ctx_t ctx)
{
  unsigned int record_length;

  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);
  
  record_length = (uint8_t)((ctx->sdr_cache + ctx->current_offset)[4]);
  
  if ((ctx->current_offset + record_length + 5) >= ctx->file_size)
    return 0;

  ctx->current_offset += 5;
  ctx->current_offset += record_length;
  
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 1;
}

int 
ipmi_sdr_cache_seek(ipmi_sdr_cache_ctx_t ctx, unsigned int index)
{
  off_t offset;
  int i;
  
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  SDR_CACHE_ERR_PARAMETERS(index < ctx->record_count);

  offset = ctx->records_start_offset;
  for (i = 0; i < index; i++)
    {
      unsigned int record_length;
      
      record_length = (uint8_t)((ctx->sdr_cache + offset)[4]);
      offset += 5;
      offset += record_length;
    }

  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_search_record_id(ipmi_sdr_cache_ctx_t ctx, uint16_t record_id)
{
  off_t offset;
  int found = 0;

  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  offset = ctx->records_start_offset;
  while (offset < ctx->file_size)
    {
      uint8_t *ptr = ctx->sdr_cache + offset;
      uint16_t record_id_current;
  
      /* Record ID stored little-endian */
      record_id_current = (uint16_t)ptr[0] & 0xFF;
      record_id_current |= ((uint16_t)ptr[1] & 0xFF) << 8;
      
      if (record_id == record_id_current)
	{
	  found++;
	  ctx->current_offset = offset;
	  break;
	}
      else
	{
	  unsigned int record_length;
          record_length = (uint8_t)((ctx->sdr_cache + offset)[4]);
	  offset += 5;
	  offset += record_length;
	}
    }

  if (!found)
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_NOT_FOUND);
      return -1;
    }

  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_record_read(ipmi_sdr_cache_ctx_t ctx,
                           uint8_t *buf,
                           unsigned int buflen)
{
  unsigned int record_length;

  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(buf
                           && buflen);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  record_length = (uint8_t)((ctx->sdr_cache + ctx->current_offset)[4]);

  if (buflen < (record_length + 5))
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_OVERFLOW);
      return -1;
    }

  if (ctx->flags & IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP)
    {
      char *record_str;
      
      if ((record_str = ipmi_sdr_cache_record_type_str(ctx, 
						       ctx->sdr_cache + ctx->current_offset,
						       record_length + 5)))
        {
          char *hdr_format =
            "================================================\n"
            "%s\n"
            "================================================";
          char hdrbuf[IPMI_SDR_CACHE_DEBUG_BUFLEN];
          
          snprintf(hdrbuf, 
                   IPMI_SDR_CACHE_DEBUG_BUFLEN,
                   hdr_format,
                   record_str);
          
          ipmi_dump_sdr_record (STDERR_FILENO,
                                NULL,
                                hdrbuf,
                                NULL,
                                ctx->sdr_cache + ctx->current_offset,
                                record_length + 5);
        }
    }

  memcpy(buf, ctx->sdr_cache + ctx->current_offset, record_length + 5);
  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return (record_length + 5);
}

int 
ipmi_sdr_cache_close(ipmi_sdr_cache_ctx_t ctx)
{
  ERR(ctx && ctx->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  if (ctx->fd >= 0)
    close(ctx->fd);
  if (ctx->sdr_cache)
    munmap(ctx->sdr_cache, ctx->file_size);
  ipmi_sdr_cache_init_ctx(ctx);

  ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}
