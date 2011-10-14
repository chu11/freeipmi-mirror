/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-read.c,v 1.33 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2011 Lawrence Livermore National Security, LLC.
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

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"

#include "ipmi-sdr-cache-common.h"
#include "ipmi-sdr-cache-defs.h"
#include "ipmi-sdr-cache-trace.h"
#include "ipmi-sdr-cache-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

int
ipmi_sdr_cache_open (ipmi_sdr_cache_ctx_t ctx,
                     ipmi_ctx_t ipmi_ctx,
                     const char *filename)
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

  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (!ipmi_ctx || !filename)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (ctx->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
        SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_ALREADY_INITIALIZED);
      else
        SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (stat (filename, &stat_buf) < 0)
    {
      SDR_CACHE_ERRNO_TO_SDR_CACHE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* File Size must be atleast magic_buf + file_version_buf +
   * sdr_version_buf + record_count_buf +
   * most_recent_addition_timestamp_buf +
   * most_recent_erase_timestamp-buf in size.
   */

  ctx->file_size = stat_buf.st_size;
  if (ctx->file_size < (4 + 4 + 1 + 2 + 4 + 4))
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_INVALID);
      goto cleanup;
    }

  if ((ctx->fd = open (filename, O_RDONLY)) < 0)
    {
      SDR_CACHE_ERRNO_TO_SDR_CACHE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  ctx->sdr_cache = (uint8_t *)mmap (NULL,
                                    ctx->file_size,
                                    PROT_READ,
                                    MAP_PRIVATE,
                                    ctx->fd,
                                    0);
  if (!ctx->sdr_cache || ctx->sdr_cache == ((void *) -1))
    {
      ERRNO_TRACE (errno);
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  memcpy (sdr_cache_magic_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;
  memcpy (sdr_cache_version_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;
  memcpy (&sdr_version_buf, ctx->sdr_cache + ctx->records_start_offset, 1);
  ctx->records_start_offset += 1;
  memcpy (record_count_buf, ctx->sdr_cache + ctx->records_start_offset, 2);
  ctx->records_start_offset += 2;
  memcpy (most_recent_addition_timestamp_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;
  memcpy (most_recent_erase_timestamp_buf, ctx->sdr_cache + ctx->records_start_offset, 4);
  ctx->records_start_offset += 4;

  if ((uint8_t)sdr_cache_magic_buf[0] != IPMI_SDR_CACHE_FILE_MAGIC_0
      || (uint8_t)sdr_cache_magic_buf[1] != IPMI_SDR_CACHE_FILE_MAGIC_1
      || (uint8_t)sdr_cache_magic_buf[2] != IPMI_SDR_CACHE_FILE_MAGIC_2
      || (uint8_t)sdr_cache_magic_buf[3] != IPMI_SDR_CACHE_FILE_MAGIC_3)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_INVALID);
      goto cleanup;
    }

  if ((uint8_t)sdr_cache_version_buf[0] != IPMI_SDR_CACHE_FILE_VERSION_0
      || (uint8_t)sdr_cache_version_buf[1] != IPMI_SDR_CACHE_FILE_VERSION_1
      || (uint8_t)sdr_cache_version_buf[2] != IPMI_SDR_CACHE_FILE_VERSION_2
      || (uint8_t)sdr_cache_version_buf[3] != IPMI_SDR_CACHE_FILE_VERSION_3)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_INVALID);
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

  if (ipmi_sdr_cache_info (ctx,
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
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_OUT_OF_DATE);
      goto cleanup;
    }

  ctx->current_offset = ctx->records_start_offset;
  ctx->operation = IPMI_SDR_CACHE_OPERATION_READ_CACHE;
  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);

 cleanup:
  /* ignore potential error, cleanup path */
  if (ctx->fd >= 0)
    close (ctx->fd);
  /* ignore potential error, cleanup path */
  if (ctx->sdr_cache)
    munmap ((void *)ctx->sdr_cache, ctx->file_size);
  ipmi_sdr_cache_init_ctx (ctx);
  return (-1);
}

int
ipmi_sdr_cache_sdr_version (ipmi_sdr_cache_ctx_t ctx, uint8_t *sdr_version)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (!sdr_version)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *sdr_version = ctx->sdr_version;
  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_record_count (ipmi_sdr_cache_ctx_t ctx, uint16_t *record_count)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (!record_count)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *record_count = ctx->record_count;
  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_most_recent_addition_timestamp (ipmi_sdr_cache_ctx_t ctx, uint32_t *most_recent_addition_timestamp)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (!most_recent_addition_timestamp)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *most_recent_addition_timestamp = ctx->most_recent_addition_timestamp;
  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_most_recent_erase_timestamp (ipmi_sdr_cache_ctx_t ctx, uint32_t *most_recent_erase_timestamp)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (!most_recent_erase_timestamp)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *most_recent_erase_timestamp = ctx->most_recent_erase_timestamp;
  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_first (ipmi_sdr_cache_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  ctx->current_offset = ctx->records_start_offset;

  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_next (ipmi_sdr_cache_ctx_t ctx)
{
  unsigned int record_length;

  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  record_length = (uint8_t)((ctx->sdr_cache + ctx->current_offset)[IPMI_SDR_CACHE_SDR_RECORD_LENGTH_INDEX]);

  if ((ctx->current_offset + record_length + IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH) >= ctx->file_size)
    return (0);

  ctx->current_offset += IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH;
  ctx->current_offset += record_length;

  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (1);
}

int
ipmi_sdr_cache_seek (ipmi_sdr_cache_ctx_t ctx, unsigned int index)
{
  off_t offset;
  unsigned int i;

  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  if (index >= ctx->record_count)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_PARAMETERS);
      return (-1);
    }

  offset = ctx->records_start_offset;
  for (i = 0; i < index; i++)
    {
      unsigned int record_length;

      record_length = (uint8_t)((ctx->sdr_cache + offset)[IPMI_SDR_CACHE_SDR_RECORD_LENGTH_INDEX]);
      offset += IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH;
      offset += record_length;
    }

  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_search_record_id (ipmi_sdr_cache_ctx_t ctx, uint16_t record_id)
{
  off_t offset;
  int found = 0;

  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  offset = ctx->records_start_offset;
  while (offset < ctx->file_size)
    {
      uint8_t *ptr = ctx->sdr_cache + offset;
      uint16_t record_id_current;
      unsigned int record_length;

      /* Record ID stored little-endian */
      record_id_current = (uint16_t)ptr[IPMI_SDR_CACHE_SDR_RECORD_ID_INDEX_LS] & 0xFF;
      record_id_current |= ((uint16_t)ptr[IPMI_SDR_CACHE_SDR_RECORD_ID_INDEX_MS] & 0xFF) << 8;

      if (record_id == record_id_current)
        {
          found++;
          ctx->current_offset = offset;
          break;
        }

      record_length = (uint8_t)((ctx->sdr_cache + offset)[IPMI_SDR_CACHE_SDR_RECORD_LENGTH_INDEX]);
      offset += IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH;
      offset += record_length;
    }

  if (!found)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_NOT_FOUND);
      return (-1);
    }

  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_search_sensor (ipmi_sdr_cache_ctx_t ctx, uint8_t sensor_number, uint8_t sensor_owner_id)
{
  off_t offset;
  int found = 0;

  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  offset = ctx->records_start_offset;
  while (offset < ctx->file_size)
    {
      uint8_t *ptr = ctx->sdr_cache + offset;
      uint8_t record_type_current;
      unsigned int record_length;

      record_type_current = ptr[IPMI_SDR_CACHE_SDR_RECORD_TYPE_INDEX];

      if (record_type_current == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
          || record_type_current == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
          || record_type_current == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
        {
          uint8_t sensor_number_current;
          uint8_t sensor_owner_id_current;

          sensor_owner_id_current = ptr[IPMI_SDR_CACHE_SDR_RECORD_SENSOR_OWNER_ID_INDEX];
          sensor_number_current = ptr[IPMI_SDR_CACHE_SDR_RECORD_SENSOR_NUMBER_INDEX];

          if (sensor_owner_id_current == sensor_owner_id
              && sensor_number_current == sensor_number)
            {
              found++;
              ctx->current_offset = offset;
              break;
            }

          /* Compact sensor records can do record sharing, so check
           * for this case if the sensor_owner_id matches up.
           */
          if (sensor_owner_id_current == sensor_owner_id
              && (record_type_current == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
                  || record_type_current == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD))
            {
              uint8_t share_count;
              
              share_count = ptr[IPMI_SDR_CACHE_SDR_RECORD_COMPACT_SHARE_COUNT];
              share_count >>= IPMI_SDR_CACHE_SDR_RECORD_COMPACT_SHARE_COUNT_SHIFT;
              
              /* IPMI spec gives the following example:
               *
               * "If the starting sensor number was 10, and the share
               * count was 3, then sensors 10, 11, and 12 would share
               * the record"
               */
              if (share_count > 1
                  && (sensor_number > sensor_number_current
                      && sensor_number <= (sensor_number_current + (share_count - 1))))
                {
                  found++;
                  ctx->current_offset = offset;
                  break;
                }
            }
        }

      record_length = (uint8_t)((ctx->sdr_cache + offset)[IPMI_SDR_CACHE_SDR_RECORD_LENGTH_INDEX]);
      offset += IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH;
      offset += record_length;
    }

  if (!found)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_NOT_FOUND);
      return (-1);
    }

  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_record_read (ipmi_sdr_cache_ctx_t ctx,
                            void *buf,
                            unsigned int buflen)
{
  unsigned int record_length;

  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (!buf || !buflen)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  record_length = (uint8_t)((ctx->sdr_cache + ctx->current_offset)[IPMI_SDR_CACHE_SDR_RECORD_LENGTH_INDEX]);

  if (buflen < (record_length + IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH))
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_OVERFLOW);
      return (-1);
    }

  if (ctx->flags & IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP)
    {
      const char *record_str;

      if ((record_str = ipmi_sdr_cache_record_type_str (ctx,
                                                        ctx->sdr_cache + ctx->current_offset,
                                                        record_length + IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH)))
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
                                ctx->sdr_cache + ctx->current_offset,
                                record_length + IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH);
        }
    }

  memcpy (buf, ctx->sdr_cache + ctx->current_offset, record_length + IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH);
  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (record_length + IPMI_SDR_CACHE_SDR_RECORD_HEADER_LENGTH);
}

int
ipmi_sdr_cache_close (ipmi_sdr_cache_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_cache_ctx_errormsg (ctx), ipmi_sdr_cache_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      SDR_CACHE_SET_ERRNUM (ctx, IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  /* ignore potential error, cleanup path */
  if (ctx->fd >= 0)
    close (ctx->fd);
  /* ignore potential error, cleanup path */
  if (ctx->sdr_cache)
    munmap ((void *)ctx->sdr_cache, ctx->file_size);
  ipmi_sdr_cache_init_ctx (ctx);

  ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (0);
}
