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
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "freeipmi-portability.h"

static void
_sdr_set_current_offset (ipmi_sdr_ctx_t ctx, off_t new_offset)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);

  ctx->current_offset.offset = new_offset;
  ctx->current_offset.offset_dumped = 0;
}

int
ipmi_sdr_cache_open (ipmi_sdr_ctx_t ctx,
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

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!filename)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_UNINITIALIZED)
    {
      if (ctx->operation == IPMI_SDR_OPERATION_READ_CACHE)
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_ALREADY_INITIALIZED);
      else
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (stat (filename, &stat_buf) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
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
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_INVALID);
      goto cleanup;
    }

  if ((ctx->fd = open (filename, O_RDONLY)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
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
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
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
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_INVALID);
      goto cleanup;
    }

  if (((uint8_t)sdr_cache_version_buf[0] != IPMI_SDR_CACHE_FILE_VERSION_1_0
       || (uint8_t)sdr_cache_version_buf[1] != IPMI_SDR_CACHE_FILE_VERSION_1_1
       || (uint8_t)sdr_cache_version_buf[2] != IPMI_SDR_CACHE_FILE_VERSION_1_2
       || (uint8_t)sdr_cache_version_buf[3] != IPMI_SDR_CACHE_FILE_VERSION_1_3)
      && ((uint8_t)sdr_cache_version_buf[0] != IPMI_SDR_CACHE_FILE_VERSION_1_2_0
	  || (uint8_t)sdr_cache_version_buf[1] != IPMI_SDR_CACHE_FILE_VERSION_1_2_1
	  || (uint8_t)sdr_cache_version_buf[2] != IPMI_SDR_CACHE_FILE_VERSION_1_2_2
	  || (uint8_t)sdr_cache_version_buf[3] != IPMI_SDR_CACHE_FILE_VERSION_1_2_3))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_INVALID);
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

  if (ipmi_ctx)
    {
      if (sdr_info (ctx,
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
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_OUT_OF_DATE);
	  goto cleanup;
	}
    }

  if ((uint8_t)sdr_cache_version_buf[0] == IPMI_SDR_CACHE_FILE_VERSION_1_2_0
      && (uint8_t)sdr_cache_version_buf[1] == IPMI_SDR_CACHE_FILE_VERSION_1_2_1
      && (uint8_t)sdr_cache_version_buf[2] == IPMI_SDR_CACHE_FILE_VERSION_1_2_2
      && (uint8_t)sdr_cache_version_buf[3] == IPMI_SDR_CACHE_FILE_VERSION_1_2_3)
    {
      uint8_t header_checksum_buf[512];
      unsigned int header_checksum_buf_len = 0;
      uint8_t header_checksum, header_checksum_cache;
      uint8_t trailer_checksum, trailer_checksum_cache;
      char total_bytes_written_buf[4];
      unsigned int total_bytes_written;
      unsigned int header_bytes_len;
      unsigned int trailer_bytes_len;

      /* File Size must be atleast magic_buf + file_version_buf +
       * sdr_version_buf + record_count_buf +
       * most_recent_addition_timestamp_buf +
       * most_recent_erase_timestamp-buf + header_checksum + trailer
       * bytes written + trailer records checksum.
       */
      
      header_bytes_len = 4 + 4 + 1 + 2 + 4 + 4 + 1;
      trailer_bytes_len = 4 + 1;

      if (ctx->file_size < (header_bytes_len + trailer_bytes_len))
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_INVALID);
	  goto cleanup;
	}

      memcpy (&header_checksum_cache, ctx->sdr_cache + ctx->records_start_offset, 1);
      ctx->records_start_offset += 1;

      memcpy(&header_checksum_buf[header_checksum_buf_len], sdr_cache_magic_buf, 4);
      header_checksum_buf_len += 4;
      memcpy(&header_checksum_buf[header_checksum_buf_len], sdr_cache_version_buf, 4);
      header_checksum_buf_len += 4;
      memcpy(&header_checksum_buf[header_checksum_buf_len], &sdr_version_buf, 1);
      header_checksum_buf_len += 1;
      memcpy(&header_checksum_buf[header_checksum_buf_len], record_count_buf, 2);
      header_checksum_buf_len += 2;
      memcpy(&header_checksum_buf[header_checksum_buf_len], most_recent_addition_timestamp_buf, 4);
      header_checksum_buf_len += 4;
      memcpy(&header_checksum_buf[header_checksum_buf_len], most_recent_erase_timestamp_buf, 4);
      header_checksum_buf_len += 4;
      
      header_checksum = ipmi_checksum (header_checksum_buf, header_checksum_buf_len);
      if (header_checksum != header_checksum_cache)
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_INVALID);
	  goto cleanup;
	}

      /* total_bytes_written is written before checksum */
      /* checksum of records is last byte written */
      memcpy (total_bytes_written_buf, ctx->sdr_cache + ctx->file_size - 5, 4);
      memcpy (&trailer_checksum_cache, ctx->sdr_cache + ctx->file_size - 1, 1);

      total_bytes_written = ((uint32_t)total_bytes_written_buf[0] & 0xFF);
      total_bytes_written |= ((uint32_t)total_bytes_written_buf[1] & 0xFF) << 8;
      total_bytes_written |= ((uint32_t)total_bytes_written_buf[2] & 0xFF) << 16;
      total_bytes_written |= ((uint32_t)total_bytes_written_buf[3] & 0xFF) << 24;

      if (total_bytes_written != ctx->file_size)
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_INVALID);
	  goto cleanup;
	}

      /* -1 for checksum */
      trailer_checksum = ipmi_checksum (ctx->sdr_cache + ctx->records_start_offset,
					total_bytes_written - ctx->records_start_offset - 1);

      if (trailer_checksum != trailer_checksum_cache)
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_INVALID);
	  goto cleanup;
	}

      ctx->records_end_offset = ctx->file_size - trailer_bytes_len;
    }
  else /* (uint8_t)sdr_cache_version_buf[0] == IPMI_SDR_CACHE_FILE_VERSION_1_0
	  && (uint8_t)sdr_cache_version_buf[1] == IPMI_SDR_CACHE_FILE_VERSION_1_1
	  && (uint8_t)sdr_cache_version_buf[2] == IPMI_SDR_CACHE_FILE_VERSION_1_2
	  && (uint8_t)sdr_cache_version_buf[3] == IPMI_SDR_CACHE_FILE_VERSION_1_3 */
    ctx->records_end_offset = ctx->file_size;

  _sdr_set_current_offset (ctx, ctx->records_start_offset);
  ctx->operation = IPMI_SDR_OPERATION_READ_CACHE;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);

 cleanup:
  /* ignore potential error, cleanup path */
  if (ctx->fd >= 0)
    close (ctx->fd);
  /* ignore potential error, cleanup path */
  if (ctx->sdr_cache)
    munmap ((void *)ctx->sdr_cache, ctx->file_size);
  sdr_init_ctx (ctx);
  return (-1);
}

int
ipmi_sdr_cache_sdr_version (ipmi_sdr_ctx_t ctx, uint8_t *sdr_version)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!sdr_version)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *sdr_version = ctx->sdr_version;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_record_count (ipmi_sdr_ctx_t ctx, uint16_t *record_count)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!record_count)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *record_count = ctx->record_count;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_most_recent_addition_timestamp (ipmi_sdr_ctx_t ctx, uint32_t *most_recent_addition_timestamp)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!most_recent_addition_timestamp)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *most_recent_addition_timestamp = ctx->most_recent_addition_timestamp;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_most_recent_erase_timestamp (ipmi_sdr_ctx_t ctx, uint32_t *most_recent_erase_timestamp)
{
  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!most_recent_erase_timestamp)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  *most_recent_erase_timestamp = ctx->most_recent_erase_timestamp;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_first (ipmi_sdr_ctx_t ctx)
{
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

  _sdr_set_current_offset (ctx, ctx->records_start_offset);

  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_next (ipmi_sdr_ctx_t ctx)
{
  unsigned int record_length;

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

  record_length = (uint8_t)((ctx->sdr_cache + ctx->current_offset.offset)[IPMI_SDR_RECORD_LENGTH_INDEX]);

  if ((ctx->current_offset.offset + record_length + IPMI_SDR_RECORD_HEADER_LENGTH) >= ctx->records_end_offset)
    return (0);

  _sdr_set_current_offset (ctx, ctx->current_offset.offset + IPMI_SDR_RECORD_HEADER_LENGTH + record_length);

  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (1);
}

int
ipmi_sdr_cache_seek (ipmi_sdr_ctx_t ctx, unsigned int index)
{
  off_t offset;
  unsigned int i;

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

  if (index >= ctx->record_count)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  offset = ctx->records_start_offset;
  for (i = 0; i < index; i++)
    {
      unsigned int record_length;

      record_length = (uint8_t)((ctx->sdr_cache + offset)[IPMI_SDR_RECORD_LENGTH_INDEX]);

      if ((offset + record_length + IPMI_SDR_RECORD_HEADER_LENGTH) >= ctx->records_end_offset)
	break;

      offset += IPMI_SDR_RECORD_HEADER_LENGTH;
      offset += record_length;
    }

  _sdr_set_current_offset (ctx, offset);

  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_search_record_id (ipmi_sdr_ctx_t ctx, uint16_t record_id)
{
  off_t offset;
  int found = 0;

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

  offset = ctx->records_start_offset;
  while (offset < ctx->records_end_offset)
    {
      uint8_t *ptr = ctx->sdr_cache + offset;
      uint16_t record_id_current;
      unsigned int record_length;

      /* Record ID stored little-endian */
      record_id_current = (uint16_t)ptr[IPMI_SDR_RECORD_ID_INDEX_LS] & 0xFF;
      record_id_current |= ((uint16_t)ptr[IPMI_SDR_RECORD_ID_INDEX_MS] & 0xFF) << 8;

      if (record_id == record_id_current)
        {
          found++;
	  _sdr_set_current_offset (ctx, offset);
          break;
        }

      record_length = (uint8_t)((ctx->sdr_cache + offset)[IPMI_SDR_RECORD_LENGTH_INDEX]);

      if ((offset + record_length + IPMI_SDR_RECORD_HEADER_LENGTH) >= ctx->records_end_offset)
	break;

      offset += IPMI_SDR_RECORD_HEADER_LENGTH;
      offset += record_length;
    }

  if (!found)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_NOT_FOUND);
      return (-1);
    }

  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_search_sensor (ipmi_sdr_ctx_t ctx, uint8_t sensor_number, uint8_t sensor_owner_id)
{
  off_t offset;
  int found = 0;

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

  offset = ctx->records_start_offset;
  while (offset < ctx->records_end_offset)
    {
      uint8_t *ptr = ctx->sdr_cache + offset;
      uint8_t record_type_current;
      unsigned int record_length;

      record_type_current = ptr[IPMI_SDR_RECORD_TYPE_INDEX];

      if (record_type_current == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
          || record_type_current == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
          || record_type_current == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
        {
          uint8_t sensor_number_current;
          uint8_t sensor_owner_id_current;

          sensor_owner_id_current = ptr[IPMI_SDR_RECORD_SENSOR_OWNER_ID_INDEX];
          sensor_number_current = ptr[IPMI_SDR_RECORD_SENSOR_NUMBER_INDEX];

          if (sensor_owner_id_current == sensor_owner_id
              && sensor_number_current == sensor_number)
            {
              found++;
	      _sdr_set_current_offset (ctx, offset);
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
              
	      if (record_type_current == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
		{
		  share_count = ptr[IPMI_SDR_RECORD_COMPACT_SHARE_COUNT];
		  share_count &= IPMI_SDR_RECORD_COMPACT_SHARE_COUNT_BITMASK;
		  share_count >>= IPMI_SDR_RECORD_COMPACT_SHARE_COUNT_SHIFT;
		}
	      else
		{
		  share_count = ptr[IPMI_SDR_RECORD_EVENT_SHARE_COUNT];
		  share_count &= IPMI_SDR_RECORD_EVENT_SHARE_COUNT_BITMASK;
		  share_count >>= IPMI_SDR_RECORD_EVENT_SHARE_COUNT_SHIFT;
		}
              
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
		  _sdr_set_current_offset (ctx, offset);
                  break;
                }
            }
        }

      record_length = (uint8_t)((ctx->sdr_cache + offset)[IPMI_SDR_RECORD_LENGTH_INDEX]);

      if ((offset + record_length + IPMI_SDR_RECORD_HEADER_LENGTH) >= ctx->records_end_offset)
	break;

      offset += IPMI_SDR_RECORD_HEADER_LENGTH;
      offset += record_length;
    }

  if (!found)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_NOT_FOUND);
      return (-1);
    }

  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}

int
ipmi_sdr_cache_record_read (ipmi_sdr_ctx_t ctx,
                            void *buf,
                            unsigned int buflen)
{
  unsigned int record_length;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!buf || !buflen)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  record_length = (uint8_t)((ctx->sdr_cache + ctx->current_offset.offset)[IPMI_SDR_RECORD_LENGTH_INDEX]);

  if (buflen < (record_length + IPMI_SDR_RECORD_HEADER_LENGTH))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_OVERFLOW);
      return (-1);
    }

  sdr_check_read_status (ctx);

  memcpy (buf, ctx->sdr_cache + ctx->current_offset.offset, record_length + IPMI_SDR_RECORD_HEADER_LENGTH);
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (record_length + IPMI_SDR_RECORD_HEADER_LENGTH);
}

static int
_sdr_save_current_offset (ipmi_sdr_ctx_t ctx)
{
  struct ipmi_sdr_offset *saved_offset = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);

  if (!(saved_offset = (struct ipmi_sdr_offset *)malloc (sizeof (struct ipmi_sdr_offset))))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_OUT_OF_MEMORY);
      return (-1);
    }

  memcpy (saved_offset, &ctx->current_offset, sizeof (struct ipmi_sdr_offset));

  if (!list_push (ctx->saved_offsets, saved_offset))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
      goto cleanup;
    }	

  return (0);

 cleanup:
  free (saved_offset);
  return (-1);
}

static int
_sdr_reset_current_offset (ipmi_sdr_ctx_t ctx)
{
  struct ipmi_sdr_offset *saved_offset = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (list_count (ctx->saved_offsets) > 0);

  if (!(saved_offset = list_pop (ctx->saved_offsets)))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
      return (-1);
    }

  memcpy (&ctx->current_offset, saved_offset, sizeof (struct ipmi_sdr_offset));
  free (saved_offset);
  return (0);
}

int
ipmi_sdr_cache_iterate (ipmi_sdr_ctx_t ctx,
			Ipmi_Sdr_Cache_Iterate_Callback iterate_callback,
			void *iterate_callback_data)
{
  uint16_t record_count;
  unsigned int i;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!iterate_callback)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_READ_CACHE)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_READ_INITIALIZATION);
      return (-1);
    }

  if (_sdr_save_current_offset (ctx) < 0)
    return (-1);

  if (ipmi_sdr_cache_record_count (ctx, &record_count) < 0)
    {
      SDR_SET_INTERNAL_ERRNUM (ctx);
      goto cleanup;
    }

  if (ipmi_sdr_cache_first (ctx) < 0)
    {
      SDR_SET_INTERNAL_ERRNUM (ctx);
      goto cleanup;
    }

  for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (ctx))
    {
      uint8_t sdr_record[IPMI_SDR_MAX_RECORD_LENGTH];
      int sdr_record_len;
      uint8_t record_type;
      int ret;

      memset (sdr_record, '\0', IPMI_SDR_MAX_RECORD_LENGTH);
      
      if ((sdr_record_len = ipmi_sdr_cache_record_read (ctx,
                                                        sdr_record,
                                                        IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
	{
	  SDR_SET_INTERNAL_ERRNUM (ctx);
	  goto cleanup;
	}

      record_type = sdr_record[IPMI_SDR_RECORD_TYPE_INDEX];
      
      ctx->callback_lock = 1;
      ret = iterate_callback (ctx,
			      record_type,
			      sdr_record,
			      (unsigned int)sdr_record_len,
			      iterate_callback_data);
      ctx->callback_lock = 0;
      
      if (ret < 0)
	{
	  rv = ret;
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_ERROR_RETURNED_IN_CALLBACK);
	  goto cleanup;
	}

      if (ret > 0)
	{
	  rv = ret;
	  goto out;
	}
    }
  
  rv = 0;
 out:
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  return (_sdr_reset_current_offset (ctx));
}

int
ipmi_sdr_cache_close (ipmi_sdr_ctx_t ctx)
{
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

  if (ctx->callback_lock)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CONTEXT_PERFORMING_OTHER_OPERATION);
      return (-1);
    }

  /* ignore potential error, cleanup path */
  if (ctx->fd >= 0)
    close (ctx->fd);
  /* ignore potential error, cleanup path */
  if (ctx->sdr_cache)
    munmap ((void *)ctx->sdr_cache, ctx->file_size);
  sdr_init_ctx (ctx);

  ctx->operation = IPMI_SDR_OPERATION_UNINITIALIZED;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
  return (0);
}
