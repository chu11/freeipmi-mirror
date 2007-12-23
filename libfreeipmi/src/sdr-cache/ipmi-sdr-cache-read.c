/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-read.c,v 1.1.2.2 2007-12-23 02:03:12 chu11 Exp $
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

#include "ipmi-sdr-cache-common.h"
#include "ipmi-sdr-cache-defs.h"

#include "libcommon/ipmi-err-wrappers.h"

int 
ipmi_sdr_cache_open(ipmi_sdr_cache_ctx_t c, 
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
  
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);
  
  SDR_CACHE_ERR_PARAMETERS(ipmi_ctx
                           && filename);
  
  if (c->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
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
  
  c->file_size = stat_buf.st_size;
  if (c->file_size < (4 + 4 + 1 + 2 + 4 + 4))
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID);
      goto cleanup;
    }

  SDR_CACHE_ERR_CLEANUP(!((c->fd = open(filename, O_RDONLY)) < 0));

  c->sdr_cache = mmap(NULL,
                      c->file_size,
                      PROT_READ,
                      MAP_PRIVATE,
                      c->fd,
                      0);
  if (!c->sdr_cache || c->sdr_cache == ((void *) -1))
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  
  memcpy(sdr_cache_magic_buf, c->sdr_cache + c->records_start_offset, 4);
  c->records_start_offset += 4;
  memcpy(sdr_cache_version_buf, c->sdr_cache + c->records_start_offset, 4);
  c->records_start_offset += 4;
  memcpy(&sdr_version_buf, c->sdr_cache + c->records_start_offset, 1);
  c->records_start_offset += 1;
  memcpy(record_count_buf, c->sdr_cache + c->records_start_offset, 2);
  c->records_start_offset += 2;
  memcpy(most_recent_addition_timestamp_buf, c->sdr_cache + c->records_start_offset, 4);
  c->records_start_offset += 4;
  memcpy(most_recent_erase_timestamp_buf, c->sdr_cache + c->records_start_offset, 4);
  c->records_start_offset += 4;

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

  c->sdr_version = (uint8_t)sdr_version_buf;
  c->record_count = ((uint16_t)record_count_buf[0] & 0xFF);
  c->record_count |= ((uint16_t)record_count_buf[1] & 0xFF) << 8;
  c->most_recent_addition_timestamp = ((uint32_t)most_recent_addition_timestamp_buf[0] & 0xFF);
  c->most_recent_addition_timestamp = ((uint32_t)most_recent_addition_timestamp_buf[1] & 0xFF) << 8;
  c->most_recent_addition_timestamp = ((uint32_t)most_recent_addition_timestamp_buf[2] & 0xFF) << 16;
  c->most_recent_addition_timestamp = ((uint32_t)most_recent_addition_timestamp_buf[3] & 0xFF) << 24;
  c->most_recent_erase_timestamp = ((uint32_t)most_recent_erase_timestamp_buf[0] & 0xFF);
  c->most_recent_erase_timestamp = ((uint32_t)most_recent_erase_timestamp_buf[1] & 0xFF) << 8;
  c->most_recent_erase_timestamp = ((uint32_t)most_recent_erase_timestamp_buf[2] & 0xFF) << 16;
  c->most_recent_erase_timestamp = ((uint32_t)most_recent_erase_timestamp_buf[3] & 0xFF) << 24;

  if (ipmi_sdr_cache_info(c,
                          ipmi_ctx,
                          &sdr_version,
                          &record_count,
                          &most_recent_addition_timestamp,
                          &most_recent_erase_timestamp) < 0)
    goto cleanup; 

  if (c->sdr_version != sdr_version
      || c->most_recent_addition_timestamp != most_recent_addition_timestamp
      || c->most_recent_erase_timestamp != most_recent_erase_timestamp)
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE);
      goto cleanup;
    }

  c->current_offset = c->records_start_offset;
  c->operation = IPMI_SDR_CACHE_OPERATION_READ_CACHE;
  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;

 cleanup:
  if (c->fd >= 0)
    close(c->fd);
  if (c->sdr_cache)
    munmap(c->sdr_cache, c->file_size);
  ipmi_sdr_cache_init_ctx(c);
  return -1;
}

int 
ipmi_sdr_cache_sdr_version(ipmi_sdr_cache_ctx_t c, uint8_t *sdr_version)
{
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(sdr_version);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *sdr_version = c->sdr_version;
  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_record_count(ipmi_sdr_cache_ctx_t c, uint16_t *record_count)
{
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(record_count);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *record_count = c->record_count;
  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_most_recent_addition_timestamp(ipmi_sdr_cache_ctx_t c, uint32_t *most_recent_addition_timestamp)
{
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(most_recent_addition_timestamp);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *most_recent_addition_timestamp = c->most_recent_addition_timestamp;
  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_most_recent_erase_timestamp(ipmi_sdr_cache_ctx_t c, uint32_t *most_recent_erase_timestamp)
{
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(most_recent_erase_timestamp);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  *most_recent_erase_timestamp = c->most_recent_erase_timestamp;
  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_first(ipmi_sdr_cache_ctx_t c)
{
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  c->current_offset = c->records_start_offset;

  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_next(ipmi_sdr_cache_ctx_t c)
{
  unsigned int record_length;

  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);
  
  record_length = (uint8_t)((c->sdr_cache + c->current_offset)[4]);
  
  if ((c->current_offset + record_length + 5) >= c->file_size)
    return 0;

  c->current_offset += 5;
  c->current_offset += record_length;
  
  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 1;
}

int 
ipmi_sdr_cache_seek(ipmi_sdr_cache_ctx_t c, unsigned int index)
{
  off_t offset;
  int i;
  
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  SDR_CACHE_ERR_PARAMETERS(index < c->record_count);

  offset = c->records_start_offset;
  for (i = 0; i < index; i++)
    {
      unsigned int record_length;
      
      record_length = (uint8_t)((c->sdr_cache + offset)[4]);
      offset += 5;
      offset += record_length;
    }

  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_search_record_id(ipmi_sdr_cache_ctx_t c, uint16_t record_id)
{
  off_t offset;
  int found = 0;

  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  offset = c->records_start_offset;
  while (offset < c->file_size)
    {
      uint8_t *ptr = c->sdr_cache + offset;
      uint16_t record_id_current;
  
      /* Record ID stored little-endian */
      record_id_current = (uint16_t)ptr[0] & 0xFF;
      record_id_current |= ((uint16_t)ptr[1] & 0xFF) << 8;
      
      if (record_id == record_id_current)
	{
	  found++;
	  c->current_offset = offset;
	  break;
	}
      else
	{
	  unsigned int record_length;
          record_length = (uint8_t)((c->sdr_cache + offset)[4]);
	  offset += 5;
	  offset += record_length;
	}
    }

  if (!found)
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_NOT_FOUND);
      return -1;
    }

  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_record_read(ipmi_sdr_cache_ctx_t c,
                           uint8_t *buf,
                           unsigned int buflen)
{
  unsigned int record_length;

  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_PARAMETERS(buf
                           && buflen);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  record_length = (uint8_t)((c->sdr_cache + c->current_offset)[4]);

  if (buflen < (record_length + 5))
    {
      SDR_CACHE_ERRNUM_SET(IPMI_SDR_CACHE_CTX_ERR_OVERFLOW);
      return -1;
    }

  memcpy(buf, c->sdr_cache + c->current_offset, record_length + 5);
  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return (record_length + 5);
}

int 
ipmi_sdr_cache_close(ipmi_sdr_cache_ctx_t c)
{
  ERR(c && c->magic == IPMI_SDR_CACHE_MAGIC);

  SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE);

  if (c->fd >= 0)
    close(c->fd);
  if (c->sdr_cache)
    munmap(c->sdr_cache, c->file_size);
  ipmi_sdr_cache_init_ctx(c);

  c->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;
  return 0;
}
