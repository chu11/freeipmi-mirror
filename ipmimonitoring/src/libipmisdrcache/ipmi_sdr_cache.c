/*****************************************************************************\
 *  $Id: ipmi_sdr_cache.c,v 1.6 2007-08-02 20:50:15 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
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
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
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

#include "ipmi_sdr_cache.h"

#include "fd.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

static char *ipmi_sdr_cache_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parmaeters",
    "out of memory",
    "filename invalid",
    "file system error",
    "filename path permission error",
    "SDR cache exists",
    "SDR cache create initialization already called",
    "SDR cache context set for reading",
    "cache creation not initialized",
    "number of records written has reached record count",
    "invalid SDR record length",
    "SDR record with an identical record id already written",
    "SDR record with an identical sensor number already written",
    "incomplete number of records written",
    "SDR cache reading initialization already called",
    "SDR cache context set for creation",
    "cache reading not initialized",
    "SDR cache does not exist",
    "SDR cache context set for creation",
    "SDR cache context set for reading",
    "SDR cache invalid",
    "not found",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

#define IPMI_SDR_CACHE_MAGIC        0xABCD9876
#define IPMI_SDR_CACHE_FILE_MAGIC_0 0xEF
#define IPMI_SDR_CACHE_FILE_MAGIC_1 0xE7
#define IPMI_SDR_CACHE_FILE_MAGIC_2 0x35
#define IPMI_SDR_CACHE_FILE_MAGIC_3 0x7C

#define IPMI_SDR_CACHE_OPERATION_UNINITIALIZED 0
#define IPMI_SDR_CACHE_OPERATION_CREATE_CACHE  1
#define IPMI_SDR_CACHE_OPERATION_READ_CACHE    2

struct ipmi_sdr_cache_ctx {
  uint32_t magic;
  unsigned int errnum;
  unsigned int operation;
  
  int fd;
  char filename[MAXPATHLEN+1];
  uint8_t version;
  uint16_t record_count;

  /* Cache Creation Vars */
  unsigned int record_count_written;
  unsigned int total_bytes_written;
  int validation_flags;
  uint16_t *record_ids;
  unsigned int record_ids_count;
  uint8_t *sensor_numbers;
  unsigned int sensor_numbers_count;

  /* Cache Reading Vars */
  off_t file_size;
  off_t records_start_offset;
  char *sdr_cache;
  off_t current_offset;
};

static void
_init_ctx(ipmi_sdr_cache_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_SDR_CACHE_MAGIC);

  c->operation = IPMI_SDR_CACHE_OPERATION_UNINITIALIZED;

  c->fd = -1;
  memset(c->filename, '\0', MAXPATHLEN+1);
  c->version = 0;
  c->record_count = 0;

  c->record_count_written = 0;
  c->total_bytes_written = 0;
  c->validation_flags = 0;
  c->record_ids = NULL;
  c->record_ids_count = 0;
  c->sensor_numbers = NULL;
  c->sensor_numbers_count = 0;

  c->file_size = 0;
  c->records_start_offset = 0;
  c->sdr_cache = NULL;
}

static void
_destroy_ctx(ipmi_sdr_cache_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_SDR_CACHE_MAGIC);

  if (c->fd >= 0)
    {
      /* If the cache create never completed, try to remove the file */
      if (c->operation == IPMI_SDR_CACHE_OPERATION_CREATE_CACHE)
	unlink(c->filename);
      close(c->fd);
    }
  if (c->sdr_cache)
    munmap(c->sdr_cache, c->file_size);
  if (c->record_ids)
    free(c->record_ids);
  if (c->sensor_numbers)
    free(c->sensor_numbers);

  c->magic = ~IPMI_SDR_CACHE_MAGIC;
  c->operation = IPMI_SDR_CACHE_OPERATION_UNINITIALIZED;
  free(c);
}

ipmi_sdr_cache_ctx_t
ipmi_sdr_cache_ctx_create(void)
{
  struct ipmi_sdr_cache_ctx *c = NULL;

  if (!(c = (ipmi_sdr_cache_ctx_t)malloc(sizeof(struct ipmi_sdr_cache_ctx))))
    return NULL;
  c->magic = IPMI_SDR_CACHE_MAGIC;
  _init_ctx(c);
  return c;
}

void
ipmi_sdr_cache_ctx_destroy(ipmi_sdr_cache_ctx_t c)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return;

  _destroy_ctx(c);
}

int 
ipmi_sdr_cache_ctx_errnum(ipmi_sdr_cache_ctx_t c)
{
  if (!c)
    return IPMI_SDR_CACHE_ERR_CONTEXT_NULL;
  else if (c->magic != IPMI_SDR_CACHE_MAGIC)
    return IPMI_SDR_CACHE_ERR_CONTEXT_INVALID;
  else
    return c->errnum;
}

char *
ipmi_sdr_cache_ctx_strerror(int errnum)
{
  if (errnum >= IPMI_SDR_CACHE_ERR_SUCCESS && errnum <= IPMI_SDR_CACHE_ERR_ERRNUMRANGE)
    return ipmi_sdr_cache_errmsgs[errnum];
  else
    return ipmi_sdr_cache_errmsgs[IPMI_SDR_CACHE_ERR_ERRNUMRANGE];
}

int 
ipmi_sdr_cache_create(ipmi_sdr_cache_ctx_t c, 
		      char *filename, 
		      uint8_t version,
		      uint16_t record_count,
		      int create_flags,
		      int validation_flags)
{
  int open_flags;
  char record_count_buf[2];
  char magic_buf[4];
  ssize_t n;

  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  /* Version cannot be 0h according to the IPMI spec */
  if (!filename
      || strlen(filename) > MAXPATHLEN
      || (create_flags != IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT
	  && create_flags != IPMI_SDR_CACHE_CREATE_FLAGS_OVERWRITE)
      || (validation_flags & ~(IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_RECORD_ID | IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_SENSOR_NUMBER))
      || !version
      || !record_count)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }

  if (c->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (c->operation == IPMI_SDR_CACHE_OPERATION_CREATE_CACHE)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_ALREADY_INITIALIZED;
      else if (c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_CTX_SET_TO_READ;
      else
        c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  strcpy(c->filename, filename);

  if (create_flags == IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT)
    open_flags = O_CREAT | O_EXCL | O_WRONLY;
  else
    open_flags = O_CREAT | O_TRUNC | O_WRONLY;

  if ((c->fd = open(filename, open_flags, 0644)) < 0)
    {
      if (create_flags == IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT
	  && errno == EEXIST)
	c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_CACHE_EXISTS;
      else if (errno == EPERM
               || errno == EACCES
               || errno == EISDIR
	       || errno == EROFS)
	c->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
      else if (errno == ENAMETOOLONG
	       || errno == ENOENT
	       || errno == ELOOP)
	c->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
      else if (errno == ENOSPC
	       || errno == EMFILE
	       || errno == ENFILE)
	c->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
      else
	c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
  c->total_bytes_written = 0;

  magic_buf[0] = IPMI_SDR_CACHE_FILE_MAGIC_0;
  magic_buf[1] = IPMI_SDR_CACHE_FILE_MAGIC_1;
  magic_buf[2] = IPMI_SDR_CACHE_FILE_MAGIC_2;
  magic_buf[3] = IPMI_SDR_CACHE_FILE_MAGIC_3;

  if ((n = fd_write_n(c->fd, magic_buf, 4)) < 0)
    {
      if (errno == ENOSPC)
	c->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
      else
	c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (n != 4)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
  c->total_bytes_written += 1;

  if ((n = fd_write_n(c->fd, (char *)&version, 1)) < 0)
    {
      if (errno == ENOSPC)
	c->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
      else
	c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (n != 1)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
  c->total_bytes_written += 1;
  
  /* Store record count little-endian */
  record_count_buf[0] = (record_count & 0x00FF);
  record_count_buf[1] = (record_count & 0xFF00) >> 8;
  
  if ((n = fd_write_n(c->fd, record_count_buf, 2)) < 0)
    {
      if (errno == ENOSPC)
	c->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
      else
	c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (n != 2)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
  c->total_bytes_written += 2;

  c->version = version;
  c->record_count = record_count;
  c->validation_flags = validation_flags;

  if (c->validation_flags & IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_RECORD_ID)
    {
      if (!(c->record_ids = (uint16_t *)malloc(c->record_count * sizeof(uint16_t))))
	{
	  c->errnum = IPMI_SDR_CACHE_ERR_OUT_OF_MEMORY;
	  goto cleanup;
	}
      c->record_ids_count = 0;
    }

  if (c->validation_flags & IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_SENSOR_NUMBER)
    {
      if (!(c->sensor_numbers = (uint8_t *)malloc(c->record_count * sizeof(uint8_t))))
	{
	  c->errnum = IPMI_SDR_CACHE_ERR_OUT_OF_MEMORY;
	  goto cleanup;
	}
      c->sensor_numbers_count = 0;
    }
  
  c->operation = IPMI_SDR_CACHE_OPERATION_CREATE_CACHE;
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;

 cleanup:
  if (c->fd >= 0)
    close(c->fd);
  if (c->record_ids)
    free(c->record_ids);
  if (c->sensor_numbers)
    free(c->sensor_numbers);
  _init_ctx(c);
  return -1;
}

int 
ipmi_sdr_cache_record_write(ipmi_sdr_cache_ctx_t c,
			    char *buf,
			    unsigned int buflen)
{
  ssize_t n;

  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (!buf
      || !buflen)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }
  
  if (c->operation != IPMI_SDR_CACHE_OPERATION_CREATE_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_INITIALIZATION;
      return -1;
    }

  if (c->record_count_written >= c->record_count)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_RECORD_COUNT_REACHED;
      return -1;
    }

  /* Record header bytes are 5 bytes */
  if (buflen < 5)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_INVALID_RECORD_LENGTH;
      return -1;
    }

  /* Record Length is stored in byte #5.  That plus the header bytes
   * should match buflen.
   */
  if ((((uint8_t)buf[4]) + 5) != buflen)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_INVALID_RECORD_LENGTH;
      return -1;
    }

  if (c->validation_flags & IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_RECORD_ID)
    {
      uint16_t record_id;
      int i;

      assert(c->record_ids);

      /* Record ID stored little endian */
      record_id = ((uint16_t)buf[0] & 0xFF);
      record_id |= ((uint16_t)buf[1] & 0xFF) << 8;
      
      for (i = 0; i < c->record_ids_count; i++)
	{
	  if (c->record_ids[i] == record_id)
	    {
	      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_DUPLICATE_RECORD_ID;
	      return -1;
	    }
	}
      c->record_ids[c->record_ids_count] = record_id;
      c->record_ids_count++;
    }

  /* Not all SDR entries may contain a sensor number, buf[3] indicates
   * SDR record type
   */
  if (c->validation_flags & IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_SENSOR_NUMBER
      && (buf[3] == 0x01	/* Full Sensor Record */
	  || buf[3] == 0x02	/* Compact Sensor Record */
	  || buf[3] == 0x03))	/* Event-Only Record */
    {
      uint8_t sensor_number;
      int i;

      assert(c->sensor_numbers);

      sensor_number = (uint8_t)buf[7];

      for (i = 0; i < c->sensor_numbers_count; i++)
	{
	  if (c->sensor_numbers[i] == sensor_number)
	    {
	      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_DUPLICATE_SENSOR_NUMBER;
	      return -1;
	    }
	}
      c->sensor_numbers[c->sensor_numbers_count] = sensor_number;
      c->sensor_numbers_count++;
    }

  assert(c->fd >= 0);

  if ((n = fd_write_n(c->fd, buf, buflen)) < 0)
    {
      if (errno == ENOSPC)
	c->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
      else
	c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (n != buflen)
    {
      /* Try to lseek back to our original spot */
      lseek(c->fd, SEEK_SET, c->total_bytes_written);
      c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }
  c->total_bytes_written += buflen;

  c->record_count_written++;
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
  
}

int 
ipmi_sdr_cache_complete(ipmi_sdr_cache_ctx_t c)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (c->operation != IPMI_SDR_CACHE_OPERATION_CREATE_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_INITIALIZATION;
      return -1;
    }

  if (c->record_count_written != c->record_count)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_CREATE_INCOMPLETE_RECORD_COUNT_WRITTEN;
      return -1;
    }

  assert(c->fd >= 0);
  close(c->fd);

  if (c->validation_flags & IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_RECORD_ID)
    {
      assert(c->record_ids);
      free(c->record_ids);
    }

  if (c->validation_flags & IPMI_SDR_CACHE_VALIDATION_FLAGS_DUPLICATE_SENSOR_NUMBER)
    {
      assert(c->sensor_numbers);
      free(c->sensor_numbers);
    }

  _init_ctx(c);
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_open(ipmi_sdr_cache_ctx_t c, char *filename)
{
  char version_buf;
  char record_count_buf[2];
  char magic_buf[4];
  struct stat stat_buf;
  
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;
  
  if (!filename)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }
  
  if (c->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_ALREADY_INITIALIZED;
      else if (c->operation == IPMI_SDR_CACHE_OPERATION_CREATE_CACHE)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CTX_SET_TO_CREATE;
      else
        c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (stat(filename, &stat_buf) < 0)
    {
      if (errno == ENOENT
	  || errno == ENOTDIR)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST;
      else if (errno == EACCES)
	c->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
      else if (errno == ENAMETOOLONG
	       || errno == ELOOP)
	c->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
      else
	c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  /* File Size must be atleast magic_buf + version_buf +
   * record_count_buf in size. 
   */
  
  c->file_size = stat_buf.st_size;
  if (c->file_size < (4 + 1 + 2))
    {
      c->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
      goto cleanup;
    }

  if ((c->fd = open(filename, O_RDONLY)) < 0)
    {
      if (errno == ENOENT)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST;
      else if (errno == EPERM
               || errno == EACCES
               || errno == EISDIR
	       || errno == EROFS)
	c->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
      else if (errno == ENAMETOOLONG
	       || errno == ELOOP)
	c->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
      else if (errno == ENOSPC
	       || errno == EMFILE
	       || errno == ENFILE)
	c->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
      else
	c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  c->sdr_cache = mmap(NULL,
		      c->file_size,
		      PROT_READ,
		      MAP_PRIVATE,
		      c->fd,
		      0);
  if (!c->sdr_cache || c->sdr_cache == ((void *) -1))
    {
      c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
    
  memcpy(magic_buf, c->sdr_cache + c->records_start_offset, 4);
  c->records_start_offset += 4;
  memcpy(&version_buf, c->sdr_cache + c->records_start_offset, 1);
  c->records_start_offset += 1;
  memcpy(record_count_buf, c->sdr_cache + c->records_start_offset, 2);
  c->records_start_offset += 2;

  if ((uint8_t)magic_buf[0] != IPMI_SDR_CACHE_FILE_MAGIC_0
      || (uint8_t)magic_buf[1] != IPMI_SDR_CACHE_FILE_MAGIC_1
      || (uint8_t)magic_buf[2] != IPMI_SDR_CACHE_FILE_MAGIC_2
      || (uint8_t)magic_buf[3] != IPMI_SDR_CACHE_FILE_MAGIC_3)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_INVALID;
      goto cleanup;
    }

  c->version = (uint8_t)version_buf;
  c->record_count = ((uint16_t)record_count_buf[0] & 0xFF);
  c->record_count |= ((uint16_t)record_count_buf[1] & 0xFF) << 8;

  c->current_offset = c->records_start_offset;
  c->operation = IPMI_SDR_CACHE_OPERATION_READ_CACHE;
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;

 cleanup:
  if (c->fd >= 0)
    close(c->fd);
  if (c->sdr_cache)
    munmap(c->sdr_cache, c->file_size);
  _init_ctx(c);
  return -1;
}

int 
ipmi_sdr_cache_version(ipmi_sdr_cache_ctx_t c, uint8_t *version)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (!version)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }

  *version = c->version;
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_record_count(ipmi_sdr_cache_ctx_t c, uint16_t *record_count)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (!record_count)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }

  *record_count = c->record_count;
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_first(ipmi_sdr_cache_ctx_t c)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }

  c->current_offset = c->records_start_offset;

  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_next(ipmi_sdr_cache_ctx_t c)
{
  unsigned int record_length;

  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }
  
  record_length = (uint8_t)((c->sdr_cache + c->current_offset)[4]);
  
  if ((c->current_offset + record_length + 5) >= c->file_size)
    return 0;

  c->current_offset += 5;
  c->current_offset += record_length;
  
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 1;
}

int 
ipmi_sdr_cache_seek(ipmi_sdr_cache_ctx_t c, unsigned int index)
{
  off_t offset;
  int i;
  
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }

  if (index >= c->record_count)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }

  offset = c->records_start_offset;
  for (i = 0; i < index; i++)
    {
      unsigned int record_length;
      
      record_length = (uint8_t)((c->sdr_cache + offset)[4]);
      offset += 5;
      offset += record_length;
    }

  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_search_record_id(ipmi_sdr_cache_ctx_t c, uint16_t record_id)
{
  off_t offset;
  int found = 0;

  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }

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
      c->errnum = IPMI_SDR_CACHE_ERR_NOT_FOUND;
      return -1;
    }

  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_record_read(ipmi_sdr_cache_ctx_t c,
                           char *buf,
                           unsigned int buflen)
{
  unsigned int record_length;

  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (!buf 
      || !buflen)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }

  record_length = (uint8_t)((c->sdr_cache + c->current_offset)[4]);

  if (buflen < (record_length + 5))
    {
      c->errnum = IPMI_SDR_CACHE_ERR_OVERFLOW;
      return -1;
    }

  memcpy(buf, c->sdr_cache + c->current_offset, record_length + 5);
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return (record_length + 5);
}

int 
ipmi_sdr_cache_close(ipmi_sdr_cache_ctx_t c)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;

  if (c->operation != IPMI_SDR_CACHE_OPERATION_READ_CACHE)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_INITIALIZATION;
      return -1;
    }

  if (c->fd >= 0)
    close(c->fd);
  if (c->sdr_cache)
    munmap(c->sdr_cache, c->file_size);
  _init_ctx(c);

  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

int 
ipmi_sdr_cache_delete(ipmi_sdr_cache_ctx_t c, char *filename)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;
  
  if (!filename)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }

  if (c->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_ALREADY_INITIALIZED;
      else if (c->operation == IPMI_SDR_CACHE_OPERATION_CREATE_CACHE)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CTX_SET_TO_CREATE;
      else
        c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (unlink(filename) < 0)
    {
      if (errno == EPERM 
          || errno == EACCES
          || errno == EROFS)
        c->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
      else if (errno == EISDIR
               || errno == ENAMETOOLONG
               || errno == ENOENT
               || errno == ELOOP)
        c->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
      else
        c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}
