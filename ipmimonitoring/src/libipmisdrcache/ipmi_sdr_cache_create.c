/*****************************************************************************\
 *  $Id: ipmi_sdr_cache_create.c,v 1.1.2.2 2007-12-20 23:25:41 chu11 Exp $
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
#include <assert.h>
#include <errno.h>

#include "ipmi_sdr_cache.h"
#include "ipmi_sdr_cache_common.h"
#include "ipmi_sdr_cache_defs.h"

#include "fd.h"

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
      if (c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
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
  
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;

 cleanup:
  if (c->fd >= 0)
    close(c->fd);
  if (c->record_ids)
    free(c->record_ids);
  if (c->sensor_numbers)
    free(c->sensor_numbers);
  ipmi_sdr_cache_init_ctx(c);
  return -1;
}

int 
ipmi_sdr_cache_record_write(ipmi_sdr_cache_ctx_t c,
			    uint8_t *buf,
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

  ipmi_sdr_cache_init_ctx(c);
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}

