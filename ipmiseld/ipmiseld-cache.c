/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <sys/param.h>		/* MAXPATHLEN */
#include <assert.h>
#include <errno.h>

#include "ipmiseld.h"
#include "ipmiseld-cache.h"
#include "ipmiseld-common.h"
#include "ipmiseld-debug.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "fd.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMISELD_CACHE_INBAND             "localhost"

#define IPMISELD_SDR_CACHE_FILENAME       "ipmiseldsdrcache"

/* 
 * Data Cache Format
 *
 * All numbers stored little endian
 *
 * uint32_t file_magic
 * uint32_t file_version_ 
 * uint16_t last_record_id
 * uint32_t last_percent_full
 * uint16_t entries
 * uint16_t free_space;
 * uint32_t most_recent_addition_timestamp;
 * uint32_t most_recent_erase_timestamp;
 * uint8_t delete_sel_command_supported;
 * uint8_t reserve_sel_command_supported;
 * uint8_t overflow_flag;
 * uint8_t zerosumchecksum;
 */

#define IPMISELD_DATA_CACHE_FILENAME      "ipmiselddata"

#define IPMISELD_DATA_CACHE_FILE_MAGIC    0x4A1B11E6

#define IPMISELD_DATA_CACHE_FILE_VERSION  0x00000001

#define IPMISELD_DATA_CACHE_LENGTH        (4 + 4 + 2 + 4 + 2 + 2 + 4 + 4 + 1 + 1 + 1 + 1)

static int
_ipmiseld_sdr_cache_create (ipmiseld_host_data_t *host_data,
			    char *filename)
{
  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->sdr_ctx);
  assert (host_data->host_poll->ipmi_ctx);
  assert (filename && strlen (filename));

  if (ipmi_sdr_cache_create (host_data->host_poll->sdr_ctx,
                             host_data->host_poll->ipmi_ctx,
                             filename,
                             IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT,
                             NULL,
                             NULL) < 0)
    {
      if (ipmi_sdr_ctx_errnum (host_data->host_poll->sdr_ctx) == IPMI_SDR_ERR_FILENAME_INVALID
	  || ipmi_sdr_ctx_errnum (host_data->host_poll->sdr_ctx) == IPMI_SDR_ERR_FILESYSTEM
	  || ipmi_sdr_ctx_errnum (host_data->host_poll->sdr_ctx) == IPMI_SDR_ERR_PERMISSION)
	ipmiseld_err_output (host_data,
			     "Error creating SDR cache  '%s': %s",
			     filename,
			     ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
      else
	ipmiseld_err_output (host_data,
			     "ipmi_sdr_cache_create: %s",
			     ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
      return (-1);
    }

  return (0);
}

int
ipmiseld_sdr_cache_create_and_load (ipmiseld_host_data_t *host_data)
{
  char filename[MAXPATHLEN+1];
  char *sdr_cache_dir;
  char *hostname;

  assert (host_data);
  assert (host_data->host_poll);
  assert (host_data->host_poll->ipmi_ctx);
  
  memset (filename, '\0', MAXPATHLEN + 1);
  
  if (!(host_data->host_poll->sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      ipmiseld_err_output (host_data, "ipmi_sdr_ctx_create: %s", strerror (errno));
      goto cleanup;
    }
  
  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug > 1)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_ctx_set_flags (host_data->host_poll->sdr_ctx, IPMI_SDR_FLAGS_DEBUG_DUMP) < 0)
        ipmiseld_err_output (host_data,
			     "ipmi_sdr_ctx_set_flags: %s",
			     ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
      
      if (host_data->hostname)
        {
          if (ipmi_sdr_ctx_set_debug_prefix (host_data->host_poll->sdr_ctx, host_data->hostname) < 0)
            ipmiseld_err_output (host_data,
				 "ipmi_sdr_ctx_set_debug_prefix: %s",
				 ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
        }
    }
  
  if (host_data->prog_data->args->cache_directory)
    sdr_cache_dir = host_data->prog_data->args->cache_directory;
  else
    sdr_cache_dir = IPMISELD_CACHE_DIRECTORY;
  
  hostname = host_data->hostname;
  if (!hostname)
    hostname = IPMISELD_CACHE_INBAND;
  
  snprintf (filename,
	    MAXPATHLEN,
            "%s/%s.%s",
            sdr_cache_dir,
	    IPMISELD_SDR_CACHE_FILENAME,
	    hostname);

  if (host_data->prog_data->args->re_download_sdr
      && !host_data->re_download_sdr_done)
    {
      if (host_data->prog_data->args->common_args.debug)
	IPMISELD_HOST_DEBUG (("SDR cache - deleting"));

      if (ipmi_sdr_cache_delete (host_data->host_poll->sdr_ctx, filename) < 0)
	{
	  ipmiseld_err_output (host_data,
			       "ipmi_sdr_cache_delete: %s",
			       ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
	  goto cleanup;
	}
      host_data->re_download_sdr_done = 1;
    }
  
  if (ipmi_sdr_cache_open (host_data->host_poll->sdr_ctx,
                           host_data->host_poll->ipmi_ctx,
                           filename) < 0)
    {
      if (ipmi_sdr_ctx_errnum (host_data->host_poll->sdr_ctx) == IPMI_SDR_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST)
        {
	  if (host_data->prog_data->args->common_args.debug)
	    IPMISELD_HOST_DEBUG (("SDR cache not available - creating"));

          if (_ipmiseld_sdr_cache_create (host_data, filename) < 0)
            goto cleanup;
        }
      else if (ipmi_sdr_ctx_errnum (host_data->host_poll->sdr_ctx) == IPMI_SDR_ERR_CACHE_INVALID
               || ipmi_sdr_ctx_errnum (host_data->host_poll->sdr_ctx) == IPMI_SDR_ERR_CACHE_OUT_OF_DATE)
        {
	  if (host_data->prog_data->args->common_args.debug)
	    IPMISELD_HOST_DEBUG (("SDR cache invalid - delete and recreate cache"));
	  
	  if (ipmi_sdr_cache_delete (host_data->host_poll->sdr_ctx, filename) < 0)
	    {
	      ipmiseld_err_output (host_data,
				   "ipmi_sdr_cache_delete: %s",
				   ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
	      goto cleanup;
	    }
	  
          if (_ipmiseld_sdr_cache_create (host_data, filename) < 0)
            goto cleanup;
        }
      else
        {
          ipmiseld_err_output (host_data,
			       "ipmi_sdr_cache_open: %s",
			       ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
          goto cleanup;
        }
      
      /* 2nd try after the sdr was retrieved */
      if (ipmi_sdr_cache_open (host_data->host_poll->sdr_ctx,
                               host_data->host_poll->ipmi_ctx,
                               filename) < 0)
        {
	  ipmiseld_err_output (host_data,
			       "ipmi_sdr_cache_open: %s",
			       ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
	  goto cleanup;
        }
    }
  
  return (0);
  
 cleanup:
  if (strlen (filename))
    ipmi_sdr_cache_delete (host_data->host_poll->sdr_ctx, filename);
  ipmi_sdr_ctx_destroy (host_data->host_poll->sdr_ctx);
  return (-1);
}

static void
_data_cache_filename (ipmiseld_host_data_t *host_data,
		      char *filename_buf,
		      unsigned int filename_buflen)
{
  char *sdr_cache_dir;
  char *hostname;

  assert (host_data);
  assert (filename_buf);
  assert (filename_buflen);

  if (host_data->prog_data->args->cache_directory)
    sdr_cache_dir = host_data->prog_data->args->cache_directory;
  else
    sdr_cache_dir = IPMISELD_CACHE_DIRECTORY;
  
  hostname = host_data->hostname;
  if (!hostname)
    hostname = IPMISELD_CACHE_INBAND;
  
  snprintf (filename_buf,
	    filename_buflen,
            "%s/%s.%s",
            sdr_cache_dir,
	    IPMISELD_DATA_CACHE_FILENAME,
	    hostname);
}

static unsigned int
_unmarshall_uint32 (uint8_t *databuf, uint32_t *value)
{
  assert (databuf);
  assert (value);
  
  /* stored little endian */
  (*value) = databuf[0];
  (*value) |= (databuf[1] << 8);
  (*value) |= (databuf[2] << 16);
  (*value) |= (databuf[3] << 24);
  
  return (sizeof (uint32_t));
}

static unsigned int
_unmarshall_uint16 (uint8_t *databuf, uint16_t *value)
{
  assert (databuf);
  assert (value);

  /* stored little endian */
  (*value) = databuf[0];
  (*value) |= (databuf[1] << 8);

  return (sizeof (uint16_t));
}

static unsigned int
_unmarshall_uint8 (uint8_t *databuf, uint8_t *value)
{
  assert (databuf);
  assert (value);

  (*value) = databuf[0];

  return (sizeof (uint8_t));
}

/* returns 1 on data found/loaded, 0 if not found, -1 on error loading
 *  (permission, corrupted, etc.)
 */
int
ipmiseld_data_cache_load (ipmiseld_host_data_t *host_data)
{
  uint32_t file_magic;
  uint32_t file_version;
  uint8_t zerosumchecksum = 0;
  char filename[MAXPATHLEN+1];
  uint8_t databuf[IPMISELD_DATA_CACHE_LENGTH];
  unsigned int databuf_offset = 0;
  int databuflen;
  unsigned int i;
  int fd = -1;
  int rv = -1;
  
  assert (host_data);

  memset (filename, '\0', MAXPATHLEN + 1);

  _data_cache_filename (host_data,
			filename,
			MAXPATHLEN);

  if (access (filename, F_OK) < 0)
    {
      if (errno != ENOENT)
	{
	  ipmiseld_err_output (host_data,"Error finding '%s': %s", filename, strerror (errno));
	  goto cleanup;
	}

      rv = 0;
      goto cleanup;
    }
  else
    {
      if (access (filename, R_OK) < 0)
	{
	  ipmiseld_err_output (host_data, "Error read accesing '%s': %s", filename, strerror (errno));
	  goto cleanup;
	}
    }
  
  if ((fd = open (filename, O_RDONLY)) < 0)
    {
      ipmiseld_err_output (host_data, "Error opening '%s': %s", filename, strerror (errno));
      goto cleanup;
    }

  if ((databuflen = fd_read_n (fd, databuf, IPMISELD_DATA_CACHE_LENGTH)) < 0)
    {
      ipmiseld_err_output (host_data, "fd_write_n: %s", strerror (errno));
      goto cleanup;
    }

  if (databuflen < IPMISELD_DATA_CACHE_LENGTH)
    {
      ipmiseld_err_output (host_data, "invalid read length = %d", databuflen);
      goto cleanup;
    } 

  for (i = 0; i < databuflen; i++)
    zerosumchecksum += databuf[i];

  if (zerosumchecksum)
    {
      ipmiseld_err_output (host_data, "data cache corrupted");
      goto cleanup;
    }

  databuf_offset += _unmarshall_uint32 (databuf + databuf_offset, &file_magic);

  if (file_magic != IPMISELD_DATA_CACHE_FILE_MAGIC)
    {
      ipmiseld_err_output (host_data, "data cache corrupted");
      goto cleanup;
    }
  
  databuf_offset += _unmarshall_uint32 (databuf + databuf_offset, &file_version);

  if (file_version != IPMISELD_DATA_CACHE_FILE_VERSION)
    {
      ipmiseld_err_output (host_data, "data cache out of date");
      goto cleanup;
    }
  
  databuf_offset += _unmarshall_uint16 (databuf + databuf_offset, &host_data->last_host_state.last_record_id.record_id);
  host_data->last_host_state.last_record_id.loaded = 1; 
  databuf_offset += _unmarshall_uint32 (databuf + databuf_offset, &host_data->last_host_state.last_percent_full);
  databuf_offset += _unmarshall_uint16 (databuf + databuf_offset, &host_data->last_host_state.sel_info.entries);
  databuf_offset += _unmarshall_uint16 (databuf + databuf_offset, &host_data->last_host_state.sel_info.free_space);
  databuf_offset += _unmarshall_uint32 (databuf + databuf_offset, &host_data->last_host_state.sel_info.most_recent_addition_timestamp);
  databuf_offset += _unmarshall_uint32 (databuf + databuf_offset, &host_data->last_host_state.sel_info.most_recent_erase_timestamp);
  databuf_offset += _unmarshall_uint8 (databuf + databuf_offset, &host_data->last_host_state.sel_info.delete_sel_command_supported);
  databuf_offset += _unmarshall_uint8 (databuf + databuf_offset, &host_data->last_host_state.sel_info.reserve_sel_command_supported);
  databuf_offset += _unmarshall_uint8 (databuf + databuf_offset, &host_data->last_host_state.sel_info.overflow_flag);
  host_data->last_host_state.initialized = 1;
  
  rv = 1;
 cleanup:
  close (fd);
  return (rv);
}

static unsigned int
_marshall_uint32 (uint8_t *databuf, uint32_t value)
{
  assert (databuf);

  /* store little endian */
  databuf[0] = (value & 0x000000FF);
  databuf[1] = (value & 0x0000FF00) >> 8;
  databuf[2] = (value & 0x00FF0000) >> 16;
  databuf[3] = (value & 0xFF000000) >> 24;

  return (sizeof (uint32_t));
}

static unsigned int
_marshall_uint16 (uint8_t *databuf, uint16_t value)
{
  assert (databuf);

  /* store little endian */
  databuf[0] = (value & 0x00FF);
  databuf[1] = (value & 0xFF00) >> 8;

  return (sizeof (uint16_t));
}

static unsigned int
_marshall_uint8 (uint8_t *databuf, uint8_t value)
{
  assert (databuf);

  databuf[0] = value;

  return (sizeof (uint8_t));
}

int
ipmiseld_data_cache_store (ipmiseld_host_data_t *host_data)
{
  uint32_t file_magic = IPMISELD_DATA_CACHE_FILE_MAGIC;
  uint32_t file_version = IPMISELD_DATA_CACHE_FILE_VERSION;
  char filename[MAXPATHLEN+1];
  uint8_t databuf[IPMISELD_DATA_CACHE_LENGTH];
  unsigned int databuf_offset = 0;
  uint8_t zerosumchecksum = 0;
  unsigned int i;
  int n;
  int open_flags;
  int file_found = 0;
  int fd = -1;
  int rv = -1;

  assert (host_data);

  memset (filename, '\0', MAXPATHLEN + 1);

  _data_cache_filename (host_data,
			filename,
			MAXPATHLEN);

  if (access (filename, F_OK) < 0)
    {
      if (errno != ENOENT)
	{
	  ipmiseld_err_output (host_data, "Error finding '%s': %s", filename, strerror (errno));
	  goto cleanup;
	}
    }
  else
    {
      if (access (filename, W_OK) < 0)
	{
	  ipmiseld_err_output (host_data, "Error write accesing '%s': %s", filename, strerror (errno));
	  goto cleanup;
	}
      
      file_found++;
    }
  
  if (file_found)
    open_flags = O_CREAT | O_TRUNC | O_WRONLY;
  else
    open_flags = O_CREAT | O_EXCL | O_WRONLY;

  if ((fd = open (filename, open_flags, 0644)) < 0)
    {
      ipmiseld_err_output (host_data, "Error opening '%s': %s", filename, strerror (errno));
      goto cleanup;
    }

  databuf_offset += _marshall_uint32 (databuf + databuf_offset, file_magic);
  databuf_offset += _marshall_uint32 (databuf + databuf_offset, file_version);
  databuf_offset += _marshall_uint16 (databuf + databuf_offset, host_data->last_host_state.last_record_id.record_id);
  databuf_offset += _marshall_uint32 (databuf + databuf_offset, host_data->last_host_state.last_percent_full);
  databuf_offset += _marshall_uint16 (databuf + databuf_offset, host_data->last_host_state.sel_info.entries);
  databuf_offset += _marshall_uint16 (databuf + databuf_offset, host_data->last_host_state.sel_info.free_space);
  databuf_offset += _marshall_uint32 (databuf + databuf_offset, host_data->last_host_state.sel_info.most_recent_addition_timestamp);
  databuf_offset += _marshall_uint32 (databuf + databuf_offset, host_data->last_host_state.sel_info.most_recent_erase_timestamp);
  databuf_offset += _marshall_uint8 (databuf + databuf_offset, host_data->last_host_state.sel_info.delete_sel_command_supported);
  databuf_offset += _marshall_uint8 (databuf + databuf_offset, host_data->last_host_state.sel_info.reserve_sel_command_supported);
  databuf_offset += _marshall_uint8 (databuf + databuf_offset, host_data->last_host_state.sel_info.overflow_flag);

  for (i = 0; i < databuf_offset; i++)
    zerosumchecksum += databuf[i];

  databuf_offset += _marshall_uint8 (databuf + databuf_offset, 0xFF - zerosumchecksum + 1);

  assert (databuf_offset == IPMISELD_DATA_CACHE_LENGTH);
  
  if ((n = fd_write_n (fd, databuf, databuf_offset)) < 0)
    {
      ipmiseld_err_output (host_data, "fd_write_n: %s", strerror (errno));
      goto cleanup;
    }

  if (n != databuf_offset)
    {
      ipmiseld_err_output (host_data, "incomplete write");
      goto cleanup;
    }

  if (fsync (fd) < 0)
    {
      ipmiseld_err_output (host_data, "fsync: %s", strerror (errno));
      goto cleanup;
    }

  if (close (fd) < 0)
    {
      ipmiseld_err_output (host_data, "close: %s", strerror (errno));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  if (rv < 0)
    {
      unlink (filename);
      close (fd);
    }
  return (rv);
}
