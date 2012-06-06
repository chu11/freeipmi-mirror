/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012 Lawrence Livermore National Security, LLC.
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/param.h>		/* MAXPATHLEN */
#include <assert.h>
#include <errno.h>

#include "ipmiseld.h"
#include "ipmiseld-cache.h"
#include "ipmiseld-debug.h"

#include "freeipmi-portability.h"
#include "error.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMISELD_CACHE_INBAND             "localhost"

#define IPMISELD_SDR_CACHE_FILENAME       "ipmiseldsdrcache"

#define IPMISELD_DATA_CACHE_FILENAME      "ipmiselddata"

#define IPMISELD_DATA_CACHE_FILE_MAGIC_0 0x4A
#define IPMISELD_DATA_CACHE_FILE_MAGIC_1 0x1B
#define IPMISELD_DATA_CACHE_FILE_MAGIC_2 0x11
#define IPMISELD_DATA_CACHE_FILE_MAGIC_3 0xE6

#define IPMISELD_DATA_CACHE_FILE_VERSION_0 0x00
#define IPMISELD_DATA_CACHE_FILE_VERSION_1 0x00
#define IPMISELD_DATA_CACHE_FILE_VERSION_2 0x00
#define IPMISELD_DATA_CACHE_FILE_VERSION_3 0x01

extern uint32_t _ipmiseld_flags;

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
      err_output ("ipmi_sdr_cache_create: %s", ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
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
      err_output ("ipmi_sdr_cache_create: %s", strerror (errno));
      goto cleanup;
    }
  
  if (host_data->prog_data->args->foreground
      && host_data->prog_data->args->common_args.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_ctx_set_flags (host_data->host_poll->sdr_ctx, IPMI_SDR_FLAGS_DEBUG_DUMP) < 0)
        err_output ("ipmi_sdr_ctx_set_flags: %s", ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
      
      if (host_data->hostname)
        {
          if (ipmi_sdr_ctx_set_debug_prefix (host_data->host_poll->sdr_ctx, host_data->hostname) < 0)
            err_output("ipmi_sdr_ctx_set_debug_prefix: %s",
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
	      err_output ("ipmi_sdr_cache_delete: %s", ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
	      goto cleanup;
	    }
	  
          if (_ipmiseld_sdr_cache_create (host_data, filename) < 0)
            goto cleanup;
        }
      else
        {
          err_output ("ipmi_sdr_cache_open: %s", ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
          goto cleanup;
        }
      
      /* 2nd try after the sdr was retrieved */
      if (ipmi_sdr_cache_open (host_data->host_poll->sdr_ctx,
                               host_data->host_poll->ipmi_ctx,
                               filename) < 0)
        {
	  err_output ("ipmi_sdr_cache_open: %s", ipmi_sdr_ctx_errormsg (host_data->host_poll->sdr_ctx));
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

int
ipmiseld_data_cache_load (ipmiseld_host_data_t *host_data,
			  uint16_t *last_record_id_logged)
{
  char filename[MAXPATHLEN+1];
  
  assert (host_data);

  memset (filename, '\0', MAXPATHLEN + 1);

  _data_cache_filename (host_data,
			filename,
			MAXPATHLEN);

  return (0);
}

int
ipmiseld_data_cache_store (ipmiseld_host_data_t *host_data,
			   uint16_t last_record_id_logged)
{
  char filename[MAXPATHLEN+1];
  
  assert (host_data);

  memset (filename, '\0', MAXPATHLEN + 1);

  _data_cache_filename (host_data,
			filename,
			MAXPATHLEN);

  return (0);
}
