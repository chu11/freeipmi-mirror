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

#include "freeipmi-portability.h"
#include "error.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMISELD_SDR_CACHE_DIRECTORY      IPMISELD_CACHE_DIRECTORY

#define IPMISELD_SDR_CACHE_FILENAME       "ipmiseldsdrcache"
#define IPMISELD_SDR_CACHE_INBAND         "localhost"

extern uint32_t _ipmiseld_flags;

static int
_ipmiseld_sdr_cache_create (ipmiseld_state_data_t *state_data,
			    char *filename)
{
  assert (state_data);
  assert (state_data->sdr_ctx);
  assert (state_data->ipmi_ctx);
  assert (filename && strlen (filename));

  if (ipmi_sdr_cache_create (state_data->sdr_ctx,
                             state_data->ipmi_ctx,
                             filename,
                             IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT,
                             NULL,
                             NULL) < 0)
    {
      err_debug ("ipmi_sdr_cache_create: %s", ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }

  return (0);
}

int
ipmiseld_sdr_cache_create_and_load (ipmiseld_state_data_t *state_data,
				    const char *hostname)
{
  char filename[MAXPATHLEN+1];
  char *sdr_cache_dir;
  
  assert (state_data);
  assert (state_data->ipmi_ctx);
  
  memset (filename, '\0', MAXPATHLEN + 1);
  
  if (!(state_data->sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      err_output ("ipmi_sdr_cache_create: %s", strerror (errno));
      goto cleanup;
    }

  if (state_data->prog_data->args->foreground
      && state_data->prog_data->args->common_args.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_ctx_set_flags (state_data->sdr_ctx, IPMI_SDR_FLAGS_DEBUG_DUMP) < 0)
        err_debug ("ipmi_sdr_ctx_set_flags: %s", ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      
      if (hostname)
        {
          if (ipmi_sdr_ctx_set_debug_prefix (state_data->sdr_ctx, hostname) < 0)
            err_debug("ipmi_sdr_ctx_set_debug_prefix: %s",
		      ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
        }
    }
  
  if (state_data->prog_data->args->cache_directory)
    sdr_cache_dir = state_data->prog_data->args->cache_directory;
  else
    sdr_cache_dir = IPMISELD_SDR_CACHE_DIRECTORY;
  
  if (!hostname)
    hostname = IPMISELD_SDR_CACHE_INBAND;
  
  snprintf (filename,
	    MAXPATHLEN,
            "%s/%s.%s",
            sdr_cache_dir,
	    IPMISELD_SDR_CACHE_FILENAME,
	    hostname);

  if (ipmi_sdr_cache_open (state_data->sdr_ctx,
                           state_data->ipmi_ctx,
                           filename) < 0)
    {
      if (ipmi_sdr_ctx_errnum (state_data->sdr_ctx) == IPMI_SDR_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST)
        {
          if (_ipmiseld_sdr_cache_create (state_data, filename) < 0)
            goto cleanup;
        }
      else if (ipmi_sdr_ctx_errnum (state_data->sdr_ctx) == IPMI_SDR_ERR_CACHE_INVALID
               || ipmi_sdr_ctx_errnum (state_data->sdr_ctx) == IPMI_SDR_ERR_CACHE_OUT_OF_DATE)
        {
	  if (ipmi_sdr_cache_delete (state_data->sdr_ctx, filename) < 0)
	    {
	      err_debug ("ipmi_sdr_cache_delete: %s", ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	      goto cleanup;
	    }

          if (_ipmiseld_sdr_cache_create (state_data, filename) < 0)
            goto cleanup;
        }
      else
        {
          err_debug ("ipmi_sdr_cache_open: %s", ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          goto cleanup;
        }
      
      /* 2nd try after the sdr was retrieved */
      if (ipmi_sdr_cache_open (state_data->sdr_ctx,
                               state_data->ipmi_ctx,
                               filename) < 0)
        {
	  err_debug ("ipmi_sdr_cache_open: %s", ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  goto cleanup;
        }
    }
  
  return (0);
  
 cleanup:
  if (strlen (filename))
    ipmi_sdr_cache_delete (state_data->sdr_ctx, filename);
  ipmi_sdr_ctx_destroy (state_data->sdr_ctx);
  state_data->sdr_ctx = NULL;
  return (-1);
}
