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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <syslog.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmiseld.h"
#include "ipmiseld-common.h"
#include "ipmiseld-ipmi-communication.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "tool-util-common.h"

static void
_last_errnum_manage (ipmiseld_host_data_t *host_data)
{
  assert (host_data);

  if (host_data->last_ipmi_errnum != ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx))
    {
      host_data->last_ipmi_errnum = ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx);
      host_data->last_ipmi_errnum_count = 1;
    }
  else
    {
      host_data->last_ipmi_errnum_count++;
      if (host_data->last_ipmi_errnum_count > IPMISELD_ERROR_OUTPUT_LIMIT)
	{
	  host_data->last_ipmi_errnum = 0;
	  host_data->last_ipmi_errnum_count = 0;
	}
    }
}

int
ipmiseld_ipmi_setup (ipmiseld_host_data_t *host_data)
{
  struct common_cmd_args *common_args;
  unsigned int workaround_flags = 0;
  int rv = -1;

  assert (host_data);
  assert (host_data->host_poll);

  common_args = &(host_data->prog_data->args->common_args);

  if (!(host_data->host_poll->ipmi_ctx = ipmi_ctx_create ()))
    {
      ipmiseld_err_output (host_data, "ipmi_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (host_data->hostname
      && strcasecmp (host_data->hostname, "localhost") != 0
      && strcmp (host_data->hostname, "127.0.0.1") != 0)
    {
      if (common_args->driver_type == IPMI_DEVICE_LAN_2_0)
        {
          parse_get_freeipmi_outofband_2_0_flags (common_args->workaround_flags_outofband_2_0,
                                                  &workaround_flags);
          
          if (ipmi_ctx_open_outofband_2_0 (host_data->host_poll->ipmi_ctx,
                                           host_data->hostname,
                                           common_args->username,
                                           common_args->password,
                                           (common_args->k_g_len) ? common_args->k_g : NULL,
                                           (common_args->k_g_len) ? common_args->k_g_len : 0,
                                           common_args->privilege_level,
                                           common_args->cipher_suite_id,
                                           common_args->session_timeout,
                                           common_args->retransmission_timeout,
                                           workaround_flags,
                                           (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      if (host_data->last_ipmi_errnum != ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx)
		  || host_data->prog_data->args->verbose_count)
		{
		  if (ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_USERNAME_INVALID
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PASSWORD_INVALID
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_K_G_INVALID
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_IPMI_2_0_UNAVAILABLE
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_CONNECTION_TIMEOUT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_SESSION_TIMEOUT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID)
		    ipmiseld_err_output (host_data,
					 "Error connecting: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		  else
		    ipmiseld_err_output (host_data,
					 "ipmi_ctx_open_outofband_2_0: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		}

	      _last_errnum_manage (host_data);

              goto cleanup;
            }
        }
      else
        {
          if (ipmi_ctx_open_outofband (host_data->host_poll->ipmi_ctx,
                                       host_data->hostname,
                                       common_args->username,
                                       common_args->password,
                                       common_args->authentication_type,
                                       common_args->privilege_level,
                                       common_args->session_timeout,
                                       common_args->retransmission_timeout,
                                       workaround_flags,
                                       (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      if (host_data->last_ipmi_errnum != ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx)
		  || host_data->prog_data->args->verbose_count)
		{
		  if (ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_USERNAME_INVALID
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PASSWORD_INVALID
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_CONNECTION_TIMEOUT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_SESSION_TIMEOUT
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID)
		    ipmiseld_err_output (host_data,
					 "Error connecting: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		  else
		    ipmiseld_err_output (host_data,
					 "ipmi_ctx_open_outofband: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		}

	      _last_errnum_manage (host_data);

              goto cleanup;
            }
        }
    }
  else
    {
      if (!ipmi_is_root ())
        {
	  ipmiseld_err_output (host_data, "%s", ipmi_ctx_strerror (IPMI_ERR_PERMISSION));
          goto cleanup;
        }

      parse_get_freeipmi_inband_flags (common_args->workaround_flags_inband,
                                       &workaround_flags);

      if (common_args->driver_type == IPMI_DEVICE_UNKNOWN)
        {
          int ret;

          if ((ret = ipmi_ctx_find_inband (host_data->host_poll->ipmi_ctx,
                                           NULL,
                                           common_args->disable_auto_probe,
                                           common_args->driver_address,
                                           common_args->register_spacing,
                                           common_args->driver_device,
                                           workaround_flags,
                                           (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT)) < 0)
            {
	      if (host_data->last_ipmi_errnum != ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx)
		  || host_data->prog_data->args->verbose_count)
		{
		  if (ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PERMISSION
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_DRIVER_BUSY
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_DRIVER_TIMEOUT)
		    ipmiseld_err_output (host_data,
					 "Error loading driver: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		  else
		    ipmiseld_err_output (host_data,
					 "ipmi_ctx_find_inband: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		}

	      _last_errnum_manage (host_data);

	      goto cleanup;
	    }

          if (!ret)
            {
	      /* XXX deal w/ specific errors */
              ipmiseld_err_output (host_data, "could not find inband device");
              goto cleanup;
            }
        }
      else
        {
	  if (ipmi_ctx_open_inband (host_data->host_poll->ipmi_ctx,
                                    common_args->driver_type,
                                    common_args->disable_auto_probe,
                                    common_args->driver_address,
                                    common_args->register_spacing,
                                    common_args->driver_device,
                                    workaround_flags,
                                    (common_args->debug > 1) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
	      if (host_data->last_ipmi_errnum != ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx)
		  || host_data->prog_data->args->verbose_count)
		{
		  if (ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_PERMISSION
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_DEVICE_NOT_FOUND
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_DRIVER_BUSY
		      || ipmi_ctx_errnum (host_data->host_poll->ipmi_ctx) == IPMI_ERR_DRIVER_TIMEOUT)
		    ipmiseld_err_output (host_data,
					 "Error loading driver: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		  else
		    ipmiseld_err_output (host_data,
					 "ipmi_ctx_open_inband: %s",
					 ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
		}

	      _last_errnum_manage (host_data);

              goto cleanup;
            }
        }
    }

  if (common_args->target_channel_number_is_set
      || common_args->target_slave_address_is_set)
    {
      if (ipmi_ctx_set_target (host_data->host_poll->ipmi_ctx,
                               common_args->target_channel_number_is_set ? &common_args->target_channel_number : NULL,
                               common_args->target_slave_address_is_set ? &common_args->target_slave_address : NULL) < 0)
        {
	  ipmiseld_err_output (host_data,
			       "ipmi_ctx_set_target: %s",
			       ipmi_ctx_errormsg (host_data->host_poll->ipmi_ctx));
          goto cleanup;
        } 
    }
  
  rv = 0;
 cleanup:
  if (rv < 0)
    {
      ipmi_ctx_close (host_data->host_poll->ipmi_ctx);
      ipmi_ctx_destroy (host_data->host_poll->ipmi_ctx);
    }
  return (rv);
}
