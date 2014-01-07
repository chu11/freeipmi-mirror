/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#if HAVE_CONFIG_H
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
#include <sys/types.h>
#include <sys/resource.h>
#include <errno.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "tool-common.h"
#include "tool-util-common.h"

#include "parse-common.h"

#include "freeipmi-portability.h"

ipmi_ctx_t
ipmi_open (const char *progname,
           const char *hostname,
           struct common_cmd_args *common_args,
	   pstdout_state_t pstate)
{
  ipmi_ctx_t ipmi_ctx = NULL;
  unsigned int workaround_flags = 0;

  assert (progname);
  assert (common_args);

  if (!(ipmi_ctx = ipmi_ctx_create ()))
    {
      PSTDOUT_FPRINTF (pstate,
		       stderr,
		       "ipmi_ctx_create: %s",
		       strerror (errno));
      goto cleanup;
    }

  if (hostname
      && strcasecmp (hostname, "localhost") != 0
      && strcmp (hostname, "127.0.0.1") != 0)
    {
      if (common_args->driver_type == IPMI_DEVICE_LAN_2_0)
        {
	  parse_get_freeipmi_outofband_2_0_flags (common_args->workaround_flags_outofband_2_0,
						  &workaround_flags);
          
          if (ipmi_ctx_open_outofband_2_0 (ipmi_ctx,
                                           hostname,
                                           common_args->username,
                                           common_args->password,
                                           (common_args->k_g_len) ? common_args->k_g : NULL,
                                           (common_args->k_g_len) ? common_args->k_g_len : 0,
                                           common_args->privilege_level,
                                           common_args->cipher_suite_id,
                                           common_args->session_timeout,
                                           common_args->retransmission_timeout,
                                           workaround_flags,
                                           (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
              if (ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_USERNAME_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PASSWORD_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_K_G_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_IPMI_2_0_UNAVAILABLE
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_CONNECTION_TIMEOUT)
                PSTDOUT_FPRINTF (pstate,
				 stderr,
				 "%s: %s\n",
				 progname,
				 ipmi_ctx_errormsg (ipmi_ctx));
              else
                PSTDOUT_FPRINTF (pstate,
				 stderr,
				 "ipmi_ctx_open_outofband_2_0: %s\n",
				 ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }
        }
      else
        {
	  parse_get_freeipmi_outofband_flags (common_args->workaround_flags_outofband,
					      &workaround_flags);

          if (ipmi_ctx_open_outofband (ipmi_ctx,
                                       hostname,
                                       common_args->username,
                                       common_args->password,
                                       common_args->authentication_type,
                                       common_args->privilege_level,
                                       common_args->session_timeout,
                                       common_args->retransmission_timeout,
                                       workaround_flags,
                                       (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
              if (ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_USERNAME_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PASSWORD_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_CONNECTION_TIMEOUT)
                PSTDOUT_FPRINTF (pstate,
				 stderr,
				 "%s: %s\n",
				 progname,
				 ipmi_ctx_errormsg (ipmi_ctx));
              else
                PSTDOUT_FPRINTF (pstate,
				 stderr,
				 "ipmi_ctx_open_outofband: %s\n",
				 ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }
        }
    }
  else
    {
      if (!ipmi_is_root ())
        {
          PSTDOUT_FPRINTF (pstate,
			   stderr,
			   "%s: %s\n",
			   progname,
			   ipmi_ctx_strerror (IPMI_ERR_PERMISSION));
          goto cleanup;
        }

      parse_get_freeipmi_inband_flags (common_args->workaround_flags_inband,
				       &workaround_flags);

      if (common_args->driver_type == IPMI_DEVICE_UNKNOWN)
        {
          int ret;

          if ((ret = ipmi_ctx_find_inband (ipmi_ctx,
                                           NULL,
                                           common_args->disable_auto_probe,
                                           common_args->driver_address,
                                           common_args->register_spacing,
                                           common_args->driver_device,
                                           workaround_flags,
                                           (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT)) < 0)
            {
              PSTDOUT_FPRINTF (pstate,
			       stderr,
			       "ipmi_ctx_find_inband: %s\n",
			       ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }

          if (!ret)
            {
              PSTDOUT_FPRINTF (pstate,
			       stderr,
			       "could not find inband device");
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_ctx_open_inband (ipmi_ctx,
                                    common_args->driver_type,
                                    common_args->disable_auto_probe,
                                    common_args->driver_address,
                                    common_args->register_spacing,
                                    common_args->driver_device,
                                    workaround_flags,
                                    (common_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
              if (ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_DEVICE_NOT_FOUND)
                PSTDOUT_FPRINTF (pstate,
				 stderr,
				 "%s: %s\n",
				 progname,
				 ipmi_ctx_errormsg (ipmi_ctx));
              else
                PSTDOUT_FPRINTF (pstate,
				 stderr,
				 "ipmi_ctx_open_inband: %s\n",
				 ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }
        }
    }

  if (common_args->target_channel_number_is_set
      || common_args->target_slave_address_is_set)
    {
      if (ipmi_ctx_set_target (ipmi_ctx,
			       common_args->target_channel_number_is_set ? &common_args->target_channel_number : NULL,
			       common_args->target_slave_address_is_set ? &common_args->target_slave_address : NULL) < 0)
	{
	  PSTDOUT_FPRINTF (pstate,
			   stderr,
			   "ipmi_ctx_set_target: %s\n",
			   ipmi_ctx_errormsg (ipmi_ctx));
	  goto cleanup;
	} 
    }
  
  return (ipmi_ctx);

 cleanup: 
  ipmi_ctx_close (ipmi_ctx);
  ipmi_ctx_destroy (ipmi_ctx);
  return (NULL);
}
