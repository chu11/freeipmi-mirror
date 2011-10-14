/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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

#include "freeipmi-portability.h"

int
ipmi_is_root ()
{
#if IPMI_DONT_CHECK_FOR_ROOT
  return (1);
#else /* !IPMI_DONT_CHECK_FOR_ROOT */
  uid_t uid = getuid ();
  if (uid == 0)
    return (1);
  return (0);
#endif /* !IPMI_DONT_CHECK_FOR_ROOT */
}

void
ipmi_disable_coredump (void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit (RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) < 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

ipmi_ctx_t
ipmi_open (const char *progname,
           const char *hostname,
           struct common_cmd_args *cmd_args,
           char *errmsg,
           unsigned int errmsglen)
{
  ipmi_ctx_t ipmi_ctx = NULL;
  unsigned int workaround_flags = 0;

  if (!(ipmi_ctx = ipmi_ctx_create ()))
    {
      snprintf (errmsg,
                errmsglen,
                "ipmi_ctx_create: %s",
                strerror (errno));
      goto cleanup;
    }

  if (hostname
      && strcasecmp (hostname, "localhost") != 0
      && strcmp (hostname, "127.0.0.1") != 0)
    {
      if (cmd_args->driver_type == IPMI_DEVICE_LAN_2_0)
        {
          if (cmd_args->workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES;
          if (cmd_args->workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION;
          if (cmd_args->workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION;
          if (cmd_args->workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION;
          if (cmd_args->workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE;
          if (cmd_args->workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE;
          
          if (ipmi_ctx_open_outofband_2_0 (ipmi_ctx,
                                           hostname,
                                           cmd_args->username,
                                           cmd_args->password,
                                           (cmd_args->k_g_len) ? cmd_args->k_g : NULL,
                                           (cmd_args->k_g_len) ? cmd_args->k_g_len : 0,
                                           cmd_args->privilege_level,
                                           cmd_args->cipher_suite_id,
                                           cmd_args->session_timeout,
                                           cmd_args->retransmission_timeout,
                                           workaround_flags,
                                           (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
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
                snprintf (errmsg,
                          errmsglen,
                          "%s: %s",
                          progname,
                          ipmi_ctx_errormsg (ipmi_ctx));
              else
                snprintf (errmsg,
                          errmsglen,
                          "ipmi_ctx_open_outofband_2_0: %s",
                          ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }
        }
      else
        {
          if (cmd_args->workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO;
          if (cmd_args->workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION;
          if (cmd_args->workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE;
          if (cmd_args->workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER;
          if (cmd_args->workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES;

          if (ipmi_ctx_open_outofband (ipmi_ctx,
                                       hostname,
                                       cmd_args->username,
                                       cmd_args->password,
                                       cmd_args->authentication_type,
                                       cmd_args->privilege_level,
                                       cmd_args->session_timeout,
                                       cmd_args->retransmission_timeout,
                                       workaround_flags,
                                       (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
              if (ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_USERNAME_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PASSWORD_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID
                  || ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_CONNECTION_TIMEOUT)
                snprintf (errmsg,
                          errmsglen,
                          "%s: %s",
                          progname,
                          ipmi_ctx_errormsg (ipmi_ctx));
              else
                snprintf (errmsg,
                          errmsglen,
                          "ipmi_ctx_open_outofband: %s",
                          ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }
        }
    }
  else
    {
      if (!ipmi_is_root ())
        {
          snprintf (errmsg,
                    errmsglen,
                    "%s: %s",
                    progname,
                    ipmi_ctx_strerror (IPMI_ERR_PERMISSION));
          goto cleanup;
        }

      if (cmd_args->workaround_flags_inband & IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS)
        workaround_flags |= IPMI_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS;

      if (cmd_args->workaround_flags_inband & IPMI_PARSE_WORKAROUND_FLAGS_INBAND_SPIN_POLL)
        workaround_flags |= IPMI_WORKAROUND_FLAGS_INBAND_SPIN_POLL;

      if (cmd_args->driver_type == IPMI_DEVICE_UNKNOWN)
        {
          int ret;

          if ((ret = ipmi_ctx_find_inband (ipmi_ctx,
                                           NULL,
                                           cmd_args->disable_auto_probe,
                                           cmd_args->driver_address,
                                           cmd_args->register_spacing,
                                           cmd_args->driver_device,
                                           workaround_flags,
                                           (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT)) < 0)
            {
              snprintf (errmsg,
                        errmsglen,
                        "ipmi_ctx_find_inband: %s",
                        ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }

          if (!ret)
            {
              snprintf (errmsg,
                        errmsglen,
                        "could not find inband device");
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_ctx_open_inband (ipmi_ctx,
                                    cmd_args->driver_type,
                                    cmd_args->disable_auto_probe,
                                    cmd_args->driver_address,
                                    cmd_args->register_spacing,
                                    cmd_args->driver_device,
                                    workaround_flags,
                                    (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0)
            {
              if (ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_DEVICE_NOT_FOUND)
                snprintf (errmsg,
                          errmsglen,
                          "%s: %s",
                          progname,
                          ipmi_ctx_errormsg (ipmi_ctx));
              else
                snprintf (errmsg,
                          errmsglen,
                          "ipmi_ctx_open_inband: %s",
                          ipmi_ctx_errormsg (ipmi_ctx));
              goto cleanup;
            }
        }
    }

  return (ipmi_ctx);

 cleanup:
  if (ipmi_ctx)
    {
      ipmi_ctx_close (ipmi_ctx);
      ipmi_ctx_destroy (ipmi_ctx);
    }
  return (NULL);
}

/* Check if kg len is decent */
int
check_kg_len (const char *in)
{
  assert (in);

  if (strlen (in) == 0)
    return (0);

  if (strncasecmp (in, "0x", 2) == 0)
    {
      if (strlen (in) > IPMI_MAX_K_G_LENGTH*2+2)
        return (-1);
    }
  else
    {
      if (strlen (in) > IPMI_MAX_K_G_LENGTH)
        return (-1);
    }

  return (0);
}

char *
format_kg (char *out, unsigned int outlen, const void *k_g)
{
  unsigned int i;
  int printable = 1;
  int foundnull = 0;
  char *p;
  const uint8_t *k_g_ptr = k_g;

  assert (out);
  assert (outlen > IPMI_MAX_K_G_LENGTH*2+2);
  assert (k_g);

  /* Are there any characters that would prevent printing this as a
     string on a single line? */
  for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
    {
      if (k_g_ptr[i] == '\0')
        {
          ++foundnull;
          continue;
        }
      if (!(isgraph (k_g_ptr[i]) || k_g_ptr[i] == ' ') || foundnull)
        {
          printable = 0;
          break;
        }
    }

  /* print out an entirely null key in hex rather than an empty
     string */
  if (foundnull == IPMI_MAX_K_G_LENGTH)
    printable = 0;

  /* don't print out a key starting with a literal '0x' as a string,
     since parse_kg will try to interpret such strings as hex */
  if (k_g_ptr[0] == '0' && k_g_ptr[1] == 'x')
    printable = 0;

  if (printable)
    {
      if (outlen < IPMI_MAX_K_G_LENGTH+1)
        return (NULL);
      p = out;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
        {
          if (k_g_ptr[i] == '\0')
            break;
          p[i] = k_g_ptr[i];
        }
      p[i] = 0;
    }
  else
    {
      if (outlen < IPMI_MAX_K_G_LENGTH*2+3)
        return (NULL);
      p = out;
      p[0] = '0'; p[1] = 'x';
      p+=2;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++, p+=2)
        sprintf (p, "%02x", k_g_ptr[i]);
    }

  return (out);
}
