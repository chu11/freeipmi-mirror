/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <sys/resource.h>
#include <errno.h>
#include <assert.h>

#include "tool-common.h"

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/locate/ipmi-locate.h"

#include "freeipmi-portability.h"

int
ipmi_is_root ()
{
#if IPMI_DONT_CHECK_FOR_ROOT
  return (1);
#else /* !IPMI_DONT_CHECK_FOR_ROOT */
  uid_t uid = getuid ();
  if (uid == 0)
    return 1;
#endif /* !IPMI_DONT_CHECK_FOR_ROOT */
  return 0;
}

void
ipmi_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

ipmi_ctx_t
ipmi_open(const char *progname,
          const char *hostname,
          struct common_cmd_args *cmd_args,
          char *errmsg,
          unsigned int errmsglen)
{
  ipmi_ctx_t ipmi_ctx = NULL;
  unsigned int workaround_flags = 0;

  if (!(ipmi_ctx = ipmi_ctx_create()))
    {
      snprintf(errmsg, 
               errmsglen, 
               "ipmi_ctx_create: %s",
               strerror(errno));
      goto cleanup;
    }

  workaround_flags = 0;

  if (hostname 
      && strcasecmp(hostname, "localhost") != 0
      && strcmp(hostname, "127.0.0.1") != 0)
    {
      if (cmd_args->driver_type == IPMI_DEVICE_LAN_2_0)
        {
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_SUN_2_0_SESSION;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE;

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
              if (ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_USERNAME_INVALID
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_PASSWORD_INVALID
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_K_G_INVALID
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_IPMI_2_0_UNAVAILABLE
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_CONNECTION_TIMEOUT)
                snprintf(errmsg,
                         errmsglen,
                         "%s: %s",
                         progname,
                         ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
              else
                snprintf(errmsg,
                         errmsglen,
                         "ipmi_ctx_open_outofband_2_0: %s",
                         ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
              goto cleanup;
            }
        }
      else
        {
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER;
          if (cmd_args->workaround_flags & IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)
            workaround_flags |= IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES;

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
              if (ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_USERNAME_INVALID
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_PASSWORD_INVALID
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_HOSTNAME_INVALID
                  || ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_CONNECTION_TIMEOUT)
                snprintf(errmsg,
                         errmsglen,
                         "%s: %s",
                         progname,
                         ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
              else
                snprintf(errmsg,
                         errmsglen,
                         "ipmi_ctx_open_outofband: %s",
                         ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
              goto cleanup;
            }
        }
    }
  else
    {
      if (!ipmi_is_root())
        {
          snprintf(errmsg,
                   errmsglen,
                   "%s: %s",
                   progname,
                   ipmi_ctx_strerror(IPMI_ERR_PERMISSION));
          goto cleanup;
        }

      if (cmd_args->driver_type == IPMI_DEVICE_UNKNOWN)
        {
          struct ipmi_locate_info locate_info;

          /* achu
           *
           * Try SunBMC and OpenIPMI drivers first, since they cannot
           * be found via probing.  Do it before probing for KCS/SSIF,
           * because it is possible, even though the SunBMC/OpenIPMI
           * driver is installed, probing may find KCS/SSIF anyways,
           * and try to use those.
           */
          if (!ipmi_ctx_open_inband (ipmi_ctx,
                                     IPMI_DEVICE_SUNBMC,
                                     cmd_args->disable_auto_probe,
                                     cmd_args->driver_address,
                                     cmd_args->register_spacing,
                                     cmd_args->driver_device,
                                     workaround_flags,
                                     (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT))
            goto out;
 
          if (!ipmi_ctx_open_inband (ipmi_ctx,
                                     IPMI_DEVICE_OPENIPMI,
                                     cmd_args->disable_auto_probe,
                                     cmd_args->driver_address,
                                     cmd_args->register_spacing,
                                     cmd_args->driver_device,
                                     workaround_flags,
                                     (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT))
            goto out;

          /* achu: 
           * 
           * If one of KCS or SSIF is found, we try that one first.
           * We don't want to hang on one or another if one is bad.
           *
           * If neither is found (perhaps b/c the vendor just assumes
           * default values), then there's not much we can do, we can
           * only guess.
           *
           * This does mean in-band communication is slower (doing
           * excessive early probing).  It's a justified cost to me.
           */
          
          if (!ipmi_locate_discover_device_info (IPMI_INTERFACE_KCS, &locate_info))
            {
              if (!(ipmi_ctx_open_inband (ipmi_ctx,
                                          IPMI_DEVICE_KCS,
                                          cmd_args->disable_auto_probe,
                                          cmd_args->driver_address,
                                          cmd_args->register_spacing,
                                          cmd_args->driver_device,
                                          workaround_flags,
                                          (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0))
                goto out;
            }

          if (!ipmi_locate_discover_device_info (IPMI_INTERFACE_SSIF, &locate_info))
            {
              if (!(ipmi_ctx_open_inband (ipmi_ctx,
                                          IPMI_DEVICE_SSIF,
                                          cmd_args->disable_auto_probe,
                                          cmd_args->driver_address,
                                          cmd_args->register_spacing,
                                          cmd_args->driver_device,
                                          workaround_flags,
                                          (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT) < 0))
                goto out;
            }
          
          if (!ipmi_ctx_open_inband (ipmi_ctx,
                                     IPMI_DEVICE_KCS,
                                     cmd_args->disable_auto_probe,
                                     cmd_args->driver_address,
                                     cmd_args->register_spacing,
                                     cmd_args->driver_device,
                                     workaround_flags,
                                     (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT))
            goto out;
          
          if (!ipmi_ctx_open_inband (ipmi_ctx,
                                     IPMI_DEVICE_SSIF,
                                     cmd_args->disable_auto_probe,
                                     cmd_args->driver_address,
                                     cmd_args->register_spacing,
                                     cmd_args->driver_device,
                                     workaround_flags,
                                     (cmd_args->debug) ? IPMI_FLAGS_DEBUG_DUMP : IPMI_FLAGS_DEFAULT))
            goto out;
          
          /* else error ... */
          snprintf(errmsg,
                   errmsglen,
                   "could not find inband device");
          goto cleanup;
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
              if (ipmi_ctx_errnum(ipmi_ctx) == IPMI_ERR_DEVICE_NOT_FOUND)
                snprintf(errmsg,
                         errmsglen,
                         "%s: %s",
                         progname,
                         ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
              else
                snprintf(errmsg,
                         errmsglen,
                         "ipmi_ctx_open_inband: %s",
                         ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
              goto cleanup;
            }
        }
    }

 out:
  return ipmi_ctx;

 cleanup:
  if (ipmi_ctx)
    {
      ipmi_ctx_close (ipmi_ctx);
      ipmi_ctx_destroy (ipmi_ctx);
    }
  return NULL;
}              

/* Check if kg len is decent */
int
check_kg_len(const char *instr)
{
  assert(instr != NULL);
  
  if (strlen(instr) == 0)
    return 0;

  if (strncasecmp(instr, "0x", 2) == 0) 
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH*2+2)
        return -1;
    }
  else
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH)
        return -1;
    }

  return 0;
}

/* a k_g key is interpreted as ascii text unless it is prefixed with
   "0x", in which case is it interpreted as hexadecimal */
int
parse_kg(unsigned char *outbuf, int outsz, const char *instr)
{
  char *p, *q;
  int i, j;
  char buf[3] = {0, 0, 0};
  int rv = 0;

  assert(outbuf != NULL);
  assert(instr != NULL);
  assert(outsz > IPMI_MAX_K_G_LENGTH);

  if (strlen(instr) == 0)
    return 0;

  if (strncasecmp(instr, "0x", 2) == 0) 
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH*2+2)
        return -1;
      p = (char *)instr + 2;
      memset(outbuf, 0, IPMI_MAX_K_G_LENGTH);
      for (i = j = 0; i < strlen(p); i+=2, j++)
        {
          if (!isxdigit(p[i])
              || (p[i+1] && !isxdigit(p[i+1])))
            return -1;
          buf[0] = p[i]; 
          if (p[i+1])
            buf[1] = p[i+1]; 
          else
            buf[1] = 0;
          buf[2] = '\0';
          errno = 0;
          outbuf[j] = strtoul(buf, &q, 16);
          if (errno 
              || ((p[i+1] && (q != buf + 2))
                  || (!p[i+1] && (q != buf + 1))))
            return -1;
          rv++;
        }
    }
  else
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH)
        return -1;
      memset(outbuf, 0, IPMI_MAX_K_G_LENGTH);
      memcpy(outbuf, instr, strlen(instr));
      rv = strlen(instr);
    }

  return rv;
}

char *
format_kg(char *outstr, int outsz, const unsigned char *k_g)
{
  int i;
  int printable = 1;
  int foundnull = 0;
  char *p;

  assert(outstr != NULL);
  assert(outsz > IPMI_MAX_K_G_LENGTH*2+2);
  assert(k_g != NULL);

  /* Are there any characters that would prevent printing this as a
     string on a single line? */
  for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
    {
      if (k_g[i] == '\0')
        {
          ++foundnull;
          continue;
        }
      if (!(isgraph(k_g[i]) || k_g[i] == ' ') || foundnull)
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
  if (k_g[0] == '0' && k_g[1] == 'x')
    printable = 0;

  if (printable)
    {
      if (outsz < IPMI_MAX_K_G_LENGTH+1)
        return NULL;
      p = outstr;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
        {
          if (k_g[i] == '\0')
            break;
          p[i] = k_g[i];
        }
      p[i] = 0;
    }
  else
    {
      if (outsz < IPMI_MAX_K_G_LENGTH*2+3)
        return NULL;
      p = outstr;
      p[0] = '0'; p[1] = 'x';
      p+=2;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++, p+=2)
        sprintf(p, "%02x", k_g[i]);
    }

  return outstr;
}

