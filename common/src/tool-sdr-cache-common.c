/*
  Copyright (C) 2006 FreeIPMI Core Team

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
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <sys/stat.h>
#include <sys/param.h>
#include <limits.h>
#include <arpa/inet.h>
#include <pwd.h>
#include <syslog.h>
#include <assert.h>
#include <errno.h>

#define SDR_CACHE_DIR                "sdr-cache"
#define SDR_CACHE_FILENAME_PREFIX    "sdr-cache"
#define FREEIPMI_CONFIG_DIRECTORY_MODE    0700

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define MAXIPADDRLEN 128

#include "freeipmi/api/ipmi-sdr-repository-cmds-api.h"
#include "freeipmi/api/ipmi-sensor-cmds-api.h"
#include "freeipmi/cmds/ipmi-sdr-repository-cmds.h"
#include "freeipmi/cmds/ipmi-sensor-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/util/ipmi-sensor-util.h"

#include "freeipmi-portability.h"

#if 0
#define GETHOSTBYNAME_AUX_BUFLEN 1024

static int
_get_home_directory (sdr_cache_ctx_t ctx,
                     char *homebuf,
                     unsigned int homebuflen)
{
  uid_t user_id;
  struct passwd *user_passwd = alloca (sizeof (*user_passwd));
  long int buf_len;
  char *buf;
  int ret;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(homebuf);
  assert(homebuflen);

#if defined(_SC_GETPW_R_SIZE_MAX)
  buf_len = sysconf(_SC_GETPW_R_SIZE_MAX);
  if (buf_len < 0)
    /* Variable was not implemented */
#endif
    buf_len = 1024;	/* XXX */
  if (!(buf = alloca (buf_len)))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
      return -1;
    }

  user_id = getuid ();
  if (getpwuid_r (user_id, user_passwd, buf, buf_len, &user_passwd) != 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  if (!user_passwd) 
    {
      /* User not found - can't figure out cache directory */
      ctx->errnum = SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY;
      return -1;
    }

  if (user_passwd->pw_dir)
    {
      if (!access (user_passwd->pw_dir, R_OK|W_OK|X_OK)) {
        if (homebuflen < strlen(user_passwd->pw_dir))
          {
            ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
            return -1;
          }
        strcpy(homebuf, user_passwd->pw_dir);
        return 0;
      }
    }

  if ((ret = snprintf(homebuf, 
                      homebuflen,
                      "/tmp/.%s-%s",
                      PACKAGE_NAME, 
                      user_passwd->pw_name)) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  if (ret >= homebuflen)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }
           
  if (access (homebuf, R_OK|W_OK|X_OK) < 0)
    {
      if (errno == ENOENT)
	{
	  if (mkdir (homebuf, FREEIPMI_CONFIG_DIRECTORY_MODE) < 0)
            {
              if (errno == EPERM 
                  || errno == EACCES)
                ctx->errnum = SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_PERMISSION;
              else
                ctx->errnum = SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_CREATION;
              return -1;
            }
	}
      else
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_PERMISSION;
          return -1;
        }
    }

  return 0;
}

static int
_get_freeipmi_config_directory (sdr_cache_ctx_t ctx,
                                char *user_cache_dir,
                                char *configbuf,
                                unsigned int configbuflen)
{
  char homebuf[MAXPATHLEN+1];
  int ret;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(configbuf);
  assert(configbuflen);

  memset(homebuf, '\0', MAXPATHLEN+1);
  if (!user_cache_dir)
    {
      if (_get_home_directory(ctx,
                              homebuf,
                              MAXPATHLEN) < 0)
        return -1;
    }
  else
    {
      if (MAXPATHLEN < strlen(user_cache_dir))
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          return -1;
        }
      strcpy(homebuf, user_cache_dir);

      if (access (homebuf, R_OK|W_OK|X_OK) < 0)
        {
          if (errno == ENOENT)
            {
              if (mkdir (homebuf, FREEIPMI_CONFIG_DIRECTORY_MODE) < 0)
                {
                  if (errno == EPERM 
                      || errno == EACCES)
                    ctx->errnum = SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_PERMISSION;
                  else
                    ctx->errnum = SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_CREATION;
                  return -1;
                }
            }
          else
            {
              ctx->errnum = SDR_CACHE_CTX_ERR_SDR_CACHE_DIRECTORY_PERMISSION;
              return -1;
            }
        }
    }
  
  if ((ret = snprintf(configbuf, 
                      configbuflen,
                      "%s/.%s",
                      homebuf,
                      PACKAGE_NAME)) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  if (ret >= configbuflen)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  return 0;
}

static int
_get_sdr_cache_directory (sdr_cache_ctx_t ctx,
                          char *user_cache_dir,
                          char *cachebuf,
                          unsigned int cachebuflen)
{
  char configbuf[MAXPATHLEN+1];
  int ret;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cachebuf);
  assert(cachebuflen);
  
  memset(configbuf, '\0', MAXPATHLEN+1);
  if (_get_freeipmi_config_directory(ctx,
                                     user_cache_dir,
                                     configbuf,
                                     MAXPATHLEN) < 0)
    return -1;

  if ((ret = snprintf(cachebuf, 
                      cachebuflen,
                      "%s/%s",
                      configbuf,
                      SDR_CACHE_DIR)) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  if (ret >= cachebuflen)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  return 0;
}

static int
_get_ipmi_host_ip_address (sdr_cache_ctx_t ctx,
                           char *host,
                           char *ipbuf,
                           unsigned int ipbuflen)
{
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(ipbuf);
  assert(ipbuflen);

  if (host) /* OUT-OF-BAND */
    {
#ifdef HAVE_FUNC_GETHOSTBYNAME_R_6
      struct hostent hent;
      int h_errnop;
      char buf[GETHOSTBYNAME_AUX_BUFLEN];
#endif /* HAVE_FUNC_GETHOSTBYNAME_R_6 */
      struct hostent *hptr;
      struct in_addr *in_addr;
      
#ifdef HAVE_FUNC_GETHOSTBYNAME_R_6
      memset(&hent, '\0', sizeof(struct hostent));
      if (gethostbyname_r(host,
                          &hent,
                          buf,
                          GETHOSTBYNAME_AUX_BUFLEN,
                          &hptr,
                          &h_errnop) != 0)
        {
          if (h_errnop == HOST_NOT_FOUND
              || h_errnop == NO_ADDRESS
              || h_errnop == NO_DATA)
            ctx->errnum = SDR_CACHE_CTX_ERR_HOSTNAME_INVALID;
          else
            ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          return -1;
        }

      if (!hptr)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_HOSTNAME_INVALID;
          return -1;
        }
#else /* !HAVE_FUNC_GETHOSTBYNAME_R */
#error Additional threadsafe gethostbyname support needed
#endif /* !HAVE_FUNC_GETHOSTBYNAME_R */

      in_addr = (struct in_addr *) hptr->h_addr_list[0];
      if (inet_ntop(AF_INET, 
                    &(in_addr->s_addr),
                    ipbuf,
                    ipbuflen) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          return -1;
        }
    }
  else /* IN-BAND */
    {
      int ret;
  
      if ((ret = snprintf(ipbuf, 
                          ipbuflen,
                          "127.0.0.1")) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          return -1;
        }

      if (ret >= ipbuflen)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          return -1;
        }
    }
  
  return 0;
}

static int
_get_sdr_cache_filename (sdr_cache_ctx_t ctx,
                         char *host,
                         char *user_cache_dir,
                         char *cachefilenamebuf,
                         unsigned int cachefilenamebuflen)
{
  char hostbuf[MAXHOSTNAMELEN+1];
  char ipbuf[MAXIPADDRLEN+1];
  char cachebuf[MAXPATHLEN+1];
  char *ptr;
  int ret;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cachefilenamebuf);
  assert(cachefilenamebuflen);

  memset(hostbuf, '\0', MAXHOSTNAMELEN+1);
  if (gethostname(hostbuf, MAXHOSTNAMELEN) < 0)
    snprintf(hostbuf, MAXHOSTNAMELEN, "localhost");

  /* shorten hostname if necessary */
  if ((ptr = strchr(hostbuf, '.')))
    *ptr = '\0';
  
  if (_get_ipmi_host_ip_address(ctx,
                                host,
                                ipbuf,
                                MAXIPADDRLEN) < 0)
    return -1;

  if (_get_sdr_cache_directory(ctx,
                               user_cache_dir,
                               cachebuf,
                               MAXPATHLEN) < 0)
    return -1;

  if ((ret = snprintf(cachefilenamebuf, 
                      cachefilenamebuflen,
                      "%s/%s-%s.%s", 
                      cachebuf, 
                      SDR_CACHE_FILENAME_PREFIX, 
                      hostbuf,
                      ipbuf)) < 0)
                      
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }
  
  if (ret >= cachefilenamebuflen)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  return 0;
}

static int 
_setup_sdr_cache_directory (sdr_cache_ctx_t ctx,
                            char *host,
                            char *user_cache_dir)
{
  char configbuf[MAXPATHLEN+1];
  char cachebuf[MAXPATHLEN+1];
  int ret;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);

  memset(configbuf, '\0', MAXPATHLEN+1);
  memset(cachebuf, '\0', MAXPATHLEN+1);

  if (_get_freeipmi_config_directory(ctx,
                                     user_cache_dir,
                                     configbuf,
                                     MAXPATHLEN) < 0)
    return -1;

  errno = 0;
  ret = mkdir (configbuf, FREEIPMI_CONFIG_DIRECTORY_MODE);
  if (ret < 0 && errno != EEXIST)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  if (_get_sdr_cache_directory(ctx,
                               user_cache_dir,
                               cachebuf,
                               MAXPATHLEN) < 0)
    return -1;

  errno = 0;
  ret = mkdir (cachebuf, FREEIPMI_CONFIG_DIRECTORY_MODE);
  if (ret < 0 && errno != EEXIST)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }
  
  return 0;
}

int 
flush_sdr_cache (void)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  int rv = -1;
  
  if (!ctx || ctx->magic != SDR_CACHE_CTX_MAGIC)
    return -1;

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_get_sdr_cache_filename(ctx,
                              host,
                              user_cache_dir,
                              cachefilenamebuf,
                              MAXPATHLEN) < 0)
    goto cleanup;

  if (unlink(cachefilenamebuf) < 0)
    {
      if (errno == ENOENT)
        {
          /* actually this ok */
          rv = 0;
          goto cleanup;
        }
      if (errno == EPERM
          || errno == EACCES)
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_PERMISSION;
      else
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      goto cleanup;
    }

  rv = 0;
  ctx->errnum = SDR_CACHE_CTX_ERR_SUCCESS;
 cleanup:
  return rv;
}
#endif
