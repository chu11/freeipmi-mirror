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
#include <pwd.h>
#include <sys/param.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */

#include <assert.h>
#include <errno.h>

#define SDR_CACHE_DIR                     "sdr-cache"
#define SDR_CACHE_FILENAME_PREFIX         "sdr-cache"
#define FREEIPMI_CONFIG_DIRECTORY_MODE    0700

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define MAXIPADDRLEN 128

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"

#include "tool-sdr-cache-common.h"

#include "freeipmi-portability.h"

static int
_get_home_directory (pstdout_state_t pstate,
                     char *buf,
                     unsigned int buflen)
{
  uid_t user_id;
  struct passwd *user_passwd;
  long int tbuf_len;
  char *tbuf;
  int ret;

  assert(pstate);
  assert(buf);
  assert(buflen);

#if defined(_SC_GETPW_R_SIZE_MAX)
  tbuf_len = sysconf(_SC_GETPW_R_SIZE_MAX);
  if (tbuf_len < 0)
    /* Variable was not implemented */
#endif
    tbuf_len = 1024;	/* XXX */

  if (!(user_passwd = alloca (sizeof (*user_passwd))))
    {
      pstdout_perror(pstate, "alloca");
      return -1;
    }

  if (!(tbuf = alloca (tbuf_len)))
    {
      pstdout_perror(pstate, "alloca");
      return -1;
    }

  user_id = getuid ();
  if (getpwuid_r (user_id, 
                  user_passwd, 
                  tbuf,
                  tbuf_len,
                  &user_passwd) != 0)
    {
      pstdout_perror(pstate, "getpwuid_r");
      return -1;
    }

  if (!user_passwd) 
    {
      /* User not found - can't figure out cache directory */
      pstdout_perror(pstate, "getpwuid_r");
      return -1;
    }

  if (user_passwd->pw_dir)
    {
      if (!access (user_passwd->pw_dir, R_OK|W_OK|X_OK)) {
        if (strlen(user_passwd->pw_dir) > (buflen - 1))
          {
            pstdout_fprintf(pstate, 
                            stderr,
                            "internal overflow error\n");
            return -1;
          }
        strcpy(buf, user_passwd->pw_dir);
        return 0;
      }
    }

  if ((ret = snprintf(buf, 
                      buflen,
                      "/tmp/.%s-%s",
                      PACKAGE_NAME, 
                      user_passwd->pw_name)) < 0)
    {
      pstdout_perror(pstate, "snprintf");
      return -1;
    }

  if (ret >= buflen)
    {
      pstdout_fprintf(pstate, 
                      stderr,
                      "snprintf invalid bytes written\n");
      return -1;
    }
           
  if (access (buf, R_OK|W_OK|X_OK) < 0)
    {
      if (errno == ENOENT)
	{
	  if (mkdir (buf, FREEIPMI_CONFIG_DIRECTORY_MODE) < 0)
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "Cannot make cache directory: %s: %s\n",
                              buf,
                              errno);
              return -1;
            }
	}
      else
        {
          pstdout_fprintf(pstate,
                          stderr,
                          "Cannot access cache directory: %s\n",
                          buf);
          return -1;
        }
    }

  return 0;
}

static int
_get_config_directory (pstdout_state_t pstate,
                       const char *cache_dir,
                       char *buf,
                       unsigned int buflen)
{
  char tbuf[MAXPATHLEN+1];
  int ret;

  assert(pstate);
  assert(buf);
  assert(buflen);

  memset(tbuf, '\0', MAXPATHLEN+1);
  if (!cache_dir)
    {
      if (_get_home_directory(pstate,
                              tbuf,
                              MAXPATHLEN) < 0)
        return -1;
    }
  else
    {
      if (strlen(cache_dir) > (MAXPATHLEN - 1))
        {
          pstdout_fprintf(pstate, 
                          stderr,
                          "internal overflow error\n");
          return -1;
        }
      strcpy(tbuf, cache_dir);

      if (access (tbuf, R_OK|W_OK|X_OK) < 0)
        {
          if (errno == ENOENT)
            {
              if (mkdir (tbuf, FREEIPMI_CONFIG_DIRECTORY_MODE) < 0)
                {
                  pstdout_fprintf(pstate,
                                  stderr,
                                  "Cannot make cache directory: %s: %s\n",
                                  tbuf,
                                  errno);
                  return -1;
                }
            }
          else
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "Cannot access cache directory: %s\n",
                              tbuf);
              return -1;
            }
        }
    }
  
  if ((ret = snprintf(buf, 
                      buflen,
                      "%s/.%s",
                      tbuf,
                      PACKAGE_NAME)) < 0)
    {
      pstdout_perror(pstate, "snprintf");
      return -1;
    }

  if (ret >= buflen)
    {
      pstdout_fprintf(pstate, 
                      stderr,
                      "snprintf invalid bytes written\n");
      return -1;
    }

  return 0;
}

int
sdr_cache_get_cache_directory (pstdout_state_t pstate,
                               const char *cache_dir,
                               char *buf,
                               unsigned int buflen)
{
  char tbuf[MAXPATHLEN+1];
  int ret;

  assert(pstate);
  assert(buf);
  assert(buflen);
  
  memset(tbuf, '\0', MAXPATHLEN+1);
  if (_get_config_directory(pstate,
                            cache_dir,
                            tbuf,
                            MAXPATHLEN) < 0)
    return -1;

  if ((ret = snprintf(buf, 
                      buflen,
                      "%s/%s",
                      tbuf,
                      SDR_CACHE_DIR)) < 0)
    {
      pstdout_perror(pstate, "snprintf");
      return -1;
    }
  
  if (ret >= buflen)
    {
      pstdout_fprintf(pstate, 
                      stderr,
                      "snprintf invalid bytes written\n");
      return -1;
    }
  
  return 0;
}

static int
_get_cache_filename (pstdout_state_t pstate,
                     const char *hostname,
                     const char *cache_dir,
                     char *buf,
                     unsigned int buflen)
{
  char hostnamebuf[MAXHOSTNAMELEN+1];
  char sdrcachebuf[MAXPATHLEN+1];
  char *ptr;
  int ret;

  assert(pstate);
  assert(buf);
  assert(buflen);

  memset(hostnamebuf, '\0', MAXHOSTNAMELEN+1);
  if (gethostname(hostnamebuf, MAXHOSTNAMELEN) < 0)
    snprintf(hostnamebuf, MAXHOSTNAMELEN, "localhost");

  /* shorten hostname if necessary */
  if ((ptr = strchr(hostnamebuf, '.')))
    *ptr = '\0';
  
  if (sdr_cache_get_cache_directory(pstate,
                                    cache_dir,
                                    sdrcachebuf,
                                    MAXPATHLEN) < 0)
    return -1;

  if ((ret = snprintf(buf, 
                      buflen,
                      "%s/%s-%s.%s", 
                      sdrcachebuf, 
                      SDR_CACHE_FILENAME_PREFIX, 
                      hostnamebuf,
                      hostname ? hostname : "localhost")) < 0)
    
    {
      pstdout_perror(pstate, "snprintf");
      return -1;
    }
  
  if (ret >= buflen)
    {
      pstdout_fprintf(pstate, 
                      stderr,
                      "snprintf invalid bytes written\n");
      return -1;
    }

  return 0;
}

static int 
_setup_sdr_cache_directory (pstdout_state_t pstate,
                            const char *hostname,
                            const char *cache_dir)
{
  char configbuf[MAXPATHLEN+1];
  char cachebuf[MAXPATHLEN+1];
  int ret;
  
  assert(pstate);

  memset(configbuf, '\0', MAXPATHLEN+1);
  memset(cachebuf, '\0', MAXPATHLEN+1);

  if (_get_config_directory(pstate,
                            cache_dir,
                            configbuf,
                            MAXPATHLEN) < 0)
    return -1;

  errno = 0;
  ret = mkdir (configbuf, FREEIPMI_CONFIG_DIRECTORY_MODE);
  if (ret < 0 && errno != EEXIST)
    {
      pstdout_fprintf(pstate,
                      stderr,
                      "Cannot make cache directory: %s: %s\n",
                      configbuf,
                      errno);
      return -1;
    }

  if (sdr_cache_get_cache_directory(pstate,
                                    cache_dir,
                                    cachebuf,
                                    MAXPATHLEN) < 0)
    return -1;

  errno = 0;
  ret = mkdir (cachebuf, FREEIPMI_CONFIG_DIRECTORY_MODE);
  if (ret < 0 && errno != EEXIST)
    {
      pstdout_fprintf(pstate,
                      stderr,
                      "Cannot make cache directory: %s: %s\n",
                      cachebuf,
                      errno);
      return -1;
    }
  
  return 0;
}

void
_sdr_cache_create_callback(uint8_t sdr_version,
                           uint16_t record_count,
                           uint32_t most_recent_addition_timestamp,
                           uint32_t most_recent_erase_timestamp,
                           uint16_t record_id,
                           void *data)
{
  int *count;

  assert(data);

  count = (int *)data;

  (*count)++;

  /* pstdout library can't handle \r, its the responsibility of
   * tool code to set quiet_cache if there are multiple
   * hosts are generating the cache at the same time.
   */
  fprintf (stderr, 
           "Caching SDR record %d of %d (current record ID %d) \r",
           *count,
           record_count,
           record_id);
}


int 
sdr_cache_create (ipmi_sdr_cache_ctx_t ctx,
                  pstdout_state_t pstate,
                  ipmi_ctx_t ipmi_ctx,
                  int quiet_cache,
                  const char *hostname,
                  const char *cache_dir)
{
  char cachedirectorybuf[MAXPATHLEN+1];
  char cachefilenamebuf[MAXPATHLEN+1];
  struct stat buf;
  int count = 0;
  int rv = -1;

  assert(ctx);
  assert(pstate);
  assert(ipmi_ctx);
  
  if (sdr_cache_get_cache_directory (pstate,
                                     cache_dir,
                                     cachedirectorybuf,
                                     MAXPATHLEN) < 0)
    goto cleanup;

  if (stat(cachedirectorybuf, &buf) < 0)
    {
      if (_setup_sdr_cache_directory (pstate,
                                      hostname,
                                      cache_dir) < 0)
        goto cleanup;
    }

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_get_cache_filename(pstate,
                          hostname,
                          cache_dir,
                          cachefilenamebuf,
                          MAXPATHLEN) < 0)
    goto cleanup;

  /* pstdout library can't handle \r, its the responsibility of
   * tool code to set quiet_cache if there are multiple
   * hosts are generating the cache at the same time.
   */
  if (!quiet_cache)
    fprintf (stderr, "Caching SDR repository information ... \n");

  if (ipmi_sdr_cache_create(ctx,
                            ipmi_ctx,
                            cachefilenamebuf,
                            IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT,
                            IPMI_SDR_CACHE_VALIDATION_FLAGS_DEFAULT,
                            quiet_cache ? NULL : _sdr_cache_create_callback,
                            quiet_cache ? NULL : (void *)&count) < 0)
    {
      pstdout_fprintf(pstate,
                      stderr,
                      "ipmi_sdr_cache_create: %s\n",
                      ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(ctx)));
      goto cleanup;
    }

  if (!quiet_cache)
    fprintf (stderr, "\n");

  rv = 0;
 cleanup:
  if (rv < 0)
    ipmi_sdr_cache_delete(ctx, cachefilenamebuf);
  return rv;
}

int
sdr_cache_create_and_load (ipmi_sdr_cache_ctx_t ctx,
                           pstdout_state_t pstate,
                           ipmi_ctx_t ipmi_ctx,
                           int quiet_cache,
                           const char *hostname,
                           const char *cache_dir)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  int rv = -1;

  assert(ctx);
  assert(pstate);

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_get_cache_filename(pstate,
                          hostname,
                          cache_dir,
                          cachefilenamebuf,
                          MAXPATHLEN) < 0)
    goto cleanup;

  if (ipmi_sdr_cache_open(ctx,
                          ipmi_ctx,
                          cachefilenamebuf) < 0)
    {
      if (ipmi_sdr_cache_ctx_errnum(ctx) != IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST)
        {
          if (ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID)
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "SDR Cache '%s' invalid: Please flush the cache and regenerate it\n",
                              cachefilenamebuf);
              goto cleanup;
            }
          else if (ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE)
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "SDR Cache '%s' out of date: Please flush the cache and regenerate it\n",
                              cachefilenamebuf);
              goto cleanup;
              
            }
          else
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "ipmi_sdr_cache_open: %s: %s\n",
                              cachefilenamebuf,
                              ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(ctx)));
              goto cleanup;
            }
        }
    }

  if (ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID)
    {
      if (sdr_cache_create (ctx,
                            pstate,
                            ipmi_ctx,
                            quiet_cache,
                            hostname,
                            cache_dir) < 0)
        goto cleanup;

      if (ipmi_sdr_cache_open(ctx,
                              ipmi_ctx,
                              cachefilenamebuf) < 0)
        {
          pstdout_fprintf(pstate,
                          stderr,
                          "ipmi_sdr_cache_open: %s: %s\n",
                          cachefilenamebuf,
                          ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(ctx)));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  return rv;
}

int 
sdr_cache_flush_cache (ipmi_sdr_cache_ctx_t ctx,
                       pstdout_state_t pstate,
                       int quiet_cache,
                       const char *hostname,
                       const char *cache_dir)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  int rv = -1;

  assert(ctx);
  assert(pstate);

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_get_cache_filename(pstate,
                          hostname,
                          cache_dir,
                          cachefilenamebuf,
                          MAXPATHLEN) < 0)
    goto cleanup;
  
  if (!quiet_cache)
    pstdout_printf (pstate, "flushing cache... ");

  if (ipmi_sdr_cache_delete(ctx, cachefilenamebuf) < 0)
    {
      pstdout_fprintf(pstate,
                      stderr,
                      "ipmi_sdr_cache_delete: %s\n",
                      ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(ctx)));
      goto cleanup;
    }

  if (!quiet_cache)
    pstdout_printf (pstate, "done\n");

  rv = 0;
 cleanup:
  return rv;
}
