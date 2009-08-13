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
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi-portability.h"
#include "pstdout.h"

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

#define IPMI_SDR_RECORD_TYPE_ALL_RECORDS                                 0x0000
#define IPMI_SDR_RECORD_TYPE_FULL_RECORD                                 0x0001
#define IPMI_SDR_RECORD_TYPE_COMPACT_RECORD                              0x0002
#define IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD                           0x0004
#define IPMI_SDR_RECORD_TYPE_ENTITY_ASSOCIATION_RECORD                   0x0008     
#define IPMI_SDR_RECORD_TYPE_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD   0x0010
#define IPMI_SDR_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD               0x0020
#define IPMI_SDR_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD                   0x0040
#define IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD 0x0080
#define IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD   0x0100
#define IPMI_SDR_RECORD_TYPE_BMC_MESSAGE_CHANNEL_INFO_RECORD             0x0200
#define IPMI_SDR_RECORD_TYPE_OEM_RECORD                                  0x0400

#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/util/ipmi-sensor-util.h"

#include "tool-sdr-cache-common.h"
#include "tool-fiid-wrappers.h"

#include "freeipmi-portability.h"

/* achu:
 *
 * We don't have "state_data" so can't use most of the 
 * tool-fiid-wrappers.h.
 */

#define _SDR_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                      \
  do {                                                                   \
    if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)              \
      {                                                                  \
        PSTDOUT_PERROR(pstate, "fiid_template_len_bytes");               \
        goto cleanup;                                                    \
      }                                                                  \
  } while (0)

#define _SDR_FIID_OBJ_CREATE(__obj, __tmpl)                              \
  do {                                                                   \
    if (!((__obj) = fiid_obj_create ((__tmpl))))                         \
      {                                                                  \
        PSTDOUT_PERROR(pstate, "fiid_obj_create");                       \
        goto cleanup;                                                    \
      }                                                                  \
  } while (0)

#define _SDR_FIID_OBJ_COPY(__obj_dest, __obj_src, __alt_tmpl)            \
  do {                                                                   \
    if (!((__obj_dest) = fiid_obj_copy ((__obj_src), (__alt_tmpl))))     \
      {                                                                  \
        PSTDOUT_FPRINTF(pstate,                                          \
                        stderr,                                          \
                        "fiid_obj_copy: %s\n",                           \
                        fiid_strerror(fiid_obj_errnum((__obj_src))));    \
        goto cleanup;                                                    \
      }                                                                  \
  } while (0)

#define _SDR_FIID_OBJ_GET(__obj, __field, __val)                         \
  do {                                                                   \
    uint64_t __tmp_val = 0, *__val_ptr;                                  \
    __val_ptr = (__val);                                                 \
    if (fiid_obj_get ((__obj), (__field), &__tmp_val) < 0)               \
      {                                                                  \
        PSTDOUT_FPRINTF(pstate,                                          \
                        stderr,                                          \
                        "fiid_obj_get: %s\n",                            \
                        fiid_strerror(fiid_obj_errnum((__obj))));        \
        goto cleanup;                                                    \
      }                                                                  \
    *__val_ptr = __tmp_val;                                              \
  } while (0)

#define _SDR_FIID_OBJ_GET_DATA(__obj, __field, __data, __datalen)        \
  do {                                                                   \
    if (fiid_obj_get_data ((__obj),                                      \
                           (__field),                                    \
                           (__data),                                     \
                           (__datalen)) < 0)                             \
      {                                                                  \
        PSTDOUT_FPRINTF(pstate,                                          \
                        stderr,                                          \
                        "fiid_obj_get_data: %s\n",                       \
                        fiid_strerror(fiid_obj_errnum((__obj))));        \
        goto cleanup;                                                    \
      }                                                                  \
  } while (0)

#define _SDR_FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __datalen) \
  do {                                                                       \
    if (((__len) = fiid_obj_get_data ((__obj),                               \
                                      (__field),                             \
                                      (__data),                              \
                                      (__datalen))) < 0)                     \
      {                                                                      \
        PSTDOUT_FPRINTF(pstate,                                              \
                        stderr,                                              \
                        "fiid_obj_get_data: %s\n",                           \
                        fiid_strerror(fiid_obj_errnum((__obj))));            \
        goto cleanup;                                                        \
      }                                                                      \
  } while (0)

#define _SDR_FIID_OBJ_SET_ALL(__obj, __data, __datalen)                  \
  do {                                                                   \
    if (fiid_obj_set_all ((__obj),                                       \
                          (__data),                                      \
                          (__datalen)) < 0)                              \
      {                                                                  \
        PSTDOUT_FPRINTF(pstate,                                          \
                        stderr,                                          \
                        "fiid_obj_set_all: %s\n",                        \
                        fiid_strerror(fiid_obj_errnum((__obj))));        \
        goto cleanup;                                                    \
      }                                                                  \
  } while (0)

static int
_get_home_directory (pstdout_state_t pstate,
                     char *buf,
                     unsigned int buflen)
{
  uid_t user_id;
  struct passwd pwd;
#if defined(HAVE_FUNC_GETPWUID_R_5)
  struct passwd *pwdptr = NULL;
#endif
  long int tbuf_len;
  char *tbuf;
  int ret;

  assert(buf);
  assert(buflen);

#if defined(_SC_GETPW_R_SIZE_MAX)
  tbuf_len = sysconf(_SC_GETPW_R_SIZE_MAX);
  if (tbuf_len < 0)
    /* Variable was not implemented */
#endif
    tbuf_len = 1024;	/* XXX */

  if (!(tbuf = alloca (tbuf_len)))
    {
      PSTDOUT_PERROR(pstate, "alloca");
      return -1;
    }
  
  user_id = getuid ();
  memset(&pwd, '\0', sizeof(struct passwd));
#if defined(HAVE_FUNC_GETPWUID_R_5)
  if (getpwuid_r (user_id, 
                  &pwd,
                  tbuf,
                  tbuf_len,
                  &(pwdptr)) != 0)
    {
      PSTDOUT_PERROR(pstate, "getpwuid_r");
      return -1;
    }

  if (!pwdptr) 
    {
      /* User not found - can't figure out cache directory */
      PSTDOUT_PERROR(pstate, "getpwuid_r");
      return -1;
    }
#elif defined(HAVE_FUNC_GETPWUID_R_4)
  /* Jan Forch - Solaris getpwuid_r returns ptr, not integer */
  if (!getpwuid_r (user_id, 
                   &pwd,
                   tbuf,
                   tbuf_len))
    {
      PSTDOUT_PERROR(pstate, "getpwuid_r");
      return -1;
    }
#endif /* !defined(HAVE_FUNC_GETPWUID_R_4) */

  if (pwd.pw_dir)
    {
      if (!access (pwd.pw_dir, R_OK|W_OK|X_OK)) {
        if (strlen(pwd.pw_dir) > (buflen - 1))
          {
            PSTDOUT_FPRINTF(pstate, 
                            stderr,
                            "internal overflow error\n");
            return -1;
          }
        strcpy(buf, pwd.pw_dir);
        return 0;
      }
    }

  if ((ret = snprintf(buf, 
                      buflen,
                      "/tmp/.%s-%s",
                      PACKAGE_NAME, 
                      pwd.pw_name)) < 0)
    {
      PSTDOUT_PERROR(pstate, "snprintf");
      return -1;
    }

  if (ret >= buflen)
    {
      PSTDOUT_FPRINTF(pstate, 
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
              PSTDOUT_FPRINTF(pstate,
                              stderr,
                              "Cannot make cache directory: %s: %s\n",
                              buf,
                              strerror(errno));
              return -1;
            }
	}
      else
        {
          PSTDOUT_FPRINTF(pstate,
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
          PSTDOUT_FPRINTF(pstate, 
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
                  PSTDOUT_FPRINTF(pstate,
                                  stderr,
                                  "Cannot make cache directory: %s: %s\n",
                                  tbuf,
                                  strerror(errno));
                  return -1;
                }
            }
          else
            {
              PSTDOUT_FPRINTF(pstate,
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
      PSTDOUT_PERROR(pstate, "snprintf");
      return -1;
    }

  if (ret >= buflen)
    {
      PSTDOUT_FPRINTF(pstate, 
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
      PSTDOUT_PERROR(pstate, "snprintf");
      return -1;
    }
  
  if (ret >= buflen)
    {
      PSTDOUT_FPRINTF(pstate, 
                      stderr,
                      "snprintf invalid bytes written\n");
      return -1;
    }
  
  return 0;
}

int
sdr_cache_get_cache_filename (pstdout_state_t pstate,
                              const char *hostname,
                              const char *cache_dir,
                              char *buf,
                              unsigned int buflen)
{
  char hostnamebuf[MAXHOSTNAMELEN+1];
  char sdrcachebuf[MAXPATHLEN+1];
  char *ptr;
  int ret;

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
      PSTDOUT_PERROR(pstate, "snprintf");
      return -1;
    }
  
  if (ret >= buflen)
    {
      PSTDOUT_FPRINTF(pstate, 
                      stderr,
                      "snprintf invalid bytes written\n");
      return -1;
    }

  return 0;
}

static int 
_setup_sdr_cache_directory (pstdout_state_t pstate,
                            const char *cache_dir)
{
  char configbuf[MAXPATHLEN+1];
  char cachebuf[MAXPATHLEN+1];
  int ret;
  
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
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Cannot make cache directory: %s: %s\n",
                      configbuf,
                      strerror(errno));
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
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Cannot make cache directory: %s: %s\n",
                      cachebuf,
                      strerror(errno));
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
sdr_cache_create_directory (pstdout_state_t pstate,
                            const char *cache_dir)
{
  char cachedirectorybuf[MAXPATHLEN+1];
  struct stat buf;

  if (sdr_cache_get_cache_directory (pstate,
                                     cache_dir,
                                     cachedirectorybuf,
                                     MAXPATHLEN) < 0)
    return -1;

  if (stat(cachedirectorybuf, &buf) < 0)
    {
      if (_setup_sdr_cache_directory (pstate,
                                      cache_dir) < 0)
        return -1;
    }

  return 0;
}
                            
int 
sdr_cache_create (ipmi_sdr_cache_ctx_t ctx,
                  pstdout_state_t pstate,
                  ipmi_ctx_t ipmi_ctx,
                  int quiet_cache,
                  int sdr_cache_recreate,
                  const char *hostname,
                  const char *cache_dir)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  int count = 0;
  int create_flags = 0;
  int rv = -1;

  assert(ctx);
  assert(ipmi_ctx);
  
  if (sdr_cache_create_directory (pstate, cache_dir) < 0)
    goto cleanup;

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (sdr_cache_get_cache_filename(pstate,
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
    fprintf (stderr, 
             "Caching SDR repository information: %s\n",
             cachefilenamebuf);

  if (sdr_cache_recreate)
    create_flags = IPMI_SDR_CACHE_CREATE_FLAGS_OVERWRITE;
  else
    create_flags = IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT;

  if (ipmi_sdr_cache_create(ctx,
                            ipmi_ctx,
                            cachefilenamebuf,
                            create_flags,
                            IPMI_SDR_CACHE_VALIDATION_FLAGS_DEFAULT,
                            quiet_cache ? NULL : _sdr_cache_create_callback,
                            quiet_cache ? NULL : (void *)&count) < 0)
    {
      /* unique output corner case */
      if (count && !quiet_cache)
        fprintf (stderr, "\n");
      
      PSTDOUT_FPRINTF(pstate,
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
                           int sdr_cache_recreate,
                           const char *hostname,
                           const char *cache_dir)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  int rv = -1;

  assert(ctx);

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (sdr_cache_get_cache_filename(pstate,
                                   hostname,
                                   cache_dir,
                                   cachefilenamebuf,
                                   MAXPATHLEN) < 0)
    goto cleanup;

  if (ipmi_sdr_cache_open(ctx,
                          ipmi_ctx,
                          cachefilenamebuf) < 0)
    {
      if (ipmi_sdr_cache_ctx_errnum(ctx) != IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST
          && !((ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID
                || ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE)
               && sdr_cache_recreate))
        {
          if (ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID)
            {
              PSTDOUT_FPRINTF(pstate,
                              stderr,
                              "SDR Cache '%s' invalid: Please flush the cache and regenerate it\n",
                              cachefilenamebuf);
              goto cleanup;
            }
          else if (ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE)
            {
              PSTDOUT_FPRINTF(pstate,
                              stderr,
                              "SDR Cache '%s' out of date: Please flush the cache and regenerate it\n",
                              cachefilenamebuf);
              goto cleanup;
              
            }
          else
            {
              PSTDOUT_FPRINTF(pstate,
                              stderr,
                              "ipmi_sdr_cache_open: %s: %s\n",
                              cachefilenamebuf,
                              ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(ctx)));
              goto cleanup;
            }
        }
    }

  if (ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST
      || ((ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_INVALID
           || ipmi_sdr_cache_ctx_errnum(ctx) == IPMI_SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE)
          && sdr_cache_recreate))
    {
      if (sdr_cache_create (ctx,
                            pstate,
                            ipmi_ctx,
                            quiet_cache,
                            sdr_cache_recreate,
                            hostname,
                            cache_dir) < 0)
        goto cleanup;

      if (ipmi_sdr_cache_open(ctx,
                              ipmi_ctx,
                              cachefilenamebuf) < 0)
        {
          PSTDOUT_FPRINTF(pstate,
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

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (sdr_cache_get_cache_filename(pstate,
                                   hostname,
                                   cache_dir,
                                   cachefilenamebuf,
                                   MAXPATHLEN) < 0)
    goto cleanup;
  
  if (!quiet_cache)
    PSTDOUT_PRINTF (pstate, "Flushing cache: %s\n", cachefilenamebuf);
      
  if (ipmi_sdr_cache_delete(ctx, cachefilenamebuf) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "ipmi_sdr_cache_delete: %s\n",
                      ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return rv;
}

int 
sdr_cache_get_record_id_and_type (pstdout_state_t pstate,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len,
                                  uint16_t *record_id,
                                  uint8_t *record_type)
{
  fiid_obj_t obj_sdr_record_header = NULL;
  int32_t sdr_record_header_len;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  _SDR_FIID_TEMPLATE_LEN_BYTES(sdr_record_header_len, tmpl_sdr_record_header);

  if (sdr_record_len < sdr_record_header_len)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "sdr_record invalid len: %d\n",
                      sdr_record_len);
      goto cleanup;
    }

  _SDR_FIID_OBJ_CREATE(obj_sdr_record_header, tmpl_sdr_record_header);

  _SDR_FIID_OBJ_SET_ALL(obj_sdr_record_header,
                        sdr_record,
                        sdr_record_header_len);

  if (record_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_header, "record_id", &val);
      *record_id = val;
    }

  if (record_type)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_header, "record_type", &val);
      *record_type = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record_header);
  return rv;
}

static fiid_obj_t
_sdr_cache_get_common(pstdout_state_t pstate,
                      uint8_t *sdr_record,
                      unsigned int sdr_record_len,
                      uint32_t acceptable_record_types)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t record_type;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(acceptable_record_types);

  if (sdr_cache_get_record_id_and_type (pstate,
                                        sdr_record,
                                        sdr_record_len,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  if (!(((acceptable_record_types & IPMI_SDR_RECORD_TYPE_FULL_RECORD)
         && record_type == IPMI_SDR_FORMAT_FULL_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_COMPACT_RECORD)
            && record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD)
            && record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_ENTITY_ASSOCIATION_RECORD)
            && record_type == IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)
            && record_type == IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)

        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD)
            && record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD)
            && record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
            && record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
            && record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_BMC_MESSAGE_CHANNEL_INFO_RECORD)
            && record_type == IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD)
        || ((acceptable_record_types & IPMI_SDR_RECORD_TYPE_OEM_RECORD)
            && record_type == IPMI_SDR_FORMAT_OEM_RECORD)
        ))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid record type passed in: %X, %X\n",
                      record_type,
                      acceptable_record_types);
      goto cleanup;
    }

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_full_sensor_record);
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_compact_sensor_record);
  else if (record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_event_only_record);
  else if (record_type == IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_entity_association_record);
  else if (record_type == IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_device_relative_entity_association_record);
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_generic_device_locator_record);
  else if (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_fru_device_locator_record);
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_management_controller_device_locator_record);
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_management_controller_confirmation_record);
  else if (record_type == IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_bmc_message_channel_info_record);
  else if (record_type == IPMI_SDR_FORMAT_OEM_RECORD)
    _SDR_FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_oem_record);

  _SDR_FIID_OBJ_SET_ALL(obj_sdr_record,
                        sdr_record,
                        sdr_record_len);
  
  return obj_sdr_record;

 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return NULL;
}

int 
sdr_cache_get_sensor_owner_id (pstdout_state_t pstate,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               uint8_t *sensor_owner_id_type,
                               uint8_t *sensor_owner_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  if (sensor_owner_id_type)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "sensor_owner_id.type", &val);
      *sensor_owner_id_type = val;
    }

  if (sensor_owner_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "sensor_owner_id", &val);
      *sensor_owner_id = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_sensor_owner_lun (pstdout_state_t pstate,
                                uint8_t *sdr_record,
                                unsigned int sdr_record_len,
                                uint8_t *sensor_owner_lun,
                                uint8_t *channel_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  if (sensor_owner_lun)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "sensor_owner_lun", &val);
      *sensor_owner_lun = val;
    }

  if (channel_number)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "channel_number", &val);
      *channel_number = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_entity_id_instance_type (pstdout_state_t pstate,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint8_t *entity_id,
                                       uint8_t *entity_instance,
                                       uint8_t *entity_instance_type)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  if (entity_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "entity_id", &val);
      *entity_id = val;
    }
  if (entity_instance)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "entity_instance", &val);
      *entity_instance = val;
    }
  if (entity_instance_type)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "entity_instance.type", &val);
      *entity_instance_type = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_sensor_number (pstdout_state_t pstate,
                             uint8_t *sdr_record,
                             unsigned int sdr_record_len,
                             uint8_t *sensor_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(sensor_number);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  _SDR_FIID_OBJ_GET(obj_sdr_record, "sensor_number", &val);
  *sensor_number = val;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_sensor_type (pstdout_state_t pstate,
                           uint8_t *sdr_record,
                           unsigned int sdr_record_len,
                           uint8_t *sensor_type)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(sensor_type);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (fiid_obj_get(obj_sdr_record,
                   "sensor_type",
                   &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_sdr_record)));
      goto cleanup;
    }
  *sensor_type = val;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_event_reading_type_code (pstdout_state_t pstate,
                                       uint8_t *sdr_record,
                                       unsigned int sdr_record_len,
                                       uint8_t *event_reading_type_code)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(event_reading_type_code);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (fiid_obj_get(obj_sdr_record,
                   "event_reading_type_code",
                   &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_sdr_record)));
      goto cleanup;
    }
  *event_reading_type_code = val;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_sensor_unit (pstdout_state_t pstate,
                           uint8_t *sdr_record,
                           unsigned int sdr_record_len,
                           uint8_t *sensor_unit)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(sensor_unit);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  if (fiid_obj_get(obj_sdr_record,
                   "sensor_unit2.base_unit",
                   &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_sdr_record)));
      goto cleanup;
    }
  
  if (!IPMI_SENSOR_UNIT_VALID(val))
    val = IPMI_SENSOR_UNIT_UNSPECIFIED;
  
  *sensor_unit = val;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_id_string (pstdout_state_t pstate,
                         uint8_t *sdr_record,
                         unsigned int sdr_record_len,
                         char *id_string,
                         unsigned int id_string_len)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(id_string);
  assert(id_string_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  _SDR_FIID_OBJ_GET_DATA(obj_sdr_record,
                         "id_string",
                         (uint8_t *)id_string,
                         id_string_len);

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_device_id_string (pstdout_state_t pstate,
                                uint8_t *sdr_record,
                                unsigned int sdr_record_len,
                                char *device_id_string,
                                unsigned int device_id_string_len)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(device_id_string);
  assert(device_id_string_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  _SDR_FIID_OBJ_GET_DATA(obj_sdr_record,
                         "device_id_string",
                         (uint8_t *)device_id_string,
                         device_id_string_len);

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_sensor_capabilities (pstdout_state_t pstate,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint8_t *event_message_control_support,
                                   uint8_t *threshold_access_support,
                                   uint8_t *hysteresis_support,
                                   uint8_t *auto_re_arm_support,
                                   uint8_t *entity_ignore_support)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
    
  if (event_message_control_support)
    {
      _SDR_FIID_OBJ_GET (obj_sdr_record, 
                         "sensor_capabilities.event_message_control_support", 
                         &val);
      *event_message_control_support = val;
    }
  if (threshold_access_support)
    {
      _SDR_FIID_OBJ_GET (obj_sdr_record, 
                         "sensor_capabilities.threshold_access_support", 
                         &val);
      *threshold_access_support = val;
    }
  if (hysteresis_support)
    {
      _SDR_FIID_OBJ_GET (obj_sdr_record, 
                         "sensor_capabilities.hysteresis_support", 
                         &val);
      *hysteresis_support = val;
    }
  if (auto_re_arm_support)
    {
      _SDR_FIID_OBJ_GET (obj_sdr_record, 
                         "sensor_capabilities.auto_re_arm_support", 
                         &val);
      *auto_re_arm_support = val;
    }
  if (entity_ignore_support)
    {
      _SDR_FIID_OBJ_GET (obj_sdr_record, 
                         "sensor_capabilities.entity_ignore_support", 
                         &val);
      *entity_ignore_support = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv; 
}

int 
sdr_cache_get_sensor_decoding_data (pstdout_state_t pstate,
                                    uint8_t *sdr_record,
                                    unsigned int sdr_record_len,
                                    int8_t *r_exponent,
                                    int8_t *b_exponent,
                                    int16_t *m,
                                    int16_t *b,
                                    uint8_t *linearization,
                                    uint8_t *analog_data_format)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val, val1, val2;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(r_exponent);
  assert(b_exponent);
  assert(m);
  assert(b);
  assert(linearization);
  assert(analog_data_format);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
    
  _SDR_FIID_OBJ_GET (obj_sdr_record, "r_exponent", &val);
  *r_exponent = (int8_t) val;
  if (*r_exponent & 0x08)
    *r_exponent |= 0xF0;

  _SDR_FIID_OBJ_GET (obj_sdr_record, "b_exponent", &val);
  *b_exponent = (int8_t) val;
  if (*b_exponent & 0x08)
    *b_exponent |= 0xF0;

  _SDR_FIID_OBJ_GET (obj_sdr_record, "m_ls", &val1);
  _SDR_FIID_OBJ_GET (obj_sdr_record, "m_ms", &val2);
  *m = (int16_t)val1;
  *m |= ((val2 & 0x3) << 8);
  if (*m & 0x200)
    *m |= 0xFE00;

  _SDR_FIID_OBJ_GET (obj_sdr_record, "b_ls", &val1);
  _SDR_FIID_OBJ_GET (obj_sdr_record, "b_ms", &val2);
  *b = (int16_t)val1;
  *b |= ((val2 & 0x3) << 8);
  if (*b & 0x200)
    *b |= 0xFE00;

  _SDR_FIID_OBJ_GET (obj_sdr_record, "linearization", &val);
  *linearization = (uint8_t)val;

  _SDR_FIID_OBJ_GET (obj_sdr_record, "sensor_unit1.analog_data_format", &val);
  *analog_data_format = (uint8_t) val;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv; 
}

int
sdr_cache_get_sensor_reading_ranges (pstdout_state_t pstate,
                                     uint8_t *sdr_record,
                                     unsigned int sdr_record_len,
                                     double **nominal_reading,
                                     double **normal_maximum,
                                     double **normal_minimum,
                                     double **sensor_maximum_reading,
                                     double **sensor_minimum_reading)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  double *tmp_nominal_reading = NULL;
  double *tmp_normal_maximum = NULL;
  double *tmp_normal_minimum = NULL;
  double *tmp_sensor_maximum_reading = NULL;
  double *tmp_sensor_minimum_reading = NULL;
  uint64_t val;
  double reading;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  if (nominal_reading)
    *nominal_reading = NULL;
  if (normal_maximum)
    *normal_maximum = NULL;
  if (normal_minimum)
    *normal_minimum = NULL;
  if (sensor_maximum_reading)
    *sensor_maximum_reading = NULL;
  if (sensor_minimum_reading)
    *sensor_minimum_reading = NULL;

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  if (sdr_cache_get_sensor_decoding_data(pstate,
                                         sdr_record,
                                         sdr_record_len,
                                         &r_exponent,
                                         &b_exponent,
                                         &m,
                                         &b,
                                         &linearization,
                                         &analog_data_format) < 0)
    goto cleanup;

  /* if the sensor is not analog, this is most likely a bug in the
   * SDR, since we shouldn't be decoding a non-threshold sensor.
   *
   * Don't return an error.  Allow code to output "NA" or something.
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format))
    {
      rv = 0;
      goto cleanup;
    }

  /* if the sensor is non-linear, I just don't know what to do
   *
   * Don't return an error.  Allow code to output "NA" or something.
   */
  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization))
    {
      rv = 0;
      goto cleanup;
    }

  if (nominal_reading)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record,
                        "nominal_reading",
                        &val);

      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linearization,
                                    analog_data_format,
                                    val,
                                    &reading) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sensor_decode_value: %s\n",
                           strerror(errno));
          goto cleanup;
        }

      if (!(tmp_nominal_reading = (double *)malloc(sizeof(double))))
        {
          PSTDOUT_PERROR(pstate, "malloc");
          goto cleanup;
        }
      *tmp_nominal_reading = reading;
    }
  if (normal_maximum)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record,
                        "normal_maximum",
                        &val);

      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linearization,
                                    analog_data_format,
                                    val,
                                    &reading) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sensor_decode_value: %s\n",
                           strerror(errno));
          goto cleanup;
        }

      if (!(tmp_normal_maximum = (double *)malloc(sizeof(double))))
        {
          PSTDOUT_PERROR(pstate, "malloc");
          goto cleanup;
        }
      *tmp_normal_maximum = reading;
    }
  if (normal_minimum)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record,
                        "normal_minimum",
                        &val);
      
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linearization,
                                    analog_data_format,
                                    val,
                                    &reading) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sensor_decode_value: %s\n",
                           strerror(errno));
          goto cleanup;
        }

      if (!(tmp_normal_minimum = (double *)malloc(sizeof(double))))
        {
          PSTDOUT_PERROR(pstate, "malloc");
          goto cleanup;
        }
      *tmp_normal_minimum = reading;
    }
  if (sensor_maximum_reading)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record,
                        "sensor_maximum_reading",
                        &val);

      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linearization,
                                    analog_data_format,
                                    val,
                                    &reading) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sensor_decode_value: %s\n",
                           strerror(errno));
          goto cleanup;
        }

      if (!(tmp_sensor_maximum_reading = (double *)malloc(sizeof(double))))
        {
          PSTDOUT_PERROR(pstate, "malloc");
          goto cleanup;
        }
      *tmp_sensor_maximum_reading = reading;
    }
  if (sensor_minimum_reading)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record,
                        "sensor_minimum_reading",
                        &val);

      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linearization,
                                    analog_data_format,
                                    val,
                                    &reading) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sensor_decode_value: %s\n",
                           strerror(errno));
          goto cleanup;
        }

      if (!(tmp_sensor_minimum_reading = (double *)malloc(sizeof(double))))
        {
          PSTDOUT_PERROR(pstate, "malloc");
          goto cleanup;
        }
      *tmp_sensor_minimum_reading = reading;
    }
  
  if (nominal_reading)
    *nominal_reading = tmp_nominal_reading;
  if (normal_maximum)
    *normal_maximum = tmp_normal_maximum;
  if (normal_minimum)
    *normal_minimum = tmp_normal_minimum;
  if (sensor_maximum_reading)
    *sensor_maximum_reading = tmp_sensor_maximum_reading;
  if (sensor_minimum_reading)
    *sensor_minimum_reading = tmp_sensor_minimum_reading;
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  if (rv < 0)
    {
      if (tmp_nominal_reading)
        free(nominal_reading);
      if (tmp_normal_maximum)
        free(normal_maximum);
      if (tmp_normal_minimum)
        free(normal_minimum);
      if (tmp_sensor_maximum_reading)
        free(sensor_maximum_reading);
      if (tmp_sensor_minimum_reading)
        free(sensor_minimum_reading);
    }
  return rv; 
}

int
sdr_cache_get_hysteresis (pstdout_state_t pstate,
                          uint8_t *sdr_record,
                          unsigned int sdr_record_len,
                          uint8_t *positive_going_threshold_hysteresis,
                          uint8_t *negative_going_threshold_hysteresis)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (positive_going_threshold_hysteresis)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record,
                        "positive_going_threshold_hysteresis",
                        &val);
      *positive_going_threshold_hysteresis = val;
    }
  if (negative_going_threshold_hysteresis)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record,
                        "negative_going_threshold_hysteresis",
                        &val);
      *negative_going_threshold_hysteresis = val;
    }
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv; 
}

int 
sdr_cache_get_container_entity (pstdout_state_t pstate,
                                uint8_t *sdr_record,
                                unsigned int sdr_record_len,
                                uint8_t *container_entity_id,
                                uint8_t *container_entity_instance)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_ENTITY_ASSOCIATION_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  if (container_entity_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "container_entity_id", &val);
      *container_entity_id = val;
    }

  if (container_entity_instance)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "container_entity_instance", &val);
      *container_entity_instance = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_general_device_locator_parameters (pstdout_state_t pstate,
                                                 uint8_t *sdr_record,
                                                 unsigned int sdr_record_len,
                                                 uint8_t *direct_access_address,
                                                 uint8_t *channel_number,
                                                 uint8_t *device_slave_address,
                                                 uint8_t *private_bus_id,
                                                 uint8_t *lun_for_master_write_read_command,
                                                 uint8_t *address_span)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val1, val2;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (direct_access_address)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "direct_access_address", &val);
      *direct_access_address = val;
    }
  if (channel_number)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "channel_number_ls", &val1);
      _SDR_FIID_OBJ_GET(obj_sdr_record, "channel_number_ms", &val2);
      *channel_number = (val1 << 3) | val2;
    }
  if (device_slave_address)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "device_slave_address", &val);
      *device_slave_address = val;
    }
  if (private_bus_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "private_bus_id", &val);
      *private_bus_id = val;
    }
  if (lun_for_master_write_read_command)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "lun_for_master_write_read_command", &val);
      *lun_for_master_write_read_command = val;
    }
  if (address_span)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "address_span", &val);
      *address_span = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_fru_device_locator_parameters (pstdout_state_t pstate,
                                             uint8_t *sdr_record,
                                             unsigned int sdr_record_len,
                                             uint8_t *direct_access_address,
                                             uint8_t *logical_fru_device_device_slave_address,
                                             uint8_t *private_bus_id,
                                             uint8_t *lun_for_master_write_read_fru_command,
                                             uint8_t *logical_physical_fru_device,
                                             uint8_t *channel_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;
  
  assert(sdr_record);
  assert(sdr_record_len);
  
  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;
  
  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (direct_access_address)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "direct_access_address", &val);
      *direct_access_address = val;
    }
  if (logical_fru_device_device_slave_address)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "logical_fru_device_device_slave_address", &val);
      *logical_fru_device_device_slave_address = val;
    }
  if (private_bus_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "private_bus_id", &val);
      *private_bus_id = val;
    }
  if (lun_for_master_write_read_fru_command)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "lun_for_master_write_read_fru_command", &val);
      *lun_for_master_write_read_fru_command = val;
    }
  if (logical_physical_fru_device)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "logical_physical_fru_device", &val);
      *logical_physical_fru_device = val;
    }
  if (channel_number)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "channel_number", &val);
      *channel_number = val;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_management_controller_device_locator_parameters (pstdout_state_t pstate,
                                                               uint8_t *sdr_record,
                                                               unsigned int sdr_record_len,
                                                               uint8_t *device_slave_address,
                                                               uint8_t *channel_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;
  
  assert(sdr_record);
  assert(sdr_record_len);
  
  acceptable_record_types = IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD;
  
  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (device_slave_address)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "device_slave_address", &val);
      *device_slave_address = val;
    }
  if (channel_number)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "channel_number", &val);
      *channel_number = val;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_device_type (pstdout_state_t pstate,
                           uint8_t *sdr_record,
                           unsigned int sdr_record_len,
                           uint8_t *device_type,
                           uint8_t *device_type_modifier)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (device_type)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "device_type", &val);
      *device_type = val;
    }
  if (device_type_modifier)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "device_type_modifier", &val);
      *device_type_modifier = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_entity_id_and_instance (pstdout_state_t pstate,
                                      uint8_t *sdr_record,
                                      unsigned int sdr_record_len,
                                      uint8_t *entity_id,
                                      uint8_t *entity_instance)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (entity_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "entity_id", &val);
      *entity_id = val;
    }
  if (entity_instance)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "entity_instance", &val);
      *entity_instance = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_fru_entity_id_and_instance (pstdout_state_t pstate,
                                          uint8_t *sdr_record,
                                          unsigned int sdr_record_len,
                                          uint8_t *fru_entity_id,
                                          uint8_t *fru_entity_instance)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (fru_entity_id)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "fru_entity_id", &val);
      *fru_entity_id = val;
    }
  if (fru_entity_instance)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record, "fru_entity_instance", &val);
      *fru_entity_instance = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_manufacturer_id (pstdout_state_t pstate,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               uint32_t *manufacturer_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(manufacturer_id);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_OEM_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  _SDR_FIID_OBJ_GET(obj_sdr_record, "manufacturer_id", &val);
  *manufacturer_id = val;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_product_id (pstdout_state_t pstate,
                          uint8_t *sdr_record,
                          unsigned int sdr_record_len,
                          uint16_t *product_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(product_id);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  _SDR_FIID_OBJ_GET(obj_sdr_record, "product_id", &val);
  *product_id = val;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int
sdr_cache_get_oem_data (pstdout_state_t pstate,
                        uint8_t *sdr_record,
                        unsigned int sdr_record_len,
                        uint8_t *oem_data,
                        unsigned int *oem_data_len)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int32_t len;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);
  assert(!oem_data || oem_data_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_OEM_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;

  _SDR_FIID_OBJ_GET_DATA_LEN(len,
                             obj_sdr_record,
                             "oem_data", 
                             oem_data, 
                             *oem_data_len);
  *oem_data_len = len;

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

int 
sdr_cache_get_assertion_supported (pstdout_state_t pstate,
                                   uint8_t *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint8_t *event_state_0,
                                   uint8_t *event_state_1,
                                   uint8_t *event_state_2,
                                   uint8_t *event_state_3,
                                   uint8_t *event_state_4,
                                   uint8_t *event_state_5,
                                   uint8_t *event_state_6,
                                   uint8_t *event_state_7,
                                   uint8_t *event_state_8,
                                   uint8_t *event_state_9,
                                   uint8_t *event_state_10,
                                   uint8_t *event_state_11,
                                   uint8_t *event_state_12,
                                   uint8_t *event_state_13,
                                   uint8_t *event_state_14)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_discrete = NULL;
  uint32_t acceptable_record_types;
  uint8_t record_type;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  
  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (sdr_cache_get_event_reading_type_code (pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code)
      && !IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC(event_reading_type_code))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid event reading type code passed in: %X\n",
                      event_reading_type_code);
      goto cleanup;
    }

  /* convert obj_sdr_record to appropriate format we care about */
  if (sdr_cache_get_record_id_and_type (pstate,
                                        sdr_record,
                                        sdr_record_len,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    _SDR_FIID_OBJ_COPY(obj_sdr_record_discrete,
                       obj_sdr_record,
                       tmpl_sdr_full_sensor_record_non_threshold_based_sensors);
  else
    _SDR_FIID_OBJ_COPY(obj_sdr_record_discrete,
                       obj_sdr_record,
                       tmpl_sdr_compact_sensor_record_non_threshold_based_sensors);

  if (event_state_0)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_0",
                        &val);
      *event_state_0 = val;
    }

  if (event_state_1)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_1",
                        &val);
      *event_state_1 = val;
    }

  if (event_state_2)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_2",
                        &val);
      *event_state_2 = val;
    }

  if (event_state_3)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_3",
                        &val);
      *event_state_3 = val;
    }

  if (event_state_4)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_4",
                        &val);
      *event_state_4 = val;
    }

  if (event_state_5)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_5",
                        &val);
      *event_state_5 = val;
    }

  if (event_state_6)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_6",
                        &val);
      *event_state_6 = val;
    }

  if (event_state_7)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_7",
                        &val);
      *event_state_7 = val;
    }

  if (event_state_8)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_8",
                        &val);
      *event_state_8 = val;
    }

  if (event_state_9)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_9",
                        &val);
      *event_state_9 = val;
    }

  if (event_state_10)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_10",
                        &val);
      *event_state_10 = val;
    }

  if (event_state_11)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_11",
                        &val);
      *event_state_11 = val;
    }

  if (event_state_12)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_12",
                        &val);
      *event_state_12 = val;
    }

  if (event_state_13)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_13",
                        &val);
      *event_state_13 = val;
    }

  if (event_state_14)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_14",
                        &val);
      *event_state_14 = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  _FIID_OBJ_DESTROY(obj_sdr_record_discrete);
  return rv; 
}

int 
sdr_cache_get_deassertion_supported (pstdout_state_t pstate,
                                     uint8_t *sdr_record,
                                     unsigned int sdr_record_len,
                                     uint8_t *event_state_0,
                                     uint8_t *event_state_1,
                                     uint8_t *event_state_2,
                                     uint8_t *event_state_3,
                                     uint8_t *event_state_4,
                                     uint8_t *event_state_5,
                                     uint8_t *event_state_6,
                                     uint8_t *event_state_7,
                                     uint8_t *event_state_8,
                                     uint8_t *event_state_9,
                                     uint8_t *event_state_10,
                                     uint8_t *event_state_11,
                                     uint8_t *event_state_12,
                                     uint8_t *event_state_13,
                                     uint8_t *event_state_14)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_discrete = NULL;
  uint32_t acceptable_record_types;
  uint8_t record_type;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  acceptable_record_types |= IPMI_SDR_RECORD_TYPE_COMPACT_RECORD;
  
  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  if (sdr_cache_get_event_reading_type_code (pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(event_reading_type_code)
      && !IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC(event_reading_type_code))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid event reading type code passed in: %X\n",
                      event_reading_type_code);
      goto cleanup;
    }

  /* convert obj_sdr_record to appropriate format we care about */
  if (sdr_cache_get_record_id_and_type (pstate,
                                        sdr_record,
                                        sdr_record_len,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    _SDR_FIID_OBJ_COPY(obj_sdr_record_discrete,
                       obj_sdr_record,
                       tmpl_sdr_full_sensor_record_non_threshold_based_sensors);
  else
    _SDR_FIID_OBJ_COPY(obj_sdr_record_discrete,
                       obj_sdr_record,
                       tmpl_sdr_compact_sensor_record_non_threshold_based_sensors);

  if (event_state_0)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_0",
                        &val);
      *event_state_0 = val;
    }

  if (event_state_1)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_1",
                        &val);
      *event_state_1 = val;
    }

  if (event_state_2)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_2",
                        &val);
      *event_state_2 = val;
    }

  if (event_state_3)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_3",
                        &val);
      *event_state_3 = val;
    }

  if (event_state_4)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_4",
                        &val);
      *event_state_4 = val;
    }

  if (event_state_5)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_5",
                        &val);
      *event_state_5 = val;
    }

  if (event_state_6)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_6",
                        &val);
      *event_state_6 = val;
    }

  if (event_state_7)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_7",
                        &val);
      *event_state_7 = val;
    }

  if (event_state_8)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_8",
                        &val);
      *event_state_8 = val;
    }

  if (event_state_9)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_9",
                        &val);
      *event_state_9 = val;
    }

  if (event_state_10)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_10",
                        &val);
      *event_state_10 = val;
    }

  if (event_state_11)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_11",
                        &val);
      *event_state_11 = val;
    }

  if (event_state_12)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_12",
                        &val);
      *event_state_12 = val;
    }

  if (event_state_13)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_13",
                        &val);
      *event_state_13 = val;
    }

  if (event_state_14)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_14",
                        &val);
      *event_state_14 = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  _FIID_OBJ_DESTROY(obj_sdr_record_discrete);
  return rv; 
}

int 
sdr_cache_get_threshold_assertion_supported (pstdout_state_t pstate,
                                             uint8_t *sdr_record,
                                             unsigned int sdr_record_len,
                                             uint8_t *lower_non_critical_going_low,
                                             uint8_t *lower_non_critical_going_high,
                                             uint8_t *lower_critical_going_low,
                                             uint8_t *lower_critical_going_high,
                                             uint8_t *lower_non_recoverable_going_low,
                                             uint8_t *lower_non_recoverable_going_high,
                                             uint8_t *upper_non_critical_going_low,
                                             uint8_t *upper_non_critical_going_high,
                                             uint8_t *upper_critical_going_low,
                                             uint8_t *upper_critical_going_high,
                                             uint8_t *upper_non_recoverable_going_low,
                                             uint8_t *upper_non_recoverable_going_high)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (i.e. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (sdr_cache_get_event_reading_type_code (pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid event reading type code passed in: %X\n",
                      event_reading_type_code);
      goto cleanup;
    }

  _SDR_FIID_OBJ_COPY(obj_sdr_record_threshold,
                     obj_sdr_record,
                     tmpl_sdr_full_sensor_record_threshold_based_sensors);

  if (lower_non_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_critical_going_low_supported",
                        &val);
      *lower_non_critical_going_low = val;
    }

  if (lower_non_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_critical_going_high_supported",
                        &val);
      *lower_non_critical_going_high = val;
    }

  if (lower_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_critical_going_low_supported",
                        &val);
      *lower_critical_going_low = val;
    }

  if (lower_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_critical_going_high_supported",
                        &val);
      *lower_critical_going_high = val;
    }

  if (lower_non_recoverable_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_recoverable_going_low_supported",
                        &val);
      *lower_non_recoverable_going_low = val;
    }

  if (lower_non_recoverable_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_recoverable_going_high_supported",
                        &val);
      *lower_non_recoverable_going_high = val;
    }

  if (upper_non_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_critical_going_low_supported",
                        &val);
      *upper_non_critical_going_low = val;
    }

  if (upper_non_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_critical_going_high_supported",
                        &val);
      *upper_non_critical_going_high = val;
    }

  if (upper_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_critical_going_low_supported",
                        &val);
      *upper_critical_going_low = val;
    }

  if (upper_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_critical_going_high_supported",
                        &val);
      *upper_critical_going_high = val;
    }

  if (upper_non_recoverable_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_recoverable_going_low_supported",
                        &val);
      *upper_non_recoverable_going_low = val;
    }

  if (upper_non_recoverable_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_recoverable_going_high_supported",
                        &val);
      *upper_non_recoverable_going_high = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  _FIID_OBJ_DESTROY(obj_sdr_record_threshold);
  return rv; 
}

int 
sdr_cache_get_threshold_deassertion_supported (pstdout_state_t pstate,
                                               uint8_t *sdr_record,
                                               unsigned int sdr_record_len,
                                               uint8_t *lower_non_critical_going_low,
                                               uint8_t *lower_non_critical_going_high,
                                               uint8_t *lower_critical_going_low,
                                               uint8_t *lower_critical_going_high,
                                               uint8_t *lower_non_recoverable_going_low,
                                               uint8_t *lower_non_recoverable_going_high,
                                               uint8_t *upper_non_critical_going_low,
                                               uint8_t *upper_non_critical_going_high,
                                               uint8_t *upper_critical_going_low,
                                               uint8_t *upper_critical_going_high,
                                               uint8_t *upper_non_recoverable_going_low,
                                               uint8_t *upper_non_recoverable_going_high)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (i.e. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (sdr_cache_get_event_reading_type_code (pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid event reading type code passed in: %X\n",
                      event_reading_type_code);
      goto cleanup;
    }

  _SDR_FIID_OBJ_COPY(obj_sdr_record_threshold,
                     obj_sdr_record,
                     tmpl_sdr_full_sensor_record_threshold_based_sensors);

  if (lower_non_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_critical_going_low_supported",
                        &val);
      *lower_non_critical_going_low = val;
    }

  if (lower_non_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_critical_going_high_supported",
                        &val);
      *lower_non_critical_going_high = val;
    }

  if (lower_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_critical_going_low_supported",
                        &val);
      *lower_critical_going_low = val;
    }

  if (lower_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_critical_going_high_supported",
                        &val);
      *lower_critical_going_high = val;
    }

  if (lower_non_recoverable_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_recoverable_going_low_supported",
                        &val);
      *lower_non_recoverable_going_low = val;
    }

  if (lower_non_recoverable_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_recoverable_going_high_supported",
                        &val);
      *lower_non_recoverable_going_high = val;
    }

  if (upper_non_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_critical_going_low_supported",
                        &val);
      *upper_non_critical_going_low = val;
    }

  if (upper_non_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_critical_going_high_supported",
                        &val);
      *upper_non_critical_going_high = val;
    }

  if (upper_critical_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_critical_going_low_supported",
                        &val);
      *upper_critical_going_low = val;
    }

  if (upper_critical_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_critical_going_high_supported",
                        &val);
      *upper_critical_going_high = val;
    }

  if (upper_non_recoverable_going_low)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_recoverable_going_low_supported",
                        &val);
      *upper_non_recoverable_going_low = val;
    }

  if (upper_non_recoverable_going_high)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_recoverable_going_high_supported",
                        &val);
      *upper_non_recoverable_going_high = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  _FIID_OBJ_DESTROY(obj_sdr_record_threshold);
  return rv; 
}

int 
sdr_cache_get_threshold_readable (pstdout_state_t pstate,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len,
                                  uint8_t *lower_non_critical_threshold,
                                  uint8_t *lower_critical_threshold,
                                  uint8_t *lower_non_recoverable_threshold,
                                  uint8_t *upper_non_critical_threshold,
                                  uint8_t *upper_critical_threshold,
                                  uint8_t *upper_non_recoverable_threshold)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (i.e. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (sdr_cache_get_event_reading_type_code (pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid event reading type code passed in: %X\n",
                      event_reading_type_code);
      goto cleanup;
    }

  _SDR_FIID_OBJ_COPY(obj_sdr_record_threshold,
                     obj_sdr_record,
                     tmpl_sdr_full_sensor_record_threshold_based_sensors);

  if (lower_non_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "readable_threshold_mask.lower_non_critical_threshold_is_readable",
                        &val);
      *lower_non_critical_threshold = val;
    }

  if (lower_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "readable_threshold_mask.lower_critical_threshold_is_readable",
                        &val);
      *lower_critical_threshold = val;
    }

  if (lower_non_recoverable_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "readable_threshold_mask.lower_non_recoverable_threshold_is_readable",
                        &val);
      *lower_non_recoverable_threshold = val;
    }

  if (upper_non_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "readable_threshold_mask.upper_non_critical_threshold_is_readable",
                        &val);
      *upper_non_critical_threshold = val;
    }

  if (upper_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "readable_threshold_mask.upper_critical_threshold_is_readable",
                        &val);
      *upper_critical_threshold = val;
    }

  if (upper_non_recoverable_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "readable_threshold_mask.upper_non_recoverable_threshold_is_readable",
                        &val);
      *upper_non_recoverable_threshold = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  _FIID_OBJ_DESTROY(obj_sdr_record_threshold);
  return rv; 
}

int 
sdr_cache_get_threshold_settable (pstdout_state_t pstate,
                                  uint8_t *sdr_record,
                                  unsigned int sdr_record_len,
                                  uint8_t *lower_non_critical_threshold,
                                  uint8_t *lower_critical_threshold,
                                  uint8_t *lower_non_recoverable_threshold,
                                  uint8_t *upper_non_critical_threshold,
                                  uint8_t *upper_critical_threshold,
                                  uint8_t *upper_non_recoverable_threshold)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (i.e. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;

  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (sdr_cache_get_event_reading_type_code (pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid event reading type code passed in: %X\n",
                      event_reading_type_code);
      goto cleanup;
    }

  _SDR_FIID_OBJ_COPY(obj_sdr_record_threshold,
                     obj_sdr_record,
                     tmpl_sdr_full_sensor_record_threshold_based_sensors);

  if (lower_non_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "settable_threshold_mask.lower_non_critical_threshold_is_settable",
                        &val);
      *lower_non_critical_threshold = val;
    }

  if (lower_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "settable_threshold_mask.lower_critical_threshold_is_settable",
                        &val);
      *lower_critical_threshold = val;
    }

  if (lower_non_recoverable_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "settable_threshold_mask.lower_non_recoverable_threshold_is_settable",
                        &val);
      *lower_non_recoverable_threshold = val;
    }

  if (upper_non_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "settable_threshold_mask.upper_non_critical_threshold_is_settable",
                        &val);
      *upper_non_critical_threshold = val;
    }

  if (upper_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "settable_threshold_mask.upper_critical_threshold_is_settable",
                        &val);
      *upper_critical_threshold = val;
    }

  if (upper_non_recoverable_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "settable_threshold_mask.upper_non_recoverable_threshold_is_settable",
                        &val);
      *upper_non_recoverable_threshold = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  _FIID_OBJ_DESTROY(obj_sdr_record_threshold);
  return rv; 
}

int 
sdr_cache_get_thresholds_raw (pstdout_state_t pstate,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len,
                              uint8_t *lower_non_critical_threshold,
                              uint8_t *lower_critical_threshold,
                              uint8_t *lower_non_recoverable_threshold,
                              uint8_t *upper_non_critical_threshold,
                              uint8_t *upper_critical_threshold,
                              uint8_t *upper_non_recoverable_threshold)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  assert(sdr_record);
  assert(sdr_record_len);

  acceptable_record_types = IPMI_SDR_RECORD_TYPE_FULL_RECORD;
  
  if (!(obj_sdr_record = _sdr_cache_get_common(pstate,
                                               sdr_record,
                                               sdr_record_len,
                                               acceptable_record_types)))
    goto cleanup;
  
  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (sdr_cache_get_event_reading_type_code (pstate,
                                             sdr_record,
                                             sdr_record_len,
                                             &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(event_reading_type_code))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "Invalid event reading type code passed in: %X\n",
                      event_reading_type_code);
      goto cleanup;
    }
  
  _SDR_FIID_OBJ_COPY(obj_sdr_record_threshold,
                     obj_sdr_record,
                     tmpl_sdr_full_sensor_record_threshold_based_sensors);
  
  if (lower_non_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "lower_non_critical_threshold",
                        &val);
      *lower_non_critical_threshold = val;
    }

  if (lower_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "lower_critical_threshold",
                        &val);
      *lower_critical_threshold = val;
    }

  if (lower_non_recoverable_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "lower_non_recoverable_threshold",
                        &val);
      *lower_non_recoverable_threshold = val;
    }

  if (upper_non_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "upper_non_critical_threshold",
                        &val);
      *upper_non_critical_threshold = val;
    }

  if (upper_critical_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "upper_critical_threshold",
                        &val);
      *upper_critical_threshold = val;
    }

  if (upper_non_recoverable_threshold)
    {
      _SDR_FIID_OBJ_GET(obj_sdr_record_threshold,
                        "upper_non_recoverable_threshold",
                        &val);
      *upper_non_recoverable_threshold = val;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(obj_sdr_record);
  _FIID_OBJ_DESTROY(obj_sdr_record_threshold);
  return rv; 
}
