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

#include "freeipmi/ipmi-sdr-repository-cmds.h"
#include "freeipmi/ipmi-sensor-cmds.h"
#include "freeipmi/api/ipmi-sdr-repository-cmds-api.h"
#include "freeipmi/api/ipmi-sensor-cmds-api.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/util/ipmi-sensor-util.h"

#include "freeipmi-portability.h"
#include "string-utils.h"

#include "ipmi-sdr-cache.h"
#include "ipmi-sdr-cache-defs.h"
#include "ipmi-sdr-cache-writes.h"
#include "ipmi-sdr-cache-reads.h"

static char * sdr_cache_ctx_errmsg[] =
  {
    "success",
    "sdr context is null",
    "sdr context is invalid",
    "invalid parameter",
    "sdr cache directory cannot be determined",
    "sdr cache directory cannot be accessed",
    "sdr cache directory cannot be created",
    "hostname invalid",
    "cache does not exist",
    "cache already exists",
    "cache invalid",
    "cache empty",
    "cache out of date",
    "cache permission denied",
    "out of memory",
    "ipmi communication error",
    "internal error",
    "error number out of range",
    NULL,
  };

#define GETHOSTBYNAME_AUX_BUFLEN 1024

sdr_cache_ctx_t
sdr_cache_ctx_create(void)
{
  sdr_cache_ctx_t ctx = NULL;

  if (!(ctx = (sdr_cache_ctx_t)malloc(sizeof(struct sdr_cache_ctx))))
    goto cleanup;

  ctx->magic = SDR_CACHE_CTX_MAGIC;

  ctx->errnum = SDR_CACHE_CTX_ERR_SUCCESS;
  return ctx;

 cleanup:
  if (ctx)
    free(ctx);
  return (NULL);

}

void
sdr_cache_ctx_destroy(sdr_cache_ctx_t ctx)
{
  if (!(ctx && ctx->magic == SDR_CACHE_CTX_MAGIC))
    return;

  ctx->magic = ~SDR_CACHE_CTX_MAGIC;
  ctx->errnum = SDR_CACHE_CTX_ERR_SUCCESS;
  free(ctx);
}

char *
sdr_cache_ctx_strerror(int32_t errnum)
{
  if (errnum >= SDR_CACHE_CTX_ERR_SUCCESS && errnum <= SDR_CACHE_CTX_ERR_ERRNUMRANGE)
    return sdr_cache_ctx_errmsg[errnum];
  else
    return sdr_cache_ctx_errmsg[SDR_CACHE_CTX_ERR_ERRNUMRANGE];
}

int
sdr_cache_ctx_errnum(sdr_cache_ctx_t ctx)
{
  if (!ctx)
    return (SDR_CACHE_CTX_ERR_NULL);
  else if (ctx->magic != SDR_CACHE_CTX_MAGIC)
    return (SDR_CACHE_CTX_ERR_INVALID);
  else
    return (ctx->errnum);
}

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

static int
_get_decode_parameters (sdr_cache_ctx_t ctx,
                        fiid_obj_t obj,
                        uint8_t *analog_data_format, 
                        char *r_exponent, 
                        char *b_exponent, 
                        char *linear, 
                        short *b, 
                        short *m)
{
  uint64_t val;
  
  uint64_t m_ls;
  uint64_t m_ms;
  
  uint64_t b_ls;
  uint64_t b_ms;

  int rv = -1;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(analog_data_format);
  assert(r_exponent);
  assert(b_exponent);
  assert(linear);
  assert(b);
  assert(m);

  if (!fiid_obj_valid(obj))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      return -1;
    }

  _SDR_FIID_OBJ_TEMPLATE_COMPARE (obj, tmpl_sdr_full_sensor_record);

  _SDR_FIID_OBJ_GET (obj, "r_exponent", &val);
  *r_exponent = (char) val;
  if (*r_exponent & 0x08)
    *r_exponent |= 0xF0;
  
  _SDR_FIID_OBJ_GET (obj, "b_exponent", &val);
  *b_exponent = (char) val;
  if (*b_exponent & 0x08)
    *b_exponent |= 0xF0;
  
  _SDR_FIID_OBJ_GET (obj, "m_ls", &m_ls);
  _SDR_FIID_OBJ_GET (obj, "m_ms", &m_ms);
  *m = (short)m_ls;
  *m |= ((m_ms & 0x3) << 8);
  if (*m & 0x200)
    *m |= 0xFE00;
  
  _SDR_FIID_OBJ_GET (obj, "b_ls", &b_ls);
  _SDR_FIID_OBJ_GET (obj, "b_ms", &b_ms);
  *b = (short)b_ls;
  *b |= ((b_ms & 0x3) << 8);
  if (*b & 0x200)
    *b |= 0xFE00;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_unit1.analog_data_format", &val);
  *analog_data_format = (uint8_t) val;

  _SDR_FIID_OBJ_GET (obj, "linearization", &val);
  *linear = (char)val;

  rv = 0;
 cleanup:
  return rv;
}

static int
_get_sdr_full_record (sdr_cache_ctx_t ctx,
                      uint8_t *sdr_record_data, 
                      uint32_t sdr_record_data_len,
                      sdr_full_record_t *sdr_full_record,
                      int debug)
{
  uint64_t val;
  
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  uint8_t linear;
  uint8_t analog_data_format;  
  fiid_obj_t obj = NULL;
  int rv = -1;

  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_full_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_full_sensor_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj,sdr_record_data, sdr_record_data_len);

#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR FULL RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  if (_get_decode_parameters (ctx,
                              obj,
                              &analog_data_format,
                              &r_exponent,
                              &b_exponent, 
                              (char *)&linear, 
                              &b, 
                              &m) < 0)
    goto cleanup;

  sdr_full_record->b = b;
  sdr_full_record->m = m;
  sdr_full_record->r_exponent = r_exponent;
  sdr_full_record->b_exponent = b_exponent;
  sdr_full_record->linear = linear;
  sdr_full_record->analog_data_format = analog_data_format;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_owner_id", &val);
  sdr_full_record->sensor_owner_id = val;

  _SDR_FIID_OBJ_GET (obj, "sensor_number", &val);
  sdr_full_record->sensor_number = val;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_type", &val);
  sdr_full_record->sensor_type = val;
  
  _SDR_FIID_OBJ_GET (obj, "event_reading_type_code", &val);
  sdr_full_record->event_reading_type_code = val;

  _SDR_FIID_OBJ_GET (obj, "sensor_unit2.base_unit", &val);
  sdr_full_record->sensor_unit = val;
  
  _SDR_FIID_OBJ_GET (obj, "nominal_reading", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->nominal_reading)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "normal_minimum", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->normal_minimum)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "normal_maximum", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->normal_maximum)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "sensor_minimum_reading", &val);
  
  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->sensor_minimum_reading)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "sensor_maximum_reading", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->sensor_maximum_reading)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "negative_going_threshold_hysteresis", &val);
  sdr_full_record->negative_going_threshold_hysteresis = val;
  
  _SDR_FIID_OBJ_GET (obj, "positive_going_threshold_hysteresis", &val);
  sdr_full_record->positive_going_threshold_hysteresis = val;
  
  _SDR_FIID_OBJ_GET (obj, "lower_non_recoverable_threshold", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->lower_non_recoverable_threshold)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "upper_non_recoverable_threshold", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->upper_non_recoverable_threshold)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "lower_critical_threshold", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->lower_critical_threshold)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "upper_critical_threshold", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->upper_critical_threshold)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "lower_non_critical_threshold", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->lower_non_critical_threshold)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  _SDR_FIID_OBJ_GET (obj, "upper_non_critical_threshold", &val);

  if (analog_data_format != IPMI_SDR_ANALOG_DATA_FORMAT_NOT_ANALOG)
    {
      if (ipmi_sensor_decode_value (r_exponent,
                                    b_exponent,
                                    m,
                                    b,
                                    linear,
                                    analog_data_format,
                                    val,
                                    &(sdr_full_record->upper_non_critical_threshold)) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }

  memset(sdr_full_record->sensor_name, '\0', IPMI_SENSOR_NAME_MAX+1);
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj,
                          "id_string",
                          (uint8_t *)sdr_full_record->sensor_name,
                          IPMI_SENSOR_NAME_MAX);

  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int 
_get_sdr_compact_record (sdr_cache_ctx_t ctx,
                         uint8_t *sdr_record_data, 
                         uint32_t sdr_record_data_len,
                         sdr_compact_record_t *sdr_compact_record,
                         int debug)
{
  uint64_t val;
  
  fiid_obj_t obj = NULL;

  int rv = -1;

  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_compact_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_compact_sensor_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj, sdr_record_data, sdr_record_data_len);

#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR COMPACT RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  _SDR_FIID_OBJ_GET (obj, "sensor_owner_id", &val);
  sdr_compact_record->sensor_owner_id = val;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_number", &val);
  sdr_compact_record->sensor_number = val;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_type", &val);
  sdr_compact_record->sensor_type = val;
  
  _SDR_FIID_OBJ_GET (obj, "event_reading_type_code", &val);
  sdr_compact_record->event_reading_type_code = val;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_unit2.base_unit", &val);
  sdr_compact_record->sensor_unit = val;
  
  _SDR_FIID_OBJ_GET (obj, "negative_going_threshold_hysteresis", &val);
  sdr_compact_record->negative_going_threshold_hysteresis = val;
  
  _SDR_FIID_OBJ_GET (obj, "positive_going_threshold_hysteresis", &val);
  sdr_compact_record->positive_going_threshold_hysteresis = val;
  
  memset(sdr_compact_record->sensor_name, '\0', IPMI_SENSOR_NAME_MAX+1);
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj,
                          "id_string",
                          (uint8_t *)sdr_compact_record->sensor_name,
                          IPMI_SENSOR_NAME_MAX);

  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int 
_get_sdr_event_only_record (sdr_cache_ctx_t ctx,
                            uint8_t *sdr_record_data, 
                            uint32_t sdr_record_data_len,
                            sdr_event_only_record_t *sdr_event_only_record,
                            int debug)
{
  uint64_t val;  
  fiid_obj_t obj = NULL;
  int rv = -1;
  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_event_only_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_event_only_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj, sdr_record_data, sdr_record_data_len);
  
#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR EVENT ONLY RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  _SDR_FIID_OBJ_GET (obj, "sensor_owner_id", &val);
  sdr_event_only_record->sensor_owner_id = val;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_number", &val);
  sdr_event_only_record->sensor_number = val;
  
  _SDR_FIID_OBJ_GET (obj, "sensor_type", &val);
  sdr_event_only_record->sensor_type = val;
  
  _SDR_FIID_OBJ_GET (obj, "event_reading_type_code", &val);
  sdr_event_only_record->event_reading_type_code = val;
  
  memset(sdr_event_only_record->sensor_name, '\0', IPMI_SENSOR_NAME_MAX+1);
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj,
                          "id_string",
                          (uint8_t *)sdr_event_only_record->sensor_name,
                          IPMI_SENSOR_NAME_MAX);
  
  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int 
_get_sdr_entity_association_record (sdr_cache_ctx_t ctx,
                                    uint8_t *sdr_record_data, 
                                    uint32_t sdr_record_data_len,
                                    sdr_entity_association_record_t *sdr_entity_association_record,
                                    int debug)
{
  uint64_t val;
  fiid_obj_t obj = NULL;
  int rv = -1;
  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_entity_association_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_entity_association_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj, sdr_record_data, sdr_record_data_len);

#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR ENTITY ASSOCIATION RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  _SDR_FIID_OBJ_GET (obj, "container_entity_id", &val);
  sdr_entity_association_record->container_entity_id = val;
  
  _SDR_FIID_OBJ_GET (obj, "container_entity_instance", &val);
  sdr_entity_association_record->container_entity_instance = val;
  
  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int 
_get_sdr_generic_device_locator_record (sdr_cache_ctx_t ctx,
                                        uint8_t *sdr_record_data, 
                                        uint32_t sdr_record_data_len,
                                        sdr_generic_device_locator_record_t *sdr_generic_device_locator_record,
                                        int debug)
{
  uint64_t val;
  uint64_t tmp;
  fiid_obj_t obj = NULL;
  int rv = -1;
  int32_t len;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_generic_device_locator_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_generic_device_locator_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj, sdr_record_data, sdr_record_data_len);
   
#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR GENERIC DEVICE LOCATOR RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  _SDR_FIID_OBJ_GET (obj, "direct_access_address", &val);
  sdr_generic_device_locator_record->direct_access_address = val;
  
  tmp = 0;
  _SDR_FIID_OBJ_GET (obj, "channel_number_ms", &val);
  tmp = val << 4;
  _SDR_FIID_OBJ_GET (obj, "channel_number_ls", &val);
  tmp |= val;
  sdr_generic_device_locator_record->channel_number = tmp;
  
  _SDR_FIID_OBJ_GET (obj, "device_slave_address", &val);
  sdr_generic_device_locator_record->device_slave_address = val;
  
  _SDR_FIID_OBJ_GET (obj, "private_bus_id", &val);
  sdr_generic_device_locator_record->private_bus_id = val;

  _SDR_FIID_OBJ_GET (obj, "lun_for_master_write_read_command", &val);
  sdr_generic_device_locator_record->lun_for_master_write_read_command = val;
  
  _SDR_FIID_OBJ_GET (obj, "address_span", &val);    
  sdr_generic_device_locator_record->address_span = val;
  
  _SDR_FIID_OBJ_GET (obj, "device_type", &val);
  sdr_generic_device_locator_record->device_type = val;
  
  _SDR_FIID_OBJ_GET (obj, "device_type_modifier", &val);
  sdr_generic_device_locator_record->device_type_modifier = val;
  
  _SDR_FIID_OBJ_GET (obj, "entity_id", &val);
  sdr_generic_device_locator_record->entity_id = val;
  
  _SDR_FIID_OBJ_GET (obj, "entity_instance", &val);
  sdr_generic_device_locator_record->entity_instance = val;
  
  memset(sdr_generic_device_locator_record->device_name, '\0', IPMI_DEVICE_NAME_MAX+1);
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj,
                          "device_id_string",
                          (uint8_t *)sdr_generic_device_locator_record->device_name,
                          IPMI_DEVICE_NAME_MAX);

  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int 
_get_sdr_fru_device_locator_record (sdr_cache_ctx_t ctx,
                                    uint8_t *sdr_record_data, 
				    uint32_t sdr_record_data_len,
				    sdr_fru_device_locator_record_t *sdr_fru_device_locator_record,
				    int debug)
{
  uint64_t val;
  fiid_obj_t obj = NULL;
  int rv = -1;
  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_fru_device_locator_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_fru_device_locator_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj, sdr_record_data, sdr_record_data_len);
  
#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR LOGICAL FRU DEVICE LOCATOR RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  _SDR_FIID_OBJ_GET (obj, "logical_fru_device_device_slave_address", &val);
  sdr_fru_device_locator_record->logical_fru_device_device_slave_address = val;
  
  _SDR_FIID_OBJ_GET (obj, "logical_physical_fru_device", &val);
  sdr_fru_device_locator_record->logical_physical_fru_device = val;

  _SDR_FIID_OBJ_GET (obj, "device_type", &val);
  sdr_fru_device_locator_record->device_type = val;
  
  _SDR_FIID_OBJ_GET (obj, "device_type_modifier", &val);
  sdr_fru_device_locator_record->device_type_modifier = val;
  
  _SDR_FIID_OBJ_GET (obj, "fru_entity_id", &val);
  sdr_fru_device_locator_record->fru_entity_id = val;
  
  _SDR_FIID_OBJ_GET (obj, "fru_entity_instance", &val);
  sdr_fru_device_locator_record->fru_entity_instance = val;
  
  memset(sdr_fru_device_locator_record->device_name, '\0', IPMI_DEVICE_NAME_MAX+1);
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj,
                          "device_string",
                          (uint8_t *)sdr_fru_device_locator_record->device_name,
                          IPMI_DEVICE_NAME_MAX);
 
  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int 
_get_sdr_management_controller_device_locator_record (sdr_cache_ctx_t ctx,
                                                      uint8_t *sdr_record_data, 
                                                      uint32_t sdr_record_data_len,
                                                      sdr_management_controller_device_locator_record_t *sdr_management_controller_device_locator_record,
                                                      int debug)
{
  uint64_t val;
  fiid_obj_t obj = NULL;
  int rv = -1;
  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_management_controller_device_locator_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_management_controller_device_locator_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj, sdr_record_data, sdr_record_data_len);

#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR MANAGEMENT CONTROLLER DEVICE LOCATOR RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  _SDR_FIID_OBJ_GET (obj, "entity_id", &val);
  sdr_management_controller_device_locator_record->entity_id = val;
  
  _SDR_FIID_OBJ_GET (obj, "entity_instance", &val);
  sdr_management_controller_device_locator_record->entity_instance = val;
  
  memset(sdr_management_controller_device_locator_record->device_name, '\0', IPMI_DEVICE_NAME_MAX+1);
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj,
                          "device_id_string",
                          (uint8_t *)sdr_management_controller_device_locator_record->device_name,
                          IPMI_DEVICE_NAME_MAX);
  
  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int 
_get_sdr_oem_record (sdr_cache_ctx_t ctx,
                     uint8_t *sdr_record_data, 
                     uint32_t sdr_record_data_len,
                     sdr_oem_record_t *sdr_oem_record,
                     int debug)
{
  uint64_t val;
  fiid_obj_t obj = NULL;
  int32_t len;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(sdr_record_data);
  assert(sdr_oem_record);

  _SDR_FIID_OBJ_CREATE(obj, tmpl_sdr_oem_record);

  _SDR_FIID_OBJ_SET_ALL (len, obj, sdr_record_data, sdr_record_data_len);

#ifndef NDEBUG
  if (debug)
    {
      char *hdr = 
        "================================================\n"
        "SDR OEM RECORD\n"
        "================================================";

      ipmi_obj_dump_perror(STDERR_FILENO, NULL, hdr, NULL, obj);
    }
#endif /* NDEBUG */

  _SDR_FIID_OBJ_GET (obj, "manufacturer_id", &val);
  sdr_oem_record->manufacturer_id = val;

  memset(sdr_oem_record->oem_data, '\0', IPMI_OEM_DATA_MAX+1);
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj,
                          "oem_data",
                          sdr_oem_record->oem_data,
                          IPMI_OEM_DATA_MAX);
  sdr_oem_record->oem_data_length = len;

  rv = 0;
 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return rv;
}

static int
_get_sdr_sensor_record (sdr_cache_ctx_t ctx,
                        ipmi_ctx_t ipmi_ctx, 
                        uint16_t record_id, 
                        fiid_obj_t obj_cmd_rs, 
                        uint8_t *sensor_record,
                        uint32_t *sensor_record_len)
{
  uint64_t val = 0;
  
  uint8_t record_length = 0;
  uint16_t reservation_id = 0;
  uint8_t offset_into_record = 0;
  uint8_t bytes_to_read = 0; 
  uint8_t chunk_data[16];
  uint8_t *record_data = NULL;
  int8_t rv = -1;

  fiid_obj_t record_header = NULL;
  fiid_obj_t obj_reserve_sdr_rs = NULL;
  int32_t record_header_len;
  uint8_t *record_header_buf = NULL;

  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(ipmi_ctx);
  assert(sensor_record);
  assert(sensor_record_len);

  if (!fiid_obj_valid(obj_cmd_rs))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      goto cleanup;
    }

  _SDR_FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sdr_rs);

  _SDR_FIID_OBJ_CREATE(record_header, tmpl_sdr_record_header);
    
  _SDR_FIID_TEMPLATE_LEN_BYTES (record_header_len, 
                                tmpl_sdr_record_header);

  if (!(record_header_buf = alloca (record_header_len)))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  memset (record_header_buf, 0, record_header_len);

  _SDR_FIID_OBJ_CREATE(obj_reserve_sdr_rs, tmpl_cmd_reserve_sdr_repository_rs);

  if (ipmi_cmd_reserve_sdr_repository (ipmi_ctx, obj_reserve_sdr_rs) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_IPMI_COMMUNICATION;
      goto cleanup;
    }
  
  _SDR_FIID_OBJ_GET (obj_reserve_sdr_rs, "reservation_id", &val);
  reservation_id = (uint16_t) val;
  
  if (ipmi_cmd_get_sdr (ipmi_ctx, 
                        reservation_id,
                        record_id, 
                        0, 
                        record_header_len, 
                        obj_cmd_rs) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_IPMI_COMMUNICATION;
      goto cleanup;
    }
  
  _SDR_FIID_OBJ_GET_DATA (len,
                          obj_cmd_rs,
                          "record_data",
                          record_header_buf,
                          record_header_len);
  
  _SDR_FIID_OBJ_SET_ALL (len,
                         record_header, 
                         record_header_buf, 
                         record_header_len);

  _SDR_FIID_OBJ_GET (record_header, "record_length", &val);
  record_length = val;
  record_length += record_header_len;
  
  _SDR_FIID_OBJ_CLEAR(obj_reserve_sdr_rs);

  if (ipmi_cmd_reserve_sdr_repository (ipmi_ctx, obj_reserve_sdr_rs) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_IPMI_COMMUNICATION;
      goto cleanup;
    }
  
  _SDR_FIID_OBJ_GET (obj_reserve_sdr_rs, "reservation_id", &val);
  reservation_id = (uint16_t) val;
  
  if (!(record_data = alloca (record_length)))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  memset (record_data, 0, record_length);
  
  for (offset_into_record = 0; offset_into_record < record_length; offset_into_record += 16)
    {
      bytes_to_read = 16;
      if ((offset_into_record + bytes_to_read) > record_length)
	bytes_to_read = record_length - offset_into_record;
      
      _SDR_FIID_OBJ_CLEAR (obj_cmd_rs);
      
      if (ipmi_cmd_get_sdr (ipmi_ctx, 
                            reservation_id, 
                            record_id, 
                            offset_into_record, 
                            bytes_to_read, 
                            obj_cmd_rs) < 0)
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_IPMI_COMMUNICATION;
          goto cleanup;
        }
      
      _SDR_FIID_OBJ_GET_DATA (len, obj_cmd_rs, "record_data", chunk_data, 16);

      memcpy (record_data + offset_into_record, chunk_data, bytes_to_read);
    }

  if (*sensor_record_len < record_length)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      goto cleanup;
    }
  
  memcpy(sensor_record, record_data, record_length);
  *sensor_record_len = record_length;
  
  rv = 0;
 cleanup:
  if (record_header)
    fiid_obj_destroy(record_header);
  if (obj_reserve_sdr_rs)
    fiid_obj_destroy(obj_reserve_sdr_rs);
  return (rv);
}

static int
_get_sdr_record (sdr_cache_ctx_t ctx,
                 ipmi_ctx_t ipmi_ctx, 
                 uint16_t record_id, 
                 uint16_t *next_record_id, 
                 sdr_record_t *sdr_record,
                 int debug)
{
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t sensor_record[1024];
  uint32_t sensor_record_len;
  uint64_t val = 0;
  int rv = -1;
  int32_t len;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(ipmi_ctx);

  _SDR_FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sdr_rs);
  _SDR_FIID_OBJ_CREATE (obj_sdr_record, tmpl_sdr_record_header);

  sensor_record_len = 1024;
  if (_get_sdr_sensor_record (ctx,
                              ipmi_ctx, 
                              record_id, 
                              obj_cmd_rs, 
                              sensor_record,
                              &sensor_record_len) < 0)
    goto cleanup;
  
  memset (sdr_record, 0, sizeof (sdr_record_t));
  
  _SDR_FIID_OBJ_GET (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  _SDR_FIID_OBJ_SET_ALL (len, obj_sdr_record, sensor_record, sensor_record_len);
  
  _SDR_FIID_OBJ_GET (obj_sdr_record, "record_id", &val);
  sdr_record->record_id = val;
  
  _SDR_FIID_OBJ_GET (obj_sdr_record, "record_type", &val);
  sdr_record->record_type = val;

  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      if (_get_sdr_full_record (ctx,
                                sensor_record,
                                sensor_record_len,
                                &(sdr_record->record.sdr_full_record),
                                debug) < 0)
        goto cleanup;

      /* 0x01 == THRESHOLD sensor class */
      if (sdr_record->record.sdr_full_record.event_reading_type_code != 0x01)
        break;
      
      _SDR_FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sensor_thresholds_rs);

      if (ipmi_cmd_get_sensor_thresholds (ipmi_ctx, 
					  sdr_record->record.sdr_full_record.sensor_number, 
					  obj_cmd_rs) != 0)
        /* This is ok - no biggie if we can't get thresholds*/
        break;
      
      _SDR_FIID_OBJ_GET (obj_cmd_rs, 
                         "readable_thresholds.lower_critical_threshold", 
                         &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_critical_threshold = val;
      _SDR_FIID_OBJ_GET (obj_cmd_rs, 
                         "readable_thresholds.upper_critical_threshold", 
                         &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_critical_threshold = val;
      _SDR_FIID_OBJ_GET (obj_cmd_rs, 
                         "readable_thresholds.lower_non_critical_threshold", 
                         &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_non_critical_threshold = val;
      _SDR_FIID_OBJ_GET (obj_cmd_rs, 
                         "readable_thresholds.upper_non_critical_threshold", 
                         &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_non_critical_threshold = val;
      _SDR_FIID_OBJ_GET (obj_cmd_rs, 
                         "readable_thresholds.lower_non_recoverable_threshold", 
                         &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_non_recoverable_threshold = val;
      _SDR_FIID_OBJ_GET (obj_cmd_rs, 
                         "readable_thresholds.upper_non_recoverable_threshold", 
                         &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_non_recoverable_threshold = val;
      
      break;
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      if (_get_sdr_compact_record (ctx,
                                   sensor_record,
                                   sensor_record_len,
                                   &(sdr_record->record.sdr_compact_record),
                                   debug) < 0)
        goto cleanup;
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      if (_get_sdr_event_only_record (ctx,
                                      sensor_record,
                                      sensor_record_len,
                                      &(sdr_record->record.sdr_event_only_record),
                                      debug) < 0)
        goto cleanup;
      break;
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      if (_get_sdr_entity_association_record (ctx,
                                              sensor_record,
                                              sensor_record_len,
                                              &(sdr_record->record.sdr_entity_association_record),
                                              debug) < 0)
        goto cleanup;
      break;
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      if (_get_sdr_generic_device_locator_record (ctx,
                                                  sensor_record,
                                                  sensor_record_len,
                                                  &(sdr_record->record.sdr_generic_device_locator_record),
                                                  debug) < 0)
        goto cleanup;
      break;
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      if (_get_sdr_fru_device_locator_record (ctx,
                                              sensor_record,
                                              sensor_record_len,
                                              &(sdr_record->record.sdr_fru_device_locator_record),
                                              debug) < 0)
        goto cleanup;
      break;
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      if (_get_sdr_management_controller_device_locator_record (ctx,
                                                                sensor_record,
                                                                sensor_record_len,
                                                                &(sdr_record->record.sdr_management_controller_device_locator_record),
                                                                debug) < 0)
        goto cleanup;
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      if (_get_sdr_oem_record (ctx,
                               sensor_record,
                               sensor_record_len,
                               &(sdr_record->record.sdr_oem_record),
                               debug) < 0)
        goto cleanup;
      break;
    case IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD:
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MESAAGE_CHANNEL_INFO_RECORD:
    default:
      break;
    }
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  if (obj_sdr_record)
    fiid_obj_destroy(obj_sdr_record);
  return (rv);
}

int 
sdr_cache_create (sdr_cache_ctx_t ctx,
                  ipmi_ctx_t ipmi_ctx, 
                  char *host,
                  char *user_cache_dir,
                  int verbose,
                  int debug)
{
  char cachebuf[MAXPATHLEN+1];
  char cachefilenamebuf[MAXPATHLEN+1];
  struct stat buf;
  FILE *fp = NULL;
  unsigned int sdr_record_count = 0;
  int rv = -1;

  if (!ctx || ctx->magic != SDR_CACHE_CTX_MAGIC)
    return -1;

  if (!ipmi_ctx)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_PARAMETERS;
      return -1;
    }

  if (_get_sdr_cache_directory (ctx,
                                user_cache_dir,
                                cachebuf,
                                MAXPATHLEN) < 0)
    goto cleanup;

  if (stat(cachefilenamebuf, &buf) < 0)
    {
      if (_setup_sdr_cache_directory (ctx,
                                      host,
                                      user_cache_dir) < 0)
        goto cleanup;
    }

  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_get_sdr_cache_filename(ctx,
                              host,
                              user_cache_dir,
                              cachefilenamebuf,
                              MAXPATHLEN) < 0)
    goto cleanup;
                            
  if (!stat(cachefilenamebuf, &buf))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_EXISTS;
      goto cleanup;
    }
  
  if (!(fp = fopen (cachefilenamebuf, "w")))
    {
      if (errno == EPERM
          || errno == EACCES)
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_PERMISSION;
      else
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      goto cleanup;
    }

  if (verbose)
    fprintf (stderr, "Fetching SDR repository information... ");

  if (sdr_cache_write_repository_info (ctx, 
                                       ipmi_ctx, 
                                       fp,
                                       &sdr_record_count) < 0)
    goto cleanup;

  if (verbose)
    fprintf (stderr, "done\n");
  
  {
    uint16_t record_id = 0;
    uint16_t next_record_id = 0;
    sdr_record_t sdr_record;
    int i = 0;
    
    while (record_id != 0xFFFF)
      {
	memset (&sdr_record, 0, sizeof (sdr_record_t));

	if (verbose)
          fprintf (stderr, "Fetching record %d of %d (current record ID %d) \r", 
                   i++, 
                   sdr_record_count, 
                   record_id);

	if (_get_sdr_record (ctx,
                             ipmi_ctx, 
			     record_id, 
			     &next_record_id, 
			     &sdr_record,
                             debug) < 0)
          {
	    if (verbose)
              fprintf (stderr, "FAILED: Retrieval of SDR record 0x%X\n", record_id);
            goto cleanup;
          }
	
	if (sdr_cache_write_record (ctx, fp, &sdr_record) < 0)
          goto cleanup;
	
	record_id = next_record_id;
	next_record_id = 0;
      }
  }
  
  if (verbose)
    fprintf (stderr, "\n");

  rv = 0;
  ctx->errnum = SDR_CACHE_CTX_ERR_SUCCESS;
 cleanup:
  if (rv < 0)
    unlink(cachefilenamebuf);
  if (fp)
    fclose(fp);
  return rv;
}

int 
sdr_cache_flush (sdr_cache_ctx_t ctx,
                 char *host,
                 char *user_cache_dir)
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

static int 
_fread_record (sdr_cache_ctx_t ctx,
               FILE *fp, 
               char **cache_record)
{
  char *record = NULL;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(fp);

  record = NULL;
  
  while (1)
    {
      char *line = NULL;
      size_t n = 0;
      
      if (getline (&line, &n, fp) < 0)
        return (-1);
      else 
	{
	  char *tmp_lineptr = line;
	  if (!(line = strdupa (stripwhite (tmp_lineptr))))
            {
              ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
              return -1;
            }
	}

      if (!strlen (line))
	{
	  *cache_record = NULL;
	  if (record)
            {
              if (!(*cache_record = strdup (record)))
                {
                  ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
                  return -1;
                }
            }
	  return 0;
	}
      
      {
	char *l_record = NULL;
	int len = 0;
	
	if (record)
	  {
	    len = strlen (record) + strlen (line) + 2;
	    if (!(l_record = alloca (len)))
              {
                ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
                return -1;
              }
	    strcpy (l_record, record);
	    strcat (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
	else 
	  {
	    len = strlen (line) + 2;
	    if (!(l_record = alloca (len)))
              {
                ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
                return -1;
              }
	    strcpy (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
      }
    }
  
  return 0;
}

static int 
_get_record_count(sdr_cache_ctx_t ctx,
                  FILE *fp, 
                  int *sdr_record_count)
{
  char *cache_record = NULL;
  int rcount = 0;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(fp);
  assert(sdr_record_count);

  while (1)
    {
      cache_record = NULL;
      if (_fread_record (ctx, fp, &cache_record) < 0)
	{
	  *sdr_record_count = rcount;
	  return 0;
	}
      rcount++;
      free (cache_record);
    }
  
  return (-1);
}

static int 
_get_sdr_timestamps (sdr_cache_ctx_t ctx,
                     ipmi_ctx_t ipmi_ctx, 
                     unsigned int *addition_timestamp,
                     unsigned int *erase_timestamp)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(ipmi_ctx);
  assert(addition_timestamp);
  assert(erase_timestamp);

  _SDR_FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sdr_repository_info_rs);
  
  if (ipmi_cmd_get_sdr_repository_info (ipmi_ctx, obj_cmd_rs) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_IPMI_COMMUNICATION;
      goto cleanup;
    }
  
  _SDR_FIID_OBJ_GET (obj_cmd_rs, "most_recent_addition_timestamp", &val);
  *addition_timestamp = val;
  
  _SDR_FIID_OBJ_GET (obj_cmd_rs, "most_recent_erase_timestamp", &val);
  *erase_timestamp = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int 
sdr_cache_load (sdr_cache_ctx_t ctx,
                ipmi_ctx_t ipmi_ctx,
                char *host,
                char *user_cache_dir,
		sdr_record_t **sdr_record_list,
                unsigned int *sdr_record_count)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  char *cache_record = NULL;
  sdr_record_t *l_sdr_record_list = NULL;
  FILE *fp = NULL;
  int rv = -1;
  unsigned int sdr_addition_timestamp, sdr_erase_timestamp;
  unsigned int cache_addition_timestamp, cache_erase_timestamp;

  if (!ctx || ctx->magic != SDR_CACHE_CTX_MAGIC)
    return -1;

  if (!ipmi_ctx
      || !sdr_record_list
      || !sdr_record_count)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_PARAMETERS;
      return -1;
    }
  
  memset(cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_get_sdr_cache_filename(ctx,
                              host,
                              user_cache_dir,
                              cachefilenamebuf,
                              MAXPATHLEN) < 0)
    goto cleanup;

  if (!(fp = fopen (cachefilenamebuf, "r")))
    {
      if (errno == EPERM
          || errno == EACCES)
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_PERMISSION;
      else if (errno == ENOENT)
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_DOES_NOT_EXIST;
      else
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
      goto cleanup;
    }

  /* This particular _fread_record() should not fail.  If it does, the
   * cache is bad.  
   */
  if (_fread_record (ctx, fp, &cache_record) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;
      goto cleanup;
    }

  if (sdr_cache_read_repository_info_timestamps (ctx,
                                                 cache_record, 
                                                 &cache_addition_timestamp,
                                                 &cache_erase_timestamp) < 0)
    goto cleanup;
  free (cache_record);
  cache_record = NULL;
  
  if (_get_sdr_timestamps (ctx,
                           ipmi_ctx, 
                           &sdr_addition_timestamp,
                           &sdr_erase_timestamp) < 0)
    goto cleanup;

  if (!(sdr_addition_timestamp == cache_addition_timestamp
        && sdr_erase_timestamp == cache_erase_timestamp))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE;
      goto cleanup;
    }

  {
    long int current_position;
    int l_count;
    int i;
    
    if ((current_position = ftell (fp)) < 0)
      {
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
        goto cleanup;
      }

    if (_get_record_count (ctx, fp, &l_count) < 0)
      goto cleanup;

    if (!l_count)
      {
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_EMPTY;
        goto cleanup;
      }

    if (fseek (fp, current_position, SEEK_SET))
      {
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
        goto cleanup;
      }
    
    l_sdr_record_list = calloc (l_count, sizeof (sdr_record_t));
    for (i = 0; i < l_count; i++)
      {
	if (_fread_record (ctx, fp, &cache_record) < 0)
	  {
            ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;
            goto cleanup;
	  }
	if (sdr_cache_read_record (ctx, cache_record, (l_sdr_record_list + i)) < 0)
          goto cleanup;
        free(cache_record);
        cache_record = NULL;
      }
    
    *sdr_record_list = l_sdr_record_list;
    l_sdr_record_list = NULL;
    *sdr_record_count = l_count;
  }
  
  rv = 0;
  ctx->errnum = SDR_CACHE_CTX_ERR_SUCCESS;
 cleanup:
  if (l_sdr_record_list)
    free (l_sdr_record_list);
  if (cache_record)
    free(cache_record);
  if (fp)
    fclose(fp);
  return rv;
}

int 
sdr_cache_create_and_load (sdr_cache_ctx_t ctx,
                           ipmi_ctx_t ipmi_ctx,
                           char *host,
                           char *user_cache_dir,
                           int verbose,
                           int debug,
                           sdr_record_t **sdr_record_list,
                           unsigned int *sdr_record_count,
                           char *errmsg,
                           unsigned int errmsglen)
{
  int rv = -1;

  if (!ctx || ctx->magic != SDR_CACHE_CTX_MAGIC)
    return -1;

  if (!ipmi_ctx
      || !sdr_record_list
      || !sdr_record_count
      || !errmsg
      || !errmsglen)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_PARAMETERS;
      return -1;
    }

  *sdr_record_list = NULL;
  *sdr_record_count = 0;
  if (sdr_cache_load(ctx,
                     ipmi_ctx,
                     host,
                     user_cache_dir,
                     sdr_record_list,
                     sdr_record_count) < 0)
    {
      if (sdr_cache_ctx_errnum(ctx) != SDR_CACHE_CTX_ERR_CACHE_DOES_NOT_EXIST)
        {
          if (sdr_cache_ctx_errnum(ctx) == SDR_CACHE_CTX_ERR_CACHE_INVALID
              || sdr_cache_ctx_errnum(ctx) ==  SDR_CACHE_CTX_ERR_CACHE_OUT_OF_DATE)
            snprintf(errmsg,
                     errmsglen,
                     "SDR Cache load failed: %s; SDR cache may need to be flushed and reloaded",
                     sdr_cache_ctx_strerror(sdr_cache_ctx_errnum(ctx)));
          else
            snprintf(errmsg,
                     errmsglen,
                     "SDR Cache load failed: %s",
                     sdr_cache_ctx_strerror(sdr_cache_ctx_errnum(ctx)));
          goto cleanup;
        }
    }

  if (sdr_cache_ctx_errnum(ctx) == SDR_CACHE_CTX_ERR_CACHE_DOES_NOT_EXIST)
    {
      if (sdr_cache_create (ctx,
                            ipmi_ctx,
                            host,
                            user_cache_dir,
                            verbose,
                            debug) < 0)
        {
          snprintf(errmsg,
                   errmsglen,
                   "SDR Cache creation failed: %s",
                   sdr_cache_ctx_strerror(sdr_cache_ctx_errnum(ctx)));
          goto cleanup;
        }

      if (sdr_cache_load(ctx,
                         ipmi_ctx,
                         host,
                         user_cache_dir,
                         sdr_record_list,
                         sdr_record_count) < 0)
        {
          snprintf(errmsg,
                   errmsglen,
                   "SDR Cache load failed: %s",
                   sdr_cache_ctx_strerror(sdr_cache_ctx_errnum(ctx)));
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  if (rv < 0)
    {
      if (*sdr_record_count)
        {
          free(*sdr_record_list);
          *sdr_record_list = NULL;
          *sdr_record_count = 0;
        }
    }
  return (rv);
}
