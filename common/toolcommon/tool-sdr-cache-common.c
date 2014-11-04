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
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <pwd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <limits.h>             /* MAXHOSTNAMELEN */
#ifdef HAVE_NETDB_H
#include <netdb.h>              /* MAXHOSTNAMELEN Solaris */
#endif /* HAVE_NETDB_H */
#include <libgen.h>
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

#include "tool-sdr-cache-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-cmdline-common.h"

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
  char *tbuf = NULL;
  int ret;
  int rv = -1;

  assert (buf);
  assert (buflen);

#if defined(_SC_GETPW_R_SIZE_MAX)
  tbuf_len = sysconf (_SC_GETPW_R_SIZE_MAX);
  if (tbuf_len < 0)
    /* Variable was not implemented */
#endif
    tbuf_len = 4096;      /* XXX */

  if (!(tbuf = malloc (tbuf_len)))
    {
      PSTDOUT_PERROR (pstate, "malloc");
      goto cleanup;
    }

  user_id = getuid ();
  memset (&pwd, '\0', sizeof (struct passwd));
#if defined(HAVE_FUNC_GETPWUID_R_5)
  if (getpwuid_r (user_id,
                  &pwd,
                  tbuf,
                  tbuf_len,
                  &(pwdptr)) != 0)
    {
      PSTDOUT_PERROR (pstate, "getpwuid_r");
      goto cleanup;
    }

  if (!pwdptr)
    {
      /* User not found - can't figure out cache directory */
      PSTDOUT_PERROR (pstate, "getpwuid_r");
      goto cleanup;
    }
#elif defined(HAVE_FUNC_GETPWUID_R_4)
  /* Jan Forch - Solaris getpwuid_r returns ptr, not integer */
  if (!getpwuid_r (user_id,
                   &pwd,
                   tbuf,
                   tbuf_len))
    {
      PSTDOUT_PERROR (pstate, "getpwuid_r");
      goto cleanup;
    }
#endif /* !defined(HAVE_FUNC_GETPWUID_R_4) */

  if (pwd.pw_dir)
    {
      if (!access (pwd.pw_dir, R_OK|W_OK|X_OK))
        {
          if (strlen (pwd.pw_dir) > (buflen - 1))
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "internal overflow error\n");
              goto cleanup;
            }

          strcpy (buf, pwd.pw_dir);
          rv = 0;
          goto cleanup;
        }
    }

  if ((ret = snprintf (buf,
                       buflen,
                       "/tmp/.%s-%s",
                       PACKAGE_NAME,
                       pwd.pw_name)) < 0)
    {
      PSTDOUT_PERROR (pstate, "snprintf");
      goto cleanup;
    }

  if (ret >= buflen)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "snprintf invalid bytes written\n");
      goto cleanup;
    }

  if (access (buf, R_OK|W_OK|X_OK) < 0)
    {
      if (errno == ENOENT)
        {
          if (mkdir (buf, FREEIPMI_CONFIG_DIRECTORY_MODE) < 0)
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "Cannot make cache directory: %s: %s\n",
                               buf,
                               strerror (errno));
              goto cleanup;
            }
        }
      else
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "Cannot access cache directory: %s\n",
                           buf);
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  free (tbuf);
  return (0);
}

static int
_get_config_directory (pstdout_state_t pstate,
                       const char *cache_dir,
                       char *buf,
                       unsigned int buflen)
{
  char tbuf[MAXPATHLEN+1];
  int ret;

  assert (buf);
  assert (buflen);

  memset (tbuf, '\0', MAXPATHLEN+1);
  if (!cache_dir)
    {
      if (_get_home_directory (pstate,
                               tbuf,
                               MAXPATHLEN) < 0)
        return (-1);
    }
  else
    {
      if (strlen (cache_dir) > (MAXPATHLEN - 1))
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "internal overflow error\n");
          return (-1);
        }
      strcpy (tbuf, cache_dir);

      if (access (tbuf, R_OK|W_OK|X_OK) < 0)
        {
          if (errno == ENOENT)
            {
              if (mkdir (tbuf, FREEIPMI_CONFIG_DIRECTORY_MODE) < 0)
                {
                  PSTDOUT_FPRINTF (pstate,
                                   stderr,
                                   "Cannot make cache directory: %s: %s\n",
                                   tbuf,
                                   strerror (errno));
                  return (-1);
                }
            }
          else
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "Cannot access cache directory: %s\n",
                               tbuf);
              return (-1);
            }
        }
    }

  if ((ret = snprintf (buf,
                       buflen,
                       "%s/.%s",
                       tbuf,
                       PACKAGE_NAME)) < 0)
    {
      PSTDOUT_PERROR (pstate, "snprintf");
      return (-1);
    }

  if (ret >= buflen)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "snprintf invalid bytes written\n");
      return (-1);
    }

  return (0);
}

static int
_sdr_cache_get_cache_directory (pstdout_state_t pstate,
				const char *cache_dir,
				char *buf,
				unsigned int buflen)
{
  char tbuf[MAXPATHLEN+1];
  int ret;

  assert (buf);
  assert (buflen);

  memset (tbuf, '\0', MAXPATHLEN+1);
  if (_get_config_directory (pstate,
                             cache_dir,
                             tbuf,
                             MAXPATHLEN) < 0)
    return (-1);

  if ((ret = snprintf (buf,
                       buflen,
                       "%s/%s",
                       tbuf,
                       SDR_CACHE_DIR)) < 0)
    {
      PSTDOUT_PERROR (pstate, "snprintf");
      return (-1);
    }

  if (ret >= buflen)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "snprintf invalid bytes written\n");
      return (-1);
    }

  return (0);
}

static int
_sdr_cache_get_cache_filename (pstdout_state_t pstate,
			       const char *hostname,
			       const struct common_cmd_args *common_args,
			       char *buf,
			       unsigned int buflen)
{
  char sdrcachebuf[MAXPATHLEN+1];
  char *ptr;
  int ret;

  assert (common_args);
  assert (buf);
  assert (buflen);

  if (!common_args->sdr_cache_file)
    {
      char hostnamebuf[MAXHOSTNAMELEN+1];

      memset (hostnamebuf, '\0', MAXHOSTNAMELEN+1);
      if (gethostname (hostnamebuf, MAXHOSTNAMELEN) < 0)
	snprintf (hostnamebuf, MAXHOSTNAMELEN, "localhost");

      /* shorten hostname if necessary */
      if ((ptr = strchr (hostnamebuf, '.')))
	*ptr = '\0';
      
      if (_sdr_cache_get_cache_directory (pstate,
					  common_args->sdr_cache_directory,
					  sdrcachebuf,
					  MAXPATHLEN) < 0)
	return (-1);

      if ((ret = snprintf (buf,
			   buflen,
			   "%s/%s-%s.%s",
			   sdrcachebuf,
			   SDR_CACHE_FILENAME_PREFIX,
			   hostnamebuf,
			   hostname ? hostname : "localhost")) < 0)
	
	{
	  PSTDOUT_PERROR (pstate, "snprintf");
	  return (-1);
	}

      if (ret >= buflen)
	{
	  PSTDOUT_FPRINTF (pstate,
			   stderr,
			   "snprintf invalid bytes written\n");
	  return (-1);
	}
    }
  else
    {
      if (strlen (common_args->sdr_cache_file) > (MAXPATHLEN - 1))
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "internal overflow error\n");
          return (-1);
        }
      
      if (access (common_args->sdr_cache_file, R_OK) < 0)
        {
	  if (errno != ENOENT)
	    {
	      PSTDOUT_FPRINTF (pstate,
			       stderr,
			       "Cannot access cache file: %s\n",
			       common_args->sdr_cache_file);
	      return (-1);
	    }
	  else
	    {
	      /* File doesn't exist, do checks for directory */
	      char cachedirbuf[MAXPATHLEN+1];
	      char *cachedirptr;
	      strcpy (cachedirbuf, common_args->sdr_cache_file);

	      cachedirptr = dirname (cachedirbuf);
	      
	      if (_sdr_cache_get_cache_directory (pstate,
						  cachedirptr,
						  sdrcachebuf,
						  MAXPATHLEN) < 0)
		return (-1);

	    }
        }
      
      if ((ret = snprintf (buf,
                           buflen,
			   "%s",
			   common_args->sdr_cache_file)) < 0)

        {
          PSTDOUT_PERROR (pstate, "snprintf");
          return (-1);
        }

      if (ret >= buflen)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "snprintf invalid bytes written\n");
          return (-1);
	}

    }

  return (0);
}

static int
_setup_sdr_cache_directory (pstdout_state_t pstate,
                            const char *cache_dir)
{
  char configbuf[MAXPATHLEN+1];
  char cachebuf[MAXPATHLEN+1];
  int ret;

  memset (configbuf, '\0', MAXPATHLEN+1);
  memset (cachebuf, '\0', MAXPATHLEN+1);

  if (_get_config_directory (pstate,
                             cache_dir,
                             configbuf,
                             MAXPATHLEN) < 0)
    return (-1);

  errno = 0;
  ret = mkdir (configbuf, FREEIPMI_CONFIG_DIRECTORY_MODE);
  if (ret < 0 && errno != EEXIST)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "Cannot make cache directory: %s: %s\n",
                       configbuf,
                       strerror (errno));
      return (-1);
    }

  if (_sdr_cache_get_cache_directory (pstate,
				      cache_dir,
				      cachebuf,
				      MAXPATHLEN) < 0)
    return (-1);

  errno = 0;
  ret = mkdir (cachebuf, FREEIPMI_CONFIG_DIRECTORY_MODE);
  if (ret < 0 && errno != EEXIST)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "Cannot make cache directory: %s: %s\n",
                       cachebuf,
                       strerror (errno));
      return (-1);
    }

  return (0);
}

void
_sdr_cache_create_callback (uint8_t sdr_version,
                            uint16_t record_count,
                            uint32_t most_recent_addition_timestamp,
                            uint32_t most_recent_erase_timestamp,
                            uint16_t record_id,
                            void *data)
{
  int *count;

  assert (data);

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

static int
_sdr_cache_create_directory (pstdout_state_t pstate,
			     const char *cache_dir)
{
  char cachedirectorybuf[MAXPATHLEN+1];
  struct stat buf;

  if (_sdr_cache_get_cache_directory (pstate,
				      cache_dir,
				      cachedirectorybuf,
				      MAXPATHLEN) < 0)
    return (-1);

  if (stat (cachedirectorybuf, &buf) < 0)
    {
      if (_setup_sdr_cache_directory (pstate, cache_dir) < 0)
        return (-1);
    }

  return (0);
}

int
_sdr_cache_create (ipmi_sdr_ctx_t ctx,
		   pstdout_state_t pstate,
		   ipmi_ctx_t ipmi_ctx,
		   const char *hostname,
		   const struct common_cmd_args *common_args)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  int count = 0;
  int cache_create_flags = 0;
  int rv = -1;

  assert (ctx);
  assert (ipmi_ctx);
  assert (common_args);

  if (_sdr_cache_create_directory (pstate, common_args->sdr_cache_directory) < 0)
    goto cleanup;

  memset (cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_sdr_cache_get_cache_filename (pstate,
				     hostname,
				     common_args,
				     cachefilenamebuf,
				     MAXPATHLEN) < 0)
    goto cleanup;

  /* pstdout library can't handle \r, its the responsibility of
   * tool code to set quiet_cache if there are multiple
   * hosts are generating the cache at the same time.
   */
  if (!common_args->quiet_cache)
    fprintf (stderr,
             "Caching SDR repository information: %s\n",
             cachefilenamebuf);

  if (common_args->sdr_cache_recreate)
    cache_create_flags = IPMI_SDR_CACHE_CREATE_FLAGS_OVERWRITE;
  else
    cache_create_flags = IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT;

  if (common_args->workaround_flags_sdr & IPMI_PARSE_WORKAROUND_FLAGS_SDR_ASSUME_MAX_SDR_RECORD_COUNT)
    cache_create_flags |= IPMI_SDR_CACHE_CREATE_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT;

  if (ipmi_sdr_cache_create (ctx,
                             ipmi_ctx,
                             cachefilenamebuf,
                             cache_create_flags,
                             common_args->quiet_cache ? NULL : _sdr_cache_create_callback,
                             common_args->quiet_cache ? NULL : (void *)&count) < 0)
    {
      /* unique output corner case */
      if (count && !common_args->quiet_cache)
        fprintf (stderr, "\n");

      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_cache_create: %s\n",
                       ipmi_sdr_ctx_errormsg (ctx));
      goto cleanup;
    }

  if (!common_args->quiet_cache)
    fprintf (stderr, "\n");

  rv = 0;
 cleanup:
  if (rv < 0)
    ipmi_sdr_cache_delete (ctx, cachefilenamebuf);
  return (rv);
}

int
sdr_cache_create_and_load (ipmi_sdr_ctx_t sdr_ctx,
                           pstdout_state_t pstate,
                           ipmi_ctx_t ipmi_ctx,
                           const char *hostname,
			   const struct common_cmd_args *common_args)
{
  char cachefilenamebuf[MAXPATHLEN+1];
  int rv = -1;

  assert (sdr_ctx);
  assert (common_args);

  memset (cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_sdr_cache_get_cache_filename (pstate,
				     hostname,
				     common_args,
				     cachefilenamebuf,
				     MAXPATHLEN) < 0)
    goto cleanup;

  /* If user specifies cache file, don't check timestamps, just load it */

  if (ipmi_sdr_cache_open (sdr_ctx,
                           common_args->sdr_cache_file ? NULL : ipmi_ctx,
                           cachefilenamebuf) < 0)
    {
      if (ipmi_sdr_ctx_errnum (sdr_ctx) != IPMI_SDR_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST
          && !((ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_CACHE_INVALID
                || ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_CACHE_OUT_OF_DATE)
               && common_args->sdr_cache_recreate))
        {
          if (ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_CACHE_INVALID)
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "SDR Cache '%s' invalid: Please flush the cache and regenerate it\n",
                               cachefilenamebuf);
              goto cleanup;
            }
          else if (ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_CACHE_OUT_OF_DATE)
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "SDR Cache '%s' out of date: Please flush the cache and regenerate it\n",
                               cachefilenamebuf);
              goto cleanup;

            }
          else
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "ipmi_sdr_cache_open: %s: %s\n",
                               cachefilenamebuf,
                               ipmi_sdr_ctx_errormsg (sdr_ctx));
              goto cleanup;
            }
        }
    }

  if (ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST
      || ((ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_CACHE_INVALID
           || ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_CACHE_OUT_OF_DATE)
          && common_args->sdr_cache_recreate))
    {
      if (_sdr_cache_create (sdr_ctx,
			     pstate,
			     ipmi_ctx,
			     hostname,
			     common_args) < 0)
        goto cleanup;

      if (ipmi_sdr_cache_open (sdr_ctx,
			       common_args->sdr_cache_file ? NULL : ipmi_ctx,
                               cachefilenamebuf) < 0)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ipmi_sdr_cache_open: %s: %s\n",
                           cachefilenamebuf,
                           ipmi_sdr_ctx_errormsg (sdr_ctx));
          goto cleanup;
        }
    }

  if (common_args->debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_ctx_set_flags (sdr_ctx, IPMI_SDR_FLAGS_DEBUG_DUMP) < 0)
        PSTDOUT_FPRINTF (pstate,
                         stderr,
                         "ipmi_sdr_ctx_set_flags: %s\n",
                         ipmi_sdr_ctx_errormsg (sdr_ctx));
  
  if (hostname)
        {
          if (ipmi_sdr_ctx_set_debug_prefix (sdr_ctx, hostname) < 0)
            PSTDOUT_FPRINTF (pstate,
                             stderr,
                             "ipmi_sdr_ctx_set_debug_prefix: %s\n",
			     ipmi_sdr_ctx_errormsg (sdr_ctx));
	}
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

int
sdr_cache_flush_cache (pstdout_state_t pstate,
                       const char *hostname,
		       const struct common_cmd_args *common_args)
{
  ipmi_sdr_ctx_t ctx = NULL;
  char cachefilenamebuf[MAXPATHLEN+1];
  int rv = -1;

  assert (common_args);

  memset (cachefilenamebuf, '\0', MAXPATHLEN+1);
  if (_sdr_cache_get_cache_filename (pstate,
				     hostname,
				     common_args,
				     cachefilenamebuf,
				     MAXPATHLEN) < 0)
    goto cleanup;

  if (!common_args->quiet_cache)
    PSTDOUT_PRINTF (pstate, "Flushing cache: %s\n", cachefilenamebuf);

  if (!(ctx = ipmi_sdr_ctx_create ()))
    {
      PSTDOUT_PERROR (pstate, "ipmi_sdr_ctx_create");
      goto cleanup;
    }

  if (ipmi_sdr_cache_delete (ctx, cachefilenamebuf) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_sdr_cache_delete: %s\n",
                       ipmi_sdr_ctx_errormsg (ctx));
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  ipmi_sdr_ctx_destroy (ctx);
  return (rv);
}

int
ipmi_sdr_cache_search_sensor_wrapper (ipmi_sdr_ctx_t sdr_ctx,
				      uint8_t sensor_number,
				      uint8_t generator_id)
{
  int rv;

  assert (sdr_ctx);
  
  if ((rv = ipmi_sdr_cache_search_sensor (sdr_ctx,
					  sensor_number,
					  generator_id)) < 0)
    {
      /* IPMI Workaround (achu)                                                                              
       *                                                                                                     
       * Discovered on Supermicro H8QME with SIMSO daughter card.                                            
       *                                                                                                     
       * The slave address is reported incorrectly by having the
       * generator_id be shifted over by one.  This is a special "try
       * again" corner case.
       */
      if (ipmi_sdr_ctx_errnum (sdr_ctx) == IPMI_SDR_ERR_NOT_FOUND
	  && (generator_id == (IPMI_SLAVE_ADDRESS_BMC << 1)))
	{
	  if (!ipmi_sdr_cache_search_sensor (sdr_ctx,
					     sensor_number,
					     (generator_id >> 1)))
	    return (0);
	  /* else fall through to normal error case */
	}
      
      return (rv);
    }
  
  return (rv);
}

