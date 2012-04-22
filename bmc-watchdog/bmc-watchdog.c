/*****************************************************************************\
 *  $Id: bmc-watchdog.c,v 1.134 2010-06-28 20:24:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2004-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155913
 *
 *  This file is part of Bmc-watchdog, a base management controller
 *  (BMC) watchdog timer management tool. For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Bmc-Watchdog is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Bmc-Watchdog is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Bmc-Watchdog.  If not, see <http://www.gnu.org/licenses/>.
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
#include <signal.h>
#include <sys/types.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <sys/stat.h>
#include <sys/select.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "bmc-watchdog.h"
#include "bmc-watchdog-argp.h"

#include "freeipmi-portability.h"
#include "debug-util.h"
#include "tool-common.h"

#define BMC_WATCHDOG_ERR_BUFLEN           1024
#define BMC_WATCHDOG_STR_BUFLEN           1024
#define BMC_WATCHDOG_PKT_BUFLEN           1024
#define BMC_WATCHDOG_RESET_PERIOD_DEFAULT   60

#define BMC_WATCHDOG_RETRY_WAIT_TIME         1
#define BMC_WATCHDOG_RETRY_ATTEMPT           5

#define BMC_WATCHDOG_FIID_OBJ_GET(__obj, __field, __val, __func)        \
  do {                                                                  \
    uint64_t __temp;                                                    \
    if (FIID_OBJ_GET ((__obj), (__field), &__temp) < 0)                 \
      {                                                                 \
        _bmclog ("%s: fiid_obj_get: '%s': %s",                          \
                 (__func),                                              \
                 (__field),                                             \
                 fiid_obj_errormsg ((__obj)));                          \
        if (fiid_obj_errnum ((__obj)) == FIID_ERR_SUCCESS)              \
          errno = 0;                                                    \
        else if (fiid_obj_errnum ((__obj)) == FIID_ERR_OUT_OF_MEMORY)   \
          errno = ENOMEM;                                               \
        else if (fiid_obj_errnum ((__obj)) == FIID_ERR_OVERFLOW)        \
          errno = ENOSPC;                                               \
        else                                                            \
          errno = EINVAL;                                               \
        goto cleanup;                                                   \
      }                                                                 \
    *(__val) = __temp;                                                  \
  } while (0)

#define BMC_WATCHDOG_PIDFILE BMC_WATCHDOG_LOCALSTATEDIR "/run/bmc-watchdog.pid"

struct bmc_watchdog_arguments cmd_args;

/* program name */
static char *err_progname = NULL;

static ipmi_kcs_ctx_t kcs_ctx = NULL;
static ipmi_ssif_ctx_t ssif_ctx = NULL;
static ipmi_openipmi_ctx_t openipmi_ctx = NULL;
static ipmi_sunbmc_ctx_t sunbmc_ctx = NULL;
static int driver_type_used = -1;

static int shutdown_flag = 1;

static int logfile_fd = -1;

static void
_syslog (int priority, const char *fmt, ...)
{
  char buffer[BMC_WATCHDOG_ERR_BUFLEN];
  va_list ap;

  assert (fmt && err_progname);

  if (cmd_args.no_logging)
    return;

  va_start (ap, fmt);
  snprintf (buffer, BMC_WATCHDOG_ERR_BUFLEN, "%s: %s\n", err_progname, fmt);
  vsyslog (priority, buffer, ap);
  va_end (ap);
}

static void
_bmclog_write (void *buf, size_t count)
{
  ssize_t ret;
  size_t left;
  char *ptr;

  ptr = buf;
  left = count;

  while (left > 0)
    {
      if ((ret = write (logfile_fd, ptr, left)) < 0)
        {
          if (errno == EINTR)
            continue;
          else
            {
              /* The only place we should really need to syslog */
              _syslog (LOG_ERR, "_bmcwrite: write: %s", strerror (errno));
              return;
            }
        }
      ptr += ret;
      left -= ret;
    }
}

static void
_bmclog (const char *fmt, ...)
{
  time_t t;
  struct tm *tm;
  int len;
  char buffer[BMC_WATCHDOG_ERR_BUFLEN];
  char fbuffer[BMC_WATCHDOG_ERR_BUFLEN];
  va_list ap;

  assert (fmt
          && err_progname
          && (cmd_args.no_logging || logfile_fd >= 0));

  if (cmd_args.no_logging)
    return;

  va_start (ap, fmt);
  t = time (NULL);

  if (!(tm = localtime (&t)))
    {
      /* Just use the value from time() */
      snprintf (buffer, BMC_WATCHDOG_ERR_BUFLEN, "%ld: %s\n", t, fmt);
    }
  else
    {
      char tbuffer[BMC_WATCHDOG_ERR_BUFLEN];
      strftime (tbuffer, BMC_WATCHDOG_ERR_BUFLEN, "[%b %d %H:%M:%S]", tm);
      snprintf (buffer, BMC_WATCHDOG_ERR_BUFLEN, "%s: %s\n", tbuffer, fmt);
    }

  len = vsnprintf (fbuffer, BMC_WATCHDOG_ERR_BUFLEN, buffer, ap);

  _bmclog_write (fbuffer, len);

  va_end (ap);
}

static void
_err_init (char *progname)
{
  char *ptr = strrchr (progname, '/');
  err_progname = (!ptr) ? progname : ptr + 1;
}

static void
_err_exit (char *fmt, ...)
{
  char buffer[BMC_WATCHDOG_ERR_BUFLEN];
  va_list ap;

  assert (fmt && err_progname);

  va_start (ap, fmt);
  snprintf (buffer, BMC_WATCHDOG_ERR_BUFLEN, "%s: %s\n", err_progname, fmt);
  vfprintf (stderr, buffer, ap);
  va_end (ap);
  exit (1);
}

static int
_init_kcs_ipmi (void)
{
  ipmi_locate_ctx_t locate_ctx = NULL;
  struct ipmi_locate_info locate_info;
  int rv = -1;

  if (!(locate_ctx = ipmi_locate_ctx_create ()))
    {
      _bmclog ("ipmi_locate_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (!cmd_args.common.disable_auto_probe)
    {
      if (ipmi_locate_get_device_info (locate_ctx,
                                       IPMI_INTERFACE_KCS,
                                       &locate_info) < 0)
        {
          _bmclog ("ipmi_locate_get_device_info: %s",
                   ipmi_locate_ctx_errormsg (locate_ctx));
          goto cleanup;
        }
    }

  if (!(kcs_ctx = ipmi_kcs_ctx_create ()))
    {
      _bmclog ("ipmi_kcs_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (cmd_args.common.driver_address)
    locate_info.driver_address = cmd_args.common.driver_address;
  if (cmd_args.common.register_spacing)
    locate_info.register_spacing = cmd_args.common.register_spacing;

  if (!cmd_args.common.disable_auto_probe || cmd_args.common.driver_address)
    {
      if (ipmi_kcs_ctx_set_driver_address (kcs_ctx, locate_info.driver_address) < 0)
        {
          _bmclog ("ipmi_kcs_ctx_set_driver_address: %s",
                   ipmi_kcs_ctx_strerror (ipmi_kcs_ctx_errnum (kcs_ctx)));
          goto cleanup;
        }
    }

  if (!cmd_args.common.disable_auto_probe || cmd_args.common.register_spacing)
    {
      if (ipmi_kcs_ctx_set_register_spacing (kcs_ctx, locate_info.register_spacing) < 0)
        {
          _bmclog ("ipmi_kcs_ctx_set_register_spacing: %s",
                   ipmi_kcs_ctx_strerror (ipmi_kcs_ctx_errnum (kcs_ctx)));
          goto cleanup;
        }
    }

  if (ipmi_kcs_ctx_set_flags (kcs_ctx, IPMI_KCS_FLAGS_NONBLOCKING) < 0)
    {
      _bmclog ("ipmi_kcs_ctx_set_flags: %s",
               ipmi_kcs_ctx_strerror (ipmi_kcs_ctx_errnum (kcs_ctx)));
      goto cleanup;
    }

  if (ipmi_kcs_ctx_io_init (kcs_ctx) < 0)
    {
      _bmclog ("ipmi_kcs_ctx_io_init: %s",
               ipmi_kcs_ctx_strerror (ipmi_kcs_ctx_errnum (kcs_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  ipmi_locate_ctx_destroy (locate_ctx);
  return (rv);
}

static int
_init_ssif_ipmi (void)
{
  ipmi_locate_ctx_t locate_ctx = NULL;
  struct ipmi_locate_info locate_info;
  int rv = -1;

  if (!(locate_ctx = ipmi_locate_ctx_create ()))
    {
      _bmclog ("ipmi_locate_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (!cmd_args.common.disable_auto_probe)
    {
      if (ipmi_locate_get_device_info (locate_ctx,
                                       IPMI_INTERFACE_SSIF,
                                       &locate_info) < 0)
        {
          _bmclog ("ipmi_locate_get_device_info: %s",
                   ipmi_locate_ctx_errormsg (locate_ctx));
          goto cleanup;
        }
    }
  if (!(ssif_ctx = ipmi_ssif_ctx_create ()))
    {
      _bmclog ("ipmi_ssif_ctx_create: %s", strerror (errno));
      goto cleanup;
    }

  if (cmd_args.common.driver_address)
    locate_info.driver_address = cmd_args.common.driver_address;
  if (cmd_args.common.driver_device)
    {
      strncpy (locate_info.driver_device, cmd_args.common.driver_device, IPMI_LOCATE_PATH_MAX);
      locate_info.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
    }

  if (!cmd_args.common.disable_auto_probe || cmd_args.common.driver_address)
    {
      if (ipmi_ssif_ctx_set_driver_address (ssif_ctx, locate_info.driver_address) < 0)
        {
          _bmclog ("ipmi_ssif_ctx_set_driver_address: %s",
                   ipmi_ssif_ctx_strerror (ipmi_ssif_ctx_errnum (ssif_ctx)));
          goto cleanup;
        }
    }

  if (!cmd_args.common.disable_auto_probe || cmd_args.common.driver_device)
    {
      if (ipmi_ssif_ctx_set_driver_device (ssif_ctx, locate_info.driver_device) < 0)
        {
          _bmclog ("ipmi_ssif_ctx_set_driver_device: %s",
                   ipmi_ssif_ctx_strerror (ipmi_ssif_ctx_errnum (ssif_ctx)));
          goto cleanup;
        }
    }

  if (ipmi_ssif_ctx_set_flags (ssif_ctx, IPMI_SSIF_FLAGS_NONBLOCKING) < 0)
    {
      _bmclog ("ipmi_ssif_ctx_set_flags: %s",
               ipmi_ssif_ctx_strerror (ipmi_ssif_ctx_errnum (ssif_ctx)));
      goto cleanup;
    }

  if (ipmi_ssif_ctx_io_init (ssif_ctx) < 0)
    {
      _bmclog ("ipmi_ssif_ctx_io_init: %s",
               ipmi_ssif_ctx_strerror (ipmi_ssif_ctx_errnum (ssif_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  ipmi_locate_ctx_destroy (locate_ctx);
  return (rv);
}

static int
_init_openipmi_ipmi (void)
{
  if (!(openipmi_ctx = ipmi_openipmi_ctx_create ()))
    {
      _bmclog ("ipmi_openipmi_ctx_create: %s", strerror (errno));
      return (-1);
    }

  if (cmd_args.common.driver_device)
    {
      if (ipmi_openipmi_ctx_set_driver_device (openipmi_ctx,
                                               cmd_args.common.driver_device) < 0)
        {
          _bmclog ("ipmi_openipmi_ctx_set_driver_device: %s",
                   ipmi_openipmi_ctx_strerror (ipmi_openipmi_ctx_errnum (openipmi_ctx)));
          return (-1);
        }
    }

  if (ipmi_openipmi_ctx_io_init (openipmi_ctx) < 0)
    {
      _bmclog ("ipmi_openipmi_ctx_io_init: %s",
               ipmi_openipmi_ctx_strerror (ipmi_openipmi_ctx_errnum (openipmi_ctx)));
      return (-1);
    }

  return (0);
}

static int
_init_sunbmc_ipmi (void)
{
  if (!(sunbmc_ctx = ipmi_sunbmc_ctx_create ()))
    {
      _bmclog ("ipmi_sunbmc_ctx_create: %s", strerror (errno));
      return (-1);
    }

  if (cmd_args.common.driver_device)
    {
      if (ipmi_sunbmc_ctx_set_driver_device (sunbmc_ctx,
                                             cmd_args.common.driver_device) < 0)
        {
          _bmclog ("ipmi_sunbmc_ctx_set_driver_device: %s",
                   ipmi_sunbmc_ctx_strerror (ipmi_sunbmc_ctx_errnum (sunbmc_ctx)));
          return (-1);
        }
    }

  if (ipmi_sunbmc_ctx_io_init (sunbmc_ctx) < 0)
    {
      _bmclog ("ipmi_sunbmc_ctx_io_init: %s",
               ipmi_sunbmc_ctx_strerror (ipmi_sunbmc_ctx_errnum (sunbmc_ctx)));
      return (-1);
    }

  return (0);
}

/* Must be called after cmdline parsed b/c user may pass in io port */
static int
_init_ipmi (void)
{
  ipmi_locate_ctx_t locate_ctx = NULL;

  assert (err_progname);

  if (!(locate_ctx = ipmi_locate_ctx_create ()))
    _err_exit ("Error creating locate_ctx: %s", strerror (errno));

  if (!ipmi_is_root ())
    _err_exit ("Permission denied, must be root.");

  if (cmd_args.common.driver_type != IPMI_DEVICE_UNKNOWN)
    {
      if (cmd_args.common.driver_type == IPMI_DEVICE_KCS)
        {
          if (_init_kcs_ipmi () < 0)
            _err_exit ("Error initializing KCS IPMI driver");
          driver_type_used = IPMI_DEVICE_KCS;
        }
      if (cmd_args.common.driver_type == IPMI_DEVICE_SSIF)
        {
          if (_init_ssif_ipmi () < 0)
            _err_exit ("Error initializing SSIF IPMI driver");
          driver_type_used = IPMI_DEVICE_SSIF;
        }
      if (cmd_args.common.driver_type == IPMI_DEVICE_OPENIPMI)
        {
          if (_init_openipmi_ipmi () < 0)
            _err_exit ("Error initializing OPENIPMI IPMI driver");
          driver_type_used = IPMI_DEVICE_OPENIPMI;
        }
      if (cmd_args.common.driver_type == IPMI_DEVICE_SUNBMC)
        {
          if (_init_sunbmc_ipmi () < 0)
            _err_exit ("Error initializing SUNBMC IPMI driver");
          driver_type_used = IPMI_DEVICE_SUNBMC;
        }
    }
  else
    {
      struct ipmi_locate_info locate_info;

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

      if (!ipmi_locate_discover_device_info (locate_ctx,
                                             IPMI_INTERFACE_KCS,
                                             &locate_info))
        {
          if (!_init_kcs_ipmi ())
            {
              driver_type_used = IPMI_DEVICE_KCS;
              goto out;
            }
        }

      if (!ipmi_locate_discover_device_info (locate_ctx,
                                             IPMI_INTERFACE_SSIF,
                                             &locate_info))
        {
          if (!_init_ssif_ipmi ())
            {
              driver_type_used = IPMI_DEVICE_SSIF;
              goto out;
            }
        }

      if (_init_sunbmc_ipmi () < 0)
        {
          if (_init_openipmi_ipmi () < 0)
            {
              if (_init_kcs_ipmi () < 0)
                {
                  if (_init_ssif_ipmi () < 0)
                    _err_exit ("Error initializing IPMI driver");
                  else
                    driver_type_used = IPMI_DEVICE_SSIF;
                }
              else
                driver_type_used = IPMI_DEVICE_KCS;
            }
          else
            driver_type_used = IPMI_DEVICE_OPENIPMI;
        }
      else
        driver_type_used = IPMI_DEVICE_SUNBMC;
    }

 out:
  ipmi_locate_ctx_destroy (locate_ctx);
  return (0);
}

/* Must be called after cmdline parsed */
static void
_init_bmc_watchdog (int facility, int err_to_stderr)
{
  assert (facility == LOG_CRON || facility == LOG_DAEMON);

  openlog (err_progname, LOG_ODELAY | LOG_PID, facility);

  if (!cmd_args.no_logging)
    {
      if ((logfile_fd = open ((cmd_args.logfile) ? cmd_args.logfile : BMC_WATCHDOG_LOGFILE_DEFAULT,
                              O_WRONLY | O_CREAT | O_APPEND,
                              S_IRUSR | S_IWUSR)) < 0)
        {
          if (err_to_stderr)
            _err_exit ("Error opening logfile '%s': %s",
                       (cmd_args.logfile) ? cmd_args.logfile : BMC_WATCHDOG_LOGFILE_DEFAULT,
                       strerror (errno));
          else
            _syslog (LOG_ERR, "Error opening logfile '%s': %s",
                     (cmd_args.logfile) ? cmd_args.logfile : BMC_WATCHDOG_LOGFILE_DEFAULT,

                     strerror (errno));
          exit (1);
        }
    }

  if (_init_ipmi () < 0)
    {
      if (err_to_stderr)
        _err_exit ("_init_ipmi: %s", strerror (errno));
      else
        _syslog (LOG_ERR, "_init_ipmi: %s", strerror (errno));
      exit (1);
    }
}

static void
_ipmi_err_exit (uint8_t cmd, uint8_t netfn, int comp_code, char *str)
{
  char buf[BMC_WATCHDOG_ERR_BUFLEN];

  assert (str);

  if (comp_code < 0)
    {
      if (errno == EAGAIN || errno == EBUSY)
        _err_exit ("%s: BMC Busy", str);
      else
        _err_exit ("%s: %s", str, strerror (errno));
    }
  else
    {
      if (ipmi_completion_code_strerror_r (cmd,
                                           netfn,
                                           comp_code,
                                           buf,
                                           BMC_WATCHDOG_ERR_BUFLEN) < 0)
        _err_exit ("ipmi_completion_code_strerror_r: %s", strerror (errno));
      _err_exit ("%s: %s", str, buf);
    }
}

/* signal handlers + sleep(3) is a bad idea */
static int
_sleep (unsigned int sleep_len)
{
  struct timeval tv;

  if (!sleep_len)
    return (0);

  tv.tv_sec = sleep_len;
  tv.tv_usec = 0;
  if (select (1, NULL, NULL, NULL, &tv) < 0)
    {
      if (errno != EINTR)
        _err_exit ("select: %s", strerror (errno));
    }
  return (0);
}

static int
_cmd (char *str,
      int retry_wait_time,
      int retry_attempt,
      uint8_t netfn,
      uint8_t cmd,
      fiid_obj_t cmd_rq,
      fiid_obj_t cmd_rs)
{
  uint8_t comp_code;
  int retry_count = 0;
  int ret = 0;

  assert (str
          && retry_wait_time >= 0
          && retry_attempt >= 0
          && (netfn == IPMI_NET_FN_APP_RQ || netfn == IPMI_NET_FN_TRANSPORT_RQ)
          && cmd_rq
          && cmd_rs);

  if (cmd_args.common.debug)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                     DEBUG_UTIL_DIRECTION_REQUEST,
                     netfn,
                     cmd,
		     0,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_obj_dump (STDERR_FILENO, NULL, hdrbuf, NULL, cmd_rq) < 0)
        _bmclog ("%s: ipmi_obj_dump: %s", str, strerror (errno));
    }

  while (1)
    {
      if (driver_type_used == IPMI_DEVICE_KCS)
        {
          if ((ret = ipmi_kcs_cmd (kcs_ctx,
                                   IPMI_BMC_IPMB_LUN_BMC,
                                   netfn,
                                   cmd_rq,
                                   cmd_rs)) < 0)
            {
              if (ipmi_kcs_ctx_errnum (kcs_ctx) != IPMI_KCS_ERR_BUSY)
                {
                  _bmclog ("%s: ipmi_kcs_cmd: %s",
                           str,
                           ipmi_kcs_ctx_strerror (ipmi_kcs_ctx_errnum (kcs_ctx)));
                  if (ipmi_kcs_ctx_errnum (kcs_ctx) == IPMI_KCS_ERR_PARAMETERS)
                    errno = EINVAL;
                  else if (ipmi_kcs_ctx_errnum (kcs_ctx) == IPMI_KCS_ERR_PERMISSION)
                    errno = EPERM;
                  else if (ipmi_kcs_ctx_errnum (kcs_ctx) == IPMI_KCS_ERR_OUT_OF_MEMORY)
                    errno = ENOMEM;
                  else if (ipmi_kcs_ctx_errnum (kcs_ctx) == IPMI_KCS_ERR_IO_NOT_INITIALIZED)
                    errno = EIO;
                  else if (ipmi_kcs_ctx_errnum (kcs_ctx) == IPMI_KCS_ERR_OVERFLOW)
                    errno = ENOSPC;
                  else
                    errno = EINVAL;
                  return (-1);
                }
            }
        }
      else if (driver_type_used == IPMI_DEVICE_SSIF)
        {
          if ((ret = ipmi_ssif_cmd (ssif_ctx,
                                    IPMI_BMC_IPMB_LUN_BMC,
                                    netfn,
                                    cmd_rq,
                                    cmd_rs)) < 0)
            {
              if (ipmi_ssif_ctx_errnum (ssif_ctx) != IPMI_SSIF_ERR_BUSY)
                {
                  _bmclog ("%s: ipmi_ssif_cmd: %s",
                           str,
                           ipmi_ssif_ctx_strerror (ipmi_ssif_ctx_errnum (ssif_ctx)));
                  if (ipmi_ssif_ctx_errnum (ssif_ctx) == IPMI_SSIF_ERR_PARAMETERS)
                    errno = EINVAL;
                  else if (ipmi_ssif_ctx_errnum (ssif_ctx) == IPMI_SSIF_ERR_PERMISSION)
                    errno = EPERM;
                  else if (ipmi_ssif_ctx_errnum (ssif_ctx) == IPMI_SSIF_ERR_OUT_OF_MEMORY)
                    errno = ENOMEM;
                  else if (ipmi_ssif_ctx_errnum (ssif_ctx) == IPMI_SSIF_ERR_IO_NOT_INITIALIZED)
                    errno = EIO;
                  else if (ipmi_ssif_ctx_errnum (ssif_ctx) == IPMI_SSIF_ERR_OVERFLOW)
                    errno = ENOSPC;
                  else
                    errno = EINVAL;
                  return (-1);
                }
            }
        }
      else if (driver_type_used == IPMI_DEVICE_OPENIPMI)
        {
          if ((ret = ipmi_openipmi_cmd (openipmi_ctx,
                                        IPMI_BMC_IPMB_LUN_BMC,
                                        netfn,
                                        cmd_rq,
                                        cmd_rs)) < 0)
            {
              _bmclog ("%s: ipmi_openipmi_cmd: %s",
                       str,
                       ipmi_openipmi_ctx_strerror (ipmi_openipmi_ctx_errnum (openipmi_ctx)));
              if (ipmi_openipmi_ctx_errnum (openipmi_ctx) == IPMI_OPENIPMI_ERR_PARAMETERS)
                errno = EINVAL;
              else if (ipmi_openipmi_ctx_errnum (openipmi_ctx) == IPMI_OPENIPMI_ERR_PERMISSION)
                errno = EPERM;
              else if (ipmi_openipmi_ctx_errnum (openipmi_ctx) == IPMI_OPENIPMI_ERR_OUT_OF_MEMORY)
                errno = ENOMEM;
              else if (ipmi_openipmi_ctx_errnum (openipmi_ctx) == IPMI_OPENIPMI_ERR_IO_NOT_INITIALIZED)
                errno = EIO;
              else
                errno = EINVAL;
              return (-1);
            }
        }
      else if (driver_type_used == IPMI_DEVICE_SUNBMC)
        {
          if ((ret = ipmi_sunbmc_cmd (sunbmc_ctx,
                                      IPMI_BMC_IPMB_LUN_BMC,
                                      netfn,
                                      cmd_rq,
                                      cmd_rs)) < 0)
            {
              _bmclog ("%s: ipmi_sunbmc_cmd: %s",
                       str,
                       ipmi_sunbmc_ctx_strerror (ipmi_sunbmc_ctx_errnum (sunbmc_ctx)));
              if (ipmi_sunbmc_ctx_errnum (sunbmc_ctx) == IPMI_SUNBMC_ERR_PARAMETERS)
                errno = EINVAL;
              else if (ipmi_sunbmc_ctx_errnum (sunbmc_ctx) == IPMI_SUNBMC_ERR_PERMISSION)
                errno = EPERM;
              else if (ipmi_sunbmc_ctx_errnum (sunbmc_ctx) == IPMI_SUNBMC_ERR_OUT_OF_MEMORY)
                errno = ENOMEM;
              else if (ipmi_sunbmc_ctx_errnum (sunbmc_ctx) == IPMI_SUNBMC_ERR_IO_NOT_INITIALIZED)
                errno = EIO;
              else
                errno = EINVAL;
              return (-1);
            }
        }

      if (ret < 0)
        {
          if (retry_count >= retry_attempt)
            {
              _bmclog ("%s: BMC busy: retry_wait_time=%d, retry_attempt=%d",
                       str,
                       retry_wait_time,
                       retry_attempt);
              errno = EBUSY;
              return (-1);
            }

          if (cmd_args.common.debug)
            {
              fprintf (stderr, "%s: BMC busy\n", str);
              _bmclog ("%s: BMC busy", str);
            }

          _sleep (retry_wait_time);
          retry_count++;
        }
      else
        break;
    }

  if (cmd_args.common.debug)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                     DEBUG_UTIL_DIRECTION_REQUEST,
                     netfn,
                     cmd,
		     0,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_obj_dump (STDERR_FILENO, NULL, hdrbuf, NULL, cmd_rs) < 0)
        _bmclog ("%s: ipmi_obj_dump: %s", str, strerror (errno));
    }

  BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs, "comp_code", &comp_code, str);

  if (comp_code != IPMI_COMP_CODE_COMMAND_SUCCESS)
    _bmclog ("%s: cmd error: %Xh", str, comp_code);

  return (comp_code);

 cleanup:
  return (-1);
}

static int
_reset_watchdog_timer_cmd (int retry_wait_time, int retry_attempt)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;

  if (!(cmd_rq = fiid_obj_create (tmpl_cmd_reset_watchdog_timer_rq)))
    {
      _bmclog ("_reset_watchdog_timer_cmd: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_reset_watchdog_timer_rs)))
    {
      _bmclog ("_reset_watchdog_timer_cmd: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (fill_cmd_reset_watchdog_timer (cmd_rq) < 0)
    {
      _bmclog ("_reset_watchdog_timer_cmd: "
               "fill_cmd_reset_watchdog_timer: %s", strerror (errno));
      goto cleanup;
    }

  rv = _cmd ("Reset Cmd",
             retry_wait_time,
             retry_attempt,
             IPMI_NET_FN_APP_RQ,
             IPMI_CMD_RESET_WATCHDOG_TIMER,
             cmd_rq,
             cmd_rs);

 cleanup:
  fiid_obj_destroy (cmd_rq);
  fiid_obj_destroy (cmd_rs);
  return (rv);
}

static int
_set_watchdog_timer_cmd (int retry_wait_time,
                         int retry_attempt,
                         uint8_t timer_use,
                         uint8_t stop_timer,
                         uint8_t log,
                         uint8_t timeout_action,
                         uint8_t pre_timeout_interrupt,
                         uint8_t pre_timeout_interval,
                         uint8_t timer_use_expiration_flag_bios_frb2,
                         uint8_t timer_use_expiration_flag_bios_post,
                         uint8_t timer_use_expiration_flag_os_load,
                         uint8_t timer_use_expiration_flag_sms_os,
                         uint8_t timer_use_expiration_flag_oem,
                         uint16_t initial_countdown_seconds)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  uint16_t initial_countdown_chunks;
  int rv = -1;

  /* IPMI specifies timeout in 100 millisecond chunks */
  initial_countdown_chunks = initial_countdown_seconds * 10;

  if (!(cmd_rq = fiid_obj_create (tmpl_cmd_set_watchdog_timer_rq)))
    {
      _bmclog ("_set_watchdog_timer_cmd: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_set_watchdog_timer_rs)))
    {
      _bmclog ("_set_watchdog_timer_cmd: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (fill_cmd_set_watchdog_timer (timer_use,
                                   stop_timer,
                                   log,
                                   timeout_action,
                                   pre_timeout_interrupt,
                                   pre_timeout_interval,
                                   timer_use_expiration_flag_bios_frb2,
                                   timer_use_expiration_flag_bios_post,
                                   timer_use_expiration_flag_os_load,
                                   timer_use_expiration_flag_sms_os,
                                   timer_use_expiration_flag_oem,
                                   initial_countdown_chunks,
                                   cmd_rq) < 0)
    {
      _bmclog ("_set_watchdog_timer_cmd: fill_cmd_set_watchdog_timer: %s",
               strerror (errno));
      goto cleanup;
    }

  rv = _cmd ("Set Cmd",
             retry_wait_time,
             retry_attempt,
             IPMI_NET_FN_APP_RQ,
             IPMI_CMD_SET_WATCHDOG_TIMER,
             cmd_rq,
             cmd_rs);

 cleanup:
  fiid_obj_destroy (cmd_rq);
  fiid_obj_destroy (cmd_rs);
  return (rv);
}

static int
_get_watchdog_timer_cmd (int retry_wait_time,
                         int retry_attempt,
                         uint8_t *timer_use,
                         uint8_t *timer_state,
                         uint8_t *log,
                         uint8_t *timeout_action,
                         uint8_t *pre_timeout_interrupt,
                         uint8_t *pre_timeout_interval,
                         uint8_t *timer_use_expiration_flag_bios_frb2,
                         uint8_t *timer_use_expiration_flag_bios_post,
                         uint8_t *timer_use_expiration_flag_os_load,
                         uint8_t *timer_use_expiration_flag_sms_os,
                         uint8_t *timer_use_expiration_flag_oem,
                         uint16_t *initial_countdown_seconds,
                         uint16_t *present_countdown_seconds)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  uint64_t val, *valptr;
  int rv = -1;

  if (!(cmd_rq = fiid_obj_create (tmpl_cmd_get_watchdog_timer_rq)))
    {
      _bmclog ("_get_watchdog_timer_cmd: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_get_watchdog_timer_rs)))
    {
      _bmclog ("_get_watchdog_timer_cmd: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (fill_cmd_get_watchdog_timer (cmd_rq) < 0)
    {
      _bmclog ("_get_watchdog_timer_cmd: fill_cmd_get_watchdog_timer: %s",
               strerror (errno));
      goto cleanup;
    }

  if ((rv = _cmd ("Get Cmd",
                  retry_wait_time,
                  retry_attempt,
                  IPMI_NET_FN_APP_RQ,
                  IPMI_CMD_GET_WATCHDOG_TIMER,
                  cmd_rq,
                  cmd_rs)))
    goto cleanup;

  if (timer_use)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timer_use",
                               timer_use,
                               "_get_watchdog_timer_cmd");

  if (timer_state)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timer_state",
                               timer_state,
                               "_get_watchdog_timer_cmd");

  if (log)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "log",
                               log,
                               "_get_watchdog_timer_cmd");

  if (timeout_action)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timeout_action",
                               timeout_action,
                               "_get_watchdog_timer_cmd");

  if (pre_timeout_interrupt)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "pre_timeout_interrupt",
                               pre_timeout_interrupt,
                               "_get_watchdog_timer_cmd");

  if (pre_timeout_interval)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "pre_timeout_interval",
                               pre_timeout_interval,
                               "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_bios_frb2)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timer_use_expiration_flag.bios_frb2",
                               timer_use_expiration_flag_bios_frb2,
                               "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_bios_post)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timer_use_expiration_flag.bios_post",
                               timer_use_expiration_flag_bios_post,
                               "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_os_load)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timer_use_expiration_flag.os_load",
                               timer_use_expiration_flag_os_load,
                               "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_sms_os)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timer_use_expiration_flag.sms_os",
                               timer_use_expiration_flag_sms_os,
                               "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_oem)
    BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                               "timer_use_expiration_flag.oem",
                               timer_use_expiration_flag_oem,
                               "_get_watchdog_timer_cmd");

  if (initial_countdown_seconds)
    {
      valptr = &val;
      BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                                 "initial_countdown_value",
                                 valptr,
                                 "_get_watchdog_timer_cmd");
      *initial_countdown_seconds = val / 10;
    }

  if (present_countdown_seconds)
    {
      valptr = &val;
      BMC_WATCHDOG_FIID_OBJ_GET (cmd_rs,
                                 "present_countdown_value",
                                 valptr,
                                 "_get_watchdog_timer_cmd");
      *present_countdown_seconds = val / 10;
    }

 cleanup:
  fiid_obj_destroy (cmd_rq);
  fiid_obj_destroy (cmd_rs);
  return (rv);
}

static int
_get_channel_number (int retry_wait_time, int retry_attempt, uint8_t *channel_number)
{
  fiid_obj_t dev_id_cmd_rq = NULL;
  fiid_obj_t dev_id_cmd_rs = NULL;
  fiid_obj_t channel_info_cmd_rq = NULL;
  fiid_obj_t channel_info_cmd_rs = NULL;
  uint32_t manufacturer_id;
  uint16_t product_id;
  unsigned int i;
  int ret, rv = -1;

  assert (channel_number);

  if (!(dev_id_cmd_rq = fiid_obj_create (tmpl_cmd_get_device_id_rq)))
    {
      _bmclog ("fiid_obj_create: %s", strerror (errno));
      goto cleanup;
    }

  if (!(dev_id_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
    {
      _bmclog ("fiid_obj_create: %s", strerror (errno));
      goto cleanup;
    }

  if (fill_cmd_get_device_id (dev_id_cmd_rq) < 0)
    {
      _bmclog ("fill_cmd_get_device_id: %s", strerror (errno));
      goto cleanup;
    }

  if ((ret = _cmd ("Get Device Id Cmd",
                   retry_wait_time,
                   retry_attempt,
                   IPMI_NET_FN_APP_RQ,
                   IPMI_CMD_GET_DEVICE_ID,
                   dev_id_cmd_rq,
                   dev_id_cmd_rs)))
    _ipmi_err_exit (IPMI_CMD_GET_DEVICE_ID,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Get Device Id Error");

  BMC_WATCHDOG_FIID_OBJ_GET (dev_id_cmd_rs,
                             "manufacturer_id.id",
                             &manufacturer_id,
                             "_get_channel_number");
  BMC_WATCHDOG_FIID_OBJ_GET (dev_id_cmd_rs,
                             "product_id",
                             &product_id,
                             "_get_channel_number");

  switch (manufacturer_id)
    {
    case IPMI_IANA_ENTERPRISE_ID_INTEL:
    case 0xB000157: /* Intel */
      switch (product_id)
        {
        case 0x1B:
          rv = 7;
          goto cleanup;
        }
    }

  if (!(channel_info_cmd_rq = fiid_obj_create (tmpl_cmd_get_channel_info_rq)))
    {
      _bmclog ("fiid_obj_create: %s", strerror (errno));
      goto cleanup;
    }

  if (!(channel_info_cmd_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      _bmclog ("fiid_obj_create: %s", strerror (errno));
      goto cleanup;
    }

  /* Channel numbers range from 0 - 7 */
  for (i = 0; i < 8; i++)
    {
      uint8_t channel_medium_type;

      if (fill_cmd_get_channel_info (i, channel_info_cmd_rq) < 0)
        {
          _bmclog ("fill_cmd_get_channel_info: %s", strerror (errno));
          continue;
        }

      if (_cmd ("Get Channel Info Cmd",
                retry_wait_time,
                retry_attempt,
                IPMI_NET_FN_APP_RQ,
                IPMI_CMD_GET_CHANNEL_INFO_COMMAND,
                channel_info_cmd_rq,
                channel_info_cmd_rs))
        continue;

      BMC_WATCHDOG_FIID_OBJ_GET (channel_info_cmd_rs,
                                 "channel_medium_type",
                                 &channel_medium_type,
                                 "_get_channel_number");

      if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
        {
          uint8_t actual_channel_number;

          BMC_WATCHDOG_FIID_OBJ_GET (channel_info_cmd_rs,
                                     "actual_channel_number",
                                     &actual_channel_number,
                                     "_get_channel_number");

          (*channel_number) = actual_channel_number;

          rv = 0;
          goto cleanup;
        }
    }

  errno = EINVAL;
 cleanup:
  fiid_obj_destroy (dev_id_cmd_rq);
  fiid_obj_destroy (dev_id_cmd_rs);
  fiid_obj_destroy (channel_info_cmd_rq);
  fiid_obj_destroy (channel_info_cmd_rs);
  return (rv);
}

static int
_suspend_bmc_arps_cmd (int retry_wait_time,
                       int retry_attempt,
                       uint8_t gratuitous_arp,
                       uint8_t arp_response)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;
  uint8_t channel_number = 0;

  if (!(cmd_rq = fiid_obj_create (tmpl_cmd_suspend_bmc_arps_rq)))
    {
      _bmclog ("_suspend_bmc_arps: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_suspend_bmc_arps_rs)))
    {
      _bmclog ("_suspend_bmc_arps: fiid_obj_create: %s",
               strerror (errno));
      goto cleanup;
    }

  if (_get_channel_number (retry_wait_time,
                           retry_attempt,
                           &channel_number) < 0)
    goto cleanup;

  if (fill_cmd_suspend_bmc_arps (channel_number,
                                 gratuitous_arp,
                                 arp_response,
                                 cmd_rq) < 0)
    {
      _bmclog ("_suspend_bmc_arps_cmd: fill_cmd_suspend_bmc_arps: %s",
               strerror (errno));
      goto cleanup;
    }

  rv = _cmd ("Suspend Cmd",
             retry_wait_time,
             retry_attempt,
             IPMI_NET_FN_TRANSPORT_RQ,
             IPMI_CMD_SUSPEND_BMC_ARPS,
             cmd_rq,
             cmd_rs);

 cleanup:
  fiid_obj_destroy (cmd_rq);
  fiid_obj_destroy (cmd_rs);
  return (rv);
}

static void
_set_cmd (void)
{
  uint8_t timer_use, stop_timer, timer_state, log, timeout_action,
    pre_timeout_interrupt, pre_timeout_interval;
  uint16_t initial_countdown_seconds;
  int ret;

  if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      &timer_use,
                                      &timer_state,
                                      &log,
                                      &timeout_action,
                                      &pre_timeout_interrupt,
                                      &pre_timeout_interval,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      &initial_countdown_seconds,
                                      NULL)))
    _ipmi_err_exit (IPMI_CMD_GET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Get Watchdog Timer Error");

  if ((!timer_state && cmd_args.start_if_stopped)
      || (timer_state && cmd_args.reset_if_running))
    {
      if ((ret = _reset_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                            BMC_WATCHDOG_RETRY_ATTEMPT)))
        _ipmi_err_exit (IPMI_CMD_RESET_WATCHDOG_TIMER,
                        IPMI_NET_FN_APP_RQ,
                        ret,
                        "Reset Watchdog Timer Error");
    }

  timer_use = (cmd_args.timer_use) ? cmd_args.timer_use_arg : timer_use;
  stop_timer = (cmd_args.stop_timer) ? cmd_args.stop_timer_arg : timer_state;
  log = (cmd_args.log) ? cmd_args.log_arg : log;
  timeout_action = (cmd_args.timeout_action) ?
    cmd_args.timeout_action_arg : timeout_action;
  pre_timeout_interrupt = (cmd_args.pre_timeout_interrupt) ?
    cmd_args.pre_timeout_interrupt_arg : pre_timeout_interrupt;
  pre_timeout_interval = (cmd_args.pre_timeout_interval) ?
    cmd_args.pre_timeout_interval_arg : pre_timeout_interval;
  initial_countdown_seconds = (cmd_args.initial_countdown_seconds) ?
    cmd_args.initial_countdown_seconds_arg : initial_countdown_seconds;

  if ((pre_timeout_interrupt != IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_NONE)
      && (pre_timeout_interval > initial_countdown_seconds))
    _err_exit ("pre-timeout interval greater than initial countdown seconds");

  if ((ret = _set_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      timer_use, stop_timer,
                                      log,
                                      timeout_action,
                                      pre_timeout_interrupt,
                                      pre_timeout_interval,
                                      (cmd_args.clear_bios_frb2) ? 1 : 0,
                                      (cmd_args.clear_bios_post) ? 1 : 0,
                                      (cmd_args.clear_os_load) ? 1 : 0,
                                      (cmd_args.clear_sms_os) ? 1 : 0,
                                      (cmd_args.clear_oem) ? 1 : 0,
                                      initial_countdown_seconds)))
    _ipmi_err_exit (IPMI_CMD_SET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Set Watchdog Timer Error");

  if (cmd_args.start_after_set || cmd_args.reset_after_set)
    {
      if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                          BMC_WATCHDOG_RETRY_ATTEMPT,
                                          NULL,
                                          &timer_state,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL)))
        _ipmi_err_exit (IPMI_CMD_GET_WATCHDOG_TIMER,
                        IPMI_NET_FN_APP_RQ,
                        ret,
                        "Get Watchdog Timer Error");

      if ((!timer_state && cmd_args.start_after_set)
          || (timer_state && cmd_args.reset_after_set))
        {
          if ((ret = _reset_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                                BMC_WATCHDOG_RETRY_ATTEMPT)))
            _ipmi_err_exit (IPMI_CMD_RESET_WATCHDOG_TIMER,
                            IPMI_NET_FN_APP_RQ,
                            ret,
                            "Reset Watchdog Timer Error");
        }
    }

  return;
}

static char *
_log_str (uint8_t log)
{
  switch (log)
    {
    case IPMI_BMC_WATCHDOG_TIMER_LOG_ENABLE:
      return "Enabled";
    case IPMI_BMC_WATCHDOG_TIMER_LOG_DISABLE:
      return "Disabled";
    default:
      return "Internal Error, Unknown Log Value";
    }

  return (NULL);		/* NOT REACHED */
}

static char *
_timer_state_str (uint8_t timer_state)
{
  switch (timer_state)
    {
    case IPMI_BMC_WATCHDOG_TIMER_TIMER_STATE_RUNNING:
      return "Running";
    case IPMI_BMC_WATCHDOG_TIMER_TIMER_STATE_STOPPED:
      return "Stopped";
    default:
      return "Internal Error, Unknown Stop Timer Value";
    }

  return (NULL);		/* NOT REACHED */
}

static char *
_timer_use_str (uint8_t timer_use)
{
  switch (timer_use)
    {
    case IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_BIOS_FRB2:
      return "BIOS FRB2";
    case IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_BIOS_POST:
      return "BIOS POST";
    case IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_OS_LOAD:
      return "OS LOAD";
    case IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_SMS_OS:
      return "SMS/OS";
    case IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_OEM:
      return "OEM";
    default:
      return "Reserved";
    }

  return (NULL);		/* NOT REACHED */
}

static char *
_pre_timeout_interrupt_str (uint8_t pre_timeout_interrupt)
{
  switch (pre_timeout_interrupt)
    {
    case IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_NONE:
      return "None";
    case IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_SMI:
      return "SMI";
    case IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_NMI:
      return "NMI / Diagnostic Interrupt";
    case IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_MESSAGING_INTERRUPT:
      return "Messaging Interrupt";
    default:
      return "Reserved";
    }
  
  return (NULL);		/* NOT REACHED */
}

static char *
_timeout_action_str (uint8_t timeout_action)
{
  switch (timeout_action)
    {
    case IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_NO_ACTION:
      return "None";
    case IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_HARD_RESET:
      return "Hard Reset";
    case IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_POWER_DOWN:
      return "Power Down";
    case IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_POWER_CYCLE:
      return "Power Cycle";
    default:
      return "Reserved";
    }

  return (NULL);		/* NOT REACHED */
}

static void
_get_cmd (void)
{
  uint8_t timer_use, timer_state, log, timeout_action, pre_timeout_interrupt,
    pre_timeout_interval, timer_use_expiration_flag_bios_frb2,
    timer_use_expiration_flag_bios_post, timer_use_expiration_flag_os_load,
    timer_use_expiration_flag_sms_os, timer_use_expiration_flag_oem;
  uint16_t initial_countdown_seconds, present_countdown_seconds;
  int ret;

  if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      &timer_use,
                                      &timer_state,
                                      &log,
                                      &timeout_action,
                                      &pre_timeout_interrupt,
                                      &pre_timeout_interval,
                                      &timer_use_expiration_flag_bios_frb2,
                                      &timer_use_expiration_flag_bios_post,
                                      &timer_use_expiration_flag_os_load,
                                      &timer_use_expiration_flag_sms_os,
                                      &timer_use_expiration_flag_oem,
                                      &initial_countdown_seconds,
                                      &present_countdown_seconds)))
    _ipmi_err_exit (IPMI_CMD_GET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Get Watchdog Timer Error");

  printf ("Timer Use:                   %s\n",
          _timer_use_str (timer_use));
  printf ("Timer:                       %s\n",
          _timer_state_str (timer_state));
  printf ("Logging:                     %s\n",
          _log_str (log));
  printf ("Timeout Action:              %s\n",
          _timeout_action_str (timeout_action));
  printf ("Pre-Timeout Interrupt:       %s\n",
          _pre_timeout_interrupt_str (pre_timeout_interrupt));
  printf ("Pre-Timeout Interval:        %d seconds\n",
          pre_timeout_interval);
  printf ("Timer Use BIOS FRB2 Flag:    %s\n",
          (timer_use_expiration_flag_bios_frb2) ? "Set" : "Clear");
  printf ("Timer Use BIOS POST Flag:    %s\n",
          (timer_use_expiration_flag_bios_post) ? "Set" : "Clear");
  printf ("Timer Use BIOS OS Load Flag: %s\n",
          (timer_use_expiration_flag_os_load) ? "Set" : "Clear");
  printf ("Timer Use BIOS SMS/OS Flag:  %s\n",
          (timer_use_expiration_flag_sms_os) ? "Set" : "Clear");
  printf ("Timer Use BIOS OEM Flag:     %s\n",
          (timer_use_expiration_flag_oem) ? "Set" : "Clear");
  printf ("Initial Countdown:           %d seconds\n",
          initial_countdown_seconds);
  printf ("Current Countdown:           %d seconds\n",
          present_countdown_seconds);
}

static void
_reset_cmd (void)
{
  int ret;

  if ((ret = _reset_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                        BMC_WATCHDOG_RETRY_ATTEMPT)))
    _ipmi_err_exit (IPMI_CMD_RESET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Reset Watchdog Timer Error");
}

static void
_start_cmd (void)
{
  int ret;
  uint8_t timer_state;

  if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      NULL,
                                      &timer_state,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL)))
    _ipmi_err_exit (IPMI_CMD_GET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Get Watchdog Timer Error");

  if (!timer_state)
    {
      if ((ret = _reset_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                            BMC_WATCHDOG_RETRY_ATTEMPT)))
        _ipmi_err_exit (IPMI_CMD_RESET_WATCHDOG_TIMER,
                        IPMI_NET_FN_APP_RQ,
                        ret,
                        "Reset Watchdog Timer Error");
    }


  if (cmd_args.gratuitous_arp || cmd_args.arp_response)
    {
      uint8_t gratuitous_arp, arp_response;

      if (cmd_args.gratuitous_arp)
        gratuitous_arp = cmd_args.gratuitous_arp_arg;
      else
        gratuitous_arp = IPMI_BMC_GENERATED_GRATUITOUS_ARP_DO_NOT_SUSPEND;

      if (cmd_args.arp_response)
        arp_response = cmd_args.arp_response_arg;
      else
        arp_response = IPMI_BMC_GENERATED_ARP_RESPONSE_DO_NOT_SUSPEND;

      if ((ret = _suspend_bmc_arps_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                        BMC_WATCHDOG_RETRY_ATTEMPT,
                                        gratuitous_arp,
                                        arp_response)))
        _ipmi_err_exit (IPMI_CMD_SUSPEND_BMC_ARPS,
                        IPMI_NET_FN_TRANSPORT_RQ,
                        ret,
                        "Suspend BMC ARPs Error");
    }
}

static void
_stop_cmd (void)
{
  uint8_t timer_use, log, timeout_action, pre_timeout_interrupt,
    pre_timeout_interval;
  uint16_t initial_countdown_seconds;
  int ret;

  if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      &timer_use,
                                      NULL,
                                      &log,
                                      &timeout_action,
                                      &pre_timeout_interrupt,
                                      &pre_timeout_interval,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      &initial_countdown_seconds,
                                      NULL)))
    _ipmi_err_exit (IPMI_CMD_GET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Get Watchdog Timer Error");

  if ((ret = _set_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      timer_use,
                                      IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_ENABLE,
                                      log,
                                      timeout_action,
                                      pre_timeout_interrupt,
                                      pre_timeout_interval,
                                      0,
                                      0,
                                      0,
                                      0,
                                      0,
                                      initial_countdown_seconds)))
    _ipmi_err_exit (IPMI_CMD_SET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Set Watchdog Timer Error");
}

static void
_clear_cmd (void)
{
  int ret;
  uint8_t timer_use;

  /* Timer use cannot be NONE, so use whatever was there before */

  if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      &timer_use,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL,
                                      NULL)))
    _ipmi_err_exit (IPMI_CMD_GET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Get Watchdog Timer Error");

  if ((ret = _set_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                      BMC_WATCHDOG_RETRY_ATTEMPT,
                                      timer_use,
                                      IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_ENABLE,
                                      IPMI_BMC_WATCHDOG_TIMER_LOG_DISABLE,
                                      IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_NO_ACTION,
                                      IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_NONE,
                                      0,
                                      0,
                                      0,
                                      0,
                                      0,
                                      0,
                                      0)))
    _ipmi_err_exit (IPMI_CMD_SET_WATCHDOG_TIMER,
                    IPMI_NET_FN_APP_RQ,
                    ret,
                    "Set Watchdog Timer Error");
}

static void
_daemon_init ()
{
  /* Based on code in Unix network programming by R. Stevens */

  /* Run in foreground if debugging */
  if (!cmd_args.common.debug)
    {
      unsigned int i;
      pid_t pid;
      FILE *pidfile;
      int fds[2];

      if ( pipe(fds) < 0 )
        _err_exit ("pipe: %s", strerror (errno));

      if ( (pidfile = fopen(BMC_WATCHDOG_PIDFILE, "w")) == NULL )
        _err_exit ("fopen: %s", strerror (errno));

      if ((pid = fork ()) < 0)
        _err_exit ("fork: %s", strerror (errno));
      if (pid)
        {
          /* parent terminates */
          char buf;
          read(fds[0], &buf, 1);
          close(fds[1]);
          close(fds[0]);
          exit (0);
        }

      setsid ();

      if (signal (SIGHUP, SIG_IGN) == SIG_ERR)
        _err_exit ("signal: %s", strerror (errno));

      if ((pid = fork ()) < 0)
        _err_exit ("fork: %s", strerror (errno));
      if (pid) {
        /* write the 2nd child PID to the pidfile */
        fprintf(pidfile, "%u\n", pid);
        fclose(pidfile);

        exit (0);                   /* 1st child terminates */
      }

      if (chdir ("/") < 0)
        _err_exit ("chdir: %s", strerror (errno));

      umask (0);

      write(fds[1], "a", 1);
      close(fds[1]);
      close(fds[0]);
      for (i = 0; i < 64; i++)
        close (i);
    }

  _init_bmc_watchdog (LOG_DAEMON, 0);
}

static void
_daemon_cmd_error_exit (char *str, int ret)
{
  assert (str);

  if (ret < 0)
    {
      if (errno == EAGAIN || errno == EBUSY)
        _bmclog ("%s: BMC Busy", str);
      else if (errno == EIDRM)
        {
          /* Assume semaphore was deleted for some strange reason */
          _bmclog ("%s: semaphore deleted: %s", str, strerror (errno));

          if (_init_ipmi () < 0)
            exit (1);
        }
      else
        {
          _bmclog ("%s Error: %s", str, strerror (errno));
          exit (1);
        }
    }
  else
    _bmclog ("%s IPMI Error: %Xh", str, ret);
  _sleep (BMC_WATCHDOG_RETRY_WAIT_TIME);
}

static void
_daemon_setup (void)
{
  uint8_t timer_use, timer_state, log, timeout_action, pre_timeout_interrupt,
    pre_timeout_interval;
  uint32_t reset_period = BMC_WATCHDOG_RESET_PERIOD_DEFAULT;
  uint16_t initial_countdown_seconds;
  int ret;

  while (1)
    {
      if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                          BMC_WATCHDOG_RETRY_ATTEMPT,
                                          &timer_use,
                                          &timer_state,
                                          &log,
                                          &timeout_action,
                                          &pre_timeout_interrupt,
                                          &pre_timeout_interval,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          &initial_countdown_seconds,
                                          NULL)))
        {
          _daemon_cmd_error_exit ("Get Watchdog Timer", ret);
          continue;
        }
      break;
    }

  if (timer_state == IPMI_BMC_WATCHDOG_TIMER_TIMER_STATE_RUNNING)
    {
      _bmclog ("Error: watchdog timer must be stopped before running daemon");
      exit (1);
    }

  timer_use = (cmd_args.timer_use) ? cmd_args.timer_use_arg : timer_use;
  log = (cmd_args.log) ? cmd_args.log_arg : log;
  timeout_action = (cmd_args.timeout_action) ?
    cmd_args.timeout_action_arg : timeout_action;
  pre_timeout_interrupt = (cmd_args.pre_timeout_interrupt) ?
    cmd_args.pre_timeout_interrupt_arg : pre_timeout_interrupt;
  pre_timeout_interval = (cmd_args.pre_timeout_interval) ?
    cmd_args.pre_timeout_interval_arg : pre_timeout_interval;
  initial_countdown_seconds = (cmd_args.initial_countdown_seconds) ?
    cmd_args.initial_countdown_seconds_arg : initial_countdown_seconds;

  if ((pre_timeout_interrupt != IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_NONE)
      && (pre_timeout_interval > initial_countdown_seconds))
    {
      _bmclog ("Error: pre-timeout interval greater than initial countdown seconds");
      exit (1);
    }
  if (cmd_args.reset_period)
    reset_period = cmd_args.reset_period_arg;
  if (reset_period > initial_countdown_seconds)
    {
      _bmclog ("Error: reset-period interval greater than initial countdown seconds");
      exit (1);
    }

  while (1)
    {
      if ((ret = _set_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                          BMC_WATCHDOG_RETRY_ATTEMPT,
                                          timer_use,
                                          IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_ENABLE,
                                          log,
                                          timeout_action,
                                          pre_timeout_interrupt,
                                          pre_timeout_interval,
                                          (cmd_args.clear_bios_frb2) ? 1 : 0,
                                          (cmd_args.clear_bios_post) ? 1 : 0,
                                          (cmd_args.clear_os_load) ? 1 : 0,
                                          (cmd_args.clear_sms_os) ? 1 : 0,
                                          (cmd_args.clear_oem) ? 1 : 0,
                                          initial_countdown_seconds)))
        {
          _daemon_cmd_error_exit ("Set Watchdog Timer", ret);
          continue;
        }
      break;
    }

  /* Must start watchdog timer before entering loop */
  while (1)
    {
      if ((ret = _reset_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                            BMC_WATCHDOG_RETRY_ATTEMPT)))
        {
          _daemon_cmd_error_exit ("Reset Watchdog Timer", ret);
          continue;
        }
      break;
    }

  if (cmd_args.gratuitous_arp || cmd_args.arp_response)
    {
      uint8_t gratuitous_arp, arp_response;

      if (cmd_args.gratuitous_arp)
        gratuitous_arp = cmd_args.gratuitous_arp_arg;
      else
        gratuitous_arp = IPMI_BMC_GENERATED_GRATUITOUS_ARP_DO_NOT_SUSPEND;

      if (cmd_args.arp_response)
        arp_response = cmd_args.gratuitous_arp_arg;
      else
        arp_response = IPMI_BMC_GENERATED_ARP_RESPONSE_DO_NOT_SUSPEND;

      while (1)
        {
          if ((ret = _suspend_bmc_arps_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                            BMC_WATCHDOG_RETRY_ATTEMPT,
                                            gratuitous_arp,
                                            arp_response)))
            {
              _daemon_cmd_error_exit ("Suspend BMC ARPs", ret);
              continue;
            }
          break;
        }
    }

  return;
}

static void
_signal_handler (int sig)
{
  shutdown_flag = 0;
}

static void
_daemon_cmd_error_noexit (char *str, int ret)
{
  assert (str);

  if (ret < 0)
    {
      if (errno == EAGAIN || errno == EBUSY)
        _bmclog ("%s Error: BMC Busy", str);
      else if (errno == EIDRM)
        {
          /* Assume semaphore was deleted for some strange reason */
          _bmclog ("%s: semaphore deleted: %s", str, strerror (errno));
          _init_ipmi ();
        }
      else
        _bmclog ("%s Error: %s", str, strerror (errno));
    }
  else
    _bmclog ("%s IPMI Error: %Xh", str, ret);
}

static void
_daemon_cmd (void)
{
  uint32_t reset_period = BMC_WATCHDOG_RESET_PERIOD_DEFAULT;
  uint8_t timer_use, timer_state, log, timeout_action, pre_timeout_interrupt,
    pre_timeout_interval;
  uint16_t initial_countdown_seconds;
  uint16_t previous_present_countdown_seconds = 0;
  uint16_t present_countdown_seconds;
  int retry_wait_time, retry_attempt;
  int ret;

  _daemon_init ();

  _daemon_setup ();

  if (cmd_args.reset_period)
    reset_period = cmd_args.reset_period_arg;

  if (signal (SIGTERM, _signal_handler) == SIG_ERR)
    {
      _bmclog ("signal: %s", strerror (errno));
      exit (1);
    }
  if (signal (SIGINT, _signal_handler) == SIG_ERR)
    {
      _bmclog ("signal: %s", strerror (errno));
      exit (1);
    }
  if (signal (SIGQUIT, _signal_handler) == SIG_ERR)
    {
      _bmclog ("signal: %s", strerror (errno));
      exit (1);
    }

  _bmclog ("starting bmc-watchdog daemon");

  retry_wait_time = BMC_WATCHDOG_RETRY_WAIT_TIME;
  retry_attempt = BMC_WATCHDOG_RETRY_ATTEMPT;

  if ((retry_wait_time * retry_attempt) > reset_period)
    {
      retry_wait_time = 0;
      retry_attempt = 0;
    }
  else if (reset_period > retry_wait_time
           && reset_period < (retry_wait_time * retry_attempt))
    retry_attempt = reset_period/retry_wait_time;

  /* IPMI Workaround
   *
   * Discovered on Sun x4100M2 and x4200M2
   *
   * If implementing the IGNORE_STATE_FLAG workaround flag below, we
   * need to sleep a little bit to make sure the BMC timer has really
   * started.
   *
   * From 27.7 "Internal delays in the BMC may require software to
   * delay up to 100 ms before seeing the countdown value change and
   * be reflected in the Get Watchdog Timer command".
   */
  if (cmd_args.common.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG)
    _sleep (1);

  while (shutdown_flag)
    {
      struct timeval start_tv, end_tv;
      int timeval_bad = 0;

      if (gettimeofday (&start_tv, NULL) < 0)
        {
          _bmclog ("gettimeofday: %s", strerror (errno));
          timeval_bad++;
        }

      if ((ret = _get_watchdog_timer_cmd (retry_wait_time,
                                          retry_attempt,
                                          NULL,
                                          &timer_state,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          &present_countdown_seconds)))
        {
          _daemon_cmd_error_noexit ("Get Watchdog Timer", ret);
          goto sleep_now;
        }

      /* IPMI Workaround
       *
       * Discovered on Sun x4100M2 and x4200M2
       *
       * On some BMCs, the timer state flag is not functional.  Therefore,
       * to have an operational BMC watchdog, it must function without it.
       * We instead look to see if the timer is changing.
       */
      if (cmd_args.common.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG)
        {
          if (previous_present_countdown_seconds == present_countdown_seconds)
            {
              _bmclog ("timer stopped by another process");
              goto cleanup;
            }
          previous_present_countdown_seconds = present_countdown_seconds;
        }
      else
        {
          if (timer_state == IPMI_BMC_WATCHDOG_TIMER_TIMER_STATE_STOPPED)
            {
              _bmclog ("timer stopped by another process");
              goto cleanup;
            }
        }

      if ((ret = _reset_watchdog_timer_cmd (retry_wait_time,
                                            retry_attempt)))
        {
          _daemon_cmd_error_noexit ("Reset Watchdog Timer", ret);
          goto sleep_now;
        }

      /* IPMI Workaround
       *
       * Discovered on Sun x4100M2 and x4200M2
       *
       * If implementing the IGNORE_STATE_FLAG workaround flag above,
       * we need to reset the previous_present_countdown_seconds to
       * what it is after the timer reset.
       */
      if (cmd_args.common.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG)
        {
          /* From 27.7 "Internal delays in the BMC may require software to
           * delay up to 100 ms before seeing the countdown value change and
           * be reflected in the Get Watchdog Timer command".
           */
          _sleep (1);

          if ((ret = _get_watchdog_timer_cmd (retry_wait_time,
                                              retry_attempt,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              NULL,
                                              &present_countdown_seconds)))
            {
              _daemon_cmd_error_noexit ("Get Watchdog Timer", ret);
              goto sleep_now;
            }
          
          previous_present_countdown_seconds = present_countdown_seconds;
        }

    sleep_now:
      if (gettimeofday (&end_tv, NULL) < 0)
        {
          _bmclog ("gettimeofday: %s", strerror (errno));
          timeval_bad++;
        }

      if (timeval_bad)
        _sleep (reset_period);
      else
        {
          uint32_t adjusted_period = reset_period;

          /* Ignore micro secs, just seconds is good enough */
          if ((end_tv.tv_sec - start_tv.tv_sec) < adjusted_period)
            adjusted_period -= (end_tv.tv_sec - start_tv.tv_sec);

          _sleep (adjusted_period);
        }
    }

  /* Need to stop the timer, don't want it to keep on going.  Don't
   * give up until its shut off.
   */

  while (1)
    {
      if ((ret = _get_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                          BMC_WATCHDOG_RETRY_ATTEMPT,
                                          &timer_use,
                                          NULL,
                                          &log,
                                          &timeout_action,
                                          &pre_timeout_interrupt,
                                          &pre_timeout_interval,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
                                          &initial_countdown_seconds,
                                          NULL)))
        {
          _daemon_cmd_error_noexit ("Get Watchdog Timer", ret);
          continue;
        }
      break;
    }

  while (1)
    {
      if ((ret = _set_watchdog_timer_cmd (BMC_WATCHDOG_RETRY_WAIT_TIME,
                                          BMC_WATCHDOG_RETRY_ATTEMPT,
                                          timer_use,
                                          IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_ENABLE,
                                          log,
                                          timeout_action,
                                          pre_timeout_interrupt,
                                          pre_timeout_interval,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          initial_countdown_seconds)))
        {
          _daemon_cmd_error_noexit ("Set Watchdog Timer", ret);
          continue;
        }
      break;
    }

 cleanup:
  _bmclog ("stopping bmc-watchdog daemon");
}

int
main (int argc, char **argv)
{
  _err_init (argv[0]);

  ipmi_disable_coredump ();

  bmc_watchdog_argp_parse (argc, argv, &cmd_args);

  /* Early initialization.  Assumes its a cronjob if its not a daemon.
   * Daemon must do all initialization in daemon_init() b/c
   * daemon_init() needs to close all formerly open file descriptors.
   */
  if (!cmd_args.daemon)
    _init_bmc_watchdog (LOG_CRON, 1);

  if (cmd_args.set)
    _set_cmd ();
  else if (cmd_args.get)
    _get_cmd ();
  else if (cmd_args.reset)
    _reset_cmd ();
  else if (cmd_args.start)
    _start_cmd ();
  else if (cmd_args.stop)
    _stop_cmd ();
  else if (cmd_args.clear)
    _clear_cmd ();
  else if (cmd_args.daemon)
    _daemon_cmd ();
  else
    _err_exit ("internal error, command not set");

  ipmi_kcs_ctx_destroy (kcs_ctx);
  ipmi_ssif_ctx_destroy (ssif_ctx);
  ipmi_openipmi_ctx_destroy (openipmi_ctx);
  ipmi_sunbmc_ctx_destroy (sunbmc_ctx);
  if (logfile_fd >= 0)
    {
      if (close (logfile_fd) < 0)
        _err_exit ("close: %s", strerror (errno));
    }
  closelog ();
  exit (0);
}
