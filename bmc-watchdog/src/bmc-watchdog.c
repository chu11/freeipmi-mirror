/*****************************************************************************\
 *  $Id: bmc-watchdog.c,v 1.29.2.2 2006-01-12 22:02:43 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2004 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155913
 *  
 *  This file is part of Bmc-watchdog, a base management controller (BMC)
 *  watchdog timer management tool. For details, see http://www.llnl.gov/linux/.
 *  
 *  Bmc-Watchdog is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Bmc-Watchdog is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Bmc-Watchdog; if not, write to the Free Software Foundation, Inc.,
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#if 0  /* commented by AB */
#ifndef __FreeBSD__
#include <sys/io.h>
#endif
#endif /* commented by AB */
#include <syslog.h>
#include <assert.h>
#include <stdarg.h>
#include <signal.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <errno.h>
#include <getopt.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/select.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "freeipmi.h"

/* Pre Timeout Interval is 1 byte */
#define IPMI_WATCHDOG_PRE_TIMEOUT_INTERVAL_MIN_SECS  0
#define IPMI_WATCHDOG_PRE_TIMEOUT_INTERVAL_MAX_SECS  255

/* Countdown Seconds is 2 bytes of 100 millisecond chunks */
#define IPMI_WATCHDOG_INITIAL_COUNTDOWN_MIN_SECS     0
#define IPMI_WATCHDOG_INITIAL_COUNTDOWN_MAX_SECS     6553

#define BMC_WATCHDOG_ERR_BUFLEN                      1024
#define BMC_WATCHDOG_STR_BUFLEN                      1024
#define BMC_WATCHDOG_PKT_BUFLEN                      1024
#define BMC_WATCHDOG_RESET_PERIOD_DEFAULT            60

#define BMC_WATCHDOG_RETRY_WAIT_TIME                 1
#define BMC_WATCHDOG_RETRY_ATTEMPT                   5

#define BMC_WATCHDOG_LOGFILE                         "/var/log/freeipmi/bmc-watchdog.log"

#define _FIID_OBJ_GET(a, b, c, d, e) \
  do { \
     uint64_t val; \
     if (fiid_obj_get((a), (b), (c), &val) < 0) \
       { \
         _bmclog("%s: fiid_obj_get: %s", (e), strerror(errno)); \
         goto cleanup; \
       } \
     *d = val; \
  } while (0) 

struct cmdline_info
{
  int set;
  int get;
  int reset;
  int start;
  int stop;
  int clear;
  int daemon;
  int io_port;
  uint32_t io_port_val;
  int reg_space;
  uint8_t reg_space_val;
  char *logfile;
  int no_logging;
  int timer_use;
  uint8_t timer_use_val;
  int stop_timer;
  uint8_t stop_timer_val;
  int log;
  uint8_t log_val;
  int timeout_action;
  uint8_t timeout_action_val;
  int pre_timeout_interrupt;
  uint8_t pre_timeout_interrupt_val;
  int pre_timeout_interval;
  uint8_t pre_timeout_interval_val;
  int clear_bios_frb2;
  int clear_bios_post;
  int clear_os_load;
  int clear_sms_os;
  int clear_oem;
  int initial_countdown_seconds;
  uint32_t initial_countdown_seconds_val;
  int start_after_set;
  int reset_after_set;
  int start_if_stopped;
  int reset_if_running;
  int gratuitous_arp;
  uint8_t gratuitous_arp_val;
  int arp_response;
  uint8_t arp_response_val;
  int reset_period;
  uint32_t reset_period_val; 
#ifndef NDEBUG
 int debug;
#endif
};

/* program name */
char *err_progname = NULL;

ipmi_device_t ipmi_dev;

int cmdline_parsed = 0;
struct cmdline_info cinfo;

int shutdown_flag = 1;

int logfile_fd = -1;

static void
_syslog(int priority, const char *fmt, ...)
{
  char buffer[BMC_WATCHDOG_ERR_BUFLEN];
  va_list ap;

  assert (fmt != NULL && err_progname != NULL);

  if (cinfo.no_logging)
    return;

  va_start(ap, fmt);
  snprintf(buffer, BMC_WATCHDOG_ERR_BUFLEN, "%s: %s\n", err_progname, fmt);
  vsyslog(priority, buffer, ap);
  va_end(ap);
}

static void
_bmclog_write(void *buf, size_t count)
{
  size_t ret, left;
  char *ptr;

  ptr = buf;
  left = count;
  while (left > 0) 
    {
      if ((ret = write(logfile_fd, ptr, left)) < 0) 
        {
          if (errno == EINTR)
            continue;
          else
            {
              /* The only place we should really need to syslog */
              _syslog(LOG_ERR, "_bmcwrite: write: %s", strerror(errno));
              return;
            }
        }
      ptr += ret;
      left -= ret;
    }
}

static void
_bmclog(const char *fmt, ...)
{
  time_t t;
  struct tm *tm;
  int len;
  char buffer[BMC_WATCHDOG_ERR_BUFLEN];
  char fbuffer[BMC_WATCHDOG_ERR_BUFLEN];
  va_list ap;
  
  assert (fmt != NULL 
          && err_progname != NULL 
          && (cinfo.no_logging || logfile_fd >= 0));
  
  if (cinfo.no_logging)
    return;
  
  va_start(ap, fmt);
  t = time(NULL);
  if ((tm = localtime(&t)) == NULL)
    {
      /* Just use the value from time() */
      snprintf(buffer, BMC_WATCHDOG_ERR_BUFLEN, "%ld: %s\n", t, fmt);
    }
  else
    {
      char tbuffer[BMC_WATCHDOG_ERR_BUFLEN];
      strftime(tbuffer, BMC_WATCHDOG_ERR_BUFLEN, "[%b %d %H:%M:%S]", tm);
      snprintf(buffer, BMC_WATCHDOG_ERR_BUFLEN, "%s: %s\n", tbuffer, fmt);
    }
  
  len = vsnprintf(fbuffer, BMC_WATCHDOG_ERR_BUFLEN, buffer, ap);
  _bmclog_write(fbuffer, len);
 
  va_end(ap);
}

static void
_err_init(char *progname)
{
  char *ptr = strrchr(progname, '/');
  err_progname = (ptr == NULL) ? progname : ptr + 1;
}
            
static void
_err_exit(char *fmt, ...)
{
  char buffer[BMC_WATCHDOG_ERR_BUFLEN];
  va_list ap;
  
  assert (fmt != NULL && err_progname != NULL);
  
  va_start(ap, fmt);
  snprintf(buffer, BMC_WATCHDOG_ERR_BUFLEN, "%s: %s\n", err_progname, fmt);
  vfprintf(stderr, buffer, ap);
  va_end(ap);
  exit(1);
}

#if 0 /* commented by AB */
static int
_get_port_and_reg_space(uint32_t *port, uint8_t *reg_space)
{
  ipmi_locate_info_t locate_info;

  if ((port == NULL) || (reg_space == NULL))
    return (-1);

  assert(cmdline_parsed != 0);
  
  if (cinfo.io_port)
    *port      = cinfo.io_port_val;

  if (cinfo.reg_space)
    *reg_space = cinfo.reg_space_val;

  if (cinfo.io_port && cinfo.reg_space)
    return (0);
  
  if (ipmi_locate (IPMI_INTERFACE_KCS, &locate_info) != NULL && (locate_info.addr_space_id == IPMI_ADDRESS_SPACE_ID_SYSTEM_IO))
    {
      if (!cinfo.io_port)
        *port      = locate_info.base_addr.bmc_iobase_addr;
      if (!cinfo.reg_space)
        *reg_space = locate_info.reg_space;
      return (0);
    }

  *port      = IPMI_KCS_SMS_IO_BASE_DEFAULT;
  *reg_space = IPMI_REG_SPACE_DEFAULT;
  return (0);
}
#endif  /* commented by AB */

/* Must be called after cmdline parsed b/c user may pass in io port */
static int
_init_ipmi(void)
{
  int ret;
#if 0 /* commented by AB */
  uint32_t port=0;
  uint8_t  reg_space=0;
#endif /* commented by AB */

  assert(cmdline_parsed != 0 && err_progname != NULL);

#if 0 /* commented by AB */
  _get_port_and_reg_space (&port, &reg_space);
#endif /* commented by AB */

/*   if ((ret = ipmi_kcs_io_init(port, reg_space, IPMI_KCS_SLEEP_USECS)) < 0) */
/*     _bmclog("ipmi_kcs_io_init: %s", strerror(errno)); */
  memset(&ipmi_dev, 0, sizeof (ipmi_dev));
  if ((ret = ipmi_open_inband(&ipmi_dev, 0, IPMI_DEVICE_KCS, 
			      cinfo.io_port, cinfo.reg_space_val, 
			      0, IPMI_MODE_NONBLOCK)) < 0)
    _bmclog("ipmi_open_inband: %s", strerror(errno));
  
  return ret;
}

/* Must be called after cmdline parsed */
static void
_init_bmc_watchdog(int facility, int err_to_stderr)
{
  assert(facility == LOG_CRON || facility == LOG_DAEMON);

  openlog(err_progname, LOG_ODELAY | LOG_PID, facility);

  if (!cinfo.no_logging)
    {
      if ((logfile_fd = open(cinfo.logfile,
                             O_WRONLY | O_CREAT | O_APPEND,
                             S_IRUSR | S_IWUSR)) < 0)
        {
          if (err_to_stderr)
            _err_exit("Error opening logfile '%s': %s",
                      cinfo.logfile, strerror(errno));
          else
            _syslog(LOG_ERR, "Error opening logfile '%s': %s",
                     cinfo.logfile, strerror(errno));
          exit(1);
        }
    }

  if (_init_ipmi() < 0)
    {
      if (err_to_stderr)
        _err_exit("_init_ipmi: %s", strerror(errno));
      else
        _syslog(LOG_ERR, "_init_ipmi: %s", strerror(errno));
      exit(1);
    }
}

static void
_ipmi_err_exit(uint8_t cmd, int comp_code, char *str)
{
  char buf[BMC_WATCHDOG_ERR_BUFLEN];

  assert(str != NULL);

  if (comp_code < 0)
    {
      if (errno == EAGAIN || errno == EBUSY)
        _err_exit("%s: BMC Busy", str);
      else
        _err_exit("%s: %s", str, strerror(errno));
    }
  else
    {
      if (ipmi_strerror_r(cmd, comp_code, buf, BMC_WATCHDOG_ERR_BUFLEN) < 0)
        _err_exit("ipmi_strerror_r: %s", strerror(errno));
      _err_exit("%s: %s", str, buf);
    }
}

/* signal handlers + sleep(3) is a bad idea */
static int 
_sleep(unsigned int sleep_len)
{
  struct timeval tv;
  
  if (sleep_len == 0)
    return 0;

  tv.tv_sec = sleep_len;
  tv.tv_usec = 0;
  if (select(1, NULL, NULL, NULL, &tv) < 0)
    {
      if (errno != EINTR)
        _err_exit("select: %s", strerror(errno));
    }
  return 0;
}

static int
_cmd(char *str, int retry_wait_time, int retry_attempt, uint8_t netfn,
     fiid_obj_t cmd_rq, fiid_template_t tmpl_rq,
     fiid_obj_t cmd_rs, fiid_template_t tmpl_rs)
{
  uint64_t comp_code;
  int retry_count = 0;

  assert (str != NULL && retry_wait_time >= 0 && retry_attempt >= 0
          && (netfn == IPMI_NET_FN_APP_RQ || netfn == IPMI_NET_FN_TRANSPORT_RQ)
          && cmd_rq != NULL && tmpl_rq != NULL
          && cmd_rs != NULL && tmpl_rs != NULL);

#ifndef NDEBUG
  if (cinfo.debug)
    {
      if (fiid_obj_dump_perror(STDERR_FILENO, str, NULL, NULL, cmd_rq, tmpl_rq) < 0)
        _bmclog("%s: fiid_obj_dump_perror: %s", str, strerror(errno));
    }
#endif

  while (1)
    {
#if 0 /* commented by AB */
      if (ipmi_kcs_cmd_interruptible(IPMI_BMC_IPMB_LUN_BMC,
                                     netfn, cmd_rq, 
                                     tmpl_rq, cmd_rs, tmpl_rs) < 0)
#endif /* commented by AB */
      if (ipmi_cmd (&ipmi_dev, IPMI_BMC_IPMB_LUN_BMC, netfn, 
		   cmd_rq, tmpl_rq, cmd_rs, tmpl_rs) < 0)
        {
          if (errno != EAGAIN && errno != EBUSY)
            {
              _bmclog("%s: ipmi_kcs_cmd_interruptible: %s", 
                      str, strerror(errno));
              return -1;
            }
          else
            {
              if (retry_count >= retry_attempt)
                {
                  _bmclog("%s: ipmi_kcs_cmd_interruptible: BMC too busy: "
                          "retry_wait_time=%d, retry_attempt=%d", 
                          str, retry_wait_time, retry_attempt);
                  return -1;
                }
#ifndef NDEBUG
              if (cinfo.debug)
                {
                  fprintf(stderr, "%s: ipmi_kcs_cmd_interruptible: BMC busy\n", str);
                  _bmclog("%s: ipmi_kcs_cmd_interruptible: BMC busy", str);
                }
#endif
              _sleep(retry_wait_time);
              retry_count++;
            }
        }
      else
        break;
    }

#ifndef NDEBUG
  if (cinfo.debug)
    {
      if (fiid_obj_dump_perror(STDERR_FILENO, str, NULL, NULL, cmd_rs, tmpl_rs) < 0)
        _bmclog("%s: fiid_obj_dump_perror: %s", str, strerror(errno));
    }
#endif

  _FIID_OBJ_GET(cmd_rs, tmpl_rs, "comp_code", &comp_code, str);

  if (comp_code != IPMI_COMMAND_SUCCESS)
    _bmclog("%s: cmd error: %Xh", str, comp_code);

  return (int)comp_code;

 cleanup:
  return -1;
} 

static int 
_reset_watchdog_timer_cmd(int retry_wait_time, int retry_attempt)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  int retval = -1;

  if ((cmd_rq = fiid_obj_alloc(tmpl_cmd_reset_watchdog_timer_rq)) == NULL)
    {
      _bmclog("_reset_watchdog_timer_cmd: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  if ((cmd_rs = fiid_obj_alloc(tmpl_cmd_reset_watchdog_timer_rs)) == NULL)
    {
      _bmclog("_reset_watchdog_timer_cmd: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  if (fill_cmd_reset_watchdog_timer(cmd_rq) < 0)
    {
      _bmclog("_reset_watchdog_timer_cmd: "
              "fill_cmd_reset_watchdog_timer: %s", strerror(errno));
      goto cleanup;
    }
    
  retval = _cmd("Reset Cmd", retry_wait_time, retry_attempt, 
                IPMI_NET_FN_APP_RQ,
                cmd_rq, tmpl_cmd_reset_watchdog_timer_rq,
                cmd_rs, tmpl_cmd_reset_watchdog_timer_rs);

 cleanup:
  fiid_obj_free(cmd_rq);
  fiid_obj_free(cmd_rs);
  return retval;
}

static int
_set_watchdog_timer_cmd(int retry_wait_time, int retry_attempt,
                        uint8_t timer_use, uint8_t stop_timer, uint8_t log, 
                        uint8_t timeout_action, uint8_t pre_timeout_interrupt,
			uint8_t pre_timeout_interval,
                        uint8_t timer_use_expiration_flag_bios_frb2,
                        uint8_t timer_use_expiration_flag_bios_post, 
                        uint8_t timer_use_expiration_flag_os_load, 
                        uint8_t timer_use_expiration_flag_sms_os, 
                        uint8_t timer_use_expiration_flag_oem, 
			uint32_t initial_countdown_seconds)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  uint32_t initial_countdown_chunks;
  uint8_t *ptr, ls_byte, ms_byte;
  int retval = -1;
  
  /* IPMI specifies timeout in 100 millisecond chunks */
  initial_countdown_chunks = initial_countdown_seconds * 10;
  ptr = (uint8_t *)&initial_countdown_chunks;
#ifdef WORDS_BIGENDIAN
  ls_byte = ptr[3];
  ms_byte = ptr[2];
#else
  ls_byte = ptr[0];
  ms_byte = ptr[1];
#endif

  if ((cmd_rq = fiid_obj_alloc(tmpl_cmd_set_watchdog_timer_rq)) == NULL)
    {
      _bmclog("_set_watchdog_timer_cmd: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  if ((cmd_rs = fiid_obj_alloc(tmpl_cmd_set_watchdog_timer_rs)) == NULL)
    {
      _bmclog("_set_watchdog_timer_cmd: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  if (fill_cmd_set_watchdog_timer(timer_use, stop_timer, log, 
                                  timeout_action, pre_timeout_interrupt, 
                                  pre_timeout_interval, 
                                  timer_use_expiration_flag_bios_frb2,
                                  timer_use_expiration_flag_bios_post,
                                  timer_use_expiration_flag_os_load,
                                  timer_use_expiration_flag_sms_os,
                                  timer_use_expiration_flag_oem,
                                  ls_byte, ms_byte, cmd_rq) < 0)
    {
      _bmclog("_set_watchdog_timer_cmd: "
              "fill_cmd_set_watchdog_timer: %s", strerror(errno));
      goto cleanup;
    }
    
  retval = _cmd("Set Cmd", retry_wait_time, retry_attempt, 
                IPMI_NET_FN_APP_RQ,
                cmd_rq, tmpl_cmd_set_watchdog_timer_rq,
                cmd_rs, tmpl_cmd_set_watchdog_timer_rs);

 cleanup:
  fiid_obj_free(cmd_rq);
  fiid_obj_free(cmd_rs);
  return retval;
}

static uint32_t
_time_seconds(uint8_t ls_byte, uint32_t ms_byte)
{
  uint32_t time = 0;
  uint8_t *ptr;

  ptr = (uint8_t *)&time;
#if WORDS_BIGENDIAN
  ptr[3] = ls_byte;
  ptr[2] = ms_byte;
#else
  ptr[0] = ls_byte;
  ptr[1] = ms_byte;
#endif

  return (time/10);
}

static int
_get_watchdog_timer_cmd(int retry_wait_time, int retry_attempt,
                        uint8_t *timer_use, uint8_t *timer_state, uint8_t *log, 
			uint8_t *timeout_action, uint8_t *pre_timeout_interrupt, 
			uint8_t *pre_timeout_interval,	
                        uint8_t *timer_use_expiration_flag_bios_frb2,
                        uint8_t *timer_use_expiration_flag_bios_post, 
                        uint8_t *timer_use_expiration_flag_os_load, 
                        uint8_t *timer_use_expiration_flag_sms_os, 
                        uint8_t *timer_use_expiration_flag_oem, 
			uint32_t *initial_countdown_seconds,
			uint32_t *present_countdown_seconds)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  uint8_t ls_byte, ms_byte;
  int retval = -1;

  if ((cmd_rq = fiid_obj_alloc(tmpl_cmd_get_watchdog_timer_rq)) == NULL)
    {
      _bmclog("_get_watchdog_timer_cmd: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  if ((cmd_rs = fiid_obj_alloc(tmpl_cmd_get_watchdog_timer_rs)) == NULL)
    {
      _bmclog("_get_watchdog_timer_cmd: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  if (fill_cmd_get_watchdog_timer(cmd_rq) < 0)
    {
      _bmclog("_get_watchdog_timer_cmd: "
              "fill_cmd_get_watchdog_timer: %s", strerror(errno));
      goto cleanup;
    }

  if ((retval =_cmd("Get Cmd", retry_wait_time, retry_attempt, 
                    IPMI_NET_FN_APP_RQ,
                    cmd_rq, tmpl_cmd_get_watchdog_timer_rq,
                    cmd_rs, tmpl_cmd_get_watchdog_timer_rs)) != 0)
    goto cleanup;

  if (timer_use)
    _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, "timer_use", 
                  timer_use, "_get_watchdog_timer_cmd");

  if (timer_state)
    _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, "timer_state", 
                  timer_state, "_get_watchdog_timer_cmd");

  if (log)
    _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, "log", 
                  log, "_get_watchdog_timer_cmd");

  if (timeout_action)
    _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, "timeout_action", 
                  timeout_action, "_get_watchdog_timer_cmd");

  if (pre_timeout_interrupt)
    _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                  "pre_timeout_interrupt", pre_timeout_interrupt, 
                  "_get_watchdog_timer_cmd");

  if (pre_timeout_interval)
      _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                    "pre_timeout_interval", pre_timeout_interval, 
                    "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_bios_frb2)
      _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                    "timer_use_expiration_flag_bios_frb2", 
                    timer_use_expiration_flag_bios_frb2, 
                    "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_bios_post) 
     _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                   "timer_use_expiration_flag_bios_post", 
                   timer_use_expiration_flag_bios_post, 
                   "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_os_load) 
      _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                    "timer_use_expiration_flag_os_load", 
                    timer_use_expiration_flag_os_load, 
                    "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_sms_os) 
      _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                    "timer_use_expiration_flag_sms_os", 
                    timer_use_expiration_flag_sms_os, 
                    "_get_watchdog_timer_cmd");

  if (timer_use_expiration_flag_oem) 
     _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                   "timer_use_expiration_flag_oem", 
                   timer_use_expiration_flag_oem, 
                   "_get_watchdog_timer_cmd");

  if (initial_countdown_seconds)
     {
       _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                     "initial_countdown_value_ls_byte", 
                     &ls_byte, "_get_watchdog_timer_cmd");
       
       _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                     "initial_countdown_value_ms_byte", 
                     &ms_byte, "_get_watchdog_timer_cmd");
       *initial_countdown_seconds = _time_seconds(ls_byte, ms_byte);
     }

  if (present_countdown_seconds)
    {
      _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                    "present_countdown_value_ls_byte", 
                    &ls_byte, "_get_watchdog_timer_cmd");
      _FIID_OBJ_GET(cmd_rs, tmpl_cmd_get_watchdog_timer_rs, 
                    "present_countdown_value_ms_byte", 
                    &ms_byte, "_get_watchdog_timer_cmd");
      *present_countdown_seconds = _time_seconds(ls_byte, ms_byte);
    }

 cleanup:
  fiid_obj_free(cmd_rq);
  fiid_obj_free(cmd_rs);
  return retval;
}

static int
_suspend_bmc_arps_cmd(int retry_wait_time, int retry_attempt,
                      uint8_t gratuitous_arp,
                      uint8_t arp_response)
{
  fiid_obj_t cmd_rq = NULL;
  fiid_obj_t cmd_rs = NULL;
  int retval = -1;
  int8_t num;

  if ((cmd_rq = fiid_obj_alloc(tmpl_cmd_suspend_bmc_arps_rq)) == NULL)
    {
      _bmclog("_suspend_bmc_arps: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  if ((cmd_rs = fiid_obj_alloc(tmpl_cmd_suspend_bmc_arps_rs)) == NULL)
    {
      _bmclog("_suspend_bmc_arps: fiid_obj_alloc: %s", 
              strerror(errno));
      goto cleanup;
    }

  num = ipmi_get_channel_number2 (&ipmi_dev, IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3);
  if (num < 0)
    {
      _bmclog("_suspend_bmc_arps: ipmi_get_channel_number2: %s",
	      strerror(errno));
      goto cleanup;
    }

  if (fill_cmd_suspend_bmc_arps(num,
                                gratuitous_arp,
                                arp_response,
                                cmd_rq) < 0)
    {
      _bmclog("_suspend_bmc_arps_cmd: "
              "fill_cmd_suspend_bmc_arps: %s", strerror(errno));
      goto cleanup;
    }
    
  retval = _cmd("Suspend Cmd", retry_wait_time, retry_attempt,
                IPMI_NET_FN_TRANSPORT_RQ,
                cmd_rq, tmpl_cmd_suspend_bmc_arps_rq,
                cmd_rs, tmpl_cmd_suspend_bmc_arps_rs);

 cleanup:
  fiid_obj_free(cmd_rq);
  fiid_obj_free(cmd_rs);
  return retval;
}

static char *
_cmd_string(void)
{
  if (cinfo.get)
    return "--get";
  else if (cinfo.set)
    return "--set";
  else if (cinfo.reset)
    return "--reset";
  else if (cinfo.start)
    return "--start";
  else if (cinfo.stop)
    return "--stop";
  else if (cinfo.clear)
    return "--clear";
  else if (cinfo.daemon)
    return "--daemon";
  else
    return NULL;
}

static void 
_usage(void) 
{
  char *cmdstr = _cmd_string();

  if (cmdstr == NULL)
    {
      fprintf(stderr,
              "Usage: bmc-watchdog <COMMAND> [OPTIONS]... [COMMAND_OPTIONS]...\n\n");
      fprintf(stderr,
              "COMMANDS:\n"
              "  -s         --set                        Set BMC Watchdog Config\n"
              "  -g         --get                        Get BMC Watchdog Config\n"
              "  -r         --reset                      Reset BMC Watchdog Timer\n"
              "  -t         --start                      Start BMC Watchdog Timer\n"
              "  -y         --stop                       Stop BMC Watchdog Timer\n"
              "  -c         --clear                      Clear BMC Watchdog Config\n"
              "  -d         --daemon                     Run in Daemon Mode\n\n");
    }
  else
    fprintf(stderr,
            "Usage: bmc-watchdog %s [OPTIONS]... \n\n", cmdstr);

  fprintf(stderr,
	  "OPTIONS:\n"
          "  -h         --help                       Output help menu\n"
          "  -v         --version                    Output version\n"
	  "  -o INT     --io-port=INT                Base address for KCS SMS I/O\n"
          "  -R INT     --reg-space=INT              Base address register spacing in bytes\n"
          "  -f STRING  --logfile=STRING             Specify alternate logfile\n"
          "  -n         --no-logging                 Turn off all syslogging\n");
#ifndef NDEBUG
  fprintf(stderr,
	  "  -D         --debug                      Turn on debugging\n");
#endif
  fprintf(stderr, "\n");

  if (cinfo.set || cinfo.start || cinfo.daemon)
    fprintf(stderr, 
            "COMMAND SPECIFIC OPTIONS:\n");

  if (cinfo.set || cinfo.daemon)
    fprintf(stderr,
            "  -u INT     --timer-use=INT              Set timer use\n"
            "             %d = BIOS FRB2\n"
            "             %d = BIOS POST\n"
            "             %d = OS_LOAD\n"
            "             %d = SMS OS\n"
            "             %d = OEM\n"
            "  -m INT     --stop-timer=INT             Set Stop Timer Flag\n"
            "             %d = Stop Timer\n"
            "             %d = Don't Stop timer\n"
            "  -l INT     --log=INT                    Set Log Flag\n"
            "             %d = Enable Log\n"
            "             %d = Disable Log\n"
            "  -a INT     --timeout-action=INT         Set timeout action\n"
            "             %d = No action\n" 
            "             %d = Hard Reset\n"
            "             %d = Power Down\n"
            "             %d = Power Cycle\n"
            "  -p INT     --pre-timeout-interrupt=INT  Set pre-timeout interrupt\n"
            "             %d = None\n" 
            "             %d = SMI\n" 
            "             %d = NMI\n" 
            "             %d = Messaging Interrupt\n"
            "  -z SECS    --pre-timeout-interval=SECS  Set pre-timeout interval in seconds\n"
            "  -F         --clear-bios-frb2            Clear BIOS FRB2 Timer Use Flag\n"
            "  -P         --clear-bios-post            Clear BIOS POST Timer Use Flag\n"
            "  -L         --clear-os-load              Clear OS Load Timer Use Flag\n"
            "  -S         --clear-sms-os               Clear SMS/OS Timer Use Flag\n"
            "  -O         --clear-oem                  Clear OEM Timer Use Flag\n"
            "  -i SECS    --initial-countdown=SECS     Set initial countdown in seconds\n",
            IPMI_WATCHDOG_TIMER_USE_BIOS_FRB2,
            IPMI_WATCHDOG_TIMER_USE_BIOS_POST,
            IPMI_WATCHDOG_TIMER_USE_OS_LOAD, 
            IPMI_WATCHDOG_TIMER_USE_SMS_OS,
            IPMI_WATCHDOG_TIMER_USE_OEM,
            IPMI_WATCHDOG_STOP_TIMER_ENABLE, 
            IPMI_WATCHDOG_STOP_TIMER_DISABLE,
            IPMI_WATCHDOG_LOG_ENABLE, 
            IPMI_WATCHDOG_LOG_DISABLE,
            IPMI_WATCHDOG_TIMEOUT_ACTION_NO_ACTION,
            IPMI_WATCHDOG_TIMEOUT_ACTION_HARD_RESET,
            IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_DOWN,
            IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_CYCLE,
            IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NONE,
            IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_SMI,
            IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NMI,
            IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_MESSAGING_INTERRUPT);
  if (cinfo.set)
    fprintf(stderr,
            "  -w         --start-after-set            Start timer after set if timer is stopped\n"
            "  -x         --reset-after-set            Reset timer after set if timer is running\n"
            "  -j         --start-if-stopped           Don't set if timer is stopped, just start\n"
            "  -k         --reset-if-running           Don't set if timer is running, just reset\n");
  if (cinfo.start || cinfo.daemon)
    fprintf(stderr, 
            "  -G INT     --gratuitous-arp=INT         Suspend Gratuitous ARPs\n"
            "             %d = Suspend Gratuitous ARPs\n"
            "             %d = Do Not Suspend Gratuitous ARPs\n"
            "  -A INT     --arp-response=INT           Suspend ARP Responses\n"
            "             %d = Suspend ARP Responses\n"
            "             %d = Do Not Suspend ARP Responses\n",
            IPMI_WATCHDOG_GRATUITOUS_ARP_SUSPEND,
            IPMI_WATCHDOG_GRATUITOUS_ARP_NO_SUSPEND,
            IPMI_WATCHDOG_ARP_RESPONSE_SUSPEND,
            IPMI_WATCHDOG_ARP_RESPONSE_NO_SUSPEND);
  if (cinfo.daemon)
    fprintf(stderr, 
            "  -e SECS    --reset-period=SECS          Time interval before resetting timer\n");
              
  if (cinfo.set || cinfo.start || cinfo.daemon)
    fprintf(stderr, "\n");

  exit(1);
}

static void
_version(void)
{
  fprintf(stderr, "bmc-watchdog %s\n", VERSION);
  exit(1);
}

static void
_cmdline_default(void)
{
  memset(&cinfo, '\0', sizeof(cinfo));
  cinfo.logfile = BMC_WATCHDOG_LOGFILE;
}

static void
_cmdline_parse(int argc, char **argv)
{
  int c, count, base = 10;
  char options[100];
  char *ptr;
  int help_opt = 0;
  
#if HAVE_GETOPT_LONG
  struct option long_options[] = {
    {"help",                  0, NULL, 'h'},
    {"version",               0, NULL, 'v'},
    {"set",                   0, NULL, 's'},
    {"get",                   0, NULL, 'g'},
    {"reset",                 0, NULL, 'r'},
    {"start",                 0, NULL, 't'},
    {"stop",                  0, NULL, 'y'},
    {"clear",                 0, NULL, 'c'},
    {"daemon",                0, NULL, 'd'},
    {"io-port",               1, NULL, 'o'},
    {"reg-space",             1, NULL, 'R'},
    {"logfile",               1, NULL, 'f'},
    {"no-logging",            0, NULL, 'n'},
    {"timer-use",             1, NULL, 'u'},
    {"stop-timer",            1, NULL, 'm'},
    {"log",                   1, NULL, 'l'},
    {"timeout-action",        1, NULL, 'a'},
    {"pre-timeout-interrupt", 1, NULL, 'p'},
    {"pre-timeout-interval",  1, NULL, 'z'},
    {"clear-bios-frb2",       0, NULL, 'F'},
    {"clear-bios-post",       0, NULL, 'P'},
    {"clear-os-load",         0, NULL, 'L'},
    {"clear-sms-os",          0, NULL, 'S'},
    {"clear-oem",             0, NULL, 'O'},
    {"initial-countdown",     1, NULL, 'i'},
    {"start-after-set",       0, NULL, 'w'},
    {"reset-after-set",       0, NULL, 'x'},
    {"start-if-stopped",      0, NULL, 'j'}, 
    {"reset-if-running",      0, NULL, 'k'},
    {"gratuitous-arp",        1, NULL, 'G'},
    {"arp-response",          1, NULL, 'A'},
    {"reset-period",          1, NULL, 'e'},
#ifndef NDEBUG
    {"debug",                 0, NULL, 'D'},
#endif
    {0, 0, 0, 0}
  };
#endif /* HAVE_GETOPT_LONG */

  strcpy(options, "hvo:R:f:nsgrtycd u:m:l:a:p:z:FPLSOi:wxjkG:A:e:");
#ifndef NDEBUG
  strcat(options, "D");
#endif

  /* turn off output messages printed by getopt_long */
  opterr = 0;

#if HAVE_GETOPT_LONG
  while ((c = getopt_long(argc, argv, options, long_options, NULL)) != -1) 
#else
  while ((c = getopt(argc, argv, options)) != -1) 
#endif
    {
      switch(c) 
        {
        case 'h':
          help_opt++;
          break;
        case 'v':
          _version();
          break;
        case 's':
          cinfo.set++;
          break;
        case 'g':
          cinfo.get++;
          break;
        case 'r':
          cinfo.reset++;
          break;
        case 't':
          cinfo.start++;
          break;
        case 'y':
          cinfo.stop++;
          break;
        case 'c':
          cinfo.clear++;
          break;
        case 'd':
          cinfo.daemon++;
          break;
        case 'o':
          cinfo.io_port++;
          if (strstr(optarg, "0x") == optarg)
            base = 16;
          cinfo.io_port_val = strtol(optarg, &ptr, base);
          if (ptr != (optarg + strlen(optarg))
              || cinfo.io_port_val <= 0)
            _err_exit("io-port value invalid");
          break;
        case 'R':
          cinfo.reg_space++;
          cinfo.reg_space_val = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            _err_exit("reg-space value invalid");
          break;
        case 'f':
          cinfo.logfile = optarg;
          break;
        case 'n':
          cinfo.no_logging++;
          break;
        case 'u':
          cinfo.timer_use++;
          cinfo.timer_use_val = (uint8_t)strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || !IPMI_WATCHDOG_TIMER_USE_VALID(cinfo.timer_use_val))
            _err_exit("timer user invalid");
          break;
        case 'm':
          cinfo.stop_timer++;
          cinfo.stop_timer_val = (uint8_t)strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || !IPMI_WATCHDOG_STOP_TIMER_VALID(cinfo.stop_timer_val))
            _err_exit("stop timer value invalid");
          break;
        case 'l':
          cinfo.log++;
          cinfo.log_val = (uint8_t)strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || !IPMI_WATCHDOG_LOG_VALID(cinfo.log_val))
            _err_exit("log value invalid");
          break;
        case 'a':
          cinfo.timeout_action++;
          cinfo.timeout_action_val = (uint8_t)strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || !IPMI_WATCHDOG_TIMEOUT_ACTION_VALID(cinfo.timeout_action_val))
            _err_exit("timeout action value invalid");
          break;
        case 'p':
          cinfo.pre_timeout_interrupt++;
          cinfo.pre_timeout_interrupt_val = (uint8_t)strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || !IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_VALID(cinfo.pre_timeout_interrupt_val))
            _err_exit("pre timeout interrupt value invalid");
          break;
        case 'z':
          cinfo.pre_timeout_interval++;
          cinfo.pre_timeout_interval_val = (uint8_t)strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            _err_exit("pre timeout interval value invalid");
          if ((cinfo.pre_timeout_interval_val - 1) < (IPMI_WATCHDOG_PRE_TIMEOUT_INTERVAL_MIN_SECS - 1)
              || (cinfo.pre_timeout_interval_val - 1) > (IPMI_WATCHDOG_PRE_TIMEOUT_INTERVAL_MAX_SECS - 1))
            _err_exit("pre timeout interval value out of range");
          break;
        case 'F':
          cinfo.clear_bios_frb2++;
          break;
        case 'P':
          cinfo.clear_bios_post++;
          break;
        case 'L':
          cinfo.clear_os_load++;
          break;
        case 'S':
          cinfo.clear_sms_os++;
          break;
        case 'O':
          cinfo.clear_oem++;
          break;
        case 'i':
          cinfo.initial_countdown_seconds++;
          cinfo.initial_countdown_seconds_val = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            _err_exit("initial countdown value invalid");
          if (cinfo.initial_countdown_seconds_val < IPMI_WATCHDOG_INITIAL_COUNTDOWN_MIN_SECS
              || cinfo.initial_countdown_seconds_val > IPMI_WATCHDOG_INITIAL_COUNTDOWN_MAX_SECS)
            _err_exit("initial countdown value out of range");
          break;
        case 'w':
          cinfo.start_after_set++;
          break;
        case 'x':
          cinfo.reset_after_set++;
          break;
        case 'j':
          cinfo.start_if_stopped++;
        case 'k':
          cinfo.reset_if_running++;
          break;
        case 'G':
          cinfo.gratuitous_arp++;
          cinfo.gratuitous_arp_val = (uint8_t)strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || !IPMI_WATCHDOG_GRATUITOUS_ARP_VALID(cinfo.gratuitous_arp_val))
            _err_exit("gratuitous arp value invalid");
          break;
        case 'A':
          cinfo.arp_response++;
          cinfo.arp_response_val = (uint8_t)strtol(optarg, &ptr, 10);
          if ((ptr != (optarg + strlen(optarg)))
              || !IPMI_WATCHDOG_ARP_RESPONSE_VALID(cinfo.arp_response_val))
            _err_exit("arp response value invalid");
          break;
        case 'e':
          cinfo.reset_period++;
          cinfo.reset_period_val = (uint8_t)strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            _err_exit("reset period value invalid");
          if (cinfo.reset_period_val == 0
              || cinfo.reset_period_val < IPMI_WATCHDOG_INITIAL_COUNTDOWN_MIN_SECS
              || cinfo.reset_period_val > IPMI_WATCHDOG_INITIAL_COUNTDOWN_MAX_SECS)
            _err_exit("reset period value out of range");
          break;
#ifndef NDEBUG
        case 'D':
          cinfo.debug++;
          break;
#endif
        default:
          _err_exit("command line option error");
          break;
        }
    }
  
  count = cinfo.set + cinfo.get + cinfo.reset +  
    cinfo.start + cinfo.stop + cinfo.clear + cinfo.daemon;
  if (count == 0 || help_opt)
    _usage();
  if (count > 1)
    _err_exit("Only one command (set, get, reset, start, stop, clear, daemon) can be specified");

  if (((cinfo.get || cinfo.reset || cinfo.start || cinfo.stop || cinfo.clear)
       && (cinfo.timer_use || cinfo.stop_timer || cinfo.log
           || cinfo.timeout_action || cinfo.pre_timeout_interrupt
           || cinfo.pre_timeout_interval || cinfo.clear_bios_frb2
           || cinfo.clear_bios_post || cinfo.clear_sms_os 
           || cinfo.clear_oem || cinfo.initial_countdown_seconds
           || cinfo.start_after_set || cinfo.reset_after_set
           || cinfo.reset_if_running || cinfo.reset_period))
      || (cinfo.set
          && cinfo.reset_period)
      || (cinfo.daemon
          && (cinfo.stop_timer || cinfo.start_after_set || cinfo.reset_after_set
              || cinfo.reset_if_running))
      || ((cinfo.set || cinfo.get || cinfo.reset || cinfo.stop || cinfo.clear)
          && (cinfo.gratuitous_arp || cinfo.arp_response)))
    {
      char *cmdstr = _cmd_string();
      _err_exit("Invalid command option specified for '%s' command", cmdstr);
    }

  cmdline_parsed++;
}

static void
_set_cmd(void)
{
  uint8_t timer_use, stop_timer, timer_state, log, timeout_action, 
    pre_timeout_interrupt, pre_timeout_interval;
  uint32_t initial_countdown_seconds;
  int ret;
 
  if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     &timer_use, &timer_state, &log,
                                     &timeout_action, &pre_timeout_interrupt, 
                                     &pre_timeout_interval, 
                                     NULL, NULL, NULL, NULL, NULL,
                                     &initial_countdown_seconds, NULL)) != 0)
    _ipmi_err_exit(IPMI_CMD_GET_WATCHDOG_TIMER, ret, 
                   "Get Watchdog Timer Error");

  if ((!timer_state && cinfo.start_if_stopped)
      || (timer_state && cinfo.reset_if_running))
    {
      if ((ret = _reset_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                           BMC_WATCHDOG_RETRY_ATTEMPT)) != 0)
        _ipmi_err_exit(IPMI_CMD_RESET_WATCHDOG_TIMER, ret, 
                       "Reset Watchdog Timer Error");
    }
  
  timer_use = (cinfo.timer_use) ? cinfo.timer_use_val : timer_use;
  stop_timer = (cinfo.stop_timer) ? cinfo.stop_timer_val : timer_state;
  log = (cinfo.log) ? cinfo.log_val : log;
  timeout_action = (cinfo.timeout_action) ? 
    cinfo.timeout_action_val : timeout_action;
  pre_timeout_interrupt = (cinfo.pre_timeout_interrupt) ? 
    cinfo.pre_timeout_interrupt_val : pre_timeout_interrupt;
  pre_timeout_interval = (cinfo.pre_timeout_interval) ? 
    cinfo.pre_timeout_interval_val : pre_timeout_interval;
  initial_countdown_seconds = (cinfo.initial_countdown_seconds) ? 
    cinfo.initial_countdown_seconds_val : initial_countdown_seconds;
  
  if ((pre_timeout_interrupt != IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NONE)
      && (pre_timeout_interval > initial_countdown_seconds))
    _err_exit("pre-timeout interval greater than initial countdown seconds");

  if ((ret = _set_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     timer_use, stop_timer, 
                                     log, timeout_action, 
                                     pre_timeout_interrupt, pre_timeout_interval, 
                                     (cinfo.clear_bios_frb2) ? 1 : 0,
                                     (cinfo.clear_bios_post) ? 1 : 0,
                                     (cinfo.clear_os_load) ? 1 : 0,
                                     (cinfo.clear_sms_os) ? 1 : 0,
                                     (cinfo.clear_oem) ? 1 : 0,
                                     initial_countdown_seconds)) != 0)
    _ipmi_err_exit(IPMI_CMD_SET_WATCHDOG_TIMER, ret, 
                   "Set Watchdog Timer Error");

  if (cinfo.start_after_set || cinfo.reset_after_set)
    {
      if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                         BMC_WATCHDOG_RETRY_ATTEMPT,
                                         NULL, &timer_state, NULL, NULL, NULL, 
                                         NULL, NULL, NULL, NULL, NULL, NULL,
                                         NULL, NULL)) != 0)
        _ipmi_err_exit(IPMI_CMD_GET_WATCHDOG_TIMER, ret, 
                       "Get Watchdog Timer Error");
      
      if ((!timer_state && cinfo.start_after_set)
          || (timer_state && cinfo.reset_after_set))
        {
          if ((ret = _reset_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                               BMC_WATCHDOG_RETRY_ATTEMPT)) != 0)
            _ipmi_err_exit(IPMI_CMD_RESET_WATCHDOG_TIMER, ret, 
                           "Reset Watchdog Timer Error");
        }
    }

  return;
}

static char *
_log_str(uint8_t log)
{
  if (log == IPMI_WATCHDOG_LOG_ENABLE)
    return "Enabled";
  else if (log == IPMI_WATCHDOG_LOG_DISABLE)
    return "Disabled";
  else
    return "Internal Error, Unknown Log Value";

  return NULL;			/* NOT_REACHED */
}

static char *
_timer_state_str(uint8_t timer_state)
{
  if (timer_state == IPMI_WATCHDOG_TIMER_STATE_RUNNING)
    return "Running";
  else if (timer_state == IPMI_WATCHDOG_TIMER_STATE_STOPPED)
    return "Stopped";
  else
    return "Internal Error, Unknown Stop Timer Value";

  return NULL;			/* NOT_REACHED */
}

static char *
_timer_use_str(uint8_t timer_use)
{
  if (timer_use == IPMI_WATCHDOG_TIMER_USE_BIOS_FRB2)
    return "BIOS FRB2";
  else if (timer_use == IPMI_WATCHDOG_TIMER_USE_BIOS_POST)
    return "BIOS POST";
  else if (timer_use == IPMI_WATCHDOG_TIMER_USE_OS_LOAD)
    return "OS LOAD";
  else if (timer_use == IPMI_WATCHDOG_TIMER_USE_SMS_OS)
    return "SMS/OS";
  else if (timer_use == IPMI_WATCHDOG_TIMER_USE_OEM)
    return "OEM";
  else
    return "Reserved";

  return NULL;			/* NOT_REACHED */
}

static char *
_pre_timeout_interrupt_str(uint8_t pre_timeout_interrupt)
{
  if (pre_timeout_interrupt == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NONE)
    return "None";
  else if (pre_timeout_interrupt == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_SMI)
    return "SMI";
  else if (pre_timeout_interrupt == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NMI)
    return "NMI / Diagnostic Interrupt";
  else if (pre_timeout_interrupt == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_MESSAGING_INTERRUPT)
    return "Messaging Interrupt";
  else
    return "Reserved";
  
  return NULL;			/* NOT_REACHED */
}

static char *
_timeout_action_str(uint8_t timeout_action)
{
  if (timeout_action == IPMI_WATCHDOG_TIMEOUT_ACTION_NO_ACTION)
    return "None";
  else if (timeout_action == IPMI_WATCHDOG_TIMEOUT_ACTION_HARD_RESET)
    return "Hard Reset";
  else if (timeout_action == IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_DOWN)
    return "Power Down";
  else if (timeout_action == IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_CYCLE)
    return "Power Cycle";
  else
    return "Reserved";

  return NULL;			/* NOT_REACHED */
}

static void
_get_cmd(void)
{
  uint8_t timer_use, timer_state, log, timeout_action, pre_timeout_interrupt, 
    pre_timeout_interval, timer_use_expiration_flag_bios_frb2, 
    timer_use_expiration_flag_bios_post, timer_use_expiration_flag_os_load, 
    timer_use_expiration_flag_sms_os, timer_use_expiration_flag_oem;
  uint32_t initial_countdown_seconds, present_countdown_seconds;
  int ret;

  if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     &timer_use, &timer_state, &log,
                                     &timeout_action, &pre_timeout_interrupt,
                                     &pre_timeout_interval,
                                     &timer_use_expiration_flag_bios_frb2,
                                     &timer_use_expiration_flag_bios_post,
                                     &timer_use_expiration_flag_os_load,
                                     &timer_use_expiration_flag_sms_os,
                                     &timer_use_expiration_flag_oem,
                                     &initial_countdown_seconds,
                                     &present_countdown_seconds)) != 0) 
    _ipmi_err_exit(IPMI_CMD_GET_WATCHDOG_TIMER, ret, 
                   "Get Watchdog Timer Error");
  
  printf("Timer Use:                   %s\n", _timer_use_str(timer_use));
  printf("Timer:                       %s\n", _timer_state_str(timer_state));
  printf("Logging:                     %s\n", _log_str(log)); 
  printf("Timeout Action:              %s\n", 
         _timeout_action_str(timeout_action));
  printf("Pre-Timeout Interrupt:       %s\n", 
         _pre_timeout_interrupt_str(pre_timeout_interrupt));
  printf("Pre-Timeout Interval:        %d seconds\n", pre_timeout_interval);
  printf("Timer Use BIOS FRB2 Flag:    %s\n", 
         (timer_use_expiration_flag_bios_frb2) ? "Set" : "Clear");
  printf("Timer Use BIOS POST Flag:    %s\n", 
         (timer_use_expiration_flag_bios_post) ? "Set" : "Clear");
  printf("Timer Use BIOS OS Load Flag: %s\n", 
         (timer_use_expiration_flag_os_load) ? "Set" : "Clear");
  printf("Timer Use BIOS SMS/OS Flag:  %s\n", 
         (timer_use_expiration_flag_sms_os) ? "Set" : "Clear");
  printf("Timer Use BIOS OEM Flag:     %s\n", 
         (timer_use_expiration_flag_oem) ? "Set" : "Clear");
  printf("Initial Countdown:           %d seconds\n", initial_countdown_seconds);
  printf("Current Countdown:           %d seconds\n", present_countdown_seconds);
}

static void
_reset_cmd(void)
{
  int ret;
  
  if ((ret = _reset_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                       BMC_WATCHDOG_RETRY_ATTEMPT)) != 0)
    _ipmi_err_exit(IPMI_CMD_RESET_WATCHDOG_TIMER, ret, 
                   "Reset Watchdog Timer Error");
}

static void
_start_cmd(void)
{
  int ret;
  uint8_t timer_state;
  
  if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     NULL, &timer_state, NULL, NULL,
                                     NULL, NULL, NULL, NULL, NULL, NULL, 
                                     NULL, NULL, NULL)) != 0)
    _ipmi_err_exit(IPMI_CMD_GET_WATCHDOG_TIMER, ret, 
                   "Get Watchdog Timer Error");
  
  if (!timer_state)
    {
      if ((ret = _reset_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                           BMC_WATCHDOG_RETRY_ATTEMPT)) != 0)
        _ipmi_err_exit(IPMI_CMD_RESET_WATCHDOG_TIMER, ret, 
                       "Reset Watchdog Timer Error");
    }


  if (cinfo.gratuitous_arp || cinfo.arp_response)
    {
      uint8_t gratuitous_arp, arp_response;

      if (cinfo.gratuitous_arp)
        gratuitous_arp = cinfo.gratuitous_arp_val;
      else
        gratuitous_arp = IPMI_WATCHDOG_GRATUITOUS_ARP_NO_SUSPEND;

      if (cinfo.arp_response)
        arp_response = cinfo.arp_response_val;
      else
        arp_response = IPMI_WATCHDOG_ARP_RESPONSE_NO_SUSPEND;
        
      if ((ret = _suspend_bmc_arps_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                       BMC_WATCHDOG_RETRY_ATTEMPT,
                                       gratuitous_arp,
                                       arp_response)) != 0)
        _ipmi_err_exit(IPMI_CMD_SUSPEND_BMC_ARPS, ret, 
                       "Suspend BMC ARPs Error");
    }
}

static void
_stop_cmd(void)
{
  uint8_t timer_use, log, timeout_action, pre_timeout_interrupt,
    pre_timeout_interval;
  uint32_t initial_countdown_seconds;
  int ret;
   
  if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     &timer_use, NULL, &log, 
                                     &timeout_action, &pre_timeout_interrupt,
                                     &pre_timeout_interval,
                                     NULL, NULL, NULL, NULL, NULL, 
                                     &initial_countdown_seconds, NULL)) != 0)
    _ipmi_err_exit(IPMI_CMD_GET_WATCHDOG_TIMER, ret, 
                   "Get Watchdog Timer Error");
 
  if ((ret = _set_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     timer_use, IPMI_WATCHDOG_STOP_TIMER_ENABLE, 
                                     log, timeout_action, pre_timeout_interrupt, 
                                     pre_timeout_interval, 
                                     0, 0, 0, 0, 0,
                                     initial_countdown_seconds)) != 0)
    _ipmi_err_exit(IPMI_CMD_SET_WATCHDOG_TIMER, ret, 
                   "Set Watchdog Timer Error");
}

static void
_clear_cmd(void)
{
  int ret;
  uint8_t timer_use;

  /* Timer use cannot be NONE, so use whatever was there before */

  if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     &timer_use, NULL, NULL, NULL, NULL, 
                                     NULL, NULL, NULL, NULL, NULL, NULL, 
                                     NULL, NULL)) != 0)
    _ipmi_err_exit(IPMI_CMD_GET_WATCHDOG_TIMER, ret, 
                   "Get Watchdog Timer Error");

  if ((ret = _set_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                     BMC_WATCHDOG_RETRY_ATTEMPT,
                                     timer_use,
                                     IPMI_WATCHDOG_STOP_TIMER_ENABLE,
                                     IPMI_WATCHDOG_LOG_DISABLE, 
                                     IPMI_WATCHDOG_TIMEOUT_ACTION_NO_ACTION,
                                     IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NONE,
                                     0, 0, 0, 0, 0, 0, 0)) != 0)
    _ipmi_err_exit(IPMI_CMD_SET_WATCHDOG_TIMER, ret, 
                   "Set Watchdog Timer Error");
}

static void
_daemon_init()
{
  /* Based on code in Unix network programming by R. Stevens */
  int i;
  pid_t pid;

  if ((pid = fork()) < 0) 
    _err_exit("fork: %s", strerror(errno));
  if (pid != 0)
    exit(0);                    /* parent terminates */

  setsid();

  if (signal(SIGHUP, SIG_IGN) == SIG_ERR)
    _err_exit("signal: %s", strerror(errno));
  
  if ((pid = fork()) < 0) 
    _err_exit("fork: %s", strerror(errno));
  if (pid != 0)
    exit(0);                    /* 1st child terminates */

  if (chdir("/") < 0)
    _err_exit("chdir: %s", strerror(errno));

  umask(0);
  
  for (i = 0; i < 64; i++)
    close(i);

  _init_bmc_watchdog(LOG_DAEMON, 0);
}

static void
_deamon_cmd_error_exit(char *str, int ret)
{
  assert(str != NULL); 

  if (ret < 0) 
    {
      if (errno == EAGAIN || errno == EBUSY)
        _bmclog("%s: BMC Busy", str);
      else if (errno == EINVAL || errno == EIDRM)
        {
          /* Assume semaphore was deleted for some strange reason */
          _bmclog("%s: semaphore deleted: %s", str,
                  strerror(errno));
          
          if (_init_ipmi() < 0)
            exit(1);
        }
      else
        {
          _bmclog("%s Error: %s", str, strerror(errno));
          exit(1);
        }
    }
  else
    _bmclog("%s IPMI Error: %Xh", str, ret);
  _sleep(BMC_WATCHDOG_RETRY_WAIT_TIME);
}

static void
_daemon_setup(void)
{
  uint8_t timer_use, timer_state, log, timeout_action, pre_timeout_interrupt, 
    pre_timeout_interval;
  uint32_t reset_period = BMC_WATCHDOG_RESET_PERIOD_DEFAULT;
  uint32_t initial_countdown_seconds;
  int ret;

  while (1)
    {
      if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                         BMC_WATCHDOG_RETRY_ATTEMPT,
                                         &timer_use, &timer_state, &log,
                                         &timeout_action, &pre_timeout_interrupt, 
                                         &pre_timeout_interval, 
                                         NULL, NULL, NULL, NULL, NULL,
                                         &initial_countdown_seconds, NULL)) != 0)
        {
          _deamon_cmd_error_exit("Get Watchdog Timer", ret);
          continue;
        }
      break;
    }

  if (timer_state == IPMI_WATCHDOG_TIMER_STATE_RUNNING)
    _err_exit("watchdog timer must be stopped before running daemon");

  timer_use = (cinfo.timer_use) ? cinfo.timer_use_val : timer_use;
  log = (cinfo.log) ? cinfo.log_val : log;
  timeout_action = (cinfo.timeout_action) ? 
    cinfo.timeout_action_val : timeout_action;
  pre_timeout_interrupt = (cinfo.pre_timeout_interrupt) ? 
    cinfo.pre_timeout_interrupt_val : pre_timeout_interrupt;
  pre_timeout_interval = (cinfo.pre_timeout_interval) ? 
    cinfo.pre_timeout_interval_val : pre_timeout_interval;
  initial_countdown_seconds = (cinfo.initial_countdown_seconds) ? 
    cinfo.initial_countdown_seconds_val : initial_countdown_seconds;
  
  if ((pre_timeout_interrupt != IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NONE)
      && (pre_timeout_interval > initial_countdown_seconds))
    _err_exit("pre-timeout interval greater than initial countdown seconds");
  if (cinfo.reset_period)
    reset_period = cinfo.reset_period_val;
  if (reset_period > initial_countdown_seconds)
    _err_exit("reset-period interval greater than initial countdown seconds");

  while (1)
    {
      if ((ret = _set_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                         BMC_WATCHDOG_RETRY_ATTEMPT,
                                         timer_use, IPMI_WATCHDOG_STOP_TIMER_ENABLE, 
                                         log, timeout_action, 
                                         pre_timeout_interrupt, pre_timeout_interval, 
                                         (cinfo.clear_bios_frb2) ? 1 : 0,
                                         (cinfo.clear_bios_post) ? 1 : 0,
                                         (cinfo.clear_os_load) ? 1 : 0,
                                         (cinfo.clear_sms_os) ? 1 : 0,
                                         (cinfo.clear_oem) ? 1 : 0,
                                         initial_countdown_seconds)) != 0)
        {
          _deamon_cmd_error_exit("Set Watchdog Timer", ret);
          continue;
        }
      break;
    }

  /* Must start watchdog timer before entering loop */
  while (1)
    {
      if ((ret = _reset_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                           BMC_WATCHDOG_RETRY_ATTEMPT)) != 0)
        {
          _deamon_cmd_error_exit("Reset Watchdog Timer", ret);
          continue;
        }
      break;
    }

  if (cinfo.gratuitous_arp || cinfo.arp_response)
    {
      uint8_t gratuitous_arp, arp_response;
      
      if (cinfo.gratuitous_arp)
        gratuitous_arp = cinfo.gratuitous_arp_val;
      else
        gratuitous_arp = IPMI_WATCHDOG_GRATUITOUS_ARP_NO_SUSPEND;
      
      if (cinfo.arp_response)
        arp_response = cinfo.gratuitous_arp_val;
      else
        arp_response = IPMI_WATCHDOG_ARP_RESPONSE_NO_SUSPEND;

      while (1)
        {
          if ((ret = _suspend_bmc_arps_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                           BMC_WATCHDOG_RETRY_ATTEMPT,
                                           gratuitous_arp,
                                           arp_response)) != 0)
            {
              _deamon_cmd_error_exit("Suspend BMC ARPs", ret);
              continue;
            }
        }
    }

  return;
}

static void 
_signal_handler(int sig)
{
  shutdown_flag = 0;
}

static void
_daemon_cmd_error_noexit(char *str, int ret)
{
  assert(str != NULL); 

  if (ret < 0) 
    {
      if (errno == EAGAIN || errno == EBUSY)
        _bmclog("%s Error: BMC Busy", str);
      else if (errno == EINVAL || errno == EIDRM)
        {
          /* Assume semaphore was deleted for some strange reason */
          _bmclog("%s: semaphore deleted: %s", str,
                  strerror(errno));
          _init_ipmi();
        }
      else
        _bmclog("%s Error: %s", str, strerror(errno));
    }
  else
    _bmclog("%s IPMI Error: %Xh", str, ret);
}

static void
_daemon_cmd(void)
{
  uint32_t reset_period = BMC_WATCHDOG_RESET_PERIOD_DEFAULT;
  uint8_t timer_use, timer_state, log, timeout_action, pre_timeout_interrupt,
    pre_timeout_interval;
  uint32_t initial_countdown_seconds;
  int retry_wait_time, retry_attempt;
  int ret;

#ifndef NDEBUG
  /* Run in foreground if debugging */
  if (!cinfo.debug)
    _daemon_init();
#else
  _daemon_init();
#endif

  _daemon_setup();

  if (cinfo.reset_period)
    reset_period = cinfo.reset_period_val;

  if (signal(SIGTERM, _signal_handler) == SIG_ERR)
    _err_exit("signal: %s", strerror(errno));
  if (signal(SIGINT, _signal_handler) == SIG_ERR)
    _err_exit("signal: %s", strerror(errno));

  _bmclog("starting bmc-watchdog daemon");

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

  while(shutdown_flag)
    {
      struct timeval start_tv, end_tv;
      int timeval_bad = 0;

      if (gettimeofday(&start_tv, NULL) < 0) 
        {
          _bmclog("gettimeofday: %s", strerror(errno));
          timeval_bad++;
        }

      if ((ret = _get_watchdog_timer_cmd(retry_wait_time, retry_attempt, 
                                         NULL, &timer_state, NULL, NULL, 
                                         NULL, NULL, NULL, NULL, NULL, NULL, 
                                         NULL, NULL, NULL)) != 0)
        {
          _daemon_cmd_error_noexit("Get Watchdog Timer", ret);
          goto sleep_now;
        }
      
      if (timer_state == IPMI_WATCHDOG_TIMER_STATE_STOPPED)
        {
          _bmclog("timer stopped by another process");
          goto cleanup; 
        }

      if ((ret = _reset_watchdog_timer_cmd(retry_wait_time, retry_attempt)) != 0)
        {
          _daemon_cmd_error_noexit("Reset Watchdog Timer", ret);
          goto sleep_now;
        }
      if (ret != 0)
        {
          char buf[BMC_WATCHDOG_STR_BUFLEN];

          if (ipmi_strerror_r(IPMI_CMD_RESET_WATCHDOG_TIMER, ret, 
                              buf, BMC_WATCHDOG_STR_BUFLEN) < 0)
            _bmclog("Reset Watchdog Timer IPMI Error: %Xh", ret);
          else
            _bmclog("Reset Watchdog Timer IPMI Error: %s", buf);
        }
      else 
        _bmclog("BMC-Watchdog Timer Reset");


    sleep_now:
      if (gettimeofday(&end_tv, NULL) < 0)
        {
          _bmclog("gettimeofday: %s", strerror(errno));
          timeval_bad++;
        }

      if (timeval_bad)
        _sleep(reset_period);
      else
        {
          uint32_t adjusted_period = reset_period;

          /* Ignore micro secs, just seconds is good enough */ 
          if ((end_tv.tv_sec - start_tv.tv_sec) < adjusted_period)
            adjusted_period -= (end_tv.tv_sec - start_tv.tv_sec);

          _sleep(adjusted_period);
        }
    }

  /* Need to stop the timer, don't want it to keep on going.  Don't
   * give up until its shut off. 
   */

  while (1)
    {
      if ((ret = _get_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                         BMC_WATCHDOG_RETRY_ATTEMPT,
                                         &timer_use, NULL, &log, 
                                         &timeout_action, &pre_timeout_interrupt,
                                         &pre_timeout_interval,
                                         NULL, NULL, NULL, NULL, NULL, 
                                         &initial_countdown_seconds, NULL)) != 0)
        {
          _daemon_cmd_error_noexit("Get Watchdog Timer", ret);
          continue;
        }
      break;
    }
  
  while (1)
    {
      if ((ret = _set_watchdog_timer_cmd(BMC_WATCHDOG_RETRY_WAIT_TIME,
                                         BMC_WATCHDOG_RETRY_ATTEMPT,
                                         timer_use, IPMI_WATCHDOG_STOP_TIMER_ENABLE, 
                                         log, timeout_action, pre_timeout_interrupt, 
                                         pre_timeout_interval, 
                                         0, 0, 0, 0, 0,
                                         initial_countdown_seconds)) != 0)
        {
          _daemon_cmd_error_noexit("Set Watchdog Timer", ret);
          continue;
        }
      break;
    }

 cleanup:
  _bmclog("stopping bmc-watchdog daemon");
}

int 
main(int argc, char **argv)
{
#if 0 /* commented by AB */
#if	defined(__FreeBSD__) && !defined(USE_IOPERM)
  int dev_io_fd;
#endif
#endif /* commented by AB */

  _err_init(argv[0]);

#if 0  /* commented by AB */
#ifdef __FreeBSD__
#ifdef USE_IOPERM
  if (i386_set_ioperm(0, 0x10000, 1) == -1)
#else
  dev_io_fd = open("/dev/io", O_RDONLY);
  if (dev_io_fd == -1)
#endif /* !USE_IOPERM */
#else
  if(iopl(3) != 0)
#endif /* !__FreeBSD__ */
    {
      /* glibc bug on older ia64 systems returns EACCES instead of EPERM */  
      if (errno == EPERM || errno == EACCES)
        _err_exit("Permission denied, must be root.");
      else
        _err_exit("iopl: %s", strerror(errno));
    }
#endif /* commented by AB */

  _cmdline_default();
  _cmdline_parse(argc, argv);

  /* Early initialization.  Assumes its a cronjob if its not a daemon.
   * Daemon must do all initialization in daemon_init() b/c
   * daemon_init() needs to close all formerly open file descriptors.
   */
  if (!cinfo.daemon)
    _init_bmc_watchdog(LOG_CRON, 1);

  if (cinfo.set)
    _set_cmd();
  else if (cinfo.get)
    _get_cmd();
  else if (cinfo.reset)
    _reset_cmd();
  else if (cinfo.start)
    _start_cmd();
  else if (cinfo.stop)
    _stop_cmd();
  else if (cinfo.clear)
    _clear_cmd();
  else if (cinfo.daemon)
    _daemon_cmd();
  else
    _err_exit("internal error, command not set");


#if 0  /* commented by AB */
#if defined(__FreeBSD__) && !defined(USE_IOPERM)
  close(dev_io_fd);
#endif
#endif /* commented by AB */

  ipmi_close(&ipmi_dev);
  close(logfile_fd);
  closelog();
  exit(0);
}
