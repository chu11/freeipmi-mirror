/*****************************************************************************\
 *  $Id: ipmiconsole.c,v 1.43 2007-08-20 22:04:26 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *  
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmiconsole is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmiconsole is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
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
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
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
#include <sys/types.h>
#include <sys/select.h>
#ifdef WITH_PTHREADS
#include <pthread.h>
#endif /* WITH_PTHREADS */
#include <sys/resource.h>
#include <assert.h>
#include <errno.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "secure.h"
#include "ipmiconsole_debug.h"
#include "ipmiconsole_engine.h"
#include "ipmiconsole_fiid_wrappers.h"

/* 
 * ipmi console errmsgs
 */
static char *ipmiconsole_errmsgs[] =
  {
    "success",			                        /* 0 */
    "context null",		                        /* 1 */
    "context invalid",		                        /* 2 */
    "engine already setup",	                        /* 3 */
    "engine not setup",		                        /* 4 */
    "ctx already submitted",	                        /* 5 */
    "ctx not submitted",	                        /* 6 */
    "ctx is still submitted",	                        /* 7 */
    "invalid parameters",	                        /* 8 */
    "ipmi 2.0 unavailable",	                        /* 9 */
    "cipher suite id unavailable",                      /* 10 */
    "hostname invalid",		                        /* 11 */
    "username invalid",		                        /* 12 */
    "password invalid",		                        /* 13 */
    "k_g invalid",		                        /* 14 */
    "privilege level insufficient",                     /* 15 */
    "privilege level cannot be obtained for this user", /* 16 */
    "SOL unavailable",		                        /* 17 */
    "SOL in use",		                        /* 18 */
    "SOL session stolen",                               /* 19 */
    "SOL requires encryption",                          /* 20 */
    "SOL requires no encryption",                       /* 21 */
    "BMC Busy",			                        /* 22 */
    "BMC Error",		                        /* 23 */
    "internal BMC settings invalid",                    /* 24 */
    "session timeout",		                        /* 25 */
    "excess retransmissions sent",                      /* 26 */
    "excess errors received",                           /* 27 */
    "out of memory",		                        /* 28 */
    "too many open files",                              /* 29 */
    "internal system error",	                        /* 30 */
    "internal error",		                        /* 31 */
    "errnum out of range",	                        /* 32 */
    NULL
  };

int 
ipmiconsole_engine_init(unsigned int thread_count, unsigned int debug_flags)
{
  struct rlimit rlim;
  int i;

  if (!thread_count
      || thread_count > IPMICONSOLE_THREAD_COUNT_MAX
      || (debug_flags & ~IPMICONSOLE_DEBUG_MASK))
    {
      errno = EINVAL;
      return -1;
    }

  /* Note: Must be called first before anything else for debugging purposes */
  if (ipmiconsole_debug_setup(debug_flags) < 0)
    goto cleanup;

  if (ipmiconsole_engine_is_setup())
    return 0;

  if (ipmiconsole_engine_setup(thread_count) < 0)
    goto cleanup;

  for (i = 0; i < thread_count; i++)
    {
      if (ipmiconsole_engine_thread_create() < 0)
        goto cleanup;
    }
 
  /* If the file descriptor increase fails, ignore it */

  if (getrlimit(RLIMIT_NOFILE, &rlim) == 0)
    {
      rlim.rlim_cur = rlim.rlim_max;
      setrlimit(RLIMIT_NOFILE, &rlim);
    }

  return 0;

 cleanup:
  ipmiconsole_debug_cleanup();
  ipmiconsole_engine_cleanup(0);
  return -1;
}

static int
_ipmiconsole_blocking_notification_cleanup(ipmiconsole_ctx_t c)
{
  int perr;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->blocking_submit_requested);

  if ((perr = pthread_mutex_lock(&(c->blocking_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  close(c->blocking_notification[0]);
  close(c->blocking_notification[1]);
  c->blocking_notification[0] = -1;
  c->blocking_notification[1] = -1;
  c->blocking_submit_requested = 0;
  c->sol_session_established = 0;

  if ((perr = pthread_mutex_unlock(&(c->blocking_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  return 0;
}

static int
_ipmiconsole_blocking_notification_setup(ipmiconsole_ctx_t c)
{
  int perr;
  int closeonexec;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  
  if ((perr = pthread_mutex_lock(&(c->blocking_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (pipe(c->blocking_notification) < 0)
    {
      if ((perr = pthread_mutex_unlock(&(c->blocking_mutex))) != 0)
        IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
      IPMICONSOLE_CTX_DEBUG(c, ("pipe: %s", strerror(errno)));
      if (errno == EMFILE)
        c->errnum = IPMICONSOLE_ERR_TOO_MANY_OPEN_FILES;
      else
        c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  c->blocking_submit_requested++;
  c->sol_session_established = 0;

  if ((perr = pthread_mutex_unlock(&(c->blocking_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if ((closeonexec = fcntl(c->blocking_notification[0], F_GETFD, 0)) < 0)
    {
      IPMICONSOLE_DEBUG(("fcntl: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  closeonexec |= FD_CLOEXEC;
  if (fcntl(c->blocking_notification[0], F_SETFD, closeonexec) < 0)
    {
      IPMICONSOLE_DEBUG(("fcntl: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  if ((closeonexec = fcntl(c->blocking_notification[1], F_GETFD, 0)) < 0)
    {
      IPMICONSOLE_DEBUG(("fcntl: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      goto cleanup;
    }
  closeonexec |= FD_CLOEXEC;
  if (fcntl(c->blocking_notification[1], F_SETFD, closeonexec) < 0)
    {
      IPMICONSOLE_DEBUG(("fcntl: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  return 0;

 cleanup:
  _ipmiconsole_blocking_notification_cleanup(c);
  return -1;
}

static int
_ipmiconsole_block(ipmiconsole_ctx_t c)
{
  fd_set rds;
  int n;
  
  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(c->blocking_submit_requested);

  FD_ZERO(&rds);
  FD_SET(c->blocking_notification[0], &rds);

  if ((n = select(c->blocking_notification[0] + 1, &rds, NULL, NULL, NULL)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("select: %s", strerror(errno)));
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  if (!n)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("select returned 0"));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (FD_ISSET(c->blocking_notification[0], &rds))
    {
      uint8_t val;
      ssize_t len;

      if ((len = read(c->blocking_notification[0], (void *)&val, 1)) < 0)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("read: %s", strerror(errno)));
          c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
          goto cleanup;
        }
      
      if (!len)
        {
          IPMICONSOLE_CTX_DEBUG(c, ("blocking_notification closed"));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
          goto cleanup;
        }

      if (val == IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_ESTABLISHED)
        goto success;
      else if (val == IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_ERROR)
        goto cleanup;
      else if (c->security_flags & IPMICONSOLE_SECURITY_DEACTIVATE_ONLY
               && val == IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_DEACTIVATED)
        goto success;
      else
        {
          IPMICONSOLE_CTX_DEBUG(c, ("blocking_notification returned invalid data: %d", val));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
    }

 success:
  return 0;

 cleanup:
  return -1;
}

int 
ipmiconsole_engine_submit(ipmiconsole_ctx_t c, int blocking)
{
  int perr;

  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;

  if (!ipmiconsole_engine_is_setup())
    {
      c->errnum = IPMICONSOLE_ERR_NOT_SETUP;
      return -1;
    }

  if (c->session_submitted)
    {
      c->errnum = IPMICONSOLE_ERR_CTX_ALREADY_SUBMITTED;
      return -1;
    }

  if (blocking)
    {
      /* Set to success, so we know if an IPMI error occurred */
      c->errnum = IPMICONSOLE_ERR_SUCCESS;
      
      if (_ipmiconsole_blocking_notification_setup(c) < 0)
        goto cleanup;
      
      if (_ipmiconsole_init_ctx_session(c) < 0)
        {
          _ipmiconsole_blocking_notification_cleanup(c);
          goto cleanup;
        }
      
      if (ipmiconsole_engine_submit_ctx(c) < 0)
        {
          _ipmiconsole_blocking_notification_cleanup(c);
          goto cleanup;
        }
      
      /* sol_session_established flag maybe set in here */
      if (_ipmiconsole_block(c) < 0)
        {
          _ipmiconsole_blocking_notification_cleanup(c);
          /* don't go to cleanup, b/c the session will automatically
           * call _ipmiconsole_cleanup_ctx_session.
           */
          goto cleanup_ctx_managed_session_data_only;
        }

      _ipmiconsole_blocking_notification_cleanup(c);
    }
  else
    {
      if (_ipmiconsole_init_ctx_session(c) < 0)
        goto cleanup;
      
      if (ipmiconsole_engine_submit_ctx(c) < 0)
        goto cleanup;
    }

  if ((perr = pthread_mutex_lock(&(c->status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }
  
  /* Check for NONE status, conceivably ERROR or SOL_ESTABLISHED could
   * already be set 
   */
  if (c->status == IPMICONSOLE_CONTEXT_STATUS_NOT_SUBMITTED)
    c->status = IPMICONSOLE_CONTEXT_STATUS_SUBMITTED;
  
  if ((perr = pthread_mutex_unlock(&(c->status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  c->session_submitted++;
  return 0;

 cleanup:
  _ipmiconsole_cleanup_ctx_session(c);
 cleanup_ctx_managed_session_data_only:
  _ipmiconsole_cleanup_ctx_managed_session_data(c);
  _ipmiconsole_init_ctx_managed_session_data(c);
  return -1;
}

void
ipmiconsole_engine_teardown(int cleanup_sol_sessions)
{
  ipmiconsole_debug_cleanup();
  ipmiconsole_engine_cleanup(cleanup_sol_sessions);
}

ipmiconsole_ctx_t 
ipmiconsole_ctx_create(char *hostname,
		       struct ipmiconsole_ipmi_config *ipmi_config,
		       struct ipmiconsole_protocol_config *protocol_config)
{
  ipmiconsole_ctx_t c = NULL;
  int perr;

  if (!hostname
      || (hostname && strlen(hostname) > MAXHOSTNAMELEN)
      || !ipmi_config
      || !protocol_config
      || (ipmi_config->username && strlen(ipmi_config->username) > IPMI_MAX_USER_NAME_LENGTH)
      || (ipmi_config->password && strlen(ipmi_config->password) > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || (ipmi_config->k_g && ipmi_config->k_g_len > IPMI_MAX_K_G_LENGTH)
      || (ipmi_config->privilege_level >= 0
	  && (ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_USER
	      && ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_OPERATOR
	      && ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_ADMIN))
      || (ipmi_config->cipher_suite_id >= IPMI_CIPHER_SUITE_ID_MIN
	  && !IPMI_CIPHER_SUITE_ID_SUPPORTED(ipmi_config->cipher_suite_id))
      || (protocol_config->engine_flags & ~IPMICONSOLE_ENGINE_MASK)
      || (protocol_config->debug_flags & ~IPMICONSOLE_DEBUG_MASK)
      || (protocol_config->security_flags & ~IPMICONSOLE_SECURITY_MASK)
      || (protocol_config->workaround_flags & ~IPMICONSOLE_WORKAROUND_MASK))
    {
      errno = EINVAL;
      return NULL;
    }

  if (protocol_config->security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY)
    {
      if (!(c = (ipmiconsole_ctx_t)secure_malloc(sizeof(struct ipmiconsole_ctx))))
        {
          errno = ENOMEM;
          goto cleanup;
        }
    }
  else
    {
      if (!(c = (ipmiconsole_ctx_t)malloc(sizeof(struct ipmiconsole_ctx))))
        {
          errno = ENOMEM;
          goto cleanup;
        }
    }

  memset(c, '\0', sizeof(struct ipmiconsole_ctx));

  c->magic = IPMICONSOLE_CTX_MAGIC;

  strcpy(c->hostname, hostname);

  c->errnum = IPMICONSOLE_ERR_SUCCESS;

  if (ipmi_config->username)
    strcpy((char *)c->username, ipmi_config->username);

  if (ipmi_config->password)
    strcpy((char *)c->password, ipmi_config->password);

  /* k_g may contain nulls */
  if (ipmi_config->k_g && ipmi_config->k_g_len) 
    {
      memcpy(c->k_g, ipmi_config->k_g, ipmi_config->k_g_len);
      c->k_g_len = ipmi_config->k_g_len;
    }

  if (ipmi_config->privilege_level >= 0)
    {
      if (ipmi_config->privilege_level == IPMICONSOLE_PRIVILEGE_USER)
        c->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
      else if (ipmi_config->privilege_level == IPMICONSOLE_PRIVILEGE_OPERATOR)
        c->privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
      else
        c->privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
    }
  else
    c->privilege_level = IPMI_PRIVILEGE_LEVEL_DEFAULT;

  if (ipmi_config->cipher_suite_id >= IPMI_CIPHER_SUITE_ID_MIN)
    c->cipher_suite_id = ipmi_config->cipher_suite_id;
  else
    c->cipher_suite_id = IPMI_CIPHER_SUITE_ID_DEFAULT;

  if (protocol_config->session_timeout_len > 0)
    c->session_timeout_len = protocol_config->session_timeout_len;
  else
    c->session_timeout_len = IPMICONSOLE_SESSION_TIMEOUT_LENGTH_DEFAULT;

  if (protocol_config->retransmission_timeout_len > 0)
    c->retransmission_timeout_len = protocol_config->retransmission_timeout_len;
  else
    c->retransmission_timeout_len = IPMICONSOLE_RETRANSMISSION_TIMEOUT_LENGTH_DEFAULT;

  if (protocol_config->retransmission_backoff_count > 0)
    c->retransmission_backoff_count = protocol_config->retransmission_backoff_count;
  else
    c->retransmission_backoff_count = IPMICONSOLE_RETRANSMISSION_BACKOFF_COUNT_DEFAULT;

  if (protocol_config->keepalive_timeout_len > 0)
    c->keepalive_timeout_len = protocol_config->keepalive_timeout_len;
  else
    c->keepalive_timeout_len = IPMICONSOLE_KEEPALIVE_TIMEOUT_LENGTH_DEFAULT;

  if (protocol_config->retransmission_keepalive_timeout_len > 0)
    c->retransmission_keepalive_timeout_len = protocol_config->retransmission_keepalive_timeout_len;
  else
    c->retransmission_keepalive_timeout_len = IPMICONSOLE_RETRANSMISSION_KEEPALIVE_TIMEOUT_LENGTH_DEFAULT;

  if (protocol_config->acceptable_packet_errors_count > 0)
    c->acceptable_packet_errors_count = protocol_config->acceptable_packet_errors_count;
  else
    c->acceptable_packet_errors_count = IPMICONSOLE_ACCEPTABLE_PACKET_ERRORS_COUNT_DEFAULT;

  if (protocol_config->maximum_retransmission_count > 0)
    c->maximum_retransmission_count = protocol_config->maximum_retransmission_count;
  else
    c->maximum_retransmission_count = IPMICONSOLE_MAXIMUM_RETRANSMISSION_COUNT_DEFAULT;

  /* Retransmission timeout cannot be larger than the session timeout */
  if (c->retransmission_timeout_len > c->session_timeout_len)
    {
      errno = EINVAL;
      goto cleanup;
    }

  /* Keepalive timeout cannot be larger than the session timeout */
  if (c->keepalive_timeout_len > c->session_timeout_len)
    {
      errno = EINVAL;
      goto cleanup;
    }
  
  /* Retransmission timeout cannot be larger than the keepalive timeout */
  if (c->retransmission_timeout_len > c->keepalive_timeout_len)
    {
      errno = EINVAL;
      goto cleanup;
    }

  /* Retransmission keepalive timeout cannot be larger than the keepalive timeout */
  if (c->retransmission_keepalive_timeout_len > c->keepalive_timeout_len)
    {
      errno = EINVAL;
      goto cleanup;
    }
 
  if (ipmiconsole_ctx_debug_setup(c, protocol_config->debug_flags) < 0)
    goto cleanup;

  c->engine_flags = protocol_config->engine_flags;

  c->security_flags = protocol_config->security_flags;

  c->workaround_flags = protocol_config->workaround_flags;

  if ((perr = pthread_mutex_init(&c->status_mutex, NULL)) != 0)
    {
      errno = perr;
      goto cleanup;
    }
  c->status = IPMICONSOLE_CONTEXT_STATUS_NOT_SUBMITTED;

  if ((perr = pthread_mutex_init(&c->blocking_mutex, NULL)) != 0)
    {
      errno = perr;
      goto cleanup;
    }
  c->blocking_submit_requested = 0;
  c->blocking_notification[0] = -1;
  c->blocking_notification[1] = -1;

  c->sol_session_established = 0;

  _ipmiconsole_init_ctx_managed_session_data(c);

  c->session_submitted = 0;

  if ((perr = pthread_mutex_init(&c->exitted_mutex, NULL)) != 0)
    {
      errno = perr;
      goto cleanup;
    }
  c->exitted = 0;

  c->errnum = IPMICONSOLE_ERR_SUCCESS;
  return c;

 cleanup:
  ipmiconsole_ctx_debug_cleanup(c);
  if (protocol_config->security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY)
    secure_free(c, sizeof(struct ipmiconsole_ctx));
  else
    free(c);
  return NULL;
}

int 
ipmiconsole_ctx_errnum(ipmiconsole_ctx_t c)
{
  if (!c)
    return IPMICONSOLE_ERR_CONTEXT_NULL;
  else if (c->magic != IPMICONSOLE_CTX_MAGIC)
    return IPMICONSOLE_ERR_CONTEXT_INVALID;
  else
    return c->errnum;
}

char *
ipmiconsole_ctx_strerror(int errnum)
{
  if (errnum >= IPMICONSOLE_ERR_SUCCESS && errnum <= IPMICONSOLE_ERR_ERRNUMRANGE)
    return ipmiconsole_errmsgs[errnum];
  else
    return ipmiconsole_errmsgs[IPMICONSOLE_ERR_ERRNUMRANGE];
}

int 
ipmiconsole_ctx_status(ipmiconsole_ctx_t c)
{
  int status;
  int perr;

  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;

  /* Do not check if the context is submitted, b/c it may not be.
   *
   * Also, do not set errnum == success for this function, it could be
   * returning IPMICONSOLE_CONTEXT_STATUS_ERROR.
   */

  if ((perr = pthread_mutex_lock(&(c->status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  status = c->status;
  
  if ((perr = pthread_mutex_unlock(&(c->status_mutex))) != 0)
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
      return -1;
    }

  return status;
}

int 
ipmiconsole_ctx_fd(ipmiconsole_ctx_t c)
{
  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;
  
  if (!c->session_submitted)
    {
      c->errnum = IPMICONSOLE_ERR_CTX_NOT_SUBMITTED;
      return -1;
    }

  c->user_fd_retrieved++;
  c->errnum = IPMICONSOLE_ERR_SUCCESS;
  return c->user_fd;
}

int 
ipmiconsole_ctx_generate_break(ipmiconsole_ctx_t c)
{
  uint8_t val;

  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;

  if (!c->session_submitted)
    {
      c->errnum = IPMICONSOLE_ERR_CTX_NOT_SUBMITTED;
      return -1;
    }

  val = IPMICONSOLE_PIPE_GENERATE_BREAK_CODE;
  if (write(c->asynccomm[1], &val, 1) < 0)
    {
      c->errnum = IPMICONSOLE_ERR_SYSTEM_ERROR;
      return -1;
    }

  c->errnum = IPMICONSOLE_ERR_SUCCESS;
  return 0;
}

int
ipmiconsole_ctx_destroy(ipmiconsole_ctx_t c)
{
  int perr;

  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;
  
  if (c->session_submitted)
    {
      if ((perr = pthread_mutex_lock(&(c->exitted_mutex))) != 0)
        {
          IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(perr)));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
          return -1;
        }

      if (!c->exitted)
        {
          if ((perr = pthread_mutex_unlock(&(c->exitted_mutex))) != 0)
            IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
          c->errnum = IPMICONSOLE_ERR_CTX_IS_SUBMITTED;
          return -1;
        }

      if ((perr = pthread_mutex_unlock(&(c->exitted_mutex))) != 0)
        {
          IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(perr)));
          c->errnum = IPMICONSOLE_ERR_INTERNAL_ERROR;
          return -1;
        }
    }

  pthread_mutex_destroy(&(c->status_mutex));

  pthread_mutex_destroy(&(c->blocking_mutex));

  ipmiconsole_ctx_debug_cleanup(c);
  
  _ipmiconsole_cleanup_ctx_managed_session_data(c);
  
  pthread_mutex_destroy(&(c->exitted_mutex));
      
  c->errnum = IPMICONSOLE_ERR_CONTEXT_INVALID;
  c->magic = ~IPMICONSOLE_CTX_MAGIC;
  if (c->security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY)
    secure_free(c, sizeof(struct ipmiconsole_ctx));
  else
    free(c);
  return 0;
}
