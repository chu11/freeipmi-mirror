/*****************************************************************************\
 *  $Id: ipmiconsole.c,v 1.5 2007-03-20 21:26:37 chu11 Exp $
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
    "success",			/* 0 */
    "context null",		/* 1 */
    "context invalid",		/* 2 */
    "engine already setup",	/* 3 */
    "engine not setup",		/* 4 */
    "ctx already submitted",	/* 5 */
    "ctx not submitted",	/* 6 */
    "ctx is still submitted",	/* 7 */
    "invalid parmaeters",	/* 8 */
    "ipmi 2.0 unavailable",	/* 9 */
    "cipher suite unavailable",	/* 10 */
    "hostname invalid",		/* 11 */
    "username invalid",		/* 12 */
    "password invalid",		/* 13 */
    "k_g invalid",		/* 14 */
    "privilege invalid",	/* 15 */
    "cipher suite invalid",	/* 16 */
    "SOL unavailable",		/* 17 */
    "SOL in use",		/* 18 */
    "SOL not responding",	/* 19 */
    "SOL session stolen",       /* 20 */
    "BMC Busy",			/* 21 */
    "BMC Error",		/* 22 */
    "session timeout",		/* 23 */
    "out of memory",		/* 24 */
    "internal system error",	/* 25 */
    "internal error",		/* 26 */
    "errnum out of range",	/* 27 */
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

  if (ipmiconsole_engine_setup() < 0)
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
  ipmiconsole_engine_cleanup();
  return -1;
}

int 
ipmiconsole_engine_submit(ipmiconsole_ctx_t c)
{
  int rv;

  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;

  if (!ipmiconsole_engine_is_setup())
    {
      c->errnum = IPMICONSOLE_ERR_NOT_SETUP;
      return -1;
    }

  if ((rv = pthread_mutex_lock(&(c->session_submitted_mutex))))
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_lock: %s", strerror(rv)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL;
      return -1;
    }

  if (c->session_submitted)
    {
      c->errnum = IPMICONSOLE_ERR_CTX_ALREADY_SUBMITTED;
      return -1;
    }

  if ((rv = pthread_mutex_unlock(&(c->session_submitted_mutex))))
    {
      IPMICONSOLE_DEBUG(("pthread_mutex_unlock: %s", strerror(rv)));
      c->errnum = IPMICONSOLE_ERR_INTERNAL;
      return -1;
    }

  if (_ipmiconsole_init_ctx_session(c) < 0)
    goto cleanup;

  /* session_submitted flag set in here */
  if (ipmiconsole_engine_submit_ctx(c) < 0)
    goto cleanup;

  return 0;

 cleanup:
  _ipmiconsole_cleanup_ctx_session(c);
  return -1;
}

void
ipmiconsole_engine_teardown(void)
{
  ipmiconsole_debug_cleanup();
  ipmiconsole_engine_cleanup();
}

ipmiconsole_ctx_t 
ipmiconsole_ctx_create(char *hostname,
		       struct ipmiconsole_ipmi_config *ipmi_config,
		       struct ipmiconsole_protocol_config *protocol_config)
{
  ipmiconsole_ctx_t c = NULL;
  int rv;

  if (!hostname
      || (hostname && strlen(hostname) > MAXHOSTNAMELEN)
      || !ipmi_config
      || !protocol_config
      || (ipmi_config->username && strlen(ipmi_config->username) > IPMI_MAX_USER_NAME_LENGTH)
      || (ipmi_config->password && strlen(ipmi_config->password) > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || (ipmi_config->k_g && strlen(ipmi_config->k_g) > IPMI_MAX_K_G_LENGTH)
      || (ipmi_config->privilege_level >= 0
	  && (ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_USER
	      && ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_OPERATOR
	      && ipmi_config->privilege_level != IPMICONSOLE_PRIVILEGE_ADMIN))
      || (ipmi_config->cipher_suite_id >= IPMI_CIPHER_SUITE_ID_MIN
	  && !IPMI_CIPHER_SUITE_ID_SUPPORTED(ipmi_config->cipher_suite_id))
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

  if (ipmi_config->k_g)
    strcpy((char *)c->k_g, ipmi_config->k_g);

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

  c->security_flags = protocol_config->security_flags;

  c->workaround_flags = protocol_config->workaround_flags;

  if ((rv = pthread_mutex_init(&c->session_submitted_mutex, NULL)) != 0)
    {
      errno = rv;
      goto cleanup;
    }
  c->session_submitted = 0;
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

static int
_is_submitted(ipmiconsole_ctx_t c)
{
  int rv = -1;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);

  if (pthread_mutex_lock(&(c->session_submitted_mutex)) != 0)
    {
      c->errnum = IPMICONSOLE_ERR_INTERNAL;
      return -1;
    }
  
  if (!c->session_submitted)
    c->errnum = IPMICONSOLE_ERR_CTX_NOT_SUBMITTED;
  else 
    rv = 0;
  
  if (pthread_mutex_unlock(&(c->session_submitted_mutex)) != 0)
    {
      c->errnum = IPMICONSOLE_ERR_INTERNAL;
      return -1;
    }

  return 0;
}

int 
ipmiconsole_ctx_fd(ipmiconsole_ctx_t c)
{
  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;
  
  if (_is_submitted(c) < 0)
    return -1;

  c->errnum = IPMICONSOLE_ERR_SUCCESS;
  return c->session.user_fd;
}

int 
ipmiconsole_ctx_generate_break(ipmiconsole_ctx_t c)
{
  uint8_t val;

  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;

  if (_is_submitted(c) < 0)
    return -1;

  val = IPMICONSOLE_PIPE_GENERATE_BREAK_CODE;
  if (write(c->session.asynccomm[1], &val, 1) < 0)
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
  if (!c || c->magic != IPMICONSOLE_CTX_MAGIC)
    return -1;
  
  if (c->session_submitted)
    {
      c->errnum = IPMICONSOLE_ERR_CTX_IS_SUBMITTED;
      return -1;
    }

  ipmiconsole_ctx_debug_cleanup(c);

  pthread_mutex_destroy(&(c->session_submitted_mutex));

  c->errnum = IPMICONSOLE_ERR_CONTEXT_INVALID;
  c->magic = ~IPMICONSOLE_CTX_MAGIC;
  if (c->security_flags & IPMICONSOLE_SECURITY_LOCK_MEMORY)
    secure_free(c, sizeof(struct ipmiconsole_ctx));
  else
    free(c);
  return 0;
}
