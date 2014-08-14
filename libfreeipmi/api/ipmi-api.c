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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif  /* !TIME_WITH_SYS_TIME */
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/cmds/ipmi-event-cmds.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/driver/ipmi-inteldcmi-driver.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/driver/ipmi-sunbmc-driver.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/interface/ipmi-kcs-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-cipher-suite-util.h"
#include "freeipmi/util/ipmi-outofband-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-inteldcmi-driver-api.h"
#include "ipmi-lan-interface-api.h"
#include "ipmi-lan-session-common.h"
#include "ipmi-kcs-driver-api.h"
#include "ipmi-openipmi-driver-api.h"
#include "ipmi-sunbmc-driver-api.h"
#include "ipmi-ssif-driver-api.h"

#include "libcommon/ipmi-crypt.h"
#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"
#include "secure.h"

#define IPMI_POLL_INTERVAL_USECS             10

#define GETHOSTBYNAME_AUX_BUFLEN             1024
extern int h_errno;

static char *ipmi_errmsg[] =
  {
    "success",                                                          /* 0 */
    "device null",                                                      /* 1 */
    "device invalid",                                                   /* 2 */
    "permission denied",                                                /* 3 */
    "username invalid",                                                 /* 4 */
    "password invalid",                                                 /* 5 */
    "k_g invalid",                                                      /* 6 */
    "privilege level insufficient",                                     /* 7 */
    "privilege level cannot be obtained for this user",                 /* 8 */
    "authentication type unavailable for attempted privilege level",    /* 9 */
    "cipher suite id unavailable",                                      /* 10 */
    "password verification timeout",                                    /* 11 */
    "ipmi 2.0 unavailable",                                             /* 12 */
    "connection timeout",                                               /* 13 */
    "session timeout",                                                  /* 14 */
    "device already open",                                              /* 15 */
    "device not open",                                                  /* 16 */
    "device not supported",                                             /* 17 */
    "device not found",                                                 /* 18 */
    "driver busy",			                                /* 19 */
    "driver timeout",                                                   /* 20 */
    "message timeout",                                                  /* 21 */
    "command invalid for selected interface",                           /* 22 */
    "command invalid or unsupported",                                   /* 23 */
    "bad completion code",                                              /* 24 */
    "bad rmcpplus status code",                                         /* 25 */
    "not found",                                                        /* 26 */
    "BMC busy",                                                         /* 27 */
    "out of memory",                                                    /* 28 */
    "invalid hostname",                                                 /* 29 */
    "invalid parameters",                                               /* 30 */
    "driver path required",                                             /* 31 */
    "internal IPMI error",                                              /* 32 */
    "internal system error",                                            /* 33 */
    "internal error",                                                   /* 34 */
    "errnum out of range",                                              /* 35 */
  };

static void
_ipmi_ctx_init (struct ipmi_ctx *ctx)
{
  assert (ctx);

  memset (ctx, '\0', sizeof (struct ipmi_ctx));
  ctx->magic = IPMI_CTX_MAGIC;
  ctx->type = IPMI_DEVICE_UNKNOWN;
}

ipmi_ctx_t
ipmi_ctx_create (void)
{
  struct ipmi_ctx *ctx;

  if (!(ctx = (struct ipmi_ctx *)malloc (sizeof (struct ipmi_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }

  _ipmi_ctx_init (ctx);
  ctx->errnum = IPMI_ERR_SUCCESS;

  return (ctx);
}

int
ipmi_ctx_errnum (ipmi_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_ERR_CTX_NULL);
  else if (ctx->magic != IPMI_CTX_MAGIC)
    return (IPMI_ERR_CTX_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_ERR_SUCCESS && errnum <= IPMI_ERR_ERRNUMRANGE)
    return (ipmi_errmsg[errnum]);
  else
    return (ipmi_errmsg[IPMI_ERR_ERRNUMRANGE]);
}

char *
ipmi_ctx_errormsg (ipmi_ctx_t ctx)
{
  return (ipmi_ctx_strerror (ipmi_ctx_errnum (ctx)));
}

int
ipmi_ctx_get_flags (ipmi_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  (*flags) = ctx->flags;
  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_ctx_set_flags (ipmi_ctx_t ctx, unsigned int flags)
{
  unsigned int flags_mask = (IPMI_FLAGS_NONBLOCKING
			     | IPMI_FLAGS_NOSESSION
                             | IPMI_FLAGS_DEBUG_DUMP
                             | IPMI_FLAGS_NO_VALID_CHECK
			     | IPMI_FLAGS_NO_LEGAL_CHECK
			     | IPMI_FLAGS_IGNORE_AUTHENTICATION_CODE);

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~flags_mask)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  /* Some flags are only configurable at creation time, cannot be
   * changed mid-session.
   */
  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      flags_mask = IPMI_FLAGS_NOSESSION;
      
      if ((ctx->flags & flags_mask) != (flags & flags_mask))
	{
	  API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
	  return (-1);
	}
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);
}

static void
_ipmi_outofband_free (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert (ctx);
  assert (ctx->magic == IPMI_CTX_MAGIC);

  fiid_obj_destroy (ctx->io.outofband.rq.obj_rmcp_hdr);
  ctx->io.outofband.rq.obj_rmcp_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rq.obj_lan_session_hdr);
  ctx->io.outofband.rq.obj_lan_session_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rq.obj_rmcpplus_session_hdr);
  ctx->io.outofband.rq.obj_rmcpplus_session_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rq.obj_lan_msg_hdr);
  ctx->io.outofband.rq.obj_lan_msg_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rq.obj_rmcpplus_session_trlr);
  ctx->io.outofband.rq.obj_rmcpplus_session_trlr = NULL;

  fiid_obj_destroy (ctx->io.outofband.rs.obj_rmcp_hdr);
  ctx->io.outofband.rs.obj_rmcp_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rs.obj_lan_session_hdr);
  ctx->io.outofband.rs.obj_lan_session_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
  ctx->io.outofband.rs.obj_rmcpplus_session_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rs.obj_lan_msg_hdr);
  ctx->io.outofband.rs.obj_lan_msg_hdr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rs.obj_rmcpplus_payload);
  ctx->io.outofband.rs.obj_rmcpplus_payload = NULL;
  fiid_obj_destroy (ctx->io.outofband.rs.obj_lan_msg_trlr);
  ctx->io.outofband.rs.obj_lan_msg_trlr = NULL;
  fiid_obj_destroy (ctx->io.outofband.rs.obj_rmcpplus_session_trlr);
  ctx->io.outofband.rs.obj_rmcpplus_session_trlr = NULL;
}

static void
_ipmi_inband_free (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert (ctx);
  assert (ctx->magic == IPMI_CTX_MAGIC);

  if (ctx->type == IPMI_DEVICE_KCS)
    {
      ipmi_kcs_ctx_destroy (ctx->io.inband.kcs_ctx);
      ctx->io.inband.kcs_ctx = NULL;
    }
  if (ctx->type == IPMI_DEVICE_SSIF)
    {
      ipmi_ssif_ctx_destroy (ctx->io.inband.ssif_ctx);
      ctx->io.inband.ssif_ctx = NULL;
    }
  if (ctx->type == IPMI_DEVICE_OPENIPMI)
    {
      ipmi_openipmi_ctx_destroy (ctx->io.inband.openipmi_ctx);
      ctx->io.inband.openipmi_ctx = NULL;
    }
  if (ctx->type == IPMI_DEVICE_SUNBMC)
    {
      ipmi_sunbmc_ctx_destroy (ctx->io.inband.sunbmc_ctx);
      ctx->io.inband.sunbmc_ctx = NULL;
    }
  if (ctx->type == IPMI_DEVICE_INTELDCMI)
    {
      ipmi_inteldcmi_ctx_destroy (ctx->io.inband.inteldcmi_ctx);
      ctx->io.inband.inteldcmi_ctx = NULL;
    }

  fiid_obj_destroy (ctx->io.inband.rq.obj_hdr);
  ctx->io.inband.rq.obj_hdr = NULL;
  fiid_obj_destroy (ctx->io.inband.rs.obj_hdr);
  ctx->io.inband.rs.obj_hdr = NULL;
}

static int
_setup_hostname (ipmi_ctx_t ctx, const char *hostname)
{
  struct hostent hent;
  int h_errnop;
  char buf[GETHOSTBYNAME_AUX_BUFLEN];
#if defined(HAVE_FUNC_GETHOSTBYNAME_R_6)
  struct hostent *hptr;
#elif defined(HAVE_FUNC_GETHOSTBYNAME_R_5)
#else /* !HAVE_FUNC_GETHOSTBYNAME_R */
  struct hostent *hptr;
#endif /* !HAVE_FUNC_GETHOSTBYNAME_R */
  char *hostname_copy = NULL;
  char *hostname_ptr;
  uint16_t port = RMCP_AUX_BUS_SHUNT;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_CTX_MAGIC);
  assert (hostname);

  if (strchr (hostname, ':'))
    {
      char *ptr;

      if (!(hostname_copy = strdup ((char *)hostname)))
	{
	  API_SET_ERRNUM (ctx, IPMI_ERR_OUT_OF_MEMORY);
	  goto cleanup;
	}

      if ((ptr = strchr (hostname_copy, ':')))
	{
	  char *endptr;
	  int tmp;

	  *ptr = '\0';
	  ptr++;

	  if (strlen (hostname_copy) > MAXHOSTNAMELEN)
	    {
	      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
	      return (-1);
	    }
	  
	  errno = 0;
	  tmp = strtol (ptr, &endptr, 0);
	  if (errno
	      || endptr[0] != '\0'
	      || tmp <= 0
	      || tmp > USHRT_MAX)
	    {
	      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
	      goto cleanup;
	    }
	  
	  port = tmp;
	}
      hostname_ptr = hostname_copy;
    }
  else
    {
      if (strlen (hostname) > MAXHOSTNAMELEN)
	{
	  API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
	  return (-1);
	}

      hostname_ptr = (char *)hostname;
    }
  
  memset (&hent, '\0', sizeof (struct hostent));
#if defined(HAVE_FUNC_GETHOSTBYNAME_R_6)
  if (gethostbyname_r (hostname_ptr,
                       &hent,
                       buf,
                       GETHOSTBYNAME_AUX_BUFLEN,
                       &hptr,
                       &h_errnop))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_HOSTNAME_INVALID);
      goto cleanup;
    }
  if (!hptr)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_HOSTNAME_INVALID);
      goto cleanup;
    }
#elif defined(HAVE_FUNC_GETHOSTBYNAME_R_5)
  /* Jan Forch - Solaris gethostbyname_r returns ptr, not integer */
  if (!gethostbyname_r (hostname_ptr,
                        &hent,
                        buf,
                        GETHOSTBYNAME_AUX_BUFLEN,
                        &h_errnop))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_HOSTNAME_INVALID);
      goto cleanup;
    }
#else  /* !HAVE_FUNC_GETHOSTBYNAME_R */
  if (freeipmi_gethostbyname_r (hostname_ptr,
                                &hent,
                                buf,
                                GETHOSTBYNAME_AUX_BUFLEN,
                                &hptr,
                                &h_errnop))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_HOSTNAME_INVALID);
      goto cleanup;
    }
  if (!hptr)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_HOSTNAME_INVALID);
      goto cleanup;
    }
#endif /* !HAVE_FUNC_GETHOSTBYNAME_R */

  strncpy (ctx->io.outofband.hostname,
           hostname_ptr,
           MAXHOSTNAMELEN);

  ctx->io.outofband.remote_host.sin_family = AF_INET;
  ctx->io.outofband.remote_host.sin_port = htons (port);
  ctx->io.outofband.remote_host.sin_addr = *(struct in_addr *) hent.h_addr;

  rv = 0;
 cleanup:
  free (hostname_copy);
  return (rv);
}

static int
_setup_socket (ipmi_ctx_t ctx)
{
  struct sockaddr_in addr;

  assert (ctx);
  assert (ctx->magic == IPMI_CTX_MAGIC);

  /* Open client (local) UDP socket */
  /* achu: ephemeral ports are > 1023, so no way we will bind to an IPMI port */

  if ((ctx->io.outofband.sockfd = socket (AF_INET,
                                          SOCK_DGRAM,
                                          0)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  memset (&addr, 0, sizeof (struct sockaddr_in));
  addr.sin_family = AF_INET;
  addr.sin_port   = htons (0);
  addr.sin_addr.s_addr = htonl (INADDR_ANY);

  if (bind (ctx->io.outofband.sockfd,
            (struct sockaddr *)&addr,
            sizeof (struct sockaddr_in)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  return (0);
}


int
ipmi_ctx_open_outofband (ipmi_ctx_t ctx,
                         const char *hostname,
                         const char *username,
                         const char *password,
                         uint8_t authentication_type,
                         uint8_t privilege_level,
                         unsigned int session_timeout,
                         unsigned int retransmission_timeout,
                         unsigned int workaround_flags,
                         unsigned int flags)
{
  unsigned int workaround_flags_mask = (IPMI_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER
					| IPMI_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK
					| IPMI_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK);
  unsigned int flags_mask = (IPMI_FLAGS_NOSESSION
			     | IPMI_FLAGS_DEBUG_DUMP
                             | IPMI_FLAGS_NO_VALID_CHECK
			     | IPMI_FLAGS_NO_LEGAL_CHECK);

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* hostname length checks in _setup_hostname() */
  if (!hostname
      || (username && strlen (username) > IPMI_MAX_USER_NAME_LENGTH)
      || (password && strlen (password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
      || !IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type)
      || !IPMI_PRIVILEGE_LEVEL_VALID (privilege_level)
      || (workaround_flags & ~workaround_flags_mask)
      || (flags & ~flags_mask))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_ALREADY_OPEN);
      return (-1);
    }

  ctx->type = IPMI_DEVICE_LAN;
  ctx->workaround_flags_outofband = workaround_flags;
  ctx->flags = flags;

  if (_setup_hostname (ctx, hostname) < 0)
    goto cleanup;

  memset (ctx->io.outofband.username, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
  if (username)
    memcpy (ctx->io.outofband.username,
            username,
            strlen (username));
  memset (ctx->io.outofband.password, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH+1);
  if (password)
    memcpy (ctx->io.outofband.password,
            password,
            strlen (password));
  ctx->io.outofband.privilege_level = privilege_level;
  ctx->io.outofband.session_timeout = (session_timeout ? session_timeout : IPMI_SESSION_TIMEOUT_DEFAULT);
  ctx->io.outofband.retransmission_timeout = (retransmission_timeout ? retransmission_timeout : IPMI_RETRANSMISSION_TIMEOUT_DEFAULT);

  if (ctx->io.outofband.retransmission_timeout >= ctx->io.outofband.session_timeout)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  memset (&ctx->io.outofband.last_send, '\0', sizeof (struct timeval));
  memset (&ctx->io.outofband.last_received, '\0', sizeof (struct timeval));

  if (ipmi_check_session_sequence_number_1_5_init (&(ctx->io.outofband.highest_received_sequence_number),
                                                   &(ctx->io.outofband.previously_received_list)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  ctx->io.outofband.authentication_type = authentication_type;

  if (!(ctx->io.outofband.rq.obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (_setup_socket (ctx) < 0)
    goto cleanup;

  /* errnum set in api_lan_open_session */
  if (api_lan_open_session (ctx) < 0)
    goto cleanup;

  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);

 cleanup:
  /* ignore potential error, cleanup path */
  if (ctx->io.outofband.sockfd)
    close (ctx->io.outofband.sockfd);
  _ipmi_outofband_free (ctx);
  ctx->type = IPMI_DEVICE_UNKNOWN;
  return (-1);
}

int
ipmi_ctx_open_outofband_2_0 (ipmi_ctx_t ctx,
                             const char *hostname,
                             const char *username,
                             const char *password,
                             const unsigned char *k_g,
                             unsigned int k_g_len,
                             uint8_t privilege_level,
                             uint8_t cipher_suite_id,
                             unsigned int session_timeout,
                             unsigned int retransmission_timeout,
                             unsigned int workaround_flags,
                             unsigned int flags)
{
  unsigned int workaround_flags_mask = (IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION
                                        | IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE
					| IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE
					| IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK);
  unsigned int flags_mask = (IPMI_FLAGS_DEBUG_DUMP
                             | IPMI_FLAGS_NO_VALID_CHECK
			     | IPMI_FLAGS_NO_LEGAL_CHECK);

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* hostname length checks in _setup_hostname() */
  if (!hostname
      || (username && strlen (username) > IPMI_MAX_USER_NAME_LENGTH)
      || (password && strlen (password) > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || (k_g && k_g_len > IPMI_MAX_K_G_LENGTH)
      || !IPMI_PRIVILEGE_LEVEL_VALID (privilege_level)
      || !IPMI_CIPHER_SUITE_ID_SUPPORTED (cipher_suite_id)
      || (workaround_flags & ~workaround_flags_mask)
      || (flags & ~flags_mask))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_ALREADY_OPEN);
      return (-1);
    }

  if (ipmi_rmcpplus_init () < 0)
    {
      if (errno == EPERM)
        API_SET_ERRNUM (ctx, IPMI_ERR_SYSTEM_ERROR);
      else
        API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  ctx->type = IPMI_DEVICE_LAN_2_0;
  ctx->workaround_flags_outofband_2_0 = workaround_flags;
  ctx->flags = flags;

  if (_setup_hostname (ctx, hostname) < 0)
    goto cleanup;

  memset (ctx->io.outofband.username, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
  if (username)
    memcpy (ctx->io.outofband.username,
            username,
            strlen (username));

  memset (ctx->io.outofband.password, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH+1);
  if (password)
    memcpy (ctx->io.outofband.password,
            password,
            strlen (password));

  ctx->io.outofband.privilege_level = privilege_level;
  ctx->io.outofband.session_timeout = (session_timeout ? session_timeout : IPMI_SESSION_TIMEOUT_DEFAULT);
  ctx->io.outofband.retransmission_timeout = (retransmission_timeout ? retransmission_timeout : IPMI_RETRANSMISSION_TIMEOUT_DEFAULT);

  if (ctx->io.outofband.retransmission_timeout >= ctx->io.outofband.session_timeout)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  memset (ctx->io.outofband.k_g, '\0', IPMI_MAX_K_G_LENGTH);
  ctx->io.outofband.k_g_configured = 0;
  if (k_g && k_g_len)
    {
      unsigned int i;
      int all_zeroes = 1;

      memcpy (ctx->io.outofband.k_g,
              k_g,
              k_g_len);

      ctx->io.outofband.k_g_configured++;

      /* Special case, check to make sure user didn't input zero as a
       * k_g key.
       */
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
        {
          if (k_g[i] != 0)
            {
              all_zeroes = 0;
              break;
            }
        }
      
      if (all_zeroes)
        ctx->io.outofband.k_g_configured = 0;
    }

  ctx->io.outofband.cipher_suite_id = cipher_suite_id;

  memset (ctx->io.outofband.sik_key, '\0', IPMI_MAX_SIK_KEY_LENGTH);
  ctx->io.outofband.sik_key_ptr = ctx->io.outofband.sik_key;
  ctx->io.outofband.sik_key_len = IPMI_MAX_SIK_KEY_LENGTH;
  memset (ctx->io.outofband.integrity_key, '\0', IPMI_MAX_INTEGRITY_KEY_LENGTH);
  ctx->io.outofband.integrity_key_ptr = ctx->io.outofband.integrity_key;
  ctx->io.outofband.integrity_key_len = IPMI_MAX_INTEGRITY_KEY_LENGTH;
  memset (ctx->io.outofband.confidentiality_key, '\0', IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH);
  ctx->io.outofband.confidentiality_key_ptr = ctx->io.outofband.confidentiality_key;
  ctx->io.outofband.confidentiality_key_len = IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH;
  memset (&ctx->io.outofband.last_send, '\0', sizeof (struct timeval));
  memset (&ctx->io.outofband.last_received, '\0', sizeof (struct timeval));

  if (ipmi_check_session_sequence_number_2_0_init (&(ctx->io.outofband.highest_received_sequence_number),
                                                   &(ctx->io.outofband.previously_received_list)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(ctx->io.outofband.rq.obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_rmcpplus_session_hdr = fiid_obj_create (tmpl_rmcpplus_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_rmcpplus_session_trlr = fiid_obj_create (tmpl_rmcpplus_session_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(ctx->io.outofband.rs.obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcpplus_session_hdr = fiid_obj_create (tmpl_rmcpplus_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcpplus_payload = fiid_obj_create (tmpl_rmcpplus_payload)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcpplus_session_trlr = fiid_obj_create (tmpl_rmcpplus_session_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (_setup_socket (ctx) < 0)
    goto cleanup;

  /* errnum set in api_lan_2_0_open_session */
  if (api_lan_2_0_open_session (ctx) < 0)
    goto cleanup;

  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);

 cleanup:
  /* ignore potential error, cleanup path */
  if (ctx->io.outofband.sockfd)
    close (ctx->io.outofband.sockfd);
  _ipmi_outofband_free (ctx);
  ctx->type = IPMI_DEVICE_UNKNOWN;
  return (-1);
}

int
ipmi_ctx_open_inband (ipmi_ctx_t ctx,
                      ipmi_driver_type_t driver_type,
                      int disable_auto_probe,
                      uint16_t driver_address,
                      uint8_t register_spacing,
                      const char *driver_device,
                      unsigned int workaround_flags,
                      unsigned int flags)
{
  ipmi_locate_ctx_t locate_ctx = NULL;
  struct ipmi_locate_info locate_info;
  unsigned int seedp;
  unsigned int temp_flags = 0;
  unsigned int workaround_flags_mask = (IPMI_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS
					| IPMI_WORKAROUND_FLAGS_INBAND_SPIN_POLL);
  unsigned int flags_mask = (IPMI_FLAGS_NONBLOCKING
                             | IPMI_FLAGS_DEBUG_DUMP
                             | IPMI_FLAGS_NO_VALID_CHECK
			     | IPMI_FLAGS_NO_LEGAL_CHECK);

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  /* No workaround flags currently supported */
  if ((driver_type != IPMI_DEVICE_KCS
       && driver_type != IPMI_DEVICE_SMIC
       && driver_type != IPMI_DEVICE_BT
       && driver_type != IPMI_DEVICE_SSIF
       && driver_type != IPMI_DEVICE_OPENIPMI
       && driver_type != IPMI_DEVICE_SUNBMC
       && driver_type != IPMI_DEVICE_INTELDCMI)
      || (workaround_flags & ~workaround_flags_mask)
      || (flags & ~flags_mask))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_ALREADY_OPEN);
      return (-1);
    }

  ctx->workaround_flags_inband = workaround_flags;
  ctx->flags = flags;

  ctx->io.inband.kcs_ctx = NULL;
  ctx->io.inband.ssif_ctx = NULL;
  ctx->io.inband.openipmi_ctx = NULL;
  ctx->io.inband.sunbmc_ctx = NULL;

  /* Random number generation */
  seedp = (unsigned int) clock () + (unsigned int) time (NULL);
  srand (seedp);

  ctx->io.inband.rq_seq = (double)(IPMI_IPMB_REQUESTER_SEQUENCE_NUMBER_MAX) * (rand ()/(RAND_MAX + 1.0));

  if (!(locate_ctx = ipmi_locate_ctx_create ()))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  switch (driver_type)
    {
    case IPMI_DEVICE_KCS:
      if (disable_auto_probe)
        {
          memset (&locate_info, '\0', sizeof (struct ipmi_locate_info));

          locate_info.ipmi_version_major = 1;
          locate_info.ipmi_version_minor = 5;
          locate_info.locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
          locate_info.interface_type = IPMI_INTERFACE_KCS;
          if (driver_device)
            {
              strncpy (locate_info.driver_device, driver_device, IPMI_LOCATE_PATH_MAX);
              locate_info.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
            }
          locate_info.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
          locate_info.driver_address = driver_address;
          locate_info.register_spacing = register_spacing;
        }
      else
        {
          if (ipmi_locate_get_device_info (locate_ctx,
                                           IPMI_INTERFACE_KCS,
                                           &locate_info) < 0)
            {
              API_LOCATE_ERRNUM_TO_API_ERRNUM (ctx, ipmi_locate_ctx_errnum (locate_ctx));
              goto cleanup;
            }
          if (driver_device)
            {
              strncpy (locate_info.driver_device,
                       driver_device,
                       IPMI_LOCATE_PATH_MAX);
              locate_info.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
            }
          if (driver_address)
            locate_info.driver_address = driver_address;
          if (register_spacing)
            locate_info.register_spacing = register_spacing;
        }
      ctx->type = driver_type;

      /* IPMI Workaround
       *
       * Discovered on HP ProLiant DL145 G1
       *
       * The system uses KCS w/ an I/O base address, but the bit is
       * set incorrectly.  Ignore it if specified by the user.
       *
       * Note: Linux OpenIPMI kernel driver always assumes System I/O.
       * In ipmi_si_intf.c
       *
       * "Note that technically, the lower bit of the base address
       * should be 1 if the address is I/O and 0 if the address is in
       * memory.  So many systems get that wrong (and all that I have
       * seen are I/O) so we just ignore that bit and assume I/O.
       * Systems that use memory should use the newer spec, anyway."
       */
      if (!(ctx->workaround_flags_inband & IPMI_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS))
        {
          /* At this point we only support SYSTEM_IO, i.e. inb/outb style IO.
           * If we cant find the base address, we better exit.
           * -- Anand Babu
           */

          /* achu: IPMI spec indicates KCS is only IO, not Memory
           * based.  Memory based KCS is not part of IPMI spec
           */
          if (locate_info.address_space_id != IPMI_ADDRESS_SPACE_ID_SYSTEM_IO)
            {
              API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_SUPPORTED);
              goto cleanup;
            }
        }

      if (!(ctx->io.inband.kcs_ctx = ipmi_kcs_ctx_create ()))
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (ipmi_kcs_ctx_set_driver_address (ctx->io.inband.kcs_ctx,
                                           locate_info.driver_address) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM (ctx, ipmi_kcs_ctx_errnum (ctx->io.inband.kcs_ctx));
          goto cleanup;
        }

      if (ipmi_kcs_ctx_set_register_spacing (ctx->io.inband.kcs_ctx,
                                             locate_info.register_spacing) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM (ctx, ipmi_kcs_ctx_errnum (ctx->io.inband.kcs_ctx));
          goto cleanup;
        }

      if (ipmi_kcs_ctx_set_poll_interval (ctx->io.inband.kcs_ctx,
                                          IPMI_POLL_INTERVAL_USECS) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM (ctx, ipmi_kcs_ctx_errnum (ctx->io.inband.kcs_ctx));
          goto cleanup;
        }

      if (ctx->flags & IPMI_FLAGS_NONBLOCKING)
        temp_flags |= IPMI_KCS_FLAGS_NONBLOCKING;

      if (ctx->workaround_flags_inband & IPMI_WORKAROUND_FLAGS_INBAND_SPIN_POLL)
	temp_flags |= IPMI_KCS_FLAGS_SPIN_POLL;

      if (ipmi_kcs_ctx_set_flags (ctx->io.inband.kcs_ctx, temp_flags) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM (ctx, ipmi_kcs_ctx_errnum (ctx->io.inband.kcs_ctx));
          goto cleanup;
        }

      if (ipmi_kcs_ctx_io_init (ctx->io.inband.kcs_ctx) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM (ctx, ipmi_kcs_ctx_errnum (ctx->io.inband.kcs_ctx));
          goto cleanup;
        }

      break;
    case IPMI_DEVICE_SMIC:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;
      goto cleanup;
    case IPMI_DEVICE_BT:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;
      goto cleanup;
    case IPMI_DEVICE_SSIF:
      if (disable_auto_probe)
        {
          memset (&locate_info, '\0', sizeof (struct ipmi_locate_info));

          locate_info.ipmi_version_major = 1;
          locate_info.ipmi_version_minor = 5;
          locate_info.locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
          locate_info.interface_type = IPMI_INTERFACE_SSIF;
          if (driver_device)
            {
              strncpy (locate_info.driver_device, driver_device, IPMI_LOCATE_PATH_MAX);
              locate_info.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
            }
          locate_info.address_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
          locate_info.driver_address = driver_address;
          locate_info.register_spacing = register_spacing;
        }
      else
        {
          if (ipmi_locate_get_device_info (locate_ctx,
                                           IPMI_INTERFACE_SSIF,
                                           &locate_info) < 0)
            {
              API_LOCATE_ERRNUM_TO_API_ERRNUM (ctx, ipmi_locate_ctx_errnum (locate_ctx));
              goto cleanup;
            }
          if (driver_device)
            {
              strncpy (locate_info.driver_device,
                       driver_device,
                       IPMI_LOCATE_PATH_MAX);
              locate_info.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
            }
          if (driver_address)
            locate_info.driver_address = driver_address;
          if (register_spacing)
            locate_info.register_spacing = register_spacing;
        }
      ctx->type = driver_type;

      if (!(ctx->io.inband.ssif_ctx = ipmi_ssif_ctx_create ()))
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (driver_device)
        {
          if (ipmi_ssif_ctx_set_driver_device (ctx->io.inband.ssif_ctx,
                                               locate_info.driver_device) < 0)
            {
              API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
              goto cleanup;
            }
        }

      if (ipmi_ssif_ctx_set_driver_address (ctx->io.inband.ssif_ctx,
                                            locate_info.driver_address) < 0)
        {
          API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
          goto cleanup;
        }

      if (ctx->flags & IPMI_FLAGS_NONBLOCKING)
        temp_flags |= IPMI_SSIF_FLAGS_NONBLOCKING;

      if (ipmi_ssif_ctx_set_flags (ctx->io.inband.ssif_ctx, temp_flags) < 0)
        {
          API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
          goto cleanup;
        }

      if (ipmi_ssif_ctx_io_init (ctx->io.inband.ssif_ctx) < 0)
        {
          API_SSIF_ERRNUM_TO_API_ERRNUM (ctx, ipmi_ssif_ctx_errnum (ctx->io.inband.ssif_ctx));
          goto cleanup;
        }

      break;

    case IPMI_DEVICE_OPENIPMI:
      ctx->type = driver_type;

      if (!(ctx->io.inband.openipmi_ctx = ipmi_openipmi_ctx_create ()))
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (driver_device)
        {
          if (ipmi_openipmi_ctx_set_driver_device (ctx->io.inband.openipmi_ctx,
                                                   driver_device) < 0)
            {
              API_OPENIPMI_ERRNUM_TO_API_ERRNUM (ctx, ipmi_openipmi_ctx_errnum (ctx->io.inband.openipmi_ctx));
              goto cleanup;
            }
        }

      if (ipmi_openipmi_ctx_io_init (ctx->io.inband.openipmi_ctx) < 0)
        {
          API_OPENIPMI_ERRNUM_TO_API_ERRNUM (ctx, ipmi_openipmi_ctx_errnum (ctx->io.inband.openipmi_ctx));
          goto cleanup;
        }

      break;

    case IPMI_DEVICE_SUNBMC:
      ctx->type = driver_type;

      if (!(ctx->io.inband.sunbmc_ctx = ipmi_sunbmc_ctx_create ()))
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (driver_device)
        {
          if (ipmi_sunbmc_ctx_set_driver_device (ctx->io.inband.sunbmc_ctx,
                                                 driver_device) < 0)
            {
              API_SUNBMC_ERRNUM_TO_API_ERRNUM (ctx, ipmi_sunbmc_ctx_errnum (ctx->io.inband.sunbmc_ctx));
              goto cleanup;
            }
        }

      if (ipmi_sunbmc_ctx_io_init (ctx->io.inband.sunbmc_ctx) < 0)
        {
          API_SUNBMC_ERRNUM_TO_API_ERRNUM (ctx, ipmi_sunbmc_ctx_errnum (ctx->io.inband.sunbmc_ctx));
          goto cleanup;
        }

      break;

    case IPMI_DEVICE_INTELDCMI:
      ctx->type = driver_type;

      if (!(ctx->io.inband.inteldcmi_ctx = ipmi_inteldcmi_ctx_create ()))
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      if (driver_device)
        {
          if (ipmi_inteldcmi_ctx_set_driver_device (ctx->io.inband.inteldcmi_ctx,
						    driver_device) < 0)
            {
              API_INTELDCMI_ERRNUM_TO_API_ERRNUM (ctx, ipmi_inteldcmi_ctx_errnum (ctx->io.inband.inteldcmi_ctx));
              goto cleanup;
            }
        }
      
      if (ipmi_inteldcmi_ctx_io_init (ctx->io.inband.inteldcmi_ctx) < 0)
        {
          API_INTELDCMI_ERRNUM_TO_API_ERRNUM (ctx, ipmi_inteldcmi_ctx_errnum (ctx->io.inband.inteldcmi_ctx));
          goto cleanup;
        }

      break;

    default:
      goto cleanup;
    }

  /* Prepare in-band headers */
  if (!(ctx->io.inband.rq.obj_hdr = fiid_obj_create (tmpl_hdr_kcs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.inband.rs.obj_hdr = fiid_obj_create (tmpl_hdr_kcs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  ipmi_locate_ctx_destroy (locate_ctx);
  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);

 cleanup:
  ipmi_locate_ctx_destroy (locate_ctx);
  _ipmi_inband_free (ctx);
  ctx->type = IPMI_DEVICE_UNKNOWN;
  return (-1);
}

static int
_is_ctx_fatal_error (ipmi_ctx_t ctx)
{
  /* some are fatal b/c they are outfoband and shouldn't happen */
  /* note parameters is not fatal, for some drivers, inputs from users may not be ok */
  /* note internal error is not fatal, could be a bad errno from a syscall */
  
  assert (ctx);
  assert (ctx->magic == IPMI_CTX_MAGIC);

  if (ipmi_ctx_errnum (ctx) == IPMI_ERR_CTX_NULL
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_CTX_INVALID
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_USERNAME_INVALID
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_PASSWORD_INVALID
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_IPMI_2_0_UNAVAILABLE
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_CONNECTION_TIMEOUT
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_SESSION_TIMEOUT
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_DEVICE_ALREADY_OPEN
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_DEVICE_NOT_OPEN
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_NOT_FOUND
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_OUT_OF_MEMORY
      || ipmi_ctx_errnum (ctx) == IPMI_ERR_HOSTNAME_INVALID)
    return (1);

  return (0);
}

static int
_is_locate_ctx_fatal_error (ipmi_locate_ctx_t locate_ctx)
{
  /* some are fatal b/c they are outfoband and shouldn't happen */
  /* note parameters is not fatal, for some drivers, inputs from users may not be ok */
  /* note internal error is not fatal, could be a bad errno from a syscall */
  
  assert (locate_ctx);

  if (ipmi_locate_ctx_errnum (locate_ctx) == IPMI_LOCATE_ERR_NULL
      || ipmi_locate_ctx_errnum (locate_ctx) == IPMI_LOCATE_ERR_INVALID
      || ipmi_locate_ctx_errnum (locate_ctx) == IPMI_LOCATE_ERR_OUT_OF_MEMORY)
    return (1);

  return (0);
}

int
ipmi_ctx_find_inband (ipmi_ctx_t ctx,
                      ipmi_driver_type_t *driver_type,
                      int disable_auto_probe,
                      uint16_t driver_address,
                      uint8_t register_spacing,
                      const char *driver_device,
                      unsigned int workaround_flags,
                      unsigned int flags)
{
  ipmi_locate_ctx_t locate_ctx = NULL;
  struct ipmi_locate_info locate_info;
  int ret, rv = -1;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }
  
  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_ALREADY_OPEN);
      return (-1);
    }

  if (!(locate_ctx = ipmi_locate_ctx_create ()))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* achu
   *
   * Try OpenIPMI, SunBMC, and IntelDCMI drivers first, since they
   * cannot be found via probing.  Do it before probing for KCS/SSIF,
   * because it is possible, even though the OpenIPMI/SunBMC driver is
   * installed, probing may find KCS/SSIF anyways, and try to use
   * those first/instead.
   */
  if ((ret = ipmi_ctx_open_inband (ctx,
                                   IPMI_DEVICE_OPENIPMI,
                                   disable_auto_probe,
                                   driver_address,
                                   register_spacing,
                                   driver_device,
                                   workaround_flags,
                                   flags)) < 0)
    {
      if (_is_ctx_fatal_error (ctx))
        goto cleanup;
    }
  
  if (!ret)
    {
      rv = 1;
      goto out;
    }
  
  if ((ret = ipmi_ctx_open_inband (ctx,
                                   IPMI_DEVICE_SUNBMC,
                                   disable_auto_probe,
                                   driver_address,
                                   register_spacing,
                                   driver_device,
                                   workaround_flags,
                                   flags)) < 0)
    {
      if (_is_ctx_fatal_error (ctx))
        goto cleanup;
    }

  if ((ret = ipmi_ctx_open_inband (ctx,
                                   IPMI_DEVICE_INTELDCMI,
                                   disable_auto_probe,
                                   driver_address,
                                   register_spacing,
                                   driver_device,
                                   workaround_flags,
                                   flags)) < 0)
    {
      if (_is_ctx_fatal_error (ctx))
        goto cleanup;
    }
  
  if (!ret)
    {
      rv = 1;
      goto out;
    }
  
  /* achu
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
      
  if ((ret = ipmi_locate_discover_device_info (locate_ctx,
                                               IPMI_INTERFACE_KCS,
                                               &locate_info)) < 0)
    {
      if (_is_locate_ctx_fatal_error (locate_ctx))
        goto cleanup;
    }
  
  if (!ret)
    {
      if ((ret = ipmi_ctx_open_inband (ctx,
                                       IPMI_DEVICE_KCS,
                                       disable_auto_probe,
                                       driver_address,
                                       register_spacing,
                                       driver_device,
                                       workaround_flags,
                                       flags)) < 0)
        {
          if (_is_ctx_fatal_error (ctx))
            goto cleanup;
        }
      
      if (!ret)
        {
          rv = 1;
          goto out;
        }
    }

  if (!ipmi_locate_discover_device_info (locate_ctx,
                                         IPMI_INTERFACE_SSIF,
                                         &locate_info))
    {
      if (_is_locate_ctx_fatal_error (locate_ctx))
        goto cleanup;
    }

  if (!ret)
    {
      if ((ret = ipmi_ctx_open_inband (ctx,
                                       IPMI_DEVICE_SSIF,
                                       disable_auto_probe,
                                       driver_address,
                                       register_spacing,
                                       driver_device,
                                       workaround_flags,
                                       flags)) < 0)
        {
          if (_is_ctx_fatal_error (ctx))
            goto cleanup;
        }
      
      if (!ret)
        {
          rv = 1;
          goto out;
        }
    }

  /* achu
   *
   * If KCS/SSIF was not discovered, try plain old default values
   */

  if ((ret = ipmi_ctx_open_inband (ctx,
                                   IPMI_DEVICE_KCS,
                                   disable_auto_probe,
                                   driver_address,
                                   register_spacing,
                                   driver_device,
                                   workaround_flags,
                                   flags)) < 0)
    {
      if (_is_ctx_fatal_error (ctx))
        goto cleanup;
    }
  
  if (!ret)
    {
      rv = 1;
      goto out;
    }
  
  if ((ret = ipmi_ctx_open_inband (ctx,
                                   IPMI_DEVICE_SSIF,
                                   disable_auto_probe,
                                   driver_address,
                                   register_spacing,
                                   driver_device,
                                   workaround_flags,
                                   flags)) < 0)
    {
      if (_is_ctx_fatal_error (ctx))
        goto cleanup;
    }

  if (!ret)
    {
      rv = 1;
      goto out;
    }

  rv = 0;
 out:
  if (driver_type)
    (*driver_type) = ctx->type;
  ctx->errnum = IPMI_ERR_SUCCESS;
 cleanup:
  ipmi_locate_ctx_destroy (locate_ctx);
  return (rv);
}

int
ipmi_ctx_set_target (ipmi_ctx_t ctx,
		     uint8_t *channel_number,
		     uint8_t *rs_addr)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (!channel_number && !rs_addr)
    {
      ctx->target.channel_number_is_set = 0;
      ctx->target.rs_addr_is_set = 0;
      goto out;
    }
  
  /* Special case, don't want to do bridging */
  if ((channel_number
       && (*channel_number) == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB)
      && (rs_addr
	  && (*rs_addr) == IPMI_SLAVE_ADDRESS_BMC))
    {
      ctx->target.channel_number_is_set = 0;
      ctx->target.rs_addr_is_set = 0;
      goto out;
    }

  if (channel_number)
    {
      if (!IPMI_CHANNEL_NUMBER_VALID (*channel_number))
	{
	  API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
	  return (-1);
	}
    }

  if (channel_number)
    ctx->target.channel_number = *channel_number;
  else
    ctx->target.channel_number = IPMI_CHANNEL_NUMBER_PRIMARY_IPMB;
  ctx->target.channel_number_is_set = 1;
  
  if (rs_addr)
    ctx->target.rs_addr = *rs_addr;
  else
    ctx->target.rs_addr = IPMI_SLAVE_ADDRESS_BMC;
  ctx->target.rs_addr_is_set = 1;
  
 out:
  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_ctx_get_target (ipmi_ctx_t ctx,
		     uint8_t *channel_number,
		     uint8_t *rs_addr)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }
  
  if (channel_number)
    {
      if (ctx->target.channel_number_is_set)
	(*channel_number) = ctx->target.channel_number;
      else
	(*channel_number) = IPMI_CHANNEL_NUMBER_PRIMARY_IPMB;
    }

  if (rs_addr)
    {
      if (ctx->target.rs_addr_is_set)
	(*rs_addr) = ctx->target.rs_addr;
      else
	(*rs_addr) = IPMI_SLAVE_ADDRESS_BMC;
    }

  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_cmd (ipmi_ctx_t ctx,
          uint8_t lun,
          uint8_t net_fn,
          fiid_obj_t obj_cmd_rq,
          fiid_obj_t obj_cmd_rs)
{
  int rv = 0;

  /* achu:
   *
   * Thanks to the OpenIPMI folks and tcpdumps from their project. I
   * had trouble figuring out a few chunks of the bridging code.
   */

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }
  
  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }
  
  if (ctx->flags & IPMI_FLAGS_NOSESSION
      && ctx->type != IPMI_DEVICE_LAN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }
  
  if (ctx->type != IPMI_DEVICE_LAN
      && ctx->type != IPMI_DEVICE_LAN_2_0
      && ctx->type != IPMI_DEVICE_KCS
      && ctx->type != IPMI_DEVICE_SSIF
      && ctx->type != IPMI_DEVICE_OPENIPMI
      && ctx->type != IPMI_DEVICE_SUNBMC
      && ctx->type != IPMI_DEVICE_INTELDCMI)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }
  
  if (!fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (!fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (FIID_OBJ_PACKET_VALID (obj_cmd_rq) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      return (-1);
    }

  ctx->target.lun = lun;
  ctx->target.net_fn = net_fn;

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* lan packets are dumped in ipmi lan code */
      /* kcs packets are dumped in kcs code */
      /* ssif packets are dumped in ssif code */
      if (ctx->type != IPMI_DEVICE_LAN
          && ctx->type != IPMI_DEVICE_LAN_2_0
          && ctx->type != IPMI_DEVICE_KCS
          && ctx->type != IPMI_DEVICE_SSIF)
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
          uint8_t cmd = 0;
	  uint8_t group_extension = 0;
          uint64_t val;

          /* ignore error, continue on */
          if (FIID_OBJ_GET (obj_cmd_rq,
                            "cmd",
                            &val) < 0)
            API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
          else
            cmd = val;

	  if (IPMI_NET_FN_GROUP_EXTENSION (ctx->target.net_fn))
	    {
	      /* ignore error, continue on */
	      if (FIID_OBJ_GET (obj_cmd_rq,
				"group_extension_identification",
				&val) < 0)
		API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
	      else
		group_extension = val;
	    }

          debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                         DEBUG_UTIL_DIRECTION_REQUEST,
                         ctx->target.net_fn,
                         cmd,
			 group_extension,
                         hdrbuf,
                         DEBUG_UTIL_HDR_BUFLEN);

          if (ctx->tmpl_ipmb_cmd_rq
              && cmd == IPMI_CMD_SEND_MESSAGE)
            {
              ipmi_obj_dump_ipmb (STDERR_FILENO,
                                  NULL,
                                  hdrbuf,
                                  NULL,
                                  obj_cmd_rq,
                                  tmpl_ipmb_msg_hdr_rq,
                                  ctx->tmpl_ipmb_cmd_rq);
            }
          else
            ipmi_obj_dump (STDERR_FILENO,
                           NULL,
                           hdrbuf,
                           NULL,
                           obj_cmd_rq);
        }
    }

  if (ctx->type == IPMI_DEVICE_LAN)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_lan_cmd_ipmb (ctx,
			       obj_cmd_rq,
			       obj_cmd_rs);
      else
	rv = api_lan_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
    }
  else if (ctx->type == IPMI_DEVICE_LAN_2_0)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_lan_2_0_cmd_ipmb (ctx,
				   obj_cmd_rq,
				   obj_cmd_rs);
      else
	rv = api_lan_2_0_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
    }
  else if (ctx->type == IPMI_DEVICE_KCS)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_kcs_cmd_ipmb (ctx,
			       obj_cmd_rq,
			       obj_cmd_rs);
      else
	rv = api_kcs_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
    }
  else if (ctx->type == IPMI_DEVICE_SSIF)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_ssif_cmd_ipmb (ctx,
				obj_cmd_rq,
				obj_cmd_rs);
      else
	rv = api_ssif_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
    }
  else if (ctx->type == IPMI_DEVICE_OPENIPMI)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_openipmi_cmd_ipmb (ctx,
				    obj_cmd_rq,
				    obj_cmd_rs);
      else
	rv = api_openipmi_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
    }
  else if (ctx->type == IPMI_DEVICE_SUNBMC)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	{
	  API_SET_ERRNUM (ctx, IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE);
	  rv = -1;
	}
      else
	rv = api_sunbmc_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
    }
  else /* ctx->type == IPMI_DEVICE_INTELDCMI */
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_inteldcmi_cmd_ipmb (ctx,
				    obj_cmd_rq,
				    obj_cmd_rs);
      else
	rv = api_inteldcmi_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* lan packets are dumped in ipmi lan code */
      /* kcs packets are dumped in kcs code */
      /* ssif packets are dumped in ssif code */
      if (ctx->type != IPMI_DEVICE_LAN
          && ctx->type != IPMI_DEVICE_LAN_2_0
          && ctx->type != IPMI_DEVICE_KCS
          && ctx->type != IPMI_DEVICE_SSIF)
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
          uint8_t cmd = 0;
	  uint8_t group_extension = 0;
          uint64_t val;

          /* ignore error, continue on */
          if (FIID_OBJ_GET (obj_cmd_rq,
                            "cmd",
                            &val) < 0)
            API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
          else
            cmd = val;

	  if (IPMI_NET_FN_GROUP_EXTENSION (ctx->target.net_fn))
	    {
	      /* ignore error, continue on */
	      if (FIID_OBJ_GET (obj_cmd_rq,
				"group_extension_identification",
				&val) < 0)
		API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
	      else
		group_extension = val;
	    }

          /* its ok to use the "request" net_fn */
          debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                         DEBUG_UTIL_DIRECTION_RESPONSE,
                         ctx->target.net_fn,
                         cmd,
			 group_extension,
                         hdrbuf,
                         DEBUG_UTIL_HDR_BUFLEN);

          if (ctx->tmpl_ipmb_cmd_rs
              && cmd == IPMI_CMD_GET_MESSAGE)
            {
              ipmi_obj_dump_ipmb (STDERR_FILENO,
                                  NULL,
                                  hdrbuf,
                                  NULL,
                                  obj_cmd_rs,
                                  tmpl_ipmb_msg_hdr_rs,
                                  ctx->tmpl_ipmb_cmd_rs);
            }
          else
            ipmi_obj_dump (STDERR_FILENO,
                           NULL,
                           hdrbuf,
                           NULL,
                           obj_cmd_rs);
        }
    }

  /* errnum set in ipmi_*_cmd functions */
  return (rv);
}

int
ipmi_cmd_ipmb (ipmi_ctx_t ctx,
               uint8_t channel_number,
               uint8_t rs_addr,
               uint8_t lun,
               uint8_t net_fn,
               fiid_obj_t obj_cmd_rq,
               fiid_obj_t obj_cmd_rs)
{
  struct ipmi_ctx_target target_save;
  int rv = 0;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  memcpy (&target_save, &ctx->target, sizeof (target_save));
  
  ctx->target.channel_number = channel_number;
  ctx->target.channel_number_is_set = 1;
  ctx->target.rs_addr = rs_addr;
  ctx->target.rs_addr_is_set = 1;
  
  rv = ipmi_cmd (ctx, lun, net_fn, obj_cmd_rq, obj_cmd_rs);
  
  memcpy (&ctx->target, &target_save, sizeof (target_save));
  
  /* errnum set in ipmi_cmd() */
  return (rv);
}

int
ipmi_cmd_raw (ipmi_ctx_t ctx,
              uint8_t lun,
              uint8_t net_fn,
              const void *buf_rq,
              unsigned int buf_rq_len,
              void *buf_rs,
              unsigned int buf_rs_len)
{
  int rv = 0;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!buf_rq
      || !buf_rq_len
      || !buf_rs
      || !buf_rs_len)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (!IPMI_NET_FN_RQ_VALID (net_fn))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (ctx->flags & IPMI_FLAGS_NOSESSION)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_LAN
      && ctx->type != IPMI_DEVICE_LAN_2_0
      && ctx->type != IPMI_DEVICE_KCS
      && ctx->type != IPMI_DEVICE_SSIF
      && ctx->type != IPMI_DEVICE_OPENIPMI
      && ctx->type != IPMI_DEVICE_SUNBMC
      && ctx->type != IPMI_DEVICE_INTELDCMI)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  ctx->target.lun = lun;
  ctx->target.net_fn = net_fn;

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* lan packets are dumped in ipmi lan code */
      /* kcs packets are dumped in kcs code */
      /* ssif packets are dumped in ssif code */
      if (ctx->type != IPMI_DEVICE_LAN
          && ctx->type != IPMI_DEVICE_LAN_2_0
          && ctx->type != IPMI_DEVICE_KCS
          && ctx->type != IPMI_DEVICE_SSIF)
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
          uint8_t cmd = 0;
	  uint8_t group_extension = 0;

          cmd = ((uint8_t *)buf_rq)[0];
	  if (IPMI_NET_FN_GROUP_EXTENSION (ctx->target.net_fn))
	    {
	      if (buf_rq_len > 1)
		group_extension = ((uint8_t *)buf_rq)[1];
	    }
    
          debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                         DEBUG_UTIL_DIRECTION_REQUEST,
                         ctx->target.net_fn,
                         cmd,
			 group_extension,
                         hdrbuf,
                         DEBUG_UTIL_HDR_BUFLEN);

          ipmi_dump_hex (STDERR_FILENO,
                         NULL,
                         hdrbuf,
                         NULL,
                         buf_rq,
                         buf_rq_len);
        }
    }

  if (ctx->type == IPMI_DEVICE_LAN)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_lan_cmd_raw_ipmb (ctx,
				   buf_rq,
				   buf_rq_len,
				   buf_rs,
				   buf_rs_len);
      else
	rv = api_lan_cmd_raw (ctx, buf_rq, buf_rq_len, buf_rs, buf_rs_len);
    }
  else if (ctx->type == IPMI_DEVICE_LAN_2_0)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_lan_2_0_cmd_raw_ipmb (ctx,
				       buf_rq,
				       buf_rq_len,
				       buf_rs,
				       buf_rs_len);
      else
	rv = api_lan_2_0_cmd_raw (ctx, buf_rq, buf_rq_len, buf_rs, buf_rs_len);
    }
  else if (ctx->type == IPMI_DEVICE_KCS)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_kcs_cmd_raw_ipmb (ctx,
				   buf_rq,
				   buf_rq_len,
				   buf_rs,
				   buf_rs_len);
      else
	rv = api_kcs_cmd_raw (ctx, buf_rq, buf_rq_len, buf_rs, buf_rs_len);
    }
  else if (ctx->type == IPMI_DEVICE_SSIF)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_ssif_cmd_raw_ipmb (ctx,
				    buf_rq,
				    buf_rq_len,
				    buf_rs,
				    buf_rs_len);
      else
	rv = api_ssif_cmd_raw (ctx, buf_rq, buf_rq_len, buf_rs, buf_rs_len);
    }
  else if (ctx->type == IPMI_DEVICE_OPENIPMI)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_openipmi_cmd_raw_ipmb (ctx,
					buf_rq,
					buf_rq_len,
					buf_rs,
					buf_rs_len);
      else
	rv = api_openipmi_cmd_raw (ctx, buf_rq, buf_rq_len, buf_rs, buf_rs_len);
    }
  else if (ctx->type == IPMI_DEVICE_SUNBMC)
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	{
	  API_SET_ERRNUM (ctx, IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE);
	  rv = -1;
	}
      else
	rv = api_sunbmc_cmd_raw (ctx, buf_rq, buf_rq_len, buf_rs, buf_rs_len);
    }
  else /* ctx->type == IPMI_DEVICE_INTELDCMI */
    {
      if (ctx->target.channel_number_is_set
	  && ctx->target.rs_addr_is_set)
	rv = api_inteldcmi_cmd_raw_ipmb (ctx,
					buf_rq,
					buf_rq_len,
					buf_rs,
					buf_rs_len);
      else
	rv = api_inteldcmi_cmd_raw (ctx, buf_rq, buf_rq_len, buf_rs, buf_rs_len);
    }

  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && rv >= 0)
    {
      /* lan packets are dumped in ipmi lan code */
      /* kcs packets are dumped in kcs code */
      /* ssif packets are dumped in ssif code */
      if (ctx->type != IPMI_DEVICE_LAN
          && ctx->type != IPMI_DEVICE_LAN_2_0
          && ctx->type != IPMI_DEVICE_KCS
          && ctx->type != IPMI_DEVICE_SSIF)
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
          uint8_t cmd = 0;
	  uint8_t group_extension = 0;

          cmd = ((uint8_t *)buf_rq)[0];
	  if (IPMI_NET_FN_GROUP_EXTENSION (ctx->target.net_fn))
	    {
	      if (buf_rq_len > 1)
		group_extension = ((uint8_t *)buf_rq)[1];
	    }

          /* its ok to use the "request" net_fn */
          debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                         DEBUG_UTIL_DIRECTION_RESPONSE,
                         ctx->target.net_fn,
                         cmd,
			 group_extension,
                         hdrbuf,
                         DEBUG_UTIL_HDR_BUFLEN);

          ipmi_dump_hex (STDERR_FILENO,
                         NULL,
                         hdrbuf,
                         NULL,
                         buf_rs,
                         rv);
        }
    }

  /* errnum set in ipmi_*_cmd_raw functions */
  return (rv);
}

int
ipmi_cmd_raw_ipmb (ipmi_ctx_t ctx,
		   uint8_t channel_number,
		   uint8_t rs_addr,
		   uint8_t lun,
		   uint8_t net_fn,
		   const void *buf_rq,
		   unsigned int buf_rq_len,
		   void *buf_rs,
		   unsigned int buf_rs_len)
{
  struct ipmi_ctx_target target_save;
  int rv = 0;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  memcpy (&target_save, &ctx->target, sizeof (target_save));

  ctx->target.channel_number = channel_number;
  ctx->target.channel_number_is_set = 1;
  ctx->target.rs_addr = rs_addr;
  ctx->target.rs_addr_is_set = 1;

  rv = ipmi_cmd_raw (ctx,
		     lun,
		     net_fn,
		     buf_rq,
		     buf_rq_len,
		     buf_rs,
		     buf_rs_len);

  memcpy (&ctx->target, &target_save, sizeof (target_save));

  /* errnum set in ipmi_cmd_raw() */
  return (rv);
}

static void
_ipmi_outofband_close (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN);

  /* No need to set errnum - if the anything in close session
   * fails, session will eventually timeout anyways
   */
  if (!(ctx->flags & IPMI_FLAGS_NOSESSION))
    {
      if (api_lan_close_session (ctx) < 0)
	goto cleanup;
    }

 cleanup:
  /* ignore potential error, destroy path */
  if (ctx->io.outofband.sockfd)
    close (ctx->io.outofband.sockfd);
  _ipmi_outofband_free (ctx);
}

static void
_ipmi_outofband_2_0_close (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_LAN_2_0);
  
  /* No need to set errnum - if the anything in close session
   * fails, session will eventually timeout anyways
   */

  if (api_lan_2_0_close_session (ctx) < 0)
    goto cleanup;

 cleanup:
  /* ignore potential error, destroy path */
  if (ctx->io.outofband.sockfd)
    close (ctx->io.outofband.sockfd);
  _ipmi_outofband_free (ctx);
}

static void
_ipmi_inband_close (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->type == IPMI_DEVICE_KCS
              || ctx->type == IPMI_DEVICE_SMIC
              || ctx->type == IPMI_DEVICE_BT
              || ctx->type == IPMI_DEVICE_SSIF
              || ctx->type == IPMI_DEVICE_OPENIPMI
              || ctx->type == IPMI_DEVICE_SUNBMC
              || ctx->type == IPMI_DEVICE_INTELDCMI));

  _ipmi_inband_free (ctx);
}

int
ipmi_ctx_close (ipmi_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_LAN
      && ctx->type != IPMI_DEVICE_LAN_2_0
      && ctx->type != IPMI_DEVICE_KCS
      && ctx->type != IPMI_DEVICE_SMIC
      && ctx->type != IPMI_DEVICE_BT
      && ctx->type != IPMI_DEVICE_SSIF
      && ctx->type != IPMI_DEVICE_OPENIPMI
      && ctx->type != IPMI_DEVICE_SUNBMC
      && ctx->type != IPMI_DEVICE_INTELDCMI)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  /* closing session - end channel/slave targeting */
  ctx->target.channel_number_is_set = 0;
  ctx->target.rs_addr_is_set = 0;
  
  if (ctx->type == IPMI_DEVICE_LAN)
    _ipmi_outofband_close (ctx);
  else if (ctx->type == IPMI_DEVICE_LAN_2_0)
    _ipmi_outofband_2_0_close (ctx);
  else
    _ipmi_inband_close (ctx);

  ctx->type = IPMI_DEVICE_UNKNOWN;
  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);
}

void
ipmi_ctx_destroy (ipmi_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return;
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    ipmi_ctx_close (ctx);

  secure_memset (ctx, '\0', sizeof (struct ipmi_ctx));
  free (ctx);
}
