/* 
   Copyright (C) 2003-2009 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

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
#include <assert.h>
#include <errno.h>

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/cmds/ipmi-event-cmds.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/driver/ipmi-sunbmc-driver.h"
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
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-ctx.h"
#include "ipmi-trace-wrappers-api.h"
#include "ipmi-lan-interface-api.h"
#include "ipmi-lan-session-common.h"
#include "ipmi-kcs-driver-api.h"
#include "ipmi-openipmi-driver-api.h"
#include "ipmi-sunbmc-driver-api.h"
#include "ipmi-ssif-driver-api.h"

#include "libcommon/ipmi-crypt.h"

#include "freeipmi-portability.h"
#include "debug-util.h"
#include "secure.h"

#define IPMI_SESSION_TIMEOUT         20000
#define IPMI_RETRANSMISSION_TIMEOUT  1000
#define IPMI_POLL_INTERVAL_USECS     60

#define GETHOSTBYNAME_AUX_BUFLEN     1024
extern int h_errno;

static char *ipmi_errmsg[] =
  {
    "success",			                                      /* 0 */
    "device null", 	                                              /* 1 */
    "device invalid",		                                      /* 2 */
    "permission denied",	                                      /* 3 */
    "username invalid",		                                      /* 4 */
    "password invalid",		                                      /* 5 */
    "k_g invalid",                                                    /* 6 */
    "privilege level insufficient",                                   /* 7 */
    "privilege level cannot be obtained for this user",               /* 8 */
    "authentication type unavailable for attempted privilege level",  /* 9 */
    "cipher suite id unavailable",                                    /* 10 */
    "password verification timeout",                                  /* 11 */
    "ipmi 2.0 unavailable",                                           /* 12 */
    "connection timeout",                                             /* 13 */
    "session timeout",		                                      /* 14 */
    "device already open",                                            /* 15 */
    "device not open",		                                      /* 16 */
    "device not supported",	                                      /* 17 */
    "device not found",                                               /* 18 */
    "driver timeout",                                                 /* 19 */
    "message timeout",		                                      /* 20 */
    "command invalid for selected interface",                         /* 21 */
    "bad completion code: node busy/out of resources",                /* 22 */
    "bad completion code: command invalid/not supported",             /* 23 */
    "bad completion code: request data/parameter invalid", 	      /* 24 */
    "bad completion code",	                                      /* 25 */
    "bad rmcpplus status code",                                       /* 26 */
    "not found",                                                      /* 27 */
    "BMC busy",			                                      /* 28 */
    "out of memory",		                                      /* 29 */
    "invalid hostname",                                               /* 30 */
    "invalid parameters",	                                      /* 31 */
    "driver path required",                                           /* 32 */
    "internal IPMI error",	                                      /* 33 */
    "internal system error",	                                      /* 34 */
    "internal library error",	                                      /* 35 */
    "internal error",		                                      /* 36 */
    "errnum out of range",	                                      /* 37 */
  };

static void
_ipmi_ctx_init(struct ipmi_ctx *ctx)
{
  assert(ctx);

  memset(ctx, '\0', sizeof(struct ipmi_ctx));
  ctx->magic = IPMI_CTX_MAGIC;
  ctx->type = IPMI_DEVICE_UNKNOWN;
}

ipmi_ctx_t
ipmi_ctx_create(void)
{
  struct ipmi_ctx *ctx;

  if (!(ctx = (struct ipmi_ctx *)malloc(sizeof(struct ipmi_ctx))))
    return NULL;

  _ipmi_ctx_init(ctx);
  ctx->errnum = IPMI_ERR_SUCCESS;

  return ctx;
}

int
ipmi_ctx_errnum(ipmi_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_ERR_CTX_NULL);
  else if (ctx->magic != IPMI_CTX_MAGIC)
    return (IPMI_ERR_CTX_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_ctx_strerror(int errnum)
{
  if (errnum >= IPMI_ERR_SUCCESS && errnum <= IPMI_ERR_ERRNUMRANGE)
    return ipmi_errmsg[errnum];
  else
    return ipmi_errmsg[IPMI_ERR_ERRNUMRANGE];
}

char *
ipmi_ctx_errormsg(ipmi_ctx_t ctx)
{
  return ipmi_ctx_strerror(ipmi_ctx_errnum(ctx));
}

static void 
_ipmi_outofband_free (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert(ctx && ctx->magic == IPMI_CTX_MAGIC);

  FIID_OBJ_DESTROY (ctx->io.outofband.rq.obj_rmcp_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rq.obj_lan_session_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rq.obj_rmcpplus_session_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rq.obj_lan_msg_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rq.obj_rmcpplus_session_trlr);

  FIID_OBJ_DESTROY (ctx->io.outofband.rs.obj_rmcp_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rs.obj_lan_session_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rs.obj_rmcpplus_session_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rs.obj_lan_msg_hdr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rs.obj_rmcpplus_payload);
  FIID_OBJ_DESTROY (ctx->io.outofband.rs.obj_lan_msg_trlr);
  FIID_OBJ_DESTROY (ctx->io.outofband.rs.obj_rmcpplus_session_trlr);
}

static void 
_ipmi_inband_free (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert(ctx && ctx->magic == IPMI_CTX_MAGIC);
  
  if (ctx->type == IPMI_DEVICE_KCS && ctx->io.inband.kcs_ctx)
    ipmi_kcs_ctx_destroy(ctx->io.inband.kcs_ctx);
  if (ctx->type == IPMI_DEVICE_SSIF && ctx->io.inband.ssif_ctx)
    ipmi_ssif_ctx_destroy(ctx->io.inband.ssif_ctx);
  if (ctx->type == IPMI_DEVICE_OPENIPMI && ctx->io.inband.openipmi_ctx)
    ipmi_openipmi_ctx_destroy(ctx->io.inband.openipmi_ctx);
  if (ctx->type == IPMI_DEVICE_SUNBMC && ctx->io.inband.sunbmc_ctx)
    ipmi_sunbmc_ctx_destroy(ctx->io.inband.sunbmc_ctx);

  FIID_OBJ_DESTROY (ctx->io.inband.rq.obj_hdr);
  FIID_OBJ_DESTROY (ctx->io.inband.rs.obj_hdr);
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

  assert(ctx && ctx->magic == IPMI_CTX_MAGIC);
  assert(hostname);

  memset(&hent, '\0', sizeof(struct hostent));
#if defined(HAVE_FUNC_GETHOSTBYNAME_R_6)
  if (gethostbyname_r(hostname,
                      &hent,
                      buf,
                      GETHOSTBYNAME_AUX_BUFLEN,
                      &hptr,
                      &h_errnop))
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_HOSTNAME_INVALID);
      return (-1);
    }
  if (!hptr)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_HOSTNAME_INVALID);
      return (-1);
    }
#elif defined(HAVE_FUNC_GETHOSTBYNAME_R_5)
  if (gethostbyname_r(hostname,
                      &hent,
                      buf,
                      GETHOSTBYNAME_AUX_BUFLEN,
                      &h_errnop))
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_HOSTNAME_INVALID);
      return (-1);
    }
#else  /* !HAVE_FUNC_GETHOSTBYNAME_R */
  if (freeipmi_gethostbyname_r(hostname,
                               &hent,
                               buf,
                               GETHOSTBYNAME_AUX_BUFLEN,
                               &hptr,
                               &h_errnop))
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_HOSTNAME_INVALID);
      return (-1);
    }
  if (!hptr)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_HOSTNAME_INVALID);
      return (-1);
    }
#endif /* !HAVE_FUNC_GETHOSTBYNAME_R */

  strncpy(ctx->io.outofband.hostname,
          hostname,
          MAXHOSTNAMELEN);

  ctx->io.outofband.remote_host.sin_family = AF_INET;
  ctx->io.outofband.remote_host.sin_port = htons(RMCP_AUX_BUS_SHUNT);
  ctx->io.outofband.remote_host.sin_addr = *(struct in_addr *) hent.h_addr;

  return (0);
}

static int
_setup_socket (ipmi_ctx_t ctx)
{
  struct sockaddr_in addr;

  assert(ctx && ctx->magic == IPMI_CTX_MAGIC);

  /* Open client (local) UDP socket */
  /* achu: ephemeral ports are > 1023, so no way we will bind to an IPMI port */
  
  if ((ctx->io.outofband.sockfd = socket (AF_INET, 
                                          SOCK_DGRAM, 
                                          0)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }
  
  memset (&addr, 0, sizeof (struct sockaddr_in));
  addr.sin_family = AF_INET;
  addr.sin_port   = htons (0);
  addr.sin_addr.s_addr = htonl (INADDR_ANY);
  
  if (bind(ctx->io.outofband.sockfd, 
           (struct sockaddr *)&addr,
           sizeof(struct sockaddr_in)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }

  return 0;
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
                         uint32_t workaround_flags,
                         uint32_t flags)
{
  uint32_t flags_mask = (IPMI_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO
                         | IPMI_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION
                         | IPMI_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE
                         | IPMI_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER
                         | IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES);
  
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (!hostname
      || strlen(hostname) > MAXHOSTNAMELEN
      || (username && strlen (username) > IPMI_MAX_USER_NAME_LENGTH)
      || (password && strlen (password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
      || !IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type)
      || !IPMI_PRIVILEGE_LEVEL_VALID (privilege_level)
      || (workaround_flags & ~flags_mask))
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_ALREADY_OPEN);
      return (-1);
    }

  ctx->type = IPMI_DEVICE_LAN;
  ctx->workaround_flags = workaround_flags;
  ctx->flags = flags;

  if (_setup_hostname (ctx, hostname) < 0)
    goto cleanup;
 
  memset(ctx->io.outofband.username, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
  if (username)
    memcpy (ctx->io.outofband.username, 
	    username, 
	    strlen (username));
  memset(ctx->io.outofband.password, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH+1);
  if (password)
    memcpy (ctx->io.outofband.password, 
	    password, 
	    strlen (password));
  ctx->io.outofband.privilege_level = privilege_level;
  ctx->io.outofband.session_timeout = (session_timeout ? session_timeout : IPMI_SESSION_TIMEOUT);
  ctx->io.outofband.retransmission_timeout = (retransmission_timeout ? retransmission_timeout : IPMI_RETRANSMISSION_TIMEOUT);

  if (ctx->io.outofband.retransmission_timeout >= ctx->io.outofband.session_timeout)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  memset(&ctx->io.outofband.last_send, '\0', sizeof(struct timeval));
  memset(&ctx->io.outofband.last_received, '\0', sizeof(struct timeval));

  ctx->io.outofband.highest_received_sequence_number = 0;
  ctx->io.outofband.previously_received_list = 0xFF;
 
  ctx->io.outofband.authentication_type = authentication_type;

  if (!(ctx->io.outofband.rq.obj_rmcp_hdr = fiid_obj_create(tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_session_hdr = fiid_obj_create(tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_msg_hdr = fiid_obj_create(tmpl_lan_msg_hdr_rq)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcp_hdr = fiid_obj_create(tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_session_hdr = fiid_obj_create(tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_hdr = fiid_obj_create(tmpl_lan_msg_hdr_rs)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_trlr = fiid_obj_create(tmpl_lan_msg_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
   
  if (_setup_socket (ctx) < 0)
    goto cleanup;

  /* errnum set in ipmi_lan_open_session */
  if (ipmi_lan_open_session (ctx) < 0)
    goto cleanup;

  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);

 cleanup:
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
                             uint32_t workaround_flags,
                             uint32_t flags)
{
  uint32_t flags_mask = (IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES
                         | IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION
                         | IPMI_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION
                         | IPMI_WORKAROUND_FLAGS_SUN_2_0_SESSION);
  
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (!hostname
      || strlen(hostname) > MAXHOSTNAMELEN
      || (username && strlen (username) > IPMI_MAX_USER_NAME_LENGTH)
      || (password && strlen (password) > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || (k_g && k_g_len > IPMI_MAX_K_G_LENGTH)
      || !IPMI_PRIVILEGE_LEVEL_VALID (privilege_level)
      || !IPMI_CIPHER_SUITE_ID_SUPPORTED (cipher_suite_id)
      || (workaround_flags & ~flags_mask))
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_ALREADY_OPEN);
      return (-1);
    }

  if (ipmi_rmcpplus_init() < 0)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }

  ctx->type = IPMI_DEVICE_LAN_2_0;
  ctx->workaround_flags = workaround_flags;
  ctx->flags = flags;

  if (_setup_hostname (ctx, hostname) < 0)
    goto cleanup;
 
  memset(ctx->io.outofband.username, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
  if (username)
    memcpy (ctx->io.outofband.username, 
	    username, 
	    strlen (username));

  memset(ctx->io.outofband.password, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH+1);
  if (password)
    memcpy (ctx->io.outofband.password, 
	    password, 
	    strlen (password));

  ctx->io.outofband.privilege_level = privilege_level;
  ctx->io.outofband.session_timeout = (session_timeout ? session_timeout : IPMI_SESSION_TIMEOUT);
  ctx->io.outofband.retransmission_timeout = (retransmission_timeout ? retransmission_timeout : IPMI_RETRANSMISSION_TIMEOUT);

  if (ctx->io.outofband.retransmission_timeout >= ctx->io.outofband.session_timeout)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  memset(ctx->io.outofband.k_g, '\0', IPMI_MAX_K_G_LENGTH);
  ctx->io.outofband.k_g_configured = 0;
  if (k_g && k_g_len)
    {
      memcpy(ctx->io.outofband.k_g,
             k_g,
             k_g_len);
      ctx->io.outofband.k_g_configured++;
    }

  ctx->io.outofband.cipher_suite_id = cipher_suite_id;

  memset(ctx->io.outofband.sik_key, '\0', IPMI_MAX_SIK_KEY_LENGTH);
  ctx->io.outofband.sik_key_ptr = ctx->io.outofband.sik_key;
  ctx->io.outofband.sik_key_len = IPMI_MAX_SIK_KEY_LENGTH;
  memset(ctx->io.outofband.integrity_key, '\0', IPMI_MAX_INTEGRITY_KEY_LENGTH);
  ctx->io.outofband.integrity_key_ptr = ctx->io.outofband.integrity_key;
  ctx->io.outofband.integrity_key_len = IPMI_MAX_INTEGRITY_KEY_LENGTH;
  memset(ctx->io.outofband.confidentiality_key, '\0', IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH);
  ctx->io.outofband.confidentiality_key_ptr = ctx->io.outofband.confidentiality_key;
  ctx->io.outofband.confidentiality_key_len = IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH;
  memset(&ctx->io.outofband.last_send, '\0', sizeof(struct timeval));
  memset(&ctx->io.outofband.last_received, '\0', sizeof(struct timeval));

  ctx->io.outofband.highest_received_sequence_number = 0;
  ctx->io.outofband.previously_received_list = 0xFF;

  if (!(ctx->io.outofband.rq.obj_rmcp_hdr = fiid_obj_create(tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_session_hdr = fiid_obj_create(tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_rmcpplus_session_hdr = fiid_obj_create(tmpl_rmcpplus_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_lan_msg_hdr = fiid_obj_create(tmpl_lan_msg_hdr_rq)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rq.obj_rmcpplus_session_trlr = fiid_obj_create(tmpl_rmcpplus_session_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
 
  if (!(ctx->io.outofband.rs.obj_rmcp_hdr = fiid_obj_create(tmpl_rmcp_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_session_hdr = fiid_obj_create(tmpl_lan_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcpplus_session_hdr = fiid_obj_create(tmpl_rmcpplus_session_hdr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_hdr = fiid_obj_create(tmpl_lan_msg_hdr_rs)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcpplus_payload = fiid_obj_create(tmpl_rmcpplus_payload)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_lan_msg_trlr = fiid_obj_create(tmpl_lan_msg_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.outofband.rs.obj_rmcpplus_session_trlr = fiid_obj_create(tmpl_rmcpplus_session_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  
  if (_setup_socket (ctx) < 0)
    goto cleanup;

  /* errnum set in ipmi_lan_2_0_open_session */
  if (ipmi_lan_2_0_open_session (ctx) < 0)
    goto cleanup;

  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);

 cleanup:
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
                      char *driver_device, 
                      uint32_t workaround_flags,
                      uint32_t flags)
{
  struct ipmi_locate_info locate_info;
  unsigned int seedp;
  uint32_t temp_flags = 0;
  int errnum;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  /* No workaround flags currently supported */
  if ((driver_type != IPMI_DEVICE_KCS
       && driver_type != IPMI_DEVICE_SMIC
       && driver_type != IPMI_DEVICE_BT
       && driver_type != IPMI_DEVICE_SSIF
       && driver_type != IPMI_DEVICE_OPENIPMI
       && driver_type != IPMI_DEVICE_SUNBMC)
      || workaround_flags)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_ALREADY_OPEN);
      return (-1);
    }

  ctx->workaround_flags = workaround_flags;
  ctx->flags = flags;

  ctx->io.inband.kcs_ctx = NULL;
  ctx->io.inband.ssif_ctx = NULL;
  ctx->io.inband.openipmi_ctx = NULL;
  ctx->io.inband.sunbmc_ctx = NULL;

  /* Random number generation */
  seedp = (unsigned int) clock () + (unsigned int) time (NULL);
  srand (seedp);

  ctx->io.inband.rq_seq = (double)(IPMI_IPMB_REQUESTER_SEQUENCE_NUMBER_MAX) * (rand()/(RAND_MAX + 1.0));

  switch (driver_type)
    {
    case IPMI_DEVICE_KCS:
      if (disable_auto_probe)
	{
	  memset(&locate_info, '\0', sizeof(struct ipmi_locate_info));

	  locate_info.ipmi_version_major = 1;
	  locate_info.ipmi_version_minor = 5;
	  locate_info.locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
	  locate_info.interface_type = IPMI_INTERFACE_KCS;
	  if (driver_device)
	    {
	      strncpy(locate_info.driver_device, driver_device, IPMI_LOCATE_PATH_MAX);
	      locate_info.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
	    }
	  locate_info.address_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
	  locate_info.driver_address = driver_address;
	  locate_info.register_spacing = register_spacing;
	}
      else 
	{
	  if ((errnum = ipmi_locate_get_device_info (IPMI_INTERFACE_KCS, &locate_info)))
            {
              API_LOCATE_ERRNUM_TO_API_ERRNUM(ctx, errnum);
              goto cleanup;
            }
	  if (driver_device)
	    {
	      strncpy(locate_info.driver_device, 
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
      
      /* At this point we only support SYSTEM_IO, i.e. inb/outb style IO. 
	 If we cant find the bass address, we better exit. -- Anand Babu */
      if (locate_info.address_space_id != IPMI_ADDRESS_SPACE_ID_SYSTEM_IO)
        {
          API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_NOT_SUPPORTED);
          return (-1);
        }
      
      if (!(ctx->io.inband.kcs_ctx = ipmi_kcs_ctx_create()))
        {
          API_ERRNO_TO_API_ERRNUM(ctx, errno);
          goto cleanup;
        }
      
      if (ipmi_kcs_ctx_set_driver_address(ctx->io.inband.kcs_ctx, 
                                          locate_info.driver_address) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM(ctx, ipmi_kcs_ctx_errnum(ctx->io.inband.kcs_ctx));
          goto cleanup;
        }

      if (ipmi_kcs_ctx_set_register_spacing(ctx->io.inband.kcs_ctx, 
                                            locate_info.register_spacing) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM(ctx, ipmi_kcs_ctx_errnum(ctx->io.inband.kcs_ctx));
          goto cleanup;
        }
      
      if (ipmi_kcs_ctx_set_poll_interval(ctx->io.inband.kcs_ctx, 
                                         IPMI_POLL_INTERVAL_USECS) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM(ctx, ipmi_kcs_ctx_errnum(ctx->io.inband.kcs_ctx));
          goto cleanup;
        }
      
      if (ctx->flags & IPMI_FLAGS_NONBLOCKING)
        temp_flags |= IPMI_KCS_FLAGS_NONBLOCKING;
      
      if (ipmi_kcs_ctx_set_flags(ctx->io.inband.kcs_ctx, temp_flags) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM(ctx, ipmi_kcs_ctx_errnum(ctx->io.inband.kcs_ctx));
          goto cleanup;
        }
      
      if (ipmi_kcs_ctx_io_init(ctx->io.inband.kcs_ctx) < 0)
        {
          API_KCS_ERRNUM_TO_API_ERRNUM(ctx, ipmi_kcs_ctx_errnum(ctx->io.inband.kcs_ctx));
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
	  memset(&locate_info, '\0', sizeof(struct ipmi_locate_info));

	  locate_info.ipmi_version_major = 1;
	  locate_info.ipmi_version_minor = 5;
	  locate_info.locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
	  locate_info.interface_type = IPMI_INTERFACE_SSIF;
	  if (driver_device)
	    {
	      strncpy(locate_info.driver_device, driver_device, IPMI_LOCATE_PATH_MAX);
	      locate_info.driver_device[IPMI_LOCATE_PATH_MAX - 1] = '\0';
	    }
	  locate_info.address_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
	  locate_info.driver_address = driver_address;
	  locate_info.register_spacing = register_spacing;
	}
      else 
	{
	  if ((errnum = ipmi_locate_get_device_info (IPMI_INTERFACE_SSIF, 
                                                     &locate_info)))
            {
              API_LOCATE_ERRNUM_TO_API_ERRNUM(ctx, errnum);
              goto cleanup;
            }
	  if (driver_device)
	    {
	      strncpy(locate_info.driver_device, 
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

      if (!(ctx->io.inband.ssif_ctx = ipmi_ssif_ctx_create()))
        {
          API_ERRNO_TO_API_ERRNUM(ctx, errno);
          goto cleanup;
        }
      
      if (driver_device)
        {
          if (ipmi_ssif_ctx_set_driver_device(ctx->io.inband.ssif_ctx, 
                                              locate_info.driver_device) < 0)
            {
              API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
              goto cleanup;
            }
        }
 
      if (ipmi_ssif_ctx_set_driver_address(ctx->io.inband.ssif_ctx, 
                                           locate_info.driver_address) < 0)
        {
          API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
          goto cleanup;
        }

      if (ctx->flags & IPMI_FLAGS_NONBLOCKING)
        temp_flags |= IPMI_SSIF_FLAGS_NONBLOCKING;
      
      if (ipmi_ssif_ctx_set_flags(ctx->io.inband.ssif_ctx, temp_flags) < 0)
        {
          API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
          goto cleanup;
        }

      if (ipmi_ssif_ctx_io_init(ctx->io.inband.ssif_ctx) < 0)
        {
          API_SSIF_ERRNUM_TO_API_ERRNUM(ctx, ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx));
          goto cleanup;
        }

      break;

    case IPMI_DEVICE_OPENIPMI:
      ctx->type = driver_type;

      if (!(ctx->io.inband.openipmi_ctx = ipmi_openipmi_ctx_create()))
        {
          API_ERRNO_TO_API_ERRNUM(ctx, errno);
          goto cleanup;
        }
      
      if (driver_device)
        {
          if (ipmi_openipmi_ctx_set_driver_device(ctx->io.inband.openipmi_ctx,
                                                  driver_device) < 0)
            {
              API_OPENIPMI_ERRNUM_TO_API_ERRNUM(ctx, ipmi_openipmi_ctx_errnum(ctx->io.inband.openipmi_ctx));
              goto cleanup;
            }
        }
      
      if (ipmi_openipmi_ctx_io_init(ctx->io.inband.openipmi_ctx) < 0)
        {
          API_OPENIPMI_ERRNUM_TO_API_ERRNUM(ctx, ipmi_openipmi_ctx_errnum(ctx->io.inband.openipmi_ctx));
          goto cleanup;
        }

      break;

    case IPMI_DEVICE_SUNBMC:
      ctx->type = driver_type;

      if (!(ctx->io.inband.sunbmc_ctx = ipmi_sunbmc_ctx_create()))
        {
          API_ERRNO_TO_API_ERRNUM(ctx, errno);
          goto cleanup;
        }
      
      if (driver_device)
        {
          if (ipmi_sunbmc_ctx_set_driver_device(ctx->io.inband.sunbmc_ctx,
                                                driver_device) < 0)
            {
              API_SUNBMC_ERRNUM_TO_API_ERRNUM(ctx, ipmi_sunbmc_ctx_errnum(ctx->io.inband.sunbmc_ctx));
              goto cleanup;
            }
        }
      
      if (ipmi_sunbmc_ctx_io_init(ctx->io.inband.sunbmc_ctx) < 0)
        {
          API_SUNBMC_ERRNUM_TO_API_ERRNUM(ctx, ipmi_sunbmc_ctx_errnum(ctx->io.inband.sunbmc_ctx));
          goto cleanup;
        }

      break;

    default:
      goto cleanup;
    }
  
  /* Prepare in-band headers */
  if (!(ctx->io.inband.rq.obj_hdr = fiid_obj_create(tmpl_hdr_kcs)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  if (!(ctx->io.inband.rs.obj_hdr = fiid_obj_create(tmpl_hdr_kcs)))
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      goto cleanup;
    }
  
  ctx->errnum = IPMI_ERR_SUCCESS;
  return (0);

 cleanup:
  _ipmi_inband_free (ctx);
  ctx->type = IPMI_DEVICE_UNKNOWN;
  return (-1);
}

int 
ipmi_cmd (ipmi_ctx_t ctx, 
	  uint8_t lun, 
	  uint8_t net_fn, 
	  fiid_obj_t obj_cmd_rq, 
	  fiid_obj_t obj_cmd_rs)
{
  int8_t status = 0;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_LAN
      && ctx->type != IPMI_DEVICE_LAN_2_0
      && ctx->type != IPMI_DEVICE_KCS
      && ctx->type != IPMI_DEVICE_SSIF
      && ctx->type != IPMI_DEVICE_OPENIPMI
      && ctx->type != IPMI_DEVICE_SUNBMC)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (api_fiid_obj_packet_valid(ctx, obj_cmd_rq) < 0)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  ctx->lun = lun;
  ctx->net_fn = net_fn;
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* lan packets are dumped in ipmi lan code */
      /* kcs packets are dumped in kcs code */
      if (ctx->type != IPMI_DEVICE_LAN
          && ctx->type != IPMI_DEVICE_LAN_2_0
          && ctx->type != IPMI_DEVICE_KCS)
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
          uint64_t cmd = 0;

          /* ignore error, continue on */
          if (api_fiid_obj_get(ctx,
                               obj_cmd_rq,
                               "cmd",
                               &cmd) < 0)
            ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));

          debug_hdr_cmd(DEBUG_UTIL_TYPE_INBAND,
                        DEBUG_UTIL_DIRECTION_REQUEST,
                        ctx->net_fn,
                        cmd,
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
    status = ipmi_lan_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
  else if (ctx->type == IPMI_DEVICE_LAN_2_0)
    status = ipmi_lan_2_0_cmd (ctx, obj_cmd_rq, obj_cmd_rs);
  else if (ctx->type == IPMI_DEVICE_KCS)
    status = ipmi_kcs_cmd_api (ctx, obj_cmd_rq, obj_cmd_rs);
  else if (ctx->type == IPMI_DEVICE_SSIF)
    status = ipmi_ssif_cmd_api (ctx, obj_cmd_rq, obj_cmd_rs);
  else if (ctx->type == IPMI_DEVICE_OPENIPMI)
    status = ipmi_openipmi_cmd_api (ctx, obj_cmd_rq, obj_cmd_rs);
  else /* ctx->type == IPMI_DEVICE_SUNBMC */
    status = ipmi_sunbmc_cmd_api (ctx, obj_cmd_rq, obj_cmd_rs);
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* lan packets are dumped in ipmi lan code */
      /* kcs packets are dumped in kcs code */
      if (ctx->type != IPMI_DEVICE_LAN
          && ctx->type != IPMI_DEVICE_LAN_2_0
          && ctx->type != IPMI_DEVICE_KCS)
        {
          char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
          uint64_t cmd = 0;
          
          /* ignore error, continue on */
          if (api_fiid_obj_get(ctx,
                               obj_cmd_rq,
                               "cmd",
                               &cmd) < 0)
            ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));

	  /* its ok to use the "request" net_fn */
          debug_hdr_cmd(DEBUG_UTIL_TYPE_INBAND,
                        DEBUG_UTIL_DIRECTION_RESPONSE,
                        ctx->net_fn,
                        cmd,
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
  return (status);
}

int
ipmi_cmd_ipmb (ipmi_ctx_t ctx,
               uint8_t rs_addr,
               uint8_t lun,
               uint8_t net_fn,
               fiid_obj_t obj_cmd_rq, 
               fiid_obj_t obj_cmd_rs)
{
  int8_t status = 0;

  /* achu:
   *
   * Thanks to the OpenIPMI folks and tcpdumps from their project. I
   * had trouble figuring out a few chunks of the bridging code.
   */

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_LAN
      && ctx->type != IPMI_DEVICE_LAN_2_0
      && ctx->type != IPMI_DEVICE_KCS
      && ctx->type != IPMI_DEVICE_SSIF
      && ctx->type != IPMI_DEVICE_OPENIPMI
      && ctx->type != IPMI_DEVICE_SUNBMC)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (api_fiid_obj_packet_valid(ctx, obj_cmd_rq) < 0)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  ctx->rs_addr = rs_addr;
  ctx->lun = lun;
  ctx->net_fn = net_fn;

  if (ctx->type == IPMI_DEVICE_LAN)
    status = ipmi_lan_cmd_wrapper_ipmb (ctx,
					obj_cmd_rq,
					obj_cmd_rs);
  else if (ctx->type == IPMI_DEVICE_LAN_2_0)
    status = ipmi_lan_2_0_cmd_wrapper_ipmb (ctx,
					    obj_cmd_rq,
					    obj_cmd_rs);
  else if (ctx->type == IPMI_DEVICE_KCS)
    status = ipmi_kcs_cmd_api_ipmb (ctx,
				    obj_cmd_rq,
				    obj_cmd_rs);
  else if (ctx->type == IPMI_DEVICE_OPENIPMI)
    status = ipmi_openipmi_cmd_api_ipmb (ctx,
					 obj_cmd_rq,
					 obj_cmd_rs);
  else
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE);
      return (-1);
    }
                         
  /* errnum set in ipmi_*_cmd functions */
  return (status);
}              

int 
ipmi_cmd_raw (ipmi_ctx_t ctx, 
              uint8_t lun,
              uint8_t net_fn,
	      uint8_t *in, 
	      size_t in_len, 
	      uint8_t *out, 
	      size_t out_len)
{
  int8_t status = 0;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (!in
      || !in_len
      || !out
      || !out_len)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_LAN
      && ctx->type != IPMI_DEVICE_LAN_2_0
      && ctx->type != IPMI_DEVICE_KCS
      && ctx->type != IPMI_DEVICE_SSIF
      && ctx->type != IPMI_DEVICE_OPENIPMI
      && ctx->type != IPMI_DEVICE_SUNBMC)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }
  
  ctx->lun = lun;
  ctx->net_fn = net_fn;

 if (ctx->type == IPMI_DEVICE_LAN)
   status = ipmi_lan_cmd_raw (ctx, in, in_len, out, out_len);
 else if (ctx->type == IPMI_DEVICE_LAN_2_0)
   status = ipmi_lan_2_0_cmd_raw (ctx, in, in_len, out, out_len);
 else if (ctx->type == IPMI_DEVICE_KCS)
   status = ipmi_kcs_cmd_raw_api (ctx, in, in_len, out, out_len);
 else if (ctx->type == IPMI_DEVICE_SSIF)
   status = ipmi_ssif_cmd_raw_api (ctx, in, in_len, out, out_len);
 else if (ctx->type == IPMI_DEVICE_OPENIPMI)
   status = ipmi_openipmi_cmd_raw_api (ctx, in, in_len, out, out_len);
 else /* ctx->type == IPMI_DEVICE_SUNBMC */
   status = ipmi_sunbmc_cmd_raw_api (ctx, in, in_len, out, out_len);

  /* errnum set in ipmi_*_cmd_raw functions */
  return (status);
}

static void
_ipmi_outofband_close (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert(ctx
	 && ctx->magic == IPMI_CTX_MAGIC
	 && (ctx->type == IPMI_DEVICE_LAN
             || ctx->type == IPMI_DEVICE_LAN_2_0));
 
  /* No need to set errnum - if the anything in close session
   * fails, session will eventually timeout anyways
   */

  if (ipmi_lan_close_session (ctx) < 0)
    goto cleanup;

 cleanup:
  if (ctx->io.outofband.sockfd)
    close (ctx->io.outofband.sockfd);
  _ipmi_outofband_free (ctx);
}

static void
_ipmi_inband_close (ipmi_ctx_t ctx)
{
  /* Function Note: No need to set errnum - just return */
  assert(ctx 
	 && ctx->magic == IPMI_CTX_MAGIC
	 && (ctx->type == IPMI_DEVICE_KCS
	     || ctx->type == IPMI_DEVICE_SMIC
	     || ctx->type == IPMI_DEVICE_BT
	     || ctx->type == IPMI_DEVICE_SSIF
	     || ctx->type == IPMI_DEVICE_OPENIPMI
	     || ctx->type == IPMI_DEVICE_SUNBMC));
  
  _ipmi_inband_free (ctx);
}

int 
ipmi_ctx_close (ipmi_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_UNKNOWN)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_DEVICE_NOT_OPEN);
      return (-1);
    }

  if (ctx->type != IPMI_DEVICE_LAN
      && ctx->type != IPMI_DEVICE_LAN_2_0
      && ctx->type != IPMI_DEVICE_KCS
      && ctx->type != IPMI_DEVICE_SMIC
      && ctx->type != IPMI_DEVICE_BT
      && ctx->type != IPMI_DEVICE_SSIF
      && ctx->type != IPMI_DEVICE_OPENIPMI
      && ctx->type != IPMI_DEVICE_SUNBMC)
    {
      API_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (ctx->type == IPMI_DEVICE_LAN
      || ctx->type == IPMI_DEVICE_LAN_2_0)
    _ipmi_outofband_close (ctx);
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
      ERR_TRACE(ipmi_ctx_errormsg(ctx), ipmi_ctx_errnum(ctx));
      return;
    }

  if (ctx->type != IPMI_DEVICE_UNKNOWN)
    ipmi_ctx_close(ctx);

  secure_memset(ctx, '\0', sizeof(ipmi_ctx_t));
  free(ctx);
}
