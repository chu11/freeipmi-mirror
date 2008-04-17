/*****************************************************************************\
 *  $Id: ipmiping.c,v 1.45 2008-04-17 23:10:15 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155448
 *
 *  This file is part of Ipmiping, tools for pinging IPMI and RMCP compliant
 *  remote systems. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiping is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiping is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiping.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "debug-common.h"
#include "ipmi-ping.h"

#define _setstr(x)   (x) ? "set" : "clear"

/* IPMI has a 6 bit sequence number */
#define IPMI_RQ_SEQ_MAX  0x3F

static fiid_obj_t
_fiid_obj_create(fiid_template_t tmpl)
{
  fiid_obj_t obj;

  assert(tmpl != NULL);

  if ((obj = fiid_obj_create(tmpl)) == NULL)
    ipmi_ping_err_exit("fiid_obj_create: %s", strerror(errno));

  return obj;
}

static void 
_fiid_obj_get(fiid_obj_t obj, char *field, uint64_t *val)
{
  assert(obj != NULL && field != NULL && val != NULL);

  if (fiid_obj_get(obj, field, val) < 0)
    ipmi_ping_err_exit("fiid_obj_get: %s", 
                       fiid_strerror(fiid_obj_errnum(obj)));
}

int 
createpacket(char *destination,
             char *buffer, 
             int buflen, 
             unsigned int sequence_number,
             int version,
             int debug) 
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_lan_session_hdr = NULL;
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_field_t *tmpl_cmd_get_channel_authentication_capabilities_ptr = NULL;
  int len;

  assert(destination != NULL);
  assert(buffer != NULL);
  assert(version == IPMI_PING_VERSION_1_5 || version == IPMI_PING_VERSION_2_0);

  if (buflen < 0)
    return -1;

  if (buflen == 0)
    return 0;

  if (version == IPMI_PING_VERSION_1_5)
    tmpl_cmd_get_channel_authentication_capabilities_ptr = (fiid_field_t *)&tmpl_cmd_get_channel_authentication_capabilities_rq[0];
  else
    tmpl_cmd_get_channel_authentication_capabilities_ptr = (fiid_field_t *)&tmpl_cmd_get_channel_authentication_capabilities_v20_rq[0];

  obj_rmcp_hdr = _fiid_obj_create(tmpl_rmcp_hdr);
  obj_lan_session_hdr = _fiid_obj_create(tmpl_lan_session_hdr);
  obj_lan_msg_hdr = _fiid_obj_create(tmpl_lan_msg_hdr_rq);
  obj_cmd = _fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_ptr);
   
  if (fill_rmcp_hdr_ipmi(obj_rmcp_hdr) < 0)
    ipmi_ping_err_exit("fill_rmcp_hdr_ipmi: %s", strerror(errno));

  if (fill_lan_session_hdr(IPMI_AUTHENTICATION_TYPE_NONE,
                           0, 
                           0,
                           obj_lan_session_hdr) < 0)
    ipmi_ping_err_exit("fill_lan_session_hdr: %s", strerror(errno));

  if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, 
                       sequence_number % (IPMI_RQ_SEQ_MAX+1), obj_lan_msg_hdr) < 0)
    ipmi_ping_err_exit("fill_lan_msg_hdr: %s", strerror(errno));

  if (version == IPMI_PING_VERSION_1_5)
    {
      if (fill_cmd_get_channel_authentication_capabilities(IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                           IPMI_PRIVILEGE_LEVEL_USER, 
                                                           obj_cmd) < 0)
        ipmi_ping_err_exit("fill_cmd_get_channel_authentication_capabilities: %s", strerror(errno));
    }
  else
    {
      if (fill_cmd_get_channel_authentication_capabilities_v20(IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                               IPMI_PRIVILEGE_LEVEL_USER, 
                                                               IPMI_GET_IPMI_V20_EXTENDED_DATA, 
                                                               obj_cmd) < 0)
        ipmi_ping_err_exit("fill_cmd_get_channel_authentication_capabilities_v20: %s", strerror(errno));
    }

  if ((len = assemble_ipmi_lan_pkt(obj_rmcp_hdr, 
				   obj_lan_session_hdr, 
                                   obj_lan_msg_hdr, 
                                   obj_cmd, 
				   NULL, 
				   0,
                                   (uint8_t *)buffer, 
				   buflen)) < 0)
    ipmi_ping_err_exit("assemble_ipmi_lan_pkt: %s", strerror(errno));

  if (debug)
    {
      char hdrbuf[DEBUG_COMMON_HDR_BUFLEN];

      debug_hdr_cmd((version == IPMI_PING_VERSION_1_5) ? DEBUG_COMMON_TYPE_IPMI_1_5 : DEBUG_COMMON_TYPE_IPMI_2_0,
                    DEBUG_COMMON_DIRECTION_REQUEST,
                    IPMI_NET_FN_APP_RQ,
                    IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
                    hdrbuf,
                    DEBUG_COMMON_HDR_BUFLEN);

      if (ipmi_dump_lan_packet(STDERR_FILENO, 
                               destination,
                               hdrbuf, 
                               NULL,
                               (uint8_t *)buffer, 
                               len, 
                               tmpl_lan_msg_hdr_rq, 
                               tmpl_cmd_get_channel_authentication_capabilities_ptr) < 0)
        ipmi_ping_err_exit("ipmi_dump_lan_packet: %s", strerror(errno));
    }

  fiid_obj_destroy(obj_rmcp_hdr);
  fiid_obj_destroy(obj_lan_session_hdr);
  fiid_obj_destroy(obj_lan_msg_hdr);
  fiid_obj_destroy(obj_cmd);

  return len;
}

int 
parsepacket(char *destination,
            char *buffer, 
            int buflen, 
            const char *from,
            unsigned int sequence_number, 
            int verbose, 
            int version,
            int debug)
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_lan_session_hdr = NULL;
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  uint64_t req_seq, none, md2, md5, straight_password_key, oem, 
    anonymous_login, null_username, non_null_username,
    user_level_authentication, per_message_authentication,
    k_g, ipmi_v20_extended_capabilities_available, ipmi_v15, ipmi_v20;
  fiid_field_t *tmpl_cmd_get_channel_authentication_capabilities_ptr = NULL;
  int ret, retval = -1;

  assert(destination != NULL);
  assert(buffer != NULL && from != NULL);
  assert(version == IPMI_PING_VERSION_1_5 || version == IPMI_PING_VERSION_2_0);

  if (buflen < 0)
    return -1;

  if (buflen == 0)
    return 0;

  if (version == IPMI_PING_VERSION_1_5)
    tmpl_cmd_get_channel_authentication_capabilities_ptr = (fiid_field_t *)&tmpl_cmd_get_channel_authentication_capabilities_rs[0];
  else
    tmpl_cmd_get_channel_authentication_capabilities_ptr = (fiid_field_t *)&tmpl_cmd_get_channel_authentication_capabilities_v20_rs[0];

  obj_rmcp_hdr = _fiid_obj_create(tmpl_rmcp_hdr);
  obj_lan_session_hdr = _fiid_obj_create(tmpl_lan_session_hdr);
  obj_lan_msg_hdr = _fiid_obj_create(tmpl_lan_msg_hdr_rs);
  obj_cmd = _fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_ptr);
  obj_lan_msg_trlr = _fiid_obj_create(tmpl_lan_msg_trlr);

  if (debug)
    {
      char hdrbuf[DEBUG_COMMON_HDR_BUFLEN];
      
      debug_hdr_cmd((version == IPMI_PING_VERSION_1_5) ? DEBUG_COMMON_TYPE_IPMI_1_5 : DEBUG_COMMON_TYPE_IPMI_2_0,
                    DEBUG_COMMON_DIRECTION_RESPONSE,
                    IPMI_NET_FN_APP_RQ,
                    IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
                    hdrbuf,
                    DEBUG_COMMON_HDR_BUFLEN);
      
      if (ipmi_dump_lan_packet(STDERR_FILENO, 
                               destination,
                               hdrbuf,
                               NULL,
                               (uint8_t *)buffer, 
                               buflen, 
                               tmpl_lan_msg_hdr_rs, 
                               tmpl_cmd_get_channel_authentication_capabilities_ptr) < 0)
        ipmi_ping_err_exit("ipmi_dump_lan_packet: %s", strerror(errno));
    }

  if ((ret = ipmi_lan_check_packet_checksum((uint8_t *)buffer, buflen)) < 0)
    ipmi_ping_err_exit("ipmi_lan_check_checksum: %s", strerror(errno));

  if (!ret)
    {
      if (debug)
        fprintf(stderr, "%s(%d): checksum failed\n", __FUNCTION__, __LINE__);
      retval = 0;
      goto cleanup;
    }

  if (unassemble_ipmi_lan_pkt((uint8_t *)buffer, 
			      buflen, 
                              obj_rmcp_hdr, 
			      obj_lan_session_hdr, 
                              obj_lan_msg_hdr,
			      obj_cmd, 
			      obj_lan_msg_trlr) < 0)
    ipmi_ping_err_exit("unassemble_ipmi_lan_pkt: %s", strerror(errno));

  if ((ret = ipmi_lan_check_net_fn(obj_lan_msg_hdr, IPMI_NET_FN_APP_RS)) < 0)
    ipmi_ping_err_exit("ipmi_lan_check_net_fn: %s", strerror(errno));

  if (!ret)
    {
      if (debug)
        fprintf(stderr, "%s(%d): net_fn failed\n", __FUNCTION__, __LINE__);
      retval = 0;
      goto cleanup;
    }

  if ((ret = ipmi_check_cmd(obj_cmd, IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES)) < 0)
    ipmi_ping_err_exit("ipmi_check_cmd: %s", strerror(errno));

  if (!ret)
    {
      if (debug)
        fprintf(stderr, "%s(%d): cmd failed\n", __FUNCTION__, __LINE__);
      retval = 0;
      goto cleanup;
    }

  if ((ret = ipmi_check_completion_code_success(obj_cmd)) < 0)
    ipmi_ping_err_exit("ipmi_check_comp_code: %s", strerror(errno));

  if (!ret)
    {
      if (debug)
        fprintf(stderr, "%s(%d): comp_code failed\n", __FUNCTION__, __LINE__);
      retval = 0;
      goto cleanup;
    }

  _fiid_obj_get(obj_lan_msg_hdr, "rq_seq", (uint64_t *)&req_seq);

  if (req_seq != sequence_number % (IPMI_RQ_SEQ_MAX + 1)) 
    {
      if (debug)
        fprintf(stderr, "%s(%d): req_seq failed\n", __FUNCTION__, __LINE__);
      retval = 0;
      goto cleanup;
    }
  
  printf("response received from %s: rq_seq=%u", from, (uint32_t)req_seq);
  if (verbose)
    {
      _fiid_obj_get(obj_cmd, 
		    "authentication_type.none", 
		    &none);
      _fiid_obj_get(obj_cmd, 
		    "authentication_type.md2", 
		    &md2);
      _fiid_obj_get(obj_cmd, 
		    "authentication_type.md5", 
		    &md5);
      _fiid_obj_get(obj_cmd, 
		    "authentication_type.straight_password_key", 
		    &straight_password_key);
      _fiid_obj_get(obj_cmd, 
		    "authentication_type.oem_prop", 
		    &oem);
      _fiid_obj_get(obj_cmd, 
		    "authentication_status.anonymous_login", 
		    &anonymous_login);
      _fiid_obj_get(obj_cmd, 
		    "authentication_status.null_username", 
		    &null_username);
      _fiid_obj_get(obj_cmd, 
		    "authentication_status.non_null_username", 
		    &non_null_username);
      _fiid_obj_get(obj_cmd, 
		    "authentication_status.user_level_authentication", 
		    &user_level_authentication);
      _fiid_obj_get(obj_cmd, 
		    "authentication_status.per_message_authentication", 
		    &per_message_authentication);
      _fiid_obj_get(obj_cmd, 
		    "authentication_status.per_message_authentication", 
		    &per_message_authentication);
      
      printf(", auth: none=%s md2=%s md5=%s password=%s oem=%s anon=%s null=%s non-null=%s user=%s permsg=%s ",
             _setstr(none), _setstr(md2), _setstr(md5), 
             _setstr(straight_password_key),_setstr(oem), 
             _setstr(anonymous_login), _setstr(null_username), 
             _setstr(non_null_username), _setstr(user_level_authentication), 
             _setstr(per_message_authentication));
      
      if (version == IPMI_PING_VERSION_2_0)
        {
          _fiid_obj_get(obj_cmd, 
			"authentication_type.ipmi_v2.0_extended_capabilities_available", 
			&ipmi_v20_extended_capabilities_available);
	  
	  _fiid_obj_get(obj_cmd, 
			"authentication_status.k_g", 
			&k_g);

          printf("k_g=%s ipmi_v2.0_extended_capabilities_available=%s ",
		 _setstr(k_g),
                 _setstr(ipmi_v20_extended_capabilities_available));

          if (ipmi_v20_extended_capabilities_available)
            {
              _fiid_obj_get(obj_cmd, 
			    "channel_supports_ipmi_v1.5_connections", 
			    (u_int64_t *)&ipmi_v15);
              _fiid_obj_get(obj_cmd, 
			    "channel_supports_ipmi_v2.0_connections", 
			    (u_int64_t *)&ipmi_v20);
	      
              printf("ipmi_v1.5=%s ipmi_v2.0=%s ", _setstr(ipmi_v15), _setstr(ipmi_v20));
            }
        }
    }
  printf("\n");
  
  retval = 1;
 cleanup:
  fiid_obj_destroy(obj_rmcp_hdr);
  fiid_obj_destroy(obj_lan_session_hdr);
  fiid_obj_destroy(obj_lan_msg_hdr);
  fiid_obj_destroy(obj_cmd);
  fiid_obj_destroy(obj_lan_msg_trlr);
  return retval;
}

void 
latepacket(unsigned int sequence_number) 
{
  printf("response timed out: rq_seq=%u\n", sequence_number % (IPMI_RQ_SEQ_MAX + 1));
}

int
endresult(const char *progname, 
          const char *dest, 
          unsigned int sent_count, 
          unsigned int recv_count) 
{
  double percent = 0;

  assert(progname != NULL && dest != NULL);

  if (sent_count > 0)
    percent = ((double)(sent_count - recv_count)/sent_count)*100;

  printf("--- %s %s statistics ---\n", progname, dest);
  printf("%d requests transmitted, %d responses received in time, "
         "%2.1f%% packet loss\n",
         sent_count, recv_count, percent);

  return ((recv_count > 0) ? 0 : 1);
}

int 
main(int argc, char **argv) 
{
  ipmi_ping_setup(argc, argv, 0, IPMI_RQ_SEQ_MAX, "hVc:i:I:t:vr:s:d");
  ipmi_ping_loop(createpacket, parsepacket, latepacket, endresult);
  exit(1);                    /* NOT REACHED */
}
