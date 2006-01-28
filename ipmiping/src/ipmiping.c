/*****************************************************************************\
 *  $Id: ipmiping.c,v 1.7.2.4 2006-01-28 15:31:56 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
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
 *  with Ipmiping; if not, write to the Free Software Foundation, Inc.,
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
#include <errno.h>
#include <assert.h>
#include "freeipmi.h"

#define _setstr(x)   (x) ? "set" : "clear"

/* IPMI has a 6 bit sequence number */
#define IPMI_RQ_SEQ_MAX  0x3F

fiid_obj_t
Fiid_obj_create(fiid_template_t tmpl)
{
  fiid_obj_t obj;

  assert(tmpl != NULL);

  if ((obj = fiid_obj_create(tmpl)) == NULL)
    ipmi_ping_err_exit("fiid_obj_create: %s", strerror(errno));

  return obj;
}

void 
Fiid_obj_destroy(fiid_obj_t obj)
{
  fiid_obj_destroy(obj);
}

void 
Fiid_obj_get(fiid_obj_t obj, uint8_t *field, uint64_t *val)
{
  assert(obj != NULL && field != NULL && val != NULL);

  if (fiid_obj_get(obj, field, val) < 0)
    ipmi_ping_err_exit("fiid_obj_get: %s", strerror(errno));
}

int 
createpacket(char *buffer, 
             int buflen, 
             unsigned int seq_num,
             int debug) 
{
  fiid_obj_t obj_hdr_rmcp = NULL;
  fiid_obj_t obj_hdr_session = NULL;
  fiid_obj_t obj_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  int len;

  assert(buffer != NULL);

  if (buflen < 0)
    return -1;

  if (buflen == 0)
    return 0;

  obj_hdr_rmcp = Fiid_obj_create(tmpl_hdr_rmcp);
  obj_hdr_session = Fiid_obj_create(tmpl_hdr_session_auth_calc);
  obj_msg_hdr = Fiid_obj_create(tmpl_lan_msg_hdr_rq);
  obj_cmd = Fiid_obj_create(tmpl_cmd_get_channel_auth_caps_rq);
    
  if (fill_hdr_rmcp_ipmi(obj_hdr_rmcp) < 0)
    ipmi_ping_err_exit("fill_hdr_rmcp_ipmi: %s", strerror(errno));

  if (fill_hdr_session(IPMI_SESSION_AUTH_TYPE_NONE,
                       0, 0, NULL, 0, tmpl_cmd_get_channel_auth_caps_rq, 
                       obj_hdr_session) < 0)
    ipmi_ping_err_exit("fill_hdr_session: %s", strerror(errno));

  if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, IPMI_BMC_IPMB_LUN_BMC, 
                       seq_num % (IPMI_RQ_SEQ_MAX+1), obj_msg_hdr) < 0)
    ipmi_ping_err_exit("fill_lan_msg_hdr: %s", strerror(errno));

  if (fill_cmd_get_channel_auth_caps(IPMI_CHANNEL_CURRENT_CHANNEL,
                                     IPMI_PRIV_LEVEL_USER, 
                                     obj_cmd) < 0)
    ipmi_ping_err_exit("fill_cmd_get_channel_auth_caps: %s", strerror(errno));

  if ((len = assemble_ipmi_lan_pkt(obj_hdr_rmcp, 
				   obj_hdr_session, 
                                   obj_msg_hdr, 
                                   obj_cmd, 
                                   (uint8_t *)buffer, 
				   buflen)) < 0)
    ipmi_ping_err_exit("assemble_ipmi_lan_pkt: %s", strerror(errno));

#ifndef NDEBUG
  if (debug)
    {
      if (fiid_obj_dump_lan(STDERR_FILENO, 
			    "Request", 
			    NULL, 
                            (uint8_t *)buffer, 
			    len, 
			    tmpl_lan_msg_hdr_rq, 
                            tmpl_cmd_get_channel_auth_caps_rq) < 0)
        ipmi_ping_err_exit("fiid_obj_dump_lan: %s", strerror(errno));
    }
#endif

  Fiid_obj_destroy(obj_hdr_rmcp);
  Fiid_obj_destroy(obj_hdr_session);
  Fiid_obj_destroy(obj_msg_hdr);
  Fiid_obj_destroy(obj_cmd);

  return len;
}

int 
parsepacket(char *buffer, 
            int buflen, 
            const char *from,
            unsigned int seq_num, 
            int verbose, 
            int debug)
{
  fiid_obj_t obj_hdr_rmcp = NULL;
  fiid_obj_t obj_hdr_session = NULL;
  fiid_obj_t obj_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_msg_trlr = NULL;
  uint64_t req_seq, none, md2, md5, straight_passwd_key, oem, 
    anonymous_login, null_username, non_null_username,
    user_level_auth, per_message_auth;
  int ret, retval = -1;

  assert(buffer != NULL && from != NULL);

  if (buflen < 0)
    return -1;

  if (buflen == 0)
    return 0;

  obj_hdr_rmcp = Fiid_obj_create(tmpl_hdr_rmcp);
  obj_hdr_session = Fiid_obj_create(tmpl_hdr_session_auth_calc);
  obj_msg_hdr = Fiid_obj_create(tmpl_lan_msg_hdr_rs);
  obj_cmd = Fiid_obj_create(tmpl_cmd_get_channel_auth_caps_rs);
  obj_msg_trlr = Fiid_obj_create(tmpl_lan_msg_trlr);

#ifndef NDEBUG
  if (debug)
    {
      if (fiid_obj_dump_lan(STDERR_FILENO, 
			    "Response", 
			    NULL, 
                            (uint8_t *)buffer, 
			    buflen, 
                            tmpl_lan_msg_hdr_rs, 
                            tmpl_cmd_get_channel_auth_caps_rs) < 0)
        ipmi_ping_err_exit("fiid_obj_dump_lan: %s", strerror(errno));
    }
#endif

  if ((ret = ipmi_lan_check_chksum((uint8_t *)buffer, buflen)) < 0)
    ipmi_ping_err_exit("ipmi_lan_check_chksum: %s", strerror(errno));

  if (!ret)
    {
      retval = 0;
      goto cleanup;
    }

  if (unassemble_ipmi_lan_pkt((uint8_t *)buffer, 
			      buflen, 
                              obj_hdr_rmcp, 
			      obj_hdr_session, 
                              obj_msg_hdr,
			      obj_cmd, 
			      obj_msg_trlr) < 0)
    ipmi_ping_err_exit("unassemble_ipmi_lan_pkt: %s", strerror(errno));

  if ((ret = ipmi_lan_check_net_fn(obj_msg_hdr, IPMI_NET_FN_APP_RS)) < 0)
    ipmi_ping_err_exit("ipmi_lan_check_net_fn: %s", strerror(errno));

  if (!ret)
    {
      retval = 0;
      goto cleanup;
    }

  if ((ret = ipmi_lan_check_net_fn(obj_msg_hdr, IPMI_NET_FN_APP_RS)) < 0)
    ipmi_ping_err_exit("ipmi_lan_check_net_fn: %s", strerror(errno));

  if (!ret)
    {
      retval = 0;
      goto cleanup;
    }

  if ((ret = ipmi_check_cmd(obj_cmd, IPMI_CMD_GET_CHANNEL_AUTH_CAPS)) < 0)
    ipmi_ping_err_exit("ipmi_lan_check_net_fn: %s", strerror(errno));

  if (!ret)
    {
      retval = 0;
      goto cleanup;
    }

  if ((ret = ipmi_check_comp_code(obj_cmd, IPMI_COMMAND_SUCCESS)) < 0)
    ipmi_ping_err_exit("ipmi_lan_check_net_fn: %s", strerror(errno));

  if (!ret)
    {
      retval = 0;
      goto cleanup;
    }

  Fiid_obj_get(obj_msg_hdr, (uint8_t *)"rq_seq", (uint64_t *)&req_seq);

  if (req_seq != seq_num % (IPMI_RQ_SEQ_MAX + 1)) 
    {
      retval = 0;
      goto cleanup;
    }
  
  printf("response received from %s: rq_seq=%u", from, (uint32_t)req_seq);
  if (verbose)
    {
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_type.none", 
		   (uint64_t *)&none);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_type.md2", 
		   (uint64_t *)&md2);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_type.md5", 
		   (uint64_t *)&md5);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_type.straight_passwd_key", 
                   (uint64_t *)&straight_passwd_key);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_type.oem_prop", 
		   (uint64_t *)&oem);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_status.anonymous_login", 
                   (uint64_t *)&anonymous_login);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_status.null_username", 
                   (uint64_t *)&null_username);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_status.non_null_username", 
                   (uint64_t *)&non_null_username);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_status.user_level_auth", 
                   (uint64_t *)&user_level_auth);
      Fiid_obj_get(obj_cmd, 
                   (uint8_t *)"auth_status.per_message_auth", 
                   (uint64_t *)&per_message_auth);
      printf(", auth: none=%s md2=%s md5=%s passwd=%s oem=%s anon=%s null=%s non-null=%s user=%s permsg=%s ",
             _setstr(none), _setstr(md2), _setstr(md5), 
             _setstr(straight_passwd_key),_setstr(oem), 
             _setstr(anonymous_login), _setstr(null_username), 
             _setstr(non_null_username), _setstr(user_level_auth), 
             _setstr(per_message_auth));
    }
  printf("\n");
    
  retval = 1;
 cleanup:
  Fiid_obj_destroy(obj_hdr_rmcp);
  Fiid_obj_destroy(obj_hdr_session);
  Fiid_obj_destroy(obj_msg_hdr);
  Fiid_obj_destroy(obj_cmd);
  Fiid_obj_destroy(obj_msg_trlr);
  return retval;
}

void 
latepacket(unsigned int seq_num) 
{
  printf("response timed out: rq_seq=%u\n", seq_num % (IPMI_RQ_SEQ_MAX + 1));
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
#ifndef NDEBUG
  ipmi_ping_setup(argc, argv, 0, IPMI_RQ_SEQ_MAX, "hVc:i:I:t:vs:d");
#else
  ipmi_ping_setup(argc, argv, 0, IPMI_RQ_SEQ_MAX, "hVc:i:I:t:vs:");
#endif
  ipmi_ping_loop(createpacket, parsepacket, latepacket, endresult);

  exit(1);                    /* NOT REACHED */
}
