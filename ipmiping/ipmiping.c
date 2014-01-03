/*****************************************************************************\
 *  $Id: ipmiping.c,v 1.75 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#endif /* HAVE_CONFIG_H */

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

#include "freeipmi-portability.h"
#include "debug-util.h"

#include "ping-tool-common.h"

#define _setstr(x)   (x) ? "set" : "clear"

/* IPMI has a 6 bit sequence number */
#define IPMI_RQ_SEQ_MAX  0x3F

int
createpacket (const char *destination,
              void *buf,
              unsigned int buflen,
              unsigned int sequence_number,
              int version,
              int debug)
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_lan_session_hdr = NULL;
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  int len;

  assert (destination);
  assert (buf);
  assert (version == IPMI_PING_VERSION_1_5 || version == IPMI_PING_VERSION_2_0);

  if (!buflen)
    return (0);

  if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rq)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_cmd = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rq)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));

  if (fill_rmcp_hdr_ipmi (obj_rmcp_hdr) < 0)
    ipmi_ping_err_exit ("fill_rmcp_hdr_ipmi: %s", strerror (errno));

  if (fill_lan_session_hdr (IPMI_AUTHENTICATION_TYPE_NONE,
                            0,
                            0,
                            obj_lan_session_hdr) < 0)
    ipmi_ping_err_exit ("fill_lan_session_hdr: %s", strerror (errno));

  if (fill_lan_msg_hdr (IPMI_SLAVE_ADDRESS_BMC,
                        IPMI_NET_FN_APP_RQ,
                        IPMI_BMC_IPMB_LUN_BMC,
                        sequence_number % (IPMI_RQ_SEQ_MAX+1),
                        obj_lan_msg_hdr) < 0)
    ipmi_ping_err_exit ("fill_lan_msg_hdr: %s", strerror (errno));

  if (version == IPMI_PING_VERSION_1_5)
    {
      if (fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                            IPMI_PRIVILEGE_LEVEL_USER,
                                                            IPMI_GET_IPMI_V15_DATA,
                                                            obj_cmd) < 0)
        ipmi_ping_err_exit ("fill_cmd_get_channel_authentication_capabilities: %s", strerror (errno));
    }
  else
    {
      if (fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                            IPMI_PRIVILEGE_LEVEL_USER,
                                                            IPMI_GET_IPMI_V20_EXTENDED_DATA,
                                                            obj_cmd) < 0)
        ipmi_ping_err_exit ("fill_cmd_get_channel_authentication_capabilities: %s", strerror (errno));
    }

  if ((len = assemble_ipmi_lan_pkt (obj_rmcp_hdr,
                                    obj_lan_session_hdr,
                                    obj_lan_msg_hdr,
                                    obj_cmd,
                                    NULL,
                                    0,
                                    buf,
                                    buflen,
				    IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    ipmi_ping_err_exit ("assemble_ipmi_lan_pkt: %s", strerror (errno));

  if (debug)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd ((version == IPMI_PING_VERSION_1_5) ? DEBUG_UTIL_TYPE_IPMI_1_5 : DEBUG_UTIL_TYPE_IPMI_2_0,
                     DEBUG_UTIL_DIRECTION_REQUEST,
                     IPMI_NET_FN_APP_RQ,
                     IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
		     0,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_dump_lan_packet (STDERR_FILENO,
                                destination,
                                hdrbuf,
                                NULL,
                                buf,
                                len,
                                tmpl_lan_msg_hdr_rq,
                                tmpl_cmd_get_channel_authentication_capabilities_rq) < 0)
        ipmi_ping_err_exit ("ipmi_dump_lan_packet: %s", strerror (errno));
    }

  fiid_obj_destroy (obj_rmcp_hdr);
  fiid_obj_destroy (obj_lan_session_hdr);
  fiid_obj_destroy (obj_lan_msg_hdr);
  fiid_obj_destroy (obj_cmd);

  return (len);
}

int
parsepacket (const char *destination,
             const void *buf,
             unsigned int buflen,
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
  uint8_t req_seq, none, md2, md5, straight_password_key, oem,
    anonymous_login, null_username, non_null_username,
    user_level_authentication, per_message_authentication,
    k_g, ipmi_v20_extended_capabilities_available, ipmi_v15, ipmi_v20;
  uint64_t val;
  int ret, rv = -1;

  assert (destination);
  assert (buf);
  assert (from);
  assert (version == IPMI_PING_VERSION_1_5 || version == IPMI_PING_VERSION_2_0);

  if (!buflen)
    return (0);

  if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_lan_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr_rs)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_cmd = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rs)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));

  if (debug)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_cmd ((version == IPMI_PING_VERSION_1_5) ? DEBUG_UTIL_TYPE_IPMI_1_5 : DEBUG_UTIL_TYPE_IPMI_2_0,
                     DEBUG_UTIL_DIRECTION_RESPONSE,
                     IPMI_NET_FN_APP_RQ,
                     IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES,
		     0,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_dump_lan_packet (STDERR_FILENO,
                                destination,
                                hdrbuf,
                                NULL,
                                buf,
                                buflen,
                                tmpl_lan_msg_hdr_rs,
                                tmpl_cmd_get_channel_authentication_capabilities_rs) < 0)
        ipmi_ping_err_exit ("ipmi_dump_lan_packet: %s", strerror (errno));
    }

  if ((ret = ipmi_lan_check_packet_checksum (buf, buflen)) < 0)
    ipmi_ping_err_exit ("ipmi_lan_check_checksum: %s", strerror (errno));

  if (!ret)
    {
      if (debug)
        fprintf (stderr, "%s(%d): checksum failed\n", __FUNCTION__, __LINE__);
      rv = 0;
      goto cleanup;
    }

  if ((ret = unassemble_ipmi_lan_pkt (buf,
                                      buflen,
                                      obj_rmcp_hdr,
                                      obj_lan_session_hdr,
                                      obj_lan_msg_hdr,
                                      obj_cmd,
                                      obj_lan_msg_trlr,
				      IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    ipmi_ping_err_exit ("unassemble_ipmi_lan_pkt: %s", strerror (errno));

  if (!ret)
    {
      if (debug)
        fprintf (stderr, "%s(%d): Could not unassemble packet\n", __FUNCTION__, __LINE__);
      rv = 0;
      goto cleanup;
    }

  if ((ret = ipmi_lan_check_net_fn (obj_lan_msg_hdr, IPMI_NET_FN_APP_RS)) < 0)
    ipmi_ping_err_exit ("ipmi_lan_check_net_fn: %s", strerror (errno));

  if (!ret)
    {
      if (debug)
        fprintf (stderr, "%s(%d): net_fn failed\n", __FUNCTION__, __LINE__);
      rv = 0;
      goto cleanup;
    }

  if ((ret = ipmi_check_cmd (obj_cmd, IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES)) < 0)
    ipmi_ping_err_exit ("ipmi_check_cmd: %s", strerror (errno));

  if (!ret)
    {
      if (debug)
        fprintf (stderr, "%s(%d): cmd failed\n", __FUNCTION__, __LINE__);
      rv = 0;
      goto cleanup;
    }

  if ((ret = ipmi_check_completion_code_success (obj_cmd)) < 0)
    ipmi_ping_err_exit ("ipmi_check_comp_code: %s", strerror (errno));

  if (!ret)
    {
      if (debug)
        fprintf (stderr, "%s(%d): comp_code failed\n", __FUNCTION__, __LINE__);
      rv = 0;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_lan_msg_hdr,
                    "rq_seq",
                    &val) < 0)
    ipmi_ping_err_exit ("fiid_obj_get: 'rq_seq': %s",
                        fiid_obj_errormsg (obj_lan_msg_hdr));
  req_seq = val;

  if (req_seq != sequence_number % (IPMI_RQ_SEQ_MAX + 1))
    {
      if (debug)
        fprintf (stderr, "%s(%d): req_seq failed\n", __FUNCTION__, __LINE__);
      rv = 0;
      goto cleanup;
    }

  printf ("response received from %s: rq_seq=%u", from, req_seq);
  if (verbose)
    {
      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.none",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_type.none': %s",
                            fiid_obj_errormsg (obj_cmd));
      none = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.md2",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_type.md2': %s",
                            fiid_obj_errormsg (obj_cmd));
      md2 = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.md5",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_type.md5': %s",
                            fiid_obj_errormsg (obj_cmd));
      md5 = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.straight_password_key",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_type.straight_password_key': %s",
                            fiid_obj_errormsg (obj_cmd));
      straight_password_key = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_type.oem_prop",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_type.oem_prop': %s",
                            fiid_obj_errormsg (obj_cmd));
      oem = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_status.anonymous_login",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_status.anonymous_login': %s",
                            fiid_obj_errormsg (obj_cmd));
      anonymous_login = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_status.null_username",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_status.null_username': %s",
                            fiid_obj_errormsg (obj_cmd));
      null_username = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_status.non_null_username",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_status.non_null_username': %s",
                            fiid_obj_errormsg (obj_cmd));
      non_null_username = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_status.user_level_authentication",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_status.user_level_authentication': %s",
                            fiid_obj_errormsg (obj_cmd));
      user_level_authentication = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_status.per_message_authentication",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_status.per_message_authentication': %s",
                            fiid_obj_errormsg (obj_cmd));
      per_message_authentication = val;

      if (FIID_OBJ_GET (obj_cmd,
                        "authentication_status.per_message_authentication",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'authentication_status.per_message_authentication': %s",
                            fiid_obj_errormsg (obj_cmd));
      per_message_authentication = val;

      printf (", auth: none=%s md2=%s md5=%s password=%s oem=%s anon=%s null=%s non-null=%s user=%s permsg=%s ",
              _setstr (none), _setstr (md2), _setstr (md5),
              _setstr (straight_password_key),_setstr (oem),
              _setstr (anonymous_login), _setstr (null_username),
              _setstr (non_null_username), _setstr (user_level_authentication),
              _setstr (per_message_authentication));

      if (version == IPMI_PING_VERSION_2_0)
        {
          if (FIID_OBJ_GET (obj_cmd,
                            "authentication_type.ipmi_v2.0_extended_capabilities_available",
                            &val) < 0)
            ipmi_ping_err_exit ("fiid_obj_get: 'authentication_type.ipmi_v2.0_extended_capabilities_available': %s",
                                fiid_obj_errormsg (obj_cmd));
          ipmi_v20_extended_capabilities_available = val;

          if (FIID_OBJ_GET (obj_cmd,
                            "authentication_status.k_g",
                            &val) < 0)
            ipmi_ping_err_exit ("fiid_obj_get: 'authentication_status.k_g': %s",
                                fiid_obj_errormsg (obj_cmd));
          k_g = val;

          printf ("k_g=%s ipmi_v2.0_extended_capabilities_available=%s ",
                  _setstr (k_g),
                  _setstr (ipmi_v20_extended_capabilities_available));

          if (ipmi_v20_extended_capabilities_available)
            {
              if (FIID_OBJ_GET (obj_cmd,
                                "channel_supports_ipmi_v1.5_connections",
                                &val) < 0)
                ipmi_ping_err_exit ("fiid_obj_get: 'channel_supports_ipmi_v1.5_connections': %s",
                                    fiid_obj_errormsg (obj_cmd));
              ipmi_v15 = val;

              if (FIID_OBJ_GET (obj_cmd,
                                "channel_supports_ipmi_v2.0_connections",
                                &val) < 0)
                ipmi_ping_err_exit ("fiid_obj_get: 'channel_supports_ipmi_v2.0_connections': %s",
                                    fiid_obj_errormsg (obj_cmd));
              ipmi_v20 = val;

              printf ("ipmi_v1.5=%s ipmi_v2.0=%s ", _setstr (ipmi_v15), _setstr (ipmi_v20));
            }
        }
    }
  printf ("\n");

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_rmcp_hdr);
  fiid_obj_destroy (obj_lan_session_hdr);
  fiid_obj_destroy (obj_lan_msg_hdr);
  fiid_obj_destroy (obj_cmd);
  fiid_obj_destroy (obj_lan_msg_trlr);
  return (rv);
}

void
latepacket (unsigned int sequence_number)
{
  printf ("response timed out: rq_seq=%u\n", sequence_number % (IPMI_RQ_SEQ_MAX + 1));
}

int
endresult (const char *progname,
           const char *dest,
           unsigned int sent_count,
           unsigned int recv_count)
{
  double percent = 0;

  assert (progname);
  assert (dest);

  if (sent_count > 0)
    percent = ((double)(sent_count - recv_count)/sent_count)*100;

  printf ("--- %s %s statistics ---\n", progname, dest);
  printf ("%d requests transmitted, %d responses received in time, "
          "%2.1f%% packet loss\n",
          sent_count, recv_count, percent);

  return ((recv_count > 0) ? 0 : 1);
}

int
main (int argc, char **argv)
{
  ipmi_ping_setup (argc, argv, 0, IPMI_RQ_SEQ_MAX, "hVc:i:I:t:vr:s:d");
  ipmi_ping_loop (createpacket, parsepacket, latepacket, endresult);
  exit (EXIT_FAILURE);                    /* NOT REACHED */
}
