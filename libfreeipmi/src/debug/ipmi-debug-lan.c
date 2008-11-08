/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

static int8_t
_ipmi_dump_lan_packet (int fd, 
                       const char *prefix, 
                       const char *hdr,
                       const char *trlr, 
                       uint8_t *pkt, 
                       uint32_t pkt_len,
                       fiid_template_t tmpl_lan_msg_hdr, 
                       fiid_template_t tmpl_cmd, 
                       fiid_template_t tmpl_ipmb_msg_hdr, 
                       fiid_template_t tmpl_ipmb_cmd)
{
  uint32_t indx = 0;
  int32_t obj_cmd_len, obj_lan_msg_trlr_len;
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  char *rmcp_hdr = 
    "RMCP Header:\n"
    "------------";
  char *session_hdr =
    "IPMI Session Header:\n"
    "--------------------";
  char *msg_hdr =
    "IPMI Message Header:\n"
    "--------------------";
  char *cmd_hdr =
    "IPMI Command Data:\n"
    "------------------";
  char *ipmb_msg_hdr =
    "IPMB Message Header:\n"
    "--------------------";
  char *ipmb_cmd_hdr =
    "IPMB Message Data:\n"
    "------------------";
  char *ipmb_msg_trlr_hdr =
    "IPMB Message Trailer:\n"
    "---------------------";
  char *trlr_hdr =
    "IPMI Trailer:\n"
    "--------------";
  char *unexpected_hdr =
    "Unexpected Data:\n"
    "----------------";
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_session_hdr = NULL;
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_ipmb_msg_hdr = NULL;
  fiid_obj_t obj_ipmb_cmd = NULL;
  fiid_obj_t obj_ipmb_msg_trlr = NULL;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  fiid_obj_t obj_unexpected_data = NULL;
  int32_t len;
  int8_t rv = -1;
  uint64_t authentication_type;

  assert(pkt);
  assert(tmpl_lan_msg_hdr);
  assert(tmpl_cmd);
  assert((!tmpl_ipmb_msg_hdr && !tmpl_ipmb_cmd)
         || (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd));

  ERR(!(ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  ERR(!(ipmi_debug_output_str (fd, prefix_buf, hdr) < 0));

  /* Dump rmcp header */
  
  FIID_OBJ_CREATE_CLEANUP (obj_rmcp_hdr, tmpl_rmcp_hdr);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += len;

  ERR_CLEANUP (!(ipmi_obj_dump (fd, 
                                prefix, 
                                rmcp_hdr,
                                NULL,
                                obj_rmcp_hdr) < 0));
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump session header */
  /* Output of session header depends on the auth code */

  FIID_OBJ_CREATE_CLEANUP (obj_session_hdr, tmpl_lan_session_hdr);
  
  FIID_OBJ_SET_BLOCK_LEN_CLEANUP(len, 
				 obj_session_hdr, 
				 "authentication_type", 
				 "session_id", 
				 pkt + indx, 
				 pkt_len - indx);
  indx += len;
  
  if (pkt_len <= indx)
    {
      ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                                   prefix, 
                                   session_hdr, 
                                   NULL, 
                                   obj_session_hdr) < 0));

      rv = 0;
      goto cleanup;
    }

  FIID_OBJ_GET_CLEANUP (obj_session_hdr, "authentication_type", &authentication_type);

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      FIID_OBJ_SET_DATA_LEN_CLEANUP(len, 
				    obj_session_hdr,
				    "authentication_code",
				    pkt + indx,
				    pkt_len - indx);
      indx += len;
    }

  if (pkt_len <= indx)
    {
      ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                                   prefix, 
                                   session_hdr, 
                                   NULL, 
                                   obj_session_hdr) < 0));

      rv = 0;
      goto cleanup;
    }

  FIID_OBJ_SET_DATA_LEN_CLEANUP(len,
				obj_session_hdr,
				"ipmi_msg_len",
				pkt + indx,
				pkt_len - indx);
  indx += len;

  ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                               prefix, 
                               session_hdr, 
                               NULL, 
                               obj_session_hdr) < 0));

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump message header */

  FIID_OBJ_CREATE_CLEANUP (obj_lan_msg_hdr, tmpl_lan_msg_hdr);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_lan_msg_hdr, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump(fd,
                               prefix,
                               msg_hdr, 
                               NULL, 
                               obj_lan_msg_hdr) < 0));

  /* Clear out data */
  FIID_OBJ_CLEAR_CLEANUP(obj_lan_msg_hdr);

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump command data */

  FIID_OBJ_CREATE_CLEANUP (obj_cmd, tmpl_cmd);
  if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd)
    {
      FIID_OBJ_CREATE_CLEANUP (obj_ipmb_msg_hdr, tmpl_ipmb_msg_hdr);
      FIID_OBJ_CREATE_CLEANUP (obj_ipmb_cmd, tmpl_ipmb_cmd);
      FIID_OBJ_CREATE_CLEANUP (obj_ipmb_msg_trlr, tmpl_ipmb_msg_trlr);
    }
  FIID_OBJ_CREATE_CLEANUP (obj_lan_msg_trlr, tmpl_lan_msg_trlr);
  
  FIID_TEMPLATE_LEN_BYTES (obj_lan_msg_trlr_len, tmpl_lan_msg_trlr);

  if ((pkt_len - indx) >= obj_lan_msg_trlr_len)
    obj_cmd_len = (pkt_len - indx) - obj_lan_msg_trlr_len;
  else
    obj_cmd_len = 0;

  if (obj_cmd_len)
    {
      uint8_t ipmb_buf[IPMI_DEBUG_MAX_PKT_LEN];
      int32_t ipmb_buf_len = 0;

      FIID_OBJ_SET_ALL_LEN_CLEANUP (len, 
				    obj_cmd,
				    pkt + indx, 
				    obj_cmd_len);
      indx += len;
      
      if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd)
        {
          memset(ipmb_buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
          FIID_OBJ_GET_DATA_LEN_CLEANUP (ipmb_buf_len,
                                         obj_cmd,
                                         "message_data",
                                         ipmb_buf,
                                         IPMI_DEBUG_MAX_PKT_LEN);
          
          FIID_OBJ_CLEAR_FIELD_CLEANUP (obj_cmd, "message_data");
        }

      ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                                   prefix, 
                                   cmd_hdr, 
                                   NULL, 
                                   obj_cmd) < 0));

      if (pkt_len <= indx)
	{
	  rv = 0;
	  goto cleanup;
	}

      if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd && ipmb_buf_len)
        {
          int32_t obj_ipmb_msg_trlr_len = 0;
          int32_t obj_ipmb_cmd_len = 0;
          int32_t ipmb_hdr_len = 0;
          int32_t ipmb_cmd_len = 0;
         
          FIID_TEMPLATE_LEN_BYTES (obj_ipmb_msg_trlr_len, tmpl_ipmb_msg_trlr);

          FIID_OBJ_SET_ALL_LEN_CLEANUP (ipmb_hdr_len,
                                        obj_ipmb_msg_hdr,
                                        ipmb_buf,
                                        ipmb_buf_len);

          ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                                       prefix, 
                                       ipmb_msg_hdr,
                                       NULL, 
                                       obj_ipmb_msg_hdr) < 0));

          if ((ipmb_buf_len - ipmb_hdr_len) >= obj_ipmb_msg_trlr_len)
            obj_ipmb_cmd_len = (ipmb_buf_len - ipmb_hdr_len) - obj_ipmb_msg_trlr_len;
          else
            obj_ipmb_cmd_len = 0;

          if (obj_ipmb_cmd_len) 
            {
              FIID_OBJ_SET_ALL_LEN_CLEANUP (ipmb_cmd_len,
                                            obj_ipmb_cmd,
                                            ipmb_buf + ipmb_hdr_len,
                                            obj_ipmb_cmd_len);
              
              ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                                           prefix, 
                                           ipmb_cmd_hdr,
                                           NULL, 
                                           obj_ipmb_cmd) < 0));
            }

          FIID_OBJ_SET_ALL_LEN_CLEANUP (len,
                                        obj_ipmb_msg_trlr, 
                                        ipmb_buf + ipmb_hdr_len + ipmb_cmd_len,
                                        (ipmb_buf_len - ipmb_hdr_len - ipmb_cmd_len));
          
          ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                                       prefix, 
                                       ipmb_msg_trlr_hdr,
                                       NULL, 
                                       obj_ipmb_msg_trlr) < 0));
        }
    }

  /* Dump trailer */

  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_lan_msg_trlr, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                               prefix,
                               trlr_hdr,
                               NULL, 
                               obj_lan_msg_trlr) < 0));
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump unexpected stuff */
  
  FIID_OBJ_CREATE_CLEANUP (obj_unexpected_data, tmpl_unexpected_data);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_unexpected_data, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump(fd, 
                               prefix, 
                               unexpected_hdr, 
                               NULL,
                               obj_unexpected_data) < 0));
  
  ERR_CLEANUP (!(ipmi_debug_output_str(fd, prefix_buf, trlr) < 0));

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY(obj_rmcp_hdr);
  FIID_OBJ_DESTROY(obj_session_hdr);
  FIID_OBJ_DESTROY(obj_lan_msg_hdr);
  FIID_OBJ_DESTROY(obj_cmd);
  FIID_OBJ_DESTROY(obj_ipmb_msg_hdr);
  FIID_OBJ_DESTROY(obj_ipmb_cmd);
  FIID_OBJ_DESTROY(obj_ipmb_msg_trlr);
  FIID_OBJ_DESTROY(obj_lan_msg_trlr);
  FIID_OBJ_DESTROY(obj_unexpected_data);
  return (rv);
}

int8_t
ipmi_dump_lan_packet (int fd, 
                      const char *prefix,
                      const char *hdr, 
                      const char *trlr, 
                      uint8_t *pkt, 
                      uint32_t pkt_len, 
                      fiid_template_t tmpl_lan_msg_hdr, 
                      fiid_template_t tmpl_cmd)
{
  ERR_EINVAL (pkt && tmpl_lan_msg_hdr && tmpl_cmd);

  return _ipmi_dump_lan_packet (fd, 
                                prefix, 
                                hdr, 
                                trlr, 
                                pkt, 
                                pkt_len, 
                                tmpl_lan_msg_hdr, 
                                tmpl_cmd,
                                NULL,
                                NULL);
}

int8_t
ipmi_dump_lan_packet_ipmb (int fd,
                           const char *prefix, 
                           const char *hdr, 
                           const char *trlr, 
                           uint8_t *pkt,
                           uint32_t pkt_len, 
                           fiid_template_t tmpl_lan_msg_hdr, 
                           fiid_template_t tmpl_cmd,
                           fiid_template_t tmpl_ipmb_msg_hdr, 
                           fiid_template_t tmpl_ipmb_cmd)
{
  int ret1, ret2;

  ERR_EINVAL (pkt
              && tmpl_lan_msg_hdr
              && tmpl_cmd
              && tmpl_ipmb_msg_hdr
              && tmpl_ipmb_cmd);

  if ((ret1 = fiid_template_compare(tmpl_cmd, tmpl_cmd_send_message_rq)) < 0)
    return -1;

  if ((ret2 = fiid_template_compare(tmpl_cmd, tmpl_cmd_get_message_rs)) < 0)
    return -1;

  ERR_EINVAL ((ret1 || ret2));

  return _ipmi_dump_lan_packet (fd, 
                                prefix, 
                                hdr, 
                                trlr, 
                                pkt, 
                                pkt_len, 
                                tmpl_lan_msg_hdr, 
                                tmpl_cmd,
                                tmpl_ipmb_msg_hdr,
                                tmpl_ipmb_cmd);
}
