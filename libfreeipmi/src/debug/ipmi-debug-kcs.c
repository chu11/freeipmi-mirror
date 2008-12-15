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
#include "freeipmi/interface/ipmi-kcs-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

static int8_t
_ipmi_dump_kcs_packet (int fd, 
                       const char *prefix, 
                       const char *hdr,
                       const char *trlr, 
                       uint8_t *pkt, 
                       uint32_t pkt_len,
                       fiid_template_t tmpl_cmd, 
                       fiid_template_t tmpl_ipmb_msg_hdr, 
                       fiid_template_t tmpl_ipmb_cmd)
{
  uint32_t indx = 0;
  int32_t obj_cmd_len;
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  char *kcs_hdr = 
    "KCS Header:\n"
    "------------";
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
  char *unexpected_hdr =
    "Unexpected Data:\n"
    "----------------";
  fiid_obj_t obj_kcs_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_ipmb_msg_hdr = NULL;
  fiid_obj_t obj_ipmb_cmd = NULL;
  fiid_obj_t obj_ipmb_msg_trlr = NULL;
  fiid_obj_t obj_unexpected_data = NULL;
  int32_t len;
  int8_t rv = -1;

  assert(pkt);
  assert(tmpl_cmd);
  assert((!tmpl_ipmb_msg_hdr && !tmpl_ipmb_cmd)
         || (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd));

  ERR(!(ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  ERR(!(ipmi_debug_output_str (fd, prefix_buf, hdr) < 0));

  /* Dump kcs header */
  
  FIID_OBJ_CREATE_CLEANUP (obj_kcs_hdr, tmpl_hdr_kcs);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_kcs_hdr, pkt + indx, pkt_len - indx);
  indx += len;

  ERR_CLEANUP (!(ipmi_obj_dump (fd, 
                                prefix, 
                                kcs_hdr,
                                NULL,
                                obj_kcs_hdr) < 0));
  
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

  obj_cmd_len = (pkt_len - indx);

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

      if (pkt_len <= indx)
	{
	  rv = 0;
	  goto cleanup;
	}
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
  FIID_OBJ_DESTROY(obj_kcs_hdr);
  FIID_OBJ_DESTROY(obj_cmd);
  FIID_OBJ_DESTROY(obj_ipmb_msg_hdr);
  FIID_OBJ_DESTROY(obj_ipmb_cmd);
  FIID_OBJ_DESTROY(obj_ipmb_msg_trlr);
  FIID_OBJ_DESTROY(obj_unexpected_data);
  return (rv);
}

int8_t
ipmi_dump_kcs_packet (int fd, 
                      const char *prefix,
                      const char *hdr, 
                      const char *trlr, 
                      uint8_t *pkt, 
                      uint32_t pkt_len, 
                      fiid_template_t tmpl_cmd)
{
  ERR_EINVAL (pkt && tmpl_cmd);

  return _ipmi_dump_kcs_packet (fd, 
                                prefix, 
                                hdr, 
                                trlr, 
                                pkt, 
                                pkt_len, 
                                tmpl_cmd,
                                NULL,
                                NULL);
}

int8_t
ipmi_dump_kcs_packet_ipmb (int fd,
                           const char *prefix, 
                           const char *hdr, 
                           const char *trlr, 
                           uint8_t *pkt,
                           uint32_t pkt_len, 
                           fiid_template_t tmpl_cmd,
                           fiid_template_t tmpl_ipmb_msg_hdr,
                           fiid_template_t tmpl_ipmb_cmd)
{
  int ret1, ret2;

  ERR_EINVAL (pkt
              && tmpl_cmd
              && tmpl_ipmb_msg_hdr
              && tmpl_ipmb_cmd);

  if ((ret1 = fiid_template_compare(tmpl_cmd, tmpl_cmd_send_message_rq)) < 0)
    return -1;

  if ((ret2 = fiid_template_compare(tmpl_cmd, tmpl_cmd_get_message_rs)) < 0)
    return -1;
  
  ERR_EINVAL ((ret1 || ret2));

  return _ipmi_dump_kcs_packet (fd, 
                                prefix, 
                                hdr, 
                                trlr, 
                                pkt, 
                                pkt_len, 
                                tmpl_cmd,
                                tmpl_ipmb_msg_hdr,
                                tmpl_ipmb_cmd);
}
