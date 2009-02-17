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

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"
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

  if (ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE(errno);
      return (-1);
    }

  if (ipmi_debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE(errno);
      return (-1);
    }

  /* Dump kcs header */
  
  if (!(obj_kcs_hdr = fiid_obj_create (tmpl_hdr_kcs)))
    {
      ERRNO_TRACE(errno);
      goto cleanup;
    }
  
  if ((len = fiid_obj_set_all (obj_kcs_hdr,
                               pkt + indx, 
                               pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO(obj_kcs_hdr);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd, 
                     prefix, 
                     kcs_hdr,
                     NULL,
                     obj_kcs_hdr) < 0)
    {
      ERRNO_TRACE(errno);
      goto cleanup;
    }
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump command data */

  if (!(obj_cmd = fiid_obj_create (tmpl_cmd)))
    {
      ERRNO_TRACE(errno);
      goto cleanup;
    }
  if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd)
    {
      if (!(obj_ipmb_msg_hdr = fiid_obj_create (tmpl_ipmb_msg_hdr)))
        {
          ERRNO_TRACE(errno);
          goto cleanup;
        }
      if (!(obj_ipmb_cmd = fiid_obj_create (tmpl_ipmb_cmd)))
        {
          ERRNO_TRACE(errno);
          goto cleanup;
        }
      if (!(obj_ipmb_msg_trlr = fiid_obj_create (tmpl_ipmb_msg_trlr)))
        {
          ERRNO_TRACE(errno);
          goto cleanup;
        }
    }

  obj_cmd_len = (pkt_len - indx);

  if (obj_cmd_len)
    {
      uint8_t ipmb_buf[IPMI_DEBUG_MAX_PKT_LEN];
      int32_t ipmb_buf_len = 0;

      if ((len = fiid_obj_set_all (obj_cmd,
                                   pkt + indx, 
                                   obj_cmd_len)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO(obj_cmd);
          goto cleanup;
        }
      indx += len;
      
      if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd)
        {
          memset(ipmb_buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);

          if ((ipmb_buf_len = fiid_obj_get_data(obj_cmd,
                                                "message_data",
                                                ipmb_buf,
                                                IPMI_DEBUG_MAX_PKT_LEN)) < 0)
            {
              FIID_OBJECT_ERROR_TO_ERRNO(obj_cmd);
              goto cleanup;
            }
          
          if (fiid_obj_clear_field(obj_cmd, "message_data") < 0)
            {
              FIID_OBJECT_ERROR_TO_ERRNO(obj_cmd);
              goto cleanup;
            }
        }
      
      if (ipmi_obj_dump(fd, 
                        prefix, 
                        cmd_hdr, 
                        NULL, 
                        obj_cmd) < 0)
        {
          ERRNO_TRACE(errno);
          goto cleanup;
        }
      
      if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd && ipmb_buf_len)
        {
          int32_t obj_ipmb_msg_trlr_len = 0;
          int32_t obj_ipmb_cmd_len = 0;
          int32_t ipmb_hdr_len = 0;
          int32_t ipmb_cmd_len = 0;
          
          if ((obj_ipmb_msg_trlr_len = fiid_template_len_bytes(tmpl_ipmb_msg_trlr)) < 0)
            {
              ERRNO_TRACE(errno);
              goto cleanup;
            }
          
          if ((ipmb_hdr_len = fiid_obj_set_all (obj_ipmb_msg_hdr,
                                                ipmb_buf,
                                                ipmb_buf_len)) < 0)
            {
              FIID_OBJECT_ERROR_TO_ERRNO(obj_ipmb_msg_hdr);
              goto cleanup;
            }
          
          if (ipmi_obj_dump(fd, 
                            prefix, 
                            ipmb_msg_hdr,
                            NULL, 
                            obj_ipmb_msg_hdr) < 0)
            {
              ERRNO_TRACE(errno);
              goto cleanup;
            }
          
          if ((ipmb_buf_len - ipmb_hdr_len) >= obj_ipmb_msg_trlr_len)
            obj_ipmb_cmd_len = (ipmb_buf_len - ipmb_hdr_len) - obj_ipmb_msg_trlr_len;
          else
            obj_ipmb_cmd_len = 0;
          
          if (obj_ipmb_cmd_len) 
            {
              if ((ipmb_cmd_len = fiid_obj_set_all (obj_ipmb_cmd,
                                                    ipmb_buf + ipmb_hdr_len,
                                                    obj_ipmb_cmd_len)) < 0)
                {
                  FIID_OBJECT_ERROR_TO_ERRNO(obj_ipmb_cmd);
                  goto cleanup;
                }
              
              if (ipmi_obj_dump(fd, 
                                prefix, 
                                ipmb_cmd_hdr,
                                NULL, 
                                obj_ipmb_cmd) < 0)
                {
                  ERRNO_TRACE(errno);
                  goto cleanup;
                }
            }
          
          if ((len = fiid_obj_set_all (obj_ipmb_msg_trlr, 
                                       ipmb_buf + ipmb_hdr_len + ipmb_cmd_len,
                                       (ipmb_buf_len - ipmb_hdr_len - ipmb_cmd_len))) < 0)
            {
              FIID_OBJECT_ERROR_TO_ERRNO(obj_ipmb_msg_trlr);
              goto cleanup;
            }
          
          if (ipmi_obj_dump(fd, 
                            prefix, 
                            ipmb_msg_trlr_hdr,
                            NULL, 
                            obj_ipmb_msg_trlr) < 0)
            {
              ERRNO_TRACE(errno);
              goto cleanup;
            }
        }

      if (pkt_len <= indx)
	{
	  rv = 0;
	  goto cleanup;
	}
    }
  
  /* Dump unexpected stuff */
  
  if (!(obj_unexpected_data = fiid_obj_create (tmpl_unexpected_data)))
    {
      ERRNO_TRACE(errno);
      goto cleanup;
    }
  
  if ((len = fiid_obj_set_all (obj_unexpected_data, 
                               pkt + indx,
                               pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO(obj_unexpected_data);
      goto cleanup;
    }
  indx += len;
  
  if (ipmi_obj_dump(fd, 
                    prefix, 
                    unexpected_hdr, 
                    NULL,
                    obj_unexpected_data) < 0)
    {
      ERRNO_TRACE(errno);
      goto cleanup;
    }
  
  if (ipmi_debug_output_str(fd, prefix_buf, trlr) < 0)
    {
      ERRNO_TRACE(errno);
      goto cleanup;
    }
  
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
  if (!pkt || !tmpl_cmd)
    {
      SET_ERRNO(EINVAL);
      return (-1);
    }

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

  if (!pkt
      || !tmpl_cmd
      || !tmpl_ipmb_msg_hdr
      || !tmpl_ipmb_cmd)
    {
      SET_ERRNO(EINVAL);
      return (-1);
    }

  if ((ret1 = fiid_template_compare(tmpl_cmd, tmpl_cmd_send_message_rq)) < 0)
    return -1;

  if ((ret2 = fiid_template_compare(tmpl_cmd, tmpl_cmd_get_message_rs)) < 0)
    return -1;
  
  if (!ret1 && !ret2)
    {
      SET_ERRNO(EINVAL);
      return (-1);
    }

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
