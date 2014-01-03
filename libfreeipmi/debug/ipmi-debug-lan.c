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
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

static int
_ipmi_dump_lan_packet (int fd,
                       const char *prefix,
                       const char *hdr,
                       const char *trlr,
                       const void *pkt,
                       unsigned int pkt_len,
                       fiid_template_t tmpl_lan_msg_hdr,
                       fiid_template_t tmpl_cmd,
                       fiid_template_t tmpl_ipmb_msg_hdr,
                       fiid_template_t tmpl_ipmb_cmd)
{
  unsigned int indx = 0;
  int obj_lan_msg_trlr_len;
  unsigned int obj_cmd_len;
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
  fiid_obj_t obj_lan_msg_trlr = NULL;
  fiid_obj_t obj_unexpected_data = NULL;
  uint8_t ipmb_buf[IPMI_DEBUG_MAX_PKT_LEN];
  int ipmb_buf_len = 0;
  int len, rv = -1;
  uint8_t authentication_type;
  uint64_t val;

  assert (pkt);
  assert (tmpl_lan_msg_hdr);
  assert (tmpl_cmd);
  assert ((!tmpl_ipmb_msg_hdr && !tmpl_ipmb_cmd)
          || (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd));

  if (debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  /* Dump rmcp header */

  if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((len = fiid_obj_set_all (obj_rmcp_hdr,
                               pkt + indx,
                               pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd,
                     prefix,
                     rmcp_hdr,
                     NULL,
                     obj_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (pkt_len <= indx)
    goto out;

  /* Dump session header */
  /* Output of session header depends on the auth code */

  if (!(obj_session_hdr = fiid_obj_create (tmpl_lan_session_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((len = fiid_obj_set_block (obj_session_hdr,
                                 "authentication_type",
                                 "session_id",
                                 pkt + indx,
                                 pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr);
      goto cleanup;
    }
  indx += len;

  if (pkt_len <= indx)
    {
      if (ipmi_obj_dump (fd,
                         prefix,
                         session_hdr,
                         NULL,
                         obj_session_hdr) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
      goto out;
    }

  if (FIID_OBJ_GET (obj_session_hdr,
                    "authentication_type",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr);
      goto cleanup;
    }
  authentication_type = val;

  /* don't know how to parse, just output in raw form */
  if (!IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type))
    goto dump_extra;

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      if ((len = fiid_obj_set_data (obj_session_hdr,
                                    "authentication_code",
                                    pkt + indx,
                                    pkt_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr);
          goto cleanup;
        }
      indx += len;
    }

  if (pkt_len <= indx)
    {
      if (ipmi_obj_dump (fd,
                         prefix,
                         session_hdr,
                         NULL,
                         obj_session_hdr) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
      goto out;
    }

  if ((len = fiid_obj_set_data (obj_session_hdr,
                                "ipmi_msg_len",
                                pkt + indx,
                                pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd,
                     prefix,
                     session_hdr,
                     NULL,
                     obj_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (pkt_len <= indx)
    goto out;

  /* Dump message header */

  if (!(obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((len = fiid_obj_set_all (obj_lan_msg_hdr,
                               pkt + indx,
                               pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd,
                     prefix,
                     msg_hdr,
                     NULL,
                     obj_lan_msg_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* Clear out data */
  if (fiid_obj_clear (obj_lan_msg_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      goto cleanup;
    }

  if (pkt_len <= indx)
    goto out;

  /* Dump command data */

  if (!(obj_cmd = fiid_obj_create (tmpl_cmd)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (!(obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((pkt_len - indx) <= obj_lan_msg_trlr_len)
    goto dump_extra;

  obj_cmd_len = (pkt_len - indx) - obj_lan_msg_trlr_len;

  if ((len = fiid_obj_set_all (obj_cmd,
                               pkt + indx,
                               obj_cmd_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }
  indx += len;
  
  if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd)
    {
      memset (ipmb_buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      
      if ((ipmb_buf_len = fiid_obj_get_data (obj_cmd,
                                             "message_data",
                                             ipmb_buf,
                                             IPMI_DEBUG_MAX_PKT_LEN)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          goto cleanup;
        }
      
      if (fiid_obj_clear_field (obj_cmd, "message_data") < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          goto cleanup;
        }
    }
  
  if (ipmi_obj_dump (fd,
                     prefix,
                     cmd_hdr,
                     NULL,
                     obj_cmd) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  
  if (tmpl_ipmb_msg_hdr && tmpl_ipmb_cmd && ipmb_buf_len)
    {
      if (debug_dump_ipmb (fd,
			   prefix,
			   ipmb_buf,
			   ipmb_buf_len,
			   tmpl_ipmb_msg_hdr,
			   tmpl_ipmb_cmd) < 0)
        goto cleanup;
    }

  if (pkt_len <= indx)
    goto out;

  /* Dump trailer */

  if ((len = fiid_obj_set_all (obj_lan_msg_trlr,
                               pkt + indx,
                               pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
      goto cleanup;
    }
  indx += len;

  if (ipmi_obj_dump (fd,
                     prefix,
                     trlr_hdr,
                     NULL,
                     obj_lan_msg_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (pkt_len <= indx)
    goto out;

  /* Dump unexpected stuff */

 dump_extra:

  if ((pkt_len - indx) > 0)
    {
      if (!(obj_unexpected_data = fiid_obj_create (tmpl_unexpected_data)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
      
      if ((len = fiid_obj_set_all (obj_unexpected_data,
                                   pkt + indx,
                                   pkt_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_unexpected_data);
          goto cleanup;
        }
      indx += len;
      
      if (ipmi_obj_dump (fd,
                         prefix,
                         unexpected_hdr,
                         NULL,
                         obj_unexpected_data) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }

  if (debug_output_str (fd, prefix_buf, trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

 out:
#if WITH_RAWDUMPS
  /* For those vendors that get confused when they see the nice output
   * and want the hex output
   */
  if (ipmi_dump_hex (fd,
                     prefix,
                     hdr,
		     trlr,
		     pkt,
		     pkt_len) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
#endif

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_rmcp_hdr);
  fiid_obj_destroy (obj_session_hdr);
  fiid_obj_destroy (obj_lan_msg_hdr);
  fiid_obj_destroy (obj_cmd);
  fiid_obj_destroy (obj_lan_msg_trlr);
  fiid_obj_destroy (obj_unexpected_data);
  return (rv);
}

int
ipmi_dump_lan_packet (int fd,
                      const char *prefix,
                      const char *hdr,
                      const char *trlr,
                      const void *pkt,
                      unsigned int pkt_len,
                      fiid_template_t tmpl_lan_msg_hdr,
                      fiid_template_t tmpl_cmd)
{
  if (!pkt
      || !tmpl_lan_msg_hdr
      || !tmpl_cmd)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (_ipmi_dump_lan_packet (fd,
                                 prefix,
                                 hdr,
                                 trlr,
                                 pkt,
                                 pkt_len,
                                 tmpl_lan_msg_hdr,
                                 tmpl_cmd,
                                 NULL,
                                 NULL));
}

int
ipmi_dump_lan_packet_ipmb (int fd,
                           const char *prefix,
                           const char *hdr,
                           const char *trlr,
                           const void *pkt,
                           unsigned int pkt_len,
                           fiid_template_t tmpl_lan_msg_hdr,
                           fiid_template_t tmpl_cmd,
                           fiid_template_t tmpl_ipmb_msg_hdr,
                           fiid_template_t tmpl_ipmb_cmd)
{
  int ret1, ret2;

  if (!pkt
      || !tmpl_lan_msg_hdr
      || !tmpl_cmd
      || !tmpl_ipmb_msg_hdr
      || !tmpl_ipmb_cmd)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if ((ret1 = fiid_template_compare (tmpl_cmd, tmpl_cmd_send_message_rq)) < 0)
    return (-1);

  if ((ret2 = fiid_template_compare (tmpl_cmd, tmpl_cmd_get_message_rs)) < 0)
    return (-1);

  if (!ret1 && !ret2)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (_ipmi_dump_lan_packet (fd,
                                 prefix,
                                 hdr,
                                 trlr,
                                 pkt,
                                 pkt_len,
                                 tmpl_lan_msg_hdr,
                                 tmpl_cmd,
                                 tmpl_ipmb_msg_hdr,
                                 tmpl_ipmb_cmd));
}
