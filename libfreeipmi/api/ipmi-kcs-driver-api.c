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
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/interface/ipmi-kcs-interface.h"
#include "freeipmi/util/ipmi-ipmb-util.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-trace.h"
#include "ipmi-api-util.h"
#include "ipmi-kcs-driver-api.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

/* achu: I dunno what's a good number, 32 seems good.  Similar to the
 * inband "TIMEOUT", the purpose is to just not hang any user code
 * trying to do ipmb.  You gotta give up at some point.
 *
 * achu: On Inventec 5441/Dell Xanadu II 10 seemed too low, so upped it to
 * 32.
 *
 * achu: On Wistron / Dell Poweredge C6220 32 seemed too low, so upped
 * it to 64.
 */
#define IPMI_KCS_IPMB_RETRANSMISSION_COUNT   64
#define IPMI_KCS_IPMB_REREAD_COUNT           64
#define IPMI_KCS_IPMB_REREAD_WAIT            5000

fiid_template_t tmpl_kcs_raw =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8192, "raw_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

static void
_api_kcs_dump (ipmi_ctx_t ctx,
	       const void *pkt,
	       unsigned int pkt_len,
	       uint8_t cmd,
	       uint8_t net_fn,
	       uint8_t group_extension,
	       unsigned int debug_direction,
	       fiid_template_t tmpl_cmd,
	       fiid_template_t tmpl_ipmb_msg_hdr,
	       fiid_template_t tmpl_ipmb_cmd)
{
  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
          && pkt
          && pkt_len
          && (debug_direction == DEBUG_UTIL_DIRECTION_REQUEST
              || debug_direction == DEBUG_UTIL_DIRECTION_RESPONSE)
          && tmpl_cmd);

  /* Don't cleanup/return an error here.  It's just debug code. */

  debug_hdr_cmd (DEBUG_UTIL_TYPE_INBAND,
                 debug_direction,
                 net_fn,
                 cmd,
		 group_extension,
                 hdrbuf,
                 DEBUG_UTIL_HDR_BUFLEN);

  if (tmpl_ipmb_cmd
      && ((cmd == IPMI_CMD_SEND_MESSAGE
           && debug_direction == DEBUG_UTIL_DIRECTION_REQUEST)
          || (cmd == IPMI_CMD_GET_MESSAGE
              && debug_direction == DEBUG_UTIL_DIRECTION_RESPONSE)))
    ipmi_dump_kcs_packet_ipmb (STDERR_FILENO,
                               NULL,
                               hdrbuf,
                               NULL,
                               pkt,
                               pkt_len,
                               tmpl_cmd,
                               tmpl_ipmb_msg_hdr,
                               tmpl_ipmb_cmd);
  else
    ipmi_dump_kcs_packet (STDERR_FILENO,
                          NULL,
                          hdrbuf,
                          NULL,
                          pkt,
                          pkt_len,
                          tmpl_cmd);

}

static void
_api_kcs_dump_rq (ipmi_ctx_t ctx,
		  const void *pkt,
		  unsigned int pkt_len,
		  uint8_t cmd,
		  uint8_t net_fn,
		  uint8_t group_extension,
		  fiid_obj_t obj_cmd_rq)
{
  fiid_field_t *tmpl_cmd = NULL;

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rq)))
    {
      _api_kcs_dump (ctx,
		     pkt,
		     pkt_len,
		     cmd,
		     net_fn,
		     group_extension,
		     DEBUG_UTIL_DIRECTION_REQUEST,
		     tmpl_cmd,
		     tmpl_ipmb_msg_hdr_rq,
		     ctx->tmpl_ipmb_cmd_rq);
      fiid_template_free (tmpl_cmd);
    }
}

static void
_api_kcs_dump_rs (ipmi_ctx_t ctx,
		  const void *pkt,
		  unsigned int pkt_len,
		  uint8_t cmd,
		  uint8_t net_fn,
		  uint8_t group_extension,
		  fiid_obj_t obj_cmd_rs)
{
  fiid_field_t *tmpl_cmd = NULL;

  if ((tmpl_cmd = fiid_obj_template (obj_cmd_rs)))
    {
      _api_kcs_dump (ctx,
		     pkt,
		     pkt_len,
		     cmd,
		     net_fn,
		     group_extension,
		     DEBUG_UTIL_DIRECTION_RESPONSE,
		     tmpl_cmd,
		     tmpl_ipmb_msg_hdr_rs,
		     ctx->tmpl_ipmb_cmd_rs);
      fiid_template_free (tmpl_cmd);
    }
}

static int
_kcs_cmd_write (ipmi_ctx_t ctx,
		uint8_t cmd,
		uint8_t group_extension,
		fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  int hdr_len, cmd_len, send_len, rv = -1;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && fiid_obj_valid (obj_cmd_rq));

  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if ((cmd_len = fiid_obj_len_bytes (obj_cmd_rq)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }
  
  pkt_len = hdr_len + cmd_len;
    
  if (!(pkt = malloc (pkt_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  memset (pkt, '\0', pkt_len);

  if (fill_hdr_ipmi_kcs (ctx->target.lun,
                         ctx->target.net_fn,
                         ctx->io.inband.rq.obj_hdr) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if ((send_len = assemble_ipmi_kcs_pkt (ctx->io.inband.rq.obj_hdr,
                                         obj_cmd_rq,
                                         pkt,
                                         pkt_len,
					 IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && send_len)
    _api_kcs_dump_rq (ctx,
		      pkt,
		      send_len,
		      cmd,
		      ctx->target.net_fn,
		      group_extension,
		      obj_cmd_rq);
  
  if (ipmi_kcs_write (ctx->io.inband.kcs_ctx, pkt, send_len) < 0)
    {
      API_KCS_ERRNUM_TO_API_ERRNUM (ctx, ipmi_kcs_ctx_errnum (ctx->io.inband.kcs_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  free (pkt);
  return (rv);
}

static int
_kcs_cmd_read (ipmi_ctx_t ctx,
	       uint8_t cmd,
	       uint8_t group_extension,
	       fiid_obj_t obj_cmd_rs)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  int hdr_len, cmd_len, read_len;
  fiid_field_t *tmpl = NULL;
  int ret, rv = -1;
  unsigned int intf_flags = IPMI_INTERFACE_FLAGS_DEFAULT;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    intf_flags |= IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK;

  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(tmpl = fiid_obj_template (obj_cmd_rs)))
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  if ((cmd_len = fiid_template_len_bytes (tmpl)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  pkt_len = hdr_len + cmd_len;
  
  if (!(pkt = malloc (pkt_len)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  memset (pkt, '\0', pkt_len);

  if ((read_len = ipmi_kcs_read (ctx->io.inband.kcs_ctx,
                                 pkt,
                                 pkt_len)) < 0)
    {
      API_KCS_ERRNUM_TO_API_ERRNUM (ctx, ipmi_kcs_ctx_errnum (ctx->io.inband.kcs_ctx));
      goto cleanup;
    }
  
  if (!read_len)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_SYSTEM_ERROR);
      goto cleanup;
    }
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP && read_len)
    _api_kcs_dump_rs (ctx,
		      pkt,
		      read_len,
		      cmd,
		      ctx->target.net_fn,
		      group_extension,
		      obj_cmd_rs);
  
  if ((ret = unassemble_ipmi_kcs_pkt (pkt,
                                      read_len,
                                      ctx->io.inband.rs.obj_hdr,
                                      obj_cmd_rs,
				      intf_flags)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* IPMI didn't return enough data back to you */
  if (!ret)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_IPMI_ERROR);
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  free (pkt);
  fiid_template_free (tmpl);
  return (rv);
}

int
api_kcs_cmd (ipmi_ctx_t ctx,
	     fiid_obj_t obj_cmd_rq,
	     fiid_obj_t obj_cmd_rs)
{
  uint8_t cmd = 0;             /* used for debugging */
  uint8_t group_extension = 0; /* used for debugging */
  uint64_t val;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && ctx->type == IPMI_DEVICE_KCS
	  && fiid_obj_valid (obj_cmd_rq)
	  && fiid_obj_packet_valid (obj_cmd_rq) == 1
	  && fiid_obj_valid (obj_cmd_rs));
  
  if (ctx->flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* ignore error, continue on */
      if (FIID_OBJ_GET (obj_cmd_rq, "cmd", &val) < 0)
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
    }

  if (_kcs_cmd_write (ctx, cmd, group_extension, obj_cmd_rq) < 0)
    return (-1);

  if (_kcs_cmd_read (ctx, cmd, group_extension, obj_cmd_rs) < 0)
    return (-1);

  return (0);
}

static int
_api_kcs_ipmb_send (ipmi_ctx_t ctx,
		    fiid_obj_t obj_cmd_rq)
{
  struct ipmi_ctx_target target_save;
  uint8_t buf[IPMI_MAX_PKT_LEN];
  fiid_obj_t obj_ipmb_msg_hdr_rq = NULL;
  fiid_obj_t obj_ipmb_msg_rq = NULL;
  fiid_obj_t obj_send_cmd_rs = NULL;
  int len, ret, rv = -1;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_KCS
          && fiid_obj_valid (obj_cmd_rq)
          && fiid_obj_packet_valid (obj_cmd_rq) == 1);

  if (!(obj_ipmb_msg_hdr_rq = fiid_obj_create (tmpl_ipmb_msg_hdr_rq)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_ipmb_msg_rq = fiid_obj_create (tmpl_ipmb_msg)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_send_cmd_rs = fiid_obj_create (tmpl_cmd_send_message_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fill_ipmb_msg_hdr (ctx->target.rs_addr,
                         ctx->target.net_fn,
                         ctx->target.lun,
                         IPMI_SLAVE_ADDRESS_BMC,
                         IPMI_BMC_IPMB_LUN_SMS_MSG_LUN,
                         ctx->io.inband.rq_seq,
                         obj_ipmb_msg_hdr_rq) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (assemble_ipmi_ipmb_msg (obj_ipmb_msg_hdr_rq,
                              obj_cmd_rq,
                              obj_ipmb_msg_rq,
			      IPMI_INTERFACE_FLAGS_DEFAULT) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  memset (buf, '\0', IPMI_MAX_PKT_LEN);
  if ((len = fiid_obj_get_all (obj_ipmb_msg_rq,
                               buf,
                               IPMI_MAX_PKT_LEN)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_ipmb_msg_rq);
      goto cleanup;
    }

  /* send_message will send to the BMC, so clear out target information */
  memcpy (&target_save, &ctx->target, sizeof (target_save));
  ctx->target.channel_number_is_set = 0;
  ctx->target.rs_addr_is_set = 0;
  
  ret = ipmi_cmd_send_message (ctx,
			       target_save.channel_number,
			       IPMI_SEND_MESSAGE_AUTHENTICATION_NOT_REQUIRED,
			       IPMI_SEND_MESSAGE_ENCRYPTION_NOT_REQUIRED,
			       IPMI_SEND_MESSAGE_TRACKING_OPERATION_NO_TRACKING,
			       buf,
			       len,
			       obj_send_cmd_rs);

  /* restore target info */
  memcpy (&ctx->target, &target_save, sizeof (target_save));

  if (ret < 0)
    {
      /* assume these mean can't send message, bad slave address, etc. */
      if (ipmi_check_completion_code (obj_send_cmd_rs, IPMI_COMP_CODE_SEND_MESSAGE_LOST_ARBITRATION) == 1
          || ipmi_check_completion_code (obj_send_cmd_rs, IPMI_COMP_CODE_SEND_MESSAGE_BUS_ERROR) == 1
          || ipmi_check_completion_code (obj_send_cmd_rs, IPMI_COMP_CODE_SEND_MESSAGE_NAK_ON_WRITE) == 1)
        API_SET_ERRNUM (ctx, IPMI_ERR_MESSAGE_TIMEOUT);
      else
        API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_send_cmd_rs);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_ipmb_msg_hdr_rq);
  fiid_obj_destroy (obj_ipmb_msg_rq);
  fiid_obj_destroy (obj_send_cmd_rs);
  return (rv);
}

static int
_api_kcs_ipmb_recv (ipmi_ctx_t ctx,
		    fiid_obj_t obj_ipmb_msg_hdr_rs,
		    fiid_obj_t obj_ipmb_msg_trlr,
		    fiid_obj_t obj_cmd_rs)
{
  struct ipmi_ctx_target target_save;
  uint8_t buf[IPMI_MAX_PKT_LEN];
  fiid_obj_t obj_ipmb_msg_rs = NULL;
  fiid_obj_t obj_get_cmd_rs = NULL;
  int len, ret, rv = -1;
  unsigned int intf_flags = IPMI_INTERFACE_FLAGS_DEFAULT;

  assert (ctx
          && ctx->magic == IPMI_CTX_MAGIC
          && ctx->type == IPMI_DEVICE_KCS
          && fiid_obj_valid (obj_ipmb_msg_hdr_rs)
          && fiid_obj_valid (obj_ipmb_msg_trlr)
          && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    intf_flags |= IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK;

  if (!(obj_ipmb_msg_rs = fiid_obj_create (tmpl_ipmb_msg)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_get_cmd_rs = fiid_obj_create (tmpl_cmd_get_message_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  /* get_message will send to the BMC, so clear out target information */
  memcpy (&target_save, &ctx->target, sizeof (target_save));
  ctx->target.channel_number_is_set = 0;
  ctx->target.rs_addr_is_set = 0;
  
  ret = ipmi_cmd_get_message (ctx, obj_get_cmd_rs);

  /* restore target info */
  memcpy (&ctx->target, &target_save, sizeof (target_save));

  if (ret < 0)
    {
      if (ipmi_check_completion_code (obj_get_cmd_rs, IPMI_COMP_CODE_GET_MESSAGE_DATA_NOT_AVAILABLE) == 1)
        API_SET_ERRNUM (ctx, IPMI_ERR_MESSAGE_TIMEOUT);
      else
        API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_get_cmd_rs);
      goto cleanup;
    }

  if ((len = fiid_obj_get_data (obj_get_cmd_rs,
                                "message_data",
                                buf,
                                IPMI_MAX_PKT_LEN)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_get_cmd_rs);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_ipmb_msg_rs,
                        buf,
                        len) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_ipmb_msg_rs);
      goto cleanup;
    }

  if (unassemble_ipmi_ipmb_msg (obj_ipmb_msg_rs,
                                obj_ipmb_msg_hdr_rs,
                                obj_cmd_rs,
                                obj_ipmb_msg_trlr,
				intf_flags) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_ipmb_msg_rs);
  fiid_obj_destroy (obj_get_cmd_rs);
  return (rv);
}

int
api_kcs_cmd_ipmb (ipmi_ctx_t ctx,
		  fiid_obj_t obj_cmd_rq,
		  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_ipmb_msg_hdr_rs = NULL;
  fiid_obj_t obj_ipmb_msg_trlr = NULL;
  unsigned retransmission_count = 0;
  unsigned reread_count = 0;
  int ret, rv = -1;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && ctx->type == IPMI_DEVICE_KCS
	  && fiid_obj_valid (obj_cmd_rq)
	  && fiid_obj_packet_valid (obj_cmd_rq) == 1
	  && fiid_obj_valid (obj_cmd_rs));

  if (!(obj_ipmb_msg_hdr_rs = fiid_obj_create (tmpl_ipmb_msg_hdr_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_ipmb_msg_trlr = fiid_obj_create (tmpl_ipmb_msg_trlr)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* for debugging */
  ctx->tmpl_ipmb_cmd_rq = fiid_obj_template (obj_cmd_rq);
  ctx->tmpl_ipmb_cmd_rs = fiid_obj_template (obj_cmd_rs);

  if (_api_kcs_ipmb_send (ctx, obj_cmd_rq) < 0)
    goto cleanup;

  while (1)
    {
      if (_api_kcs_ipmb_recv (ctx,
			      obj_ipmb_msg_hdr_rs,
			      obj_ipmb_msg_trlr,
			      obj_cmd_rs) < 0)
        {
          if (ctx->errnum == IPMI_ERR_MESSAGE_TIMEOUT)
            {
              reread_count++;

              if (reread_count > IPMI_KCS_IPMB_REREAD_COUNT)
                {
                  API_SET_ERRNUM (ctx, IPMI_ERR_MESSAGE_TIMEOUT);
                  goto cleanup;
                }

	      /* Wait a little bit to avoid spinning and killing the
	       * controller.  Observed on the Quanta QSSC-S4R/Appro
	       * GB812X-CN that when bridging FRU reads, this allowed
	       * records to be read more successfully.
	       */
	      usleep (IPMI_KCS_IPMB_REREAD_WAIT);
              continue;
            }
          goto cleanup;
        }

      if ((ret = ipmi_ipmb_check_rq_seq (obj_ipmb_msg_hdr_rs,
                                         ctx->io.inband.rq_seq)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      /* if it's the wrong rq_seq, get another packet */
      if (!ret)
        continue;

      if ((ret = ipmi_ipmb_check_checksum (IPMI_SLAVE_ADDRESS_BMC,
                                           obj_ipmb_msg_hdr_rs,
                                           obj_cmd_rs,
                                           obj_ipmb_msg_trlr)) < 0)
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }

      /* if the checksum is wrong, assume an error and resend */
      if (!ret)
        {
          retransmission_count++;

          if (retransmission_count > IPMI_KCS_IPMB_RETRANSMISSION_COUNT)
            {
              API_SET_ERRNUM (ctx, IPMI_ERR_MESSAGE_TIMEOUT);
              goto cleanup;
            }

          ctx->io.inband.rq_seq = ((ctx->io.inband.rq_seq) + 1) % (IPMI_IPMB_REQUESTER_SEQUENCE_NUMBER_MAX + 1);

          if (_api_kcs_ipmb_send (ctx, obj_cmd_rq) < 0)
            goto cleanup;

          continue;
        }

      break;
    }

  rv = 0;
 cleanup:
  ctx->io.inband.rq_seq = ((ctx->io.inband.rq_seq) + 1) % (IPMI_IPMB_REQUESTER_SEQUENCE_NUMBER_MAX + 1);
  fiid_obj_destroy (obj_ipmb_msg_hdr_rs);
  fiid_obj_destroy (obj_ipmb_msg_trlr);
  fiid_template_free (ctx->tmpl_ipmb_cmd_rq);
  ctx->tmpl_ipmb_cmd_rq = NULL;
  fiid_template_free (ctx->tmpl_ipmb_cmd_rs);
  ctx->tmpl_ipmb_cmd_rs = NULL;
  return (rv);
}

int
api_kcs_cmd_raw (ipmi_ctx_t ctx,
		 const void *buf_rq,
		 unsigned int buf_rq_len,
		 void *buf_rs,
		 unsigned int buf_rs_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int len, rv = -1;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && ctx->type == IPMI_DEVICE_KCS
	  && buf_rq
	  && buf_rq_len
	  && buf_rs
	  && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_kcs_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_kcs_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_cmd_rq,
                        buf_rq,
                        buf_rq_len) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }

  if (api_kcs_cmd (ctx,
		   obj_cmd_rq,
		   obj_cmd_rs) < 0)
    goto cleanup;
  
  if ((len = fiid_obj_get_all (obj_cmd_rs,
                               buf_rs,
                               buf_rs_len)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  
  rv = len;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
api_kcs_cmd_raw_ipmb (ipmi_ctx_t ctx,
		      const void *buf_rq,
		      unsigned int buf_rq_len,
		      void *buf_rs,
		      unsigned int buf_rs_len)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int len, rv = -1;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && ctx->type == IPMI_DEVICE_KCS
	  && buf_rq
	  && buf_rq_len
	  && buf_rs
	  && buf_rs_len);

  if (!(obj_cmd_rq = fiid_obj_create (tmpl_kcs_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_kcs_raw)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_cmd_rq,
                        buf_rq,
                        buf_rq_len) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }

  if (api_kcs_cmd_ipmb (ctx,
			obj_cmd_rq,
			obj_cmd_rs) < 0)
    goto cleanup;
  
  if ((len = fiid_obj_get_all (obj_cmd_rs,
                               buf_rs,
                               buf_rs_len)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  
  rv = len;
 cleanup:
  fiid_obj_destroy (obj_cmd_rq);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
