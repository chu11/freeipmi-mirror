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
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-network.h"
#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-md2.h"
#include "libcommon/ipmi-md5.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"
#include "secure.h"

#define IPMI_LAN_PKT_PAD_SIZE   1

fiid_template_t tmpl_lan_session_hdr =
  {
    { 8, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "ipmi_msg_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* IPMI LAN Message Request Header */
fiid_template_t tmpl_lan_msg_hdr_rq =
  {
    { 8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* IPMI LAN Message Response Header */
fiid_template_t tmpl_lan_msg_hdr_rs =
  {
    { 8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* IPMI LAN Message Trailer */
fiid_template_t tmpl_lan_msg_trlr =
  {
    { 8, "checksum2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

int
fill_lan_session_hdr (uint8_t authentication_type,
                      uint32_t session_sequence_number,
                      uint32_t session_id,
                      fiid_obj_t obj_lan_session_hdr)
{
  if (!IPMI_AUTHENTICATION_TYPE_VALID (authentication_type)
      || !fiid_obj_valid (obj_lan_session_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_session_hdr, tmpl_lan_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_lan_session_hdr);
  FILL_FIID_OBJ_SET (obj_lan_session_hdr, "authentication_type", authentication_type);
  FILL_FIID_OBJ_SET (obj_lan_session_hdr, "session_sequence_number", session_sequence_number);
  FILL_FIID_OBJ_SET (obj_lan_session_hdr, "session_id", session_id);

  /* authentication_code_data calculated in assemble_ipmi_lan_pkt */
  /* ipmi_msg_len calculated in assemble_ipmi_lan_pkt */

  return (0);
}

int
fill_lan_msg_hdr (uint8_t rs_addr,
                  uint8_t net_fn,
                  uint8_t rs_lun,
                  uint8_t rq_seq,
                  fiid_obj_t obj_lan_msg_hdr)
{
  uint8_t checksum_buf[1024];
  int checksum_len;
  uint8_t checksum;

  if (!IPMI_NET_FN_VALID (net_fn)
      || !IPMI_BMC_LUN_VALID (rs_lun)
      || (rq_seq > IPMI_LAN_REQUESTER_SEQUENCE_NUMBER_MAX)
      || !fiid_obj_valid (obj_lan_msg_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_lan_msg_hdr);
  FILL_FIID_OBJ_SET (obj_lan_msg_hdr, "rs_addr", rs_addr);
  FILL_FIID_OBJ_SET (obj_lan_msg_hdr, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_lan_msg_hdr, "rs_lun", rs_lun);

  if ((checksum_len = fiid_obj_get_block (obj_lan_msg_hdr,
                                          "rs_addr",
                                          "net_fn",
                                          checksum_buf,
                                          1024)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      return (-1);
    }

  checksum = ipmi_checksum (checksum_buf, checksum_len);
  FILL_FIID_OBJ_SET (obj_lan_msg_hdr, "checksum1", checksum);
  FILL_FIID_OBJ_SET (obj_lan_msg_hdr, "rq_addr", IPMI_LAN_SOFTWARE_ID_REMOTE_CONSOLE_SOFTWARE);
  FILL_FIID_OBJ_SET (obj_lan_msg_hdr, "rq_lun", IPMI_BMC_IPMB_LUN_BMC);
  FILL_FIID_OBJ_SET (obj_lan_msg_hdr, "rq_seq", rq_seq);

  return (0);
}

static int
_ipmi_lan_pkt_rq_min_size (uint8_t authentication_type, fiid_obj_t obj_cmd)
{
  unsigned int msg_len = 0;
  int len;

  assert (IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type) && fiid_obj_valid (obj_cmd));

  if ((len = fiid_template_len_bytes (tmpl_rmcp_hdr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  msg_len += len;

  if ((len = fiid_template_len_bytes (tmpl_lan_msg_hdr_rq)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  msg_len += len;

  if ((len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  msg_len += len;

  if ((len = fiid_template_block_len_bytes (tmpl_lan_session_hdr,
                                            "authentication_type",
                                            "session_id")) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  msg_len += len;

  if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2
      || authentication_type == IPMI_AUTHENTICATION_TYPE_MD5
      || authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY
      || authentication_type == IPMI_AUTHENTICATION_TYPE_OEM_PROP)
    msg_len += IPMI_1_5_MAX_PASSWORD_LENGTH;

  if ((len = fiid_template_field_len_bytes (tmpl_lan_session_hdr, "ipmi_msg_len")) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  msg_len += len;

  if ((len = fiid_obj_len_bytes (obj_cmd)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  msg_len += len;

  return (msg_len);
}

/*
  Complete IPMI LAN Request Packet
  +----------------------+
  |  RMCP                |
  |  Session             |
  |  Message             |
  |  Command             |
  |    Data              |
  |  Checksum            |
  +----------------------+
*/

int
assemble_ipmi_lan_pkt (fiid_obj_t obj_rmcp_hdr,
                       fiid_obj_t obj_lan_session_hdr,
                       fiid_obj_t obj_lan_msg_hdr,
                       fiid_obj_t obj_cmd,
                       const void *authentication_code_data,
                       unsigned int authentication_code_data_len,
                       void *pkt,
                       unsigned int pkt_len,
		       unsigned int flags)
{
  uint8_t authentication_type;
  uint64_t val;
  unsigned int indx = 0;
  int required_len;
  void *authentication_code_field_ptr = NULL;
  void *checksum_data_ptr = NULL;
  void *msg_data_ptr = NULL;
  void *ipmi_msg_len_ptr = NULL;
  unsigned int msg_data_count = 0;
  unsigned int checksum_data_count = 0;
  uint8_t ipmi_msg_len;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  uint8_t pwbuf[IPMI_1_5_MAX_PASSWORD_LENGTH];
  uint8_t checksum;
  int len, rv = -1;
  unsigned int flags_mask = 0;

  if (!fiid_obj_valid (obj_rmcp_hdr)
      || !fiid_obj_valid (obj_lan_session_hdr)
      || !fiid_obj_valid (obj_lan_msg_hdr)
      || !fiid_obj_valid (obj_cmd)
      || (authentication_code_data && authentication_code_data_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
      || !pkt
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcp_hdr, tmpl_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_session_hdr, tmpl_lan_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_PACKET_VALID (obj_rmcp_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }

  /*
   * ipmi_msg_len is calculated in this function, so we can't use
   * fiid_obj_packet_valid() on obj_lan_session_hdr b/c ipmi_msg_len
   * is probably not set yet.
   */

  if (FIID_OBJ_PACKET_VALID (obj_lan_msg_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      return (-1);
    }
  if (FIID_OBJ_PACKET_VALID (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_lan_session_hdr,
                    "authentication_type",
                    &val) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  authentication_type = val;

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE
      && authentication_type != IPMI_AUTHENTICATION_TYPE_MD2
      && authentication_type != IPMI_AUTHENTICATION_TYPE_MD5
      && authentication_type != IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* no need for overflow checks, handled w/ _ipmi_lan_pkt_rq_min_size check */

  required_len = _ipmi_lan_pkt_rq_min_size (authentication_type, obj_cmd);
  if (pkt_len < required_len)
    {
      SET_ERRNO (EMSGSIZE);
      return (-1);
    }

  memset (pkt, 0, pkt_len);

  if ((len = fiid_obj_get_all (obj_rmcp_hdr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      goto cleanup;
    }
  indx += len;

  if ((len = fiid_obj_get_block (obj_lan_session_hdr,
                                 "authentication_type",
                                 "session_id",
                                 pkt + indx,
                                 pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
      goto cleanup;
    }

  indx += len;

  /* authentication_code generated last.  Save pointers for later calculation */
  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      authentication_code_field_ptr = (pkt + indx);
      indx += IPMI_1_5_MAX_PASSWORD_LENGTH;
    }

  ipmi_msg_len_ptr = (pkt + indx);
  if ((len = fiid_template_field_len_bytes (tmpl_lan_session_hdr, "ipmi_msg_len")) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  if (len != 1)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }
  indx += len;

  msg_data_ptr = (pkt + indx);

  if ((len = fiid_obj_get_block (obj_lan_msg_hdr,
                                 "rs_addr",
                                 "checksum1",
                                 pkt + indx,
                                 pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      goto cleanup;
    }
  indx += len;
  msg_data_count += len;

  checksum_data_ptr = (pkt + indx);

  if ((len = fiid_obj_get_block (obj_lan_msg_hdr,
                                 "rq_addr",
                                 "rq_seq",
                                 pkt + indx,
                                 pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      goto cleanup;
    }
  indx += len;
  msg_data_count += len;
  checksum_data_count += len;

  if ((len = fiid_obj_get_all (obj_cmd, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }
  indx += len;
  msg_data_count += len;
  checksum_data_count += len;

  if (!(obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  checksum = ipmi_checksum (checksum_data_ptr, checksum_data_count);

  if (fiid_obj_set_all (obj_lan_msg_trlr, &checksum, sizeof (checksum)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
      goto cleanup;
    }

  if ((len = fiid_obj_get_all (obj_lan_msg_trlr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
      goto cleanup;
    }
  indx += len;
  msg_data_count += len;

  /* ipmi_msg_len done after message length is computed */
  ipmi_msg_len = msg_data_count;
  memcpy (ipmi_msg_len_ptr,
          &ipmi_msg_len,
          sizeof (ipmi_msg_len));

  /* Auth code must be done last, some authentication like md2 and md5
   * require all fields, including checksums, to be calculated
   * beforehand
   */
  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      int authentication_len;

      memset (pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);

      if ((authentication_len = fiid_obj_field_len_bytes (obj_lan_session_hdr,
                                                          "authentication_code")) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
          goto cleanup;
        }

      if (authentication_len)
        {
          if (fiid_obj_get_data (obj_lan_session_hdr,
                                 "authentication_code",
                                 pwbuf,
                                 IPMI_1_5_MAX_PASSWORD_LENGTH) < 0)
            {
              FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
              goto cleanup;
            }

          memcpy (authentication_code_field_ptr,
                  pwbuf,
                  IPMI_1_5_MAX_PASSWORD_LENGTH);
        }
      else
        {
          if (authentication_code_data)
            memcpy (pwbuf,
                    authentication_code_data,
                    authentication_code_data_len);

          if (authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
            {
              memcpy (authentication_code_field_ptr,
                      pwbuf,
                      IPMI_1_5_MAX_PASSWORD_LENGTH);
            }
          else /* IPMI_AUTHENTICATION_TYPE_MD2 || IPMI_AUTHENTICATION_TYPE_MD5 */
            {
              uint8_t session_id_buf[1024];
              uint8_t session_sequence_number_buf[1024];
              int session_id_len, session_sequence_number_len;

              if ((session_id_len = fiid_obj_get_data (obj_lan_session_hdr,
                                                       "session_id",
                                                       session_id_buf,
                                                       1024)) < 0)
                {
                  FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
                  goto cleanup;
                }

              if ((session_sequence_number_len = fiid_obj_get_data (obj_lan_session_hdr,
                                                                    "session_sequence_number",
                                                                    session_sequence_number_buf,
                                                                    1024)) < 0)
                {
                  FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
                  goto cleanup;
                }

              if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
                {
                  md2_t ctx;
                  uint8_t digest[MD2_DIGEST_LENGTH];

                  assert (IPMI_1_5_MAX_PASSWORD_LENGTH == MD2_DIGEST_LENGTH);

                  md2_init (&ctx);
                  md2_update_data (&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
                  md2_update_data (&ctx, session_id_buf, session_id_len);
                  md2_update_data (&ctx, msg_data_ptr, msg_data_count);
                  md2_update_data (&ctx, session_sequence_number_buf, session_sequence_number_len);
                  md2_update_data (&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
                  md2_finish (&ctx, digest, MD2_DIGEST_LENGTH);
                  md2_init (&ctx);

                  memcpy (authentication_code_field_ptr, digest, IPMI_1_5_MAX_PASSWORD_LENGTH);
                  secure_memset (digest, '\0', MD2_DIGEST_LENGTH);
                }
              else if (authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
                {
                  md5_t ctx;
                  uint8_t digest[MD5_DIGEST_LENGTH];

                  assert (IPMI_1_5_MAX_PASSWORD_LENGTH == MD5_DIGEST_LENGTH);

                  md5_init (&ctx);
                  md5_update_data (&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
                  md5_update_data (&ctx, session_id_buf, session_id_len);
                  md5_update_data (&ctx, msg_data_ptr, msg_data_count);
                  md5_update_data (&ctx, session_sequence_number_buf, session_sequence_number_len);
                  md5_update_data (&ctx, pwbuf, IPMI_1_5_MAX_PASSWORD_LENGTH);
                  md5_finish (&ctx, digest, MD5_DIGEST_LENGTH);
                  md5_init (&ctx);

                  memcpy (authentication_code_field_ptr, digest, IPMI_1_5_MAX_PASSWORD_LENGTH);
                  secure_memset (digest, '\0', MD5_DIGEST_LENGTH);
                }
            }
        }
    }

  if (indx > INT_MAX)
    {
      SET_ERRNO (EMSGSIZE);
      goto cleanup;
    }

  rv = indx;
 cleanup:
  if (rv < 0)
    secure_memset (pkt, '\0', pkt_len);
  fiid_obj_destroy (obj_lan_msg_trlr);
  secure_memset (pwbuf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
  return (rv);
}

/*
  Complete IPMI LAN Response Packet
  +----------------------+
  |  Session             |
  |  RMCP                |
  |  Message             |
  |  Command             |
  |    Completion Code   |
  |    Data              |
  |  Checksum            |
  +----------------------+
  Optional Arguments : (pass NULL to ignore)
  rmcp_hdr, session, msg, cmd and checksum
*/

int
unassemble_ipmi_lan_pkt (const void *pkt,
                         unsigned int pkt_len,
                         fiid_obj_t obj_rmcp_hdr,
                         fiid_obj_t obj_lan_session_hdr,
                         fiid_obj_t obj_lan_msg_hdr,
                         fiid_obj_t obj_cmd,
                         fiid_obj_t obj_lan_msg_trlr,
			 unsigned int flags)
{
  uint8_t authentication_type;
  unsigned int indx = 0;
  unsigned int obj_cmd_len;
  int obj_lan_msg_trlr_len, len;
  uint64_t val;
  unsigned int flags_mask = (IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK);

  if (!pkt
      || !fiid_obj_valid (obj_rmcp_hdr)
      || !fiid_obj_valid (obj_lan_session_hdr)
      || !fiid_obj_valid (obj_lan_msg_hdr)
      || !fiid_obj_valid (obj_cmd)
      || !fiid_obj_valid (obj_lan_msg_trlr)
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcp_hdr, tmpl_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_session_hdr, tmpl_lan_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_msg_trlr, tmpl_lan_msg_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  indx = 0;
  if (fiid_obj_clear (obj_rmcp_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }

  if (fiid_obj_clear (obj_lan_session_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
      return (-1);
    }

  if (fiid_obj_clear (obj_lan_msg_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      return (-1);
    }
  
  if (fiid_obj_clear (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (fiid_obj_clear (obj_lan_msg_trlr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
      return (-1);
    }

  if ((len = fiid_obj_set_all (obj_rmcp_hdr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  indx += len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((len = fiid_obj_set_block (obj_lan_session_hdr,
                                 "authentication_type",
                                 "session_id",
                                 pkt + indx,
                                 pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
      return (-1);
    }
  indx += len;

  if (FIID_OBJ_GET (obj_lan_session_hdr,
                    "authentication_type",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
      return (-1);
    }
  authentication_type = val;

  if (!IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type))
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      if ((len = fiid_obj_set_data (obj_lan_session_hdr,
                                    "authentication_code",
                                    pkt + indx,
                                    pkt_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
          return (-1);
        }
      indx += len;

      if (pkt_len <= indx)
        {
          /* cannot parse packet */
          ERR_TRACE ("malformed packet", EINVAL);
          return (0);
        }
    }

  if ((len = fiid_obj_set_data (obj_lan_session_hdr,
                                "ipmi_msg_len",
                                pkt + indx,
                                pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_session_hdr);
      return (-1);
    }
  indx += len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((len = fiid_obj_set_all (obj_lan_msg_hdr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
      return (-1);
    }
  indx += len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if ((pkt_len - indx) <= obj_lan_msg_trlr_len)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  obj_cmd_len = (pkt_len - indx) - obj_lan_msg_trlr_len;

  if ((len = fiid_obj_set_all (obj_cmd, pkt + indx, obj_cmd_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  indx += len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((len = fiid_obj_set_all (obj_lan_msg_trlr, pkt + indx, pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
      return (-1);
    }
  indx += len;
  
  if (FIID_OBJ_PACKET_VALID (obj_rmcp_hdr) == 1
      && FIID_OBJ_PACKET_VALID (obj_lan_session_hdr) == 1
      && FIID_OBJ_PACKET_VALID (obj_lan_msg_hdr) == 1
      && ((flags & IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK) || FIID_OBJ_PACKET_SUFFICIENT (obj_cmd) == 1)
      && FIID_OBJ_PACKET_VALID (obj_lan_msg_trlr) == 1)
    return (1);

  return (0);
}

ssize_t
ipmi_lan_sendto (int s,
                 const void *buf,
                 size_t len,
                 int flags,
                 const struct sockaddr *to,
                 socklen_t tolen)
{
  /* achu: Specification table 13-8, indicates legacy padding for IPMI 1.5, but 
   * it appears it is specific to Ethernet 10/100 lan chips.
   *
   * It is so legacy at this point it is probably not worth providing.
   * In addition, it is difficult to calculate if a pad is necessary
   * b/c it appears the pad should be for the ethernet frame, not the
   * IP packet.
   *
   * Why not just remove this function?  Leave just in case a legacy
   * pad situation pops up and we gotta implement something.
   */ 
  return (ipmi_network_sendto (s, buf, len, flags, to, tolen));
}

ssize_t
ipmi_lan_recvfrom (int s,
                   void *buf,
                   size_t len,
                   int flags,
                   struct sockaddr *from,
                   socklen_t *fromlen)
{
  return (ipmi_network_recvfrom (s, buf, len, flags, from, fromlen));
}

