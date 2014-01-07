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
#include <assert.h>
#include <errno.h>

#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/cmds/ipmi-sol-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/payload/ipmi-sol-payload.h"
#include "freeipmi/util/ipmi-rmcpplus-util.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-crypt.h"
#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/* return data parsed on success, 0 if can't parse anymore, -1 on error */
static int
_dump_rmcpplus_session_hdr (int fd,
                            const char *prefix,
                            const char *session_hdr,
                            const void *pkt,
                            unsigned int pkt_len,
                            uint8_t *payload_type,
                            uint8_t *payload_authenticated,
                            uint8_t *payload_encrypted,
                            uint32_t *session_id,
                            uint16_t *ipmi_payload_len)
{
  fiid_obj_t obj_rmcpplus_session_hdr = NULL;
  unsigned int indx = 0;
  int obj_len, rv = -1;
  uint64_t val;

  assert (pkt
          && pkt_len
          && payload_type
          && payload_authenticated
          && payload_encrypted
          && session_id
          && ipmi_payload_len);

  /*
   * Extract auth_type and payload information
   */
  if (!(obj_rmcpplus_session_hdr = fiid_obj_create (tmpl_rmcpplus_session_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((obj_len = fiid_obj_set_block (obj_rmcpplus_session_hdr,
                                     "authentication_type",
                                     "payload_type.encrypted",
                                     pkt + indx,
                                     pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  indx += obj_len;

  if (pkt_len <= indx)
    goto output;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  (*payload_type) = val;

  /*
   * Extract OEM IANA and OEM Payload ID
   */
  if (*payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      if ((obj_len = fiid_obj_set_block (obj_rmcpplus_session_hdr,
                                         "oem_iana",
                                         "oem_payload_id",
                                         pkt + indx,
                                         pkt_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
          goto cleanup;
        }
      indx += obj_len;

      if (pkt_len <= indx)
        goto output;
    }

  /*
   * Extract Session ID, Session Sequence Number, and Payload Length
   */
  if ((obj_len = fiid_obj_set_block (obj_rmcpplus_session_hdr,
                                     "session_id",
                                     "ipmi_payload_len",
                                     pkt + indx,
                                     pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  indx += obj_len;

  if (pkt_len <= indx)
    goto output;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type.authenticated",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  (*payload_authenticated) = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type.encrypted",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  (*payload_encrypted) = val;
  
  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "session_id",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  (*session_id) = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "ipmi_payload_len",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      goto cleanup;
    }
  (*ipmi_payload_len) = val;

 output:
  if (ipmi_obj_dump (fd,
                     prefix,
                     session_hdr,
                     NULL,
                     obj_rmcpplus_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = indx;
 cleanup:
  fiid_obj_destroy (obj_rmcpplus_session_hdr);
  return (rv);
}

/* return 1 on parse success, 0 if can't parse anymore, -1 on error */
static int
_dump_rmcpplus_payload_data (int fd,
                             const char *prefix,
                             const char *msg_hdr,
                             const char *cmd_hdr,
                             const char *ipmb_msg_hdr,
                             const char *ipmb_cmd_hdr,
                             const char *ipmb_msg_trlr_hdr,
                             const char *trailer_hdr,
                             uint8_t payload_type,
                             fiid_template_t tmpl_lan_msg_hdr,
                             fiid_template_t tmpl_cmd,
                             fiid_template_t tmpl_ipmb_msg_hdr,
                             fiid_template_t tmpl_ipmb_cmd,
                             const void *pkt,
                             uint16_t ipmi_payload_len)
{
  char *payload_unexpected_hdr =
    "Payload Unexpected Data:\n"
    "------------------------";
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  fiid_obj_t obj_payload_unexpected_data = NULL;
  int obj_lan_msg_trlr_len, len, rv = -1;
  unsigned int obj_cmd_len;
  unsigned int indx = 0;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare (tmpl_lan_msg_hdr,
                                               tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare (tmpl_lan_msg_hdr,
                                                  tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare (tmpl_cmd,
                                           tmpl_sol_payload_data) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt
          && ipmi_payload_len);

  /* Dump message header */

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      uint8_t ipmb_buf[IPMI_DEBUG_MAX_PKT_LEN];
      int ipmb_buf_len = 0;

      if (!(obj_lan_msg_hdr = fiid_obj_create (tmpl_lan_msg_hdr)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if ((len = fiid_obj_set_all (obj_lan_msg_hdr,
                                   pkt + indx,
                                   ipmi_payload_len - indx)) < 0)
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

      if (ipmi_payload_len <= indx)
        {
          rv = 0;
          goto cleanup;
        }

      if ((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if ((ipmi_payload_len - indx) <= obj_lan_msg_trlr_len)
        goto dump_payload_extra;
      
      obj_cmd_len = (ipmi_payload_len - indx) - obj_lan_msg_trlr_len;
      
      if (!(obj_cmd = fiid_obj_create (tmpl_cmd)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
      
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

      if (ipmi_payload_len <= indx)
        {
          rv = 0;
          goto cleanup;
        }
          
      /* Dump trailer */
      
      if (!(obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
          
      if ((len = fiid_obj_set_all (obj_lan_msg_trlr,
                                   pkt + indx,
                                   ipmi_payload_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
          goto cleanup;
        }
      indx += len;
      
      if (ipmi_obj_dump (fd,
                         prefix,
                         trailer_hdr,
                         NULL,
                         obj_lan_msg_trlr) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else /* payload_type == IPMI_PAYLOAD_TYPE_SOL */
    {
      obj_cmd_len = ipmi_payload_len;

      if (!(obj_cmd = fiid_obj_create (tmpl_cmd)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (fiid_obj_clear (obj_cmd) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          goto cleanup;
        }

      if ((len = fiid_obj_set_all (obj_cmd,
                                   pkt + indx,
                                   obj_cmd_len)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
          goto cleanup;
        }
      indx += len;

      if (ipmi_obj_dump (fd,
                         prefix,
                         cmd_hdr,
                         NULL,
                         obj_cmd) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }

  /* Dump payload unexpected stuff */

 dump_payload_extra:

  if ((ipmi_payload_len - indx) > 0)
    {
      if (!(obj_payload_unexpected_data = fiid_obj_create (tmpl_unexpected_data)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if ((len = fiid_obj_set_all (obj_payload_unexpected_data,
                                   pkt + indx,
                                   ipmi_payload_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_payload_unexpected_data);
          goto cleanup;
        }
      indx += len;

      if (ipmi_obj_dump (fd,
                         prefix,
                         payload_unexpected_hdr,
                         NULL,
                         obj_payload_unexpected_data) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_lan_msg_hdr);
  fiid_obj_destroy (obj_cmd);
  fiid_obj_destroy (obj_lan_msg_trlr);
  fiid_obj_destroy (obj_payload_unexpected_data);
  return (rv);
}

/* return 1 on parse success, 0 if can't parse anymore, -1 on error */
static int
_dump_rmcpplus_payload_confidentiality_none (int fd,
                                             const char *prefix,
                                             const char *payload_hdr,
                                             const char *msg_hdr,
                                             const char *cmd_hdr,
                                             const char *ipmb_msg_hdr,
                                             const char *ipmb_cmd_hdr,
                                             const char *ipmb_msg_trlr_hdr,
                                             const char *trailer_hdr,
                                             uint8_t payload_type,
                                             fiid_template_t tmpl_lan_msg_hdr,
                                             fiid_template_t tmpl_cmd,
                                             fiid_template_t tmpl_ipmb_msg_hdr,
                                             fiid_template_t tmpl_ipmb_cmd,
                                             const void *pkt,
                                             uint16_t ipmi_payload_len)
{
  fiid_obj_t obj_rmcpplus_payload = NULL;
  int ret, rv = -1;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare (tmpl_lan_msg_hdr,
                                               tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare (tmpl_lan_msg_hdr,
                                                  tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare (tmpl_cmd,
                                           tmpl_sol_payload_data) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_bmc_to_remote_console) == 1))

          && pkt
          && ipmi_payload_len);

  if (!(obj_rmcpplus_payload = fiid_obj_create (tmpl_rmcpplus_payload)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         pkt,
                         ipmi_payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      goto cleanup;
    }

  if (ipmi_obj_dump (fd,
                     prefix,
                     payload_hdr,
                     NULL,
                     obj_rmcpplus_payload) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((ret = _dump_rmcpplus_payload_data (fd,
                                          prefix,
                                          msg_hdr,
                                          cmd_hdr,
                                          ipmb_msg_hdr,
                                          ipmb_cmd_hdr,
                                          ipmb_msg_trlr_hdr,
                                          trailer_hdr,
                                          payload_type,
                                          tmpl_lan_msg_hdr,
                                          tmpl_cmd,
                                          tmpl_ipmb_msg_hdr,
                                          tmpl_ipmb_cmd,
                                          pkt,
                                          ipmi_payload_len)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = ret;
 cleanup:
  fiid_obj_destroy (obj_rmcpplus_payload);
  return (rv);
}

/* return 1 on parse success, 0 if can't parse anymore, -1 on error */
static int
_dump_rmcpplus_payload_confidentiality_aes_cbc_128 (int fd,
                                                    const char *prefix,
                                                    const char *payload_hdr,
                                                    const char *msg_hdr,
                                                    const char *cmd_hdr,
                                                    const char *ipmb_msg_hdr,
                                                    const char *ipmb_cmd_hdr,
                                                    const char *ipmb_msg_trlr_hdr,
                                                    const char *trailer_hdr,
                                                    uint8_t payload_type,
                                                    fiid_template_t tmpl_lan_msg_hdr,
                                                    fiid_template_t tmpl_cmd,
                                                    fiid_template_t tmpl_ipmb_msg_hdr,
                                                    fiid_template_t tmpl_ipmb_cmd,
                                                    const void *confidentiality_key,
                                                    unsigned int confidentiality_key_len,
                                                    const void *pkt,
                                                    uint16_t ipmi_payload_len)
{
  uint8_t iv[IPMI_CRYPT_AES_CBC_128_IV_LENGTH];
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_len;
  int cipher_keylen, cipher_blocklen, decrypt_len, ret, rv = -1;
  unsigned int payload_data_len, cmd_data_len;
  fiid_obj_t obj_rmcpplus_payload = NULL;
  unsigned int indx = 0;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare (tmpl_lan_msg_hdr,
                                               tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare (tmpl_lan_msg_hdr,
                                                  tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare (tmpl_cmd,
                                           tmpl_sol_payload_data) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && confidentiality_key
          && confidentiality_key_len
          && pkt
          && ipmi_payload_len);

  if ((cipher_keylen = crypt_cipher_key_len (IPMI_CRYPT_CIPHER_AES)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  assert (!(cipher_keylen < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));

  if (confidentiality_key_len < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }
  confidentiality_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;

  if ((cipher_blocklen = crypt_cipher_block_len (IPMI_CRYPT_CIPHER_AES)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  assert (cipher_blocklen == IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);

  if (ipmi_payload_len < IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH)
    {
      rv = 0;
      goto cleanup;
    }
  
  payload_data_len = ipmi_payload_len - IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;

  if (!payload_data_len)
    {
      rv = 0;
      goto cleanup;
    }

  memcpy (iv, pkt, IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
  indx += IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;
  memcpy (payload_buf, pkt + indx, payload_data_len);

  if (!(obj_rmcpplus_payload = fiid_obj_create (tmpl_rmcpplus_payload)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "confidentiality_header",
                         iv,
                         IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      goto cleanup;
    }

  if ((decrypt_len = crypt_cipher_decrypt (IPMI_CRYPT_CIPHER_AES,
					   IPMI_CRYPT_CIPHER_MODE_CBC,
					   confidentiality_key,
					   confidentiality_key_len,
					   iv,
					   IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH,
					   payload_buf,
					   payload_data_len)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (decrypt_len != payload_data_len)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  pad_len = payload_buf[payload_data_len - 1];
  if (pad_len > IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH)
    {
      rv = 0;
      goto cleanup;
    }

  if ((pad_len + 1) > payload_data_len)
    {
      rv = 0;
      goto cleanup;
    }

  cmd_data_len = payload_data_len - pad_len - 1;

  if (cmd_data_len <= 0)
    {
      rv = 0;
      goto cleanup;
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         payload_buf,
                         cmd_data_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      goto cleanup;
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "confidentiality_trailer",
                         payload_buf + cmd_data_len,
                         pad_len + 1) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      goto cleanup;
    }

  if (ipmi_obj_dump (fd,
                     prefix,
                     payload_hdr,
                     NULL,
                     obj_rmcpplus_payload) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((ret = _dump_rmcpplus_payload_data (fd,
                                          prefix,
                                          msg_hdr,
                                          cmd_hdr,
                                          ipmb_msg_hdr,
                                          ipmb_cmd_hdr,
                                          ipmb_msg_trlr_hdr,
                                          trailer_hdr,
                                          payload_type,
                                          tmpl_lan_msg_hdr,
                                          tmpl_cmd,
                                          tmpl_ipmb_msg_hdr,
                                          tmpl_ipmb_cmd,
                                          payload_buf,
                                          cmd_data_len)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  rv = ret;
 cleanup:
  fiid_obj_destroy (obj_rmcpplus_payload);
  return (rv);
}

/* return 1 on parse success, 0 if can't parse anymore, -1 on error */
static int
_dump_rmcpplus_payload_rakp (int fd,
                             const char *prefix,
                             const char *payload_hdr,
                             const char *cmd_hdr,
                             uint8_t payload_type,
                             fiid_template_t tmpl_cmd,
                             const void *pkt,
                             uint16_t ipmi_payload_len)
{
  fiid_obj_t obj_rmcpplus_payload = NULL;
  fiid_obj_t obj_cmd = NULL;
  int rv = -1;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
               && (fiid_template_compare (tmpl_cmd,
                                          tmpl_rmcpplus_open_session_request) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
               && (fiid_template_compare (tmpl_cmd,
                                          tmpl_rmcpplus_open_session_response) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
               && (fiid_template_compare (tmpl_cmd,
                                          tmpl_rmcpplus_rakp_message_1) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
               && (fiid_template_compare (tmpl_cmd,
                                          tmpl_rmcpplus_rakp_message_2) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
               && (fiid_template_compare (tmpl_cmd,
                                          tmpl_rmcpplus_rakp_message_3) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4
               && (fiid_template_compare (tmpl_cmd,
                                          tmpl_rmcpplus_rakp_message_4) != 1))
          && pkt
          && ipmi_payload_len);

  if (!(obj_rmcpplus_payload = fiid_obj_create (tmpl_rmcpplus_payload)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         pkt,
                         ipmi_payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      goto cleanup;
    }

  if (ipmi_obj_dump (fd,
                     prefix,
                     payload_hdr,
                     NULL,
                     obj_rmcpplus_payload) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    {
      if (!(obj_cmd = fiid_obj_create (tmpl_rmcpplus_open_session_request)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
    {
      if (!(obj_cmd = fiid_obj_create (tmpl_rmcpplus_open_session_response)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    {
      if (!(obj_cmd = fiid_obj_create (tmpl_rmcpplus_rakp_message_1)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    {
      if (!(obj_cmd = fiid_obj_create (tmpl_rmcpplus_rakp_message_2)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      if (!(obj_cmd = fiid_obj_create (tmpl_rmcpplus_rakp_message_3)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  else /* IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4 */
    {
      if (!(obj_cmd = fiid_obj_create (tmpl_rmcpplus_rakp_message_4)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }

  if (fiid_obj_set_all (obj_cmd,
                        pkt,
                        ipmi_payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
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

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_rmcpplus_payload);
  fiid_obj_destroy (obj_cmd);
  return (rv);
}

/* return 1 on parse success, 0 if can't parse anymore, -1 on error */
static int
_dump_rmcpplus_payload (int fd,
                        const char *prefix,
                        const char *payload_hdr,
                        const char *msg_hdr,
                        const char *cmd_hdr,
                        const char *ipmb_msg_hdr,
                        const char *ipmb_cmd_hdr,
                        const char *ipmb_msg_trlr_hdr,
                        const char *trailer_hdr,
                        uint8_t payload_type,
                        uint8_t authentication_algorithm,
                        uint8_t confidentiality_algorithm,
                        fiid_template_t tmpl_lan_msg_hdr,
                        fiid_template_t tmpl_cmd,
                        fiid_template_t tmpl_ipmb_msg_hdr,
                        fiid_template_t tmpl_ipmb_cmd,
                        const void *confidentiality_key,
                        unsigned int confidentiality_key_len,
                        const void *pkt,
                        uint16_t ipmi_payload_len)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
          && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
               && !(confidentiality_key
                    && confidentiality_key_len))
          && ipmi_payload_len
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare (tmpl_lan_msg_hdr,
                                               tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare (tmpl_lan_msg_hdr,
                                                  tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare (tmpl_cmd,
                                           tmpl_sol_payload_data) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare (tmpl_cmd,
                                              tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt
          && ipmi_payload_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI
      || payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return (_dump_rmcpplus_payload_confidentiality_none (fd,
                                                             prefix,
                                                             payload_hdr,
                                                             msg_hdr,
                                                             cmd_hdr,
                                                             ipmb_msg_hdr,
                                                             ipmb_cmd_hdr,
                                                             ipmb_msg_trlr_hdr,
                                                             trailer_hdr,
                                                             payload_type,
                                                             tmpl_lan_msg_hdr,
                                                             tmpl_cmd,
                                                             tmpl_ipmb_msg_hdr,
                                                             tmpl_ipmb_cmd,
                                                             pkt,
                                                             ipmi_payload_len));
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return (_dump_rmcpplus_payload_confidentiality_aes_cbc_128 (fd,
                                                                    prefix,
                                                                    payload_hdr,
                                                                    msg_hdr,
                                                                    cmd_hdr,
                                                                    ipmb_msg_hdr,
                                                                    ipmb_cmd_hdr,
                                                                    ipmb_msg_trlr_hdr,
                                                                    trailer_hdr,
                                                                    payload_type,
                                                                    tmpl_lan_msg_hdr,
                                                                    tmpl_cmd,
                                                                    tmpl_ipmb_msg_hdr,
                                                                    tmpl_ipmb_cmd,
                                                                    confidentiality_key,
                                                                    confidentiality_key_len,
                                                                    pkt,
                                                                    ipmi_payload_len));
    }
  else
    return (_dump_rmcpplus_payload_rakp (fd,
                                         prefix,
                                         payload_hdr,
                                         cmd_hdr,
                                         payload_type,
                                         tmpl_cmd,
                                         pkt,
                                         ipmi_payload_len));
}

/* return data parsed on success, 0 if can't parse anymore, -1 on error */
static int
_dump_rmcpplus_session_trlr (int fd,
                             const char *prefix,
                             const char *session_trailer_hdr,
                             uint32_t session_id,
                             uint8_t payload_authenticated,
                             uint8_t integrity_algorithm,
                             const void *pkt,
                             unsigned int pkt_len)
{
  int pad_length_field_len, next_header_field_len, rv = -1;
  unsigned int pad_length, authentication_code_len = 0;
  fiid_obj_t obj_rmcpplus_session_trlr = NULL;
  unsigned int indx = 0;

  assert (IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm));

  if (!session_id || payload_authenticated == IPMI_PAYLOAD_FLAG_UNAUTHENTICATED)
    return (1);

  /* payload should be authenticated */
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    authentication_code_len = 0;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128)
    authentication_code_len = IPMI_HMAC_SHA256_128_AUTHENTICATION_CODE_LENGTH;
  else
    authentication_code_len = 0; /* just in case IPMI implementation is bogus */

  if (!(obj_rmcpplus_session_trlr = fiid_obj_create (tmpl_rmcpplus_session_trlr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr,
                                                             "pad_length")) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr,
                                                              "next_header")) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (pkt_len < (pad_length_field_len))
    {
      next_header_field_len = 0;
      authentication_code_len = 0;
    }
  else if (pkt_len < (pad_length_field_len + next_header_field_len))
    authentication_code_len = 0;
  else if (authentication_code_len
           && pkt_len < (authentication_code_len + pad_length_field_len + next_header_field_len))
    authentication_code_len = pkt_len - pad_length_field_len - next_header_field_len;

  pad_length = pkt_len - pad_length_field_len - next_header_field_len - authentication_code_len;

  if (pad_length)
    {
      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "integrity_pad",
                             pkt + indx,
                             pad_length) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          goto cleanup;
        }
      indx += pad_length;
    }

  if (pad_length_field_len)
    {
      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "pad_length",
                             pkt + indx,
                             pad_length_field_len) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          goto cleanup;
        }
      indx += pad_length_field_len;
    }

  if (next_header_field_len)
    {
      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "next_header",
                             pkt + indx,
                             next_header_field_len) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          goto cleanup;
        }
      indx += next_header_field_len;
    }

  if (authentication_code_len)
    {
      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "authentication_code",
                             pkt + indx,
                             authentication_code_len) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          goto cleanup;
        }
      indx += authentication_code_len;
    }


  if (ipmi_obj_dump (fd,
                     prefix,
                     session_trailer_hdr,
                     NULL,
                     obj_rmcpplus_session_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* Clear out data */
  if (fiid_obj_clear (obj_rmcpplus_session_trlr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
      goto cleanup;
    }

  rv = indx;
 cleanup:
  fiid_obj_destroy (obj_rmcpplus_session_trlr);
  return (rv);
}

static int
_ipmi_dump_rmcpplus_packet (int fd,
                            const char *prefix,
                            const char *hdr,
                            const char *trlr,
                            uint8_t authentication_algorithm,
                            uint8_t integrity_algorithm,
                            uint8_t confidentiality_algorithm,
                            const void *integrity_key,
                            unsigned int integrity_key_len,
                            const void *confidentiality_key,
                            unsigned int confidentiality_key_len,
                            const void *pkt,
                            unsigned int pkt_len,
                            fiid_template_t tmpl_lan_msg_hdr,
                            fiid_template_t tmpl_cmd,
                            fiid_template_t tmpl_ipmb_msg_hdr,
                            fiid_template_t tmpl_ipmb_cmd)
{
  int obj_rmcp_hdr_len, obj_len, ret, rv = -1;
  uint8_t payload_type = 0, payload_authenticated = 0, payload_encrypted = 0;
  uint32_t session_id = 0;
  uint16_t ipmi_payload_len = 0;
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_unexpected_data = NULL;
  char *rmcp_hdr =
    "RMCP Header:\n"
    "------------";
  char *session_hdr =
    "IPMI RMCPPLUS Session Header:\n"
    "-----------------------------";
  char *payload_hdr =
    "IPMI RMCPPLUS Payload:\n"
    "----------------------";
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
  char *trailer_hdr =
    "IPMI Trailer:\n"
    "-------------";
  char *session_trailer_hdr =
    "IPMI RMCPPLUS Session Trailer:\n"
    "------------------------------";
  char *extra_hdr =
    "Unexpected Data:\n"
    "----------------";
  unsigned int indx = 0;

  assert (pkt);
  assert (IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm));
  assert (IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm));
  assert (IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm));
  assert (!(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
            && !(confidentiality_key
                 && confidentiality_key_len)));
  assert (tmpl_cmd);

  if (debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (debug_output_str (fd, prefix_buf, hdr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* Dump rmcp header */

  if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((obj_rmcp_hdr_len = fiid_obj_set_all (obj_rmcp_hdr,
                                            pkt + indx,
                                            pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      goto cleanup;
    }
  indx += obj_rmcp_hdr_len;

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

  /* Dump rmcpplus session header */

  if ((obj_len = _dump_rmcpplus_session_hdr (fd,
                                             prefix,
                                             session_hdr,
                                             pkt + indx,
                                             pkt_len - indx,
                                             &payload_type,
                                             &payload_authenticated,
                                             &payload_encrypted,
                                             &session_id,
                                             &ipmi_payload_len)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* don't know how to parse, dump as just big blob of hex */
  if (!obj_len)
    goto dump_extra;

  indx += obj_len;

  if (pkt_len <= indx)
    goto out;

  /* achu: If the packet is really messed up, dump the packet in raw form */
  if ((payload_type != IPMI_PAYLOAD_TYPE_IPMI
       && payload_type != IPMI_PAYLOAD_TYPE_SOL
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
      || !IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
      || !ipmi_payload_len)
    goto dump_extra;

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (!tmpl_lan_msg_hdr
          || (fiid_template_compare (tmpl_lan_msg_hdr,
                                     tmpl_lan_msg_hdr_rq) != 1
              && fiid_template_compare (tmpl_lan_msg_hdr,
                                        tmpl_lan_msg_hdr_rs) != 1))
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      if (fiid_template_compare (tmpl_cmd,
                                 tmpl_sol_payload_data) != 1
          && fiid_template_compare (tmpl_cmd,
                                    tmpl_sol_payload_data_remote_console_to_bmc) != 1
          && fiid_template_compare (tmpl_cmd,
                                    tmpl_sol_payload_data_bmc_to_remote_console) != 1)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    {
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_open_session_request) != 1)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
    {
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_open_session_response) != 1)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    {
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_1) != 1)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    {
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_2) != 1)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_3) != 1)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      if (fiid_template_compare (tmpl_cmd, tmpl_rmcpplus_rakp_message_4) != 1)
        {
          SET_ERRNO (EINVAL);
          goto cleanup;
        }
    }

  /* Dump Payload */

  if ((ret = _dump_rmcpplus_payload (fd,
                                     prefix,
                                     payload_hdr,
                                     msg_hdr,
                                     cmd_hdr,
                                     ipmb_msg_hdr,
                                     ipmb_cmd_hdr,
                                     ipmb_msg_trlr_hdr,
                                     trailer_hdr,
                                     payload_type,
                                     authentication_algorithm,
                                     confidentiality_algorithm,
                                     tmpl_lan_msg_hdr,
                                     tmpl_cmd,
                                     tmpl_ipmb_msg_hdr,
                                     tmpl_ipmb_cmd,
                                     confidentiality_key,
                                     confidentiality_key_len,
                                     pkt + indx,
                                     ipmi_payload_len)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* don't know how to parse, dump as just big blob of hex */
  if (!ret)
    goto dump_extra;

  indx += ipmi_payload_len;

  if (pkt_len <= indx)
    goto out;

  /* Dump trailer */

  if ((obj_len = _dump_rmcpplus_session_trlr (fd,
                                              prefix,
                                              session_trailer_hdr,
                                              session_id,
                                              payload_authenticated,
                                              integrity_algorithm,
                                              pkt + indx,
                                              pkt_len - indx)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* don't know how to parse, dump as just big blob of hex */
  if (!obj_len)
    goto dump_extra;

  indx += obj_len;

  /* Dump extra stuff if packet is longer than expected */
 dump_extra:
  if ((pkt_len - indx) > 0)
    {
      if (!(obj_unexpected_data = fiid_obj_create (tmpl_unexpected_data)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if ((obj_len = fiid_obj_set_all (obj_unexpected_data,
                                       pkt + indx,
                                       pkt_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_unexpected_data);
          goto cleanup;
        }
      indx += obj_len;

      if (ipmi_obj_dump (fd,
                         prefix,
                         extra_hdr,
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
  fiid_obj_destroy (obj_unexpected_data);
  return (rv);
}

int
ipmi_dump_rmcpplus_packet (int fd,
                           const char *prefix,
                           const char *hdr,
                           const char *trlr,
                           uint8_t authentication_algorithm,
                           uint8_t integrity_algorithm,
                           uint8_t confidentiality_algorithm,
                           const void *integrity_key,
                           unsigned int integrity_key_len,
                           const void *confidentiality_key,
                           unsigned int confidentiality_key_len,
                           const void *pkt,
                           unsigned int pkt_len,
                           fiid_template_t tmpl_lan_msg_hdr,
                           fiid_template_t tmpl_cmd)
{
  if (!pkt
      || !IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
          && (!confidentiality_key
              || !confidentiality_key_len))
      || !tmpl_cmd)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  return (_ipmi_dump_rmcpplus_packet (fd,
                                      prefix,
                                      hdr,
                                      trlr,
                                      authentication_algorithm,
                                      integrity_algorithm,
                                      confidentiality_algorithm,
                                      integrity_key,
                                      integrity_key_len,
                                      confidentiality_key,
                                      confidentiality_key_len,
                                      pkt,
                                      pkt_len,
                                      tmpl_lan_msg_hdr,
                                      tmpl_cmd,
                                      NULL,
                                      NULL));
}

int
ipmi_dump_rmcpplus_packet_ipmb (int fd,
                                const char *prefix,
                                const char *hdr,
                                const char *trlr,
                                uint8_t authentication_algorithm,
                                uint8_t integrity_algorithm,
                                uint8_t confidentiality_algorithm,
                                const void *integrity_key,
                                unsigned int integrity_key_len,
                                const void *confidentiality_key,
                                unsigned int confidentiality_key_len,
                                const void *pkt,
                                unsigned int pkt_len,
                                fiid_template_t tmpl_lan_msg_hdr,
                                fiid_template_t tmpl_cmd,
                                fiid_template_t tmpl_ipmb_msg_hdr,
                                fiid_template_t tmpl_ipmb_cmd)
{
  int ret1, ret2;

  if (!pkt
      || !IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
          && (!confidentiality_key
              || !confidentiality_key_len))
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

  return (_ipmi_dump_rmcpplus_packet (fd,
                                      prefix,
                                      hdr,
                                      trlr,
                                      authentication_algorithm,
                                      integrity_algorithm,
                                      confidentiality_algorithm,
                                      integrity_key,
                                      integrity_key_len,
                                      confidentiality_key,
                                      confidentiality_key_len,
                                      pkt,
                                      pkt_len,
                                      tmpl_lan_msg_hdr,
                                      tmpl_cmd,
                                      tmpl_ipmb_msg_hdr,
                                      tmpl_ipmb_cmd));
}
