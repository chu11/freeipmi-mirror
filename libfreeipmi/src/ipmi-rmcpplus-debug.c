/* 
   ipmi-rmcpplus-interface.c - IPMI RMCPPLUS Debug

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/ipmi-rmcpplus.h"
#include "freeipmi/ipmi-rmcpplus-utils.h"
#include "freeipmi/ipmi-crypt.h"
#include "freeipmi/ipmi-debug.h"
#include "freeipmi/ipmi-lan.h"
#include "freeipmi/ipmi-sol-cmds.h"
#include "freeipmi/rmcp.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"

static int32_t
_dump_rmcpplus_session_hdr(int fd, 
                           char *prefix, 
                           char *session_hdr, 
                           uint8_t *pkt, 
                           uint32_t pkt_len,
                           uint64_t *payload_type,
                           uint64_t *payload_authenticated,
                           uint64_t *payload_encrypted,
                           uint64_t *session_id,
                           uint64_t *ipmi_payload_len)
{
  fiid_obj_t obj_rmcpplus_session_hdr = NULL;
  unsigned int indx = 0;
  int32_t obj_len, rv = -1;

  assert(pkt
         && pkt_len
         && payload_type
         && payload_authenticated
         && payload_encrypted
         && session_id
         && ipmi_payload_len);

  /*
   * Extract auth_type and payload information
   */
  FIID_OBJ_CREATE_CLEANUP(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);
  FIID_OBJ_SET_BLOCK_LEN_CLEANUP(obj_len,
				 obj_rmcpplus_session_hdr,
				 "authentication_type",
				 "payload_type.encrypted",
				 pkt + indx,
				 pkt_len - indx);
  indx += obj_len;

  if (pkt_len <= indx)
    goto output;

  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_session_hdr, "payload_type", payload_type);
  
  /*
   * Extract OEM IANA and OEM Payload ID
   */
  if (*payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      FIID_OBJ_SET_BLOCK_LEN_CLEANUP(obj_len,
				     obj_rmcpplus_session_hdr,
				     "oem_iana",
				     "oem_payload_id",
				     pkt + indx,
				     pkt_len - indx);
      indx += obj_len;

      if (pkt_len <= indx)
	goto output;
    }

  /*
   * Extract Session ID, Session Sequence Number, and Payload Length
   */
  FIID_OBJ_SET_BLOCK_LEN_CLEANUP(obj_len,
				 obj_rmcpplus_session_hdr,
				 "session_id",
				 "ipmi_payload_len",
				 pkt + indx,
				 pkt_len - indx);
  indx += obj_len;

  if (pkt_len <= indx)
    goto output;

  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_session_hdr,
			"payload_type.authenticated",
			payload_authenticated);
  
  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_session_hdr,
			"payload_type.encrypted",
			payload_encrypted);
  
  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_session_hdr,
			"session_id",
			session_id);

  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_session_hdr,
			"ipmi_payload_len",
			ipmi_payload_len);
  
 output:
  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd, prefix, session_hdr, NULL, obj_rmcpplus_session_hdr) < 0));
  
  rv = indx;
 cleanup:
  FIID_OBJ_DESTROY(obj_rmcpplus_session_hdr);
  return (rv);
}

static int32_t
_dump_rmcpplus_payload_data(int fd, 
                            char *prefix, 
                            char *msg_hdr,
                            char *cmd_hdr,
                            char *trlr_hdr,
                            uint8_t payload_type,
                            fiid_template_t tmpl_lan_msg_hdr,
                            fiid_template_t tmpl_cmd,
                            uint8_t *pkt,
                            uint32_t ipmi_payload_len)
{
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  int32_t len, obj_cmd_len, obj_lan_msg_trlr_len;
  unsigned int indx = 0;
  int32_t rv = -1;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt
          && ipmi_payload_len);

  /* Dump message header */

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      FIID_OBJ_CREATE_CLEANUP(obj_lan_msg_hdr, tmpl_lan_msg_hdr);
      FIID_OBJ_SET_ALL_LEN_CLEANUP(len, 
                                   obj_lan_msg_hdr,
                                   pkt + indx,
                                   ipmi_payload_len - indx);
      indx += len;
      ERR_CLEANUP (!(ipmi_obj_dump_perror (fd, prefix, msg_hdr, NULL, obj_lan_msg_hdr) < 0));
      
      if (ipmi_payload_len <= indx)
	{
	  rv = 0;
	  goto cleanup;
	}

      ERR_EXIT (!((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));
      
      if ((ipmi_payload_len - indx) >= obj_lan_msg_trlr_len)
        obj_cmd_len = (ipmi_payload_len - indx) - obj_lan_msg_trlr_len;
      else
        obj_cmd_len = 0;
    }
  else /* payload_type == IPMI_PAYLOAD_TYPE_SOL */
    obj_cmd_len = ipmi_payload_len;

  /* Dump command data */
  if (obj_cmd_len)
    {
      FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_cmd);
      
      FIID_OBJ_CLEAR_CLEANUP (obj_cmd);
      FIID_OBJ_SET_ALL_LEN_CLEANUP (len,
				    obj_cmd,
				    pkt + indx,
				    obj_cmd_len);
      indx += len;
      ERR_CLEANUP (!(ipmi_obj_dump_perror (fd, 
					   prefix, 
					   cmd_hdr, 
					   NULL, 
					   obj_cmd) < 0));
      
      if (ipmi_payload_len <= indx)
	{
	  rv = 0;
	  goto cleanup;
	}
    }
  
  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      /* Dump trailer */

      FIID_OBJ_CREATE_CLEANUP(obj_lan_msg_trlr, tmpl_lan_msg_trlr);
      FIID_OBJ_SET_ALL_LEN_CLEANUP (len,
                                    obj_lan_msg_trlr,
                                    pkt + indx,
                                    ipmi_payload_len - indx);
      indx += len;
      ERR_CLEANUP (!(ipmi_obj_dump_perror (fd,
                                           prefix, 
                                           trlr_hdr, 
                                           NULL, 
                                           obj_lan_msg_trlr) < 0));
    }

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_lan_msg_hdr);
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd);
  FIID_OBJ_DESTROY_NO_RETURN(obj_lan_msg_trlr);
  return (rv);
}

static int32_t
_dump_rmcpplus_payload_confidentiality_none(int fd, 
                                            char *prefix, 
                                            char *payload_hdr, 
                                            char *msg_hdr,
                                            char *cmd_hdr,
                                            char *trlr_hdr,
                                            uint8_t payload_type,
                                            fiid_template_t tmpl_lan_msg_hdr,
                                            fiid_template_t tmpl_cmd,
                                            uint8_t *pkt,
                                            uint32_t ipmi_payload_len)
{
  fiid_obj_t obj_rmcpplus_payload = NULL;
  int32_t rv = -1;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
                    
          && pkt
          && ipmi_payload_len);

  FIID_OBJ_CREATE_CLEANUP(obj_rmcpplus_payload, tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA_CLEANUP (obj_rmcpplus_payload, 
                             "payload_data",
                             pkt,
                             ipmi_payload_len);

  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd,
				       prefix,
				       payload_hdr,
				       NULL,
				       obj_rmcpplus_payload) < 0));

  ERR_CLEANUP (!(_dump_rmcpplus_payload_data(fd,
                                             prefix,
                                             msg_hdr,
                                             cmd_hdr,
                                             trlr_hdr,
                                             payload_type,
                                             tmpl_lan_msg_hdr,
                                             tmpl_cmd,
                                             pkt,
                                             ipmi_payload_len) < 0));

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_rmcpplus_payload);
  return (rv);
}

static int32_t
_dump_rmcpplus_payload_confidentiality_aes_cbc_128(int fd,
                                                   char *prefix,
                                                   char *payload_hdr,
                                                   char *msg_hdr,
                                                   char *cmd_hdr,
                                                   char *trlr_hdr,
                                                   uint8_t payload_type,
                                                   fiid_template_t tmpl_lan_msg_hdr,
                                                   fiid_template_t tmpl_cmd,
                                                   uint8_t *confidentiality_key,
                                                   uint32_t confidentiality_key_len,
                                                   uint8_t *pkt,
                                                   int32_t ipmi_payload_len)
{
  uint8_t iv[IPMI_CRYPT_AES_CBC_128_IV_LENGTH];
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_len;
  int cipher_keylen, cipher_blocklen;
  int32_t payload_data_len, decrypt_len, cmd_data_len;
  fiid_obj_t obj_rmcpplus_payload = NULL;
  unsigned int indx = 0;
  int32_t rv = -1;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && confidentiality_key
          && confidentiality_key_len
          && pkt
          && ipmi_payload_len);
  
  ERR_CLEANUP (!((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (!(cipher_keylen < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));
  
  ERR_EINVAL_CLEANUP (!(confidentiality_key_len < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));
  confidentiality_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;
  
  ERR_CLEANUP (!((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (cipher_blocklen == IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
  ERR_EINVAL_CLEANUP (!(ipmi_payload_len < IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH));

  payload_data_len = ipmi_payload_len - IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;
  ERR_EINVAL_CLEANUP (!(payload_data_len <= 0));

  memcpy(iv, pkt, IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
  indx += IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;
  memcpy(payload_buf, pkt + indx, payload_data_len);

  FIID_OBJ_CREATE_CLEANUP(obj_rmcpplus_payload, tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_payload,
                            "confidentiality_header",
                            iv,
                            IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);

  ERR_CLEANUP (!((decrypt_len = ipmi_crypt_cipher_decrypt(IPMI_CRYPT_CIPHER_AES,
                                                          IPMI_CRYPT_CIPHER_MODE_CBC,
                                                          confidentiality_key,
                                                          confidentiality_key_len,
                                                          iv,
                                                          IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH,
                                                          payload_buf,
                                                          payload_data_len)) < 0));
  ERR_CLEANUP (!(decrypt_len != payload_data_len));
  
  pad_len = payload_buf[payload_data_len - 1];
  ERR_EINVAL_CLEANUP (!(pad_len > IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH));
  
  cmd_data_len = payload_data_len - pad_len - 1;
  ERR_EINVAL_CLEANUP (!(cmd_data_len <= 0));
  
  FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_payload,
			    "payload_data",
			    payload_buf,
			    cmd_data_len);
  
  FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_payload,
			    "confidentiality_trailer",
			    payload_buf + cmd_data_len,
			    pad_len + 1);

  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd,
				       prefix,
				       payload_hdr,
				       NULL,
				       obj_rmcpplus_payload) < 0));
  
  ERR_CLEANUP (!(_dump_rmcpplus_payload_data(fd,
                                             prefix,
                                             msg_hdr,
                                             cmd_hdr,
                                             trlr_hdr,
                                             payload_type,
                                             tmpl_lan_msg_hdr,
                                             tmpl_cmd,
                                             payload_buf,
                                             cmd_data_len) < 0));
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_rmcpplus_payload);
  return (rv);  
}

static int32_t
_dump_rmcpplus_payload_rakp(int fd,
                            char *prefix,
                            char *payload_hdr,
                            char *cmd_hdr,
                            uint8_t payload_type,
                            fiid_template_t tmpl_cmd,
                            uint8_t *pkt,
                            uint32_t ipmi_payload_len)
{
  fiid_obj_t obj_rmcpplus_payload = NULL;
  fiid_obj_t obj_cmd = NULL;
  int32_t rv = -1;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
               && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_open_session_request) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
               && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_open_session_response) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
               && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_1) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
               && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_2) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
               && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_3) != 1))
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4
               && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_4) != 1))
          && pkt
          && ipmi_payload_len);
  
  FIID_OBJ_CREATE_CLEANUP(obj_rmcpplus_payload, tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_payload, 
			    "payload_data",
			    pkt,
			    ipmi_payload_len);

  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd,
				       prefix,
				       payload_hdr,
				       NULL,
				       obj_rmcpplus_payload) < 0));

  if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_open_session_request);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_open_session_response);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_rakp_message_1);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_rakp_message_2);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_rakp_message_3);
  else /* IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4 */
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_rakp_message_4);
			
  FIID_OBJ_SET_ALL_CLEANUP(obj_cmd,
			   pkt,
			   ipmi_payload_len);
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd,
				       prefix,
				       cmd_hdr,
				       NULL,
				       obj_cmd) < 0));

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_rmcpplus_payload);
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd);
  return (rv);
}

static int32_t
_dump_rmcpplus_payload(int fd, 
                       char *prefix, 
                       char *payload_hdr, 
                       char *msg_hdr,
                       char *cmd_hdr,
                       char *trlr_hdr,
                       uint8_t payload_type,
                       uint8_t authentication_algorithm,
                       uint8_t confidentiality_algorithm,
                       fiid_template_t tmpl_lan_msg_hdr, 
                       fiid_template_t tmpl_cmd,
                       uint8_t *confidentiality_key,
                       uint32_t confidentiality_key_len,
                       uint8_t *pkt, 
                       uint32_t ipmi_payload_len)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
          && (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
              || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
          && !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
               && !(confidentiality_key
                    && confidentiality_key_len))
          && ipmi_payload_len
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(tmpl_lan_msg_hdr
                    && (fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
                        || fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1)))
          && tmpl_cmd
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1
                    || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt
          && ipmi_payload_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI
      || payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _dump_rmcpplus_payload_confidentiality_none(fd,
                                                           prefix,
                                                           payload_hdr,
                                                           msg_hdr,
                                                           cmd_hdr,
                                                           trlr_hdr,
                                                           payload_type,
                                                           tmpl_lan_msg_hdr,
                                                           tmpl_cmd,
                                                           pkt,
                                                           ipmi_payload_len);
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return _dump_rmcpplus_payload_confidentiality_aes_cbc_128(fd,
                                                                  prefix,
                                                                  payload_hdr,
                                                                  msg_hdr,
                                                                  cmd_hdr,
                                                                  trlr_hdr,
                                                                  payload_type,
                                                                  tmpl_lan_msg_hdr,
                                                                  tmpl_cmd,
                                                                  confidentiality_key,
                                                                  confidentiality_key_len,
                                                                  pkt,
                                                                  ipmi_payload_len);
    }
  else
    return _dump_rmcpplus_payload_rakp(fd,
                                       prefix,
                                       payload_hdr,
                                       cmd_hdr,
                                       payload_type,
                                       tmpl_cmd,
                                       pkt,
                                       ipmi_payload_len);

  /* NOT REACHED */
  return (-1);
}

static int32_t
_dump_rmcpplus_session_trlr(int fd,
                            char *prefix,
                            char *session_trlr_hdr,
			    uint64_t session_id,
			    uint64_t payload_authenticated,
                            uint8_t integrity_algorithm,
                            uint8_t *pkt,
                            uint32_t pkt_len)
{
  int32_t pad_length_field_len, next_header_field_len, pad_length, authentication_code_len;
  fiid_obj_t obj_rmcpplus_session_trlr = NULL;
  unsigned int indx = 0;
  int32_t rv = -1;

  assert (IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm));

  if (!session_id || payload_authenticated == IPMI_PAYLOAD_FLAG_UNAUTHENTICATED)
    return (0);

  if (payload_authenticated)
    {
      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
	authentication_code_len = 0;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
	authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
	authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
      else /* integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128 */
	authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  else
    authentication_code_len = 0;

  FIID_OBJ_CREATE_CLEANUP(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  ERR_EXIT (!((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "pad_length")) < 0));
  ERR_EXIT (!((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "next_header")) < 0));
  
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
      FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_session_trlr,
                                "integrity_pad",
                                pkt + indx,
                                pad_length);
      indx += pad_length;
    }
  
  if (pad_length_field_len)
    {
      FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_session_trlr,
                                "pad_length",
                                pkt + indx,
                                pad_length_field_len);
      indx += pad_length_field_len;
    }

  if (next_header_field_len)
    {
      FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_session_trlr,
                                "next_header",
                                pkt + indx,
                                next_header_field_len);
      indx += next_header_field_len;
    }
  
  if (authentication_code_len)
    {
      FIID_OBJ_SET_DATA_CLEANUP(obj_rmcpplus_session_trlr,
                                "authentication_code",
                                pkt + indx,
                                authentication_code_len);
      indx += authentication_code_len;
    }
  
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd, 
                                       prefix, 
                                       session_trlr_hdr, 
                                       NULL, 
                                       obj_rmcpplus_session_trlr) < 0));
  /* Clear out data */
  FIID_OBJ_CLEAR_CLEANUP(obj_rmcpplus_session_trlr);

  rv = indx;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_rmcpplus_session_trlr);
  return (rv);
}

int32_t
ipmi_dump_rmcpplus_packet (int fd, 
                           char *prefix, 
                           char *hdr, 
                           uint8_t authentication_algorithm,
                           uint8_t integrity_algorithm,
                           uint8_t confidentiality_algorithm,
                           uint8_t *integrity_key,
                           uint32_t integrity_key_len,
                           uint8_t *confidentiality_key,
                           uint32_t confidentiality_key_len,
                           uint8_t *pkt, 
                           uint32_t pkt_len, 
                           fiid_template_t tmpl_lan_msg_hdr, 
                           fiid_template_t tmpl_cmd)
{
  int32_t obj_rmcp_hdr_len, obj_len;
  uint64_t payload_type=0, payload_authenticated=0, payload_encrypted, session_id=0, ipmi_payload_len=0;
  char prefix_buf[IPMI_MAX_PAYLOAD_LENGTH];
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
  char *trlr_hdr =
    "IPMI Trailer:\n"
    "-------------";
  char *session_trlr_hdr = 
    "IPMI RMCPPLUS Session Trailer:\n"
    "------------------------------";
  char *extra_hdr =
    "Unexpected Data:\n"
    "----------------";
  unsigned int indx = 0;
  int32_t rv = -1;

  ERR_EINVAL (pkt
              && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
              && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
              && (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
                  || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
              && !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
                   && !(confidentiality_key
                        && confidentiality_key_len))
	      && tmpl_cmd);

  ERR_CLEANUP (!(ipmi_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_MAX_PAYLOAD_LENGTH) < 0));

  /* Dump rmcp header */

  FIID_OBJ_CREATE_CLEANUP(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_SET_ALL_LEN_CLEANUP(obj_rmcp_hdr_len,
                               obj_rmcp_hdr,
                               pkt + indx,
                               pkt_len - indx);
  indx += obj_rmcp_hdr_len;
                           
  ERR_CLEANUP(!(ipmi_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, obj_rmcp_hdr) < 0));

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }
  
  /* Dump rmcpplus session header */

  ERR_CLEANUP (!((obj_len = _dump_rmcpplus_session_hdr(fd,
                                                       prefix_buf,
                                                       session_hdr,
                                                       pkt + indx,
                                                       pkt_len - indx,
                                                       &payload_type,
                                                       &payload_authenticated,
                                                       &payload_encrypted,
                                                       &session_id,
                                                       &ipmi_payload_len)) < 0));
  indx += obj_len;

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }
  
  /* achu: If the packet is really messed up, dump the packet in raw form */
  if ((payload_type != IPMI_PAYLOAD_TYPE_IPMI
       && payload_type != IPMI_PAYLOAD_TYPE_SOL
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
           || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
      || !ipmi_payload_len)
    goto dump_extra;
  
  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    ERR_EINVAL_CLEANUP (tmpl_lan_msg_hdr
			&& (fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
			    || fiid_template_compare(tmpl_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1));
  else if (payload_type == IPMI_PAYLOAD_TYPE_SOL)
    ERR_EINVAL_CLEANUP ((fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data) == 1
			 || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1
			 || fiid_template_compare(tmpl_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1));

  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    FIID_TEMPLATE_COMPARE_CLEANUP(tmpl_cmd, tmpl_rmcpplus_open_session_request);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
    FIID_TEMPLATE_COMPARE_CLEANUP(tmpl_cmd, tmpl_rmcpplus_open_session_response);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    FIID_TEMPLATE_COMPARE_CLEANUP(tmpl_cmd, tmpl_rmcpplus_rakp_message_1);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    FIID_TEMPLATE_COMPARE_CLEANUP(tmpl_cmd, tmpl_rmcpplus_rakp_message_2);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    FIID_TEMPLATE_COMPARE_CLEANUP(tmpl_cmd, tmpl_rmcpplus_rakp_message_3);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    FIID_TEMPLATE_COMPARE_CLEANUP(tmpl_cmd, tmpl_rmcpplus_rakp_message_4);

  /* Dump Payload */

  ERR_CLEANUP (!(_dump_rmcpplus_payload(fd, 
                                        prefix_buf, 
                                        payload_hdr, 
                                        msg_hdr,
                                        cmd_hdr,
                                        trlr_hdr,
                                        payload_type,
                                        authentication_algorithm,
                                        confidentiality_algorithm,
                                        tmpl_lan_msg_hdr, 
                                        tmpl_cmd,
                                        confidentiality_key,
                                        confidentiality_key_len,
                                        pkt + indx, 
                                        ipmi_payload_len) < 0));
  indx += ipmi_payload_len;

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump trailer */

  ERR_CLEANUP (!((obj_len = _dump_rmcpplus_session_trlr(fd,
							prefix_buf,
							session_trlr_hdr,
							session_id,
							payload_authenticated,
							integrity_algorithm,
							pkt + indx,
							pkt_len - indx)) < 0));
  indx += obj_len;

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }
  
  /* Dump extra stuff if packet is longer than expected */
 dump_extra:
  if ((pkt_len - indx) > 0)
    {
      FIID_OBJ_CREATE_CLEANUP(obj_unexpected_data, tmpl_unexpected_data);

      FIID_OBJ_SET_ALL_LEN_CLEANUP (obj_len, obj_unexpected_data, pkt + indx, pkt_len - indx);
      indx += obj_len;

      ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, extra_hdr, NULL, obj_unexpected_data) < 0));
    }

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_rmcp_hdr);
  FIID_OBJ_DESTROY_NO_RETURN(obj_unexpected_data);
  return (rv);
}
