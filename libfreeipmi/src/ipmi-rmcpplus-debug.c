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
#include <errno.h>

#include "freeipmi/ipmi-rmcpplus-debug.h"
#include "freeipmi/ipmi-rmcpplus.h"
#include "freeipmi/ipmi-debug.h"
#include "freeipmi/ipmi-lan.h"
#include "freeipmi/rmcp.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

static int32_t
_dump_rmcpplus_hdr_session(int fd, 
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
  fiid_obj_t obj_rmcpplus_hdr_session = NULL;
  unsigned int indx = 0;
  int32_t obj_len, rv = -1;

  if (!pkt
      || !payload_type
      || !payload_authenticated
      || !payload_encrypted
      || !session_id
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      return (-1);
    }
  
  /*
   * Extract auth_type and payload information
   */
  FIID_OBJ_CREATE_CLEANUP(obj_rmcpplus_hdr_session, tmpl_rmcpplus_hdr_session);
  FIID_OBJ_SET_BLOCK_LEN_CLEANUP(obj_len,
				 obj_rmcpplus_hdr_session,
				 (uint8_t *)"authentication_type",
				 (uint8_t *)"payload_type.encrypted",
				 pkt + indx,
				 pkt_len - indx);
  indx += obj_len;

  if (pkt_len <= indx)
    goto output;

  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_hdr_session, "payload_type", payload_type);
  
  /*
   * Extract OEM IANA and OEM Payload ID
   */
  if (*payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      FIID_OBJ_SET_BLOCK_LEN_CLEANUP(obj_len,
				     obj_rmcpplus_hdr_session,
				     (uint8_t *)"oem_iana",
				     (uint8_t *)"oem_payload_id",
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
				 obj_rmcpplus_hdr_session,
				 (uint8_t *)"session_id",
				 (uint8_t *)"ipmi_payload_len",
				 pkt + indx,
				 pkt_len - indx);
  indx += obj_len;

  if (pkt_len <= indx)
    goto output;

  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_hdr_session,
			"payload_type.authenticated",
			payload_authenticated);
  
  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_hdr_session,
			"payload_type.encrypted",
			payload_encrypted);
  
  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_hdr_session,
			"session_id",
			session_id);

  FIID_OBJ_GET_CLEANUP (obj_rmcpplus_hdr_session,
			"ipmi_payload_len",
			ipmi_payload_len);
  
 output:
  ERR (!(ipmi_obj_dump_perror (fd, prefix, session_hdr, NULL, obj_rmcpplus_hdr_session) < 0));
  
  rv = indx;
 cleanup:
  FIID_OBJ_DESTROY(obj_rmcpplus_hdr_session);
  return (rv);
}

static int32_t
_dump_rmcpplus_payload_special(int fd,
                               char *prefix,
                               char *payload_hdr,
                               char *cmd_hdr,
                               uint8_t payload_type,
                               fiid_template_t tmpl_msg_hdr,
                               fiid_template_t tmpl_cmd,
                               uint8_t *pkt,
                               uint32_t ipmi_payload_len)
{
  fiid_obj_t obj_payload = NULL;
  fiid_obj_t obj_cmd = NULL;
  int32_t rv = -1;

  if ((payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
      /* XXX need to dump regardless of matching? */
#if 0
      || (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
	  && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_open_session_rq) != 1))
      || (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
	  && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_open_session_rs) != 1))
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
	  && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_1) != 1))
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
	  && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_2) != 1))
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
	  && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_3) != 1))
      || (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4
	  && (fiid_template_compare(tmpl_cmd, tmpl_rmcpplus_rakp_message_4) != 1))
#endif
      || !tmpl_msg_hdr
      || !tmpl_cmd
      || !pkt)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_special: Invalid parameters");
      return (-1);
    }
  
  FIID_OBJ_CREATE_CLEANUP(obj_payload, tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA_CLEANUP(obj_payload, 
			    "payload_data",
			    pkt,
			    ipmi_payload_len);

  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd,
				       prefix,
				       payload_hdr,
				       NULL,
				       obj_payload) < 0));

  if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_open_session_rq);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
    FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_rmcpplus_open_session_rs);
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
  FIID_OBJ_DESTROY_NO_RETURN(obj_payload);
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd);
  return (rv);
}

static int32_t
_dump_rmcpplus_payload_data(int fd, 
                            char *prefix, 
                            char *msg_hdr,
                            char *cmd_hdr,
                            char *trlr_hdr,
                            fiid_template_t tmpl_msg_hdr,
                            fiid_template_t tmpl_cmd,
                            uint8_t *pkt,
                            uint32_t lan_msg_len)
{
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  int32_t len, obj_cmd_len, obj_lan_msg_trlr_len;
  unsigned int indx = 0;
  int32_t rv = -1;

  if (!tmpl_msg_hdr
#if 0
      || (fiid_template_compare(tmpl_msg_hdr, tmpl_lan_msg_hdr_rq) != 1
	  && fiid_template_compare(tmpl_msg_hdr, tmpl_lan_msg_hdr_rs) != 1)
#endif
      || !tmpl_cmd
      || !pkt
      || !lan_msg_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_data: Invalid parameters");
      return (-1);
    }
  
  /* Dump message header */

  FIID_OBJ_CREATE_CLEANUP(obj_lan_msg_hdr, tmpl_msg_hdr);
  FIID_OBJ_SET_ALL_LEN_CLEANUP(len, 
			       obj_lan_msg_hdr,
			       pkt + indx,
			       lan_msg_len - indx);
  indx += len;
  ERR_CLEANUP (!(ipmi_obj_dump_perror (fd, prefix, msg_hdr, NULL, obj_lan_msg_hdr) < 0));

  if (lan_msg_len <= indx)
    return 0;

  ERR_EXIT (!((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));
  
  if ((lan_msg_len - indx) >= obj_lan_msg_trlr_len)
    obj_cmd_len = (lan_msg_len - indx) - obj_lan_msg_trlr_len;
  else
    obj_cmd_len = 0;

  /* Dump command data */
  if (obj_cmd_len)
    {
      FIID_OBJ_CREATE_CLEANUP(obj_cmd, tmpl_cmd);

      FIID_OBJ_CLEAR (obj_cmd);
      FIID_OBJ_SET_ALL_LEN_CLEANUP (len,
				    obj_cmd,
				    pkt + indx,
				    obj_cmd_len);
      indx += len;

      if (lan_msg_len <= indx)
	return 0;
    }

  /* Dump trailer */

  FIID_OBJ_CREATE_CLEANUP(obj_lan_msg_trlr, tmpl_lan_msg_trlr);
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len,
				obj_lan_msg_trlr,
				pkt + indx,
				lan_msg_len - indx);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_lan_msg_hdr);
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd);
  FIID_OBJ_DESTROY_NO_RETURN(obj_lan_msg_trlr);
  return (rv);
}

#if 0

static int32_t
_dump_rmcpplus_payload_confidentiality_none(int fd, 
                                            char *prefix, 
                                            char *payload_hdr, 
                                            char *msg_hdr,
                                            char *cmd_hdr,
                                            char *trlr_hdr,
                                            fiid_template_t tmpl_msg_hdr,
                                            fiid_template_t tmpl_cmd,
                                            uint8_t *pkt,
                                            uint32_t ipmi_payload_len)
{
  fiid_obj_t obj_payload;

  if (!tmpl_msg_hdr
      || !tmpl_cmd
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_none: Invalid parameters");
      return (-1);
    }

  FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA(obj_payload, 
                    tmpl_rmcpplus_payload,
                    "payload_data",
                    pkt,
                    ipmi_payload_len);

  FIID_OBJ_SET(obj_payload,
               tmpl_rmcpplus_payload,
               "payload_data_len",
               ipmi_payload_len);

  /* XXX call dump itself */
  if (_dump_rmcpplus_payload_object(fd,
                                    prefix,
                                    payload_hdr,
                                    obj_payload) < 0)
    return (-1);

  if (_dump_rmcpplus_payload_data(fd,
                                  prefix,
                                  msg_hdr,
                                  cmd_hdr,
                                  trlr_hdr,
                                  tmpl_msg_hdr,
                                  tmpl_cmd,
                                  pkt,
                                  ipmi_payload_len) < 0)
    return (-1);

  return (0);
}

static int32_t
_dump_rmcpplus_payload_confidentiality_aes_cbc_128(int fd,
                                                   char *prefix,
                                                   char *payload_hdr,
                                                   char *msg_hdr,
                                                   char *cmd_hdr,
                                                   char *trlr_hdr,
                                                   fiid_template_t tmpl_msg_hdr,
                                                   fiid_template_t tmpl_cmd,
                                                   uint8_t *confidentiality_key,
                                                   uint32_t confidentiality_key_len,
                                                   uint8_t *pkt,
                                                   int32_t ipmi_payload_len)
{
  uint8_t iv[IPMI_AES_CBC_128_IV_LEN];
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_len;
  int cipher_keylen, cipher_blocklen;
  int32_t payload_data_len, decrypt_len, cmd_data_len;
  fiid_obj_t obj_payload;
  unsigned int indx = 0;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  if (!tmpl_msg_hdr
      || !tmpl_cmd
      || !confidentiality_key
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_aes_cbc_128: Invalid parameters");
      return (-1);
    }
  
  if ((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0)
    return (-1);
  
  ERR_EXIT (!(cipher_keylen < IPMI_AES_CBC_128_KEY_LEN));
  
  if (confidentiality_key_len < IPMI_AES_CBC_128_KEY_LEN)
    {
      errno = EINVAL;
      return (-1);
    }
  confidentiality_key_len = IPMI_AES_CBC_128_KEY_LEN;
  
  if ((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0)
    return (-1);
  
  ERR_EXIT (cipher_blocklen == IPMI_AES_CBC_128_BLOCK_LEN);
  
  if (ipmi_payload_len < IPMI_AES_CBC_128_BLOCK_LEN)
    {
      errno = EINVAL;
      return (-1);
    }

  payload_data_len = ipmi_payload_len - IPMI_AES_CBC_128_BLOCK_LEN;

  if (payload_data_len <= 0)
    {
      errno = EINVAL;
      return (-1);
    }

  memcpy(iv, pkt, IPMI_AES_CBC_128_BLOCK_LEN);
  indx += IPMI_AES_CBC_128_BLOCK_LEN;
  memcpy(payload_buf, pkt + indx, payload_data_len);

  FIID_OBJ_ALLOCA(obj_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_MEMSET(obj_payload, '\0', tmpl_rmcpplus_payload);

  FIID_OBJ_SET_DATA(obj_payload,
                    tmpl_rmcpplus_payload,
                    "confidentiality_header",
                    iv,
                    IPMI_AES_CBC_128_BLOCK_LEN);

  FIID_OBJ_SET(obj_payload,
               tmpl_rmcpplus_payload,
               "confidentiality_header_len",
               IPMI_AES_CBC_128_BLOCK_LEN);

  if ((decrypt_len = ipmi_crypt_cipher_decrypt(IPMI_CRYPT_CIPHER_AES,
                                               IPMI_CRYPT_CIPHER_MODE_CBC,
                                               confidentiality_key,
                                               confidentiality_key_len,
                                               iv,
					       IPMI_AES_CBC_128_BLOCK_LEN,
                                               payload_buf,
                                               payload_data_len)) < 0)
    return (-1);
  
  if (decrypt_len != payload_data_len)
    {
      ipmi_debug("ipmi_crypt_cipher_decrypt: Invalid decryption length");
      return (-1);
    }
  
  pad_len = payload_buf[payload_data_len - 1];
  if (pad_len > IPMI_AES_CBC_128_BLOCK_LEN)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_aes_cbc_128: invalid pad_len");
      return (-1);
    }
  
  cmd_data_len = payload_data_len - pad_len - 1;
  if (cmd_data_len <= 0)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload_confidentiality_aes_cbc_128: invalid cmd_data_len");
      return (-1);
    }
  
  FIID_OBJ_SET_DATA(obj_payload,
		    tmpl_rmcpplus_payload,
		    "payload_data",
		    payload_buf,
		    cmd_data_len);

  FIID_OBJ_SET(obj_payload,
	       tmpl_rmcpplus_payload,
	       "payload_data_len",
	       cmd_data_len);

  FIID_OBJ_SET_DATA(obj_payload,
		    tmpl_rmcpplus_payload,
		    "confidentiality_trailer",
		    payload_buf + cmd_data_len,
		    pad_len + 1);

  FIID_OBJ_SET(obj_payload,
	       tmpl_rmcpplus_payload,
	       "confidentiality_trailer_len",
	       pad_len + 1);
  
  /* XXX call dump itself */
  if (_dump_rmcpplus_payload_object(fd,
                                    prefix,
                                    payload_hdr,
                                    obj_payload) < 0)
    return (-1);
  
  if (_dump_rmcpplus_payload_data(fd,
                                  prefix,
                                  msg_hdr,
                                  cmd_hdr,
                                  trlr_hdr,
                                  tmpl_msg_hdr,
                                  tmpl_cmd,
                                  payload_buf,
                                  cmd_data_len) < 0)
    return (-1);

  return (0);  
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
                       fiid_template_t tmpl_msg_hdr, 
                       fiid_template_t tmpl_cmd,
                       uint8_t *confidentiality_key,
                       uint32_t confidentiality_key_len,
                       uint8_t *pkt, 
                       uint32_t ipmi_payload_len)
{
  if (!IPMI_PAYLOAD_TYPE_VALID(payload_type)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
      || !ipmi_payload_len
      || !tmpl_msg_hdr
      || !tmpl_cmd
      || !pkt
      || !ipmi_payload_len)
    {
      errno = EINVAL;
      ipmi_debug("_dump_rmcpplus_payload: Invalid parameters");
      return (-1);
    }

  /* First determine if this is a special payload_type
   *
   * Note: We don't check consider RAKP1 or RAKP3 special b/c
   * they are requests, not responses
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
      || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
      || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      return _dump_rmcpplus_payload_special(fd,
                                            prefix,
                                            payload_hdr,
                                            cmd_hdr,
                                            payload_type,
                                            tmpl_msg_hdr,
                                            tmpl_cmd,
                                            pkt,
                                            ipmi_payload_len);
    }
  else
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _dump_rmcpplus_payload_confidentiality_none(fd,
                                                           prefix,
                                                           payload_hdr,
                                                           msg_hdr,
                                                           cmd_hdr,
                                                           trlr_hdr,
                                                           tmpl_msg_hdr,
                                                           tmpl_cmd,
                                                           pkt,
                                                           ipmi_payload_len);
      else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
        return _dump_rmcpplus_payload_confidentiality_aes_cbc_128(fd,
                                                                  prefix,
                                                                  payload_hdr,
                                                                  msg_hdr,
                                                                  cmd_hdr,
                                                                  trlr_hdr,
                                                                  tmpl_msg_hdr,
                                                                  tmpl_cmd,
                                                                  confidentiality_key,
                                                                  confidentiality_key_len,
                                                                  pkt,
                                                                  ipmi_payload_len);
      else
        {
          /* achu: Even though the algorithm is legit, we don't support it yet :-( */
          errno = EINVAL;
          return (-1);
        }
    }

  /* NOT REACHED */
  return (-1);
}

static int32_t
_dump_rmcpplus_session_trlr(int fd,
                            char *prefix,
                            char *session_trlr_hdr,
                            uint8_t integrity_algorithm,
                            fiid_template_t tmpl_trlr_session,
                            uint8_t *pkt,
                            uint32_t pkt_len)
{
  int32_t pad_length_field_len, next_header_field_len, pad_length, authcode_len;
  uint8_t buf[IPMI_MAX_PAYLOAD_LENGTH];
  char *auth_field;
  fiid_field_t *tmpl_rmcpplus_session_trlr_dump;
  unsigned int indx = 0;

  if (!IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
      || !tmpl_trlr_session
      || !fiid_obj_field_lookup (tmpl_trlr_session, "integrity_pad")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "pad_length")
      || !fiid_obj_field_lookup (tmpl_trlr_session, "next_header"))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    authcode_len = 0;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    authcode_len = IPMI_HMAC_SHA1_96_AUTHCODE_LEN;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    authcode_len = IPMI_HMAC_MD5_128_AUTHCODE_LEN;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    authcode_len = IPMI_MD5_128_AUTHCODE_LEN;
  else
    {
      /* achu: Even though the algorithm is legit, we don't support it yet :-( */
      errno = EINVAL;
      return (-1);
    }
  
  ERR_EXIT (!((pad_length_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "pad_length")) < 0));
  ERR_EXIT (!((next_header_field_len = fiid_obj_field_len_bytes (tmpl_trlr_session, "next_header")) < 0));
  
  if (pkt_len < (pad_length_field_len))
    {
      next_header_field_len = 0;
      authcode_len = 0;
    }
  else if (pkt_len < (pad_length_field_len + next_header_field_len))
    authcode_len = 0;
  else if (pkt_len < (authcode_len + pad_length_field_len + next_header_field_len))
    authcode_len = pkt_len - pad_length_field_len - next_header_field_len;
  
  pad_length = pkt_len - pad_length_field_len - next_header_field_len - authcode_len;
  
  if (fiid_obj_field_lookup (tmpl_trlr_session, "auth_code"))
    auth_field = "auth_code";
  else
    auth_field = "auth_calc_data";

  memset(buf, '\0', IPMI_MAX_PAYLOAD_LENGTH);
  if (pad_length)
    {
      memcpy(buf + indx, pkt + indx, pad_length);
      indx += pad_length;
    }
  
  if (pad_length_field_len)
    {
      memcpy(buf + indx, pkt + indx, pad_length_field_len); 
      indx += pad_length_field_len;
    }

  if (next_header_field_len)
    {
      memcpy(buf + indx, pkt + indx, next_header_field_len);
      indx += next_header_field_len;
    }
  
  if (authcode_len)
    {
      memcpy(buf + indx, pkt + indx, authcode_len);
      indx += authcode_len;
    }
  
  if (pad_length)
    {
      if (!(tmpl_rmcpplus_session_trlr_dump = fiid_template_make((pad_length *8),              "integrity_pad",
                                                                 (pad_length_field_len * 8),   "pad_length",
                                                                 (next_header_field_len * 8),  "next_header",
                                                                 (authcode_len * 8),           auth_field)))
        return (-1);
    }
  else
    {
      if (!(tmpl_rmcpplus_session_trlr_dump = fiid_template_make((pad_length_field_len * 8),   "pad_length",
                                                                 (next_header_field_len * 8),  "next_header",
                                                                 (authcode_len * 8),           auth_field)))
        return (-1);
    }
  
  ERR_OUT(fiid_obj_dump_perror (fd, prefix, session_trlr_hdr, NULL, buf, tmpl_rmcpplus_session_trlr_dump) != -1);

  fiid_template_free(tmpl_rmcpplus_session_trlr_dump);

  return (indx);
}

int32_t
fiid_obj_dump_rmcpplus (int fd, 
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
                        fiid_template_t tmpl_msg_hdr, 
                        fiid_template_t tmpl_cmd,
                        fiid_template_t tmpl_trlr_session)
{
  int32_t obj_rmcp_hdr_len, obj_len;
  uint64_t payload_type, payload_authenticated, payload_encrypted, session_id, ipmi_payload_len;
  uint8_t buf[IPMI_MAX_PAYLOAD_LENGTH];
  char prefix_buf[IPMI_MAX_PAYLOAD_LENGTH];
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

  if (!pkt
      || !tmpl_msg_hdr
      || !tmpl_cmd
      || !tmpl_trlr_session)
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_MAX_PAYLOAD_LENGTH) < 0)
    return (-1);

  /* Dump rmcp header */

  obj_rmcp_hdr_len = fiid_obj_len_bytes (tmpl_rmcp_hdr);
  if ((pkt_len - indx) < obj_len)
    {
      memset(buf, '\0', IPMI_MAX_PAYLOAD_LENGTH);
      memcpy(buf, pkt + indx, (pkt_len - indx)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, buf, tmpl_rmcp_hdr) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, pkt + indx, tmpl_rmcp_hdr) != -1);
  indx += obj_rmcp_hdr_len;

  if (pkt_len <= indx)
    return 0;
  
  /* Dump rmcpplus session header */

  if ((obj_len = _dump_rmcpplus_hdr_session(fd,
                                            prefix_buf,
                                            session_hdr,
                                            pkt + indx,
                                            pkt_len - indx,
                                            &payload_type,
                                            &payload_authenticated,
                                            &payload_encrypted,
                                            &session_id,
                                            &ipmi_payload_len)) < 0)

    return (-1);
  indx += obj_len;

  if (pkt_len <= indx)
    return 0;

  /* Dump Payload */

  if (_dump_rmcpplus_payload(fd, 
                             prefix_buf, 
                             payload_hdr, 
                             msg_hdr,
                             cmd_hdr,
                             trlr_hdr,
                             payload_type,
                             authentication_algorithm,
                             confidentiality_algorithm,
                             tmpl_msg_hdr, 
                             tmpl_cmd,
                             confidentiality_key,
                             confidentiality_key_len,
                             pkt + indx, 
                             ipmi_payload_len) < 0)
    return (-1);

  indx += ipmi_payload_len;

  if (pkt_len <= indx)
    return 0;

  /* Dump trailer */

  if (session_id && payload_authenticated)
    {
      if ((obj_len = _dump_rmcpplus_session_trlr(fd,
                                                 prefix_buf,
                                                 session_trlr_hdr,
                                                 integrity_algorithm,
                                                 tmpl_trlr_session,
                                                 pkt + indx,
                                                 pkt_len - indx)) < 0)
        return (-1);
      indx += obj_len;

      if (pkt_len <= indx)
        return 0;
    }


  /* Dump extra stuff if packet is longer than expected */
  if ((pkt_len - indx) > 0)
    {
      fiid_field_t *tmpl_extra;

      if (!(tmpl_extra = fiid_template_make((pkt_len - indx) * 8, "extra")))
        return (-1);

      ERR_OUT(fiid_obj_dump_perror(fd, prefix_buf, extra_hdr, NULL, pkt + indx, tmpl_extra) != -1);

      fiid_template_free(tmpl_extra);
    }

  return 0;
}
#endif
