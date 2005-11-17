/*
   ipmi-lanplus-sessions.h - IPMI LAN Commands

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _IPMI_LANPLUS_SESSIONS_H
#define _IPMI_LANPLUS_SESSIONS_H

/**************************
 * IPMI 2.0 Payload Types *
 **************************/

#define IPMI_PAYLOAD_TYPE_IPMI                            0x00
#define IPMI_PAYLOAD_TYPE_SOL                             0x01
#define IPMI_PAYLOAD_TYPE_OEM_EXPLICIT                    0x02
#define IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST   0x10
#define IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE  0x11
#define IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1                  0x12
#define IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2                  0x13
#define IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3                  0x14
#define IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4                  0x15
/* 20h - 27h - OEM */
/* all other reserved */

#define IPMI_PAYLOAD_TYPE_VALID(__payload_type) \
        (((__payload_type) == IPMI_PAYLOAD_TYPE_IPMI \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_SOL \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1 \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2 \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3 \
          || (__payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4) ? 1 : 0)

/**************************
 * IPMI 2.0 Payload Flags *
 **************************/
#define IPMI_PAYLOAD_FLAG_UNENCRYPTED                      0x0
#define IPMI_PAYLOAD_FLAG_ENCRYPTED                        0x1
#define IPMI_PAYLOAD_FLAG_UNAUTHENTICATED                  0x0
#define IPMI_PAYLOAD_FLAG_AUTHENTICATED                    0x1

#define IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(__payload_flag) \
        (((__payload_flag) == IPMI_PAYLOAD_FLAG_UNENCRYPTED \
         || (__payload_flag) == IPMI_PAYLOAD_FLAG_ENCRYPTED) ? 1 : 0)

#define IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID(__payload_flag) \
        (((__payload_flag) == IPMI_PAYLOAD_FLAG_UNENCRYPTED \
         || (__payload_flag) == IPMI_PAYLOAD_FLAG_ENCRYPTED) ? 1 : 0)

/*********************************************
 * IPMI 2.0 Authentication Algorithm Numbers *
 *********************************************/

#define IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE           0x00
#define IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1      0x01
#define IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5       0x02
/* C0h - FFh - OEM */
/* all other reserved */

#define IPMI_AUTHENTICATION_ALGORITHM_VALID(__algorithm) \
        (((__algorithm) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE \
          || (__algorithm) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1 \
          || (__algorithm) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5) ? 1 : 0)

/****************************************
 * IPMI 2.0 Integrity Algorithm Numbers *
 ****************************************/

#define IPMI_INTEGRITY_ALGORITHM_NONE                     0x00
#define IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96             0x01
#define IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128             0x02
#define IPMI_INTEGRITY_ALGORITHM_MD5_128                  0x03
/* C0h - FFh - OEM */
/* all other reserved */

#define IPMI_INTEGRITY_ALGORITHM_VALID(__algorithm) \
        (((__algorithm) ==  IPMI_INTEGRITY_ALGORITHM_NONE \
          || (__algorithm) ==  IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96 \
          || (__algorithm) ==  IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128 \
          || (__algorithm) == IPMI_INTEGRITY_ALGORITHM_MD5_128) ? 1 : 0)

/**********************************************
 * IPMI 2.0 Confidentiality Algorithm Numbers *
 **********************************************/

#define IPMI_CONFIDENTIALITY_ALGORITHM_NONE               0x00
#define IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128        0x01
#define IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128           0x02
#define IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40            0x03
/* 30h - 3Fh - OEM */
/* all other reserved */

#define IPMI_CONFIDENTIALITY_ALGORITHM_VALID(__algorithm) \
        (((__algorithm) ==  IPMI_CONFIDENTIALITY_ALGORITHM_NONE \
          || (__algorithm) ==  IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 \
          || (__algorithm) ==  IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128 \
          || (__algorithm) == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40) ? 1 : 0)

/***************************************
 * IPMI 2.0 Misc Flags and Definitions *                       
 ***************************************/

#define IPMI_AUTHENTICATION_PAYLOAD_TYPE                  0x00
#define IPMI_AUTHENTICATION_PAYLOAD_LEN                   0x08
#define IPMI_INTEGRITY_PAYLOAD_TYPE                       0x01
#define IPMI_INTEGRITY_PAYLOAD_LEN                        0x08
#define IPMI_CONFIDENTIALITY_PAYLOAD_TYPE                 0x02
#define IPMI_CONFIDENTIALITY_PAYLOAD_LEN                  0x08

#define IPMI_USERNAME_PRIVILEGE_LOOKUP                    0x0
#define IPMI_NAMEONLY_LOOKUP                              0x1

#define IPMI_USERNAME_LOOKUP_VALID(__username_lookup_flag) \
        (((__username_lookup_flag) == IPMI_USERNAME_PRIVILEGE_LOOKUP \
         || (__username_lookup_flag) == IPMI_NAMEONLY_LOOKUP) ? 1 : 0)

#define IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LEN             16
#define IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LEN             16

#define IPMI_NEXT_HEADER                                  0x07

#define IPMI_KEY_CONSTANT_LEN                             20

#define IPMI_HMAC_SHA1_DIGEST_LEN                         20
#define IPMI_HMAC_MD5_DIGEST_LEN                          16

#define IPMI_AES_CBC_128_IV_LEN                           16
#define IPMI_AES_CBC_128_KEY_LEN                          16
#define IPMI_AES_CBC_128_BLOCK_LEN                        16

#define IPMI_HMAC_SHA1_96_AUTHCODE_LEN                    12
#define IPMI_HMAC_MD5_128_AUTHCODE_LEN                    16
#define IPMI_MD5_128_AUTHCODE_LEN                         16

#define IPMI_INTEGRITY_PAD_MULTIPLE                       4
#define IPMI_INTEGRITY_PAD_DATA                           0xFF

#define IPMI_MAX_MSG_LEN                                  65536
/* achu: b/c ipmi_msg_len is 2 bytes */
 
#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_lanplus_hdr_session;
extern fiid_template_t tmpl_lanplus_trlr_session;
extern fiid_template_t tmpl_lanplus_trlr_session_calc;

extern fiid_template_t tmpl_lanplus_payload;

extern fiid_template_t tmpl_lanplus_open_session_rq;
extern fiid_template_t tmpl_lanplus_open_session_rs;

extern fiid_template_t tmpl_lanplus_rakp_message_1;
extern fiid_template_t tmpl_lanplus_rakp_message_2;
extern fiid_template_t tmpl_lanplus_rakp_message_3;
extern fiid_template_t tmpl_lanplus_rakp_message_4;

int32_t ipmi_calculate_sik(u_int8_t authentication_algorithm, u_int8_t *key, u_int32_t key_len, u_int8_t *remote_console_random_number, u_int32_t remote_console_random_number_len, u_int8_t *managed_system_random_number, u_int32_t managed_system_random_number_len, u_int8_t requested_privilege_level, u_int8_t *username, u_int8_t username_len, u_int8_t *sik, u_int32_t sik_len);

int32_t ipmi_calculate_k1(u_int8_t authentication_algorithm, u_int8_t *sik_key, u_int32_t sik_key_len, u_int8_t *k1, u_int32_t k1_len);

int32_t ipmi_calculate_k2(u_int8_t authentication_algorithm, u_int8_t *sik_key, u_int32_t sik_key_len, u_int8_t *k2, u_int32_t k2_len);

int8_t fill_lanplus_hdr_session (fiid_template_t tmpl_session, u_int8_t auth_type, u_int8_t payload_type, u_int8_t payload_authenticated, u_int8_t payload_encrypted, u_int32_t oem_iana, u_int16_t oem_payload_id, u_int32_t session_id, u_int32_t session_seq_num, fiid_template_t tmpl_cmd, fiid_obj_t obj_hdr);

int8_t fill_lanplus_trlr_session(fiid_template_t tmpl_trlr, u_int8_t *auth_code_data, u_int32_t auth_code_data_len, fiid_obj_t obj_trlr);

int8_t fill_lanplus_payload(u_int8_t confidentiality_algorithm, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, u_int8_t *sik, u_int32_t sik_len, fiid_obj_t obj_payload);

int8_t fill_lanplus_open_session (u_int8_t message_tag, u_int8_t requested_maximum_privilege_level, u_int32_t remote_console_session_id, u_int8_t authentication_algorithm, u_int8_t integrity_algorithm, u_int8_t confidentiality_algorithm, fiid_obj_t obj_msg);

int8_t fill_lanplus_rakp_message_1(u_int8_t message_tag, u_int32_t managed_system_session_id, u_int8_t *remote_console_random_number, u_int32_t remote_console_random_number_len, u_int8_t requested_maximum_privilege_level, u_int8_t nameonly_lookup_flag, u_int8_t *username, u_int32_t username_len, fiid_obj_t obj_msg);

int32_t assemble_ipmi_lanplus_pkt (u_int8_t authentication_algorithm, u_int8_t integrity_algorithm, u_int8_t confidentiality_algorithm, u_int8_t *integrity_key, u_int32_t integrity_key_len, u_int8_t *confidentiality_key, u_int32_t confidentiality_key_len, fiid_obj_t obj_hdr_rmcp, fiid_obj_t obj_lanplus_hdr_session, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, fiid_obj_t obj_lanplus_trlr_session, fiid_template_t tmpl_trlr_session, u_int8_t *pkt, u_int32_t pkt_len);

#ifdef __cplusplus
}
#endif

#endif
