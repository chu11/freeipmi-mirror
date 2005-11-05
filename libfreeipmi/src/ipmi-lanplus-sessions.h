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

#define IPMI_PAYLOAD_TYPE_VALID(payload_type) \
        (((payload_type) == IPMI_PAYLOAD_TYPE_IPMI \
          || (payload_type) == IPMI_PAYLOAD_TYPE_SOL \
          || (payload_type) == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT \
          || (payload_type) == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST \
          || (payload_type) == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE \
          || (payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1 \
          || (payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2 \
          || (payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3 \
          || (payload_type) == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4) ? 1 : 0)

/**************************
 * IPMI 2.0 Payload Flags *
 **************************/
#define IPMI_PAYLOAD_FLAG_UNENCRYPTED                      0x0
#define IPMI_PAYLOAD_FLAG_ENCRYPTED                        0x1
#define IPMI_PAYLOAD_FLAG_UNAUTHENTICATED                  0x0
#define IPMI_PAYLOAD_FLAG_AUTHENTICATED                    0x1

#define IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_flag) \
        (((payload_flag) == IPMI_PAYLOAD_FLAG_UNENCRYPTED \
         || (payload_flag) == IPMI_PAYLOAD_FLAG_ENCRYPTED) ? 1 : 0)

#define IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID(payload_flag) \
        (((payload_flag) == IPMI_PAYLOAD_FLAG_UNENCRYPTED \
         || (payload_flag) == IPMI_PAYLOAD_FLAG_ENCRYPTED) ? 1 : 0)

/*********************************************
 * IPMI 2.0 Authentication Algorithm Numbers *
 *********************************************/

#define IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE           0x00
#define IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1      0x01
#define IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5       0x02
/* C0h - FFh - OEM */
/* all other reserved */

/****************************************
 * IPMI 2.0 Integrity Algorithm Numbers *
 ****************************************/

#define IPMI_INTEGRITY_ALGORITHM_NONE                     0x00
#define IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96             0x01
#define IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128             0x02
#define IPMI_INTEGRITY_ALGORITHM_MD5_128                  0x03
/* C0h - FFh - OEM */
/* all other reserved */

/**********************************************
 * IPMI 2.0 Confidentiality Algorithm Numbers *
 **********************************************/

#define IPMI_CONFIDENTIALITY_ALGORITHM_NONE               0x00
#define IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128        0x01
#define IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128           0x02
#define IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40            0x03
/* 30h - 3Fh - OEM */
/* all other reserved */

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_hdr_session_v20;
extern fiid_template_t tmpl_trlr_session_v20;
extern fiid_template_t tmpl_open_session_rq;
extern fiid_template_t tmpl_open_session_rs;

int8_t rmcpplus_status_strerror_r(u_int8_t rmcpplus_status_code, 
                                  char *errstr, 
                                  size_t len);

#ifdef __cplusplus
}
#endif


#endif
