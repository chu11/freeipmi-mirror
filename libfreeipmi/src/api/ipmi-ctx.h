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

#ifndef _IPMI_CTX_H
#define _IPMI_CTX_H

#include <stdint.h>
#include <netinet/in.h>
#include <sys/param.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/driver/ipmi-sunbmc-driver.h"

#include "freeipmi/api/ipmi-api.h"

#define IPMI_MAX_SIK_KEY_LENGTH                           64
#define IPMI_MAX_INTEGRITY_KEY_LENGTH                     64
#define IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH               64
#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH  64

#define IPMI_MAX_PKT_LEN                                4096

#define IPMI_CTX_MAGIC 0xfafab0b0

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

struct ipmi_ctx
{
  uint32_t           magic;

  ipmi_driver_type_t type;
  uint32_t           workaround_flags;
  uint32_t           flags;

  uint8_t            rs_addr;	/* for ipmb */
  uint8_t            lun;
  uint8_t            net_fn;

  fiid_field_t      *tmpl_ipmb_cmd_rq;
  fiid_field_t      *tmpl_ipmb_cmd_rs;
  
  ipmi_errnum_type_t errnum;

  union
  {
    struct
    {
      ipmi_kcs_ctx_t          kcs_ctx;
      ipmi_ssif_ctx_t         ssif_ctx;
      ipmi_openipmi_ctx_t     openipmi_ctx;
      ipmi_sunbmc_ctx_t       sunbmc_ctx;

      uint8_t                 rq_seq;

      struct
      {
        fiid_obj_t       obj_hdr;
      } rq;

      struct
      {
        fiid_obj_t       obj_hdr;
      } rs;
    } inband;

    struct
    {
      int                sockfd;

      char               hostname[MAXHOSTNAMELEN+1];

      struct sockaddr_in remote_host;

      /* Configured by User for IPMI 1.5 and IPMI 2.0*/
      char               username[IPMI_MAX_USER_NAME_LENGTH+1];
      char               password[IPMI_2_0_MAX_PASSWORD_LENGTH+1];
      uint8_t            privilege_level;
      unsigned int       session_timeout;
      unsigned int       retransmission_timeout;

      /* Configured by User for IPMI 1.5 */
      uint8_t            authentication_type;

      /* Configured by User for IPMI 2.0 */
      uint8_t            k_g[IPMI_MAX_K_G_LENGTH];
      int                k_g_configured;
      uint8_t            cipher_suite_id;
      /* Data based on Configuration Parameters */
      uint8_t            authentication_algorithm;
      uint8_t            integrity_algorithm;
      uint8_t            confidentiality_algorithm;

      /* Used by IPMI 1.5 and IPMI 2.0 code */
      uint32_t           session_sequence_number;
      uint8_t            rq_seq;
      struct timeval     last_send;
      struct timeval     last_received;
      uint32_t           highest_received_sequence_number;
      uint32_t           previously_received_list;

      /* Used by IPMI 1.5 */
      uint32_t           session_id;

      /* achu: per_msg_auth is backwards for some reason
       *
       * 0 = enabled
       * 1 = disabled
       */
      int                per_msg_auth_disabled;

      /* Used by IPMI 2.0 */
      uint32_t           remote_console_session_id;
      uint32_t           managed_system_session_id; 
      uint8_t            sik_key[IPMI_MAX_SIK_KEY_LENGTH]; /* not actually needed globally */
      uint8_t            *sik_key_ptr;
      uint32_t           sik_key_len;
      uint8_t            integrity_key[IPMI_MAX_INTEGRITY_KEY_LENGTH];
      uint8_t            *integrity_key_ptr;
      uint32_t           integrity_key_len;
      uint8_t            confidentiality_key[IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH];
      uint8_t            *confidentiality_key_ptr;
      uint32_t           confidentiality_key_len;

      struct
      {
        fiid_obj_t       obj_rmcp_hdr;
        fiid_obj_t       obj_lan_session_hdr;
        fiid_obj_t       obj_rmcpplus_session_hdr;
        fiid_obj_t       obj_lan_msg_hdr;
        fiid_obj_t       obj_rmcpplus_session_trlr;
      } rq;

      struct
      {
        fiid_obj_t       obj_rmcp_hdr;
        fiid_obj_t       obj_lan_session_hdr;
        fiid_obj_t       obj_rmcpplus_session_hdr;
        fiid_obj_t       obj_lan_msg_hdr;
        fiid_obj_t       obj_rmcpplus_payload;
        fiid_obj_t       obj_lan_msg_trlr;
        fiid_obj_t       obj_rmcpplus_session_trlr;
      } rs;
    } outofband;
  } io;
};

#endif /* _IPMI_CTX_H */
