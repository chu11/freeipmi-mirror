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

#ifndef IPMI_API_DEFS_H
#define IPMI_API_DEFS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <netinet/in.h>
#include <sys/param.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif  /* !TIME_WITH_SYS_TIME */
#include <limits.h>             /* MAXHOSTNAMELEN */
#ifdef HAVE_NETDB_H
#include <netdb.h>              /* MAXHOSTNAMELEN Solaris */
#endif /* HAVE_NETDB_H */

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/driver/ipmi-inteldcmi-driver.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/driver/ipmi-sunbmc-driver.h"
#include "freeipmi/locate/ipmi-locate.h"

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

struct ipmi_ctx_target
{
  uint8_t channel_number;	/* for ipmb */
  int channel_number_is_set;	/* for ipmb */
  uint8_t rs_addr;		/* for ipmb */
  int rs_addr_is_set;		/* for ipmb */
  uint8_t lun;
  uint8_t net_fn;
};

struct ipmi_ctx
{
  uint32_t magic;
  
  ipmi_driver_type_t type;
  unsigned int workaround_flags_outofband;
  unsigned int workaround_flags_outofband_2_0;
  unsigned int workaround_flags_inband;
  unsigned int flags;
  
  struct ipmi_ctx_target target;
  
  fiid_field_t      *tmpl_ipmb_cmd_rq;
  fiid_field_t      *tmpl_ipmb_cmd_rs;

  ipmi_errnum_type_t errnum;

  union
  {
    struct
    {
      ipmi_kcs_ctx_t kcs_ctx;
      ipmi_ssif_ctx_t ssif_ctx;
      ipmi_openipmi_ctx_t openipmi_ctx;
      ipmi_sunbmc_ctx_t sunbmc_ctx;
      ipmi_inteldcmi_ctx_t inteldcmi_ctx;

      uint8_t rq_seq;

      struct
      {
        fiid_obj_t obj_hdr;
      } rq;

      struct
      {
        fiid_obj_t obj_hdr;
      } rs;
    } inband;

    struct
    {
      int sockfd;

      char hostname[MAXHOSTNAMELEN+1];

      struct sockaddr_in remote_host;

      /* Configured by User for IPMI 1.5 and IPMI 2.0*/
      char username[IPMI_MAX_USER_NAME_LENGTH+1];
      char password[IPMI_2_0_MAX_PASSWORD_LENGTH+1];
      uint8_t privilege_level;
      unsigned int session_timeout;
      unsigned int retransmission_timeout;

      /* Configured by User for IPMI 1.5 */
      uint8_t authentication_type;

      /* Configured by User for IPMI 2.0 */
      uint8_t k_g[IPMI_MAX_K_G_LENGTH];
      int k_g_configured;
      uint8_t cipher_suite_id;
      /* Data based on Configuration Parameters */
      uint8_t authentication_algorithm;
      uint8_t integrity_algorithm;
      uint8_t confidentiality_algorithm;

      /* Used by IPMI 1.5 and IPMI 2.0 code */
      uint32_t session_sequence_number;
      uint8_t rq_seq;
      struct timeval last_send;
      struct timeval last_received;
      uint32_t highest_received_sequence_number;
      uint32_t previously_received_list;

      /* Used by IPMI 1.5 */
      uint32_t session_id;

      /* achu: per_msg_auth is backwards for some reason
       *
       * 0 = enabled
       * 1 = disabled
       */
      int per_msg_auth_disabled;

      /* Used by IPMI 2.0 */
      uint32_t remote_console_session_id;
      uint32_t managed_system_session_id;
      uint8_t sik_key[IPMI_MAX_SIK_KEY_LENGTH];            /* not actually needed globally */
      void *sik_key_ptr;
      unsigned int sik_key_len;
      uint8_t integrity_key[IPMI_MAX_INTEGRITY_KEY_LENGTH];
      void *integrity_key_ptr;
      unsigned int integrity_key_len;
      uint8_t confidentiality_key[IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH];
      void *confidentiality_key_ptr;
      unsigned int confidentiality_key_len;

      struct
      {
        fiid_obj_t obj_rmcp_hdr;
        fiid_obj_t obj_lan_session_hdr;
        fiid_obj_t obj_rmcpplus_session_hdr;
        fiid_obj_t obj_lan_msg_hdr;
        fiid_obj_t obj_rmcpplus_session_trlr;
      } rq;

      struct
      {
        fiid_obj_t obj_rmcp_hdr;
        fiid_obj_t obj_lan_session_hdr;
        fiid_obj_t obj_rmcpplus_session_hdr;
        fiid_obj_t obj_lan_msg_hdr;
        fiid_obj_t obj_rmcpplus_payload;
        fiid_obj_t obj_lan_msg_trlr;
        fiid_obj_t obj_rmcpplus_session_trlr;
      } rs;
    } outofband;
  } io;
};

#endif /* IPMI_API_DEFS_H */
