/*****************************************************************************\
 *  $Id: ipmiconsole_defs.h,v 1.7.2.1 2007-05-14 02:41:14 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *  
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmiconsole is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmiconsole is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#ifdef WITH_PTHREADS
#include <pthread.h>
#endif /* WITH_PTHREADS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif	/* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <sys/param.h>
#include <netinet/in.h>
#include <freeipmi/freeipmi.h>

#include "cbuf.h"

#ifndef _IPMICONSOLE_DEFS_H
#define _IPMICONSOLE_DEFS_H

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

typedef enum
  {
    IPMICONSOLE_PROTOCOL_STATE_START                                    = 0x00,
    IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_V20_SENT = 0x01,
    IPMICONSOLE_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT                = 0x02,
    IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT                      = 0x03,
    IPMICONSOLE_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT                      = 0x04,
    IPMICONSOLE_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT         = 0x05,
    IPMICONSOLE_PROTOCOL_STATE_GET_CHANNEL_PAYLOAD_SUPPORT_SENT         = 0x06,
    IPMICONSOLE_PROTOCOL_STATE_GET_PAYLOAD_ACTIVATION_STATUS_SENT       = 0x07,
    IPMICONSOLE_PROTOCOL_STATE_ACTIVATE_PAYLOAD_SENT                    = 0x08,
    IPMICONSOLE_PROTOCOL_STATE_SOL_SESSION                              = 0x09,
    IPMICONSOLE_PROTOCOL_STATE_DEACTIVATE_PAYLOAD_SENT                  = 0x0A,
    IPMICONSOLE_PROTOCOL_STATE_CLOSE_SESSION_SENT                       = 0x0B,
    IPMICONSOLE_PROTOCOL_STATE_END                                      = 0x0C,
  } ipmiconsole_protocol_state_t;

/* Note: Get Channel Payload Version will act as our "ping"
 * to keep the session state alive.
 */
typedef enum
  {
    IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RQ = 0x00,
    IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS = 0x01,
    IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST                   = 0x02,
    IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE                  = 0x03,
    IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1                         = 0x04,
    IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2                         = 0x05,
    IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3                         = 0x06,
    IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4                         = 0x07,
    IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ         = 0x08,
    IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS         = 0x09,
    IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ         = 0x0A,
    IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS         = 0x0B,
    IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ       = 0x0C,
    IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS       = 0x0D,
    IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ                    = 0x0E,
    IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS                    = 0x0F,
    IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ                    = 0x10,
    IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS                    = 0x11,
    IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ         = 0x12,
    IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS         = 0x13,
    IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ                  = 0x14,
    IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS                  = 0x15,
    IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ                       = 0x16,
    IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS                       = 0x17,
  } ipmiconsole_packet_type_t;

#define IPMICONSOLE_PACKET_TYPE_REQUEST(__p) \
   (((__p) == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST \
     || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1 \
     || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3 \
     || (__p) == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ \
     || (__p) == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) ? 1 : 0)

#define IPMICONSOLE_PACKET_TYPE_RESPONSE(__p) \
   (((__p) == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_V20_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE \
     || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2 \
     || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4 \
     || (__p) == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS \
     || (__p) == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS) ? 1 : 0)

#define IPMICONSOLE_PACKET_TYPE_VALID(__p) \
   ((IPMICONSOLE_PACKET_TYPE_REQUEST(__p) \
     || IPMICONSOLE_PACKET_TYPE_RESPONSE(__p)) ? 1 : 0)

#define IPMICONSOLE_SESSION_TIMEOUT_LENGTH_DEFAULT                  60000
#define IPMICONSOLE_RETRANSMISSION_TIMEOUT_LENGTH_DEFAULT           500
#define IPMICONSOLE_RETRANSMISSION_MAX_DEFAULT                      10
#define IPMICONSOLE_RETRANSMISSION_BACKOFF_COUNT_DEFAULT            2
#define IPMICONSOLE_KEEPALIVE_TIMEOUT_LENGTH_DEFAULT                20000
#define IPMICONSOLE_RETRANSMISSION_KEEPALIVE_TIMEOUT_LENGTH_DEFAULT 5000
#define IPMICONSOLE_ACCEPTABLE_PACKET_ERRORS_COUNT_DEFAULT          16
#define IPMICONSOLE_MAXIMUM_RETRANSMISSION_COUNT_DEFAULT            16
#define IPMI_PRIVILEGE_LEVEL_DEFAULT                                IPMI_PRIVILEGE_LEVEL_ADMIN
#define IPMI_CIPHER_SUITE_ID_DEFAULT                                3
#define IPMI_PAYLOAD_INSTANCE_DEFAULT                               1

#define CONSOLE_REMOTE_CONSOLE_TO_BMC_BUF_MIN                 (1024*2)
#define CONSOLE_REMOTE_CONSOLE_TO_BMC_BUF_MAX                 (1024*8)

#define CONSOLE_BMC_TO_REMOTE_CONSOLE_BUF_MIN                 (1024*4)
#define CONSOLE_BMC_TO_REMOTE_CONSOLE_BUF_MAX                 (1024*16)

#define IPMI_FROM_BMC_BUF_MIN                                 (1024*4)
#define IPMI_FROM_BMC_BUF_MAX                                 (1024*16)

#define IPMI_TO_BMC_BUF_MIN                                   (1024*2)
#define IPMI_TO_BMC_BUF_MAX                                   (1024*8)

/* achu: See IPMI 2.0 spec Section 24.4, Table 24-6.  The Get Payload
 * Activation Status Command indicates a maximum number of 16
 * instances are possible.
 */
#define IPMI_INSTANCES_ACTIVATED_LENGTH                       16

#define IPMI_MAX_SIK_KEY_LENGTH                          64
#define IPMI_MAX_INTEGRITY_KEY_LENGTH                    64
#define IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH              64
#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH 64
 
#define IPMI_SESSION_SEQUENCE_NUMBER_WINDOW                        16

#define IPMI_SESSION_SEQUENCE_NUMBER_PREVIOUSLY_RECEIVED_LIST_INIT 0xFFFF;

#define IPMI_SESSION_INITIAL_OUTBOUND_SEQUENCE_NUMBER              1
#define IPMI_SOL_SESSION_INITIAL_PACKET_SEQUENCE_NUMBER            1

#define IPMICONSOLE_CTX_MAGIC                 0x74AB8831

#define IPMICONSOLE_PACKET_BUFLEN             16384

#define IPMICONSOLE_MIN_CHARACTER_DATA        1
#define IPMICONSOLE_MAX_CHARACTER_DATA        255

#define IPMICONSOLE_PIPE_GENERATE_BREAK_CODE  0x01

#define IPMICONSOLE_DEBUG_MASK \
        (IPMICONSOLE_DEBUG_STDOUT \
         | IPMICONSOLE_DEBUG_STDERR \
         | IPMICONSOLE_DEBUG_SYSLOG \
         | IPMICONSOLE_DEBUG_FILE \
         | IPMICONSOLE_DEBUG_IPMI_PACKETS)

#define IPMICONSOLE_SECURITY_MASK \
        (IPMICONSOLE_SECURITY_ERROR_ON_SOL_INUSE \
         | IPMICONSOLE_SECURITY_LOCK_MEMORY \
         | IPMICONSOLE_SECURITY_DEACTIVATE_ONLY)

#define IPMICONSOLE_WORKAROUND_MASK \
        (IPMICONSOLE_WORKAROUND_INTEL_2_0 \
	 | IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0)

#define IPMICONSOLE_ENGINECOMM_FLAGS_SOL_ESTABLISHED   0x1

#define IPMICONSOLE_ENGINECOMM_SOL_SESSION_ESTABLISHED 0x1
#define IPMICONSOLE_ENGINECOMM_SOL_SESSION_ERROR       0x2
#define IPMICONSOLE_ENGINECOMM_SOL_SESSION_DEACTIVATED 0x3

struct ipmiconsole_ctx_session {

  /* File Descriptor User Interface */
  int user_fd;                  /* never touched by this library */
  int ipmiconsole_fd;
  cbuf_t console_remote_console_to_bmc;
  cbuf_t console_bmc_to_remote_console;

  /* Connection Data */
  int ipmi_fd;
  int16_t console_port;
  cbuf_t ipmi_from_bmc;
  cbuf_t ipmi_to_bmc;
  struct timeval last_ipmi_packet_sent;
  struct timeval last_ipmi_packet_received;

  /* Pipe for non-fd communication: from API to engine */
  /* Note for future: Protect w/ mutex is you add opportunities to do async comm */
  int asynccomm[2];

  /* Data based on Configuration Parameters */
  uint8_t authentication_algorithm;
  uint8_t integrity_algorithm;
  uint8_t confidentiality_algorithm;

  /* Fiid Objects */

  fiid_obj_t obj_rmcp_hdr_rq;
  fiid_obj_t obj_rmcp_hdr_rs;
  fiid_obj_t obj_lan_session_hdr_rq;
  fiid_obj_t obj_lan_session_hdr_rs;
  fiid_obj_t obj_rmcpplus_session_hdr_rq;
  fiid_obj_t obj_rmcpplus_session_hdr_rs;
  /* fiid_obj_t obj_rmcpplus_payload_rq; */
  fiid_obj_t obj_rmcpplus_payload_rs;
  fiid_obj_t obj_lan_msg_hdr_rq;
  fiid_obj_t obj_lan_msg_hdr_rs;

  /* fiid_obj_t obj_lan_msg_trlr_rq; */
  fiid_obj_t obj_lan_msg_trlr_rs;
  fiid_obj_t obj_rmcpplus_session_trlr_rq;
  fiid_obj_t obj_rmcpplus_session_trlr_rs;

  fiid_obj_t obj_authentication_capabilities_v20_rq;
  fiid_obj_t obj_authentication_capabilities_v20_rs;
  fiid_obj_t obj_open_session_request;
  fiid_obj_t obj_open_session_response;
  fiid_obj_t obj_rakp_message_1;
  fiid_obj_t obj_rakp_message_2;
  fiid_obj_t obj_rakp_message_3;
  fiid_obj_t obj_rakp_message_4;
  fiid_obj_t obj_set_session_privilege_level_rq;
  fiid_obj_t obj_set_session_privilege_level_rs;
  fiid_obj_t obj_get_channel_payload_support_rq;
  fiid_obj_t obj_get_channel_payload_support_rs;
  fiid_obj_t obj_get_payload_activation_status_rq;
  fiid_obj_t obj_get_payload_activation_status_rs;
  fiid_obj_t obj_activate_payload_rq;
  fiid_obj_t obj_activate_payload_rs;
  fiid_obj_t obj_sol_payload_data_rq;
  fiid_obj_t obj_sol_payload_data_rs;
  fiid_obj_t obj_get_channel_payload_version_rq;
  fiid_obj_t obj_get_channel_payload_version_rs;
  fiid_obj_t obj_deactivate_payload_rq;
  fiid_obj_t obj_deactivate_payload_rs;
  fiid_obj_t obj_close_session_rq;
  fiid_obj_t obj_close_session_rs;

  /* 
   * Session Data
   *
   * These will need to be re-initialized if the session is being reattempted under
   * a different port  if the activate
   * The case
   */
  struct sockaddr_in addr;

  /*
   * Protocol State Machine Variables
   */
  ipmiconsole_protocol_state_t protocol_state;
  int close_session_flag;
  int try_new_port_flag;
  int deactivate_payload_instances_and_try_again_flag;
  int close_timeout_flag;
  int deactivate_only_succeeded_flag;

  /*
   * Protocol Maintenance Variables
   */
  uint32_t retransmission_count;
  uint32_t errors_count;
  uint32_t session_sequence_number_errors_count;
  uint32_t deactivate_active_payloads_count;
  uint32_t highest_received_sequence_number;
  /* need to also store bytes read from a previous seq num */
  unsigned int previously_received_list;

  uint8_t message_tag;
  uint8_t requester_sequence_number;
  uint32_t session_sequence_number;
  uint8_t name_only_lookup;
  uint32_t remote_console_session_id;
  uint8_t remote_console_random_number[IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH];

  uint8_t sik_key[IPMI_MAX_SIK_KEY_LENGTH];
  uint8_t *sik_key_ptr;
  uint32_t sik_key_len;
  uint8_t integrity_key[IPMI_MAX_INTEGRITY_KEY_LENGTH];
  uint8_t *integrity_key_ptr;
  uint32_t integrity_key_len;
  uint8_t confidentiality_key[IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH];
  uint8_t *confidentiality_key_ptr;
  uint32_t confidentiality_key_len;

  uint8_t sol_payload_instance;
  uint32_t sol_instance_capacity;
  uint8_t sol_instances_activated[IPMI_INSTANCES_ACTIVATED_LENGTH];
  uint32_t sol_instances_activated_count;
  uint32_t sol_instances_deactivated_count;

  /* XXX  Do we need to store the following 2?  I think it's questionable - come back later */
  uint16_t max_inbound_payload_size; /* determined during session setup */
  uint16_t max_outbound_payload_size; /* determine during session setup */
  uint8_t max_sol_character_send_size; /* determine during session setup */

  /* SOL Session Maintenance */
  struct timeval last_keepalive_packet_sent;

  /* Serial Break Maintenance */
  int break_requested;
  unsigned int console_remote_console_to_bmc_bytes_before_break;

  /* SOL Input (remote console to BMC) */
  int sol_input_waiting_for_ack;
  int sol_input_waiting_for_break_ack;
  struct timeval last_sol_input_packet_sent;
  uint8_t sol_input_packet_sequence_number;
  uint8_t sol_input_character_data[IPMICONSOLE_MAX_CHARACTER_DATA+1];
  uint32_t sol_input_character_data_len;

  /* SOL Output (BMC to remote console) */
  uint8_t last_sol_output_packet_sequence_number;
  uint8_t last_sol_output_accepted_character_count;
};

struct ipmiconsole_ctx {
  uint32_t magic;
  int errnum;

  /* Configuration Parameters */
  char hostname[MAXHOSTNAMELEN+1];
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1];
  uint8_t password[IPMI_2_0_MAX_PASSWORD_LENGTH+1];
  uint8_t k_g[IPMI_MAX_K_G_LENGTH];
  uint8_t k_g_configured;
  uint8_t privilege_level;
  uint8_t cipher_suite_id;

  unsigned int session_timeout_len;
  unsigned int retransmission_timeout_len;
  unsigned int retransmission_backoff_count;
  unsigned int keepalive_timeout_len;
  unsigned int retransmission_keepalive_timeout_len;
  unsigned int acceptable_packet_errors_count;
  unsigned int maximum_retransmission_count;
  uint32_t debug_flags;
  uint32_t security_flags;
  uint32_t workaround_flags;

  /* Pipe for non-fd communication: from Engine to API */
  int enginecomm[2];
  uint32_t enginecomm_flags;
  int sol_session_established;

  /* Debug Data */
  int debug_fd;

  pthread_mutex_t session_submitted_mutex;
  unsigned int session_submitted;
  struct ipmiconsole_ctx_session session; 
};

#endif /* _IPMICONSOLE_DEFS_H */
