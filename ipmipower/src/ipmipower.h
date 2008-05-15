/*****************************************************************************\
 *  $Id: ipmipower.h,v 1.103 2008-05-15 18:09:50 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMIPOWER_H
#define _IPMIPOWER_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <sys/param.h>
#include <netinet/in.h>

#include <freeipmi/freeipmi.h>

#include "hostlist.h"
#include "cbuf.h"
#include "list.h"

#include "ipmidetect.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

/* 
 * ipmipower limits 
 */
 
#define IPMIPOWER_MIN_TTY_BUF                       1024*4
#define IPMIPOWER_MAX_TTY_BUF                       1024*32

#define IPMIPOWER_MIN_CONNECTION_BUF                1024*2
#define IPMIPOWER_MAX_CONNECTION_BUF                1024*4

#define IPMIPOWER_MINNODES                          1
#define IPMIPOWER_MAXNODES                          1024

#define IPMIPOWER_SESSION_TIMEOUT_MIN               1000   /* 1 second */
#define IPMIPOWER_SESSION_TIMEOUT_MAX               120000 /* 120 seconds */

#define IPMIPOWER_RETRANSMISSION_TIMEOUT_MIN        50     /* .05 seconds */
#define IPMIPOWER_RETRANSMISSION_TIMEOUT_MAX        IPMIPOWER_SESSION_TIMEOUT_MAX

#define IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT_MIN   50     /* .05 seconds */
#define IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT_MAX   IPMIPOWER_SESSION_TIMEOUT_MAX

#define IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT_MIN  1
#define IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT_MAX  200

#define IPMIPOWER_PING_INTERVAL_MIN                 250   /* .25 seconds */
#define IPMIPOWER_PING_INTERVAL_MAX                 IPMIPOWER_SESSION_TIMEOUT_MAX

#define IPMIPOWER_PING_TIMEOUT_MIN                  IPMIPOWER_SESSION_TIMEOUT_MIN
#define IPMIPOWER_PING_TIMEOUT_MAX                  IPMIPOWER_SESSION_TIMEOUT_MAX

#define IPMIPOWER_PING_PACKET_COUNT_MIN             2
#define IPMIPOWER_PING_PACKET_COUNT_MAX             20

#define IPMIPOWER_PING_PERCENT_MIN                  1
#define IPMIPOWER_PING_PERCENT_MAX                  100

#define IPMIPOWER_PING_CONSEC_COUNT_MIN             2
#define IPMIPOWER_PING_CONSEC_COUNT_MAX             20

/* 
 * ipmi specifics for ipmipower
 */

/* IPMI allowable sequence number range window
 */

#define IPMIPOWER_SEQUENCE_NUMBER_WINDOW 8
#define IPMIPOWER_MAX_SEQUENCE_NUMBER    0xFFFFFFFF

#define IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER       1
#define IPMIPOWER_RMCPPLUS_INITIAL_OUTBOUND_SEQUENCE_NUMBER  0

/* MISC */

#define IPMI_PACKET_BUFLEN               1024
#define RMCP_PACKET_BUFLEN               1024

#define IPMIPOWER_HOSTLIST_BUFLEN        65536

#define IPMIPOWER_DEFAULT_LOGFILE        "/tmp/ipmipower.%d"

#define IPMI_MAX_SIK_KEY_LENGTH             64

#define IPMI_MAX_INTEGRITY_KEY_LENGTH       64

#define IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH 64

#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH 64

/* power_cmd_t 
 * - power control commands 
 */
typedef enum 
  { 
    POWER_CMD_NONE             = 0x00,
    POWER_CMD_POWER_OFF        = 0x01,
    POWER_CMD_POWER_ON         = 0x02,
    POWER_CMD_POWER_CYCLE      = 0x03,
    POWER_CMD_POWER_RESET      = 0x04,
    POWER_CMD_POWER_STATUS     = 0x05,
    POWER_CMD_PULSE_DIAG_INTR  = 0x06, 
    POWER_CMD_SOFT_SHUTDOWN_OS = 0x07,
  } power_cmd_t;

#define POWER_CMD_VALID(__c) \
  ((__c) > POWER_CMD_NONE && \
   (__c) <= POWER_CMD_SOFT_SHUTDOWN_OS)

#define POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE_LEVEL(__c) \
  ((__c) == POWER_CMD_POWER_OFF \
   || (__c) == POWER_CMD_POWER_ON \
   || (__c) == POWER_CMD_POWER_CYCLE \
   || (__c) == POWER_CMD_POWER_RESET \
   || (__c) == POWER_CMD_PULSE_DIAG_INTR \
   || (__c) == POWER_CMD_SOFT_SHUTDOWN_OS)

/* packet_type_t
 * - packet types stored internally in an ipmipower_powercmd structure.
 * - Request types are *_REQ, Response types are *_RES
 */
typedef enum 
  { 
    AUTHENTICATION_CAPABILITIES_V20_REQ = 0x101, 
    AUTHENTICATION_CAPABILITIES_V20_RES = 0x201,
    AUTHENTICATION_CAPABILITIES_REQ     = 0x102, 
    AUTHENTICATION_CAPABILITIES_RES     = 0x202,
    GET_SESSION_CHALLENGE_REQ           = 0x103, 
    GET_SESSION_CHALLENGE_RES           = 0x203,
    ACTIVATE_SESSION_REQ                = 0x104, 
    ACTIVATE_SESSION_RES                = 0x204,
    OPEN_SESSION_REQ                    = 0x105,
    OPEN_SESSION_RES                    = 0x205,
    RAKP_MESSAGE_1_REQ                  = 0x106,
    RAKP_MESSAGE_2_RES                  = 0x206,
    RAKP_MESSAGE_3_REQ                  = 0x107,
    RAKP_MESSAGE_4_RES                  = 0x207,
    SET_SESSION_PRIVILEGE_LEVEL_REQ     = 0x108, 
    SET_SESSION_PRIVILEGE_LEVEL_RES     = 0x208, 
    GET_CHASSIS_STATUS_REQ              = 0x109, 
    GET_CHASSIS_STATUS_RES              = 0x209,
    CHASSIS_CONTROL_REQ                 = 0x10A, 
    CHASSIS_CONTROL_RES                 = 0x20A, 
    CLOSE_SESSION_REQ                   = 0x10B, 
    CLOSE_SESSION_RES                   = 0x20B,
  } packet_type_t;

#define PACKET_TYPE_REQ_MASK           0x100
#define PACKET_TYPE_RES_MASK           0x200
#define PACKET_TYPE_PKT_MASK           0x0FF

#define PACKET_TYPE_VALID_REQ(__p) \
  (((__p) & PACKET_TYPE_REQ_MASK) && \
   ((__p) & PACKET_TYPE_PKT_MASK))

#define PACKET_TYPE_VALID_RES(__p) \
  (((__p) & PACKET_TYPE_RES_MASK) && \
   ((__p) & PACKET_TYPE_PKT_MASK))

#define PACKET_TYPE_VALID_PKT(__p) \
  (PACKET_TYPE_VALID_REQ(__p) || \
   PACKET_TYPE_VALID_RES(__p))

/* Protocol States */
typedef enum 
  { 
    PROTOCOL_STATE_START                                = 0x00,
    PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_V20_SENT = 0x01,
    PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT     = 0x02,
    PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT           = 0x03,
    PROTOCOL_STATE_ACTIVATE_SESSION_SENT                = 0x04,
    PROTOCOL_STATE_OPEN_SESSION_SENT                    = 0x05,
    PROTOCOL_STATE_RAKP_MESSAGE_1_SENT                  = 0x06,
    PROTOCOL_STATE_RAKP_MESSAGE_3_SENT                  = 0x07,
    PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT     = 0x08,
    PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT              = 0x09,
    PROTOCOL_STATE_CHASSIS_CONTROL_SENT                 = 0x0A,
    PROTOCOL_STATE_CLOSE_SESSION_SENT                   = 0x0B,
    PROTOCOL_STATE_END                                  = 0x0C,
  } protocol_state_t;

#define PROTOCOL_STATE_VALID(__s) \
  ((__s) >= PROTOCOL_STATE_START && \
   (__s) <= PROTOCOL_STATE_END)

/* Discovery States */
typedef enum 
  { 
    STATE_DISCOVERED     = 0x01,
    STATE_UNDISCOVERED   = 0x02,
    STATE_BADCONNECTION  = 0x03,
  } discover_state_t;

#define DISCOVER_STATE_VALID(__s) \
  ((__s) >= STATE_DISCOVERED && \
   (__s) <= STATE_BADCONNECTION)

/* Link States */
typedef enum 
  {
    LINK_GOOD = 0x01,
    LINK_BAD  = 0x02,
  } link_state_t;

#define LINK_STATE_VALID(__s) \
  ((__s) >= LINK_GOOD && \
   (__s) <= LINK_BAD)

/* Cipher_Suite Ids */
typedef enum 
  { 
    CIPHER_SUITE_ID_INVALID               = 0x00,
    CIPHER_SUITE_ID_0                     = 0x01,
    CIPHER_SUITE_ID_1                     = 0x02,
    CIPHER_SUITE_ID_2                     = 0x03,
    CIPHER_SUITE_ID_3                     = 0x04,
    /* xRC4 CIPHER_SUITE_ID_4                     = 0x05, */
    /* xRC4 CIPHER_SUITE_ID_5                     = 0x06, */
    CIPHER_SUITE_ID_6                     = 0x07,
    CIPHER_SUITE_ID_7                     = 0x08,
    CIPHER_SUITE_ID_8                     = 0x09,
    /* xRC4 CIPHER_SUITE_ID_9                     = 0x0A, */
    /* xRC4 CIPHER_SUITE_ID_10                    = 0x0B, */
    CIPHER_SUITE_ID_11                    = 0x0C,
    CIPHER_SUITE_ID_12                    = 0x0D,
    /* xRC4 CIPHER_SUITE_ID_13                    = 0x0E, */
    /* xRC4 CIPHER_SUITE_ID_14                    = 0x0F, */
  } cipher_suite_id_t;

#define CIPHER_SUITE_ID_VALID(__c) \
  ((__c) == CIPHER_SUITE_ID_0 \
    || (__c) == CIPHER_SUITE_ID_1 \
    || (__c) == CIPHER_SUITE_ID_2 \
    || (__c) == CIPHER_SUITE_ID_3 \
    || (__c) == CIPHER_SUITE_ID_6 \
    || (__c) == CIPHER_SUITE_ID_7 \
    || (__c) == CIPHER_SUITE_ID_8 \
    || (__c) == CIPHER_SUITE_ID_11 \
    || (__c) == CIPHER_SUITE_ID_12)

/* Msg Types */
typedef enum 
  { 
    MSG_TYPE_SUCCESS                            =  0,
    MSG_TYPE_ON                                 =  1,
    MSG_TYPE_OFF                                =  2,
    MSG_TYPE_OK                                 =  3,
    MSG_TYPE_PERMISSION                         =  4,
    MSG_TYPE_USERNAME_INVALID                   =  5,
    MSG_TYPE_PASSWORD_INVALID                   =  6,
    MSG_TYPE_PASSWORD_LENGTH_INVALID            =  7,
    MSG_TYPE_K_G_INVALID                        =  8,
    MSG_TYPE_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED =  9,
    MSG_TYPE_OPERATION_INVALID                  = 10,
    MSG_TYPE_AUTHENTICATION_TYPE_UNAVAILABLE    = 11,
    MSG_TYPE_CIPHER_SUITE_ID_UNAVAILABLE        = 12,
    MSG_TYPE_PASSWORD_VERIFICATION_TIMEOUT      = 13,
    MSG_TYPE_SESSION_TIMEOUT                    = 14,
    MSG_TYPE_NOTDISCOVERED                      = 15,
    MSG_TYPE_BADCONNECTION                      = 16,
    MSG_TYPE_UNKNOWNNODE                        = 17,
    MSG_TYPE_RESOURCES                          = 18,
    MSG_TYPE_IPMI_2_0_UNAVAILABLE               = 19,
    MSG_TYPE_BMC_BUSY                           = 20,
    MSG_TYPE_BMC_ERROR                          = 21,
  } msg_type_t;

#define MSG_TYPE_VALID(__m) \
  ((__m) >= MSG_TYPE_SUCCESS && \
   (__m) <= MSG_TYPE_BMC_ERROR)

#define MSG_TYPE_NUM_ENTRIES (MSG_TYPE_BMC_ERROR+1)

/* Workaround Flags */
typedef enum 
  {
    WORKAROUND_FLAG_ACCEPT_SESSION_ID_ZERO      = 0x00000001,
    WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION = 0x00000002,
    WORKAROUND_FLAG_CHECK_UNEXPECTED_AUTHCODE   = 0x00000004,
    WORKAROUND_FLAG_BIG_ENDIAN_SEQUENCE_NUMBER  = 0x00000008,
    WORKAROUND_FLAG_AUTHENTICATION_CAPABILITIES = 0x00000010,
    WORKAROUND_FLAG_INTEL_2_0_SESSION           = 0x00010000,
    WORKAROUND_FLAG_SUPERMICRO_2_0_SESSION      = 0x00020000,
    WORKAROUND_FLAG_SUN_2_0_SESSION             = 0x00040000,
  } workaround_flag_t;

/* ipmipower_powercmd
 * - Stores all information needed to execute a power command
 */
struct ipmipower_powercmd {
  power_cmd_t cmd;
  protocol_state_t protocol_state; 

  /* 
   * Protocol State Machine Variables 
   */
  struct timeval time_begin;
  unsigned int retransmission_count;
  uint8_t close_timeout;

  /*
   * Protocol Maintenance Variables 
   */
  unsigned int session_inbound_count;
  uint32_t highest_received_sequence_number;
  unsigned int previously_received_list;

  /* IPMI 1.5 specific */
  int permsgauth_enabled;
  uint8_t authentication_type;

  /* IPMI 2.0 specific */
  uint8_t cipher_suite_id;
  uint8_t requested_maximum_privilege_level;
  uint8_t authentication_algorithm;
  uint8_t integrity_algorithm;
  uint8_t confidentiality_algorithm;
  uint8_t sik_key[IPMI_MAX_SIK_KEY_LENGTH];
  uint8_t *sik_key_ptr;
  uint32_t sik_key_len;
  uint8_t integrity_key[IPMI_MAX_INTEGRITY_KEY_LENGTH];
  uint8_t *integrity_key_ptr;
  uint32_t integrity_key_len;
  uint8_t confidentiality_key[IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH];
  uint8_t *confidentiality_key_ptr;
  uint32_t confidentiality_key_len;
  uint8_t initial_message_tag;
  uint8_t message_tag_count;
  uint32_t session_sequence_number;
  uint8_t name_only_lookup;
  uint32_t remote_console_session_id;  
  uint8_t remote_console_random_number[IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH];
  int wait_until_on_state;
  int wait_until_off_state;

  struct ipmipower_connection *ic;
  
  fiid_obj_t obj_rmcp_hdr_req;
  fiid_obj_t obj_rmcp_hdr_res;
  fiid_obj_t obj_lan_session_hdr_req;
  fiid_obj_t obj_lan_session_hdr_res;
  fiid_obj_t obj_lan_msg_hdr_req;
  fiid_obj_t obj_lan_msg_hdr_res;
  fiid_obj_t obj_lan_msg_trlr_res;
  fiid_obj_t obj_rmcpplus_session_hdr_req;
  fiid_obj_t obj_rmcpplus_session_hdr_res;
  fiid_obj_t obj_rmcpplus_payload_res;
  fiid_obj_t obj_rmcpplus_session_trlr_req;
  fiid_obj_t obj_rmcpplus_session_trlr_res;

  fiid_obj_t obj_authentication_capabilities_v20_req;
  fiid_obj_t obj_authentication_capabilities_v20_res;
  fiid_obj_t obj_authentication_capabilities_req;
  fiid_obj_t obj_authentication_capabilities_res;
  fiid_obj_t obj_get_session_challenge_req;
  fiid_obj_t obj_get_session_challenge_res;
  fiid_obj_t obj_activate_session_req;
  fiid_obj_t obj_activate_session_res;
  fiid_obj_t obj_open_session_req;
  fiid_obj_t obj_open_session_res;
  fiid_obj_t obj_rakp_message_1_req;
  fiid_obj_t obj_rakp_message_2_res;
  fiid_obj_t obj_rakp_message_3_req;
  fiid_obj_t obj_rakp_message_4_res;
  fiid_obj_t obj_set_session_privilege_level_req;
  fiid_obj_t obj_set_session_privilege_level_res;
  fiid_obj_t obj_get_chassis_status_req;
  fiid_obj_t obj_get_chassis_status_res;
  fiid_obj_t obj_chassis_control_req;
  fiid_obj_t obj_chassis_control_res;
  fiid_obj_t obj_close_session_req;
  fiid_obj_t obj_close_session_res;

  List sockets_to_close;
};

/* ipmipower_connection
 * - Stores various information and data for each remote node ipmi
 * "connection" we have.
 */ 
struct ipmipower_connection 
{
  int ipmi_fd;
  int ping_fd;
  cbuf_t ipmi_in;
  cbuf_t ipmi_out;
  cbuf_t ping_in;
  cbuf_t ping_out;
  uint32_t ipmi_requester_sequence_number_counter;
  uint32_t ping_sequence_number_counter;
  struct timeval last_ipmi_send;
  struct timeval last_ping_send;
  struct timeval last_ipmi_recv;
  struct timeval last_ping_recv;
  
  link_state_t link_state;
  unsigned int ping_last_packet_recv_flag;
  unsigned int ping_packet_count_send;
  unsigned int ping_packet_count_recv;
  unsigned int ping_consec_count;
  
  discover_state_t discover_state;
  char hostname[MAXHOSTNAMELEN+1];
  struct sockaddr_in destaddr;
};

/* ipmipower_config
 * - store all ipmipower configuration values
 */
struct ipmipower_config 
{
  ipmi_driver_type_t       driver_type;
  hostlist_t               hosts;
  int                      hosts_count;
  char                     username[IPMI_MAX_USER_NAME_LENGTH+1];
  char                     password[IPMI_2_0_MAX_PASSWORD_LENGTH+1];
  uint8_t                  k_g[IPMI_MAX_K_G_LENGTH+1];
  /* The k_g_len is needed b/c the k_g field may have null
   * values as part of it's hex key.  For example, if k_g ==
   * 0x00010203, then strlen(conf->k_g) == 0.  So we need a len to
   * indicate what the real length of the field is.
   */
  unsigned int             k_g_len;
  int                      session_timeout_len;
  int                      retransmission_timeout_len;
  uint8_t                  authentication_type;
  cipher_suite_id_t        cipher_suite_id;
  uint8_t                  privilege_level;
  uint32_t                 workaround_flags;
  int                      debug;
  char                     configfile[MAXPATHLEN+1];
#ifndef NDEBUG
  int                      rmcpdump;
  int                      log;
  char                     logfile[MAXPATHLEN+1];
  int                      logfile_fd;
#endif /* NDEBUG */
  /* buffer_output and always_prefix not implemented in ipmipower
   * added only for consistency to other tools.
   */
  int                      buffer_output;
  int                      consolidate_output;
  int                      fanout;
  int                      eliminate;
  int                      always_prefix;

  power_cmd_t              powercmd;
  int                      on_if_off;
  int                      wait_until_on;
  int                      wait_until_off;
  int                      retransmission_wait_timeout_len;
  int                      retransmission_backoff_count; 
  int                      ping_interval_len;
  int                      ping_timeout_len;
  int                      ping_packet_count;
  int                      ping_percent;
  int                      ping_consec_count;

  /* Flags indicating if option was set on the command line */
  int                      driver_type_set_on_cmdline;
  int                      hosts_set_on_cmdline;
  int                      username_set_on_cmdline;
  int                      password_set_on_cmdline;
  int                      k_g_set_on_cmdline;
  int                      session_timeout_len_set_on_cmdline;
  int                      retransmission_timeout_len_set_on_cmdline;
  int                      authentication_type_set_on_cmdline;
  int                      cipher_suite_id_set_on_cmdline;
  int                      privilege_level_set_on_cmdline;
  int                      workaround_flags_set_on_cmdline;
  int                      buffer_output_set_on_cmdline;
  int                      consolidate_output_set_on_cmdline;
  int                      fanout_set_on_cmdline;
  int                      eliminate_set_on_cmdline;
  int                      always_prefix_set_on_cmdline;
  int                      on_if_off_set_on_cmdline;
  int                      wait_until_on_set_on_cmdline;
  int                      wait_until_off_set_on_cmdline;
  int                      retransmission_wait_timeout_len_set_on_cmdline;
  int                      retransmission_backoff_count_set_on_cmdline;
  int                      ping_interval_len_set_on_cmdline;
  int                      ping_timeout_len_set_on_cmdline; 
  int                      ping_consec_count_set_on_cmdline;
  int                      ping_packet_count_set_on_cmdline;
  int                      ping_percent_set_on_cmdline;
};

typedef struct ipmipower_powercmd *ipmipower_powercmd_t;
typedef struct ipmipower_connection *ipmipower_connection_t;

#endif /* _IPMIPOWER_H */
