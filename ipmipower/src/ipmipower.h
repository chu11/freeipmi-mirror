/*****************************************************************************\
 *  $Id: ipmipower.h,v 1.42 2006-03-15 19:09:11 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
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
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMIPOWER_H
#define _IPMIPOWER_H

#if HAVE_CONFIG_H
#include "config.h"
#endif

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#include <sys/param.h>
#include <netinet/in.h>

#include <freeipmi/freeipmi.h>

#include "hostlist.h"
#include "cbuf.h"
#include "list.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif

/*
 * ipmipower config default
 */
#define IPMIPOWER_CONFIGFILE_DEFAULT     "/etc/ipmipower.conf" 

/* 
 * ipmipower limits 
 */
 
#define IPMIPOWER_MIN_TTY_BUF              1024*4
#define IPMIPOWER_MAX_TTY_BUF              1024*32

#define IPMIPOWER_MIN_CONNECTION_BUF       1024*2
#define IPMIPOWER_MAX_CONNECTION_BUF       1024*4

#define IPMIPOWER_MINNODES                 1
#define IPMIPOWER_MAXNODES                 1024

#define IPMIPOWER_TIMEOUT_MIN              1000   /* 1 second */
#define IPMIPOWER_TIMEOUT_MAX              120000 /* 120 seconds */

#define IPMIPOWER_RETRY_TIMEOUT_MIN        50     /* .05 seconds */
#define IPMIPOWER_RETRY_TIMEOUT_MAX        IPMIPOWER_TIMEOUT_MAX

#define IPMIPOWER_RETRY_BACKOFF_COUNT_MIN  1
#define IPMIPOWER_RETRY_BACKOFF_COUNT_MAX  200

#define IPMIPOWER_PING_INTERVAL_MIN        250   /* .25 seconds */
#define IPMIPOWER_PING_INTERVAL_MAX        IPMIPOWER_TIMEOUT_MAX

#define IPMIPOWER_PING_TIMEOUT_MIN         IPMIPOWER_TIMEOUT_MIN
#define IPMIPOWER_PING_TIMEOUT_MAX         IPMIPOWER_TIMEOUT_MAX

#define IPMIPOWER_PING_PACKET_COUNT_MIN    2
#define IPMIPOWER_PING_PACKET_COUNT_MAX    20

#define IPMIPOWER_PING_PERCENT_MIN         1
#define IPMIPOWER_PING_PERCENT_MAX         100

#define IPMIPOWER_PING_CONSEC_COUNT_MIN    2
#define IPMIPOWER_PING_CONSEC_COUNT_MAX    20

/* 
 * ipmi specifics for ipmipower
 */

/* IPMI allowable sequence number range window
 */

#define IPMIPOWER_SEQUENCE_NUMBER_WINDOW 8

#define IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER       1
#define IPMIPOWER_RMCPPLUS_INITIAL_OUTBOUND_SEQUENCE_NUMBER  0

/* MISC */

#define IPMI_PACKET_BUFLEN               1024
#define RMCP_PACKET_BUFLEN               1024

#define IPMIPOWER_HOSTLIST_BUFLEN        65536

#define IPMIPOWER_DEFAULT_LOGFILE        "/tmp/ipmipower.%d"

#define IPMI_CIPHER_SUITE_RECORD_DATA_BUFFER_LENGTH 64

/* achu: See IPMI 2.0 spec, Table 22-18 - Cipher Suite Record Format.
 * The smallest record format is 2 bytes.  So the most records I could
 * read is the buffer length divided by 2.
 */
#define IPMI_CIPHER_SUITE_IDS_LENGTH                (IPMI_CIPHER_SUITE_RECORD_DATA_BUFFER_LENGTH/2)

/* achu: See IPMI 2.0 spec, Table 22-17 - Get Channel Cipher Suites
 * Command 
 */
#define IPMI_CIPHER_SUITE_RECORD_DATA_LENGTH 16

#define IPMI_MAX_SIK_KEY_LENGTH             64

#define IPMI_MAX_INTEGRITY_KEY_LENGTH       64

#define IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH 64

#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH 64

/* ipmi_version_t
 * - holds ipmi version type
 */
typedef enum
  {
    IPMI_VERSION_INVALID = 0,
    IPMI_VERSION_AUTO    = 1,
    IPMI_VERSION_1_5     = 2,
    IPMI_VERSION_2_0     = 3,
  } ipmi_version_t;

#define IPMI_VERSION_VALID(__a) \
  ((__a) >= IPMI_VERSION_1_5 && \
   (__a) <= IPMI_VERSION_2_0)

#define IPMI_VERSION_VALID_OR_AUTO(__a) \
  ((__a) >= IPMI_VERSION_AUTO && \
   (__a) <= IPMI_VERSION_2_0)

/* ipmipower_bool_t
 * - boolean type
 */
typedef enum 
  { 
    IPMIPOWER_TRUE  = 1,
    IPMIPOWER_FALSE = 0,
  } ipmipower_bool_t;

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

#define POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE(__c) \
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
    GET_SESSION_CHALLENGE_RES           = 0x204,
    ACTIVATE_SESSION_REQ                = 0x105, 
    ACTIVATE_SESSION_RES                = 0x205,
    SET_SESSION_PRIVILEGE_REQ           = 0x106, 
    SET_SESSION_PRIVILEGE_RES           = 0x206, 
    GET_CHANNEL_CIPHER_SUITES_REQ       = 0x107,
    GET_CHANNEL_CIPHER_SUITES_RES       = 0x207,
    OPEN_SESSION_REQ                    = 0x108,
    OPEN_SESSION_RES                    = 0x208,
    RAKP_MESSAGE_1_REQ                  = 0x109,
    RAKP_MESSAGE_2_RES                  = 0x209,
    RAKP_MESSAGE_3_REQ                  = 0x10A,
    RAKP_MESSAGE_4_RES                  = 0x20A,
    CLOSE_SESSION_REQ                   = 0x10B, 
    CLOSE_SESSION_RES                   = 0x20B,
    GET_CHASSIS_STATUS_REQ              = 0x10C, 
    GET_CHASSIS_STATUS_RES              = 0x20C,
    CHASSIS_CONTROL_REQ                 = 0x10D, 
    CHASSIS_CONTROL_RES                 = 0x20D, 
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
    PROTOCOL_STATE_SET_SESSION_PRIVILEGE_SENT           = 0x05,
    PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT              = 0x06,
    PROTOCOL_STATE_CHASSIS_CONTROL_SENT                 = 0x07,
    PROTOCOL_STATE_GET_CHANNEL_CIPHER_SUITES_SENT       = 0x08,
    PROTOCOL_STATE_OPEN_SESSION_SENT                    = 0x09,
    PROTOCOL_STATE_RAKP_MESSAGE_1_SENT                  = 0x0A,
    PROTOCOL_STATE_RAKP_MESSAGE_3_SENT                  = 0x0B,
    PROTOCOL_STATE_CLOSE_SESSION_SENT                   = 0x0C,
    PROTOCOL_STATE_END                                  = 0x0D,
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

/* Authentication Types */
typedef enum 
  { 
    AUTHENTICATION_TYPE_INVALID               = 0x00,
    AUTHENTICATION_TYPE_AUTO                  = 0x01,
    AUTHENTICATION_TYPE_NONE                  = 0x02,
    AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY = 0x03,
    AUTHENTICATION_TYPE_MD2                   = 0x04,
    AUTHENTICATION_TYPE_MD5                   = 0x05,
  } authentication_type_t;

#define AUTHENTICATION_TYPE_VALID(__a) \
  ((__a) >= AUTHENTICATION_TYPE_NONE && \
   (__a) <= AUTHENTICATION_TYPE_MD5)

#define AUTHENTICATION_TYPE_VALID_OR_AUTO(__a) \
  ((__a) >= AUTHENTICATION_TYPE_AUTO && \
   (__a) <= AUTHENTICATION_TYPE_MD5)

/* Privilege Types */
typedef enum 
  {
    PRIVILEGE_TYPE_INVALID   = 0x00,
    PRIVILEGE_TYPE_AUTO      = 0x01,
    PRIVILEGE_TYPE_USER      = 0x02,
    PRIVILEGE_TYPE_OPERATOR  = 0x03,
    PRIVILEGE_TYPE_ADMIN     = 0x04,
  } privilege_type_t;

#define PRIVILEGE_TYPE_VALID(__p) \
  ((__p) >= PRIVILEGE_TYPE_USER && \
   (__p) <= PRIVILEGE_TYPE_ADMIN)

#define PRIVILEGE_TYPE_VALID_OR_AUTO(__p) \
  ((__p) >= PRIVILEGE_TYPE_AUTO && \
   (__p) <= PRIVILEGE_TYPE_ADMIN)

/* Cipher_Suite Ids */
typedef enum 
  { 
    CIPHER_SUITE_ID_INVALID               = 0x00,
    CIPHER_SUITE_ID_AUTO                  = 0x01,
    CIPHER_SUITE_ID_0                     = 0x02,
    /* CIPHER_SUITE_ID_1                     = 0x03, */
    /* CIPHER_SUITE_ID_2                     = 0x04, */
    /* CIPHER_SUITE_ID_3                     = 0x05, */
    /* xRC4 CIPHER_SUITE_ID_4                     = 0x06, */
    /* xRC4 CIPHER_SUITE_ID_5                     = 0x07, */
    /* CIPHER_SUITE_ID_6                     = 0x08, */
    /* CIPHER_SUITE_ID_7                     = 0x09, */
    /* CIPHER_SUITE_ID_8                     = 0x0A, */
    /* xRC4 CIPHER_SUITE_ID_9                     = 0x0B, */
    /* xRC4 CIPHER_SUITE_ID_10                    = 0x0C, */
    /* CIPHER_SUITE_ID_11                    = 0x0D, */
    /* CIPHER_SUITE_ID_12                    = 0x0E, */
    /* xRC4 CIPHER_SUITE_ID_13                    = 0x0F, */
    /* xRC4 CIPHER_SUITE_ID_14                    = 0x10, */
  } cipher_suite_id_t;

/* XXX */
#define CIPHER_SUITE_ID_VALID(__c) \
  ((__c) == CIPHER_SUITE_ID_0)
    /* || (__c) == CIPHER_SUITE_ID_1 \ */
    /* || (__c) == CIPHER_SUITE_ID_2 \ */
    /* || (__c) == CIPHER_SUITE_ID_3 \ */
    /* xRC4 || (__c) == CIPHER_SUITE_ID_4 \ */
    /* xRC4 || (__c) == CIPHER_SUITE_ID_5 \ */
    /* || (__c) == CIPHER_SUITE_ID_6 \ */
    /* || (__c) == CIPHER_SUITE_ID_7 \ */
    /* || (__c) == CIPHER_SUITE_ID_8 \ */
    /* xRC4 || (__c) == CIPHER_SUITE_ID_9 \ */
    /* xRC4 || (__c) == CIPHER_SUITE_ID_10 \ */
    /* || (__c) == CIPHER_SUITE_ID_11 \ */
    /* || (__c) == CIPHER_SUITE_ID_12 \ */
    /* xRC4 || (__c) == CIPHER_SUITE_ID_13 \ */
    /* xRC4 || (__c) == CIPHER_SUITE_ID_14 \ */

#define CIPHER_SUITE_ID_VALID_OR_AUTO(__c) \
  ((__c) == CIPHER_SUITE_ID_AUTO \
   || CIPHER_SUITE_ID_VALID(__c))

/* Output Types */
typedef enum { OUTPUT_TYPE_INVALID   = 0,
               OUTPUT_TYPE_NONE      = 1,
               OUTPUT_TYPE_NEWLINE   = 2,
               OUTPUT_TYPE_HOSTRANGE = 3} output_type_t;

#define OUTPUT_TYPE_VALID(__o) \
  ((__o) >= OUTPUT_TYPE_NONE && \
   (__o) <= OUTPUT_TYPE_HOSTRANGE)

/* Msg Types */
typedef enum 
  { 
    MSG_TYPE_SUCCESS                 =  0,
    MSG_TYPE_ON                      =  1,
    MSG_TYPE_OFF                     =  2,
    MSG_TYPE_OK                      =  3,
    MSG_TYPE_PERMISSION              =  4,
    MSG_TYPE_USERNAME                =  5,
    MSG_TYPE_PASSWORD                =  6,
    MSG_TYPE_PRIVILEGE               =  7,
    MSG_TYPE_OPERATION               =  8,
    MSG_TYPE_AUTHENTICATION_TYPE     =  9,
    MSG_TYPE_1_5_AUTO                = 10,
    MSG_TYPE_GIVEN_PRIVILEGE         = 11,
    MSG_TYPE_CIPHER_SUITE            = 12,
    MSG_TYPE_2_0_AUTO                = 13,
    MSG_TYPE_TIMEDOUT                = 14,
    MSG_TYPE_NOTDISCOVERED           = 15,
    MSG_TYPE_BADCONNECTION           = 16,
    MSG_TYPE_UNKNOWNNODE             = 17,
    MSG_TYPE_RESOURCES               = 18,
    MSG_TYPE_VERSION_NOT_SUPPORTED   = 19,
    MSG_TYPE_BMCBUSY                 = 20,
    MSG_TYPE_BMCERROR                = 21,
  } msg_type_t;

#define MSG_TYPE_VALID(__m) \
  ((__m) >= MSG_TYPE_SUCCESS && \
   (__m) <= MSG_TYPE_BMCERROR)

#define MSG_TYPE_NUM_ENTRIES (MSG_TYPE_BMCERROR+1)

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
  ipmipower_bool_t error_occurred;
  unsigned int retry_count;
  uint8_t close_timeout;

  /*
   * Protocol Maintenance Variables 
   */
  ipmi_version_t ipmi_version;
  unsigned int session_inbound_count;
  uint32_t highest_received_sequence_number;
  unsigned int previously_received_list;
  uint8_t privilege;

  /* IPMI 1.5 specific */
  ipmipower_bool_t permsgauth_enabled;
  uint8_t authentication_type;

  /* IPMI 2.0 specific */
  uint8_t cipher_suite_id;
  unsigned int cipher_suite_id_ranking_index;
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
  uint8_t session_sequence_number;
  uint8_t name_only_lookup;
  uint32_t remote_console_session_id;  
  uint8_t remote_console_random_number[IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH];
  uint32_t cipher_suite_list_index;
  uint8_t cipher_suite_record_data[IPMI_CIPHER_SUITE_RECORD_DATA_BUFFER_LENGTH];
  uint32_t cipher_suite_record_data_bytes;
  uint8_t cipher_suite_ids[IPMI_CIPHER_SUITE_IDS_LENGTH];
  uint32_t cipher_suite_ids_num;

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
  fiid_obj_t obj_set_session_privilege_req;
  fiid_obj_t obj_set_session_privilege_res;
  fiid_obj_t obj_get_channel_cipher_suites_req;
  fiid_obj_t obj_get_channel_cipher_suites_res;
  fiid_obj_t obj_open_session_req;
  fiid_obj_t obj_open_session_res;
  fiid_obj_t obj_rakp_message_1_req;
  fiid_obj_t obj_rakp_message_2_res;
  fiid_obj_t obj_rakp_message_3_req;
  fiid_obj_t obj_rakp_message_4_res;
  fiid_obj_t obj_close_session_req;
  fiid_obj_t obj_close_session_res;
  fiid_obj_t obj_get_chassis_status_req;
  fiid_obj_t obj_get_chassis_status_res;
  fiid_obj_t obj_chassis_control_req;
  fiid_obj_t obj_chassis_control_res;

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
  hostlist_t               hosts;
  int                      hosts_count;
  char                     username[IPMI_MAX_USER_NAME_LENGTH+1];
  char                     password[IPMI_MAX_AUTHENTICATION_CODE_LENGTH+1];
  power_cmd_t              powercmd;
  char                     configfile[MAXPATHLEN+1];

  authentication_type_t    authentication_type;
  privilege_type_t         privilege;
  ipmi_version_t           ipmi_version;
  cipher_suite_id_t        cipher_suite_id;
  ipmipower_bool_t         on_if_off;
  output_type_t            outputtype;
  ipmipower_bool_t         force_permsg_authentication;
  ipmipower_bool_t         accept_session_id_zero;
  ipmipower_bool_t         check_unexpected_authcode;
  
#ifndef NDEBUG
  ipmipower_bool_t         debug;
  ipmipower_bool_t         ipmidump;
  ipmipower_bool_t         rmcpdump;
  ipmipower_bool_t         log;
  char                     logfile[MAXPATHLEN+1];
  int                      logfile_fd;
#endif
  int                      timeout_len;
  int                      retry_timeout_len;
  int                      retry_backoff_count; 
  int                      ping_interval_len;
  int                      ping_timeout_len;
  int                      ping_packet_count;
  int                      ping_percent;
  int                      ping_consec_count;

  /* Flags indicating if option was set on the command line */
  ipmipower_bool_t         hosts_set;
  ipmipower_bool_t         username_set;
  ipmipower_bool_t         password_set;
  ipmipower_bool_t         authentication_type_set;
  ipmipower_bool_t         privilege_set;
  ipmipower_bool_t         ipmi_version_set;
  ipmipower_bool_t         cipher_suite_id_set;
  ipmipower_bool_t         on_if_off_set;
  ipmipower_bool_t         force_permsg_authentication_set;
  ipmipower_bool_t         accept_session_id_zero_set;
  ipmipower_bool_t         check_unexpected_authcode_set;
  ipmipower_bool_t         outputtype_set;
  ipmipower_bool_t         timeout_len_set;
  ipmipower_bool_t         retry_timeout_len_set;
  ipmipower_bool_t         retry_backoff_count_set;
  ipmipower_bool_t         ping_interval_len_set;
  ipmipower_bool_t         ping_timeout_len_set; 
  ipmipower_bool_t         ping_consec_count_set;
  ipmipower_bool_t         ping_packet_count_set;
  ipmipower_bool_t         ping_percent_set;
};

typedef struct ipmipower_powercmd *ipmipower_powercmd_t;
typedef struct ipmipower_connection *ipmipower_connection_t;

#endif /* _IPMIPOWER_H */
