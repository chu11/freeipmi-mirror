/*****************************************************************************\
 *  $Id: ipmipower.h,v 1.145 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#include <limits.h>             /* MAXHOSTNAMELEN */
#ifdef HAVE_NETDB_H
#include <netdb.h>              /* MAXHOSTNAMELEN Solaris */
#endif /* HAVE_NETDB_H */

#include <freeipmi/freeipmi.h>

#include "hostlist.h"
#include "cbuf.h"
#include "list.h"
#include "tool-cmdline-common.h"

#include "ipmidetect.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#define IPMIPOWER_MIN_TTY_BUF 1024*4
#define IPMIPOWER_MAX_TTY_BUF 1024*32

#define IPMIPOWER_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER       1

#define IPMIPOWER_RMCPPLUS_INITIAL_OUTBOUND_SEQUENCE_NUMBER  0

#define IPMIPOWER_PACKET_BUFLEN                          1024

#define IPMIPOWER_OUTPUT_BUFLEN                          65536

#define IPMI_MAX_SIK_KEY_LENGTH                          64

#define IPMI_MAX_INTEGRITY_KEY_LENGTH                    64

#define IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH              64

#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH 64

/* power_cmd_t
 * - power control commands
 */
typedef enum
  {
    POWER_CMD_NONE                       = 0x00,
    POWER_CMD_POWER_OFF                  = 0x01,
    POWER_CMD_POWER_ON                   = 0x02,
    POWER_CMD_POWER_CYCLE                = 0x03,
    POWER_CMD_POWER_RESET                = 0x04,
    POWER_CMD_POWER_STATUS               = 0x05,
    POWER_CMD_PULSE_DIAGNOSTIC_INTERRUPT = 0x06,
    POWER_CMD_SOFT_SHUTDOWN_OS           = 0x07,
    POWER_CMD_IDENTIFY_ON                = 0x08,
    POWER_CMD_IDENTIFY_OFF               = 0x09,
    POWER_CMD_IDENTIFY_STATUS            = 0x0A,
  } power_cmd_t;

#define POWER_CMD_VALID(__c)             \
  (((__c) >= POWER_CMD_POWER_OFF         \
    && (__c) <= POWER_CMD_IDENTIFY_STATUS) ? 1 : 0)

#define POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE_LEVEL(__c)    \
  ((__c) == POWER_CMD_POWER_OFF                             \
   || (__c) == POWER_CMD_POWER_ON                           \
   || (__c) == POWER_CMD_POWER_CYCLE                        \
   || (__c) == POWER_CMD_POWER_RESET                        \
   || (__c) == POWER_CMD_PULSE_DIAGNOSTIC_INTERRUPT         \
   || (__c) == POWER_CMD_SOFT_SHUTDOWN_OS                   \
   || (__c) == POWER_CMD_IDENTIFY_ON                        \
   || (__c) == POWER_CMD_IDENTIFY_OFF)

typedef enum
  {
    OEM_POWER_TYPE_NONE    = 0,
    OEM_POWER_TYPE_C410X   = 1,
    OEM_POWER_TYPE_INVALID = 255
  } oem_power_type_t;

#define OEM_POWER_TYPE_NONE_STR "none"
#define OEM_POWER_TYPE_C410X_STR "c410x"

#define OEM_POWER_TYPE_VALID(__v)     \
  (((__v) == OEM_POWER_TYPE_NONE      \
    || (__v) == OEM_POWER_TYPE_C410X) ? 1 : 0)

#define OEM_POWER_TYPE_REQUIRES_EXTRA_ARGUMENT(__v) \
  (((__v) == OEM_POWER_TYPE_C410X) ? 1 : 0)

#define OEM_POWER_TYPE_SUPPORT_OFF                  0x0001
#define OEM_POWER_TYPE_SUPPORT_ON                   0x0002
#define OEM_POWER_TYPE_SUPPORT_CYCLE                0x0004
#define OEM_POWER_TYPE_SUPPORT_RESET                0x0008
#define OEM_POWER_TYPE_SUPPORT_STATUS               0x0010
#define OEM_POWER_TYPE_SUPPORT_DIAGNOSTIC_INTERRUPT 0x0020
#define OEM_POWER_TYPE_SUPPORT_SOFT_SHUTDOWN_OS     0x0040
#define OEM_POWER_TYPE_SUPPORT_IDENTIFY_ON          0x0080
#define OEM_POWER_TYPE_SUPPORT_IDENTIFY_OFF         0x0100
#define OEM_POWER_TYPE_SUPPORT_IDENTIFY_STATUS      0x0200
#define OEM_POWER_TYPE_SUPPORT_ALL                  0xFFFF

struct oem_power_type_data {
  char *name;
  unsigned int supported_operations;
};

/* packet_type_t
 * - packet types stored internally in an ipmipower_powercmd structure.
 * - Request types are *_RQ, Response types are *_RS
 */
typedef enum
  {
    AUTHENTICATION_CAPABILITIES_RQ    = 0x101,
    AUTHENTICATION_CAPABILITIES_RS    = 0x201,
    GET_SESSION_CHALLENGE_RQ          = 0x102,
    GET_SESSION_CHALLENGE_RS          = 0x202,
    ACTIVATE_SESSION_RQ               = 0x103,
    ACTIVATE_SESSION_RS               = 0x203,
    OPEN_SESSION_RQ                   = 0x104,
    OPEN_SESSION_RS                   = 0x204,
    RAKP_MESSAGE_1_RQ                 = 0x105,
    RAKP_MESSAGE_2_RS                 = 0x205,
    RAKP_MESSAGE_3_RQ                 = 0x106,
    RAKP_MESSAGE_4_RS                 = 0x206,
    SET_SESSION_PRIVILEGE_LEVEL_RQ    = 0x107,
    SET_SESSION_PRIVILEGE_LEVEL_RS    = 0x207,
    GET_CHASSIS_STATUS_RQ             = 0x108,
    GET_CHASSIS_STATUS_RS             = 0x208,
    CHASSIS_CONTROL_RQ                = 0x109,
    CHASSIS_CONTROL_RS                = 0x209,
    CHASSIS_IDENTIFY_RQ               = 0x10A,
    CHASSIS_IDENTIFY_RS               = 0x20A,
    C410X_GET_SENSOR_READING_RQ       = 0x10B,
    C410X_GET_SENSOR_READING_RS       = 0x20B,
    C410X_SLOT_POWER_CONTROL_RQ       = 0x10C,
    C410X_SLOT_POWER_CONTROL_RS       = 0x20C,
    CLOSE_SESSION_RQ                  = 0x10D,
    CLOSE_SESSION_RS                  = 0x20D,
  } packet_type_t;

#define PACKET_TYPE_RQ_MASK      0x100
#define PACKET_TYPE_RS_MASK      0x200
#define PACKET_TYPE_MIN          0x001
#define PACKET_TYPE_MAX          0x00D
#define PACKET_TYPE_PACKET_MASK  0x0FF

#define PACKET_TYPE_PACKET_VALID(__p)                   \
  (((__p) & PACKET_TYPE_PACKET_MASK) >= PACKET_TYPE_MIN \
   && ((__p) & PACKET_TYPE_PACKET_MASK) <= PACKET_TYPE_MAX)

#define PACKET_TYPE_RQ(__p)       \
  ((((__p) & PACKET_TYPE_RQ_MASK) \
    && PACKET_TYPE_PACKET_VALID ((__p))) ? 1 : 0)

#define PACKET_TYPE_RS(__p)       \
  ((((__p) & PACKET_TYPE_RS_MASK) \
    && PACKET_TYPE_PACKET_VALID ((__p))) ? 1 : 0)

#define PACKET_TYPE_VALID(__p) \
  ((PACKET_TYPE_RQ ((__p))     \
    || PACKET_TYPE_RS ((__p))) ? 1 : 0)

#define PACKET_TYPE_IPMI_1_5_SETUP(__p)         \
  ((((__p) == AUTHENTICATION_CAPABILITIES_RQ    \
     || (__p) == AUTHENTICATION_CAPABILITIES_RS	\
     || (__p) == GET_SESSION_CHALLENGE_RQ	\
     || (__p) == GET_SESSION_CHALLENGE_RS	\
     || (__p) == ACTIVATE_SESSION_RQ		\
     || (__p) == ACTIVATE_SESSION_RS)) ? 1 : 0)

#define PACKET_TYPE_IPMI_1_5_SETUP_RQ(__p) \
  ((PACKET_TYPE_IPMI_1_5_SETUP (__p)	   \
    && PACKET_TYPE_RQ (__p)) ? 1 : 0)

#define PACKET_TYPE_IPMI_1_5_SETUP_RS(__p) \
  ((PACKET_TYPE_IPMI_1_5_SETUP (__p)	   \
    && PACKET_TYPE_RS (__p)) ? 1 : 0)

#define PACKET_TYPE_IPMI_2_0_SETUP(__p) \
  ((((__p) == OPEN_SESSION_RQ		\
    || (__p) == OPEN_SESSION_RS		\
    || (__p) == RAKP_MESSAGE_1_RQ       \
    || (__p) == RAKP_MESSAGE_2_RS       \
    || (__p) == RAKP_MESSAGE_3_RQ       \
    || (__p) == RAKP_MESSAGE_4_RS))  ? 1 : 0)

#define PACKET_TYPE_IPMI_2_0_SETUP_RQ(__p) \
  ((PACKET_TYPE_IPMI_2_0_SETUP (__p)	   \
    && PACKET_TYPE_RQ (__p)) ? 1 : 0)

#define PACKET_TYPE_IPMI_2_0_SETUP_RS(__p) \
  ((PACKET_TYPE_IPMI_2_0_SETUP (__p)	   \
    && PACKET_TYPE_RS (__p)) ? 1 : 0)

#define PACKET_TYPE_IPMI_SESSION_PACKET(__p)    \
  ((((__p) == SET_SESSION_PRIVILEGE_LEVEL_RQ    \
     || (__p) == SET_SESSION_PRIVILEGE_LEVEL_RS	\
     || (__p) == GET_CHASSIS_STATUS_RQ		\
     || (__p) == GET_CHASSIS_STATUS_RS		\
     || (__p) == CHASSIS_CONTROL_RQ		\
     || (__p) == CHASSIS_CONTROL_RS		\
     || (__p) == CHASSIS_IDENTIFY_RQ		\
     || (__p) == CHASSIS_IDENTIFY_RS		\
     || (__p) == C410X_GET_SENSOR_READING_RQ	\
     || (__p) == C410X_GET_SENSOR_READING_RS	\
     || (__p) == C410X_SLOT_POWER_CONTROL_RQ	\
     || (__p) == C410X_SLOT_POWER_CONTROL_RS	\
     || (__p) == CLOSE_SESSION_RQ		\
     || (__p) == CLOSE_SESSION_RS)) ? 1 : 0)

#define PACKET_TYPE_IPMI_SESSION_PACKET_RQ(__p) \
  ((PACKET_TYPE_IPMI_SESSION_PACKET (__p)	\
    && PACKET_TYPE_RQ (__p)) ? 1 : 0)

#define PACKET_TYPE_IPMI_SESSION_PACKET_RS(__p) \
  ((PACKET_TYPE_IPMI_SESSION_PACKET (__p)	\
    && PACKET_TYPE_RS (__p)) ? 1 : 0)

/* Protocol States */
typedef enum
  {
    PROTOCOL_STATE_START                                = 0x00,
    PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT     = 0x01,
    PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT           = 0x02,
    PROTOCOL_STATE_ACTIVATE_SESSION_SENT                = 0x03,
    PROTOCOL_STATE_OPEN_SESSION_SENT                    = 0x04,
    PROTOCOL_STATE_RAKP_MESSAGE_1_SENT                  = 0x05,
    PROTOCOL_STATE_RAKP_MESSAGE_3_SENT                  = 0x06,
    PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT     = 0x07,
    PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT              = 0x08,
    PROTOCOL_STATE_CHASSIS_CONTROL_SENT                 = 0x09,
    PROTOCOL_STATE_CHASSIS_IDENTIFY_SENT                = 0x0A,
    PROTOCOL_STATE_C410X_GET_SENSOR_READING_SENT        = 0x0B,
    PROTOCOL_STATE_C410X_SLOT_POWER_CONTROL_SENT        = 0x0C,
    PROTOCOL_STATE_CLOSE_SESSION_SENT                   = 0x0D,
    PROTOCOL_STATE_END                                  = 0x0E,
  } protocol_state_t;

#define PROTOCOL_STATE_VALID(__s)    \
  (((__s) >= PROTOCOL_STATE_START    \
    && (__s) <= PROTOCOL_STATE_END) ? 1 : 0)

/* Discovery States */
typedef enum
  {
    STATE_DISCOVERED     = 0x01,
    STATE_UNDISCOVERED   = 0x02,
    STATE_BADCONNECTION  = 0x03,
  } discover_state_t;

#define DISCOVER_STATE_VALID(__s)    \
  (((__s) >= STATE_DISCOVERED	     \
    && (__s) <= STATE_BADCONNECTION) ? 1 : 0)

/* Link States */
typedef enum
  {
    LINK_GOOD = 0x01,
    LINK_BAD  = 0x02,
  } link_state_t;

#define LINK_STATE_VALID(__s)    \
  (((__s) >= LINK_GOOD	         \
   && (__s) <= LINK_BAD) ? 1 : 0)

/* Msg Types */
typedef enum
  {
    MSG_TYPE_ON                                 =  0,
    MSG_TYPE_OFF                                =  1,
    MSG_TYPE_OK                                 =  2,
    MSG_TYPE_UNKNOWN                            =  3,
    MSG_TYPE_USERNAME_INVALID                   =  4,
    MSG_TYPE_PASSWORD_INVALID                   =  5,
    MSG_TYPE_PASSWORD_LENGTH_INVALID            =  6,
    MSG_TYPE_K_G_INVALID                        =  7,
    MSG_TYPE_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED =  8,
    MSG_TYPE_OPERATION_INVALID                  =  9,
    MSG_TYPE_AUTHENTICATION_TYPE_UNAVAILABLE    = 10,
    MSG_TYPE_CIPHER_SUITE_ID_UNAVAILABLE        = 11,
    MSG_TYPE_PASSWORD_VERIFICATION_TIMEOUT      = 12,
    MSG_TYPE_CONNECTION_TIMEOUT                 = 13,
    MSG_TYPE_SESSION_TIMEOUT                    = 14,
    MSG_TYPE_NOTDISCOVERED                      = 15,
    MSG_TYPE_BADCONNECTION                      = 16,
    MSG_TYPE_HOSTNAME_INVALID                   = 17,
    MSG_TYPE_UNCONFIGURED_HOSTNAME              = 18,
    MSG_TYPE_RESOURCES                          = 19,
    MSG_TYPE_IPMI_2_0_UNAVAILABLE               = 20,
    MSG_TYPE_INVALID_ARGUMENT_FOR_OEM_EXTENSION = 21,
    MSG_TYPE_BMC_BUSY                           = 22,
    MSG_TYPE_BMC_ERROR                          = 23,
  } msg_type_t;

#define MSG_TYPE_VALID(__m)         \
  ((__m) >= MSG_TYPE_ON             \
   && (__m) <= MSG_TYPE_BMC_ERROR)

#define MSG_TYPE_NUM_ENTRIES (MSG_TYPE_BMC_ERROR + 1)

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
  uint32_t previously_received_list;

  /* IPMI 1.5 specific */
  int permsgauth_enabled;

  /* IPMI 2.0 specific */
  uint8_t requested_maximum_privilege_level;
  uint8_t authentication_algorithm;
  uint8_t integrity_algorithm;
  uint8_t confidentiality_algorithm;
  uint8_t sik_key[IPMI_MAX_SIK_KEY_LENGTH];
  void *sik_key_ptr;
  unsigned int sik_key_len;
  uint8_t integrity_key[IPMI_MAX_INTEGRITY_KEY_LENGTH];
  void *integrity_key_ptr;
  unsigned int integrity_key_len;
  uint8_t confidentiality_key[IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH];
  void *confidentiality_key_ptr;
  unsigned int confidentiality_key_len;
  uint8_t initial_message_tag;
  uint8_t message_tag_count;
  uint32_t session_sequence_number;
  uint8_t name_only_lookup;
  uint32_t remote_console_session_id;
  uint8_t remote_console_random_number[IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH];

  /* Ipmipower variables */
  int wait_until_on_state;
  int wait_until_off_state;

  struct ipmipower_connection *ic;

  fiid_obj_t obj_rmcp_hdr_rq;
  fiid_obj_t obj_rmcp_hdr_rs;
  fiid_obj_t obj_lan_session_hdr_rq;
  fiid_obj_t obj_lan_session_hdr_rs;
  fiid_obj_t obj_lan_msg_hdr_rq;
  fiid_obj_t obj_lan_msg_hdr_rs;
  fiid_obj_t obj_lan_msg_trlr_rs;
  fiid_obj_t obj_rmcpplus_session_hdr_rq;
  fiid_obj_t obj_rmcpplus_session_hdr_rs;
  fiid_obj_t obj_rmcpplus_payload_rs;
  fiid_obj_t obj_rmcpplus_session_trlr_rq;
  fiid_obj_t obj_rmcpplus_session_trlr_rs;

  fiid_obj_t obj_authentication_capabilities_rq;
  fiid_obj_t obj_authentication_capabilities_rs;
  fiid_obj_t obj_get_session_challenge_rq;
  fiid_obj_t obj_get_session_challenge_rs;
  fiid_obj_t obj_activate_session_rq;
  fiid_obj_t obj_activate_session_rs;
  fiid_obj_t obj_open_session_rq;
  fiid_obj_t obj_open_session_rs;
  fiid_obj_t obj_rakp_message_1_rq;
  fiid_obj_t obj_rakp_message_2_rs;
  fiid_obj_t obj_rakp_message_3_rq;
  fiid_obj_t obj_rakp_message_4_rs;
  fiid_obj_t obj_set_session_privilege_level_rq;
  fiid_obj_t obj_set_session_privilege_level_rs;
  fiid_obj_t obj_get_chassis_status_rq;
  fiid_obj_t obj_get_chassis_status_rs;
  fiid_obj_t obj_chassis_control_rq;
  fiid_obj_t obj_chassis_control_rs;
  fiid_obj_t obj_chassis_identify_rq;
  fiid_obj_t obj_chassis_identify_rs;
  fiid_obj_t obj_c410x_get_sensor_reading_rq;
  fiid_obj_t obj_c410x_get_sensor_reading_rs;
  fiid_obj_t obj_c410x_slot_power_control_rq;
  fiid_obj_t obj_c410x_slot_power_control_rs;
  fiid_obj_t obj_close_session_rq;
  fiid_obj_t obj_close_session_rs;

  List sockets_to_close;

  /* for oem power control ; extra arg passed in via "+extra" at end of hostname */
  char *extra_arg;

  /* for oem power control to the same node */
  struct ipmipower_powercmd *next;
};

struct ipmipower_connection_extra_arg
{
  struct ipmipower_connection_extra_arg *next;
  char *extra_arg;
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
  unsigned int ipmi_requester_sequence_number_counter;
  unsigned int ping_sequence_number_counter;
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
  /* for oem power types ; extra arg passed in via "+extra" at end of hostname */
  struct ipmipower_connection_extra_arg *extra_args;
  struct sockaddr_in destaddr;

  /* for eliminate option */
  int skip;
};

typedef struct ipmipower_powercmd *ipmipower_powercmd_t;
typedef struct ipmipower_connection *ipmipower_connection_t;

enum ipmipower_argp_option_keys
  {
    IPMI_VERSION_KEY = 160,       /* legacy option */
    RMCPDUMP_KEY = 161,

    ON_KEY = 'n',
    OFF_KEY = 'f',
    CYCLE_KEY = 'c',
    RESET_KEY = 'r',
    STAT_KEY = 's',
    PULSE_KEY = 162,
    SOFT_KEY = 163,
    ON_IF_OFF_KEY = 164,
    WAIT_UNTIL_OFF_KEY = 165,
    WAIT_UNTIL_ON_KEY = 166,
    OEM_POWER_TYPE_KEY = 167,

    RETRY_WAIT_TIMEOUT_KEY = 168,
    RETRANSMISSION_WAIT_TIMEOUT_KEY = 169,
    RETRY_BACKOFF_COUNT_KEY = 170,
    RETRANSMISSION_BACKOFF_COUNT_KEY = 171,
    PING_INTERVAL_KEY = 172,
    PING_TIMEOUT_KEY = 173,
    PING_PACKET_COUNT_KEY = 174,
    PING_PERCENT_KEY = 175,
    PING_CONSEC_COUNT_KEY = 176,
  };

struct ipmipower_arguments
{
  struct common_cmd_args common;
  struct hostrange_cmd_args hostrange;
#ifndef NDEBUG
  int rmcpdump;
#endif /* NDEBUG */

  power_cmd_t powercmd;
  int on_if_off;
  int wait_until_on;
  int wait_until_off;
  oem_power_type_t oem_power_type;

  unsigned int retransmission_wait_timeout;
  unsigned int retransmission_backoff_count;
  unsigned int ping_interval;
  unsigned int ping_timeout;
  unsigned int ping_packet_count;
  unsigned int ping_percent;
  unsigned int ping_consec_count;
};

#endif /* _IPMIPOWER_H */
