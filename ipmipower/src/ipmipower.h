/*****************************************************************************\
 *  $Id: ipmipower.h,v 1.8.2.6 2005-11-08 23:50:49 chu11 Exp $
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
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
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

#include "freeipmi.h"
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

/* RMCP ASF Presence ping message tag 
 * - Use max 0xFE, b/c 0xFF indicates a unidirectional packet
 */
#define IPMIPOWER_PING_TAG_MAX         0xFE

/* IPMI Requester Sequence Number
 * - Requester Sequence Number is 6 bits, not 8 
 */
#define IPMIPOWER_RSEQ_MAX             0x3F

/* IPMI allowable sequence number range window
 */
#define IPMIPOWER_SEQ_NUM_WINDOW       8

/* MISC */
#define IPMI_PACKET_BUFLEN             1024
#define RMCP_PACKET_BUFLEN             1024

#define IPMIPOWER_HOSTLIST_BUFLEN      65536

#define IPMIPOWER_DEFAULT_LOGFILE      "/tmp/ipmipower.%d"

/* ipmipower_bool_t
 * - boolean type
 */
typedef enum { IPMIPOWER_TRUE  = 1,
               IPMIPOWER_FALSE = 0} ipmipower_bool_t;

/* power_cmd_t 
 * - power control commands 
 */
typedef enum { POWER_CMD_NONE             = 0x00,
               POWER_CMD_POWER_OFF        = 0x01,
               POWER_CMD_POWER_ON         = 0x02,
               POWER_CMD_POWER_CYCLE      = 0x03,
               POWER_CMD_POWER_RESET      = 0x04,
               POWER_CMD_POWER_STATUS     = 0x05,
               POWER_CMD_PULSE_DIAG_INTR  = 0x06, 
               POWER_CMD_SOFT_SHUTDOWN_OS = 0x07} power_cmd_t;
#define POWER_CMD_VALID(c)             (c > POWER_CMD_NONE && \
                                        c <= POWER_CMD_SOFT_SHUTDOWN_OS)

/* packet_type_t
 * - packet types stored internally in an ipmipower_powercmd structure.
 * - Request types are *_REQ, Response types are *_RES
 */
typedef enum { AUTH_REQ = 0x11, AUTH_RES = 0x21,
               SESS_REQ = 0x12, SESS_RES = 0x22,
               ACTV_REQ = 0x13, ACTV_RES = 0x23,
               PRIV_REQ = 0x14, PRIV_RES = 0x24, 
               CLOS_REQ = 0x15, CLOS_RES = 0x25,
               CHAS_REQ = 0x16, CHAS_RES = 0x26,
               CTRL_REQ = 0x17, CTRL_RES = 0x27 } packet_type_t;
#define PACKET_TYPE_REQ_MASK           0x10
#define PACKET_TYPE_RES_MASK           0x20
#define PACKET_TYPE_PKT_MASK           0x07
#define PACKET_TYPE_VALID_REQ(p)       ((p & PACKET_TYPE_REQ_MASK) && \
                                        (p & PACKET_TYPE_PKT_MASK))
#define PACKET_TYPE_VALID_RES(p)       ((p & PACKET_TYPE_RES_MASK) && \
                                        (p & PACKET_TYPE_PKT_MASK))
#define PACKET_TYPE_VALID_PKT(p)       (PACKET_TYPE_VALID_REQ(p) || \
                                        PACKET_TYPE_VALID_RES(p))

/* Protocol States */
typedef enum { PROTOCOL_STATE_START     = 0x00,
               PROTOCOL_STATE_AUTH_SENT = 0x01,
               PROTOCOL_STATE_SESS_SENT = 0x02,
               PROTOCOL_STATE_ACTV_SENT = 0x03,
               PROTOCOL_STATE_PRIV_SENT = 0x04,
               PROTOCOL_STATE_CHAS_SENT = 0x05,
               PROTOCOL_STATE_CTRL_SENT = 0x06,
               PROTOCOL_STATE_CLOS_SENT = 0x07,
               PROTOCOL_STATE_END       = 0x08 } protocol_state_t;
#define PROTOCOL_STATE_VALID(s)       (s >= PROTOCOL_STATE_START && \
                                       s <= PROTOCOL_STATE_END)

/* Discovery States */
typedef enum { STATE_DISCOVERED     = 0x01,
               STATE_UNDISCOVERED   = 0x02,
               STATE_BADCONNECTION  = 0x03} discover_state_t;
#define DISCOVER_STATE_VALID(s)       (s >= STATE_DISCOVERED && \
                                       s <= STATE_BADCONNECTION)

/* Link States */
typedef enum { LINK_GOOD = 0x01,
               LINK_BAD  = 0x02} link_state_t;
#define LINK_STATE_VALID(s)           (s >= LINK_GOOD && \
                                       s <= LINK_BAD)

/* Authentication Types */
typedef enum { AUTH_TYPE_INVALID             = 0x00,
	       AUTH_TYPE_AUTO                = 0x01,
               AUTH_TYPE_NONE                = 0x02,
               AUTH_TYPE_STRAIGHT_PASSWD_KEY = 0x03,
               AUTH_TYPE_MD2                 = 0x04,
               AUTH_TYPE_MD5                 = 0x05 } auth_type_t;
#define AUTH_TYPE_VALID(a)            (a >= AUTH_TYPE_NONE && \
                                       a <= AUTH_TYPE_MD5)
#define AUTH_TYPE_VALID_OR_AUTO(a)    (a >= AUTH_TYPE_AUTO && \
				       a <= AUTH_TYPE_MD5)

/* Output Types */
typedef enum { OUTPUT_TYPE_INVALID   = 0,
               OUTPUT_TYPE_NONE      = 1,
               OUTPUT_TYPE_NEWLINE   = 2,
               OUTPUT_TYPE_HOSTRANGE = 3} output_type_t;
#define OUTPUT_TYPE_VALID(o)          (o >= OUTPUT_TYPE_NONE && \
                                       o <= OUTPUT_TYPE_HOSTRANGE)

/* Msg Types */
typedef enum { MSG_TYPE_SUCCESS                 =  0,
               MSG_TYPE_ON                      =  1,
               MSG_TYPE_OFF                     =  2,
               MSG_TYPE_OK                      =  3,
               MSG_TYPE_PERMISSION              =  4,
               MSG_TYPE_USERNAME                =  5,
               MSG_TYPE_PASSWORD                =  6,
               MSG_TYPE_PRIVILEGE               =  7,
               MSG_TYPE_OPERATION               =  8,
               MSG_TYPE_AUTHTYPE                =  9,
	       MSG_TYPE_AUTHAUTO                = 10,
               MSG_TYPE_TIMEDOUT                = 11,
               MSG_TYPE_NOTDISCOVERED           = 12,
               MSG_TYPE_BADCONNECTION           = 13,
               MSG_TYPE_UNKNOWNNODE             = 14,
               MSG_TYPE_RESOURCES               = 15,
               MSG_TYPE_BMCBUSY                 = 16,
               MSG_TYPE_BMCERROR                = 17 } msg_type_t;
#define MSG_TYPE_VALID(m)             (m >= MSG_TYPE_SUCCESS && \
                                       m <= MSG_TYPE_BMCERROR)
#define MSG_TYPE_NUM                  (MSG_TYPE_BMCERROR+1)


/* ipmipower_powercmd
 * - Stores all information needed to execute a power command
 */
struct ipmipower_powercmd {
    power_cmd_t cmd;
    protocol_state_t protocol_state; 

    struct timeval time_begin;
    unsigned int session_inbound_count;
    u_int32_t initial_outbound_seq_num;
    u_int32_t highest_received_seq_num;
    unsigned int previously_received_list;
    unsigned int retry_count;
    ipmipower_bool_t error_occurred;
    ipmipower_bool_t permsgauth_enabled;
    u_int8_t authtype;
    u_int8_t privilege;
    u_int8_t close_timeout;

    struct ipmipower_connection *ic;
  
    fiid_obj_t rmcp_req;
    fiid_obj_t rmcp_res;
    fiid_obj_t session_req;
    fiid_obj_t session_res;
    fiid_obj_t msg_req;
    fiid_obj_t msg_res;
    fiid_obj_t trlr_res;

    fiid_obj_t auth_req;
    fiid_obj_t auth_res;
    fiid_obj_t sess_req;
    fiid_obj_t sess_res;
    fiid_obj_t actv_req;
    fiid_obj_t actv_res;
    fiid_obj_t priv_req;
    fiid_obj_t priv_res;
    fiid_obj_t clos_req;
    fiid_obj_t clos_res;
    fiid_obj_t chas_req;
    fiid_obj_t chas_res;
    fiid_obj_t ctrl_req;
    fiid_obj_t ctrl_res;

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
  u_int32_t ipmi_requester_seq_num_counter;
  u_int32_t ping_seq_num_counter;
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
  hostlist_t        hosts;
  int               hosts_count;
  char              username[IPMI_SESSION_MAX_USERNAME_LEN+1];
  char              password[IPMI_SESSION_MAX_AUTH_CODE_LEN+1];
  power_cmd_t       powercmd;
  char              configfile[MAXPATHLEN+1];

  auth_type_t       authtype;
  ipmipower_bool_t  on_if_off;
  output_type_t     outputtype;
#ifndef NDEBUG
  ipmipower_bool_t  debug;
  ipmipower_bool_t  ipmidump;
  ipmipower_bool_t  rmcpdump;
  ipmipower_bool_t  log;
  char              logfile[MAXPATHLEN+1];
  int               logfile_fd;
#endif
  int               timeout_len;
  int               retry_timeout_len;
  int               retry_backoff_count; 
  int               ping_interval_len;
  int               ping_timeout_len;
  int               ping_packet_count;
  int               ping_percent;
  int               ping_consec_count;

  /* Flags indicating if option was set on the command line */
  ipmipower_bool_t  hosts_set;
  ipmipower_bool_t  username_set;
  ipmipower_bool_t  password_set;
  ipmipower_bool_t  authtype_set;
  ipmipower_bool_t  on_if_off_set;
  ipmipower_bool_t  outputtype_set;
  ipmipower_bool_t  timeout_len_set;
  ipmipower_bool_t  retry_timeout_len_set;
  ipmipower_bool_t  retry_backoff_count_set;
  ipmipower_bool_t  ping_interval_len_set;
  ipmipower_bool_t  ping_timeout_len_set; 
  ipmipower_bool_t  ping_consec_count_set;
  ipmipower_bool_t  ping_packet_count_set;
  ipmipower_bool_t  ping_percent_set;
};

typedef struct ipmipower_powercmd *ipmipower_powercmd_t;
typedef struct ipmipower_connection *ipmipower_connection_t;

#endif /* _IPMIPOWER_H */
