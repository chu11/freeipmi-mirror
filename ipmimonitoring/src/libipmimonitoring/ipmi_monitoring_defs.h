/*****************************************************************************\
 *  $Id: ipmi_monitoring_defs.h,v 1.4.10.2 2007-08-23 23:24:34 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMI_MONITORING_DEFS_H
#define _IPMI_MONITORING_DEFS_H

#include <stdint.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <freeipmi/freeipmi.h>

#include "ipmi_sdr_cache.h"
#include "list.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

/* +1 and -1 to avoid gcc warnings */
#define IPMI_MONITORING_SENSOR_GROUP_VALID(__val) \
        ((((__val) + 1) >= (IPMI_MONITORING_SENSOR_GROUP_TEMPERATURE + 1) \
          && ((__val) - 1) <= (IPMI_MONITORING_SENSOR_GROUP_UNKNOWN - 1)) ? 1 : 0)

#define IPMI_MONITORING_SENSOR_STATE_VALID(__val) \
        (((__val) == IPMI_MONITORING_SENSOR_STATE_NOMINAL \
          || (__val) == IPMI_MONITORING_SENSOR_STATE_WARNING \
          || (__val) == IPMI_MONITORING_SENSOR_STATE_CRITICAL \
          || (__val) == IPMI_MONITORING_SENSOR_STATE_UNKNOWN) ? 1 : 0)

/* +1 and -1 to avoid gcc warnings */
#define IPMI_MONITORING_SENSOR_UNITS_VALID(__val) \
        ((((__val) + 1) >= (IPMI_MONITORING_SENSOR_UNITS_NONE + 1) \
          && ((__val) - 1) <= (IPMI_MONITORING_SENSOR_UNITS_UNKNOWN - 1)) ? 1 : 0)

#define IPMI_MONITORING_SENSOR_READING_TYPE_VALID(__val) \
        (((__val) == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL \
          || (__val) == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32 \
          || (__val) == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE \
          || (__val) == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER16_BITMASK \
          || (__val) == IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN) ? 1 : 0)

/* +1 and -1 to avoid gcc warnings */
#define IPMI_MONITORING_SENSOR_BITMASK_TYPE_VALID(__val) \
        ((((__val) + 1) >= (IPMI_MONITORING_SENSOR_BITMASK_TYPE_TRANSITION + 1) \
          && ((__val) - 1) <= (IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN - 1)) ? 1 : 0)

#define IPMI_MONITORING_MAX_USER_NAME_LENGTH IPMI_MAX_USER_NAME_LENGTH

#define IPMI_MONITORING_MAX_PASSWORD_LENGTH  IPMI_MAX_1_5_PASSWORD_LENGTH

#define IPMI_MONITORING_PRIVILEGE_VALID(__val) \
        (((__val) == IPMI_MONITORING_PRIVILEGE_USER \
          || (__val) == IPMI_MONITORING_PRIVILEGE_OPERATOR \
          || (__val) == IPMI_MONITORING_PRIVILEGE_ADMIN) ? 1 : 0)

#define IPMI_MONITORING_AUTHENTICATION_TYPE_VALID(__val) \
        (((__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_NONE \
          || (__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY \
          || (__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_MD2 \
          || (__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_MD5) ? 1 : 0)

#define IPMI_MONITORING_FLAGS_MASK \
  (IPMI_MONITORING_FLAGS_NONE \
   | IPMI_MONITORING_FLAGS_DEBUG_STDOUT \
   | IPMI_MONITORING_FLAGS_DEBUG_STDERR \
   | IPMI_MONITORING_FLAGS_DEBUG_SYSLOG \
   | IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS \
   | IPMI_MONITORING_FLAGS_LOCK_MEMORY)

#define IPMI_MONITORING_WORKAROUND_FLAGS_MASK \
  (IPMI_MONITORING_WORKAROUND_FLAGS_SESSION_ID_ZERO \
   | IPMI_MONITORING_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES)

#define IPMI_MONITORING_SENSOR_READING_FLAGS_MASK \
  (IPMI_MONITORING_SENSOR_READING_FLAGS_REREAD_SDR_CACHE \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_UNREADABLE_SENSORS)


#define IPMI_MONITORING_LAN_INITIAL_OUTBOUND_SEQUENCE_NUMBER  1
#define IPMI_MONITORING_SEQUENCE_NUMBER_WINDOW                8
#define IPMI_MONITORING_PREVIOUSLY_RECEIVED_LIST_INIT         0xFF
#define IPMI_MONITORING_SESSION_TIMEOUT_LENGTH_DEFAULT        20000
#define IPMI_MONITORING_RETRANSMISSION_TIMEOUT_LENGTH_DEFAULT 500
#define IPMI_MONITORING_RETRANSMISSION_BACKOFF_COUNT_DEFAULT  2

#define IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH      32
#define IPMI_MONITORING_MAX_SDR_RECORD_LENGTH       1024

#define IPMI_MONITORING_COMMUNICATION_UNINITIALIZED 0
#define IPMI_MONITORING_COMMUNICATION_INBAND        1
#define IPMI_MONITORING_COMMUNICATION_OUTOFBAND     2

#define IPMI_MONITORING_PRIVILEGE_LEVEL_DEFAULT              IPMI_PRIVILEGE_LEVEL_USER
#define IPMI_MONITORING_AUTHENTICATION_TYPE_DEFAULT          IPMI_AUTHENTICATION_TYPE_MD5

#define IPMI_MONITORING_MAGIC         0xABCD9876

#define IPMI_MONITORING_PACKET_BUFLEN 1024

struct ipmi_monitoring_communication {
  int communication_type;

  /* Inband Variables */
  ipmi_kcs_ctx_t kcs_ctx;

  /* OutofBand Variables */
  char hostname[MAXHOSTNAMELEN+1];
  uint8_t username[IPMI_MAX_USER_NAME_LENGTH+1];
  uint8_t password[IPMI_1_5_MAX_PASSWORD_LENGTH+1];
  uint8_t privilege_level;
  uint8_t authentication_type;  
  unsigned int session_timeout_len;
  unsigned int retransmission_timeout_len;
  unsigned int retransmission_backoff_count;
  uint32_t workaround_flags;

  int ipmi_fd;
  List sockets_to_close;
  struct sockaddr_in addr;
  struct timeval last_ipmi_packet_sent;
  struct timeval last_ipmi_packet_received;

  uint32_t retransmission_count;
  uint32_t highest_received_sequence_number;
  unsigned int previously_received_list;
  uint8_t requester_sequence_number;
  uint32_t session_inbound_count;

  fiid_obj_t obj_rmcp_hdr_rq;
  fiid_obj_t obj_rmcp_hdr_rs;
  fiid_obj_t obj_lan_session_hdr_rq;
  fiid_obj_t obj_lan_session_hdr_rs;
  fiid_obj_t obj_lan_msg_hdr_rq;
  fiid_obj_t obj_lan_msg_hdr_rs;
  fiid_obj_t obj_lan_msg_trlr_rs;
  fiid_obj_t obj_get_channel_authentication_capabilities_rq;
  fiid_obj_t obj_get_channel_authentication_capabilities_rs;
  fiid_obj_t obj_get_session_challenge_rq;
  fiid_obj_t obj_get_session_challenge_rs;
  fiid_obj_t obj_activate_session_rq;
  fiid_obj_t obj_activate_session_rs;
  fiid_obj_t obj_set_session_privilege_level_rq;
  fiid_obj_t obj_set_session_privilege_level_rs;
  fiid_obj_t obj_close_session_rq;
  fiid_obj_t obj_close_session_rs;
};

struct ipmi_sensor_config {
  char *option_str;
  int sensor_state;
};

struct ipmi_monitoring_sensor_reading {
  int record_id;
  int sensor_group;
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH];
  int sensor_state;
  int sensor_units;
  int sensor_reading_type;
  int sensor_bitmask_type;
  union {
    uint8_t bool_val;
    uint32_t integer_val;
    double double_val;
    uint16_t integer_bitmask_val;
  } sensor_reading;    
};

struct ipmi_monitoring_ctx {
  uint32_t magic;
  unsigned int errnum;

  ipmi_sdr_cache_ctx_t sc;

  struct ipmi_monitoring_communication comm;

  List sensor_readings;
  ListIterator sensor_readings_itr;
  struct ipmi_monitoring_sensor_reading *current_sensor_reading;
};

#endif /* _IPMI_MONITORING_DEFS_H */
