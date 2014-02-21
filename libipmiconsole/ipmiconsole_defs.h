/*****************************************************************************\
 *  $Id: ipmiconsole_defs.h,v 1.80 2010-06-10 22:10:12 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
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

#include "scbuf.h"

#ifndef IPMICONSOLE_DEFS_H
#define IPMICONSOLE_DEFS_H

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

/* +5 for digits (max 65535) and +1 for colon ':' */
#define MAXHOSTNAMELEN_WITH_PORT (MAXHOSTNAMELEN + 6)

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

typedef enum
  {
    IPMICONSOLE_PROTOCOL_STATE_START                                    = 0x00,
    IPMICONSOLE_PROTOCOL_STATE_GET_AUTHENTICATION_CAPABILITIES_SENT     = 0x01,
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
    IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ     = 0x00,
    IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS     = 0x01,
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

#define IPMICONSOLE_PACKET_TYPE_REQUEST(__p)                                \
  (((__p) == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RQ     \
    || (__p) == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_REQUEST                \
    || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_1                      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_3                      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RQ      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RQ    \
    || (__p) == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RQ                 \
    || (__p) == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RQ                 \
    || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RQ      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RQ               \
    || (__p) == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RQ) ? 1 : 0)

#define IPMICONSOLE_PACKET_TYPE_RESPONSE(__p)                               \
  (((__p) == IPMICONSOLE_PACKET_TYPE_GET_AUTHENTICATION_CAPABILITIES_RS     \
    || (__p) == IPMICONSOLE_PACKET_TYPE_OPEN_SESSION_RESPONSE               \
    || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_2                      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_RAKP_MESSAGE_4                      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_SUPPORT_RS      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_GET_PAYLOAD_ACTIVATION_STATUS_RS    \
    || (__p) == IPMICONSOLE_PACKET_TYPE_ACTIVATE_PAYLOAD_RS                 \
    || (__p) == IPMICONSOLE_PACKET_TYPE_SOL_PAYLOAD_DATA_RS                 \
    || (__p) == IPMICONSOLE_PACKET_TYPE_GET_CHANNEL_PAYLOAD_VERSION_RS      \
    || (__p) == IPMICONSOLE_PACKET_TYPE_DEACTIVATE_PAYLOAD_RS               \
    || (__p) == IPMICONSOLE_PACKET_TYPE_CLOSE_SESSION_RS) ? 1 : 0)

#define IPMICONSOLE_PACKET_TYPE_VALID(__p)       \
  ((IPMICONSOLE_PACKET_TYPE_REQUEST (__p)        \
    || IPMICONSOLE_PACKET_TYPE_RESPONSE (__p)) ? 1 : 0)

#define IPMICONSOLE_THREAD_COUNT_DEFAULT                            4

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

#define IPMI_MAX_SIK_KEY_LENGTH                               64
#define IPMI_MAX_INTEGRITY_KEY_LENGTH                         64
#define IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH                   64
#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH      64

#define IPMI_SESSION_INITIAL_OUTBOUND_SEQUENCE_NUMBER              1
#define IPMI_SOL_SESSION_INITIAL_PACKET_SEQUENCE_NUMBER            1

/* API magic determines if the context has been destroyed by the user
 * and can no longer be used.  However, it may not necessarily have
 * been garbage cleaned up by the libipmiconsole engine library.
 */
#define IPMICONSOLE_CTX_MAGIC                 0x74AB8831
#define IPMICONSOLE_CTX_API_MAGIC             0x83FB9202

#define IPMICONSOLE_PACKET_BUFLEN             16384

#define IPMICONSOLE_MIN_CHARACTER_DATA        1
#define IPMICONSOLE_MAX_CHARACTER_DATA        255

#define IPMICONSOLE_PIPE_GENERATE_BREAK_CODE  0x01

#define IPMICONSOLE_DEBUG_MASK         \
  (IPMICONSOLE_DEBUG_STDOUT            \
   | IPMICONSOLE_DEBUG_STDERR          \
   | IPMICONSOLE_DEBUG_SYSLOG          \
   | IPMICONSOLE_DEBUG_FILE            \
   | IPMICONSOLE_DEBUG_IPMI_PACKETS)

#define IPMICONSOLE_WORKAROUND_MASK                         \
  (IPMICONSOLE_WORKAROUND_AUTHENTICATION_CAPABILITIES       \
   | IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION               \
   | IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0_SESSION          \
   | IPMICONSOLE_WORKAROUND_SUN_2_0_SESSION                 \
   | IPMICONSOLE_WORKAROUND_OPEN_SESSION_PRIVILEGE          \
   | IPMICONSOLE_WORKAROUND_NON_EMPTY_INTEGRITY_CHECK_VALUE \
   | IPMICONSOLE_WORKAROUND_NO_CHECKSUM_CHECK               \
   | IPMICONSOLE_WORKAROUND_SERIAL_ALERTS_DEFERRED          \
   | IPMICONSOLE_WORKAROUND_INCREMENT_SOL_PACKET_SEQUENCE   \
   | IPMICONSOLE_WORKAROUND_IGNORE_SOL_PAYLOAD_SIZE         \
   | IPMICONSOLE_WORKAROUND_IGNORE_SOL_PORT                 \
   | IPMICONSOLE_WORKAROUND_SKIP_SOL_ACTIVATION_STATUS      \
   | IPMICONSOLE_WORKAROUND_SKIP_CHANNEL_PAYLOAD_SUPPORT)

#define IPMICONSOLE_ENGINE_MASK                    \
  (IPMICONSOLE_ENGINE_CLOSE_FD                     \
   | IPMICONSOLE_ENGINE_OUTPUT_ON_SOL_ESTABLISHED  \
   | IPMICONSOLE_ENGINE_LOCK_MEMORY                \
   | IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE           \
   | IPMICONSOLE_ENGINE_SERIAL_KEEPALIVE_EMPTY)

#define IPMICONSOLE_BEHAVIOR_MASK           \
  (IPMICONSOLE_BEHAVIOR_ERROR_ON_SOL_INUSE  \
   | IPMICONSOLE_BEHAVIOR_DEACTIVATE_ONLY   \
   | IPMICONSOLE_BEHAVIOR_DEACTIVATE_ALL_INSTANCES)

#define IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_ESTABLISHED 0x1
#define IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_ERROR       0x2
#define IPMICONSOLE_BLOCKING_NOTIFICATION_SOL_SESSION_DEACTIVATED 0x3

/* Protocol/User Config Data */
struct ipmiconsole_ctx_config {

  /* ipmi config */
  char hostname[MAXHOSTNAMELEN+1];
  uint16_t port;
  char username[IPMI_MAX_USER_NAME_LENGTH+1];
  char password[IPMI_2_0_MAX_PASSWORD_LENGTH+1];
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
  unsigned int k_g_len;
  uint8_t privilege_level;
  uint8_t cipher_suite_id;
  unsigned int workaround_flags;

  /* protocol config */
  unsigned int session_timeout_len;
  unsigned int retransmission_timeout_len;
  unsigned int retransmission_backoff_count;
  unsigned int keepalive_timeout_len;
  unsigned int retransmission_keepalive_timeout_len;
  unsigned int acceptable_packet_errors_count;
  unsigned int maximum_retransmission_count;

  /* engine config */
  unsigned int engine_flags;
  unsigned int behavior_flags;
  unsigned int debug_flags;

  /* advanced config */
  unsigned int sol_payload_instance;

  /* Data based on Configuration Parameters */
  uint8_t authentication_algorithm;
  uint8_t integrity_algorithm;
  uint8_t confidentiality_algorithm;
};

/* Sockets, pipes, objects, etc. used for data in a SOL session */
struct ipmiconsole_ctx_connection {

  /* File Descriptor User Interface */
  int user_fd;                  /* never touched by this library */
  int ipmiconsole_fd;
  scbuf_t console_remote_console_to_bmc;
  scbuf_t console_bmc_to_remote_console;

  /* Connection Data */
  int ipmi_fd;
  scbuf_t ipmi_from_bmc;
  scbuf_t ipmi_to_bmc;

  /* Pipe for non-fd communication: from API to engine */
  int asynccomm[2];

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

  fiid_obj_t obj_authentication_capabilities_rq;
  fiid_obj_t obj_authentication_capabilities_rs;
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
};

/*
 * IPMI Session Information - actual data used to keep track of a SOL
 * session.  Separated from ipmiconsole_ctx_connection above, b/c
 * everything below will need to be re-initialized if the session is
 * being reattempted under a different port.
 */
struct ipmiconsole_ctx_session {
  int16_t console_port;

  struct sockaddr_in addr;

  /* Session timeout, retransmission timeout, keepalive timeout maintenance */
  struct timeval last_ipmi_packet_sent;
  struct timeval last_ipmi_packet_received;
  struct timeval last_keepalive_packet_sent;

  /* Serial keepalive timeout maintenance */
  struct timeval last_sol_packet_received;
 
  /*
   * Protocol State Machine Variables
   */
  ipmiconsole_protocol_state_t protocol_state;
  int close_session_flag;
  int try_new_port_flag;
  int deactivate_payload_instances;
  /* if deactivate_payload_instances_and_try_again_flag set,
   * deactivate_payload_instances should always be set, but not vice
   * versa
   */
  int deactivate_payload_instances_and_try_again_flag;
  int close_timeout_flag;
  int deactivate_only_succeeded_flag;

  /*
   * Protocol Maintenance Variables
   */
  unsigned int retransmission_count;
  unsigned int workaround_retransmission_count; /* For IPMICONSOLE_WORKAROUND_INCREMENT_SOL_PACKET_SEQUENCE */
  unsigned int errors_count;
  unsigned int session_sequence_number_errors_count;
  unsigned int activate_payloads_count;
  unsigned int deactivate_active_payloads_count;
  uint32_t highest_received_sequence_number;
  /* need to also store bytes read from a previous seq num */
  uint32_t previously_received_list;

  uint8_t message_tag;
  uint8_t requester_sequence_number;
  uint32_t session_sequence_number;
  uint8_t name_only_lookup;
  uint32_t remote_console_session_id;
  uint8_t remote_console_random_number[IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH];

  uint8_t sik_key[IPMI_MAX_SIK_KEY_LENGTH];
  void *sik_key_ptr;
  unsigned int sik_key_len;
  uint8_t integrity_key[IPMI_MAX_INTEGRITY_KEY_LENGTH];
  void *integrity_key_ptr;
  unsigned int integrity_key_len;
  uint8_t confidentiality_key[IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH];
  void *confidentiality_key_ptr;
  unsigned int confidentiality_key_len;

  uint32_t sol_instance_capacity;
  uint8_t sol_instances_activated[IPMI_INSTANCES_ACTIVATED_LENGTH];
  uint32_t sol_instances_activated_count;
  uint32_t sol_instances_deactivated_count;

  uint8_t max_sol_character_send_size; /* determine during session setup */

  /* Serial Break Maintenance */
  int break_requested;
  unsigned int console_remote_console_to_bmc_bytes_before_break;

  /* SOL Input (remote console to BMC) */
  int sol_input_waiting_for_ack;
  int sol_input_waiting_for_break_ack;
  struct timeval last_sol_input_packet_sent;
  uint8_t sol_input_packet_sequence_number;
  uint8_t sol_input_character_data[IPMICONSOLE_MAX_CHARACTER_DATA+1];
  unsigned int sol_input_character_data_len;

  /* SOL Output (BMC to remote console) */
  uint8_t last_sol_output_packet_sequence_number;
  uint8_t last_sol_output_accepted_character_count;
};

/* Context debug stuff */
struct ipmiconsole_ctx_debug {
  int debug_fd;
};

/* Mutexes + flags for signaling between the API and engine */
struct ipmiconsole_ctx_signal {
  /* Conceptually there is not a race with the status.  The API initializes
   * the status, and the engine is the only one that modifies it.
   *
   * However, there is a tiny race in
   * ipmiconsole_engine_submit{_block}().  Conceptually, the status
   * could be set before we even initialize the status to SUBMITTED.
   */
  pthread_mutex_t status_mutex;
  unsigned int status;

  /* user_has_destroyed - flags and mutex used when the user has
   * destroyed the context and it is now the responsibility of the
   * engine/garbage-collector to cleanup.  Need to mutex to avoid
   * destroy races.
   */
  pthread_mutex_t destroyed_mutex;
  unsigned int user_has_destroyed;
  unsigned int moved_to_destroyed;
};

/* non-blocking potential parameters */
struct ipmiconsole_ctx_non_blocking {
  Ipmiconsole_callback callback;
  void *callback_arg;
};

/* Info, pipe, and mutex for engine submission blocking */
struct ipmiconsole_ctx_blocking {
  /* Conceptually, it is impossible for both to ever be touched
   * simultaneously so a mutex may not seem necessary.
   *
   * blocking_submit_requested is initialized/set in API land, and
   * then later read in engine land after the context is submitted.
   * It is never read again in API land and never written to in engine
   * land.
   *
   * sol_session_established is initialied in API land, afterwards it
   * is only written/read in the engine after a context is submitted.
   *
   * after initialization, the API and Engine only touch their
   * ends of the pipe.
   *
   * However, there is a tiny race that is possible.  After the
   * session is submitted, the blocking code in _ipmiconsole_block()
   * could fail, such as in the call to select().  We do not want the
   * engine and API to race reading/writing under this circumstance.
   */
  pthread_mutex_t blocking_mutex;
  int blocking_submit_requested;
  int blocking_notification[2];
  int sol_session_established;
};

struct ipmiconsole_ctx_fds {
  /* Copy from ipmiconsole_ctx_session, these file descriptors are
   * managed exclusively by API level, not the engine.
   *
   * The need to manage asynccomm at the API level is b/c users could
   * access it via ipmiconsole_ctx_generate_break().  If one end of
   * the asynccomm is closed by the engine, it becomes difficult to
   * know if we can actually generate a break.
   *
   * We could manage this situation through some mutexes, but that would
   * slow down closing/generate-break code.  We could capture EPIPE in the
   * API and return a "IS_CLOSING" error to the user, but that would require
   * the user to set SIGPIPE to SIG_IGN.  Moving it to all be managed in
   * the API level is best.  We just have to check for POLLNVAL in the
   * engine poll().
   *
   */
  int user_fd;
  int asynccomm[2];
};

struct ipmiconsole_ctx {
  /* Two magics - first indicates the context is still valid.  Second
   * is pretty much a flag that indicates the context has been
   * "destroyed" in API land, and should no longer be used by the API.
   */
  uint32_t magic;
  uint32_t api_magic;
  pthread_mutex_t errnum_mutex;
  int errnum;
  int errnum_retrieved;

  struct ipmiconsole_ctx_config config;

  struct ipmiconsole_ctx_debug debug;

  struct ipmiconsole_ctx_signal signal;

  struct ipmiconsole_ctx_non_blocking non_blocking;

  struct ipmiconsole_ctx_blocking blocking;

  struct ipmiconsole_ctx_connection connection;

  struct ipmiconsole_ctx_session session;

  struct ipmiconsole_ctx_fds fds;

  /* session_submitted - flag indicates context submitted to engine
   * successfully.  Does not indicate any state of success/failure for
   * either blocking or non-blocking submissions.  Primary used as a
   * flag so other functions such as ipmiconsole_ctx_fd() and
   * ipmiconsole_generate_break() know that they are capable of
   * moving on.
   *
   * Note, does not require a mutex.  Only a flag used in API-land.
   * Engine threads will never touch this.
   */
  unsigned int session_submitted;
};

#endif /* IPMICONSOLE_DEFS_H */
