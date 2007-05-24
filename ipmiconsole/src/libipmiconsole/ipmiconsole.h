/*****************************************************************************\
 *  $Id: ipmiconsole.h,v 1.11 2007-05-24 14:34:14 chu11 Exp $
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

#ifndef _IPMICONSOLE_H
#define _IPMICONSOLE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/freeipmi.h>

#define IPMICONSOLE_ERR_SUCCESS                     0
#define IPMICONSOLE_ERR_CONTEXT_NULL                1
#define IPMICONSOLE_ERR_CONTEXT_INVALID             2
#define IPMICONSOLE_ERR_ALREADY_SETUP               3
#define IPMICONSOLE_ERR_NOT_SETUP                   4
#define IPMICONSOLE_ERR_CTX_ALREADY_SUBMITTED       5
#define IPMICONSOLE_ERR_CTX_NOT_SUBMITTED           6
#define IPMICONSOLE_ERR_CTX_IS_SUBMITTED            7
#define IPMICONSOLE_ERR_CTX_IS_WAITING              8
#define IPMICONSOLE_ERR_PARAMETERS                  9
#define IPMICONSOLE_ERR_IPMI_2_0_UNAVAILABLE       10
#define IPMICONSOLE_ERR_CIPHER_SUITE_UNAVAILABLE   11
#define IPMICONSOLE_ERR_HOSTNAME_INVALID           12 
#define IPMICONSOLE_ERR_USERNAME_INVALID           13
#define IPMICONSOLE_ERR_PASSWORD_INVALID           14
#define IPMICONSOLE_ERR_K_G_INVALID                15
#define IPMICONSOLE_ERR_PRIVILEGE_INVALID          16
#define IPMICONSOLE_ERR_CIPHER_SUITE_INVALID       17
#define IPMICONSOLE_ERR_SOL_UNAVAILABLE            18
#define IPMICONSOLE_ERR_SOL_INUSE                  19
#define IPMICONSOLE_ERR_SOL_NOT_RESPONDING         20
#define IPMICONSOLE_ERR_SOL_STOLEN                 21
#define IPMICONSOLE_ERR_SOL_REQUIRES_ENCRYPTION    22
#define IPMICONSOLE_ERR_SOL_REQUIRES_NO_ENCRYPTION 23
#define IPMICONSOLE_ERR_BMC_BUSY                   24
#define IPMICONSOLE_ERR_BMC_ERROR                  25
#define IPMICONSOLE_ERR_SESSION_TIMEOUT            26
#define IPMICONSOLE_ERR_OUT_OF_MEMORY              27
#define IPMICONSOLE_ERR_SYSTEM_ERROR               28
#define IPMICONSOLE_ERR_INTERNAL                   29
#define IPMICONSOLE_ERR_ERRNUMRANGE                30

/* 
 * IPMI Privilege Constants
 */
#define IPMICONSOLE_PRIVILEGE_USER                0
#define IPMICONSOLE_PRIVILEGE_OPERATOR            1
#define IPMICONSOLE_PRIVILEGE_ADMIN               2 

/* 
 * Debug Flags
 *
 * STDOUT       - Output debugging to stdout
 * STDERR       - Output debugging to stderr
 * SYSLOG       - Output debugging to the Syslog
 * FILE         - Output debugging to the default debug file
 * IPMI_PACKETS - Dump IPMI Packets too
 */
#define IPMICONSOLE_DEBUG_STDOUT           0x00000001
#define IPMICONSOLE_DEBUG_STDERR           0x00000002
#define IPMICONSOLE_DEBUG_SYSLOG           0x00000004
#define IPMICONSOLE_DEBUG_FILE             0x00000008
#define IPMICONSOLE_DEBUG_IPMI_PACKETS     0x00000010
       
/* 
 * Security Flags
 *
 * ERROR_ON_SOL_INUSE
 *
 * Under most circumstances, if SOL is detected as being in use,
 * libipmiconsole will attempt to deactivate the earlier SOL session
 * and activate the SOL session under the current one.  This default
 * behavior exists for several reasons, most notably that earlier SOL
 * sessions may have simply not been able to be deactivate properly.
 * This security flag changes the default behavior to return an error
 * if SOL is already detected as being in use.
 *   
 * LOCK_MEMORY
 *
 * Inform libipmiconsole to lock memory to prevent sensitive
 * information (such as usernames and passwords) to be swappable.
 *
 * DEACTIVATE_ONLY
 *
 * Only attempt to deactivate the SOL session.  If an SOL session is
 * not active, do nothing.
 */
#define IPMICONSOLE_SECURITY_ERROR_ON_SOL_INUSE 0x00000001
#define IPMICONSOLE_SECURITY_LOCK_MEMORY        0x00000002
#define IPMICONSOLE_SECURITY_DEACTIVATE_ONLY    0x00000004

/* 
 * Workaround Flags
 *
 * INTEL_2_0
 *
 * All currently known IPMI 2.0 implementations on Intel motherboards
 * contain a number of authentication bugs.  They include username
 * padding bugs, RAKP 4 integrity check values calculation bugs, and
 * password truncation if the authentication algorithm is
 * HMAC-MD5-128.  This workaround flag will allow the library to
 * authenticate with an Intel motherboard.
 *
 * Security Note: When the Integrity Algorithm is MD5-128 (Cipher
 * Suite ID 11 & 12), the integrity check value of a RAKP 4 packet
 * response is not tested, it is automatically accepted.  The other
 * integrity algorithms (None, HMAC-SHA1, HMAC-MD5) have been reverse
 * engineered for this non-compliance bug, however straight forward
 * MD5-128 has not yet been reverse engineered.
 *
 * SUPERMICRO_2_0
 *
 * There are several small IPMI compliance issues on early Supermicro
 * IPMI SOL implementations.  Mostly involving the authentication
 * codes returned the RAKP2 portion of authentication.  This
 * workaround flag will get around the problem.
 *
 * SUN_2_0
 *
 * Work around several IPMI 2.0 compliance problems, mostly involving
 * invalid lengthed hash keys and unsupported payload types.
 */
#define IPMICONSOLE_WORKAROUND_INTEL_2_0      0x00000001
#define IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0 0x00000002
#define IPMICONSOLE_WORKAROUND_SUN_2_0        0x00000004

#define IPMICONSOLE_THREAD_COUNT_MAX       32

typedef struct ipmiconsole_ctx *ipmiconsole_ctx_t;

/* 
 * ipmiconsole_ipmi_config
 *
 * Configuration information for connection to the remote IPMI
 * machine.
 *
 * username 
 *
 *   BMC username. Pass NULL ptr for NULL username.  Maximum length of
 *   16 bytes.
 *
 * password
 *
 *   BMC password. Pass NULL ptr for NULL password.  Maximum length of
 *   20 bytes.
 *
 * k_g
 *
 *   BMC Key for 2-key authentication.  Pass NULL ptr to use password
 *   as BMC key.  
 *
 * k_g_len
 *
 *   Length of k_g.  Necessary b/c k_g may contain null values or in its
 *   hex key.  Maximum length of 20 bytes.
 *
 * privilege_level
 *
 *   privilege level to authenticate with.  
 *
 *   Supported privilege levels:
 *
 *   IPMICONSOLE_PRIVILEGE_USER
 *   IPMICONSOLE_PRIVILEGE_OPERATOR
 *   IPMICONSOLE_PRIVILEGE_ADMIN
 *
 *   Pass < 0 for default of IPMICONSOLE_PRIVILEGE_ADMIN.
 *
 * cipher_suite_id
 *
 *   Cipher suite identifier to determine authentication, integrity,
 *   and confidentiality algorithms to use.
 *
 *   Supported Cipher Suite IDs
 *   (Key: A - Authentication Algorithm
 *         I - Integrity Algorithm
 *         C - Confidentiality Algorithm)
 *
 *   0 - A = None; I = None; C = None
 *   1 - A = HMAC-SHA1; I = None; C = None
 *   2 - A = HMAC-SHA1; I = HMAC-SHA1-96; C = None
 *   3 - A = HMAC-SHA1; I = HMAC-SHA1-96; C = AES-CBC-128
 *   6 - A = HMAC-MD5; I = None; C = None
 *   7 - A = HMAC-MD5; I = HMAC-MD5-128; C = None
 *   8 - A = HMAC-MD5; I = HMAC-MD5-128; C = AES-CBC-128
 *   11 - A = HMAC-MD5; I = MD5-128; C = None
 *   12 - A = HMAC-MD5; I = MD5-128; C = AES-CBC-128
 *
 *   Pass < 0 for default of 3.
 */
struct ipmiconsole_ipmi_config 
{
  char *username;
  char *password;
  char *k_g;
  unsigned int k_g_len;
  int privilege_level;
  int cipher_suite_id;
};

/* 
 * ipmiconsole_protocol_config
 *
 * Configuration information for the ipmi console engine
 * and the behavior by which it should behave.
 *
 * session_timeout_len
 *
 *   Specifies the session timeout length in milliseconds.  Pass <= 0
 *   to default to 60000 (60 seconds).
 *
 * retransmission_timeout_len
 *
 *   Specifies the packet retransmission timeout length in
 *   milliseconds.  Pass <= 0 to default to 500 (0.5 seconds).
 *
 * retransmission_backoff_count
 *
 *   Specifies the packet retransmission count until retransmission
 *   timeout lengths will be backed off.  Pass <= 0 to default to 2.
 *
 * keepalive_timeout_len
 *
 *   Specifies the session timeout length in milliseconds until a
 *   keepalive packet is sent.  Pass <= 0 to default to 20000 (20
 *   seconds).
 *
 * retransmission_keepalive_timeout_len
 *
 *   Specifies the keepalive packet retransmission timeout length in
 *   milliseconds.  Pass <= 0 to default to 5000 (5 seconds).
 *
 * acceptable_packet_errors_count
 *
 *   Specifies the maximum number of consecutive packet errors that
 *   can be received from a remote BMC before an error is returned and
 *   the session ended.  Pass <= 0 to use the default of 16.
 * 
 *   Note: This has been added to the behavior of the IPMI engine due
 *   to issues where remote BMCs can become "un-synced" with sequence
 *   numbers due to a network kernel boot.  It is possible a stream of
 *   packets can be sent to the remote client with session sequence
 *   numbers that are excessively outside of the acceptable window
 *   range.
 *
 * maximum_retransmission_count
 *
 *   Specifies the maximum number of retransmissions that can be
 *   sent for any IPMI packet before an error is returned and the
 *   session ended.  Pass <= 0 to use the default of 16.
 *
 *   Note: This has been added to the behavior of the IPMI engine due
 *   to issues where remote BMCs can become "un-synced" with sequence
 *   numbers due to a network kernel boot.  It is possible for some
 *   packets (in particular 'ping' packets to keep an IPMI session
 *   alive) to be accepted by the remote BMC, but not SOL packets.
 *
 * debug_flags
 *
 *   Bitwise OR of flags indicating how debug output should (or should
 *   not) be output. Pass 0 for default of no debugging.
 *
 * security_flags
 *
 *   Bitwise OR of flags indicating any protocol behavior that should
 *   be changed from the default for security reasons.  Pass 0 for
 *   default of no modifications to behavior.
 *
 * workaround_flags
 *
 *   Bitwise OR of flags indicating any behavior which should be
 *   changed from the default to handle IPMI non-compliance problems.
 *   Some BMCs which are non-compliant may require a workaround flag
 *   for correct operation. Pass 0 for default of no modifications to
 *   behavior.
 */
struct ipmiconsole_protocol_config
{
  int session_timeout_len;
  int retransmission_timeout_len;
  int retransmission_backoff_count;
  int keepalive_timeout_len;
  int retransmission_keepalive_timeout_len;
  int acceptable_packet_errors_count;
  int maximum_retransmission_count;
  unsigned int debug_flags;
  unsigned int security_flags; 
  unsigned int workaround_flags;
};

/* 
 * ipmiconsole_engine_init
 *
 * Initialize the ipmiconsole engine.  This function must be called
 * before ipmi console contexts can be submitted into the engine.
 *
 * thread_count
 * 
 *   Number of threads the engine will support.
 *
 * debug_flags 
 *
 *   Bitwise OR of flags indicating how debug output should (or should
 *   not) be output. Pass 0 for default of no debugging.
 *
 * Returns 0 on success, -1 on error
 */
int ipmiconsole_engine_init(unsigned int thread_count, 
			    unsigned int debug_flags);

/* 
 * ipmiconsole_engine_submit
 *
 * Submit a context to the ipmiconsole engine.  May return prior to a
 * session is established or an error/timeout occurs.
 *
 * Returns 0 on success, -1 on error
 */
int ipmiconsole_engine_submit(ipmiconsole_ctx_t c);

/*
 * ipmiconsole_engine_submit_block
 *
 * Submit a context to the ipmiconsole engine.  Block until
 * session is established or an error/timeout occurs.
 *
 * Returns 0 on success, -1 on error
 */
int ipmiconsole_engine_submit_block(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_engine_teardown
 *
 * Teardown the ipmiconsole engine and all contexts submitted to it.
 * Note that the teardown will block until it all active
 * contexts are closed.  
 *
 * Returns 0 on success, -1 on error 
 */
void ipmiconsole_engine_teardown(void);

/* 
 * ipmiconsole_ctx_create
 *
 * Create a ipmiconsole context.
 *
 * hostname
 *
 *   Host or IP address you wish to connect to.
 *
 * ipmi_config
 *
 *   IPMI configuration.  See ipmiconsole_ipmi_config definition above.
 *
 * protocol_config
 *
 *   IPMI protocol configuration.  See ipmiconsole_protocol_config definition above.
 *
 * Returns ctx on success, NULL on error
 */
ipmiconsole_ctx_t ipmiconsole_ctx_create(char *hostname,
					 struct ipmiconsole_ipmi_config *ipmi_config,
					 struct ipmiconsole_protocol_config *protocol_config);

/* 
 * ipmiconsole_ctx_errnum
 *
 * Returns the errnum of the most recently caused error
 */
int ipmiconsole_ctx_errnum(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_ctx_strerror
 *
 * Returns a pointer to statically allocated string describing the
 * error code in errnum.
 */
char *ipmiconsole_ctx_strerror(int errnum);

/* 
 * ipmiconsole_ctx_fd
 *
 * Returns a file descriptor for console reading and writing, -1 on error   
 */
int ipmiconsole_ctx_fd(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_ctx_generate_break
 *
 * Generate a break.
 *
 * Returns 0 on success, -1 on error
 */
int ipmiconsole_ctx_generate_break(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_ctx_destroy
 *
 * Destroy a context.  Note that this will always fail if the context
 * is still connected to a session.
 *
 * Returns 0 on success, -1 on error
 */
int ipmiconsole_ctx_destroy(ipmiconsole_ctx_t c);

#ifdef __cplusplus
}
#endif

#endif /* _IPMICONSOLE_H */
