/*****************************************************************************\
 *  $Id: ipmiconsole.h,v 1.63 2007-08-30 18:41:26 chu11 Exp $
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

/* 
 * IPMI Console Error Codes
 */
#define IPMICONSOLE_ERR_SUCCESS                               0
#define IPMICONSOLE_ERR_CTX_NULL                              1
#define IPMICONSOLE_ERR_CTX_INVALID                           2
#define IPMICONSOLE_ERR_ALREADY_SETUP                         3
#define IPMICONSOLE_ERR_NOT_SETUP                             4
#define IPMICONSOLE_ERR_CTX_NOT_SUBMITTED                     5
#define IPMICONSOLE_ERR_CTX_IS_SUBMITTED                      6
#define IPMICONSOLE_ERR_PARAMETERS                            7
#define IPMICONSOLE_ERR_HOSTNAME_INVALID                      8 
#define IPMICONSOLE_ERR_IPMI_2_0_UNAVAILABLE                  9
#define IPMICONSOLE_ERR_CIPHER_SUITE_ID_UNAVAILABLE          10
#define IPMICONSOLE_ERR_USERNAME_INVALID                     11
#define IPMICONSOLE_ERR_PASSWORD_INVALID                     12
#define IPMICONSOLE_ERR_K_G_INVALID                          13
#define IPMICONSOLE_ERR_PRIVILEGE_LEVEL_INSUFFICIENT         14
#define IPMICONSOLE_ERR_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED   15
#define IPMICONSOLE_ERR_SOL_UNAVAILABLE                      16
#define IPMICONSOLE_ERR_SOL_INUSE                            17
#define IPMICONSOLE_ERR_SOL_STOLEN                           18
#define IPMICONSOLE_ERR_SOL_REQUIRES_ENCRYPTION              19
#define IPMICONSOLE_ERR_SOL_REQUIRES_NO_ENCRYPTION           20
#define IPMICONSOLE_ERR_BMC_BUSY                             21
#define IPMICONSOLE_ERR_BMC_ERROR                            22
#define IPMICONSOLE_ERR_BMC_IMPLEMENTATION                   23
#define IPMICONSOLE_ERR_SESSION_TIMEOUT                      24
#define IPMICONSOLE_ERR_EXCESS_RETRANSMISSIONS_SENT          25
#define IPMICONSOLE_ERR_EXCESS_ERRORS_RECEIVED               26
#define IPMICONSOLE_ERR_OUT_OF_MEMORY                        27
#define IPMICONSOLE_ERR_TOO_MANY_OPEN_FILES                  28
#define IPMICONSOLE_ERR_SYSTEM_ERROR                         29
#define IPMICONSOLE_ERR_INTERNAL_ERROR                       30
#define IPMICONSOLE_ERR_ERRNUMRANGE                          31

/* 
 * Debug Flags
 *
 * Utilized with ipmiconsole_engine_init() or with struct
 * ipmiconsole_engine_config below.
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
 * IPMI Privilege Constants
 *
 * Utilized with struct ipmiconsole_ipmi_config below.
 */
#define IPMICONSOLE_PRIVILEGE_USER                0
#define IPMICONSOLE_PRIVILEGE_OPERATOR            1
#define IPMICONSOLE_PRIVILEGE_ADMIN               2 

/* 
 * Engine Flags
 *
 * Utilized with struct ipmiconsole_engine_config below.
 * 
 * CLOSE_FD
 *
 * By default, the ipmiconsole engine will not close the file
 * descriptor (returned by ipmiconsole_ctx_fd()) when an error occurs
 * within the ipmiconsole engine (such as a session timeout).  A user
 * will subsequently see an EOF on a read() or an EPIPE on a write()
 * to know an error occurred.
 *
 * This flag will inform the engine to close the file descriptor on
 * error.  This will change the behavior of how the user should
 * program with the file descriptor.  For example, calls to read() and
 * write() would likely return with EBADF errors instead of EOF or
 * EPIPE errors respectively.  Calls to select() may return with EBADF
 * errors and calls to poll() could result in POLLNVAL returned
 * events.
 *
 * OUTPUT_ON_SOL_ESTABLISHED
 *
 * When submitting a context to the engine non-blocking, another way
 * to determine if the SOL session has been established is if data has
 * output from the remote console and is available for you to read.
 * Under most circumstances, this isn't a controllable situation.
 *
 * This flag will inform the engine to output a single NUL character
 * ('\0') to the console once a SOL session has been established.  If
 * the CLOSE_FD flag isn't used above, this would allow the user to
 * expect an EOF vs. 1 byte of data on a read() to determine if the
 * SOL session has failed or succeeded.  The user may choose to output
 * the NUL anyways (it should do no harm) or simply throw out the
 * first byte ever read from remote console.
 *
 */
#define IPMICONSOLE_ENGINE_CLOSE_FD                  0x00000001
#define IPMICONSOLE_ENGINE_OUTPUT_ON_SOL_ESTABLISHED 0x00000002
       
/* 
 * Security Flags
 *
 * Utilized with struct ipmiconsole_protocol_config below.
 * 
 * ERROR_ON_SOL_INUSE
 *
 * Under most circumstances, if SOL is detected as being in use,
 * libipmiconsole will attempt to deactivate the earlier SOL session
 * and activate the SOL session under the current one.  This default
 * behavior exists for several reasons, most notably that earlier SOL
 * sessions may have not been able to be deactivate properly.  This
 * security flag changes the default behavior to return an error if
 * SOL is already detected as being in use.  If it is detected as in
 * use, the errnum returned from ipmiconsole_ctx_errnum() would be
 * IPMICONSOLE_ERR_SOL_INUSE.
 *   
 * LOCK_MEMORY
 *
 * Inform libipmiconsole to lock memory to prevent sensitive
 * information (such as usernames and passwords) to be non-swappable.
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
 * Utilized with struct ipmiconsole_protocol_config below.
 * 
 * AUTHENTICATION_CAPABILITIES
 *
 * Discoverd on an ASUS P5M2 motherboard, the motherboard does not
 * properly report username capabilities or K_g status, leading to
 * invalid username or K_g errors.  This workaround flag will work
 * around the problem.
 *
 * IGNORE_SOL_PAYLOAD_SIZE
 *
 * Discovered on an ASUS P5M2 motherboard, the motherboard reports
 * invalid SOL payload sizes.  This workaround flag will ignore the
 * payload size and choose a reasonable default.
 *
 * INTEL_2_0_SESSION
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
 * SUPERMICRO_2_0_SESSION
 *
 * There are several small IPMI compliance issues on early Supermicro
 * IPMI SOL implementations.  Most involve the authentication codes
 * returned during the RAKP2 portion of authentication.  This
 * workaround flag will get around the problem.  These compliance bugs
 * are confirmed to be fixed on newer firmware.
 *
 * SUN_2_0_SESSION
 *
 * Work around several IPMI 2.0 compliance problems, mostly involving
 * invalid lengthed hash keys and unsupported payload types.
 *
 * Note: The non-logical bitmask order below is set for consistency of
 * masks with libfreeipmi bitmasks.
 */
#define IPMICONSOLE_WORKAROUND_AUTHENTICATION_CAPABILITIES 0x00000010
#define IPMICONSOLE_WORKAROUND_IGNORE_SOL_PAYLOAD_SIZE     0x00010000
#define IPMICONSOLE_WORKAROUND_INTEL_2_0_SESSION           0x01000000
#define IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0_SESSION      0x02000000
#define IPMICONSOLE_WORKAROUND_SUN_2_0_SESSION             0x04000000

/*
 * Context Status
 *
 * Returned by ipmiconsole_ctx_status() below.
 *
 * ERROR
 *
 * An error has occurred retrieving the status.
 * 
 * NOT_SUBMITTED
 *
 * The context has not been submitted to the engine.
 *
 * SUBMITTED
 *
 * The context has been submitted to the engine.  SOL has not been
 * established and an error has not yet occurred.
 *
 * SOL_ERROR
 *
 * The context has received an error during SOL establishment.
 *
 * SOL_ESTABLISHED
 *
 * The context has established a SOL session.
 *
 */
enum ipmiconsole_ctx_status
  {
    IPMICONSOLE_CTX_STATUS_ERROR = -1,
    IPMICONSOLE_CTX_STATUS_NOT_SUBMITTED = 0,
    IPMICONSOLE_CTX_STATUS_SUBMITTED = 1,
    IPMICONSOLE_CTX_STATUS_SOL_ERROR = 2,
    IPMICONSOLE_CTX_STATUS_SOL_ESTABLISHED = 3,
  };
typedef enum ipmiconsole_ctx_status ipmiconsole_ctx_status_t;

/* 
 * ipmiconsole_ipmi_config
 *
 * Authentication information for a connection to a remote IPMI
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
 *   as BMC key.  The k_g key need not be an ascii string.
 *
 * k_g_len
 *
 *   Length of k_g.  Necessary b/c k_g may contain null values in its
 *   key.  Maximum length of 20 bytes.
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
  unsigned char *k_g;
  unsigned int k_g_len;
  int privilege_level;
  int cipher_suite_id;
};

/* 
 * ipmiconsole_protocol_config
 *
 * Configuration information for the IPMI protocol management. 
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
  unsigned int security_flags; 
  unsigned int workaround_flags;
};

/* 
 * ipmiconsole_engine_config
 *
 * Configuration information for how the engine should interact with
 * the user or API.
 *
 * engine_flags
 *
 *   Bitwise OR of flags indicating how the ipmiconsole engine should
 *   behave for a particular context.  Pass 0 for default behavior.
 *
 * debug_flags
 *
 *   Bitwise OR of flags indicating how debug output should (or should
 *   not) be output. Pass 0 for default of no debugging.
 *
 */
struct ipmiconsole_engine_config
{
  unsigned int engine_flags;
  unsigned int debug_flags;
};

#define IPMICONSOLE_THREAD_COUNT_MAX       32

typedef struct ipmiconsole_ctx *ipmiconsole_ctx_t;

/* 
 * Ipmiconsole_callback
 *
 * Function prototype for a callback function.
 * ipmiconsole_engine_submit() below.
 */
typedef void (*Ipmiconsole_callback)(ipmiconsole_ctx_t c, void *);

/* 
 * ipmiconsole_engine_init
 *
 * Initialize the ipmiconsole engine.  Engine threads will be created
 * which will manage SOL sessions for the user.  This function must be
 * called before ipmi console contexts can be submitted into the
 * engine.
 *
 * Parameters:
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
 * Submit a context to the ipmiconsole engine non-blocking.  This
 * function can return prior to a SOL session being established.  A
 * return value of 0 indicates the context was submitted properly.  A
 * return value of -1 indicates an error occurred during the
 * submission.  On an error, ipmiconsole_ctx_errnum() can be used to
 * determine the type of error that occured.
 *
 * After a context has been submitted, the user may determine if a SOL
 * session has been established several ways:
 *
 * A) Poll on the context status, retrieved via
 * ipmiconsole_ctx_status().  On an error, ipmiconsole_ctx_errnum()
 * can be used to determine the specific IPMI related error that
 * occurred.
 *
 * B) Poll on the context file descriptor, retrieved via
 * ipmiconsole_ctx_fd().  A SOL establishment error will result in an
 * EOF being returned on the file descriptor.  A proper SOL
 * establishment can be determined via a readable character on the
 * file descriptor.  The use of the OUTPUT_ON_SOL_ESTABLISHED Engine
 * flag above can aid in this.  the CLOSE_FD Engine flag can be set to
 * slightly alter this behavior, please see above. On an error,
 * ipmiconsole_ctx_errnum() can be used to determine the specific IPMI
 * related error that occurred.
 *
 * C) Specify a callback function.  The callback function specified as
 * a parameter below will be called directly after a SOL session has
 * been established or an error has occurred.  Within those callback
 * functions, ipmiconsole_ctx_status() can be used to determine which
 * has occurred.
 *
 * Due to the non-blocking semantics of this function, it is possible
 * that multiple errors could occur simultaneously and the errnum
 * retrieved via ipmiconsole_ctx_errnum() may not be the one that
 * caused the SOL session to fail.  However, this will not occur given
 * proper usage of the API.  For example, if the user called
 * ipmiconsole_engine_submit() twice with the same context, an SOL
 * error in the engine background could race with the setting of the
 * errnum IPMICONSOLE_ERR_CTX_IS_SUBMITTED in the second call.
 *
 * Parameters:
 *
 * callback
 *  
 *   If specified, a callback function will be called from the engine
 *   when a SOL session has been established or a SOL establishment
 *   error has occurred.  Will only be called under a non-blocking
 *   engine submission via ipmiconsole_engine_submit().  Will be
 *   called once and only once during an individual engine submission.
 *   For example, if a SOL session is established then a later session
 *   timeout occurs, the later session timeout will not generate a
 *   function call to the callback.  Pass NULL for no callback.
 *
 *   The callback function call be called simultaneously from
 *   different engine threads.  It is the user's responsibility to
 *   protect against any race conditions in their callback function.
 *
 * callback_arg
 *
 *   Specify an arbitrary argument to be passed to the callback
 *   routine.
 *
 * Returns 0 on success, -1 on error.  
 */
int ipmiconsole_engine_submit(ipmiconsole_ctx_t c,
                              Ipmiconsole_callback callback,
                              void *callback_arg);

/* 
 * ipmiconsole_engine_submit_block
 *
 * Submit a context to the ipmiconsole engine and block until a SOL
 * session is established or an error/timeout occurs.  A return value
 * of 0 indicates the SOL session was established and a -1 indicates
 * an error occurred.  On an error, ipmiconsole_ctx_errnum() can be
 * used to determine the type of error that occured.
 *
 * Returns 0 on success, -1 on error.  
 */
int ipmiconsole_engine_submit_block(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_engine_teardown
 *
 * Teardown the ipmiconsole engine.  This function will destroy
 * all threads and contexts managed by the engine.
 *
 * Parameters:
 *
 * cleanup_sol_sessions
 *
 *   If set to non zero, SOL sessions will be torn down cleanly.
 *   ipmiconsole_engine_teardown() will block until all active ipmi
 *   sessions have been closed cleanly or timed out.
 */
void ipmiconsole_engine_teardown(int cleanup_sol_sessions);

/* 
 * ipmiconsole_ctx_create
 *
 * Create a ipmiconsole context.  The context can then be submitted
 * into the ipmiconsole engine (via ipmiconsole_engine_submit() or
 * ipmiconsole_engine_submit_block()) to establish a SOL session.  The
 * context cannot be submitted to the ipmiconsole engine more than
 * once.  After it has been submitted to the ipmiconsole, it cannot be
 * reused.
 *
 * Parameters:
 *
 * hostname
 *
 *   Host or IP address you wish to connect to.
 *
 * ipmi_config
 *
 *   IPMI configuration.  See ipmiconsole_ipmi_config definition
 *   above.
 *
 * protocol_config
 *
 *   IPMI protocol configuration.  See ipmiconsole_protocol_config
 *   definition above.
 *
 * engine_config
 *
 *   Ipmiconsole engine configuration.  See ipmiconsole_engine_config
 *   definition above.
 *
 * Returns ctx on success, NULL on error.
 */
ipmiconsole_ctx_t ipmiconsole_ctx_create(char *hostname,
					 struct ipmiconsole_ipmi_config *ipmi_config,
					 struct ipmiconsole_protocol_config *protocol_config,
					 struct ipmiconsole_engine_config *engine_config);

/* 
 * ipmiconsole_ctx_errnum
 *
 * Returns the errnum of the most recently recorded error for the
 * context that has not yet been read by the user.
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
 * ipmiconsole_ctx_status
 *
 * Returns the current context status.  Primarily used to determine if
 * a context submission (submitted non-blocking via
 * ipmiconsole_engine_submit()) has been established or not.  Returns
 * IPMICONSOLE_CTX_STATUS_ERROR (-1) on error.  
 */
ipmiconsole_ctx_status_t ipmiconsole_ctx_status(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_ctx_fd
 *
 * Returns a file descriptor for console reading and writing after it
 * has been submitted to the engine.  Returns -1 on error.
 * ipmiconsole_ctx_errnum() can be called to determine the cause of
 * the error.
 *
 * If the user closes the file descriptor while the serial over lan
 * session is established, the session will be torn down in the
 * engine.
 *
 * If an error occurs on the engine side (for example a session
 * timeout) the other end of the file descriptor pair (from which this
 * fd is a part of) will be closed.  The error can be determined via
 * ipmiconsole_ctx_errnum().  The user of this file descriptor will
 * typically see this affect via an EOF on a read() or an EPIPE on a
 * write().  For alternate file descriptor behavior, see ENGINE flags
 * above.
 */
int ipmiconsole_ctx_fd(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_ctx_generate_break
 *
 * Generate a break on an active serial over LAN session.
 *
 * Returns 0 on success, -1 on error.  ipmiconsole_ctx_errnum() can be
 * called to determine the cause of the error.
 */
int ipmiconsole_ctx_generate_break(ipmiconsole_ctx_t c);

/* 
 * ipmiconsole_ctx_destroy
 *
 * Destroy a context.  Will close file descriptor retrieved from
 * ipmiconsole_ctx_fd().
 */
void ipmiconsole_ctx_destroy(ipmiconsole_ctx_t c);

#ifdef __cplusplus
}
#endif

#endif /* _IPMICONSOLE_H */
