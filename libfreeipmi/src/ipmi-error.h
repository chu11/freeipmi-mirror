/* 
   ipmi-error.h - IPMI error handling

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/


#ifndef _IPMI_ERROR_H
#define	_IPMI_ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_ERR_STR_MAX_LEN                 0x0800

/* RMCP+ and RAKP Message Status Codes
   -------------------------------- */
#define RMCPPLUS_STATUS_NO_ERRORS                                                        0x00
#define RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION                       0x01
#define RMCPPLUS_STATUS_INVALID_SESSION_ID                                               0x02
#define RMCPPLUS_STATUS_INVALID_PAYLOAD_TYPE                                             0x03
#define RMCPPLUS_STATUS_INVALID_AUTHENTICATION_ALGORITHM                                 0x04
#define RMCPPLUS_STATUS_INVALID_INTEGRITY_ALGORITHM                                      0x05
#define RMCPPLUS_STATUS_NO_MATCHING_AUTHENTICATION_PAYLOAD                               0x06
#define RMCPPLUS_STATUS_NO_MATCHING_INTEGRITY_PAYLOAD                                    0x07
#define RMCPPLUS_STATUS_INACTIVE_SESSION_ID                                              0x08
#define RMCPPLUS_STATUS_INVALID_ROLE                                                     0x09
#define RMCPPLUS_STATUS_UNAUTHORIZED_ROLE_OR_PRIVILEGE_LEVEL_REQUESTED                   0x0A
#define RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME 0x0B
#define RMCPPLUS_STATUS_INVALID_NAME_LENGTH                                              0x0C
#define RMCPPLUS_STATUS_UNAUTHORIZED_NAME                                                0x0D
#define RMCPPLUS_STATUS_UNAUTHORIZED_GUID                                                0x0E
#define RMCPPLUS_STATUS_INVALID_INTEGRITY_CHECK_VALUE                                    0x0F
#define RMCPPLUS_STATUS_INVALID_CONFIDENTIALITY_ALGORITHM                                0x10
#define RMCPPLUS_STATUS_NO_CIPHER_SUITE_MATCH_WITH_PROPOSED_SECURITY_ALGORITHMS          0x11
#define RMCPPLUS_STATUS_ILLEGAL_OR_UNRECOGNIZED_PARAMETER                                0x12
/* Reserved - all others */

/* To avoid gcc warnings, added +1 and -1 in comparison */
#define RMCPPLUS_STATUS_VALID(__status) \
        (((__status + 1) >= 0x01 \
          && (__status - 1) <= 0x11) ? 1 : 0)

#if defined (FREEIPMI_LIBRARY)
#   if defined (ERR_OUT)
#      undef ERR_OUT
#   endif
#define ERR_OUT(expr)                                                   \
do {                                                                    \
  if (!(expr))                                                          \
      return (-1);                                                      \
} while (0)
#endif /* FREEIPMI_LIBRARY */

#if defined (IPMI_SYSLOG)
#define __IPMI_SYSLOG                                                   \
      char errstr[IPMI_ERR_STR_MAX_LEN];                                \
      snprintf (errstr, IPMI_ERR_STR_MAX_LEN,                           \
               "%s: %d: %s: errno (%d): expression failed", __FILE__,   \
               __LINE__, __PRETTY_FUNCTION__, save_errno);              \
      syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);
#else
#define __IPMI_SYSLOG
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)
#define __IPMI_TRACE                                                    \
      fprintf (stderr,                                                  \
               "%s: %d: %s: errno (%d): expression failed\n", __FILE__, \
               __LINE__, __PRETTY_FUNCTION__, save_errno);              \
      fflush (stderr);                                                  
#else
#define __IPMI_TRACE
#endif /* IPMI_TRACE */

#if defined (FREEIPMI_LIBRARY)
#   if defined (ERR)
#      undef ERR
#   endif
#   define ERR(expr)                                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      extern int errno;                                                 \
      int save_errno = errno;                                           \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      errno = save_errno;                                               \
      return (-1);                                                      \
    }                                                                   \
} while (0)
#endif /* FREEIPMI_LIBRARY */

#if defined (FREEIPMI_LIBRARY)
#   if defined (ERR_UNLOCK)
#      undef ERR_UNLOCK
#   endif
#   define ERR_UNLOCK(expr)                                             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      extern int errno;                                                 \
      int save_errno = errno;                                           \
      IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());                  \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      errno = save_errno;                                               \
      return (-1);                                                      \
    }                                                                   \
} while (0)
#endif /* FREEIPMI_LIBRARY */

#if defined (FREEIPMI_LIBRARY)
#   if defined (ERR_EXIT)
#      undef ERR_EXIT
#   endif
#   define ERR_EXIT(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      extern int errno;                                                 \
      int save_errno = errno;                                           \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      errno = save_errno;                                               \
      exit(1);                                                          \
    }                                                                   \
} while (0)
#endif /* FREEIPMI_LIBRARY */

int8_t ipmi_strerror_r (uint8_t cmd, 
			uint8_t comp_code, 
			char *errstr, 
			size_t len);
int8_t ipmi_strerror_cmd_r (fiid_obj_t obj_cmd, 
			    char *errstr, 
			    size_t len);
int8_t ipmi_kcs_strstatus_r (uint8_t status_code, 
			     char *errstr, 
			     size_t len);

int8_t rmcpplus_status_strerror_r(uint8_t rmcpplus_status_code,
                                  char *errstr,
                                  size_t len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-error.h */

