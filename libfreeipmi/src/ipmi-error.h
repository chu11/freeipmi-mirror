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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/


#ifndef _IPMI_ERROR_H
#define	_IPMI_ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_ERR_STR_MAX_LEN                 0x0800

/* IPMI KCS Interface Status Codes
   -------------------------------- */
#define IPMI_KCS_STATUS_NO_ERR               0x00
#define IPMI_KCS_STATUS_SUCCESS              IPMI_KCS_STATUS_NO_ERR
#define IPMI_KCS_STATUS_OK                   IPMI_KCS_STATUS_NO_ERR
#define IPMI_KCS_STATUS_ABORTED_BY_CMD       0x01
#define IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE    0x02
#define IPMI_KCS_STATUS_LEN_ERR              0x06
#define IPMI_KCS_STATUS_OEM_ERR_BEGIN        0xC0
#define IPMI_KCS_STATUS_OEM_ERR_END          0xFE
#define IPMI_KCS_STATUS_UNSPECIFIED_ERR      0xFF
/* Reserved - all others */

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
void ipmi_error (fiid_obj_t obj_cmd, 
		 const char *s);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-error.h */

