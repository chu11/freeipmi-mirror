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

/* Command Completed Normally. */
#define IPMI_COMMAND_SUCCESS                 0x00

/* Node Busy. Command could not be processed because command
   processing resources are temporarily unavailable. */
#define IPMI_ERR_NODE_BUSY                   0xC0

/* Invalid Command. Used to indicate an unrecognized or unsupported
  command. */
#define IPMI_ERR_COMMAND_INVALID             0xC1

/* Command invalid for given LUN. */
#define IPMI_ERR_COMMAND_INVALID_FOR_LUN     0xC2

/* Timeout while processing command. Response unavailable. */
#define IPMI_ERR_COMMAND_TIMEOUT             0xC3

/* Out of space. Command could not be completed because of a lack of
   storage space required to execute the given command operation. */
#define IPMI_ERR_OUT_OF_SPACE                0xC4

/* Reservation Canceled or Invalid Reservation ID. */
#define IPMI_ERR_RESERVATION_CANCELLED       0xC5
#define IPMI_ERR_INVALID_RESERVATION_ID IPMI_ERR_RESERVATION_CANCELLED

/* Request data truncated. */
#define IPMI_ERR_REQUEST_DATA_TRUNCATED      0xC6

/* Request data length invalid. */
#define IPMI_ERR_REQUEST_DATA_LENGTH_INVALID 0xC7

/* Request data field length limit exceeded. */
#define IPMI_ERR_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED 0xC8

/* Parameter out of range. One or more parameters in the data
field of the Request are out of range. This is different from  Invalid
data field (CCh) code in that it indicates that the erroneous field(s)
has a contiguous range of possible values. */
#define IPMI_ERR_PARAMETER_OUT_OF_RANGE      0xC9

/* Cannot return number of requested data bytes. */
#define IPMI_ERR_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES   0xCA

/* Requested Sensor, data, or record not present. */
#define IPMI_ERR_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT 0xCB

/* Invalid data field in Request */
#define IPMI_ERR_REQUEST_INVALID_DATA_FIELD  0xCC

/* Command illegal for specified sensor or record type. */
#define IPMI_ERR_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE 0xCD

/* Command response could not be provided. */
#define IPMI_ERR_COMMAND_CANNOT_RESPOND      0xCE

/* Cannot execute duplicated request. This completion code is for
   devices which cannot return the response that was returned for the
   original instance of the request. Such devices should provide
   separate commands that allow the completion status of the original
   request to be determined. An Event Receiver does not use this
   completion code, but returns the 00h completion code in the
   response to (valid) duplicated requests. */
#define IPMI_ERR_COMMAND_DUPLICATE_REQUEST   0xCF

/* Command response could not be provided. SDR Repository in update
   mode. */
#define IPMI_ERR_SDR_UPDATE_MODE             0xD0

/* Command response could not be provided. Device in firmware update
   mode. */
#define IPMI_ERR_FIRMWARE_UPDATE_MODE        0xD1

/* Command response could not be provided. BMC initialization or
   initialization agent in progress. */
#define IPMI_ERR_BMC_INIT_MODE               0xD2

/* Destination unavailable. Cannot deliver request to selected
   destination. E.g. this code can be returned if a request message is
   targeted to SMS, but receive message queue reception is disabled
   for the particular channel. */
#define IPMI_ERR_DESTINATION_UNAVAILABLE     0xD3

/* Cannot execute command. Insufficient privilege level. */
#define IPMI_ERR_INSUFFICIENT_PRIVILEGE_LEVEL    0xD4

/* Cannot execute command. Command, or request parameter(s), not
   supported in present state. */
#define IPMI_ERR_REQUEST_PARAMETER_NOT_SUPPORTED 0xD5

/* Unspecified error. */
#define IPMI_ERR_UNSPECIFIED_ERROR           0xFF


/* DEVICE-SPECIFIC (OEM) CODES 01h-7Eh
   This range is used for command-specific codes that are also
   specific for a particular device and version. A-priori knowledge of
   the device command set is required for interpretation of these
   codes. */ 

/* COMMAND-SPECIFIC CODES 80h-BEh 80h-BEh 
   Standard command-specific codes. This range is reserved for
   command-specific completion codes for commands specified in this
   document. */
/* IPMI_CMD_GET_SESSION_CHALLENGE */
#define IPMI_ERR_INVALID_USERNAME                  0x81
#define IPMI_ERR_NULL_USERNAME_NOT_ENABLED         0x82 // user1

/* IPMI_CMD_ACTIVATE_SESSION */
#define IPMI_ERR_NO_SESSION_SLOT_AVAILABLE         0x81
#define IPMI_ERR_NO_SLOT_AVAILABLE_FOR_GIVEN_USER  0x82
#define IPMI_ERR_NO_SLOT_AVAILABLE_TO_SUPPORT_USER 0x83
#define IPMI_ERR_SESSION_SEQ_NUM_OUT_OF_RANGE      0x84
#define IPMI_ERR_INVALID_SESSION_ID                0x85
#define IPMI_ERR_EXCEEDS_PRIV_LEVEL                0x86

/* IPMI_CMD_SET_SESSION_PRIV_LEVEL */
#define IPMI_ERR_RQ_LEVEL_NOT_AVAILABLE_FOR_USER   0x81
#define IPMI_ERR_RQ_LEVEL_EXCEEDS_USER_PRIV_LIMIT  0x82
#define IPMI_ERR_CANNOT_DISABLE_USER_LEVEL_AUTH    0x83

/* IPMI_CMD_CLOSE_SESSION */
#define IPMI_ERR_INVALID_SESSION_ID_IN_RQ          0x87


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
void ipmi_error (fiid_obj_t obj_cmd, 
		 const char *s);

int8_t rmcpplus_status_strerror_r(uint8_t rmcpplus_status_code,
                                  char *errstr,
                                  size_t len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-error.h */

