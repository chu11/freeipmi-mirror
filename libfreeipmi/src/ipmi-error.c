/* 
   ipmi-error.c - IPMI error handling

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

#include "freeipmi.h"

#if defined (_ERR) 
#  undef _ERR
#endif
#define _ERR(errdesc, str)               \
do {                                     \
   if (!str) {                           \
     errno = ERANGE;                     \
     return (-1);                        \
   }                                     \
   if (strlen (errdesc) > (len - 1)) {   \
     errno = ERANGE;                     \
     return (-1);                        \
   }                                     \
   memset (str, 0, len);                 \
   strcpy (str, errdesc);                \
} while (0)

#if defined (_ERR_RET)
#  undef _ERR_RET
#endif
#define _ERR_RET(errdesc, str)           \
do {                                     \
   _ERR (errdesc, str);                  \
   return (0);                           \
} while (0)

int8_t ipmi_strerror_r(u_int8_t cmd, u_int8_t comp_code, u_int8_t *errstr, size_t len)
{
  if ((comp_code >= 0x80) && (comp_code <= 0xBE)) {
    switch (cmd) {
    case IPMI_CMD_GET_SESSION_CHALLENGE:
      switch (comp_code) {
      case  IPMI_ERR_INVALID_USERNAME:
	_ERR_RET ("Invalid user name", errstr);
      case  IPMI_ERR_NULL_USERNAME_NOT_ENABLED:
	_ERR_RET ("Null user name (User 1) not enabled", errstr);
      }
    case IPMI_CMD_ACTIVATE_SESSION:
      switch (comp_code) {
      case IPMI_ERR_NO_SESSION_SLOT_AVAILABLE:
	_ERR_RET ("No session slot available (BMC cannot accept any more sessions)", errstr);
      case IPMI_ERR_NO_SLOT_AVAILABLE_FOR_GIVEN_USER:
	_ERR_RET ("No slot available for given user. (Limit of user sessions allowed under that name has been reached)", errstr);
      case IPMI_ERR_NO_SLOT_AVAILABLE_TO_SUPPORT_USER:
	_ERR_RET ("No slot available to support user due to maximum privilege capability. (An implementation may only be able to support a certain number of sessions based on what authentication resources are required. For example, if User Level Authentication is disabled, an implementation may be able to allow a larger number of users that are limited to User Level privilege, than users that require higher privilege.", errstr);
      case IPMI_ERR_SESSION_SEQ_NUM_OUT_OF_RANGE:
	_ERR_RET ("Session sequence number out-of-range", errstr);
      case IPMI_ERR_INVALID_SESSION_ID:
	_ERR_RET ("Invalid session ID in request", errstr);
      case IPMI_ERR_EXCEEDS_PRIV_LEVEL:
	_ERR_RET ("requested maximum privilege level exceeds user and/or channel privilege limit", errstr);
      }
    case IPMI_CMD_SET_SESSION_PRIV_LEVEL: 
      switch (comp_code) {
      case IPMI_ERR_RQ_LEVEL_NOT_AVAILABLE_FOR_USER:
	_ERR_RET ("Requested level not available for this user", errstr);
      case IPMI_ERR_RQ_LEVEL_EXCEEDS_USER_PRIV_LIMIT:
	_ERR_RET ("Requested level exceeds Channel and/or User Privilege Limit", errstr);
      case IPMI_ERR_CANNOT_DISABLE_USER_LEVEL_AUTH:
	_ERR_RET ("Cannot disable User Level authentication", errstr);
      }
    case IPMI_CMD_CLOSE_SESSION:
      switch (comp_code) {
      case IPMI_ERR_INVALID_SESSION_ID_IN_RQ:
	_ERR_RET ("Invalid session ID in request", errstr);
      }
    default:
      _ERR_RET ("COMMAND-SPECIFIC CODES 80h-BEh: Unhandled standard command-specific codes. This range is reserved for command-specific completion codes for commands specified in this document.", errstr);
    }
  }
  else {
    switch (comp_code) {
    case IPMI_COMMAND_SUCCESS:
      _ERR_RET ("Command Completed Normally.", errstr);
      
    case IPMI_ERR_NODE_BUSY:
      _ERR_RET ("Node Busy. Command could not be processed because command processing resources are temporarily unavailable.", errstr);
      
    case IPMI_ERR_COMMAND_INVALID:
      _ERR_RET ("Invalid Command. Used to indicate an unrecognized or unsupported ommand.", errstr);  
    case IPMI_ERR_COMMAND_INVALID_FOR_LUN:
      _ERR_RET ("Command invalid for given LUN.", errstr);
      
    case IPMI_ERR_COMMAND_TIMEOUT:
      _ERR_RET ("Timeout while processing command. Response unavailable.", errstr);
      
    case IPMI_ERR_OUT_OF_SPACE:
      _ERR_RET ("Out of space. Command could not be completed because of a lack of storage space required to execute the given command operation.", errstr);
      
    case IPMI_ERR_RESERVATION_CANCELLED:
      _ERR_RET ("Reservation Canceled or Invalid Reservation ID.", errstr);
      
    case IPMI_ERR_REQUEST_DATA_TRUNCATED:
      _ERR_RET ("Request data truncated.", errstr);
      
    case IPMI_ERR_REQUEST_DATA_LENGTH_INVALID:
      _ERR_RET ("Request data length invalid.", errstr);
      
    case IPMI_ERR_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED: 
      _ERR_RET ("Request data field length limit exceeded.", errstr);
      
    case IPMI_ERR_PARAMETER_OUT_OF_RANGE:
      _ERR_RET ("Parameter out of range. One or more parameters in the data field of the Request are out of range. This is different from  Invalid data field (CCh) code in that it indicates that the erroneous field(s) has a contiguous range of possible values.", errstr);
      
    case IPMI_ERR_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES:
      _ERR_RET ("Cannot return number of requested data bytes.", errstr);
      
    case IPMI_ERR_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT:
      _ERR_RET ("Requested Sensor, data, or record not present.", errstr);
      
    case IPMI_ERR_REQUEST_INVALID_DATA_FIELD:
      _ERR_RET ("Invalid data field in Request.", errstr);
      
    case IPMI_ERR_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE:
      _ERR_RET ("Command illegal for specified sensor or record type.", errstr);
      
    case IPMI_ERR_COMMAND_CANNOT_RESPOND:    
      _ERR_RET ("Command response could not be provided.", errstr);
      
    case IPMI_ERR_COMMAND_DUPLICATE_REQUEST:
      _ERR_RET ("Cannot execute duplicated request. This completion code is for devices which cannot return the response that was returned for the original instance of the request. Such devices should provide separate commands that allow the completion status of the original request to be determined. An Event Receiver does not use this completion code, but returns the 00h completion code in the response to (valid) duplicated requests.", errstr);
      
    case IPMI_ERR_SDR_UPDATE_MODE:
      _ERR_RET ("Command response could not be provided. SDR Repository in update mode.", errstr);
      
    case IPMI_ERR_FIRMWARE_UPDATE_MODE:
      _ERR_RET ("Command response could not be provided. Device in firmware update mode.", errstr);
      
    case IPMI_ERR_BMC_INIT_MODE:
      _ERR_RET ("Command response could not be provided. BMC initialization or initialization agent in progress.", errstr);
      
    case IPMI_ERR_DESTINATION_UNAVAILABLE:
      _ERR_RET ("Destination unavailable. Cannot deliver request to selected destination. E.g. this code can be returned if a request message is targeted to SMS, but receive message queue reception is disabled for the particular channel.", errstr);
      
    case IPMI_ERR_INSUFFICIENT_PRIVILEGE_LEVEL:
      _ERR_RET ("Cannot execute command. Insufficient privilege level.", errstr);
      
    case IPMI_ERR_REQUEST_PARAMETER_NOT_SUPPORTED:
      _ERR_RET ("Cannot execute command. Command, or request parameter(s), not supported in present state.", errstr);
      
    case IPMI_ERR_UNSPECIFIED_ERROR:
      _ERR_RET ("Unspecified error.", errstr);
      
    default:
      if ((comp_code >= 0x01) && (comp_code <= 0x7E))
	_ERR_RET ("DEVICE-SPECIFIC (OEM) CODES 01h-7Eh: This range is used for command-specific codes that are also specific for a particular device and version. A prior knowledge of  the device command set is required for interpretation of these codes.", errstr);
    }
  }

  _ERR ("Unknown IPMI completion code", errstr);
  errno = EINVAL;
  return (-1);
}

int8_t ipmi_strerror_cmd_r(fiid_obj_t obj_cmd, u_int8_t *errstr, size_t len)
{
  u_int8_t cmd, comp_code;

  if (!obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }

  cmd = obj_cmd[0];
  comp_code = obj_cmd[1];

  return ipmi_strerror_r(cmd, comp_code, errstr, len); 
}

#if defined (_ERR_RET) 
#  undef _ERR_RET
#endif
#define _ERR_RET(errdesc, str)           \
do {                                     \
   _ERR (errdesc, str);                  \
   return (status_code);                 \
} while (0)
int8_t
ipmi_kcs_strstatus_r(u_int8_t status_code, u_int8_t *status_str, size_t len)
{
  memset (status_str, 0, len);
  switch (status_code)
    {
    case IPMI_KCS_STATUS_NO_ERR:
      _ERR_RET ("No Error", status_str);
    case IPMI_KCS_STATUS_ABORTED_BY_CMD:
      _ERR_RET ("Aborted By Command (Transfer in progress was aborted by SMS issuing the Abort/Status control code)", status_str);
    case IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE:
      _ERR_RET ("Illegal Control Code", status_str);
    case IPMI_KCS_STATUS_LEN_ERR:
      _ERR_RET ("Length Error (e.g.overrun)", status_str); 
    case IPMI_KCS_STATUS_UNSPECIFIED_ERR:
      _ERR_RET ("Unspecified Error", status_str); 
    default:
      if ((status_code >= IPMI_KCS_STATUS_OEM_ERR_BEGIN) &&
	  (status_code <= IPMI_KCS_STATUS_OEM_ERR_END))
	{
	  _ERR_RET ("OEM Error (Error must not fit into one of above categories.)", 
		    status_str);
	}
    }
  _ERR (" Reserved KCS Interface Status Code", status_str);
  errno = EINVAL;
  return (-1);
};


