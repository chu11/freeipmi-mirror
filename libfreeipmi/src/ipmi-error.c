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

#define SNPRINTF_RETURN(arg...)    \
do				   \
{				   \
  snprintf (errstr, len, arg);	   \
  return 0;                        \
} while (0)

int8_t 
ipmi_strerror_r (uint8_t cmd, 
		 uint8_t comp_code, 
		 char *errstr, 
		 size_t len)
{
  if (errstr == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  switch (comp_code) 
    {
    case IPMI_COMMAND_SUCCESS:
      SNPRINTF_RETURN ("Command Completed Normally.");
      
    case IPMI_ERR_NODE_BUSY:
      SNPRINTF_RETURN ("Node Busy. Command could not be processed "
		       "because command processing resources are "
		       "temporarily unavailable.");
      
    case IPMI_ERR_COMMAND_INVALID:
      SNPRINTF_RETURN ("Invalid Command. Used to indicate an "
		       "unrecognized or unsupported command.");
      
    case IPMI_ERR_COMMAND_INVALID_FOR_LUN:
      SNPRINTF_RETURN ("Command invalid for given LUN.");
      
    case IPMI_ERR_COMMAND_TIMEOUT:
      SNPRINTF_RETURN ("Timeout while processing command. "
		       "Response unavailable.");
      
    case IPMI_ERR_OUT_OF_SPACE:
      SNPRINTF_RETURN ("Out of space. Command could not be "
		       "completed because of a lack of storage "
		       "space required to execute the given "
		       "command operation.");
      
    case IPMI_ERR_RESERVATION_CANCELLED:
      SNPRINTF_RETURN ("Reservation Canceled or Invalid "
		       "Reservation ID.");
      
    case IPMI_ERR_REQUEST_DATA_TRUNCATED:
      SNPRINTF_RETURN ("Request data truncated.");
      
    case IPMI_ERR_REQUEST_DATA_LENGTH_INVALID:
      SNPRINTF_RETURN ("Request data length invalid.");
      
    case IPMI_ERR_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED: 
      SNPRINTF_RETURN ("Request data field length limit exceeded.");
      
    case IPMI_ERR_PARAMETER_OUT_OF_RANGE:
      SNPRINTF_RETURN ("Parameter out of range. One or more "
		       "parameters in the data field of the "
		       "Request are out of range. This is "
		       "different from  Invalid data field "
		       "(CCh) code in that it indicates that "
		       "the erroneous field(s) has a contiguous "
		       "range of possible values.");
      
    case IPMI_ERR_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES:
      SNPRINTF_RETURN ("Cannot return number of requested data bytes.");
      
    case IPMI_ERR_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT:
      SNPRINTF_RETURN ("Requested Sensor, data, or record not present.");
      
    case IPMI_ERR_REQUEST_INVALID_DATA_FIELD:
      SNPRINTF_RETURN ("Invalid data field in Request.");
      
    case IPMI_ERR_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE:
      SNPRINTF_RETURN ("Command illegal for specified sensor or record type.");
      
    case IPMI_ERR_COMMAND_CANNOT_RESPOND:    
      SNPRINTF_RETURN ("Command response could not be provided.");
      
    case IPMI_ERR_COMMAND_DUPLICATE_REQUEST:
      SNPRINTF_RETURN ("Cannot execute duplicated request. "
		       "This completion code is for devices "
		       "which cannot return the response that "
		       "was returned for the original instance "
		       "of the request. Such devices should provide "
		       "separate commands that allow the completion "
		       "status of the original request to be determined. "
		       "An Event Receiver does not use this completion "
		       "code, but returns the 00h completion code in the "
		       "response to (valid) duplicated requests.");
      
    case IPMI_ERR_SDR_UPDATE_MODE:
      SNPRINTF_RETURN ("Command response could not be provided. "
		       "SDR Repository in update mode.");
      
    case IPMI_ERR_FIRMWARE_UPDATE_MODE:
      SNPRINTF_RETURN ("Command response could not be provided. "
		       "Device in firmware update mode.");
      
    case IPMI_ERR_BMC_INIT_MODE:
      SNPRINTF_RETURN ("Command response could not be provided. "
		       "BMC initialization or initialization "
		       "agent in progress.");
      
    case IPMI_ERR_DESTINATION_UNAVAILABLE:
      SNPRINTF_RETURN ("Destination unavailable. Cannot deliver "
		       "request to selected destination. E.g. "
		       "this code can be returned if a request "
		       "message is targeted to SMS, but receive "
		       "message queue reception is disabled for "
		       "the particular channel.");
      
    case IPMI_ERR_INSUFFICIENT_PRIVILEGE_LEVEL:
      SNPRINTF_RETURN ("Cannot execute command. "
		       "Insufficient privilege level.");
      
    case IPMI_ERR_REQUEST_PARAMETER_NOT_SUPPORTED:
      SNPRINTF_RETURN ("Cannot execute command. Command, "
		       "or request parameter(s), not supported "
		       "in present state.");
      
    case IPMI_ERR_UNSPECIFIED_ERROR:
      SNPRINTF_RETURN ("Unspecified error.");
    }

  /* OEM completion codes */
  if ((comp_code >= 0x01) && (comp_code <= 0x7E))
    {
      SNPRINTF_RETURN ("Device specific (OEM) completion code %02Xh.", 
		       comp_code);
    }
  
  /* Command specific completion codes */
  if ((comp_code >= 0x80) && (comp_code <= 0xBE)) 
    {
      switch (cmd) 
	{
	case IPMI_CMD_GET_SESSION_CHALLENGE:
	  switch (comp_code) 
	    {
	    case IPMI_ERR_INVALID_USERNAME:
	      SNPRINTF_RETURN ("Invalid user name");
	      
	    case IPMI_ERR_NULL_USERNAME_NOT_ENABLED:
	      SNPRINTF_RETURN ("Null user name (User 1) not enabled");
	    }
	  break;
	case IPMI_CMD_ACTIVATE_SESSION:
	  switch (comp_code) 
	    {
	    case IPMI_ERR_NO_SESSION_SLOT_AVAILABLE:
	      SNPRINTF_RETURN ("No session slot available "
			       "(BMC cannot accept any more sessions)");
	      
	    case IPMI_ERR_NO_SLOT_AVAILABLE_FOR_GIVEN_USER:
	      SNPRINTF_RETURN ("No slot available for given user. "
			       "(Limit of user sessions allowed under "
			       "that name has been reached)");
	      
	    case IPMI_ERR_NO_SLOT_AVAILABLE_TO_SUPPORT_USER:
	      SNPRINTF_RETURN ("No slot available to support user due to "
			       "maximum privilege capability. (An "
			       "implementation may only be able to support "
			       "a certain number of sessions based on what "
			       "authentication resources are required. For "
			       "example, if User Level Authentication is "
			       "disabled, an implementation may be able "
			       "to allow a larger number of users that are "
			       "limited to User Level privilege, than users "
			       "that require higher privilege.");
	      
	    case IPMI_ERR_SESSION_SEQ_NUM_OUT_OF_RANGE:
	      SNPRINTF_RETURN ("Session sequence number out-of-range");
	      
	    case IPMI_ERR_INVALID_SESSION_ID:
	      SNPRINTF_RETURN ("Invalid session ID in request");
	      
	    case IPMI_ERR_EXCEEDS_PRIV_LEVEL:
	      SNPRINTF_RETURN ("requested maximum privilege level exceeds user and/or "
			       "channel privilege limit");
	    }
	  break;
	case IPMI_CMD_SET_SESSION_PRIV_LEVEL: 
	  switch (comp_code) 
	    {
	    case IPMI_ERR_RQ_LEVEL_NOT_AVAILABLE_FOR_USER:
	      SNPRINTF_RETURN ("Requested level not available for this user");
	      
	    case IPMI_ERR_RQ_LEVEL_EXCEEDS_USER_PRIV_LIMIT:
	      SNPRINTF_RETURN ("Requested level exceeds Channel and/or User "
			       "Privilege Limit");
	      
	    case IPMI_ERR_CANNOT_DISABLE_USER_LEVEL_AUTH:
	      SNPRINTF_RETURN ("Cannot disable User Level authentication");
	    }
	  break;
	case IPMI_CMD_CLOSE_SESSION:
	  switch (comp_code) 
	    {
	    case IPMI_ERR_INVALID_SESSION_ID_IN_RQ:
	      SNPRINTF_RETURN ("Invalid session ID in request");
	      
	    default:
	      SNPRINTF_RETURN ("No error message found for command "
			       "%02Xh and completion code %02Xh.  "
			       "Please report to <freeipmi-devel@gnu.org>", 
			       cmd, 
			       comp_code);
	    }
	  break;
	case IPMI_CMD_DELETE_SEL_ENTRY:
	  switch (comp_code)
	    {
	    case IPMI_ERR_SEL_OPERATION_NOT_SUPPORTED:
	      SNPRINTF_RETURN ("Operation not supported for this Record Type");
	      
	    case IPMI_ERR_SEL_ERASE_IN_PROGRESS:
	      SNPRINTF_RETURN ("Cannot execute command, SEL erase in progress");
	    }
	  break;
	}
      SNPRINTF_RETURN ("No error message found for command "
		       "%02Xh and completion code %02Xh.  "
		       "Please report to <freeipmi-devel@gnu.org>", 
		       cmd, 
		       comp_code);
    }
  
  SNPRINTF_RETURN ("Unknown completion code %02Xh for command %02Xh.", 
		   comp_code, 
		   cmd);
}

int8_t 
ipmi_strerror_cmd_r (fiid_obj_t obj_cmd, 
		     char *errstr, 
		     size_t len)
{
  uint8_t cmd, comp_code;
  
  if (obj_cmd == NULL || errstr == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  cmd = obj_cmd[0];
  comp_code = obj_cmd[1];
  
  return ipmi_strerror_r (cmd, comp_code, errstr, len); 
}

int8_t 
ipmi_kcs_strstatus_r (uint8_t status_code, 
		      char *errstr, 
		      size_t len)
{
  if (errstr == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  switch (status_code)
    {
    case IPMI_KCS_STATUS_NO_ERR:
      SNPRINTF_RETURN ("No error");
      
    case IPMI_KCS_STATUS_ABORTED_BY_CMD:
      SNPRINTF_RETURN ("Aborted by command (Transfer "
		       "in progress was aborted by SMS "
		       "issuing the Abort/Status control code)");
      
    case IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE:
      SNPRINTF_RETURN ("Illegal control code");
      
    case IPMI_KCS_STATUS_LEN_ERR:
      SNPRINTF_RETURN ("Length error (e.g.overrun)"); 
      
    case IPMI_KCS_STATUS_UNSPECIFIED_ERR:
      SNPRINTF_RETURN ("Unspecified error"); 
    }
  
  if ((status_code >= IPMI_KCS_STATUS_OEM_ERR_BEGIN) &&
      (status_code <= IPMI_KCS_STATUS_OEM_ERR_END))
    {
      SNPRINTF_RETURN ("OEM status code %02Xh.", status_code);
    }
  
  SNPRINTF_RETURN ("Unknown KCS interface status code %02Xh.", status_code);
};

void 
ipmi_error (fiid_obj_t obj_cmd, const char *s)
{
  char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
  
  if (obj_cmd == NULL)
    return;
  
  ipmi_strerror_cmd_r (obj_cmd, errmsg, IPMI_ERR_STR_MAX_LEN);
  
  fprintf (stderr, 
	   "%s%s" "ipmi command %02Xh: %s\n", 
	   (s ? s : ""), 
	   (s ? ": " : ""), 
	   obj_cmd[0], 
	   errmsg);
}

