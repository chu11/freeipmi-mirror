/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/util/ipmi-error-util.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-rmcpplus-status-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

#define SNPRINTF_RETURN(arg...)    \
do				   \
{				   \
  snprintf (errstr, len, arg);	   \
  return 0;                        \
} while (0)

int8_t 
ipmi_completion_code_strerror_r (uint8_t cmd, 
                                 uint8_t netfn,
                                 uint8_t comp_code, 
                                 char *errstr, 
                                 size_t len)
{
  ERR_EINVAL (errstr);
  
  switch (comp_code)
    {
    case IPMI_COMP_CODE_COMMAND_SUCCESS:
      SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_SUCCESS_STR);

    case IPMI_COMP_CODE_NODE_BUSY:
      SNPRINTF_RETURN (IPMI_COMP_CODE_NODE_BUSY_STR);

    case IPMI_COMP_CODE_COMMAND_INVALID:
      SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_INVALID_STR);

    case IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN:
      SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN_STR);

    case IPMI_COMP_CODE_COMMAND_TIMEOUT:
      SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_TIMEOUT_STR);

    case IPMI_COMP_CODE_OUT_OF_SPACE:
      SNPRINTF_RETURN (IPMI_COMP_CODE_OUT_OF_SPACE_STR);

    case IPMI_COMP_CODE_RESERVATION_CANCELLED:
      SNPRINTF_RETURN (IPMI_COMP_CODE_RESERVATION_CANCELLED_STR);

    case IPMI_COMP_CODE_REQUEST_DATA_TRUNCATED:
      SNPRINTF_RETURN (IPMI_COMP_CODE_REQUEST_DATA_TRUNCATED_STR);

    case IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID:
      SNPRINTF_RETURN (IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID_STR);

    case IPMI_COMP_CODE_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED:
      SNPRINTF_RETURN (IPMI_COMP_CODE_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED_STR);

    case IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE:
      SNPRINTF_RETURN (IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE_STR);

    case IPMI_COMP_CODE_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES:
      SNPRINTF_RETURN (IPMI_COMP_CODE_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES_STR);

    case IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT:
      SNPRINTF_RETURN (IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT_STR);

    case IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD:
      SNPRINTF_RETURN (IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD_STR);

    case IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE:
      SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE_STR);

    case IPMI_COMP_CODE_COMMAND_CANNOT_RESPOND:
      SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_CANNOT_RESPOND_STR);

    case IPMI_COMP_CODE_COMMAND_DUPLICATE_REQUEST:
      SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_DUPLICATE_REQUEST_STR);

    case IPMI_COMP_CODE_SDR_UPDATE_MODE:
      SNPRINTF_RETURN (IPMI_COMP_CODE_SDR_UPDATE_MODE_STR);

    case IPMI_COMP_CODE_FIRMWARE_UPDATE_MODE:
      SNPRINTF_RETURN (IPMI_COMP_CODE_FIRMWARE_UPDATE_MODE_STR);

    case IPMI_COMP_CODE_BMC_INIT_MODE:
      SNPRINTF_RETURN (IPMI_COMP_CODE_BMC_INIT_MODE_STR);

    case IPMI_COMP_CODE_DESTINATION_UNAVAILABLE:
      SNPRINTF_RETURN (IPMI_COMP_CODE_DESTINATION_UNAVAILABLE_STR);

    case IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL:
      SNPRINTF_RETURN (IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL_STR);

    case IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED:
      SNPRINTF_RETURN (IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED_STR);

    case IPMI_COMP_CODE_REQUEST_PARAMETER_ILLEGAL:
      SNPRINTF_RETURN (IPMI_COMP_CODE_REQUEST_PARAMETER_ILLEGAL_STR);

    case IPMI_COMP_CODE_UNSPECIFIED_ERROR:
      SNPRINTF_RETURN (IPMI_COMP_CODE_UNSPECIFIED_ERROR_STR);
    }

  /* OEM completion codes */
  if ((comp_code >= 0x01) && (comp_code <= 0x7E))
    SNPRINTF_RETURN ("Device specific (OEM) completion code %02Xh.", comp_code);

  /* Command specific completion codes */
  if ((comp_code >= 0x80) && (comp_code <= 0xBE)) 
    {
      ERR_EINVAL (IPMI_NET_FN_VALID(netfn));
      
      switch (netfn)
        {
        case IPMI_NET_FN_CHASSIS_RQ:
        case IPMI_NET_FN_CHASSIS_RS:

          switch (cmd) 
            {
            case IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_SET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED_STR);
                case IPMI_COMP_CODE_SET_BOOT_OPTION_INVALID_SET_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_BOOT_OPTION_INVALID_SET_IN_PROGRESS_STR);
                case IPMI_COMP_CODE_SET_BOOT_OPTION_WRITE_READ_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_BOOT_OPTION_WRITE_READ_ONLY_PARAMETER_STR);
                case IPMI_COMP_CODE_SET_BOOT_OPTION_READ_WRITE_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_BOOT_OPTION_READ_WRITE_ONLY_PARAMETER_STR);
                }
              break;
            case IPMI_CMD_GET_SYSTEM_BOOT_OPTIONS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_GET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_GET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED_STR);
                }
              break;             
            }
          break;
        case IPMI_NET_FN_BRIDGE_RQ:
        case IPMI_NET_FN_BRIDGE_RS:
          break;

        case IPMI_NET_FN_SENSOR_EVENT_RQ:
        case IPMI_NET_FN_SENSOR_EVENT_RS:
          switch (cmd) 
            {
            case IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_SET_PEF_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_PEF_PARAMETER_NOT_SUPPORTED_STR);
                case IPMI_COMP_CODE_SET_PEF_INVALID_SET_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_PEF_INVALID_SET_IN_PROGRESS_STR);
                case IPMI_COMP_CODE_SET_PEF_WRITE_READ_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_PEF_WRITE_READ_ONLY_PARAMETER_STR);
                case IPMI_COMP_CODE_SET_PEF_READ_WRITE_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_PEF_READ_WRITE_ONLY_PARAMETER_STR);
                }
              break;
            case IPMI_CMD_GET_PEF_CONFIGURATION_PARAMETERS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_GET_PEF_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_GET_PEF_PARAMETER_NOT_SUPPORTED_STR);
                }
              break;
            case IPMI_CMD_SET_LAST_PROCESSED_EVENT_ID:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_SET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS_STR);
                }
              break;
              
            case IPMI_CMD_GET_LAST_PROCESSED_EVENT_ID:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_GET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_GET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS_STR);
                }
              break;
            case IPMI_CMD_ALERT_IMMEDIATE:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_ALERT_ALREADY_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_ALERT_ALREADY_IN_PROGRESS_STR);
                  
                case IPMI_COMP_CODE_ALERT_IPMI_MESSAGING_SESSION_ACTIVE:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_ALERT_IPMI_MESSAGING_SESSION_ACTIVE_STR);
                }
              break;
            }
          break;

        case IPMI_NET_FN_APP_RQ:
        case IPMI_NET_FN_APP_RS:
          switch (cmd)
            {
            case IPMI_CMD_RESET_WATCHDOG_TIMER:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_ATTEMPT_TO_START_UNINITIALIZED_WATCHDOG:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_ATTEMPT_TO_START_UNINITIALIZED_WATCHDOG_STR);
                }
              break;
	    case IPMI_CMD_GET_MESSAGE:
	      switch (comp_code)
		{
		case IPMI_COMP_CODE_DATA_NOT_AVAILABLE:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_DATA_NOT_AVAILABLE_STR);
		}
	      break;
            case IPMI_CMD_GET_SESSION_CHALLENGE:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_INVALID_USERNAME:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_INVALID_USERNAME_STR);
                case IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED_STR);
                }
              break;
            case IPMI_CMD_ACTIVATE_SESSION:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE_STR);
                case IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER_STR);
                case IPMI_COMP_CODE_NO_SLOT_AVAILABLE_TO_SUPPORT_USER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_NO_SLOT_AVAILALBE_TO_SUPPORT_USER_STR);
                case IPMI_COMP_CODE_SESSION_SEQ_NUM_OUT_OF_RANGE:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SESSION_SEQ_NUM_OUT_OF_RANGE_STR);
                case IPMI_COMP_CODE_INVALID_SESSION_ID:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_INVALID_SESSION_ID_STR);
                case IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL_STR);
                }
              break;
            case IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL: 
              switch (comp_code)
                {
                case IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER_STR);
                case IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT_STR);
                case IPMI_COMP_CODE_CANNOT_DISABLE_USER_LEVEL_AUTHENTICATION:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_CANNOT_DISABLE_USER_LEVEL_AUTHENTICATION_STR);
                }
              break;
            case IPMI_CMD_CLOSE_SESSION:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_INVALID_SESSION_ID_IN_RQ:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_INVALID_SESSION_ID_IN_RQ_STR);
                }
              break;
            case IPMI_CMD_SET_CHANNEL_ACCESS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_SET_NOT_SUPPORTED_ON_SELECTED_CHANNEL:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_NOT_SUPPORTED_ON_SELECTED_CHANNEL_STR);
                case IPMI_COMP_CODE_ACCESS_MODE_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_ACCESS_MODE_NOT_SUPPORTED_STR);
                }
              break;
            case IPMI_CMD_GET_CHANNEL_ACCESS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_COMMAND_NOT_SUPPORTED_FOR_SELECTED_CHANNEL:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_COMMAND_NOT_SUPPORTED_FOR_SELECTED_CHANNEL_STR);
                }
              break;
            case IPMI_CMD_SET_USER_PASSWORD_COMMAND:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT:
                  SNPRINTF_RETURN(IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT_STR);
                case IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT:
                  SNPRINTF_RETURN(IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT_STR);
                }
              break;
	    case IPMI_CMD_ACTIVATE_PAYLOAD:
	      switch (comp_code)
		{
		case IPMI_COMP_CODE_PAYLOAD_ALREADY_ACTIVE_ON_ANOTHER_SESSION:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_PAYLOAD_ALREADY_ACTIVE_ON_ANOTHER_SESSION_STR);
		case IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED_STR);
		case IPMI_COMP_CODE_PAYLOAD_ACTIVATION_LIMIT_REACHED:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_PAYLOAD_ACTIVATION_LIMIT_REACHED_STR);
		case IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITH_ENCRYPTION:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITH_ENCRYPTION_STR);
		case IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION_STR);
		}
	      break;
	    case IPMI_CMD_DEACTIVATE_PAYLOAD:
	      switch (comp_code)
		{
		case IPMI_COMP_CODE_PAYLOAD_ALREADY_DEACTIVATED:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_PAYLOAD_ALREADY_DEACTIVATED_STR);
		case IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED_STR);
		}
	      break;
            case IPMI_CMD_GET_CHANNEL_PAYLOAD_VERSION:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_PAYLOAD_TYPE_NOT_AVAILABLE_ON_GIVEN_CHANNEL:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_PAYLOAD_TYPE_NOT_AVAILABLE_ON_GIVEN_CHANNEL_STR);
                }
              break;
            case IPMI_CMD_GET_CHANNEL_OEM_PAYLOAD_INFO:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_OEM_PAYLOAD_IANA_OR_PAYLOAD_ID_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_OEM_PAYLOAD_IANA_OR_PAYLOAD_ID_NOT_SUPPORTED_STR);
                }
              break;
	    case IPMI_CMD_SUSPEND_RESUME_PAYLOAD_ENCRYPTION:
	      switch (comp_code)
		{
		case IPMI_COMP_CODE_OPERATION_NOT_SUPPORTED:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_OPERATION_NOT_SUPPORTED_STR);
		case IPMI_COMP_CODE_OPERATION_NOT_ALLOWED_UNDER_PRESENT_CONFIGURATION:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_OPERATION_NOT_ALLOWED_UNDER_PRESENT_CONFIGURATION_STR);
		case IPMI_COMP_CODE_ENCRYPTION_IS_NOT_AVAILABLE_FOR_SESSION:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_ENCRYPTION_IS_NOT_AVAILABLE_FOR_SESSION_STR);
		case IPMI_COMP_CODE_PAYLOAD_INSTANCE_NOT_PRESENTLY_ACTIVE:
		  SNPRINTF_RETURN (IPMI_COMP_CODE_PAYLOAD_INSTANCE_NOT_PRESENTLY_ACTIVE_STR);
		}
              break;
            case IPMI_CMD_SET_CHANNEL_SECURITY_KEYS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_CANNOT_PERFORM_SET_CONFIRM_KEY_IS_LOCKED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_CANNOT_PERFORM_SET_CONFIRM_KEY_IS_LOCKED_STR);
                case IPMI_COMP_CODE_INSUFFICIENT_KEY_BYTES:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_INSUFFICIENT_KEY_BYTES_STR);
                case IPMI_COMP_CODE_TOO_MANY_KEY_BYTES:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_TOO_MANY_KEY_BYTES_STR);
                case IPMI_COMP_CODE_KEY_VALUE_DOES_NOT_MEET_CRITERIA:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_KEY_VALUE_DOES_NOT_MEET_CRITERIA_STR);
                case IPMI_COMP_CODE_KR_IS_NOT_USED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_KR_IS_NOT_USED_STR);
                }
              break;
	    }
	  break;

        case IPMI_NET_FN_FIRMWARE_RQ:
        case IPMI_NET_FN_FIRMWARE_RS:
          break;

        case IPMI_NET_FN_STORAGE_RQ:
        case IPMI_NET_FN_STORAGE_RS:
          switch(cmd)
            {
            case IPMI_CMD_READ_FRU_DATA:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_FRU_DEVICE_BUSY:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_FRU_DEVICE_BUSY_STR);
                }
              break;
            case IPMI_CMD_WRITE_FRU_DATA:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_WRITE_PROTECTED_OFFSET:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_WRITE_PROTECTED_OFFSET_STR);
                case IPMI_COMP_CODE_FRU_DEVICE_BUSY:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_FRU_DEVICE_BUSY_STR);
                }
              break;
            case IPMI_CMD_GET_SEL_ENTRY:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_GET_SEL_ENTRY_SEL_ERASE_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_GET_SEL_ENTRY_SEL_ERASE_IN_PROGRESS_STR);
                }
              break;
            case IPMI_CMD_DELETE_SEL_ENTRY:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_OPERATION_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_OPERATION_NOT_SUPPORTED_STR);
                case IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_ERASE_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_ERASE_IN_PROGRESS_STR);
                }
              break;
            }
          break;

        case IPMI_NET_FN_TRANSPORT_RQ:
        case IPMI_NET_FN_TRANSPORT_RS:
          switch (cmd) 
            {
            case IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_SET_SOL_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SOL_PARAMETER_NOT_SUPPORTED_STR);
                case IPMI_COMP_CODE_SET_SOL_INVALID_SET_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SOL_INVALID_SET_IN_PROGRESS_STR);
                case IPMI_COMP_CODE_SET_SOL_WRITE_READ_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SOL_WRITE_READ_ONLY_PARAMETER_STR);
                case IPMI_COMP_CODE_SET_SOL_READ_WRITE_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SOL_READ_WRITE_ONLY_PARAMETER_STR);
                }
              break;
            case IPMI_CMD_GET_SOL_CONFIGURATION_PARAMETERS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_GET_SOL_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_GET_SOL_PARAMETER_NOT_SUPPORTED_STR);
                }
              break;
            case IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_SET_LAN_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_LAN_PARAMETER_NOT_SUPPORTED_STR);
                case IPMI_COMP_CODE_SET_LAN_INVALID_SET_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_LAN_INVALID_SET_IN_PROGRESS_STR);
                case IPMI_COMP_CODE_SET_LAN_WRITE_READ_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_LAN_WRITE_READ_ONLY_PARAMETER_STR);
                case IPMI_COMP_CODE_SET_LAN_READ_WRITE_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_LAN_READ_WRITE_ONLY_PARAMETER_STR);
                }
              break;
            case IPMI_CMD_GET_LAN_CONFIGURATION_PARAMETERS:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_GET_LAN_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_GET_LAN_PARAMETER_NOT_SUPPORTED_STR);
                }
              break;
            case IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_SET_SERIAL_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SERIAL_PARAMETER_NOT_SUPPORTED_STR);
                case IPMI_COMP_CODE_SET_SERIAL_INVALID_SET_IN_PROGRESS:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SERIAL_INVALID_SET_IN_PROGRESS_STR);
                case IPMI_COMP_CODE_SET_SERIAL_WRITE_READ_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SERIAL_WRITE_READ_ONLY_PARAMETER_STR);
                case IPMI_COMP_CODE_SET_SERIAL_READ_WRITE_ONLY_PARAMETER:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_SET_SERIAL_READ_WRITE_ONLY_PARAMETER_STR);
                }
              break;
            case IPMI_CMD_GET_SERIAL_MODEM_CONFIGURATION:
              switch (comp_code)
                {
                case IPMI_COMP_CODE_GET_SERIAL_PARAMETER_NOT_SUPPORTED:
                  SNPRINTF_RETURN (IPMI_COMP_CODE_GET_SERIAL_PARAMETER_NOT_SUPPORTED_STR);
                }
              break;
            }
          break;
          
        default:
	  ERR_EINVAL (0);
        }

      SNPRINTF_RETURN ("No error message found for command "
                       "%02Xh, network function %02Xh, and completion code %02Xh.  "
                       "Please report to <freeipmi-devel@gnu.org>", 
                       cmd, 
                       netfn,
                       comp_code);
    }
  
  SNPRINTF_RETURN ("Unknown completion code %02Xh for command %02Xh and network function %02Xh.", 
		   comp_code, 
		   cmd,
                   netfn);
}

int8_t 
ipmi_completion_code_strerror_cmd_r (fiid_obj_t obj_cmd, 
                                     uint8_t netfn,
                                     char *errstr, 
                                     size_t len)
{
  uint64_t cmd, comp_code;
  int32_t _len;

  /* The netfn need not be valid */
  ERR_EINVAL (fiid_obj_valid(obj_cmd) && errstr);
  
  FIID_OBJ_FIELD_LOOKUP (obj_cmd, "cmd");
  FIID_OBJ_FIELD_LOOKUP (obj_cmd, "comp_code");

  FIID_OBJ_FIELD_LEN (_len, obj_cmd, "cmd");
  ERR_EINVAL (_len);

  FIID_OBJ_FIELD_LEN (_len, obj_cmd, "comp_code");
  ERR_EINVAL (_len);

  FIID_OBJ_GET(obj_cmd, "cmd", &cmd);
  FIID_OBJ_GET(obj_cmd, "comp_code", &comp_code);
  
  return ipmi_completion_code_strerror_r (cmd, netfn, comp_code, errstr, len); 
}


int8_t 
ipmi_rmcpplus_status_strerror_r(uint8_t rmcpplus_status_code,
                                char *errstr,
                                size_t len)
{
  ERR_EINVAL (errstr);

  switch (rmcpplus_status_code) 
    {
    case RMCPPLUS_STATUS_NO_ERRORS:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_NO_ERRORS_STR);
    case RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_STR);
    case RMCPPLUS_STATUS_INVALID_SESSION_ID:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_SESSION_ID_STR);
    case RMCPPLUS_STATUS_INVALID_PAYLOAD_TYPE:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_PAYLOAD_TYPE_STR);
    case RMCPPLUS_STATUS_INVALID_AUTHENTICATION_ALGORITHM:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_AUTHENTICATION_ALGORITHM_STR);
    case RMCPPLUS_STATUS_INVALID_INTEGRITY_ALGORITHM:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_INTEGRITY_ALGORITHM_STR);
    case RMCPPLUS_STATUS_NO_MATCHING_AUTHENTICATION_PAYLOAD:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_NO_MATCHING_AUTHENTICATION_PAYLOAD_STR);
    case RMCPPLUS_STATUS_NO_MATCHING_INTEGRITY_PAYLOAD:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_NO_MATCHING_INTEGRITY_PAYLOAD_STR);
    case RMCPPLUS_STATUS_INACTIVE_SESSION_ID:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INACTIVE_SESSION_ID_STR);
    case RMCPPLUS_STATUS_INVALID_ROLE:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_ROLE_STR);
    case RMCPPLUS_STATUS_UNAUTHORIZED_ROLE_OR_PRIVILEGE_LEVEL_REQUESTED:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_UNAUTHORIZED_ROLE_OR_PRIVILEGE_LEVEL_REQUESTED_STR);
    case RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INSUFFICIENT_RESOURCES_TO_CREATE_A_SESSION_AT_THE_REQUESTED_TIME_STR);
    case RMCPPLUS_STATUS_INVALID_NAME_LENGTH:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_NAME_LENGTH_STR);
    case RMCPPLUS_STATUS_UNAUTHORIZED_NAME:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_UNAUTHORIZED_NAME_STR);
    case RMCPPLUS_STATUS_UNAUTHORIZED_GUID:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_UNAUTHORIZED_GUID_STR);
    case RMCPPLUS_STATUS_INVALID_INTEGRITY_CHECK_VALUE:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_INTEGRITY_CHECK_VALUE_STR);
    case RMCPPLUS_STATUS_INVALID_CONFIDENTIALITY_ALGORITHM:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_INVALID_CONFIDENTIALITY_ALGORITHM_STR);
    case RMCPPLUS_STATUS_NO_CIPHER_SUITE_MATCH_WITH_PROPOSED_SECURITY_ALGORITHMS:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_NO_CIPHER_SUITE_MATCH_WITH_PROPOSED_SECURITY_ALGORITHMS_STR);
    case RMCPPLUS_STATUS_ILLEGAL_OR_UNRECOGNIZED_PARAMETER:
      SNPRINTF_RETURN (RMCPPLUS_STATUS_ILLEGAL_OR_UNRECOGNIZED_PARAMETER_STR);
    }

  SNPRINTF_RETURN ("Unknown rmcp+ or rakp status code %02Xh.", 
                   rmcpplus_status_code);
}
