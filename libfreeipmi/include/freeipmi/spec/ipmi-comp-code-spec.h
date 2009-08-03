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


#ifndef _IPMI_COMP_CODE_SPEC_H
#define	_IPMI_COMP_CODE_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_COMP_CODE_COMMAND_SUCCESS                               0x00
#define IPMI_COMP_CODE_COMMAND_SUCCESS_STR \
"Command Completed Normally."

#define IPMI_COMP_CODE_NODE_BUSY                                     0xC0
#define IPMI_COMP_CODE_NODE_BUSY_STR \
"Node Busy. Command could not be processed because command " \
"processing resources are temporarily unavailable."

#define IPMI_COMP_CODE_COMMAND_INVALID                               0xC1
#define IPMI_COMP_CODE_COMMAND_INVALID_STR \
"Invalid Command. Used to indicate an unrecognized or unsupported command."


#define IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN                       0xC2
#define IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN_STR \
"Command invalid for given LUN."

#define IPMI_COMP_CODE_COMMAND_TIMEOUT                               0xC3
#define IPMI_COMP_CODE_COMMAND_TIMEOUT_STR \
"Timeout while processing command. Response unavailable."

#define IPMI_COMP_CODE_OUT_OF_SPACE                                  0xC4
#define IPMI_COMP_CODE_OUT_OF_SPACE_STR \
"Out of space. Command could not be completed because of a " \
"lack of storage space required to execute the given command " \
"operation."

#define IPMI_COMP_CODE_RESERVATION_CANCELLED                         0xC5
#define IPMI_COMP_CODE_RESERVATION_CANCELLED_STR \
"Reservation Canceled or Invalid Reservation ID."

#define IPMI_COMP_CODE_INVALID_RESERVATION_ID     IPMI_COMP_CODE_RESERVATION_CANCELLED
#define IPMI_COMP_CODE_INVALID_RESERVATION_ID_STR IPMI_COMP_CODE_RESERVATION_CANCELLED_STR

#define IPMI_COMP_CODE_REQUEST_DATA_TRUNCATED                        0xC6
#define IPMI_COMP_CODE_REQUEST_DATA_TRUNCATED_STR \
"Request data truncated."

#define IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID                   0xC7
#define IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID_STR \
"Request data length invalid."

#define IPMI_COMP_CODE_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED            0xC8
#define IPMI_COMP_CODE_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED_STR \
"Request data field length limit exceeded."

#define IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE                        0xC9
#define IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE_STR \
"Parameter out of range. One or more parameters in the data " \
"field of the Request are out of range. This is different from  " \
"'Invalid data field' (CCh) code in that it indicates that the " \
"erroneous field(s) has a contiguous range of possible values."

#define IPMI_COMP_CODE_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES       0xCA
#define IPMI_COMP_CODE_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES_STR \
"Cannot return number of requested data bytes."

#define IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT     0xCB
#define IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT_STR \
"Requested Sensor, data, or record not present."

#define IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD                    0xCC
#define IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD_STR \
"Invalid data field in Request"

#define IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE     0xCD
#define IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE_STR \
"Command illegal for specified sensor or record type."

#define IPMI_COMP_CODE_COMMAND_CANNOT_RESPOND                        0xCE
#define IPMI_COMP_CODE_COMMAND_CANNOT_RESPOND_STR \
"Command response could not be provided."

#define IPMI_COMP_CODE_COMMAND_DUPLICATE_REQUEST                     0xCF
#define IPMI_COMP_CODE_COMMAND_DUPLICATE_REQUEST_STR \
"Cannot execute duplicated request. This completion code is " \
"for devices which cannot return the response that was returned " \
"for the original instance of the request. Such devices should " \
"provide separate commands that allow the completion status of " \
"the original request to be determined. An Event Receiver does not " \
"use this completion code, but returns the 00h completion code in " \
"the response to (valid) duplicated requests."

#define IPMI_COMP_CODE_SDR_UPDATE_MODE                               0xD0
#define IPMI_COMP_CODE_SDR_UPDATE_MODE_STR \
"Command response could not be provided. SDR Repository in "\
"update mode."

#define IPMI_COMP_CODE_FIRMWARE_UPDATE_MODE                          0xD1
#define IPMI_COMP_CODE_FIRMWARE_UPDATE_MODE_STR \
"Command response could not be provided. Device in firmware " \
"update mode."

#define IPMI_COMP_CODE_BMC_INIT_MODE                                 0xD2
#define IPMI_COMP_CODE_BMC_INIT_MODE_STR \
"Command response could not be provided. BMC initialization or " \
"initialization agent in progress."

#define IPMI_COMP_CODE_DESTINATION_UNAVAILABLE                       0xD3
#define IPMI_COMP_CODE_DESTINATION_UNAVAILABLE_STR \
"Destination unavailable. Cannot deliver request to selected " \
"destination. E.g. this code can be returned if a request message " \
"is targeted to SMS, but receive message queue reception is disabled " \
"for the particular channel."

#define IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL                  0xD4
#define IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL_STR \
"Cannot execute command due to innsufficient privilege level or other " \
"security-based restriction (e.g. disabled for 'firmware firewall')."

#define IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED               0xD5
#define IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED_STR \
"Cannot execute command. Command, or request parameter(s), not " \
"supported in present state."

#define IPMI_COMP_CODE_REQUEST_PARAMETER_ILLEGAL                     0xD6
#define IPMI_COMP_CODE_REQUEST_PARAMETER_ILLEGAL_STR \
"Cannot execute command. Parameter is illegal because command " \
"sub-function has been disabled or is unavailable " \
"(e.g. disabled for 'firmware firewall')."

#define IPMI_COMP_CODE_UNSPECIFIED_ERROR                             0xFF
#define IPMI_COMP_CODE_UNSPECIFIED_ERROR_STR \
"Unspecified error."

/* DEVICE-SPECIFIC (OEM) CODES 01h-7Eh
   This range is used for command-specific codes that are also
   specific for a particular device and version. A-priori knowledge of
   the device command set is required for interpretation of these
   codes. */ 

/* COMMAND-SPECIFIC CODES 80h-BEh 80h-BEh 
   Standard command-specific codes. This range is reserved for
   command-specific completion codes for commands specified in this
   document. */

  /* 
   * IPMI Device "Global Commands"
   */


  /* 
   * BMC Watchdog Timer Commands
   */

/* IPMI_CMD_RESET_WATCHDOG_TIMER */
#define IPMI_COMP_CODE_ATTEMPT_TO_START_UNINITIALIZED_WATCHDOG            0x80
#define IPMI_COMP_CODE_ATTEMPT_TO_START_UNINITIALIZED_WATCHDOG_STR \
"Attempt to start un-initialized watchdog."

  /* 
   * BMC Device and Messaging Commands
   */

/* IPMI_CMD_GET_MESSAGE */

#define IPMI_COMP_CODE_DATA_NOT_AVAILABLE                                 0x80
#define IPMI_COMP_CODE_DATA_NOT_AVAILABLE_STR \
"data not available (queue/buffer empty)"

/* IPMI_CMD_SET_SYSTEM_INFO_PARAMETERS */

#define IPMI_COMP_CODE_SET_SYSTEM_INFO_PARAMETER_NOT_SUPPORTED            0x80
#define IPMI_COMP_CODE_SET_SYSTEM_INFO_PARAMETER_NOT_SUPPORTED_STR \
  "parameter not supported."

#define IPMI_COMP_CODE_SET_SYSTEM_INFO_INVALID_SET_IN_PROGRESS            0x81
#define IPMI_COMP_CODE_SET_SYSTEM_INFO_INVALID_SET_IN_PROGRESS_STR \
  "attempt to set the 'set in progress' value (in parameter #0) " \
  "when not int the 'set complete' state."

#define IPMI_COMP_CODE_SET_SYSTEM_INFO_WRITE_READ_ONLY_PARAMETER          0x82
#define IPMI_COMP_CODE_SET_SYSTEM_INFO_WRITE_READ_ONLY_PARAMETER_STR \
  "attempt to write read-only parameter"

#define IPMI_COMP_CODE_SET_SYSTEM_INFO_READ_WRITE_ONLY_PARAMETER          0x83
#define IPMI_COMP_CODE_SET_SYSTEM_INFO_READ_WRITE_ONLY_PARAMETER_STR \
  "attempt to read write-only parameter"

/* IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS */

#define IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETER_NOT_SUPPORTED            0x80
#define IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETER_NOT_SUPPORTED_STR \
  "parameter not supported."

/* IPMI_CMD_GET_SESSION_CHALLENGE */

#define IPMI_COMP_CODE_INVALID_USERNAME                                   0x81
#define IPMI_COMP_CODE_INVALID_USERNAME_STR \
"Invalid user name"

#define IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED                          0x82
#define IPMI_COMP_CODE_NULL_USERNAME_NOT_ENABLED_STR \
"Null user name (User 1) not enabled"

/* IPMI_CMD_ACTIVATE_SESSION */

#define IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE                          0x81
#define IPMI_COMP_CODE_NO_SESSION_SLOT_AVAILABLE_STR \
"No session slot available (BMC cannot accept any more sessions)"

#define IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER                   0x82
#define IPMI_COMP_CODE_NO_SLOT_AVAILABLE_FOR_GIVEN_USER_STR \
"No slot available for given user. (Limit of user sessions " \
"allowed under that name has been reached)"

#define IPMI_COMP_CODE_NO_SLOT_AVAILABLE_TO_SUPPORT_USER                  0x83
#define IPMI_COMP_CODE_NO_SLOT_AVAILALBE_TO_SUPPORT_USER_STR \
"No slot available to support user due to maximum privilege " \
"capability. (An implementation may only be able to support " \
"a certain number of sessions based on what authentication " \
"resources are required. For example, if User Level " \
"Authentication is disabled, an implementation may be able " \
"to allow a larger number of users that are limited to User " \
"Level privilege, than users that require higher privilege."

#define IPMI_COMP_CODE_SESSION_SEQ_NUM_OUT_OF_RANGE                       0x84
#define IPMI_COMP_CODE_SESSION_SEQ_NUM_OUT_OF_RANGE_STR \
"Session sequence number out-of-range"

#define IPMI_COMP_CODE_INVALID_SESSION_ID                                 0x85
#define IPMI_COMP_CODE_INVALID_SESSION_ID_STR \
"Invalid session ID in request"

#define IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL                            0x86
#define IPMI_COMP_CODE_EXCEEDS_PRIVILEGE_LEVEL_STR \
"Requested maximum privilege level exceeds user and/or " \
"channel privilege limit"

/* IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL */

#define IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER                    0x81
#define IPMI_COMP_CODE_RQ_LEVEL_NOT_AVAILABLE_FOR_USER_STR \
"Requested level not available for this user"

#define IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT              0x82
#define IPMI_COMP_CODE_RQ_LEVEL_EXCEEDS_USER_PRIVILEGE_LIMIT_STR \
"Requested level exceeds Channel and/or User Privilege Limit"

#define IPMI_COMP_CODE_CANNOT_DISABLE_USER_LEVEL_AUTHENTICATION           0x83
#define IPMI_COMP_CODE_CANNOT_DISABLE_USER_LEVEL_AUTHENTICATION_STR \
"Cannot disable User Level authentication"

/* IPMI_CMD_CLOSE_SESSION */

#define IPMI_COMP_CODE_INVALID_SESSION_ID_IN_RQ                           0x87
#define IPMI_COMP_CODE_INVALID_SESSION_ID_IN_RQ_STR \
"Invalid session ID in request"

/* IPMI_CMD_SET_CHANNEL_ACCESS */
#define IPMI_COMP_CODE_SET_NOT_SUPPORTED_ON_SELECTED_CHANNEL              0x82
#define IPMI_COMP_CODE_SET_NOT_SUPPORTED_ON_SELECTED_CHANNEL_STR \
"set not supported on selected channel"

#define IPMI_COMP_CODE_ACCESS_MODE_NOT_SUPPORTED                          0x83
#define IPMI_COMP_CODE_ACCESS_MODE_NOT_SUPPORTED_STR \
"access mode not supported"

/* IPMI_CMD_GET_CHANNEL_ACCESS */
#define IPMI_COMP_CODE_COMMAND_NOT_SUPPORTED_FOR_SELECTED_CHANNEL         0x82
#define IPMI_COMP_CODE_COMMAND_NOT_SUPPORTED_FOR_SELECTED_CHANNEL_STR \
"command not supported for selected channel"

/* IPMI_CMD_SET_USER_PASSWORD_CMD */
#define IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT         0x80
#define IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_CORRECT_STR \
"password test failed.  Password size correct, but password " \
"data does not match stored value."

#define IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT       0x81
#define IPMI_COMP_CODE_PASSWORD_TEST_FAILED_PASSWORD_SIZE_INCORRECT_STR \
"password test failed.  Wrong password size was used."

/* IPMI_CMD_ACTIVATE_PAYLOAD */
#define IPMI_COMP_CODE_PAYLOAD_ALREADY_ACTIVE_ON_ANOTHER_SESSION          0x80
#define IPMI_COMP_CODE_PAYLOAD_ALREADY_ACTIVE_ON_ANOTHER_SESSION_STR \
"Payload already active on another session"

#define IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED                           0x81
#define IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED_STR \
"Payload type disabled.  Given payload type is not configured " \
"to be enabled for activation."

#define IPMI_COMP_CODE_PAYLOAD_ACTIVATION_LIMIT_REACHED                   0x82
#define IPMI_COMP_CODE_PAYLOAD_ACTIVATION_LIMIT_REACHED_STR \
"Payload activation limit reached.  Cannot activate given payload type " \
"because the maximum number of simultaneous instances of that payload type " \
"are already running."

#define IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITH_ENCRYPTION            0x83
#define IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITH_ENCRYPTION_STR \
"Cannot activate payload with encryption."

#define IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION         0x84
#define IPMI_COMP_CODE_CANNOT_ACTIVATE_PAYLOAD_WITHOUT_ENCRYPTION_STR \
"Cannot activate payload without encryption.  BMC requires encryption " \
"for all payloads for given privilege level."

/* IPMI_CMD_DEACTIVATE_PAYLOAD */
#define IPMI_COMP_CODE_PAYLOAD_ALREADY_DEACTIVATED                        0x80
#define IPMI_COMP_CODE_PAYLOAD_ALREADY_DEACTIVATED_STR \
"Payload already deactivated"

#define IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED                           0x81
#define IPMI_COMP_CODE_PAYLOAD_TYPE_IS_DISABLED_STR \
"Payload type disabled.  Given payload type is not configured " \
"to be enabled for activation."

/* IPMI_CMD_GET_CHANNEL_PAYLOAD_VERSION */
#define IPMI_COMP_CODE_PAYLOAD_TYPE_NOT_AVAILABLE_ON_GIVEN_CHANNEL        0x80
#define IPMI_COMP_CODE_PAYLOAD_TYPE_NOT_AVAILABLE_ON_GIVEN_CHANNEL_STR \
"Payload type not available on given channel"

/* IPMI_CMD_GET_CHANNEL_OEM_PAYLOAD_INFO */
#define IPMI_COMP_CODE_OEM_PAYLOAD_IANA_OR_PAYLOAD_ID_NOT_SUPPORTED       0x80
#define IPMI_COMP_CODE_OEM_PAYLOAD_IANA_OR_PAYLOAD_ID_NOT_SUPPORTED_STR \
"OEM Payload IANA and/or Payload ID not supported"

/* IPMI_CMD_SUSPEND_RESUME_PAYLOAD_ENCRYPTION */
#define IPMI_COMP_CODE_OPERATION_NOT_SUPPORTED                            0x80
#define IPMI_COMP_CODE_OPERATION_NOT_SUPPORTED_STR \
"Operation not supported for given payload type."

#define IPMI_COMP_CODE_OPERATION_NOT_ALLOWED_UNDER_PRESENT_CONFIGURATION  0x81
#define IPMI_COMP_CODE_OPERATION_NOT_ALLOWED_UNDER_PRESENT_CONFIGURATION_STR \
"Operation now allowed under present configuration for given payload type."

#define IPMI_COMP_CODE_ENCRYPTION_IS_NOT_AVAILABLE_FOR_SESSION            0x82
#define IPMI_COMP_CODE_ENCRYPTION_IS_NOT_AVAILABLE_FOR_SESSION_STR \
"Encryption is not available for session that payload type is active under."

#define IPMI_COMP_CODE_PAYLOAD_INSTANCE_NOT_PRESENTLY_ACTIVE              0x83
#define IPMI_COMP_CODE_PAYLOAD_INSTANCE_NOT_PRESENTLY_ACTIVE_STR \
"The payload instance is not presently active."

/* IPMI_CMD_SET_CHANNEL_SECURITY_KEYS */
#define IPMI_COMP_CODE_CANNOT_PERFORM_SET_CONFIRM_KEY_IS_LOCKED          0x80
#define IPMI_COMP_CODE_CANNOT_PERFORM_SET_CONFIRM_KEY_IS_LOCKED_STR \
"Cannot perform set/confirm.  Key is locked"

#define IPMI_COMP_CODE_INSUFFICIENT_KEY_BYTES                            0x81
#define IPMI_COMP_CODE_INSUFFICIENT_KEY_BYTES_STR \
"insufficient key bytes"

#define IPMI_COMP_CODE_TOO_MANY_KEY_BYTES                                0x82
#define IPMI_COMP_CODE_TOO_MANY_KEY_BYTES_STR \
"too many key bytes"

#define IPMI_COMP_CODE_KEY_VALUE_DOES_NOT_MEET_CRITERIA                  0x83
#define IPMI_COMP_CODE_KEY_VALUE_DOES_NOT_MEET_CRITERIA_STR \
"key value does not meet criteria for specified type of key"

#define IPMI_COMP_CODE_KR_IS_NOT_USED                                    0x84
#define IPMI_COMP_CODE_KR_IS_NOT_USED_STR \
"K_R is not used.  BMC uses a random number generation approach " \
"that does not require a K_R value"

  /* 
   * Chassis Device Commands
   */

/* IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS */

#define IPMI_COMP_CODE_SET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED            0x80
#define IPMI_COMP_CODE_SET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

#define IPMI_COMP_CODE_SET_BOOT_OPTION_INVALID_SET_IN_PROGRESS            0x81
#define IPMI_COMP_CODE_SET_BOOT_OPTION_INVALID_SET_IN_PROGRESS_STR \
"attempt to set the 'set in progress' value (in parameter #0) " \
"when not int the 'set complete' state."

#define IPMI_COMP_CODE_SET_BOOT_OPTION_WRITE_READ_ONLY_PARAMETER          0x82
#define IPMI_COMP_CODE_SET_BOOT_OPTION_WRITE_READ_ONLY_PARAMETER_STR \
"attempt to write read-only parameter"

#define IPMI_COMP_CODE_SET_BOOT_OPTION_READ_WRITE_ONLY_PARAMETER          0x83
#define IPMI_COMP_CODE_SET_BOOT_OPTION_READ_WRITE_ONLY_PARAMETER_STR \
"attempt to read write-only parameter"

/* IPMI_CMD_GET_SYSTEM_BOOT_OPTIONS */

#define IPMI_COMP_CODE_GET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED            0x80
#define IPMI_COMP_CODE_GET_BOOT_OPTION_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

  /* 
   * Event Commands
   */

  /* 
   * PEF and Alerting Commands
   */

/* IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS */

#define IPMI_COMP_CODE_SET_PEF_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_SET_PEF_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

#define IPMI_COMP_CODE_SET_PEF_INVALID_SET_IN_PROGRESS                    0x81
#define IPMI_COMP_CODE_SET_PEF_INVALID_SET_IN_PROGRESS_STR \
"attempt to set the 'set in progress' value (in parameter #0) " \
"when not int the 'set complete' state."

#define IPMI_COMP_CODE_SET_PEF_WRITE_READ_ONLY_PARAMETER                  0x82
#define IPMI_COMP_CODE_SET_PEF_WRITE_READ_ONLY_PARAMETER_STR \
"attempt to write read-only parameter"

#define IPMI_COMP_CODE_SET_PEF_READ_WRITE_ONLY_PARAMETER                  0x83
#define IPMI_COMP_CODE_SET_PEF_READ_WRITE_ONLY_PARAMETER_STR \
"attempt to read write-only parameter"

/* IPMI_CMD_GET_PEF_CONFIGURATION_PARAMETERS */

#define IPMI_COMP_CODE_GET_PEF_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_GET_PEF_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

/* IPMI_CMD_SET_LAST_PROCESSED_EVENT_ID */
#define IPMI_COMP_CODE_SET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS  0x81
#define IPMI_COMP_CODE_SET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS_STR \
"cannot execute command, SEL erase in progress"

/* IPMI_CMD_GET_LAST_PROCESSED_EVENT_ID */
#define IPMI_COMP_CODE_GET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS  0x81
#define IPMI_COMP_CODE_GET_LAST_PROCESSED_EVENT_ID_SEL_ERASE_IN_PROGRESS_STR \
"cannot execute command, SEL erase in progress"

/* IPMI_CMD_ALERT_IMMEDIATE */
#define IPMI_COMP_CODE_ALERT_ALREADY_IN_PROGRESS                          0x81
#define IPMI_COMP_CODE_ALERT_ALREADY_IN_PROGRESS_STR \
"Alert Immediate rejected due to alert already in progress"

#define IPMI_COMP_CODE_ALERT_IPMI_MESSAGING_SESSION_ACTIVE                0x82
#define IPMI_COMP_CODE_ALERT_IPMI_MESSAGING_SESSION_ACTIVE_STR \
"Alert Immedate rejected due to IPMI messaging session active on this channel"

  /* 
   * Sensor Device Commands
   */

  /* 
   * FRU Device Commands
   */

#define IPMI_COMP_CODE_WRITE_PROTECTED_OFFSET                             0x80
#define IPMI_COMP_CODE_WRITE_PROTECTED_OFFSET_STR \
"write-protected offset. Cannot complete write because one or more " \
"bytes of FRU data are to a write-protected offset in the FRU device. " \
"Note that an implementation may have allowed a 'partial write' of the " \
"data to occur."

#define IPMI_COMP_CODE_FRU_DEVICE_BUSY                                    0x81
#define IPMI_COMP_CODE_FRU_DEVICE_BUSY_STR \
"FRU device busy. The requested cannot be completed because the " \
"implementation of the logical FRU device is in a state where the FRU " \
"information is temporarily unavailable.  This could be due to a " \
"condition such as a loss of arbitration if the FRU is implemented as a " \
"device on a shared bus."

  /* 
   * SDR Device Commands
   */

  /* 
   * SEL Device Commands
   */

/* IPMI_CMD_GET_SEL_ENTRY */
#define IPMI_COMP_CODE_GET_SEL_ENTRY_SEL_ERASE_IN_PROGRESS                0x81
#define IPMI_COMP_CODE_GET_SEL_ENTRY_SEL_ERASE_IN_PROGRESS_STR \
"cannot execute command, SEL erase in progress"

/* IPMI_CMD_DELETE_SEL_ENTRY */
#define IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_OPERATION_NOT_SUPPORTED       0x80
#define IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_OPERATION_NOT_SUPPORTED_STR \
"Operation not supported for this Record Type"

#define IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_ERASE_IN_PROGRESS             0x81
#define IPMI_COMP_CODE_DELETE_SEL_ENTRY_SEL_ERASE_IN_PROGRESS_STR \
"cannot execute command, SEL erase in progress"

  /* 
   * LAN Device Commands
   */

/* IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS */

#define IPMI_COMP_CODE_SET_LAN_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_SET_LAN_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

#define IPMI_COMP_CODE_SET_LAN_INVALID_SET_IN_PROGRESS                    0x81
#define IPMI_COMP_CODE_SET_LAN_INVALID_SET_IN_PROGRESS_STR \
"attempt to set the 'set in progress' value (in parameter #0) " \
"when not int the 'set complete' state."

#define IPMI_COMP_CODE_SET_LAN_WRITE_READ_ONLY_PARAMETER                  0x82
#define IPMI_COMP_CODE_SET_LAN_WRITE_READ_ONLY_PARAMETER_STR \
"attempt to write read-only parameter"

#define IPMI_COMP_CODE_SET_LAN_READ_WRITE_ONLY_PARAMETER                  0x83
#define IPMI_COMP_CODE_SET_LAN_READ_WRITE_ONLY_PARAMETER_STR \
"attempt to read write-only parameter"

/* IPMI_CMD_GET_LAN_CONFIGURATION_PARAMETERS */

#define IPMI_COMP_CODE_GET_LAN_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_GET_LAN_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

  /* 
   * Serial/Modem Device Commands
   */

/* IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION */
#define IPMI_COMP_CODE_SET_SERIAL_PARAMETER_NOT_SUPPORTED                 0x80
#define IPMI_COMP_CODE_SET_SERIAL_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

#define IPMI_COMP_CODE_SET_SERIAL_INVALID_SET_IN_PROGRESS                 0x81
#define IPMI_COMP_CODE_SET_SERIAL_INVALID_SET_IN_PROGRESS_STR \
"attempt to set the 'set in progress' value (in parameter #0) " \
"when not int the 'set complete' state."

#define IPMI_COMP_CODE_SET_SERIAL_WRITE_READ_ONLY_PARAMETER               0x82
#define IPMI_COMP_CODE_SET_SERIAL_WRITE_READ_ONLY_PARAMETER_STR \
"attempt to write read-only parameter"

#define IPMI_COMP_CODE_SET_SERIAL_READ_WRITE_ONLY_PARAMETER               0x83
#define IPMI_COMP_CODE_SET_SERIAL_READ_WRITE_ONLY_PARAMETER_STR \
"attempt to read write-only parameter"

/* IPMI_CMD_GET_SERIAL_MODEM_CONFIGURATION */
#define IPMI_COMP_CODE_GET_SERIAL_PARAMETER_NOT_SUPPORTED                 0x80
#define IPMI_COMP_CODE_GET_SERIAL_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

/* IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS */

#define IPMI_COMP_CODE_SET_SOL_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_SET_SOL_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

#define IPMI_COMP_CODE_SET_SOL_INVALID_SET_IN_PROGRESS                    0x81
#define IPMI_COMP_CODE_SET_SOL_INVALID_SET_IN_PROGRESS_STR \
"attempt to set the 'set in progress' value (in parameter #0) " \
"when not int the 'set complete' state."

#define IPMI_COMP_CODE_SET_SOL_WRITE_READ_ONLY_PARAMETER                  0x82
#define IPMI_COMP_CODE_SET_SOL_WRITE_READ_ONLY_PARAMETER_STR \
"attempt to write read-only parameter"

#define IPMI_COMP_CODE_SET_SOL_READ_WRITE_ONLY_PARAMETER                  0x83
#define IPMI_COMP_CODE_SET_SOL_READ_WRITE_ONLY_PARAMETER_STR \
"attempt to read write-only parameter"

/* IPMI_CMD_GET_SOL_CONFIGURATION_PARAMETERS */

#define IPMI_COMP_CODE_GET_SOL_PARAMETER_NOT_SUPPORTED                    0x80
#define IPMI_COMP_CODE_GET_SOL_PARAMETER_NOT_SUPPORTED_STR \
"parameter not supported."

  /* 
   * Bridge Management Commands (ICMB)
   */

  /* 
   * Discovery Commands (ICMB)
   */

  /* 
   * Bridging Commands (ICMB)
   */

  /* 
   * Event Commands (ICMB)
   */

  /* 
   * OEM Commands for Bridge NetFN
   */

  /* 
   * OEM Bridge Commands
   */

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_COMP_CODE_SPEC_H */

