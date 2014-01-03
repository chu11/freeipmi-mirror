/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/* 2's complement checksum of preceding bytes in the connection header
   or between the previous checksum. 8-bit checksum algorithm:
   Initialize checksum to 0.
   For each byte, checksum = (checksum + byte) modulo 256. Then find
   1's compliment of checksum and add one to it.
   To verify add all the bytes and the checksum and then % 256 should
   yield 0.
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
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_GCRYPT_H
#include <gcrypt.h>
#endif /* HAVE_GCRYPT_H */

#include "freeipmi/util/ipmi-util.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define IPMI_SEQUENCE_NUMBER_MAX            0xFFFFFFFF
#define IPMI_SEQUENCE_NUMBER_WINDOW_DEFAULT          8
#define IPMI_SEQUENCE_NUMBER_WINDOW_MAX             32
#define IPMI_SEQUENCE_NUMBER_WINDOW_MIN              1

static uint8_t
_checksum (const void *buf, unsigned int buflen, uint8_t checksum_initial)
{
  register unsigned int i = 0;
  register int8_t checksum = checksum_initial;

  if (buf == NULL || buflen == 0)
    return (checksum);

  for (; i < buflen; i++)
    checksum = (checksum + ((uint8_t *)buf)[i]) % 256;

  return (checksum);
}

uint8_t
ipmi_checksum (const void *buf, unsigned int buflen)
{
  uint8_t checksum;
  checksum = _checksum (buf, buflen, 0);
  return (-checksum);
}

uint8_t
ipmi_checksum_incremental (const void *buf, unsigned int buflen, uint8_t checksum_initial)
{
  return (_checksum (buf, buflen, checksum_initial));
}

uint8_t
ipmi_checksum_final (const void *buf, unsigned int buflen, uint8_t checksum_initial)
{
  uint8_t checksum;

  if (!buf || !buflen)
    return (-checksum_initial);

  checksum = _checksum (buf, buflen, checksum_initial);
  return (-checksum);
}

int
ipmi_check_cmd (fiid_obj_t obj_cmd, uint8_t cmd)
{
  uint8_t cmd_recv;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_FIELD_LOOKUP (obj_cmd, "cmd") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd, "cmd", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  cmd_recv = val;

  return ((cmd_recv == cmd) ? 1 : 0);
}

int
ipmi_check_completion_code (fiid_obj_t obj_cmd, uint8_t completion_code)
{
  uint8_t completion_code_recv;
  uint64_t val;

  if (!fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_FIELD_LOOKUP (obj_cmd, "comp_code") < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (FIID_OBJ_GET (obj_cmd, "comp_code", &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  completion_code_recv = val;

  return ((completion_code_recv == completion_code) ? 1 : 0);
}

int
ipmi_check_completion_code_success (fiid_obj_t obj_cmd)
{
  return (ipmi_check_completion_code (obj_cmd, IPMI_COMP_CODE_COMMAND_SUCCESS));
}

int
ipmi_get_random (void *buf, unsigned int buflen)
{
#if (HAVE_DEVURANDOM || HAVE_DEVRANDOM)
  int fd, rv;
#endif /* !(HAVE_DEVURANDOM || HAVE_DEVRANDOM) */

  if (!buf)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (!buflen)
    return (0);

#if (HAVE_DEVURANDOM || HAVE_DEVRANDOM)
#if HAVE_DEVURANDOM
  if ((fd = open ("/dev/urandom", O_RDONLY)) < 0)
    goto gcrypt_rand;
#else  /* !HAVE_DEVURANDOM */
  if ((fd = open ("/dev/random", O_RDONLY)) < 0)
    goto gcrypt_rand;
#endif /* !HAVE_DEVURANDOM */

  if ((rv = read (fd, buf, buflen)) < buflen)
    {
      close (fd);
      goto gcrypt_rand;
    }

  /* ignore potential error, cleanup path */
  close (fd);
  return (rv);
#endif /* !(HAVE_DEVURANDOM || HAVE_DEVRANDOM) */

 gcrypt_rand:
/* achu: nothing to do with encryption, but the gcrypt lib isn't loaded 
 * hopefully the user has /dev/random or /dev/urandom.
 */
#ifdef WITH_ENCRYPTION
  gcry_randomize ((unsigned char *)buf, buflen, GCRY_STRONG_RANDOM);
  return (buflen);
#else /* !WITH_ENCRYPTION */
  SET_ERRNO (EPERM);
  return (-1);
#endif /* !WITH_ENCRYPTION */
}

const char *
ipmi_cmd_str (uint8_t net_fn, uint8_t cmd)
{
  switch (net_fn)
    {
    case IPMI_NET_FN_CHASSIS_RQ:
    case IPMI_NET_FN_CHASSIS_RS:
      switch (cmd)
        {
        case IPMI_CMD_GET_CHASSIS_CAPABILITIES:
          return "Get Chassis Capabilities";
        case IPMI_CMD_GET_CHASSIS_STATUS:
          return "Get Chassis Status";
        case IPMI_CMD_CHASSIS_CONTROL:
          return "Chassis Control";
        case IPMI_CMD_CHASSIS_RESET:
          return "Chassis Reset";
        case IPMI_CMD_CHASSIS_IDENTIFY:
          return "Chassis Identify";
        case IPMI_CMD_SET_CHASSIS_CAPABILITIES:
          return "Set Chassis Capabilities";
        case IPMI_CMD_SET_POWER_RESTORE_POLICY:
          return "Set Power Restore Policy";
        case IPMI_CMD_GET_SYSTEM_RESTART_CAUSE:
          return "Get System Restart Cause";
        case IPMI_CMD_SET_SYSTEM_BOOT_OPTIONS:
          return "Set System Boot Options";
        case IPMI_CMD_GET_SYSTEM_BOOT_OPTIONS:
          return "Get System Boot Options";
        case IPMI_CMD_SET_FRONT_PANEL_BUTTON_ENABLES:
          return "Set Front Panel Button Enables";
        case IPMI_CMD_SET_POWER_CYCLE_INTERVAL:
          return "Set Power Cycle Interval";
        case IPMI_CMD_GET_POWER_ON_HOURS_COUNTER:
          return "Get Power On Hours Counter";
        default:
          return "Unknown";
        }
      break;
    case IPMI_NET_FN_BRIDGE_RQ:
    case IPMI_NET_FN_BRIDGE_RS:
      switch (cmd)
        {
        case IPMI_CMD_GET_BRIDGE_STATE:
          return "Get Bridge State";
        case IPMI_CMD_SET_BRIDGE_STATE:
          return "Set Bridge State";
        case IPMI_CMD_GET_ICMB_ADDRESS:
          return "Get ICM Address";
        case IPMI_CMD_SET_ICMB_ADDRESS:
          return "Set ICMB Address";
        case IPMI_CMD_SET_BRIDGE_PROXY_ADDRESS:
          return "Set Bridge Proxy Address";
        case IPMI_CMD_GET_BRIDGE_STATISTICS:
          return "Get Bridge Statistics";
        case IPMI_CMD_GET_ICMB_CAPABILITIES:
          return "Get ICMB Capabilities";
        case IPMI_CMD_CLEAR_BRIDGE_STATISTICS:
          return "Clear Bridge Statistics";
        case IPMI_CMD_GET_BRIDGE_PROXY_ADDRESS:
          return "Get Bridge Proxy Address";
        case IPMI_CMD_GET_ICMB_CONNECTOR_INFO:
          return "Get ICMB Connector Info";
        case IPMI_CMD_GET_ICMB_CONNECTION_ID:
          return "Get ICMB Connection ID";
        case IPMI_CMD_SEND_ICMB_CONNECTION_ID:
          return "Send ICMB Connection ID";
        case IPMI_CMD_PREPARE_FOR_DISCOVERY:
          return "Prepare For Discovery";
        case IPMI_CMD_GET_ADDRESSES:
          return "Get Addresses";
        case IPMI_CMD_SET_DISCOVERED:
          return "Set Discovered";
        case IPMI_CMD_GET_CHASSIS_DEVICE_ID:
          return "Get Chassis Device ID";
        case IPMI_CMD_SET_CHASSIS_DEVICE_ID:
          return "Set Chassis Device ID";
        case IPMI_CMD_BRIDGE_REQUEST:
          return "Bridge Request";
        case IPMI_CMD_BRIDGE_MESSAGE:
          return "Bridge Message";
        case IPMI_CMD_GET_EVENT_COUNT:
          return "Get Event Count";
        case IPMI_CMD_SET_EVENT_DESTINATION:
          return "Set Event Destination";
        case IPMI_CMD_SET_EVENT_RECEPTION_STATE:
          return "Set Event Reception State";
        case IPMI_CMD_SEND_ICMB_EVENT_MESSAGE:
          return "Send ICMB Event Message";
        case IPMI_CMD_GET_EVENT_DESTINATION:
          return "Get Event Destination";
        case IPMI_CMD_GET_EVENT_RECEPTION_STATE:
          return "Get Event Reception State";
        case IPMI_CMD_ERROR_REPORT:
          return "Error Report";
        default:
          return "Unknown";
        }
      break;
    case IPMI_NET_FN_SENSOR_EVENT_RQ:
    case IPMI_NET_FN_SENSOR_EVENT_RS:
      switch (cmd)
        {
        case IPMI_CMD_SET_EVENT_RECEIVER:
          return "Set Event Receiver";
        case IPMI_CMD_GET_EVENT_RECEIVER:
          return "Get Event Receiver";
        case IPMI_CMD_PLATFORM_EVENT:
          return "Platform Event";
        case IPMI_CMD_GET_PEF_CAPABILITIES:
          return "Get PEF Capabilities";
        case IPMI_CMD_ARM_PEF_POSTPONE_TIMER:
          return "ARM PEF Postpone Timer";
        case IPMI_CMD_SET_PEF_CONFIGURATION_PARAMETERS:
          return "Set PEF Configuration Parameters";
        case IPMI_CMD_GET_PEF_CONFIGURATION_PARAMETERS:
          return "Get PEF Configuration Parameters";
        case IPMI_CMD_SET_LAST_PROCESSED_EVENT_ID:
          return "Set Last Processed Event ID";
        case IPMI_CMD_GET_LAST_PROCESSED_EVENT_ID:
          return "Get Last Processed Event ID";
        case IPMI_CMD_ALERT_IMMEDIATE:
          return "Alert Immediate";
        case IPMI_CMD_PET_ACKNOWLEDGE:
          return "PET Acknowledge";
        case IPMI_CMD_GET_DEVICE_SDR_INFO:
          return "Get Device SDR Info";
        case IPMI_CMD_GET_DEVICE_SDR:
          return "Get Device SDR";
        case IPMI_CMD_RESERVE_DEVICE_SDR_REPOSITORY:
          return "Reserve Device SDR Repository";
        case IPMI_CMD_GET_SENSOR_READING_FACTORS:
          return "Get Sensor Reading Factors";
        case IPMI_CMD_SET_SENSOR_HYSTERESIS:
          return "Set Sensor Hysteresis";
        case IPMI_CMD_GET_SENSOR_HYSTERESIS:
          return "Get Sensor Hysteresis";
        case IPMI_CMD_SET_SENSOR_THRESHOLDS:
          return "Set Sensor Thresholds";
        case IPMI_CMD_GET_SENSOR_THRESHOLDS:
          return "Get Sensor Thresholds";
        case IPMI_CMD_SET_SENSOR_EVENT_ENABLE:
          return "Set Sensor Event Enable";
        case IPMI_CMD_GET_SENSOR_EVENT_ENABLE:
          return "Get Sensor Event Enable";
        case IPMI_CMD_RE_ARM_SENSOR_EVENTS:
          return "Re-arm Sensor Events";
        case IPMI_CMD_GET_SENSOR_EVENT_STATUS:
          return "Get Sensor Event Status";
        case IPMI_CMD_GET_SENSOR_READING:
          return "Get Sensor Reading";
        case IPMI_CMD_SET_SENSOR_TYPE:
          return "Set Sensor Type";
        case IPMI_CMD_GET_SENSOR_TYPE:
          return "Get Sensor Type";
        case IPMI_CMD_SET_SENSOR_READING_AND_EVENT_STATUS:
          return "Set Sensor Reading and Event Status";
        default:
          return "Unknown";
        }
      break;
    case IPMI_NET_FN_APP_RQ:
    case IPMI_NET_FN_APP_RS:
      switch (cmd)
        {
        case IPMI_CMD_GET_DEVICE_ID:
          return "Get Device ID";
        case IPMI_CMD_COLD_RESET:
          return "Cold Reset";
        case IPMI_CMD_WARM_RESET:
          return "Warm Reset";
        case IPMI_CMD_GET_SELF_TEST_RESULTS:
          return "Get Self Test Results";
        case IPMI_CMD_MANUFACTURING_TEST_ON:
          return "Manufacturing Test On";
        case IPMI_CMD_SET_ACPI_POWER_STATE:
          return "Set ACPI Power State";
        case IPMI_CMD_GET_ACPI_POWER_STATE:
          return "Get ACPI Power State";
        case IPMI_CMD_GET_DEVICE_GUID:
          return "Get Device GUID";
        case IPMI_CMD_GET_NETFN_SUPPORT:
          return "Get NetFN Support";
        case IPMI_CMD_GET_COMMAND_SUPPORT:
          return "Get Command Support";
        case IPMI_CMD_GET_COMMAND_SUB_FUNCTION_SUPPORT:
          return "Get Command Sub-Function Support";
        case IPMI_CMD_GET_CONFIGURABLE_COMMANDS:
          return "Get Configurable Commands";
        case IPMI_CMD_GET_CONFIGURABLE_COMMAND_SUB_FUNCTIONS:
          return "Get Configurable Command Sub-Functions";
        case IPMI_CMD_SET_COMMAND_ENABLES:
          return "Set Command Enables";
        case IPMI_CMD_GET_COMMAND_ENABLES:
          return "Get Command Enables";
        case IPMI_CMD_SET_COMMAND_SUB_FUNCTION_ENABLES:
          return "Set Command Sub-Function Enables";
        case IPMI_CMD_GET_COMMAND_SUB_FUNCTION_ENABLES:
          return "Get Command Sub-Function Enables";
        case IPMI_CMD_GET_OEM_NETFN_IANA_SUPPORT:
          return "Get OEM NetFN IANA Support";
        case IPMI_CMD_RESET_WATCHDOG_TIMER:
          return "Reset Watchdog Timer";
        case IPMI_CMD_SET_WATCHDOG_TIMER:
          return "Set Watchdog Timer";
        case IPMI_CMD_GET_WATCHDOG_TIMER:
          return "Get Watchdog Timer";
        case IPMI_CMD_SET_BMC_GLOBAL_ENABLES:
          return "Set BMC Global Enables";
        case IPMI_CMD_GET_BMC_GLOBAL_ENABLES:
          return "Get BMC Global Enables";
        case IPMI_CMD_CLEAR_MESSAGE_FLAGS:
          return "Clear Message Flags";
        case IPMI_CMD_GET_MESSAGE_FLAGS:
          return "Get Message Flags";
        case IPMI_CMD_ENABLE_MESSAGE_CHANNEL_RECEIVE:
          return "Enable Message Channel Receive";
        case IPMI_CMD_GET_MESSAGE:
          return "Get Message";
        case IPMI_CMD_SEND_MESSAGE:
          return "Send Message";
        case IPMI_CMD_READ_EVENT_MESSAGE_BUFFER:
          return "Read Event Message Buffer";
        case IPMI_CMD_GET_BT_INTERFACE_CAPABILITIES:
          return "Get BT Interface Capabilities";
        case IPMI_CMD_GET_SYSTEM_GUID:
          return "Get System GUID";
        case IPMI_CMD_SET_SYSTEM_INFO_PARAMETERS:
          return "Set System Info Parameters";
        case IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS:
          return "Get System Info Parameters";
        case IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES:
          return "Get Channel Authentication Capabilities";
        case IPMI_CMD_GET_SESSION_CHALLENGE:
          return "Get Session Challenge";
        case IPMI_CMD_ACTIVATE_SESSION:
          return "Activiate Session";
        case IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL:
          return "Set Session Privilege Level";
        case IPMI_CMD_CLOSE_SESSION:
          return "Close Session";
        case IPMI_CMD_GET_SESSION_INFO:
          return "Get Session Info";
        case IPMI_CMD_GET_AUTHCODE:
          return "Get Authcode";
        case IPMI_CMD_SET_CHANNEL_ACCESS:
          return "Set Channel Access";
        case IPMI_CMD_GET_CHANNEL_ACCESS:
          return "Get Channel Access";
        case IPMI_CMD_GET_CHANNEL_INFO_COMMAND:
          return "Get Channel Info Command";
        case IPMI_CMD_SET_USER_ACCESS_COMMAND:
          return "Set User Access Command";
        case IPMI_CMD_GET_USER_ACCESS_COMMAND:
          return "Get User Access Command";
        case IPMI_CMD_SET_USER_NAME:
          return "Set User Name Command"; /* 'Set User Name' in table, added 'Command' for consistency */
        case IPMI_CMD_GET_USER_NAME_COMMAND:
          return "Get User Name Command";
        case IPMI_CMD_SET_USER_PASSWORD_COMMAND:
          return "Set User Password Command";
        case IPMI_CMD_ACTIVATE_PAYLOAD:
          return "Activate Payload";
        case IPMI_CMD_DEACTIVATE_PAYLOAD:
          return "Deactivate Payload";
        case IPMI_CMD_GET_PAYLOAD_ACTIVATION_STATUS:
          return "Get Payload Activation Status";
        case IPMI_CMD_GET_PAYLOAD_INSTANCE_INFO:
          return "Get Payload Instance Info";
        case IPMI_CMD_SET_USER_PAYLOAD_ACCESS:
          return "Set User Payload Access";
        case IPMI_CMD_GET_USER_PAYLOAD_ACCESS:
          return "Get User Payload Access";
        case IPMI_CMD_GET_CHANNEL_PAYLOAD_SUPPORT:
          return "Get Channel Payload Support";
        case IPMI_CMD_GET_CHANNEL_PAYLOAD_VERSION:
          return "Get Channel Payload Version";
        case IPMI_CMD_GET_CHANNEL_OEM_PAYLOAD_INFO:
          return "Get Channel OEM Payload Info";
        case IPMI_CMD_MASTER_WRITE_READ:
          return "Master Write Read";
        case IPMI_CMD_GET_CHANNEL_CIPHER_SUITES:
          return "Get Channel Cipher Suites";
        case IPMI_CMD_SUSPEND_RESUME_PAYLOAD_ENCRYPTION:
          return "Suspend Resume Payload Encryption";
        case IPMI_CMD_SET_CHANNEL_SECURITY_KEYS:
          return "Set Channel Security Keys";
        case IPMI_CMD_GET_SYSTEM_INTERFACE_CAPABILITIES:
          return "Get System Interface Capabilities";
        default:
          return "Unknown";
        }
      break;
    case IPMI_NET_FN_FIRMWARE_RQ:
    case IPMI_NET_FN_FIRMWARE_RS:
      return "Unknown";
      break;
    case IPMI_NET_FN_STORAGE_RQ:
    case IPMI_NET_FN_STORAGE_RS:
      switch (cmd)
        {
        case IPMI_CMD_GET_FRU_INVENTORY_AREA_INFO:
          return "Get FRU Inventory Area Info";
        case IPMI_CMD_READ_FRU_DATA:
          return "Read FRU Data";
        case IPMI_CMD_WRITE_FRU_DATA:
          return "Write FRU Data";
        case IPMI_CMD_GET_SDR_REPOSITORY_INFO:
          return "Get SDR Repository Info";
        case IPMI_CMD_GET_SDR_REPOSITORY_ALLOCATION_INFO:
          return "Get SDR Repository Allocation Info";
        case IPMI_CMD_RESERVE_SDR_REPOSITORY:
          return "Reserve SDR Repository";
        case IPMI_CMD_GET_SDR:
          return "Get SDR";
        case IPMI_CMD_ADD_SDR:
          return "Add SDR";
        case IPMI_CMD_PARTIAL_ADD_SDR:
          return "Partial Add SDR";
        case IPMI_CMD_DELETE_SDR:
          return "Delete SDR";
        case IPMI_CMD_CLEAR_SDR_REPOSITORY:
          return "Clear SDR Repository";
        case IPMI_CMD_GET_SDR_REPOSITORY_TIME:
          return "Get SDR Repository Time";
        case IPMI_CMD_SET_SDR_REPOSITORY_TIME:
          return "Set SDR Repository Time";
        case IPMI_CMD_ENTER_SDR_REPOSITORY_UPDATE_MODE:
          return "Enter SDR Repository Update Mode";
        case IPMI_CMD_EXIT_SDR_REPOSITORY_UPDATE_MODE:
          return "Exit SDR Repository Update Mode";
        case IPMI_CMD_RUN_INITIALIZATION_AGENT:
          return "Run Initialization Agent";
        case IPMI_CMD_GET_SEL_INFO:
          return "Get SEL Info";
        case IPMI_CMD_GET_SEL_ALLOCATION_INFO:
          return "Get SEL Allocation Info";
        case IPMI_CMD_RESERVE_SEL:
          return "Reserve SEL";
        case IPMI_CMD_GET_SEL_ENTRY:
          return "Get SEL Entry";
        case IPMI_CMD_ADD_SEL_ENTRY:
          return "Add SEL Entry";
        case IPMI_CMD_PARTIAL_ADD_SEL_ENTRY:
          return "Partial Add SEL Entry";
        case IPMI_CMD_DELETE_SEL_ENTRY:
          return "Delete SEL Entry";
        case IPMI_CMD_CLEAR_SEL:
          return "Clear SEL";
        case IPMI_CMD_GET_SEL_TIME:
          return "Get SEL Time";
        case IPMI_CMD_SET_SEL_TIME:
          return "Set SEL Time";
        case IPMI_CMD_GET_AUXILIARY_LOG_STATUS:
          return "Get Auxiliary Log Status";
        case IPMI_CMD_SET_AUXILIARY_LOG_STATUS:
          return "Set Auxiliary Log Status";
        case IPMI_CMD_GET_SEL_TIME_UTC_OFFSET:
          return "Get SEL Time UTC Offset";
        case IPMI_CMD_SET_SEL_TIME_UTC_OFFSET:
          return "Set SEL Time UTC Offset";
        default:
          return "Unknown";
        }
      break;
    case IPMI_NET_FN_TRANSPORT_RQ:
    case IPMI_NET_FN_TRANSPORT_RS:
      switch (cmd)
        {
        case IPMI_CMD_SET_LAN_CONFIGURATION_PARAMETERS:
          return "Set LAN Configuration Parameters";
        case IPMI_CMD_GET_LAN_CONFIGURATION_PARAMETERS:
          return "Get LAN Configuration Parameters";
        case IPMI_CMD_SUSPEND_BMC_ARPS:
          return "Suspend BMC ARPs";
        case IPMI_CMD_GET_IP_UDP_RMCP_STATISTICS:
          return "Get IP UDP RMCP Statistics";
        case IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION:
          return "Set Serial Modem Configuration";
        case IPMI_CMD_GET_SERIAL_MODEM_CONFIGURATION:
          return "Get Serial Modem Configuration";
        case IPMI_CMD_SET_SERIAL_MODEM_MUX:
          return "Set Serial Modem Mux";
        case IPMI_CMD_GET_TAP_RESPONSE_CODES:
          return "Get TAP Response Codes";
        case IPMI_CMD_SET_PPP_UDP_PROXY_TRANSMIT_DATA:
          return "Set PPP UDP Proxy Transmit Data";
        case IPMI_CMD_GET_PPP_UDP_PROXY_TRANSMIT_DATA:
          return "Get PPP UDP Proxy Transmit Data";
        case IPMI_CMD_SEND_PPP_UDP_PROXY_PACKET:
          return "Send PPP UDP Proxy Packet";
        case IPMI_CMD_GET_PPP_UDP_PROXY_RECEIVE_DATA:
          return "Get PPP UDP Proxy Receive Data";
        case IPMI_CMD_SERIAL_MODEM_CONNECTION_ACTIVE:
          return "Serial Modem Connection Active";
        case IPMI_CMD_CALLBACK:
          return "Callback";
        case IPMI_CMD_SET_USER_CALLBACK_OPTIONS:
          return "Set User Callback Options";
        case IPMI_CMD_GET_USER_CALLBACK_OPTIONS:
          return "Get User Callback Options";
        case IPMI_CMD_SET_SERIAL_ROUTING_MUX:
          return "Set Serial Routing Mux";
        case IPMI_CMD_SOL_ACTIVATING:
          return "SOL Activating";
        case IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS:
          return "Set SOL Configuration Parameters";
        case IPMI_CMD_GET_SOL_CONFIGURATION_PARAMETERS:
          return "Get SOL Configuration Parameters";
        case IPMI_CMD_FORWARDED_COMMAND:
          return "Forwarded Command";
        case IPMI_CMD_SET_FORWARDED_COMMANDS:
          return "Set Forwarded Commands";
        case IPMI_CMD_GET_FORWARDED_COMMANDS:
          return "Get Forwarded Commands";
        case IPMI_CMD_ENABLE_FORWARDED_COMMANDS:
          return "Enable Forwarded Commands";
        default:
          return "Unknown";
        }
      break;
    default:
      return "Unknown";
    }
}

