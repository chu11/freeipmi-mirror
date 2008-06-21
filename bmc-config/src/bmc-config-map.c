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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <freeipmi/freeipmi.h>

#include "bmc-config.h"
#include "bmc-config-map.h"

#include "freeipmi-portability.h"

int
channel_access_mode (const char *string)
{
  if (same (string, "disabled"))
    return IPMI_MESSAGING_ACCESS_MODE_DISABLED;
  if (same (string, "pre_boot_only"))
    return IPMI_MESSAGING_ACCESS_MODE_PRE_BOOT_ONLY;
  if (same (string, "always_available"))
    return IPMI_MESSAGING_ACCESS_MODE_ALWAYS_AVAILABLE;
  if (same (string, "shared"))
    return IPMI_MESSAGING_ACCESS_MODE_SHARED;
  return -1;
}

char *
channel_access_mode_string (uint8_t mode)
{
  switch (mode)
    {
    case IPMI_MESSAGING_ACCESS_MODE_DISABLED:
      return "Disabled";
    case IPMI_MESSAGING_ACCESS_MODE_PRE_BOOT_ONLY:
      return "Pre_Boot_Only";
    case IPMI_MESSAGING_ACCESS_MODE_ALWAYS_AVAILABLE:
      return "Always_Available";
    case IPMI_MESSAGING_ACCESS_MODE_SHARED:
      return "Shared";
    }
  return "";
}

uint8_t
get_privilege_limit_number (const char *value)
{
  if (same (value, "callback"))
    return IPMI_PRIVILEGE_LEVEL_CALLBACK;
  if (same (value, "user"))
    return IPMI_PRIVILEGE_LEVEL_USER;
  if (same (value, "operator"))
    return IPMI_PRIVILEGE_LEVEL_OPERATOR;
  if (same (value, "administrator"))
    return IPMI_PRIVILEGE_LEVEL_ADMIN;
  if (same (value, "oem_proprietary"))
    return IPMI_PRIVILEGE_LEVEL_OEM;
  if (same (value, "no_access"))
    return IPMI_PRIVILEGE_LEVEL_NO_ACCESS;
  return 0;
}

char *
get_privilege_limit_string (uint8_t limit)
{
  switch (limit)
    {
    case IPMI_PRIVILEGE_LEVEL_CALLBACK:
      return "Callback";
    case IPMI_PRIVILEGE_LEVEL_USER:
      return "User";
    case IPMI_PRIVILEGE_LEVEL_OPERATOR:
      return "Operator";
    case IPMI_PRIVILEGE_LEVEL_ADMIN:
      return "Administrator";
    case IPMI_PRIVILEGE_LEVEL_OEM:
      return "OEM_Proprietary";
    case IPMI_PRIVILEGE_LEVEL_NO_ACCESS:
      return "No_Access";
    }
  return "";
}

int
privilege_level_number (const char *string)
{
  if (same (string, "callback"))
    return IPMI_PRIVILEGE_LEVEL_CALLBACK;
  if (same (string, "user"))
    return IPMI_PRIVILEGE_LEVEL_USER;
  if (same (string, "operator"))
    return IPMI_PRIVILEGE_LEVEL_OPERATOR;
  if (same (string, "administrator"))
    return IPMI_PRIVILEGE_LEVEL_ADMIN;
  if (same (string, "oem_proprietary"))
    return IPMI_PRIVILEGE_LEVEL_OEM;
  return -1;
}

char *
privilege_level_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_PRIVILEGE_LEVEL_CALLBACK:
      return "Callback";
    case IPMI_PRIVILEGE_LEVEL_USER:
      return "User";
    case IPMI_PRIVILEGE_LEVEL_OPERATOR:
      return "Operator";
    case IPMI_PRIVILEGE_LEVEL_ADMIN:
      return "Administrator";
    case IPMI_PRIVILEGE_LEVEL_OEM:
      return "OEM_Proprietary";
    }
  return "";
}

int
rmcpplus_priv_number (const char *value)
{
  if (same (value, "unused"))
    return 0;
  if (same (value, "callback"))
    return IPMI_PRIVILEGE_LEVEL_CALLBACK;
  if (same (value, "user"))
    return IPMI_PRIVILEGE_LEVEL_USER;
  if (same (value, "operator"))
    return IPMI_PRIVILEGE_LEVEL_OPERATOR;
  if (same (value, "administrator"))
    return IPMI_PRIVILEGE_LEVEL_ADMIN;
  if (same (value, "oem_proprietary"))
    return IPMI_PRIVILEGE_LEVEL_OEM;
  return -1;
}

char *
rmcpplus_priv_string (int value)
{
  switch (value)
    {
    case 0:
      return "Unused";
    case IPMI_PRIVILEGE_LEVEL_CALLBACK:
      return "Callback";
    case IPMI_PRIVILEGE_LEVEL_USER:
      return "User";
    case IPMI_PRIVILEGE_LEVEL_OPERATOR:
      return "Operator";
    case IPMI_PRIVILEGE_LEVEL_ADMIN:
      return "Administrator";
    case IPMI_PRIVILEGE_LEVEL_OEM:
      return "OEM_Proprietary";
    }
  return "";
}

int
ip_address_source_number (const char *source)
{
  if (same (source, "unspecified"))
    return IPMI_IP_ADDRESS_SOURCE_UNSPECIFIED;
  if (same (source, "static"))
    return IPMI_IP_ADDRESS_SOURCE_STATIC;
  if (same (source, "use_dhcp"))
    return IPMI_IP_ADDRESS_SOURCE_DHCP;
  if (same (source, "use_bios"))
    return IPMI_IP_ADDRESS_SOURCE_BIOS;
  if (same (source, "use_others"))
    return IPMI_IP_ADDRESS_SOURCE_OTHER;
  return -1;
}

char *
ip_address_source_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_IP_ADDRESS_SOURCE_UNSPECIFIED:
      return "Unspecified";
    case IPMI_IP_ADDRESS_SOURCE_STATIC:
      return "Static";
    case IPMI_IP_ADDRESS_SOURCE_DHCP:
      return "Use_DHCP";
    case IPMI_IP_ADDRESS_SOURCE_BIOS:
      return "Use_BIOS";
    case IPMI_IP_ADDRESS_SOURCE_OTHER:
      return "Use_Others";
    }
  return "";
}

int
power_restore_policy_number (const char *string)
{
  if (same (string, "off_state_ac_apply"))
    return IPMI_POWER_RESTORE_POLICY_POWERED_OFF_AFTER_AC_RETURNS;
  if (same (string, "restore_state_ac_apply"))
    return IPMI_POWER_RESTORE_POLICY_POWER_RESTORED_TO_STATE;
  if (same (string, "on_state_ac_apply"))
    return IPMI_POWER_RESTORE_POLICY_POWERS_UP_AFTER_AC_RETURNS;
  return -1;
}

char *
power_restore_policy_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_POWER_RESTORE_POLICY_POWERED_OFF_AFTER_AC_RETURNS:
      return "Off_State_AC_Apply";
    case IPMI_POWER_RESTORE_POLICY_POWER_RESTORED_TO_STATE:
      return "Restore_State_AC_Apply";
    case IPMI_POWER_RESTORE_POLICY_POWERS_UP_AFTER_AC_RETURNS:
      return "On_State_AC_Apply";
    }
  return "";
}

int
connect_mode_number (const  char *string)
{
  if (same (string, "Modem_Connect"))
    return IPMI_CONNECT_MODE_MODEM;
  if (same (string, "Direct_Connect"))
    return IPMI_CONNECT_MODE_DIRECT;

  return -1;
}

char *
connect_mode_string (uint8_t mode)
{
  switch (mode)
    {
    case IPMI_CONNECT_MODE_MODEM:
      return "Modem_Connect";
    case IPMI_CONNECT_MODE_DIRECT:
      return "Direct_Connect";
    }
  return "";
}

int
flow_control_number (const char *string)
{
  if (same (string, "no_flow_control"))
    return IPMI_FLOW_CONTROL_NO_FLOW_CONTROL;
  if (same (string, "rts_cts"))
    return IPMI_FLOW_CONTROL_RTS_CTS_FLOW_CONTROL;
  if (same (string, "xon_xoff"))
    return IPMI_FLOW_CONTROL_XON_XOFF_FLOW_CONTROL;
  return -1;
}

char *
flow_control_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_FLOW_CONTROL_NO_FLOW_CONTROL:
      return "No_Flow_Control";
    case IPMI_FLOW_CONTROL_RTS_CTS_FLOW_CONTROL:
      return "RTS_CTS";
    case IPMI_FLOW_CONTROL_XON_XOFF_FLOW_CONTROL:
      return "XON_XOFF";
    }
  return "";
}

int
bit_rate_number (const char *string)
{
  if (same (string, "9600"))
    return IPMI_BIT_RATE_9600_BPS;
  if (same (string, "19200"))
    return IPMI_BIT_RATE_19200_BPS;
  if (same (string, "38400"))
    return IPMI_BIT_RATE_38400_BPS;
  if (same (string, "57600"))
    return IPMI_BIT_RATE_57600_BPS;
  if (same (string, "115200"))
    return IPMI_BIT_RATE_115200_BPS;
  return -1;
}

char *
bit_rate_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_BIT_RATE_9600_BPS:
      return "9600";
    case IPMI_BIT_RATE_19200_BPS:
      return "19200";
    case IPMI_BIT_RATE_38400_BPS:
      return "38400";
    case IPMI_BIT_RATE_57600_BPS:
      return "57600";
    case IPMI_BIT_RATE_115200_BPS:
      return "115200";
    }
  return "";
}

int
sol_bit_rate_number (const char *string)
{
  if (same (string, "serial"))
    return IPMI_SOL_BIT_RATE_SERIAL_BIT_RATE;
  if (same (string, "9600"))
    return IPMI_SOL_BIT_RATE_96_KBPS;
  if (same (string, "19200"))
    return IPMI_SOL_BIT_RATE_192_KBPS;
  if (same (string, "38400"))
    return IPMI_SOL_BIT_RATE_384_KBPS;
  if (same (string, "57600"))
    return IPMI_SOL_BIT_RATE_576_KBPS;
  if (same (string, "115200"))
    return IPMI_SOL_BIT_RATE_1152_KBPS;
  return -1;
}

char *
sol_bit_rate_string (uint8_t value)
{
  switch (value) 
    {
    case IPMI_SOL_BIT_RATE_SERIAL_BIT_RATE:
      return "Serial";
    case IPMI_SOL_BIT_RATE_96_KBPS:
      return "9600";
    case IPMI_SOL_BIT_RATE_192_KBPS:
      return "19200";
    case IPMI_SOL_BIT_RATE_384_KBPS:
      return "38400";
    case IPMI_SOL_BIT_RATE_576_KBPS:
      return "57600";
    case IPMI_SOL_BIT_RATE_1152_KBPS:
      return "115200";
    }
  return "";
}

int
alert_destination_type_number (const char *source)
{
  if (same (source, "pet_trap"))
    return IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION;
  if (same (source, "oem1"))
    return IPMI_DESTINATION_TYPE_OEM1;
  if (same (source, "oem2"))
    return IPMI_DESTINATION_TYPE_OEM2;
  return -1;
}

char *
alert_destination_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION:
      return "PET_Trap";
    case IPMI_DESTINATION_TYPE_OEM1:
      return "OEM1";
    case IPMI_DESTINATION_TYPE_OEM2:
      return "OEM2";
    }
  return "";
}

int
alert_gateway_number (const char *source)
{
  if (same (source, "default"))
    return IPMI_GATEWAY_SELECTOR_DEFAULT;
  if (same (source, "backup"))
    return IPMI_GATEWAY_SELECTOR_BACKUP;
  return -1;
}

char *
alert_gateway_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_GATEWAY_SELECTOR_DEFAULT:
      return "Default";
    case IPMI_GATEWAY_SELECTOR_BACKUP:
      return "Backup";
    }
  return "";
}
