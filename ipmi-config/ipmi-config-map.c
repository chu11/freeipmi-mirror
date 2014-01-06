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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-map.h"

#include "freeipmi-portability.h"

int
channel_access_mode (const char *string)
{
  assert (string);

  if (same (string, "disabled"))
    return (IPMI_MESSAGING_ACCESS_MODE_DISABLED);
  if (same (string, "pre_boot_only"))
    return (IPMI_MESSAGING_ACCESS_MODE_PRE_BOOT_ONLY);
  if (same (string, "always_available"))
    return (IPMI_MESSAGING_ACCESS_MODE_ALWAYS_AVAILABLE);
  if (same (string, "shared"))
    return (IPMI_MESSAGING_ACCESS_MODE_SHARED);
  return (-1);
}

char *
channel_access_mode_string (uint8_t value)
{
  switch (value)
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
get_privilege_limit_number (const char *string)
{
  assert (string);

  if (same (string, "callback"))
    return (IPMI_PRIVILEGE_LEVEL_CALLBACK);
  if (same (string, "user"))
    return (IPMI_PRIVILEGE_LEVEL_USER);
  if (same (string, "operator"))
    return (IPMI_PRIVILEGE_LEVEL_OPERATOR);
  if (same (string, "administrator"))
    return (IPMI_PRIVILEGE_LEVEL_ADMIN);
  if (same (string, "oem_proprietary"))
    return (IPMI_PRIVILEGE_LEVEL_OEM);
  if (same (string, "no_access"))
    return (IPMI_PRIVILEGE_LEVEL_NO_ACCESS);
  return (0);
}

char *
get_privilege_limit_string (uint8_t value)
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
    case IPMI_PRIVILEGE_LEVEL_NO_ACCESS:
      return "No_Access";
    }
  return "";
}

int
privilege_level_number (const char *string)
{
  assert (string);

  if (same (string, "callback"))
    return (IPMI_PRIVILEGE_LEVEL_CALLBACK);
  if (same (string, "user"))
    return (IPMI_PRIVILEGE_LEVEL_USER);
  if (same (string, "operator"))
    return (IPMI_PRIVILEGE_LEVEL_OPERATOR);
  if (same (string, "administrator"))
    return (IPMI_PRIVILEGE_LEVEL_ADMIN);
  if (same (string, "oem_proprietary"))
    return (IPMI_PRIVILEGE_LEVEL_OEM);
  return (-1);
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
rmcpplus_priv_number (const char *string)
{
  assert (string);

  if (same (string, "unused"))
    return (IPMI_PRIVILEGE_LEVEL_UNSPECIFIED);
  if (same (string, "callback"))
    return (IPMI_PRIVILEGE_LEVEL_CALLBACK);
  if (same (string, "user"))
    return (IPMI_PRIVILEGE_LEVEL_USER);
  if (same (string, "operator"))
    return (IPMI_PRIVILEGE_LEVEL_OPERATOR);
  if (same (string, "administrator"))
    return (IPMI_PRIVILEGE_LEVEL_ADMIN);
  if (same (string, "oem_proprietary"))
    return (IPMI_PRIVILEGE_LEVEL_OEM);
  return (-1);
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
ip_address_source_number (const char *string)
{
  assert (string);

  if (same (string, "unspecified"))
    return (IPMI_IP_ADDRESS_SOURCE_UNSPECIFIED);
  if (same (string, "static"))
    return (IPMI_IP_ADDRESS_SOURCE_STATIC);
  if (same (string, "use_dhcp"))
    return (IPMI_IP_ADDRESS_SOURCE_DHCP);
  if (same (string, "use_bios"))
    return (IPMI_IP_ADDRESS_SOURCE_BIOS);
  if (same (string, "use_others"))
    return (IPMI_IP_ADDRESS_SOURCE_OTHER);
  return (-1);
}

char *
ip_address_source_string (uint8_t value)
{
  switch (value)
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
  assert (string);

  if (same (string, "off_state_ac_apply"))
    return (IPMI_POWER_RESTORE_POLICY_POWERED_OFF_AFTER_AC_RETURNS);
  if (same (string, "restore_state_ac_apply"))
    return (IPMI_POWER_RESTORE_POLICY_POWER_RESTORED_TO_STATE);
  if (same (string, "on_state_ac_apply"))
    return (IPMI_POWER_RESTORE_POLICY_POWERS_UP_AFTER_AC_RETURNS);
  return (-1);
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
connect_mode_number (const char *string)
{
  assert (string);

  if (same (string, "Modem_Connect"))
    return (IPMI_CONNECT_MODE_MODEM);
  if (same (string, "Direct_Connect"))
    return (IPMI_CONNECT_MODE_DIRECT);

  return (-1);
}

char *
connect_mode_string (uint8_t value)
{
  switch (value)
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
  assert (string);

  if (same (string, "no_flow_control"))
    return (IPMI_FLOW_CONTROL_NO_FLOW_CONTROL);
  if (same (string, "rts_cts"))
    return (IPMI_FLOW_CONTROL_RTS_CTS_FLOW_CONTROL);
  if (same (string, "xon_xoff"))
    return (IPMI_FLOW_CONTROL_XON_XOFF_FLOW_CONTROL);
  return (-1);
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
  assert (string);

  if (same (string, "9600"))
    return (IPMI_BIT_RATE_9600_BPS);
  if (same (string, "19200"))
    return (IPMI_BIT_RATE_19200_BPS);
  if (same (string, "38400"))
    return (IPMI_BIT_RATE_38400_BPS);
  if (same (string, "57600"))
    return (IPMI_BIT_RATE_57600_BPS);
  if (same (string, "115200"))
    return (IPMI_BIT_RATE_115200_BPS);
  return (-1);
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
  assert (string);

  if (same (string, "serial"))
    return (IPMI_SOL_BIT_RATE_SERIAL_BIT_RATE);
  if (same (string, "9600"))
    return (IPMI_SOL_BIT_RATE_96_KBPS);
  if (same (string, "19200"))
    return (IPMI_SOL_BIT_RATE_192_KBPS);
  if (same (string, "38400"))
    return (IPMI_SOL_BIT_RATE_384_KBPS);
  if (same (string, "57600"))
    return (IPMI_SOL_BIT_RATE_576_KBPS);
  if (same (string, "115200"))
    return (IPMI_SOL_BIT_RATE_1152_KBPS);
  return (-1);
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
alert_destination_type_number (const char *string)
{
  assert (string);

  if (same (string, "pet_trap"))
    return (IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION);
  if (same (string, "oem1"))
    return (IPMI_DESTINATION_TYPE_OEM1);
  if (same (string, "oem2"))
    return (IPMI_DESTINATION_TYPE_OEM2);
  return (-1);
}

char *
alert_destination_type_string (uint8_t value)
{
  switch (value)
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
alert_gateway_number (const char *string)
{
  assert (string);

  if (same (string, "default"))
    return (IPMI_GATEWAY_SELECTOR_DEFAULT);
  if (same (string, "backup"))
    return (IPMI_GATEWAY_SELECTOR_BACKUP);
  return (-1);
}

char *
alert_gateway_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_GATEWAY_SELECTOR_DEFAULT:
      return "Default";
    case IPMI_GATEWAY_SELECTOR_BACKUP:
      return "Backup";
    }
  return "";
}

int
bios_boot_type_number (const char *string)
{
  assert (string);

  if (same (string, "PC-COMPATIBLE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_TYPE_PC_COMPATIBLE);
  if (same (string, "EFI"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_TYPE_EFI);
  return (-1);
}

char *
bios_boot_type_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_TYPE_PC_COMPATIBLE:
      return "PC-COMPATIBLE";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_TYPE_EFI:
      return "EFI";
    }
  return "";
}

int
boot_device_number (const char *string)
{
  assert (string);

  if (same (string, "NO-OVERRIDE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_NO_OVERRIDE);
  if (same (string, "PXE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_PXE);
  if (same (string, "HARD-DRIVE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE);
  if (same (string, "HARD-DRIVE-SAFE-MODE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE_SAFE_MODE);
  if (same (string, "DIAGNOSTIC_PARTITION"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_DIAGNOSTIC_PARTITION);
  if (same (string, "CD-DVD"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_CD_DVD);
  if (same (string, "BIOS-SETUP"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_BIOS_SETUP);
  if (same (string, "REMOTE-FLOPPY"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_REMOTELY_CONNECTED_FLOPPY_PRIMARY_REMOVEABLE_MEDIA);
  if (same (string, "PRIMARY-REMOTE-MEDIA"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_PRIMARY_REMOTE_MEDIA);
  if (same (string, "REMOTE-CD-DVD"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_REMOTELY_CONNECTED_CD_DVD);
  if (same (string, "REMOTE-HARD-DRIVE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_REMOTELY_CONNECTED_HARD_DRIVE);
  if (same (string, "FLOPPY"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_FLOPPY_REMOVEABLE_MEDIA);
  return (-1);
}

char *
boot_device_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_NO_OVERRIDE:
      return "NO-OVERRIDE";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_PXE:
      return "PXE";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE:
      return "HARD-DRIVE";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE_SAFE_MODE:
      return "HARD-DRIVE-SAFE-MODE";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_DIAGNOSTIC_PARTITION:
      return "DIAGNOSTIC_PARTITION";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_CD_DVD:
      return "CD-DVD";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_BIOS_SETUP:
      return "BIOS-SETUP";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_REMOTELY_CONNECTED_FLOPPY_PRIMARY_REMOVEABLE_MEDIA:
      return "REMOTE-FLOPPY";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_PRIMARY_REMOTE_MEDIA:
      return "PRIMARY-REMOTE-MEDIA";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_REMOTELY_CONNECTED_CD_DVD:
      return "REMOTE-CD-DVD";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_REMOTELY_CONNECTED_HARD_DRIVE:
      return "REMOTE-HARD-DRIVE";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_FLOPPY_REMOVEABLE_MEDIA:
      return "FLOPPY";
    }
  return "";
}

int
device_instance_selector_number (const char *string)
{
  uint8_t device_instance_selector;
  char *str = NULL;

  assert (string);

  if (same (string, "none"))
    return (0);

  if ((str = stristr (string, "external-")))
    {
      device_instance_selector = atoi (str + strlen ("external-"));

      if (IPMI_SYSTEM_BOOT_OPTION_DEVICE_INSTANCE_SELECTOR_RANGE_VALID (device_instance_selector))
        return (device_instance_selector);
    }

  if ((str = stristr (string, "internal-")))
    {
      device_instance_selector = atoi (str + strlen ("internal-"));

      if (IPMI_SYSTEM_BOOT_OPTION_DEVICE_INSTANCE_SELECTOR_RANGE_VALID (device_instance_selector))
        return (device_instance_selector | IPMI_SYSTEM_BOOT_OPTION_DEVICE_INSTANCE_SELECTOR_INTERNAL_BITMASK);
    }

  return (-1);
}

char *
device_instance_selector_string (uint8_t value)
{
  /* achu: this is dumb, but that's the way these map functions work */
  switch (value)
    {
    case 0x00:
      return "None";
    case 0x01:
      return "External-1";
    case 0x02:
      return "External-2";
    case 0x03:
      return "External-3";
    case 0x04:
      return "External-4";
    case 0x05:
      return "External-5";
    case 0x06:
      return "External-6";
    case 0x07:
      return "External-7";
    case 0x08:
      return "External-8";
    case 0x09:
      return "External-9";
    case 0x0A:
      return "External-10";
    case 0x0B:
      return "External-11";
    case 0x0C:
      return "External-12";
    case 0x0D:
      return "External-13";
    case 0x0E:
      return "External-14";
    case 0x0F:
      return "External-15";
    case 0x11:
      return "Internal-1";
    case 0x12:
      return "Internal-2";
    case 0x13:
      return "Internal-3";
    case 0x14:
      return "Internal-4";
    case 0x15:
      return "Internal-5";
    case 0x16:
      return "Internal-6";
    case 0x17:
      return "Internal-7";
    case 0x18:
      return "Internal-8";
    case 0x19:
      return "Internal-9";
    case 0x1A:
      return "Internal-10";
    case 0x1B:
      return "Internal-11";
    case 0x1C:
      return "Internal-12";
    case 0x1D:
      return "Internal-13";
    case 0x1E:
      return "Internal-14";
    case 0x1F:
      return "Internal-15";
    }
  return "";
}

int
firmware_bios_verbosity_number (const char *string)
{
  assert (string);

  if (same (string, "DEFAULT"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_DEFAULT);
  if (same (string, "QUIET"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_QUIET);
  if (same (string, "VERBOSE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VERBOSE);
  return (-1);
}

char *
firmware_bios_verbosity_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_DEFAULT:
      return "DEFAULT";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_QUIET:
      return "QUIET";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VERBOSE:
      return "VERBOSE";
    }
  return "";
}

int
console_redirection_number (const char *string)
{
  assert (string);

  if (same (string, "BIOS-SETTING"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_BIOS_SETTING);
  if (same (string, "SUPPRESS"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_SUPPRESS);
  if (same (string, "ENABLE"))
    return (IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_ENABLE);
  return (-1);
}

char *
console_redirection_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_BIOS_SETTING:
      return "BIOS-SETTING";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_SUPPRESS:
      return "SUPPRESS";
    case IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_ENABLE:
      return "ENABLE";
    }
  return "";
}

int
policy_type_number (const char *string)
{
  assert (string);

  if (same (string, "always_send_to_this_destination"))
    return (IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION);
  if (same (string, "proceed_to_next_entry"))
    return (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY);
  if (same (string, "do_not_proceed_any_more_entries"))
    return (IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES);
  if (same (string, "proceed_to_next_entry_different_channel"))
    return (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL);
  if (same (string, "proceed_to_next_entry_different_destination_type"))
    return (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE);
  return (-1);
}

char *
policy_type_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION:
      return "Always_Send_To_This_Destination";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY:
      return "Proceed_To_Next_Entry";
      break;
    case IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES:
      return "Do_Not_Proceed_Any_More_Entries";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL:
      return "Proceed_To_Next_Entry_Different_Channel";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE:
      return "Proceed_To_Next_Entry_Different_Destination_Type";
      break;
    }

  return "";
}

int
filter_type_number (const char *string)
{
  assert (string);

  if (same (string, "manufacturer_pre_configured"))
    return (IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER);
  if (same (string, "software_configurable"))
    return (IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER);
  if (same (string, "reserved1"))
    return (0x1);
  if (same (string, "reserved3"))
    return (0x3);

  return (-1);
}

char *
filter_type_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER:
      return "Manufacturer_Pre_Configured";
    case IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER:
      return "Software_Configurable";
    case 0x1:
      return "Reserved1";
    case 0x3:
      return "Reserved3";
    }
  return "";
}

int
event_severity_number (const char *string)
{
  assert (string);

  if (same (string, "unspecified"))
    return (IPMI_EVENT_SEVERITY_UNSPECIFIED);
  if (same (string, "monitor"))
    return (IPMI_EVENT_SEVERITY_MONITOR);
  if (same (string, "information"))
    return (IPMI_EVENT_SEVERITY_INFORMATION);
  if (same (string, "ok"))
    return (IPMI_EVENT_SEVERITY_OK);
  if (same (string, "non_critical"))
    return (IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION);
  if (same (string, "critical"))
    return (IPMI_EVENT_SEVERITY_CRITICAL_CONDITION);
  if (same (string, "non_recoverable"))
    return (IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION);

  return (-1);
}

char *
event_severity_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_EVENT_SEVERITY_UNSPECIFIED:
      return "Unspecified";
    case IPMI_EVENT_SEVERITY_MONITOR:
      return "Monitor";
    case IPMI_EVENT_SEVERITY_INFORMATION:
      return "Information";
    case IPMI_EVENT_SEVERITY_OK:
      return "OK";
    case IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION:
      return "Non_Critical";
    case IPMI_EVENT_SEVERITY_CRITICAL_CONDITION:
      return "Critical";
    case IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION:
      return "Non_Recoverable";
    }
  return "";
}

int
sensor_type_number (const char *string)
{
  assert (string);

  if (same (string, "reserved"))
    return (IPMI_EVENT_SENSOR_TYPE_RESERVED);
  if (same (string, "temperature"))
    return (IPMI_EVENT_SENSOR_TYPE_TEMPERATURE);
  if (same (string, "voltage"))
    return (IPMI_EVENT_SENSOR_TYPE_VOLTAGE);
  if (same (string, "current"))
    return (IPMI_EVENT_SENSOR_TYPE_CURRENT);
  if (same (string, "fan"))
    return (IPMI_EVENT_SENSOR_TYPE_FAN);
  if (same (string, "physical_security"))
    return (IPMI_EVENT_SENSOR_TYPE_PHYSICAL_SECURITY);
  if (same (string, "platform_security_violation_attempt"))
    return (IPMI_EVENT_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT);
  if (same (string, "processor"))
    return (IPMI_EVENT_SENSOR_TYPE_PROCESSOR);
  if (same (string, "power_supply"))
    return (IPMI_EVENT_SENSOR_TYPE_POWER_SUPPLY);
  if (same (string, "power_unit"))
    return (IPMI_EVENT_SENSOR_TYPE_POWER_UNIT);
  if (same (string, "cooling_device"))
    return (IPMI_EVENT_SENSOR_TYPE_COOLING_DEVICE);
  if (same (string, "other_units_based_sensor"))
    return (IPMI_EVENT_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR);
  if (same (string, "memory"))
    return (IPMI_EVENT_SENSOR_TYPE_MEMORY);
  if (same (string, "drive_slot"))
    return (IPMI_EVENT_SENSOR_TYPE_DRIVE_SLOT);
  if (same (string, "post_memory_resize"))
    return (IPMI_EVENT_SENSOR_TYPE_POST_MEMORY_RESIZE);
  if (same (string, "system_firmware_progress"))
    return (IPMI_EVENT_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS);
  if (same (string, "event_logging_disabled"))
    return (IPMI_EVENT_SENSOR_TYPE_EVENT_LOGGING_DISABLED);
  if (same (string, "watchdog1"))
    return (IPMI_EVENT_SENSOR_TYPE_WATCHDOG1);
  if (same (string, "system_event"))
    return (IPMI_EVENT_SENSOR_TYPE_SYSTEM_EVENT);
  if (same (string, "critical_interrupt"))
    return (IPMI_EVENT_SENSOR_TYPE_CRITICAL_INTERRUPT);
  if (same (string, "button_switch"))
    return (IPMI_EVENT_SENSOR_TYPE_BUTTON_SWITCH);
  if (same (string, "module_board"))
    return (IPMI_EVENT_SENSOR_TYPE_MODULE_BOARD);
  if (same (string, "microcontroller_coprocessor"))
    return (IPMI_EVENT_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR);
  if (same (string, "add_in_card"))
    return (IPMI_EVENT_SENSOR_TYPE_ADD_IN_CARD);
  if (same (string, "chassis"))
    return (IPMI_EVENT_SENSOR_TYPE_CHASSIS);
  if (same (string, "chip_set"))
    return (IPMI_EVENT_SENSOR_TYPE_CHIP_SET);
  if (same (string, "other_fru"))
    return (IPMI_EVENT_SENSOR_TYPE_OTHER_FRU);
  if (same (string, "cable_interconnect"))
    return (IPMI_EVENT_SENSOR_TYPE_CABLE_INTERCONNECT);
  if (same (string, "terminator"))
    return (IPMI_EVENT_SENSOR_TYPE_TERMINATOR);
  if (same (string, "system_boot_initiated"))
    return (IPMI_EVENT_SENSOR_TYPE_SYSTEM_BOOT_INITIATED);
  if (same (string, "boot_error"))
    return (IPMI_EVENT_SENSOR_TYPE_BOOT_ERROR);
  if (same (string, "os_boot"))
    return (IPMI_EVENT_SENSOR_TYPE_OS_BOOT);
  if (same (string, "os_critical_stop"))
    return (IPMI_EVENT_SENSOR_TYPE_OS_CRITICAL_STOP);
  if (same (string, "slot_connector"))
    return (IPMI_EVENT_SENSOR_TYPE_SLOT_CONNECTOR);
  if (same (string, "system_acpi_power_state"))
    return (IPMI_EVENT_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE);
  if (same (string, "watchdog2"))
    return (IPMI_EVENT_SENSOR_TYPE_WATCHDOG2);
  if (same (string, "platform_alert"))
    return (IPMI_EVENT_SENSOR_TYPE_PLATFORM_ALERT);
  if (same (string, "entity_presence"))
    return (IPMI_EVENT_SENSOR_TYPE_ENTITY_PRESENCE);
  if (same (string, "monitor_asic_ic"))
    return (IPMI_EVENT_SENSOR_TYPE_MONITOR_ASIC_IC);
  if (same (string, "lan"))
    return (IPMI_EVENT_SENSOR_TYPE_LAN);
  if (same (string, "management_subsystem_health"))
    return (IPMI_EVENT_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH);
  if (same (string, "battery"))
    return (IPMI_EVENT_SENSOR_TYPE_BATTERY);
  if (same (string, "session_audit"))
    return (IPMI_EVENT_SENSOR_TYPE_SESSION_AUDIT);
  if (same (string, "version_change"))
    return (IPMI_EVENT_SENSOR_TYPE_VERSION_CHANGE);
  if (same (string, "fru_state"))
    return (IPMI_EVENT_SENSOR_TYPE_FRU_STATE);
  if (same (string, "any"))
    return (IPMI_EVENT_SENSOR_TYPE_ANY);

  return (-1);
}

char *
sensor_type_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_EVENT_SENSOR_TYPE_RESERVED:
      return "Reserved";
    case IPMI_EVENT_SENSOR_TYPE_TEMPERATURE:
      return "Temperature";
    case IPMI_EVENT_SENSOR_TYPE_VOLTAGE:
      return "Voltage";
    case IPMI_EVENT_SENSOR_TYPE_CURRENT:
      return "Current";
    case IPMI_EVENT_SENSOR_TYPE_FAN:
      return "Fan";
    case IPMI_EVENT_SENSOR_TYPE_PHYSICAL_SECURITY:
      return "Physical_Security";
    case IPMI_EVENT_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return "Platform_Security_Violation_Attempt";
    case IPMI_EVENT_SENSOR_TYPE_PROCESSOR:
      return "Processor";
    case IPMI_EVENT_SENSOR_TYPE_POWER_SUPPLY:
      return "Power_Supply";
    case IPMI_EVENT_SENSOR_TYPE_POWER_UNIT:
      return "Power_Unit";
    case IPMI_EVENT_SENSOR_TYPE_COOLING_DEVICE:
      return "Cooling_Device";
    case IPMI_EVENT_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR:
      return "Other_Units_Based_Sensor";
    case IPMI_EVENT_SENSOR_TYPE_MEMORY:
      return "Memory";
    case IPMI_EVENT_SENSOR_TYPE_DRIVE_SLOT:
      return "Drive_Slot";
    case IPMI_EVENT_SENSOR_TYPE_POST_MEMORY_RESIZE:
      return "Post_Memory_Resize";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return "System_Firmware_Progress";
    case IPMI_EVENT_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return "Event_Logging_Disabled";
    case IPMI_EVENT_SENSOR_TYPE_WATCHDOG1:
      return "Watchdog1";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_EVENT:
      return "System_Event";
    case IPMI_EVENT_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return "Critical_interrupt";
    case IPMI_EVENT_SENSOR_TYPE_BUTTON_SWITCH:
      return "Button_Switch";
    case IPMI_EVENT_SENSOR_TYPE_MODULE_BOARD:
      return "Module_Board";
    case IPMI_EVENT_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR:
      return "Microcontroller_Coprocessor";
    case IPMI_EVENT_SENSOR_TYPE_ADD_IN_CARD:
      return "Add_In_Card";
    case IPMI_EVENT_SENSOR_TYPE_CHASSIS:
      return "Chassis";
    case IPMI_EVENT_SENSOR_TYPE_CHIP_SET:
      return "Chip_Set";
    case IPMI_EVENT_SENSOR_TYPE_OTHER_FRU:
      return "Other_FRU";
    case IPMI_EVENT_SENSOR_TYPE_CABLE_INTERCONNECT:
      return "Cable_Interconnect";
    case IPMI_EVENT_SENSOR_TYPE_TERMINATOR:
      return "Terminator";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return "System_Boot_Initiated";
    case IPMI_EVENT_SENSOR_TYPE_BOOT_ERROR:
      return "Boot_Error";
    case IPMI_EVENT_SENSOR_TYPE_OS_BOOT:
      return "OS_Boot";
    case IPMI_EVENT_SENSOR_TYPE_OS_CRITICAL_STOP:
      return "OS_Critical_Stop";
    case IPMI_EVENT_SENSOR_TYPE_SLOT_CONNECTOR:
      return "Slot_Connector";
    case IPMI_EVENT_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE:
      return "System_ACPI_Power_State";
    case IPMI_EVENT_SENSOR_TYPE_WATCHDOG2:
      return "Watchdog2";
    case IPMI_EVENT_SENSOR_TYPE_PLATFORM_ALERT:
      return "Platform_Alert";
    case IPMI_EVENT_SENSOR_TYPE_ENTITY_PRESENCE:
      return "Entity_Presence";
    case IPMI_EVENT_SENSOR_TYPE_MONITOR_ASIC_IC:
      return "Monitor_ASIC_IC";
    case IPMI_EVENT_SENSOR_TYPE_LAN:
      return "LAN";
    case IPMI_EVENT_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return "Management_Subsystem_Health";
    case IPMI_EVENT_SENSOR_TYPE_BATTERY:
      return "Battery";
    case IPMI_EVENT_SENSOR_TYPE_SESSION_AUDIT:
      return "Session_Audit";
    case IPMI_EVENT_SENSOR_TYPE_VERSION_CHANGE:
      return "Version_Change";
    case IPMI_EVENT_SENSOR_TYPE_FRU_STATE:
      return "FRU_State";
    case IPMI_EVENT_SENSOR_TYPE_ANY:
      return "Any";

    }
  return "";
}

int
exception_actions_number (const char *string)
{
  assert (string);

  if (same (string, "NO_ACTION"))
    return (IPMI_DCMI_EXCEPTION_ACTION_NO_ACTION);
  if (same (string, "HARD_POWER_OFF_SYSTEM"))
    return (IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM);
  if (same (string, "LOG_EVENT_TO_SEL_ONLY"))
    return (IPMI_DCMI_EXCEPTION_ACTION_LOG_EVENT_TO_SEL_ONLY);

  return (-1);
}

char *
exception_actions_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_DCMI_EXCEPTION_ACTION_NO_ACTION:
      return "NO_ACTION";
    case IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM:
      return "HARD_POWER_OFF_SYSTEM";
    case IPMI_DCMI_EXCEPTION_ACTION_LOG_EVENT_TO_SEL_ONLY:
      return "LOG_EVENT_TO_SEL_ONLY";
    }
  return "";
}
