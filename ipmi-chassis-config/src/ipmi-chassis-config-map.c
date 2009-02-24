/* 
   Copyright (C) 2008 FreeIPMI Core Team

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

#include "ipmi-chassis-config.h"
#include "ipmi-chassis-config-map.h"

#include "freeipmi-portability.h"

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
bios_boot_type_number (const char *string)
{
  if (same (string, "PC-COMPATIBLE"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_TYPE_PC_COMPATIBLE;
  if (same (string, "EFI"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_TYPE_EFI;
  return -1;
}

char *
bios_boot_type_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_TYPE_PC_COMPATIBLE:
      return "PC-COMPATIBLE";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_TYPE_EFI:
      return "EFI";
    }
  return "";
}

int
boot_device_number (const char *string)
{
  if (same (string, "NO-OVERRIDE"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_NO_OVERRIDE;
  if (same (string, "PXE"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_PXE;
  if (same (string, "HARD-DRIVE"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE;
  if (same (string, "HARD-DRIVE-SAFE-MODE"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE_SAFE_MODE;
  if (same (string, "DIAGNOSTIC_PARTITION"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_DIAGNOSTIC_PARTITION;
  if (same (string, "CD-DVD"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_CD_DVD;
  if (same (string, "BIOS-SETUP"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_BIOS_SETUP;
  if (same (string, "FLOPPY"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_FLOPPY_REMOVEABLE_MEDIA;
  return -1;
}

char *
boot_device_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_NO_OVERRIDE:
      return "NO-OVERRIDE";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_PXE:
      return "PXE";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE:
      return "HARD-DRIVE";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE_SAFE_MODE:
      return "HARD-DRIVE-SAFE-MODE";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_DIAGNOSTIC_PARTITION:
      return "DIAGNOSTIC_PARTITION";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_CD_DVD:
      return "CD-DVD";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_BIOS_SETUP:
      return "BIOS-SETUP";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_FLOPPY_REMOVEABLE_MEDIA:
      return "FLOPPY";
    }
  return "";
}

int 
firmware_bios_verbosity_number (const char *string)
{
  if (same (string, "DEFAULT"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_DEFAULT;
  if (same (string, "QUIET"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_QUIET;
  if (same (string, "VERBOSE"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VERBOSE;
  return -1;
}

char *
firmware_bios_verbosity_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_DEFAULT:
      return "DEFAULT";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_QUIET:
      return "QUIET";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VERBOSE:
      return "VERBOSE";
    }
  return "";
}

int 
console_redirection_number (const char *string)
{
  if (same (string, "BIOS-SETTING"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_BIOS_SETTING;
  if (same (string, "SUPPRESS"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_SUPPRESS;
  if (same (string, "ENABLE"))
    return IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_ENABLE;
  return -1;
}

char *
console_redirection_string (uint8_t value)
{
  switch (value)
    {
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_BIOS_SETTING:
      return "BIOS-SETTING";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_SUPPRESS:
      return "SUPPRESS";
    case IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_ENABLE:
      return "ENABLE";
    }
  return "";
}
