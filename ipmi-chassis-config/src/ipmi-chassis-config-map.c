/*
 * Copyright (C) 2008-2011 FreeIPMI Core Team
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

#include "ipmi-chassis-config.h"
#include "ipmi-chassis-config-map.h"

#include "freeipmi-portability.h"

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
