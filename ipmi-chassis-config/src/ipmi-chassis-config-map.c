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
