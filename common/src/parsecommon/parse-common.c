/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "freeipmi-portability.h"
#include "parse-common.h"

#define WORKAROUND_FLAG_BUFLEN 1024

int
parse_inband_driver_type (const char *str)
{
  assert (str);

  if (strcasecmp (str, IPMI_PARSE_DEVICE_KCS_STR) == 0)
    return (IPMI_DEVICE_KCS);
  else if (strcasecmp (str, IPMI_PARSE_DEVICE_SSIF_STR) == 0)
    return (IPMI_DEVICE_SSIF);
  /* support "open" for those that might be used to
   * ipmitool.
   */
  else if (strcasecmp (str, IPMI_PARSE_DEVICE_OPENIPMI_STR) == 0
           || strcasecmp (str, IPMI_PARSE_DEVICE_OPENIPMI_STR2) == 0)
    return (IPMI_DEVICE_OPENIPMI);
  /* support "bmc" for those that might be used to
   * ipmitool.
   */
  else if (strcasecmp (str, IPMI_PARSE_DEVICE_SUNBMC_STR) == 0
           || strcasecmp (str, IPMI_PARSE_DEVICE_SUNBMC_STR2) == 0)
    return (IPMI_DEVICE_SUNBMC);

  return (-1);
}

int
parse_outofband_driver_type (const char *str)
{
  assert (str);

  if (strcasecmp (str, IPMI_PARSE_DEVICE_LAN_STR) == 0)
    return (IPMI_DEVICE_LAN);
  /* support "lanplus" for those that might be used to ipmitool.
   * support typo variants to ease.
   */
  else if (strcasecmp (str, IPMI_PARSE_DEVICE_LAN_2_0_STR) == 0
           || strcasecmp (str, IPMI_PARSE_DEVICE_LAN_2_0_STR2) == 0
           || strcasecmp (str, IPMI_PARSE_DEVICE_LAN_2_0_STR3) == 0
           || strcasecmp (str, IPMI_PARSE_DEVICE_LAN_2_0_STR4) == 0
           || strcasecmp (str, IPMI_PARSE_DEVICE_LAN_2_0_STR5) == 0)
    return (IPMI_DEVICE_LAN_2_0);

  return (-1);
}

int
parse_driver_type (const char *str)
{
  int ret;

  assert (str);

  if ((ret = parse_inband_driver_type (str)) < 0)
    ret = parse_outofband_driver_type (str);

  return (ret);
}

int
parse_authentication_type (const char *str)
{
  assert (str);

  if (strcasecmp (str, IPMI_PARSE_AUTHENTICATION_TYPE_NONE_STR) == 0)
    return (IPMI_AUTHENTICATION_TYPE_NONE);
  /* keep "plain" for backwards compatability */
  else if (strcasecmp (str, IPMI_PARSE_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR) == 0
           || strcasecmp (str, IPMI_PARSE_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR2) == 0)
    return (IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY);
  else if (strcasecmp (str, IPMI_PARSE_AUTHENTICATION_TYPE_MD2_STR) == 0)
    return (IPMI_AUTHENTICATION_TYPE_MD2);
  else if (strcasecmp (str, IPMI_PARSE_AUTHENTICATION_TYPE_MD5_STR) == 0)
    return (IPMI_AUTHENTICATION_TYPE_MD5);

  return (-1);
}

int
parse_privilege_level (const char *str)
{
  assert (str);

  if (strcasecmp (str, IPMI_PARSE_PRIVILEGE_LEVEL_USER_STR) == 0)
    return (IPMI_PRIVILEGE_LEVEL_USER);
  else if (strcasecmp (str, IPMI_PARSE_PRIVILEGE_LEVEL_OPERATOR_STR) == 0)
    return (IPMI_PRIVILEGE_LEVEL_OPERATOR);
  else if (strcasecmp (str, IPMI_PARSE_PRIVILEGE_LEVEL_ADMIN_STR) == 0
           || strcasecmp (str, IPMI_PARSE_PRIVILEGE_LEVEL_ADMIN_STR2) == 0)
    return (IPMI_PRIVILEGE_LEVEL_ADMIN);

  return (-1);
}

int
parse_workaround_flags (const char *str,
                        unsigned int *workaround_flags_outofband,
                        unsigned int *workaround_flags_outofband_2_0,
                        unsigned int *workaround_flags_inband,
                        unsigned int *section_specific_workaround_flags)
{
  char buf[WORKAROUND_FLAG_BUFLEN+1];
  char *tok;

  assert (str);
  assert (workaround_flags_outofband);
  assert (workaround_flags_outofband_2_0);
  assert (workaround_flags_inband);

  memset (buf, '\0', WORKAROUND_FLAG_BUFLEN+1);
  strncpy (buf, str, WORKAROUND_FLAG_BUFLEN);

  if (workaround_flags_outofband)
    (*workaround_flags_outofband) = 0;
  if (workaround_flags_outofband_2_0)
    (*workaround_flags_outofband_2_0) = 0;
  if (workaround_flags_inband)
    (*workaround_flags_inband) = 0;
  if (section_specific_workaround_flags)
    (*section_specific_workaround_flags) = 0;

  tok = strtok (buf, ",");
  while (tok)
    {
      /* special case, may apply to outofband and outofband_2_0 */
      if (!strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES_STR))
        {
          if (workaround_flags_outofband)
            (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES;
          if (workaround_flags_outofband_2_0)
            (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES;
        }
      else if (workaround_flags_outofband
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO_STR))
        (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO;
      else if (workaround_flags_outofband
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION_STR))
        (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION;
      else if (workaround_flags_outofband
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE_STR))
        (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE;
      else if (workaround_flags_outofband
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER_STR))
        (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER;
#if 0
      /* handled above w/ special case */
      else if (workaround_flags_outofband_2_0
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES_STR))
        (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES;
#endif
      else if (workaround_flags_outofband_2_0
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION_STR))
        (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION;
      else if (workaround_flags_outofband_2_0
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION_STR))
        (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION;
      else if (workaround_flags_outofband_2_0
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION_STR))
        (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION;
      else if (workaround_flags_outofband_2_0
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE_STR))
        (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE;
      else if (workaround_flags_outofband_2_0
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE_STR))
        (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE;
      else if (workaround_flags_outofband_2_0
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS_STR))
        (*workaround_flags_inband) |= IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PORT_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SOL_PORT;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SLOW_COMMIT_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SLOW_COMMIT;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SOL_CHANNEL_ASSUME_LAN_CHANNEL_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SOL_CHANNEL_ASSUME_LAN_CHANNEL;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_STATE_FLAG;
      else
        return (-1);
      tok = strtok (NULL, ",");
    }

  return (0);
}
