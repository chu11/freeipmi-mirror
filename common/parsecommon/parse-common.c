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
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>
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
  else if (strcasecmp (str, IPMI_PARSE_DEVICE_INTELDCMI_STR) == 0)
    return (IPMI_DEVICE_INTELDCMI);

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

static int
_parse_workaround_flags (const char *str,
			 unsigned int *workaround_flags_outofband,
			 unsigned int *workaround_flags_outofband_2_0,
			 unsigned int *workaround_flags_inband,
			 unsigned int *workaround_flags_sdr,
			 unsigned int *section_specific_workaround_flags,
			 int command_line_flag)
{
  char buf[WORKAROUND_FLAG_BUFLEN+1];
  char *tok;

  assert (str);

  memset (buf, '\0', WORKAROUND_FLAG_BUFLEN+1);
  strncpy (buf, str, WORKAROUND_FLAG_BUFLEN);

  if (workaround_flags_outofband)
    (*workaround_flags_outofband) = 0;
  if (workaround_flags_outofband_2_0)
    (*workaround_flags_outofband_2_0) = 0;
  if (workaround_flags_inband)
    (*workaround_flags_inband) = 0;
  if (workaround_flags_sdr)
    (*workaround_flags_sdr) = 0;
  if (section_specific_workaround_flags)
    (*section_specific_workaround_flags) = 0;

  tok = strtok (buf, ",");
  while (tok)
    {
      if (command_line_flag
	  && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_NONE_STR))
	{
	  if (workaround_flags_outofband)
	    (*workaround_flags_outofband) = 0;
	  if (workaround_flags_outofband_2_0)
	    (*workaround_flags_outofband_2_0) = 0;
	  if (workaround_flags_inband)
	    (*workaround_flags_inband) = 0;
	  if (workaround_flags_sdr)
	    (*workaround_flags_sdr) = 0;
	  if (section_specific_workaround_flags)
	    (*section_specific_workaround_flags) = 0;
	  break;
	}
      
      /* special case, may apply to outofband and outofband_2_0 */
      if (!strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES_STR))
        {
          if (workaround_flags_outofband)
            (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES;
          if (workaround_flags_outofband_2_0)
            (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES;
        }
      /* special case, may apply to outofband and outofband_2_0 */
      else if (!strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK_STR))
	{
          if (workaround_flags_outofband)
            (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK;
          if (workaround_flags_outofband_2_0)
            (*workaround_flags_outofband_2_0) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK;
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
      else if (workaround_flags_outofband
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK_STR))
        (*workaround_flags_outofband) |= IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK;
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
      else if (workaround_flags_inband
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS_STR))
        (*workaround_flags_inband) |= IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS;
      else if (workaround_flags_inband
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_INBAND_SPIN_POLL_STR))
        (*workaround_flags_inband) |= IPMI_PARSE_WORKAROUND_FLAGS_INBAND_SPIN_POLL;
      else if (workaround_flags_sdr
               && !strcasecmp (tok, IPMI_PARSE_WORKAROUND_FLAGS_SDR_ASSUME_MAX_SDR_RECORD_COUNT_STR))
        (*workaround_flags_sdr) |= IPMI_PARSE_WORKAROUND_FLAGS_SDR_ASSUME_MAX_SDR_RECORD_COUNT;
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
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHANNEL_PAYLOAD_SUPPORT_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHANNEL_PAYLOAD_SUPPORT;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SERIAL_ALERTS_DEFERRED_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SERIAL_ALERTS_DEFERRED;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_INCREMENT_SOL_PACKET_SEQUENCE_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_INCREMENT_SOL_PACKET_SEQUENCE;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_SYSTEM_EVENT;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_DISCRETE_READING_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_DISCRETE_READING;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SCANNING_DISABLED_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_SCANNING_DISABLED;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_BMC_OWNER_STR))
	(*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_ASSUME_BMC_OWNER;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_AUTH_CODE_STR))
	(*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IGNORE_AUTH_CODE;
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
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_MALFORMED_ACK_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_MALFORMED_ACK;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_GUID_FORMAT_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_GUID_FORMAT;
      else if (section_specific_workaround_flags
               && !strcasecmp (tok, IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING_STR))
        (*section_specific_workaround_flags) |= IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING;
      else
        return (-1);
      tok = strtok (NULL, ",");
    }

  return (0);
}

int
parse_workaround_flags (const char *str,
			unsigned int *workaround_flags_outofband,
			unsigned int *workaround_flags_outofband_2_0,
			unsigned int *workaround_flags_inband,
			unsigned int *workaround_flags_sdr,
			unsigned int *section_specific_workaround_flags)
{
  return (_parse_workaround_flags (str,
				   workaround_flags_outofband,
				   workaround_flags_outofband_2_0,
				   workaround_flags_inband,
				   workaround_flags_sdr,
				   section_specific_workaround_flags,
				   0));
}

int
parse_workaround_flags_tool (const char *str,
			     unsigned int *workaround_flags_outofband,
			     unsigned int *workaround_flags_outofband_2_0,
			     unsigned int *workaround_flags_inband,
			     unsigned int *workaround_flags_sdr,
			     unsigned int *section_specific_workaround_flags)
{
  return (_parse_workaround_flags (str,
				   workaround_flags_outofband,
				   workaround_flags_outofband_2_0,
				   workaround_flags_inband,
				   workaround_flags_sdr,
				   section_specific_workaround_flags,
				   1));
}

/* a k_g key is interpreted as ascii text unless it is prefixed with
   "0x", in which case is it interpreted as hexadecimal */
int
parse_kg (void *out, unsigned int outlen, const char *in)
{
  char *p, *q;
  unsigned int i, j;
  char buf[3] = { 0, 0, 0};
  int rv = 0;

  assert (out);
  assert (in);
  assert (outlen > IPMI_MAX_K_G_LENGTH);

  if (!strlen (in))
    return (0);

  if (!strncasecmp (in, "0x", 2))
    {
      if (strlen (in) > IPMI_MAX_K_G_LENGTH*2+2)
        return (-1);
      p = (char *)in + 2;
      memset (out, 0, IPMI_MAX_K_G_LENGTH);
      for (i = j = 0; i < strlen (p); i+=2, j++)
        {
          if (!isxdigit (p[i])
              || (p[i+1] && !isxdigit (p[i+1])))
            return (-1);
          buf[0] = p[i];
          if (p[i+1])
            buf[1] = p[i+1];
          else
            buf[1] = 0;
          buf[2] = '\0';
          errno = 0;
          (((uint8_t *)out)[j]) = (uint8_t)strtoul (buf, &q, 16);
          if (errno
              || ((p[i+1] && (q != buf + 2))
                  || (!p[i+1] && (q != buf + 1))))
            return (-1);
          rv++;
        }
    }
  else
    {
      if (strlen (in) > IPMI_MAX_K_G_LENGTH)
        return (-1);
      memset (out, 0, IPMI_MAX_K_G_LENGTH);
      memcpy (out, in, strlen (in));
      rv = strlen (in);
    }

  return (rv);
}

void
parse_get_freeipmi_outofband_flags (unsigned int parse_workaround_flags_outofband,
				    unsigned int *freeipmi_workaround_flags_outofband)
{
  assert (freeipmi_workaround_flags_outofband);

  (*freeipmi_workaround_flags_outofband) = 0;

  if (parse_workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES)
    (*freeipmi_workaround_flags_outofband) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES;
  if (parse_workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO)
    (*freeipmi_workaround_flags_outofband) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO;
  if (parse_workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION)
    (*freeipmi_workaround_flags_outofband) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION;
  if (parse_workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE)
    (*freeipmi_workaround_flags_outofband) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE;
  if (parse_workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER)
    (*freeipmi_workaround_flags_outofband) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER;
  if (parse_workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK)
    (*freeipmi_workaround_flags_outofband) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK;
  if (parse_workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK)
    (*freeipmi_workaround_flags_outofband) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK;
}

void
parse_get_freeipmi_outofband_2_0_flags (unsigned int parse_workaround_flags_outofband_2_0,
					unsigned int *freeipmi_workaround_flags_outofband_2_0)
{
  assert (freeipmi_workaround_flags_outofband_2_0);

  (*freeipmi_workaround_flags_outofband_2_0) = 0;

  if (parse_workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES)
    (*freeipmi_workaround_flags_outofband_2_0) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES;
  if (parse_workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    (*freeipmi_workaround_flags_outofband_2_0) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION;
  if (parse_workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION)
    (*freeipmi_workaround_flags_outofband_2_0) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION;
  if (parse_workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION)
    (*freeipmi_workaround_flags_outofband_2_0) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION;
  if (parse_workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE)
    (*freeipmi_workaround_flags_outofband_2_0) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE;
  if (parse_workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE)
    (*freeipmi_workaround_flags_outofband_2_0) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE;
  if (parse_workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK)
    (*freeipmi_workaround_flags_outofband_2_0) |= IPMI_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK;
}

void
parse_get_freeipmi_inband_flags (unsigned int parse_workaround_flags_inband,
				 unsigned int *freeipmi_workaround_flags_inband)
{
  assert (freeipmi_workaround_flags_inband);

  (*freeipmi_workaround_flags_inband) = 0;

  if (parse_workaround_flags_inband & IPMI_PARSE_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS)
    (*freeipmi_workaround_flags_inband) |= IPMI_WORKAROUND_FLAGS_INBAND_ASSUME_IO_BASE_ADDRESS;
  
  if (parse_workaround_flags_inband & IPMI_PARSE_WORKAROUND_FLAGS_INBAND_SPIN_POLL)
    (*freeipmi_workaround_flags_inband) |= IPMI_WORKAROUND_FLAGS_INBAND_SPIN_POLL;
}
