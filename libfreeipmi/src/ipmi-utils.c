/* 
   ipmi-utils.c - general utility procedures

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

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
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <stdint.h>
#include <errno.h>
#if defined (IPMI_SYSLOG)
#include <syslog.h>
#endif /* IPMI_SYSLOG */

#include "freeipmi-portability.h"
#include "fiid.h"
#include "fiid-wrappers.h"
#include "ipmi-comp-code-spec.h"

#include "bit-ops.h"

int8_t
ipmi_checksum (uint8_t *buf, uint64_t len)
{
  register uint64_t i = 0;
  register int8_t checksum = 0;
 
  if (buf == NULL || len == 0)
    return (checksum);

  for (; i < len; i++)
    checksum = (checksum + buf[i]) % 256;

  return (-checksum);
}

int8_t 
ipmi_completion_code_check (fiid_obj_t obj_cmd)
{
#if defined (IPMI_SYSLOG)
  uint64_t cmd;
#endif /* IPMI_SYSLOG */
  uint64_t comp_code;
  int32_t len;

  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

#if defined (IPMI_SYSLOG)
  FIID_OBJ_FIELD_LOOKUP (obj_cmd, (uint8_t *)"cmd");
#endif /* IPMI_SYSLOG */

  FIID_OBJ_FIELD_LOOKUP (obj_cmd, (uint8_t *)"comp_code");

  FIID_OBJ_FIELD_LEN (len, obj_cmd, (uint8_t *)"comp_code");

  if (!len)
    {
      errno = EINVAL;
      return (-1);
    }

#if defined (IPMI_SYSLOG)
  FIID_OBJ_GET (obj_cmd, (uint8_t *)"cmd", &cmd);
#endif /* IPMI_SYSLOG */

  FIID_OBJ_GET (obj_cmd, (uint8_t *)"comp_code", &comp_code);

  if (comp_code != IPMI_COMP_CODE_COMMAND_SUCCESS)
    {
#if defined (IPMI_SYSLOG)
      char errstr[IPMI_ERR_STR_MAX_LEN], _str[IPMI_ERR_STR_MAX_LEN];
      ipmi_strerror_cmd_r (obj_cmd, _str, IPMI_ERR_STR_MAX_LEN);
      sprintf (errstr, "cmd[%llX].comp_code[%llX]: %s",
               cmd, comp_code, _str);
      syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);
#endif /* IPMI_SYSLOG */
      errno = EIO;
      return (0);
    }

  return (1);
}

int8_t
ipmi_ipv4_address_string2int(char *src, uint32_t *dest)
{
  unsigned int b1, b2, b3, b4;
  uint64_t val;
  int rv;

  if (!src || !dest)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((rv = sscanf (src, "%u.%u.%u.%u", &b1, &b2, &b3, &b4)) < 0)
    return (-1);
  if (rv != 4)
    {
      errno = EINVAL;
      return (-1);
    }

  val = 0;
  if (bits_merge (val, 0,  8,  b1, &val) < 0)
    return (-1);
  if (bits_merge (val, 8,  16, b2, &val) < 0)
    return (-1);
  if (bits_merge (val, 16, 24, b3, &val) < 0)
    return (-1);
  if (bits_merge (val, 24, 32, b4, &val) < 0)
    return (-1);

  *dest = val;
  return (0);
}

int8_t
ipmi_mac_address_string2int(char *src, uint64_t *dest)
{
  unsigned int b1, b2, b3, b4, b5, b6;
  uint64_t val;
  int rv;

  if (!src || !dest)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((rv = sscanf (src, "%02X:%02X:%02X:%02X:%02X:%02X", &b1, &b2, &b3, &b4, &b5, &b6)) < 0)
    return (-1);
  if (rv != 6)
    {
      errno = EINVAL;
      return (-1);
    }

  val = 0;
  if (bits_merge (val, 0,  8,  b1, &val) < 0)
    return (-1);
  if (bits_merge (val, 8,  16, b2, &val) < 0)
    return (-1);
  if (bits_merge (val, 16, 24, b3, &val) < 0)
    return (-1);
  if (bits_merge (val, 24, 32, b4, &val) < 0)
    return (-1);
  if (bits_merge (val, 32, 40, b5, &val) < 0)
    return (-1);
  if (bits_merge (val, 40, 48, b6, &val) < 0)
    return (-1);

  *dest = val;
  return (0);
}

