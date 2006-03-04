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
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if defined (IPMI_SYSLOG)
#include <syslog.h>
#endif /* IPMI_SYSLOG */
#include <gcrypt.h>

#include "freeipmi/ipmi-utils.h"
#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-comp-code-spec.h"

#include "bit-ops.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

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
ipmi_check_cmd(fiid_obj_t obj_cmd, uint8_t cmd)
{
  uint64_t cmd_recv;
  int32_t len;

  ERR_EINVAL (obj_cmd);

  FIID_OBJ_FIELD_LOOKUP (obj_cmd, (uint8_t *)"cmd");

  FIID_OBJ_FIELD_LEN (len, obj_cmd, (uint8_t *)"cmd");

  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_cmd, (uint8_t *)"cmd", &cmd_recv);

  return ((((uint8_t)cmd_recv) == cmd) ? 1 : 0);
}

int8_t
ipmi_check_completion_code(fiid_obj_t obj_cmd, uint8_t completion_code)
{
  uint64_t completion_code_recv;
  int32_t len;

  ERR_EINVAL (obj_cmd);

  FIID_OBJ_FIELD_LOOKUP (obj_cmd, (uint8_t *)"comp_code");

  FIID_OBJ_FIELD_LEN (len, obj_cmd, (uint8_t *)"comp_code");

  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_cmd, (uint8_t *)"comp_code", &completion_code_recv);

  return ((((uint8_t)completion_code_recv) == completion_code) ? 1 : 0);
}

int8_t 
ipmi_check_completion_code_success (fiid_obj_t obj_cmd)
{
  return ipmi_check_completion_code(obj_cmd, IPMI_COMP_CODE_COMMAND_SUCCESS);
}

int
ipmi_get_random(char *buf, unsigned int buflen)
{
#if (HAVE_DEVURANDOM || HAVE_DEVRANDOM)
  int fd, rv;
#endif /* !(HAVE_DEVURANDOM || HAVE_DEVRANDOM) */

  ERR_EINVAL (buf);
  
  if (!buflen)
    return (0);
  
#if (HAVE_DEVURANDOM || HAVE_DEVRANDOM)
#if HAVE_DEVURANDOM
  if ((fd = open("/dev/urandom", O_RDONLY)) < 0)
    goto gcrypt_rand;
#else  /* !HAVE_DEVURANDOM */
  if ((fd = open ("/dev/random", O_RDONLY)) < 0)
    goto gcrypt_rand;
#endif /* !HAVE_DEVURANDOM */

  if ((rv = read(fd, buf, buflen)) < buflen)
    goto gcrypt_rand;

  close(fd);
  return rv;
#endif /* !(HAVE_DEVURANDOM || HAVE_DEVRANDOM) */

 gcrypt_rand:
  gcry_randomize((unsigned char *)buf, buflen, GCRY_STRONG_RANDOM);
  return buflen;
}

int8_t
ipmi_ipv4_address_string2int(char *src, uint32_t *dest)
{
  unsigned int b1, b2, b3, b4;
  uint64_t val;
  int rv;

  ERR_EINVAL (src && dest);
  
  ERR (!((rv = sscanf (src, "%u.%u.%u.%u", &b1, &b2, &b3, &b4)) < 0));

  ERR_EINVAL (rv == 4);

  val = 0;
  ERR (!(bits_merge (val, 0,  8,  b1, &val) < 0));
  ERR (!(bits_merge (val, 8,  16, b2, &val) < 0));
  ERR (!(bits_merge (val, 16, 24, b3, &val) < 0));
  ERR (!(bits_merge (val, 24, 32, b4, &val) < 0));

  *dest = val;
  return (0);
}

int8_t
ipmi_mac_address_string2int(char *src, uint64_t *dest)
{
  unsigned int b1, b2, b3, b4, b5, b6;
  uint64_t val;
  int rv;

  ERR_EINVAL (src && dest);
  
  ERR (!((rv = sscanf (src, "%02X:%02X:%02X:%02X:%02X:%02X", &b1, &b2, &b3, &b4, &b5, &b6)) < 0));

  ERR_EINVAL (rv == 6);

  val = 0;
  ERR (!(bits_merge (val, 0,  8,  b1, &val) < 0));
  ERR (!(bits_merge (val, 8,  16, b2, &val) < 0));
  ERR (!(bits_merge (val, 16, 24, b3, &val) < 0));
  ERR (!(bits_merge (val, 24, 32, b4, &val) < 0));
  ERR (!(bits_merge (val, 32, 40, b5, &val) < 0));
  ERR (!(bits_merge (val, 40, 48, b6, &val) < 0));

  *dest = val;
  return (0);
}

