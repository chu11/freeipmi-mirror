/* 
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

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

#include "freeipmi/ipmi-util.h"
#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-comp-code-spec.h"
#include "freeipmi/rmcp-interface.h"

#include "ipmi-err-wrappers.h"
#include "ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

uint8_t
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

  ERR_EINVAL (fiid_obj_valid(obj_cmd));

  FIID_OBJ_FIELD_LOOKUP (obj_cmd, "cmd");

  FIID_OBJ_FIELD_LEN (len, obj_cmd, "cmd");

  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_cmd, "cmd", &cmd_recv);

  return ((((uint8_t)cmd_recv) == cmd) ? 1 : 0);
}

int8_t
ipmi_check_completion_code(fiid_obj_t obj_cmd, uint8_t completion_code)
{
  uint64_t completion_code_recv;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(obj_cmd));

  FIID_OBJ_FIELD_LOOKUP (obj_cmd, "comp_code");

  FIID_OBJ_FIELD_LEN (len, obj_cmd, "comp_code");

  ERR_EINVAL (len);

  FIID_OBJ_GET(obj_cmd, "comp_code", &completion_code_recv);

#if defined (IPMI_SYSLOG) || defined (IPMI_TRACE)
  if ((uint8_t)completion_code_recv != completion_code)
    {
#if defined (IPMI_SYSLOG)
      char errstr[ERR_WRAPPER_STR_MAX_LEN];
      snprintf (errstr, ERR_WRAPPER_STR_MAX_LEN,
                "ipmi_check_completion_code: completion code invalid: %x",
                (uint8_t)completion_code_recv);
      syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);
#endif /* IPMI_SYSLOG */
#if defined (IPMI_TRACE)
      fprintf(stderr,
              "ipmi_check_completion_code: completion code invalid: %x\n",
              (uint8_t)completion_code_recv);
      fflush(stderr);
#endif /* IPMI_TRACE */
    }
#endif /* IPMI_SYSLOG || IPMI_TRACE */

  return ((((uint8_t)completion_code_recv) == completion_code) ? 1 : 0);
}

int8_t 
ipmi_check_completion_code_success (fiid_obj_t obj_cmd)
{
  return ipmi_check_completion_code(obj_cmd, IPMI_COMP_CODE_COMMAND_SUCCESS);
}

int
ipmi_get_random(uint8_t *buf, uint32_t buflen)
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

  if ((rv = read(fd, (void *)buf, buflen)) < buflen)
    goto gcrypt_rand;

  close(fd);
  return rv;
#endif /* !(HAVE_DEVURANDOM || HAVE_DEVRANDOM) */

 gcrypt_rand:
  gcry_randomize((unsigned char *)buf, buflen, GCRY_STRONG_RANDOM);
  return buflen;
}

int8_t
ipmi_is_ipmi_1_5_packet(uint8_t *pkt, uint32_t pkt_len)
{
  int32_t rmcp_hdr_len;
  uint8_t auth_type;

  FIID_TEMPLATE_LEN_BYTES(rmcp_hdr_len, tmpl_rmcp_hdr);

  ERR_EINVAL (!(pkt_len <= rmcp_hdr_len));

  auth_type = *(pkt + rmcp_hdr_len);
  auth_type &= 0x0F;
  return ((auth_type != IPMI_AUTHENTICATION_TYPE_RMCPPLUS) ? 1 : 0);
}

int8_t
ipmi_is_ipmi_2_0_packet(uint8_t *pkt, uint32_t pkt_len)
{
  int32_t rmcp_hdr_len;
  uint8_t auth_type;

  FIID_TEMPLATE_LEN_BYTES(rmcp_hdr_len, tmpl_rmcp_hdr);

  ERR_EINVAL (!(pkt_len <= rmcp_hdr_len));

  auth_type = *(pkt + rmcp_hdr_len);
  auth_type &= 0x0F;
  return ((auth_type == IPMI_AUTHENTICATION_TYPE_RMCPPLUS) ? 1 : 0);
}

