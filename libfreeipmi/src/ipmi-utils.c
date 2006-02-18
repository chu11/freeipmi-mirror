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

#include "freeipmi.h"
#include "fiid-wrappers.h"

int8_t
ipmi_chksum (uint8_t *buf, uint64_t len)
{
  register uint64_t i = 0;
  register int8_t chksum = 0;
 
  if (buf == NULL || len == 0)
    return (chksum);

  for (; i < len; i++)
    chksum = (chksum + buf[i]) % 256;

  return (-chksum);
}

int8_t 
ipmi_comp_test (fiid_obj_t obj_cmd)
{
#if defined (IPMI_SYSLOG)
  uint64_t cmd;
#endif /* IPMI_SYSLOG */
  uint64_t comp_code;
  int32_t len;
  int8_t rv;

  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

#if defined (IPMI_SYSLOG)
  FIID_OBJ_FIELD_LOOKUP_RV (rv, obj_cmd, (uint8_t *)"cmd");

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }
#endif /* IPMI_SYSLOG */

  FIID_OBJ_FIELD_LOOKUP_RV (rv, obj_cmd, (uint8_t *)"comp_code");

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

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

int
ipmi_open_free_udp_port (void)
{
  int sockfd;
  int sockname_len;
  struct sockaddr_in sockname;
  int free_port=1025;
  int err;
  extern int errno;

  sockfd = socket (AF_INET, SOCK_DGRAM, 0);
  if (sockfd < 0)
    return (-1);

  for (; free_port < 65535; free_port++)
    {
      /* Instead of probing if the current (may be the client side)
      system has IPMI LAN support too, it is easier to avoid these two
      RMCP reserved ports. -- Anand Babu*/
      if ((free_port == RMCP_AUX_BUS_SHUNT) || 
	  (free_port == RMCP_SECURE_AUX_BUS))
	continue;

      memset (&sockname, 0, sizeof (struct sockaddr_in));
      sockname.sin_family = AF_INET;
      sockname.sin_port   = htons (free_port);
      sockname.sin_addr.s_addr = htonl (INADDR_ANY);
      sockname_len = sizeof (struct sockaddr_in);
      
      if ((err = bind (sockfd, (struct sockaddr *) &sockname, sockname_len)) == 0)
	return sockfd;
      else
	{
	  if (errno == EADDRINUSE)
	    continue;
	  else
	    return (-1);
	}
    }
  close (sockfd);
  errno = EBUSY;
  return (-1);
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

