/* 
   ipmi-utils.c - general utility procedures

   Copyright (C) 2003, 2004 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

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

#include <errno.h>
#ifdef __FreeBSD__
#include <sys/time.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "freeipmi.h"

ipmi_chksum_t
ipmi_chksum (u_int8_t *buf, u_int64_t len)
{
  register u_int64_t i = 0;
  register ipmi_chksum_t chksum = 0;
 
  if (buf == NULL || len == 0)
    return (chksum);

  for (; i < len; i++)
    chksum = (chksum + buf[i]) % 256;

  return (-chksum);
}

int8_t
ipmi_chksum_test (u_int8_t *buf, u_int64_t len) 
{
  ipmi_chksum_t chksum_val;
  ipmi_chksum_t chksum_calc;

  if (buf == NULL || len == 0)
    {
      errno = EINVAL;
      return (-1);
    }

  chksum_val = buf[len - 1];
  chksum_calc = ipmi_chksum(buf, len - 1);
  return ((chksum_val == chksum_calc) ? 1 : 0);
}

int8_t
ipmi_comp_test (fiid_obj_t obj_cmd)
{
  if (!obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (IPMI_COMP_CODE (obj_cmd) != IPMI_COMMAND_SUCCESS)
    {
#if defined (IPMI_SYSLOG)
      char errstr[IPMI_ERR_STR_MAX_LEN], _str[IPMI_ERR_STR_MAX_LEN]; 
      ipmi_strerror_cmd_r (obj_cmd, _str, IPMI_ERR_STR_MAX_LEN);
      sprintf (errstr, "cmd[%d].comp_code[%d]: %s", obj_cmd[0],
	       IPMI_COMP_CODE (obj_cmd), _str);
      syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);
#endif /* IPMI_SYSLOG */
      return (0);
    }
  return (1); 
}

int
ipmi_input_timeout (int fd, unsigned int seconds)
{
  fd_set set;
  struct timeval timeout;
  
  /* Initialize the file descriptor set. */
  FD_ZERO (&set);
  FD_SET (fd, &set);

  /* Initialize the timeout data structure. */
  timeout.tv_sec = seconds;
  timeout.tv_usec = 0;

  /* `select' returns 0 if timeout, 1 if input available, -1 if error. */
  return TEMP_FAILURE_RETRY (select (FD_SETSIZE,
				     &set, NULL, NULL,
				     &timeout));
}

int 
ipmi_is_root ()
{
  uid_t uid = getuid ();
  if (uid == 0)
    return 1;
  return 0;
}


int
open_free_udp_port (void)
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

