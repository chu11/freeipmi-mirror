/* 
   ipmi-lan-interface.c - IPMI LAN Interface

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-lan-interface.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"

ssize_t 
ipmi_lan_sendto (int s, 
		 const void *buf, 
		 size_t len, 
		 int flags, 
		 const struct sockaddr *to, 
		 unsigned int tolen)
{
  void *_buf;
  ssize_t bytes_sent;
  size_t _len;
  size_t pad_len = 0;

  ERR_EINVAL (buf && len);

  /*
    Note from Table 12-8, RMCP Packet for IPMI via Ethernet footnote
    Some LAN adapter chips may have a problem where packets of overall
    lengths 56, 84, 112, 128, or 156 are not handled correctly. The
    PAD byte is added as necessary to avoid these overall
    lengths. Remote console software must use the PAD byte when
    formatting packets to any 10/100 Ethernet device that accepts RMCP
    packets. -- Anand Babu
  */
  _len = len;
  if (_len == 56  ||
      _len == 84  ||
      _len == 112 ||
      _len == 128 ||
      _len == 156)
    pad_len += IPMI_LAN_PKT_PAD_SIZE;

  _len += pad_len;
  _buf = alloca (_len);         
  memset (_buf, 0, _len);
  memcpy (_buf, buf, len);
  
  bytes_sent = sendto (s, _buf, _len, flags, to, tolen);

  if (bytes_sent == -1)
    return -1;
  else
    return (bytes_sent - pad_len);
}

ssize_t 
ipmi_lan_recvfrom (int s, 
		   void *buf, 
		   size_t len, 
		   int flags, 
		   struct sockaddr *from, 
		   unsigned int *fromlen)
{
  ssize_t bytes_recvd = 0;
  void *recv_buf;
  size_t recv_buf_len;
  size_t pad_len = 0;

  ERR_EINVAL (buf && len);

  if (len < 1024)
    recv_buf_len = 1024;
  else
    recv_buf_len = len;
  
  /* See comment in ipmi_lan_sendto */
  /* WILL LET THIS CHECK GO SOON --ab@gnu.org.in */
  if (recv_buf_len == 56  ||
      recv_buf_len == 84  ||
      recv_buf_len == 112 ||
      recv_buf_len == 128 ||
      recv_buf_len == 156)
    pad_len = IPMI_LAN_PKT_PAD_SIZE;

  recv_buf_len += pad_len;
  recv_buf = alloca (recv_buf_len);
  
  bytes_recvd = recvfrom (s, recv_buf, recv_buf_len, flags, from, fromlen);
  if (bytes_recvd == -1)
    {
      /*  if (recv_buf) free (recv_buf); */
      return -1;
    }
  
  recv_buf_len = pad_len ? (bytes_recvd - pad_len) : bytes_recvd;
  memcpy (buf, recv_buf, recv_buf_len);
  /* if (recv_buf) free (recv_buf); */
  return (recv_buf_len);
}

