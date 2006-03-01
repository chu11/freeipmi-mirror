/* 
   ipmi-lan-interface.h - IPMI LAN Interface

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

#ifndef _IPMI_LAN_INTERFACE_H
#define	_IPMI_LAN_INTERFACE_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include <sys/socket.h>

#define IPMI_LAN_PKT_PAD_SIZE   1

ssize_t ipmi_lan_sendto (int sockfd, 
			 const void *pkt, 
			 size_t pkt_len, 
			 int flags, 
			 const struct sockaddr *to, 
			 unsigned int tolen);

ssize_t ipmi_lan_recvfrom (int sockfd, 
			   void *pkt, 
			   size_t pkt_len, 
			   int flags, 
			   struct sockaddr *from, 
			   unsigned int *fromlen);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-lan-interface.h */


