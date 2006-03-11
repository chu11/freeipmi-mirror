/*****************************************************************************\
 *  $Id: ipmipower_packet.h,v 1.9 2006-03-11 20:15:23 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMIPOWER_PACKETS_H
#define _IPMIPOWER_PACKETS_H

#include "ipmipower.h"

/* ipmipower_packet_cmd_template
 * - retrieve template of the appropriate packet type
 */
/* fiid_template_t */
fiid_field_t *
ipmipower_packet_cmd_template(ipmipower_powercmd_t ip, packet_type_t pkt);

/* ipmipower_packet_cmd_obj
 * - retrieve object pointer of the appropriate packet type
 */
fiid_obj_t ipmipower_packet_cmd_obj(ipmipower_powercmd_t ip, packet_type_t pkt);

/* ipmipower_packet_store
 * - Store pkt into ipmipower_powercmd_t structure
 * - if buffer too long, packet truncated
 * - Only works with response packet types
 * - Returns -1 if the packet is un-storeable, returns 0 on success
 */
int ipmipower_packet_store(ipmipower_powercmd_t ip, packet_type_t pkt,
			   char *buffer, int len);

/* ipmipower_packet_dump
 * - Dump contents of ipmi packet
 */
void ipmipower_packet_dump(ipmipower_powercmd_t ip, packet_type_t pkt,
                           char *buffer, int len);

/* ipmipower_packet_create
 * - Create packet of the specified packet type
 * - Only works with request packet types
 * - Returns length of packet stored in buffer
 */
int ipmipower_packet_create(ipmipower_powercmd_t ip, packet_type_t pkt,
                             char *buffer, int len);

/* ipmipower_packet_errmsg
 * - Get error message type
 * - Only works with response packet types
 * Returns message error code for appropriate error message
 */
msg_type_t ipmipower_packet_errmsg(ipmipower_powercmd_t ip, packet_type_t pkt);

#endif /* _IPMIPOWER_PACKETS_H */
