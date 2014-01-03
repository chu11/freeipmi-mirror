/*****************************************************************************\
 *  $Id: ipmipower_packet.h,v 1.24 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmipower is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmipower is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMIPOWER_PACKETS_H
#define IPMIPOWER_PACKETS_H

#include "ipmipower.h"

fiid_field_t *ipmipower_packet_cmd_template (ipmipower_powercmd_t ip,
					     ipmipower_packet_type_t pkt);

fiid_obj_t ipmipower_packet_cmd_obj (ipmipower_powercmd_t ip,
				     ipmipower_packet_type_t pkt);

/* ipmipower_packet_store
 * - Store pkt into ipmipower_powercmd_t structure
 * - if buf too long, packet truncated
 * - Only works with response packet types
 * - Returns 1 if packet unparsed properly, 0 if not
 */
int ipmipower_packet_store (ipmipower_powercmd_t ip,
                            ipmipower_packet_type_t pkt,
                            const void *buf,
                            unsigned int buflen);

void ipmipower_packet_dump (ipmipower_powercmd_t ip,
                            ipmipower_packet_type_t pkt,
                            const void *buf,
                            unsigned int buflen);

/* ipmipower_packet_create
 * - Create packet of the specified packet type
 * - Only works with request packet types
 * - Returns length of packet stored in buf
 */
int ipmipower_packet_create (ipmipower_powercmd_t ip,
                             ipmipower_packet_type_t pkt,
                             void *buf,
                             unsigned int buflen);

/* ipmipower_packet_errmsg
 * - Get error message type
 * - Only works with response packet types
 * Returns message error code for appropriate error message
 */
ipmipower_msg_type_t ipmipower_packet_errmsg (ipmipower_powercmd_t ip,
					      ipmipower_packet_type_t pkt);

#endif /* IPMIPOWER_PACKETS_H */
