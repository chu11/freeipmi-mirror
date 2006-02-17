/*****************************************************************************\
 *  $Id: ipmipower_ping.h,v 1.4 2006-02-17 19:34:34 chu11 Exp $
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

#ifndef _IPMIPOWER_PING_H
#define _IPMIPOWER_PING_H

#include "ipmipower.h"

/* ipmipower_ping_force_discovery_sweep
 * - Force a sweep of discovery when a new set of hostnames are set
 */
void ipmipower_ping_force_discovery_sweep();

/* ipmipower_ping_process
 * - Process pings
 */
void ipmipower_ping_process_pings(int *timeout);

#endif /* _IPMIPOWER_PING_H */
