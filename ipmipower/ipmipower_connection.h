/*****************************************************************************\
 *  $Id: ipmipower_connection.h,v 1.17 2010-02-08 22:02:31 chu11 Exp $
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

#ifndef IPMIPOWER_CONNECTION_H
#define IPMIPOWER_CONNECTION_H

#include "ipmipower.h"

/* ipmipower_connection_clear
 * - clear per-session data from ipmi_connection structure
 */
void ipmipower_connection_clear (ipmipower_connection_t ic);

/* ipmipower_connection_array_create
 * - Create ipmipower_connection array
 * - Returns pointer on success, NULL on error.
 */
struct ipmipower_connection *ipmipower_connection_array_create (const char *hostname,
                                                                unsigned int *len);

/* ipmipower_connection_array_destroy
 * - Destroy ipmipower_connection array
 */
void ipmipower_connection_array_destroy (struct ipmipower_connection *ics,
                                         unsigned int ics_len);

/* ipmipower_connection_hostname_index
 * - Find ics entry with given hostname
 * - Returns index of entry, -1 if not found
 */
int ipmipower_connection_hostname_index (struct ipmipower_connection *ics,
                                         unsigned int ics_len,
                                         const char *hostname);

#endif /* IPMIPOWER_CONNECTION_H */
