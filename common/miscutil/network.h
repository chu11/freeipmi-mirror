/*****************************************************************************\
 *  $Id: hostlist.h,v 1.3 2009-12-16 17:49:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
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

/* Convenience network functions */

#ifndef _NETWORK_H
#define _NETWORK_H

#include <stdint.h>

/* Convenience functions */

/* Determine if host is in "[Ipv6 address]:port" format
 *
 * If true and addr and/or port are non-NULL, set addr and port
 * appropriately.  User is required to free addr & port appropriately.
 */
int host_is_ipv6_with_port (const char *host, char **addr, char **port);

/* Determine if host is in hostname & port format, will check for IPv6
 * format [Ipv6 address]:port or normal Ipv4:port.
 *
 * If true and addr and/or port are non-NULL, set addr and port
 * appropriately.  User is required to free addr & port appropriately.
 */
int host_is_host_with_port (const char *host, char **addr, char **port);

/* Determine if strings returned from host_is_host_with_port() are
 * valid.  'addr' is required to be passed in, 'port' is optional.  If
 * 'port' passed in and 'portptr' not NULL, set port value in
 * 'portptr'.  Returns 1 if valid, 0 if not, -1 on error.
 */
int host_is_valid (const char *addr, const char *port, uint16_t *portptr);

/* Determine if hostname is a "localhost" or equivalent string,
 * returns 1 for yes, 0 for no.  No host resolution will be done, only
 * string matching for common strings.
 */
int host_is_localhost (const char *host);

#endif /* !_NETWORK_H */
