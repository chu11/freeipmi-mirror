/*****************************************************************************\
 *  $Id: ipmipower_util.h,v 1.23 2010-02-08 22:02:31 chu11 Exp $
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

#ifndef IPMIPOWER_UTIL_H
#define IPMIPOWER_UTIL_H

#include <sys/poll.h>

#include "ipmipower.h"

#include "cbuf.h"

char *ipmipower_power_cmd_to_string (ipmipower_power_cmd_t cmd);

/* ipmipower_power_cmd_check_privilege
 * - check if privilege level ok for power cmd
 * - returns 1 if ok, 0 if not, -1 on error
 * - if returns 0 or -1, error message in buf 
 */
int ipmipower_power_cmd_check_privilege (ipmipower_power_cmd_t cmd,
					 char *errbuf,
					 unsigned int errbuflen);

/* ipmipower_poll
 * - safe poll()
 * - Returns number of fds
 */
int ipmipower_poll (struct pollfd *ufds, unsigned int nfds, int timeout);

/* ipmipower_cbuf_printf
 * - wrapper for vsnprintf and cbuf_write
 */
void ipmipower_cbuf_printf(cbuf_t cbuf, const char *fmt, ...);

/* ipmipower_cbuf_peek_and_drop
 * - wrapper for cbuf_peek and cbuf_drop
 * - will drop remaining data in cbuf if buffer not large enough
 * Returns length of packet received, 0 if no packet seen
 */
int ipmipower_cbuf_peek_and_drop (cbuf_t buf, void *buffer, int len);

#endif /* IPMIPOWER_UTIL_H */
