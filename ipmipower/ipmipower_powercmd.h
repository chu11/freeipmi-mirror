/*****************************************************************************\
 *  $Id: ipmipower_powercmd.h,v 1.17 2010-02-08 22:02:31 chu11 Exp $
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

#ifndef IPMIPOWER_POWERCMD_H
#define IPMIPOWER_POWERCMD_H

#include "ipmipower.h"

void ipmipower_powercmd_setup ();

void ipmipower_powercmd_cleanup ();

void ipmipower_powercmd_queue (ipmipower_power_cmd_t cmd,
			       struct ipmipower_connection *ic,
			       const char *extra_arg);

/* ipmipower_powercmd_pending
 * - Determines if any commands are still pending
 * Returns 1 if commands are still being executed, 0 if not
 */
int ipmipower_powercmd_pending ();

/* ipmipower_powercmd_process_pending
 * - Process remaining commands still in the queue
 * - Sets timeout to min timeout of all pending requests
 * - Does not set timeout if no pending requests exist
 * Returns number of pending requests, 0 if none
 */
int ipmipower_powercmd_process_pending (int *timeout);

#endif /* IPMIPOWER_POWERCMD_H */
