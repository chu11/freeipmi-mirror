/*****************************************************************************\
 *  $Id: ipmipower_workarounds.h,v 1.5 2007-10-18 16:18:53 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 2 of the License, or (at your 
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

#ifndef _IPMIPOWER_WORKAROUNDS_H
#define _IPMIPOWER_WORKAROUNDS_H

#include "ipmipower.h"

/* ipmipower_workarounds_parse
 * - Parse string of workarounds
 */
int ipmipower_workarounds_parse(char *str, uint32_t *workaround_flags);

/* ipmipower_workarounds_string
 * - Return string of workarounds stored in a static buffer.
 */
char *ipmipower_workarounds_string(uint32_t workaround_flags);

/* ipmipower_workarounds_list
 * - Return static string list of all available workarounds types
 */
char *ipmipower_workarounds_list(void); 


#endif /* _IPMIPOWER_WORKAROUNDS_H */
