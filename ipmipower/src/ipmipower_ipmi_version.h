/*****************************************************************************\
 *  $Id: ipmipower_ipmi_version.h,v 1.4 2007-10-18 00:33:13 chu11 Exp $
 *****************************************************************************
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


#ifndef _IPMIPOWER_IPMI_VERSION_H
#define _IPMIPOWER_IPMI_VERSION_H

#include "ipmipower.h"

/* ipmipower_ipmi_version_index
 * - Return the index of the specified ipmi_version 
 */
ipmi_version_t ipmipower_ipmi_version_index(char *str);

/* ipmipower_ipmi_version_string
 * - Return static string description of ipmi_version 
 */
char *ipmipower_ipmi_version_string(ipmi_version_t ipmi_version);

/* ipmipower_ipmi_version_list
 * - Return static string list of all available ipmi versions
 */
char *ipmipower_ipmi_version_list(void);

#endif /* _IPMIPOWER_IPMI_VERSION_H */
