/*****************************************************************************\
 *  $Id: ipmipower_privilege_level.h,v 1.7 2008-03-28 00:14:48 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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


#ifndef _IPMIPOWER_PRIVILEGE_LEVEL_H
#define _IPMIPOWER_PRIVILEGE_LEVEL_H

#include "ipmipower.h"

/* ipmipower_privilege_index
 * - Return the index of the specified privilege type 
 */
privilege_level_t ipmipower_privilege_level_index(char *str);

/* ipmipower_privilege_string
 * - Return static string description of privilege type 
 */
char *ipmipower_privilege_level_string(privilege_level_t priv);

/* ipmipower_privilege_list
 * - Return static string list of all available privilege types
 */
char *ipmipower_privilege_level_list(void);

/* ipmipower_ipmi_privilege_level
 * - Return IPMI privilege type according to privilege_level_t type
 */
uint8_t ipmipower_ipmi_privilege_level(privilege_level_t priv);

#endif /* _IPMIPOWER_PRIVILEGE_LEVEL_H */
