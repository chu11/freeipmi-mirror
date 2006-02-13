/*****************************************************************************\
 *  $Id: ipmipower_privilege.h,v 1.2.2.2 2006-02-13 18:29:02 chu11 Exp $
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


#ifndef _IPMIPOWER_PRIVILEGE_H
#define _IPMIPOWER_PRIVILEGE_H

#include "ipmipower.h"

/* ipmipower_privilege_index
 * - Return the index of the specified privilege type 
 */
privilege_type_t ipmipower_privilege_index(char *str);

/* ipmipower_privilege_string
 * - Return static string description of privilege type 
 */
char *ipmipower_privilege_string(privilege_type_t priv);

/* ipmipower_privilege_list
 * - Return static string list of all available privilege types
 */
char *ipmipower_privilege_list(void);

/* ipmipower_ipmi_privilege_type
 * - Return IPMI privilege type according to privilege_type_t type
 */
uint8_t ipmipower_ipmi_privilege_type(privilege_type_t priv);

#endif /* _IPMIPOWER_PRIVILEGE_H */
