/*****************************************************************************\
 *  $Id: ipmipower_authentication_type.h,v 1.6 2007-10-18 16:18:51 chu11 Exp $
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


#ifndef _IPMIPOWER_AUTHENTICATION_TYPE_H
#define _IPMIPOWER_AUTHENTICATION_TYPE_H

#include "ipmipower.h"

/* ipmipower_authentication_type_index
 * - Return the index of the specified authentication_type 
 */
authentication_type_t ipmipower_authentication_type_index(char *str);

/* ipmipower_authentication_type_string
 * - Return static string description of authentication_type 
 */
char *ipmipower_authentication_type_string(authentication_type_t at);

/* ipmipower_authentication_type_list
 * - Return static string list of all available authentication types
 */
char *ipmipower_authentication_type_list(void);

/* ipmipower_ipmi_authentication_type
 * - Return IPMI authentication type according to authentication_type_t type
 */
uint8_t ipmipower_ipmi_authentication_type(authentication_type_t at);

#endif /* _IPMIPOWER_AUTHENTICATION_TYPE_H */
