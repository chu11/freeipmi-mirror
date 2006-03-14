/*****************************************************************************\
 *  $Id: ipmipower_cipher_suite.h,v 1.1 2006-03-14 14:58:26 chu11 Exp $
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


#ifndef _IPMIPOWER_CIPHER_SUITE_H
#define _IPMIPOWER_CIPHER_SUITE_H

#include "ipmipower.h"

/* ipmipower_cipher_suite_id_index
 * - Return the index of the specified cipher_suite_id 
 */
cipher_suite_id_t ipmipower_cipher_suite_id_index(char *str);

/* ipmipower_cipher_suite_id_string
 * - Return static string description of cipher_suite_id 
 */
char *ipmipower_cipher_suite_id_string(cipher_suite_id_t id);

/* ipmipower_cipher_suite_id_description
 * - Return full static string description of cipher_suite_id 
 */
char *ipmipower_cipher_suite_id_description(cipher_suite_id_t id);

/* ipmipower_cipher_suite_id_list
 * - Return static string list of all available cipher_suite types
 */
char *ipmipower_cipher_suite_id_list(void);

/* ipmipower_ipmi_cipher_suite_id
 * - Return IPMI cipher_suite type according to cipher_suite_id_t type
 */
uint8_t ipmipower_ipmi_cipher_suite_id(cipher_suite_id_t id);

#endif /* _IPMIPOWER_CIPHER_SUITE_H */
