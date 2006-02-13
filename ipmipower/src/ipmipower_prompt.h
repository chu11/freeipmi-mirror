/*****************************************************************************\
 *  $Id: ipmipower_prompt.h,v 1.3.2.2 2006-02-13 22:21:16 chu11 Exp $
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

#ifndef _IPMIPOWER_PROMPT_H
#define _IPMIPOWER_PROMPT_H

#include "ipmipower.h"

/* ipmipower_prompt_process_cmdline
 * - Process a single line
 * - Do nothing if power commands still pending and processing
 * Returns 1 if we should continue, 0 if we should quit
 */
int ipmipower_prompt_process_cmdline(void);

#endif /* _IPMIPOWER_PROMPT_H */
