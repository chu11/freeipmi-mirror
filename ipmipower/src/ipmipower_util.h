/*****************************************************************************\
 *  $Id: ipmipower_util.h,v 1.2.2.2 2006-02-13 18:29:02 chu11 Exp $
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

#ifndef _IPMIPOWER_UTIL_H
#define _IPMIPOWER_UTIL_H

/* 
 * get_rand
 * - something better than rand()
 * 
 */
unsigned int get_rand(void);

/* millisec_add
 * - Add ms milliseconds to old, store new result in new
 */
void millisec_add(struct timeval *old, struct timeval *new, unsigned int ms);

/* millisec_diff
 * Return the difference in terms of milliseconds.  Returns 0
 * if after is less than before.
 */
int millisec_diff(struct timeval *after, struct timeval *before);

/* millisec_gt
 * Return 1 if time1 is greater than time2
 */
int millisec_gt(struct timeval *time1, struct timeval *time2);

/* Secure_malloc
 * "Malloc" non-swappable memory space.
 */
void *Secure_malloc(size_t len);

/* Secure_free
 * "Free" non-swappable memory space.
 */
void Secure_free(void *ptr, size_t len);

#endif /* _IPMIPOWER_UTIL_H */
