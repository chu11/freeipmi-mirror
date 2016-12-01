/*****************************************************************************\
 *  $Id: hostlist.h,v 1.3 2009-12-16 17:49:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
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

/* This library is a wrapper around hostlist to handle some special
 * case parsing in FreeIPMI.
 *
 * For instructions on usage, see hostlist.h equivalent functions.
 */

#ifndef _FI_HOSTLIST_H
#define _FI_HOSTLIST_H

typedef struct fi_hostlist * fi_hostlist_t;

typedef struct fi_hostlist_iterator * fi_hostlist_iterator_t;

fi_hostlist_t fi_hostlist_create (const char *hostlist);

fi_hostlist_t fi_hostlist_copy (const fi_hostlist_t fihl);

void fi_hostlist_destroy (fi_hostlist_t fihl);

int fi_hostlist_push (fi_hostlist_t fihl, const char *hosts);

int fi_hostlist_push_host (fi_hostlist_t fihl, const char *host);

int fi_hostlist_push_list (fi_hostlist_t fihl1, fi_hostlist_t fihl2);

int fi_hostlist_find (fi_hostlist_t fihl, const char *hostname);

int fi_hostlist_delete (fi_hostlist_t fihl, const char *hosts);

int fi_hostlist_delete_host (fi_hostlist_t fihl, const char *hostname);

int fi_hostlist_count (fi_hostlist_t fihl);

#define fi_hostlist_is_empty(__fihl) ( fi_hostlist_count(__fihl) == 0 )

void fi_hostlist_sort (fi_hostlist_t fihl);

void fi_hostlist_uniq (fi_hostlist_t fihl);

ssize_t fi_hostlist_ranged_string (fi_hostlist_t fihl, size_t n, char *buf);

ssize_t fi_hostlist_deranged_string (fi_hostlist_t fihl, size_t n, char *buf);

fi_hostlist_iterator_t fi_hostlist_iterator_create (fi_hostlist_t fihl);

void fi_hostlist_iterator_destroy (fi_hostlist_iterator_t fiitr);

void fi_hostlist_iterator_reset (fi_hostlist_iterator_t fiitr);

char * fi_hostlist_next (fi_hostlist_iterator_t fiitr);

int fi_hostlist_remove (fi_hostlist_iterator_t fiitr);

#endif /* !_FI_HOSTLIST_H */
