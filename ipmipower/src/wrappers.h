/*****************************************************************************\
 *  $Id: wrappers.h,v 1.17 2009-06-05 21:58:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  The code in this file began with the code in the Powerman project.
 *  See below for original copyright information.
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
 *****************************************************************************
 *  Copyright (C) 2001-2002 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Andrew Uselton (uselton2@llnl.gov>
 *  UCRL-CODE-2002-008.
 *  
 *  This file is part of PowerMan, a remote power management program.
 *  For details, see <http://www.llnl.gov/linux/powerman/>.
 *  
 *  PowerMan is free software; you can redistribute it and/or modify it under
 *  the terms of the GNU General Public License as published by the Free
 *  Software Foundation; either version 2 of the License, or (at your option)
 *  any later version.
 *  
 *  PowerMan is distributed in the hope that it will be useful, but WITHOUT 
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 *  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with PowerMan; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifndef WRAPPERS_H
#define WRAPPERS_H

#include <sys/types.h>

#include <stdint.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/poll.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */

/*
 * If WITH_LSD_FATAL_ERROR_FUNC is defined, the linker will expect to
 * find an external lsd_fatal_error(file,line,mesg) function.  By default,
 * lsd_fatal_error(file,line,mesg) is a macro definition that outputs an
 * error message to stderr.  This macro may be redefined to invoke another
 * routine instead.
 * 
 * If WITH_LSD_NOMEM_ERROR_FUNC is defined, the linker will expect to
 * find an external lsd_nomem_error(file,line,mesg) function.  By default,
 * lsd_nomem_error(file,line,mesg) is a macro definition that returns NULL.
 * This macro may be redefined to invoke another routine instead.
 *
 * Credit: borrowed from cbuf.c (Chris Dunlap)
 */

/* Wrapper functions (in wrappers.c) */

int Bind(int fd, struct sockaddr_in *addr, socklen_t len);

#define Malloc(size)          wrap_malloc(__FILE__, __LINE__, size)
char *wrap_malloc(char *file, int line, int size);

void Free(void *ptr);
int Close(int fd);

void Gettimeofday(struct timeval *tv, struct timezone *tz);
time_t Time(time_t * t);

/* Recvfrom, Sendto, Poll by achu */

ssize_t Recvfrom(int fd,
                 void *p,
                 size_t len,
                 int flags, 
                 struct sockaddr_in *from,
                 socklen_t *fromlen);

ssize_t Sendto(int fd,
               void *p,
               size_t len,
               int flags, 
               struct sockaddr_in *to,
               socklen_t tolen);

int Poll(struct pollfd *ufds, unsigned int nfds, int timeout);

#endif                          /* WRAPPERS_H */

/*
 * vi:tabstop=4 shiftwidth=4 expandtab
 */
