/*****************************************************************************\
 *  $Id: wrappers.h,v 1.2.2.1 2005-12-20 19:05:00 chu11 Exp $
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
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#ifndef WRAPPERS_H
#define WRAPPERS_H

#ifdef __FreeBSD__
#include <stdint.h>
#endif
#include <regex.h>
#include <netdb.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/poll.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

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

int Socket(int family, int type, int protocol);
int Setsockopt(int fd, int level, int optname, const void *opt_val,
               socklen_t optlen);
int Bind(int fd, struct sockaddr_in *addr, socklen_t len);
int Getsockopt(int fd, int level, int optname, void *opt_val,
               socklen_t * optlen);
int Listen(int fd, int backlog);
int Fcntl(int fd, int cmd, int arg);
int Select(int maxfd, fd_set * rset, fd_set * wset, fd_set * eset,
           struct timeval *tv);
void Delay(struct timeval *tv);

#define Malloc(size)          wrap_malloc(__FILE__, __LINE__, size)
#define Realloc(item,newsize) wrap_realloc(__FILE__, __LINE__, item, newsize)
char *wrap_malloc(char *file, int line, int size);
char *wrap_realloc(char *file, int line, char *item, int newsize);

void Free(void *ptr);
char *Strdup(const char *str);
int Accept(int fd, struct sockaddr_in *addr, socklen_t * addrlen);
int Connect(int fd, struct sockaddr *addr, socklen_t addrlen);
int Read(int fd, unsigned char *p, int max);
int Write(int fd, unsigned char *p, int max);
int Open(char *str, int flags, int mode);
int Close(int fd);
int Getaddrinfo(char *host, char *service, struct addrinfo *hints,
                struct addrinfo **addrinfo);
void Regcomp(regex_t * preg, const char *regex, int cflags);
int Regexec(const regex_t * preg, const char *string,
            size_t nmatch, regmatch_t pmatch[], int eflags);
pid_t Fork(void);
typedef void Sigfunc(int);
Sigfunc *Signal(int signo, Sigfunc * func);
int Memory(void);
void Gettimeofday(struct timeval *tv, struct timezone *tz);
time_t Time(time_t * t);
char *Strncpy(char *s1, const char *s2, int len);
void Usleep(unsigned long usec);
void Pipe(int filedes[2]);
void Dup2(int oldfd, int newfd);
void Execv(const char *path, char *const argv[]);
pid_t Waitpid(pid_t pid, int *status, int options);

/* Recvfrom, Sendto, Poll by achu */

int Recvfrom(int fd, unsigned char *p, int len, int flags, 
             struct sockaddr_in *from, socklen_t *fromlen);
int Sendto(int fd, unsigned char *p, int len, int flags, 
           struct sockaddr_in *to, socklen_t tolen);

int Poll(struct pollfd *ufds, unsigned int nfds, int timeout);

#endif                          /* WRAPPERS_H */

/*
 * vi:tabstop=4 shiftwidth=4 expandtab
 */
