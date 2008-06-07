/*****************************************************************************\
 *  $Id: wrappers.c,v 1.18 2008-06-07 16:09:58 chu11 Exp $
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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <stdint.h>
#include <sys/socket.h>
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif /* HAVE_SYS_WAIT_H */
#include <sys/poll.h>
#include <limits.h>

#include "freeipmi-portability.h"
#include "error.h"

#include "wrappers.h"

#define MAX_REG_BUF 64000

#ifndef NDEBUG
static int memory_alloc = 0;
#endif

/*********************
 *  lsd_fatal_error  *
 *********************/
/* Credit: borrowed from cbuf.c (Chris Dunlap) */

#ifdef WITH_LSD_FATAL_ERROR_FUNC
#  undef lsd_fatal_error
   extern void lsd_fatal_error(char *file, int line, char *mesg);
#else /* !WITH_LSD_FATAL_ERROR_FUNC */
#  ifndef lsd_fatal_error
#    include <errno.h>
#    include <stdio.h>
#    include <string.h>
#    define lsd_fatal_error(file, line, mesg)                                 \
          do {                                                                \
             fprintf(stderr, "ERROR: [%s:%d] %s: %s\n",                       \
                   file, line, mesg, strerror(errno));                        \
          } while (0)
#  endif /* !lsd_fatal_error */
#endif /* !WITH_LSD_FATAL_ERROR_FUNC */

/*********************
 *  lsd_nomem_error  *
 *********************/
/* Credit: borrowed from cbuf.c (Chris Dunlap) */

#ifdef WITH_LSD_NOMEM_ERROR_FUNC
#  undef lsd_nomem_error
   extern void * lsd_nomem_error(char *file, int line, char *mesg);
#else /* !WITH_LSD_NOMEM_ERROR_FUNC */
#  ifndef lsd_nomem_error
#    define lsd_nomem_error(file, line, mesg) (NULL)
#  endif /* !lsd_nomem_error */
#endif /* !WITH_LSD_NOMEM_ERROR_FUNC */

/*
 *   Taken nearly verbatim from Stevens, "UNIX Network Programming".
 * Some are my own extrapolations of his ideas, but should be
 * immediately obvious.  
 */

int Socket(int family, int type, int protocol)
{
    int fd;

    fd = socket(family, type, protocol);
    if (fd < 0)
        lsd_fatal_error(__FILE__, __LINE__, "socket");
    return fd;
}

int
Setsockopt(int fd, int level, int optname, const void *opt_val,
           socklen_t optlen)
{
    int ret_code;

    ret_code = setsockopt(fd, level, optname, opt_val, optlen);
    if (ret_code < 0)
        lsd_fatal_error(__FILE__, __LINE__, "setsockopt");
    return ret_code;
}

int Bind(int fd, struct sockaddr_in *saddr, socklen_t len)
{
    int ret_code;

    ret_code = bind(fd, (struct sockaddr *) saddr, len);
    if (ret_code < 0)
        lsd_fatal_error(__FILE__, __LINE__, "bind");
    return ret_code;
}

int
Getsockopt(int fd, int level, int optname, void *opt_val,
           socklen_t * optlen)
{
    int ret_code;

    ret_code = getsockopt(fd, level, optname, opt_val, optlen);
    if (ret_code < 0)
        lsd_fatal_error(__FILE__, __LINE__, "getsockopt");
    return ret_code;
}

int Listen(int fd, int backlog)
{
    int ret_code;

    ret_code = listen(fd, backlog);
    if (ret_code < 0)
        lsd_fatal_error(__FILE__, __LINE__, "listen");
    return ret_code;
}

int Fcntl(int fd, int cmd, int arg)
{
    int ret_code;

    ret_code = fcntl(fd, cmd, arg);
    if (ret_code < 0)
        lsd_fatal_error(__FILE__, __LINE__, "fcntl");
    return ret_code;
}

char *Strncpy(char *s1, const char *s2, int len)
{
    char *res = strncpy(s1, s2, len);

    s1[len - 1] = '\0';
    return res;
}

time_t Time(time_t * t)
{
    time_t n;

    n = time(t);
    if (n < 0)
        lsd_fatal_error(__FILE__, __LINE__, "time");
    return n;
}

void Gettimeofday(struct timeval *tv, struct timezone *tz)
{
    if (gettimeofday(tv, tz) < 0)
        lsd_fatal_error(__FILE__, __LINE__, "gettimeofday");
}

static void _clear_sets(fd_set * rset, fd_set * wset, fd_set * eset)
{
    if (rset != NULL)
        FD_ZERO(rset);
    if (wset != NULL)
        FD_ZERO(wset);
    if (eset != NULL)
        FD_ZERO(eset);
}

/*
 * Select wrapper that retries select on EINTR with appropriate timeout
 * adjustments, and Err on any other failures.
 * Can return 0 indicating timeout or a value > 0.
 * NOTE: fd_sets are cleared on timeout.
 */
int
Select(int maxfd, fd_set * rset, fd_set * wset, fd_set * eset,
       struct timeval *tv)
{
    int n;
    struct timeval tv_orig = { 0, };
    struct timeval start, end, delta;

    /* prep for EINTR handling */
    if (tv) {
        tv_orig = *tv;
        Gettimeofday(&start, NULL);
    }
    /* repeat select if interrupted */
    do {
        n = select(maxfd, rset, wset, eset, tv);
        if (n < 0 && errno != EINTR)    /* unrecov error */
            lsd_fatal_error(__FILE__, __LINE__, "select");
        if (n < 0 && tv != NULL) {      /* EINTR - adjust tv */
            Gettimeofday(&end, NULL);
            timersub(&end, &start, &delta);     /* delta = end-start */
            timersub(&tv_orig, &delta, tv);     /* tv = tvsave-delta */
        }
        /*if (n < 0)
            fprintf(stderr, "retrying interrupted select\n");*/
    } while (n < 0);
    /* Main select loop needs the fd_sets cleared on timeout */
    if (n == 0)
        _clear_sets(rset, wset, eset);
    return n;
}

/* select-based usleep(3) */
void 
Usleep(unsigned long usec)
{
    fd_set rset, wset, eset;
    struct timeval tv;

    tv.tv_usec = usec % 1000000;
    tv.tv_sec  = usec / 1000000;

    FD_ZERO(&rset);
    FD_ZERO(&wset);
    FD_ZERO(&eset);
    Select(0, &rset, &wset, &eset, &tv);
}

/* Review: look into dmalloc */
#define MALLOC_MAGIC 0xf00fbaab
#define MALLOC_PAD_SIZE   16
#define MALLOC_PAD_FILL 0x55

static int _checkfill(unsigned char *buf, unsigned char fill, int size)
{
  (void) &_checkfill;    /* Avoid warning "defined but not used" */

    while (size-- > 0)
        if (buf[size] != fill)
            return 0;
    return 1;
}

char *wrap_malloc(char *file, int line, int size)
{
    char *new;
    int *p;

    assert(size > 0 && size <= INT_MAX);
    p = (int *) malloc(2*sizeof(int) + size + MALLOC_PAD_SIZE);
    if (p == NULL)
        return lsd_nomem_error(file, line, "malloc");
    p[0] = MALLOC_MAGIC;                           /* magic cookie */
    p[1] = size;                                   /* store size in buffer */
#ifndef NDEBUG
    memory_alloc += size;
#endif
    new = (char *) &p[2];
    memset(new, 0, size);
    memset(new + size, MALLOC_PAD_FILL, MALLOC_PAD_SIZE);
    return new;
}

char *wrap_realloc(char *file, int line, char *item , int newsize)
{
    char *new;
    int *p = (int *)item - 2;
    int oldsize;

    assert(item != NULL);
    assert(newsize > 0 && newsize <= INT_MAX);
    assert(p[0] == MALLOC_MAGIC);
    oldsize = p[1];
    assert(_checkfill((unsigned char *)(item + oldsize), MALLOC_PAD_FILL, MALLOC_PAD_SIZE));
    p = (int *)realloc(p, 2*sizeof(int) + newsize + MALLOC_PAD_SIZE);
    if (p == NULL)
        return lsd_nomem_error(file, line, "realloc");
    assert(p[0] == MALLOC_MAGIC);
    p[1] = newsize;
#ifndef NDEBUG
    memory_alloc += (newsize - oldsize);
#endif
    new = (char *) &p[2];
    if (newsize > oldsize)
        memset(new + oldsize, 0, newsize - oldsize);
    memset(new + newsize, MALLOC_PAD_FILL, MALLOC_PAD_SIZE);
    return new;
}

void Free(void *ptr)
{
    if (ptr != NULL) {
        int *p = (int *) ptr - 2;
        int size;

        assert(p[0] == MALLOC_MAGIC);   /* magic cookie still there? */
        size = p[1];
        assert(_checkfill((unsigned char *)(ptr + size), MALLOC_PAD_FILL, MALLOC_PAD_SIZE));
        memset(p, 0, 2*sizeof(int) + size + MALLOC_PAD_SIZE);
#ifndef NDEBUG
        memory_alloc -= size;
#endif
        free(p);
    }
}

char *Strdup(const char *str)
{
    char *cpy;

    cpy = Malloc(strlen(str) + 1);

    strcpy(cpy, str);
    return cpy;
}


int Accept(int fd, struct sockaddr_in *addr, socklen_t * addrlen)
{
    int new;

    new = accept(fd, (struct sockaddr *) addr, addrlen);
    if (new < 0) {
        /* 
         *   A client could abort before a ready connection is accepted.  
         *  "The fix for this problem is to:  ...  2.  "Ignore the following 
         *  errors ..."  - Stevens, UNP p424
         */
        if (!((errno == EWOULDBLOCK) ||
              (errno == ECONNABORTED) ||
#ifdef EPROTO
              (errno == EPROTO) ||
#endif
              (errno == EINTR)))
            lsd_fatal_error(__FILE__, __LINE__, "accept");
    }
    return new;
}

int Connect(int fd, struct sockaddr *addr, socklen_t addrlen)
{
    int n;

    n = connect(fd, addr, addrlen);
    if (n < 0) {
        if (errno != EINPROGRESS)
            lsd_fatal_error(__FILE__, __LINE__, "connect");
    }
    return n;
}

int Read(int fd, unsigned char *p, int max)
{
    int n;

    do {
        n = read(fd, p, max);
    } while (n < 0 && errno == EINTR);
    if (n < 0 && errno != EWOULDBLOCK && errno != ECONNRESET)
        lsd_fatal_error(__FILE__, __LINE__, "read");
    return n;
}

int Write(int fd, unsigned char *p, int max)
{
    int n;

    do {
        n = write(fd, p, max);
    } while (n < 0 && errno == EINTR);
    if (n < 0 && errno != EAGAIN && errno != ECONNRESET && errno != EPIPE)
        lsd_fatal_error(__FILE__, __LINE__, "write");
    return n;
}


int Open(char *str, int flags, int mode)
{
    int fd;

    assert(str != NULL);
    fd = open(str, flags, mode);
    if (fd < 0)
        lsd_fatal_error(__FILE__, __LINE__, str);
    return fd;
}

int Close(int fd)
{
    int n;

    n = close(fd);
    if (n < 0)
        lsd_fatal_error(__FILE__, __LINE__, "close");
    return n;
}

int Recvfrom(int fd, unsigned char *p, int len, int flags, 
             struct sockaddr_in *from, socklen_t *fromlen)
{
    int n;

    do {
        n = recvfrom(fd, p, len, flags, (struct sockaddr *)from, fromlen);
    } while (n < 0 && errno == EINTR);
    if (n < 0)
        lsd_fatal_error(__FILE__, __LINE__, "recvfrom");
    return n;
}

int Sendto(int fd, unsigned char *p, int len, int flags,
           struct sockaddr_in *to, socklen_t tolen)
{
    int n;

    do {
        n = sendto(fd, p, len, flags, (struct sockaddr *)to, tolen);
    } while (n < 0 && errno == EINTR);
    if (n < 0)
        lsd_fatal_error(__FILE__, __LINE__, "write");
    return n;
}

/*
 * Poll wrapper that retries poll on EINTR with appropriate timeout
 * adjustments, and Err on any other failures.
 * Can return 0 indicating timeout or a value > 0.
 */
int
Poll(struct pollfd *ufds, unsigned int nfds, int timeout)
{
    int n;
    struct timeval tv, tv_orig = { 0, };
    struct timeval start, end, delta;

    /* prep for EINTR handling */
    if (timeout >= 0) {
        /* Poll uses timeout in milliseconds */
        tv_orig.tv_sec = (long)timeout/1000;
        tv_orig.tv_usec = (timeout % 1000) * 1000;
        Gettimeofday(&start, NULL);
    }
    /* repeat poll if interrupted */
    do {
        n = poll(ufds, nfds, timeout);
        if (n < 0 && errno != EINTR)    /* unrecov error */
            lsd_fatal_error(__FILE__, __LINE__, "poll");
        if (n < 0 && timeout >= 0) {      /* EINTR - adjust timeout */
            Gettimeofday(&end, NULL);
            timersub(&end, &start, &delta);     /* delta = end-start */
            timersub(&tv_orig, &delta, &tv);    /* tv = tvsave-delta */
            timeout = (tv.tv_sec * 1000) + (tv.tv_usec/1000);
        }
    } while (n < 0);

    return n;
}

/*
 * vi:tabstop=4 shiftwidth=4 expandtab
 */
