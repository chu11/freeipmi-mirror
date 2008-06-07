/*****************************************************************************\
 *  $Id: ierror.c,v 1.4 2008-06-07 16:09:57 chu11 Exp $
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
#endif /* HAVE_CONFIG_H */

#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <syslog.h>
#include <stdio.h>

#include "fd.h"
#include "cbuf.h"
#include "freeipmi-portability.h"

#include "ierror.h"
#include "wrappers.h"

/* achu
 * 
 * Modified to support multiple error outputting types
 */

#define IERR_NONE               0x00
#define IERR_FILE_STREAM        0x01
#define IERR_FILE_DESCRIPTOR    0x02
#define IERR_SYSLOG             0x04
#define IERR_CBUF               0x08

#define IERR_CBUF_DUMP_FILE_STREAM      0x10

#define IERROR_BUFLEN           4096

/* basename of calling program */
static char  *ierr_prog = NULL;  

/* error reporting destinations */
static int    ierr_dest = IERR_NONE;
static FILE  *ierr_fstream = NULL;
static int    ierr_fd = -1;
static cbuf_t ierr_cb = NULL;
static FILE  *ierr_dump_fstream = NULL;

/* Initialize error module.  'prog' is the name of the calling program
 * and will be the prefix of each error message.  Start logging to stderr.
 */
void ierr_init(char *prog)
{
    char *p = strrchr(prog, '/');       /* store only the basename */

    ierr_prog = p ? p + 1 : prog;
    ierr_none();

    openlog(ierr_prog, LOG_NDELAY | LOG_PID, LOG_DAEMON);
}

void ierr_none() 
{
    ierr_dest = IERR_NONE;
    ierr_fstream = NULL;
    ierr_fd = -1;
    ierr_cb = NULL;
}

void ierr_syslog(int toggle) 
{
    if (toggle)
        ierr_dest |= IERR_SYSLOG;
    else
        ierr_dest &= ~IERR_SYSLOG;
}

void ierr_file_stream(int toggle, FILE *stream) 
{
    if (toggle) {
        ierr_dest |= IERR_FILE_STREAM;
        ierr_fstream = stream; 
    }
    else {
        ierr_dest &= ~IERR_FILE_STREAM;
        ierr_fstream = NULL;
    }
}

void ierr_file_descriptor(int toggle, int fd) 
{
    if (toggle) {
        ierr_dest |= IERR_FILE_DESCRIPTOR;
        ierr_fd = fd;
    }
    else {
        ierr_dest &= ~IERR_FILE_DESCRIPTOR;
        ierr_fd = -1;
    }
}

void ierr_cbuf_dump_file_stream(int toggle, FILE *stream) 
{
    if (toggle) {
        ierr_dest |= IERR_CBUF_DUMP_FILE_STREAM;
        ierr_dump_fstream = stream; 
    }
    else {
        ierr_dest &= ~IERR_CBUF_DUMP_FILE_STREAM;
        ierr_dump_fstream = NULL;
    }
}

void ierr_cbuf(int toggle, cbuf_t buf) 
{
    if (toggle) {
        assert(buf);
        ierr_dest |= IERR_CBUF;
        ierr_cb = buf;
    }
    else {
        ierr_dest &= ~IERR_CBUF;
        ierr_cb = NULL;
    }
}

/* Common error/debug handing function */
static void _verr(int syslog_level, const char *fmt, va_list ap)
{
    char buf[IERROR_BUFLEN];
    int len;

    if (ierr_dest == IERR_NONE)
        return;

    len = vsnprintf(buf, IERROR_BUFLEN, fmt, ap);  /* overflow ignored */
    if (ierr_dest & IERR_SYSLOG) {
        assert(ierr_prog);
        syslog(syslog_level, "%s", buf);
    }
    if (ierr_dest & IERR_FILE_STREAM) {
        assert(ierr_prog);
        assert(ierr_fstream);
        fprintf(ierr_fstream, "%s: %s\n", ierr_prog, buf);
    }
    if (ierr_dest & IERR_FILE_DESCRIPTOR) {
        char buf2[IERROR_BUFLEN];
        assert(ierr_prog);
        assert(ierr_fd > -1);
        len = snprintf(buf2, IERROR_BUFLEN, "%s: %s\n", ierr_prog, buf);
        fd_write_n(ierr_fd, buf2, len); /* ignore errors */
    }
    if (ierr_dest & IERR_CBUF) {
        assert(ierr_prog);
        assert(ierr_cb);
        /* may call malloc - inappropriate for out of mem errs */
        cbuf_printf(ierr_cb, "%s: %s\n", ierr_prog, buf);
    }
}

/* Report error message and exit.
 */
void ierr_exit(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    _verr(LOG_ERR, fmt, ap);
    va_end(ap);

    /* Dump on exit if appropriate.  We're exiting, so if
     * something fails, don't bother.
     */
    if ((ierr_dest & IERR_CBUF_DUMP_FILE_STREAM) > 0)
      {
        int fd = fileno(ierr_dump_fstream);
        cbuf_peek_to_fd(ierr_cb, fd, -1);
      }

    exit(1);
}

/* Report error message.
 */
void ierr_output(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    _verr(LOG_ERR, fmt, ap);
    va_end(ap);
}

/* Report debug message.
 */
void ierr_dbg(const char *fmt, ...)
{
#ifndef NDEBUG
    va_list ap;

    va_start(ap, fmt);
    _verr(LOG_DEBUG, fmt, ap);
    va_end(ap);
#endif /* !NDEBUG */
}

/* Error routines for cbuf.c, list.c, wrappers.c
 */
void lsd_fatal_error(char *file, int line, char *mesg)
{
    ierr_exit("ERROR: [%s::%d] %s: %s", file, line, mesg, strerror(errno));
}

void *lsd_nomem_error(char *file, int line, char *mesg)
{
    ierr_exit("OUT OF MEMORY: [%s::%d] %s", file, line, mesg);
    /*NOTREACHED*/
    return NULL;
}

void cbuf_printf(cbuf_t cbuf, const char *fmt, ...)
{
    char buf[IERROR_BUFLEN];
    va_list ap;
    int written, dropped;
    int len;

    if (ierr_dest == IERR_NONE)
        return;

    va_start(ap, fmt);
    len = vsnprintf(buf, IERROR_BUFLEN, fmt, ap);  /* overflow ignored */

    written = cbuf_write(cbuf, buf, len, &dropped);
    if (written < 0)
        ierr_exit("cbuf_printf: cbuf write: %m");

    va_end(ap);
}

/*
 * vi:tabstop=4 shiftwidth=4 expandtab
 */
