/*****************************************************************************\
 *  $Id: error.c,v 1.6 2006-03-07 07:25:59 chu11 Exp $
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
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <syslog.h>
#include <stdio.h>

#include "fd.h"
#include "cbuf.h"
#include "error.h"
#include "hprintf.h"
#include "wrappers.h"

/* achu
 * 
 * Modified to support multiple error outputting types
 */

#define ERR_NONE               0x00
#define ERR_FILE_STREAM        0x01
#define ERR_FILE_DESCRIPTOR    0x02
#define ERR_SYSLOG             0x04
#define ERR_CBUF               0x08

#define ERR_CBUF_DUMP_FILE_STREAM      0x10
#define ERR_CBUF_DUMP_FILE_DESCRIPTOR  0x20

#define ERROR_BUFLEN           1024

/* basename of calling program */
static char  *err_prog = NULL;  

/* error reporting destinations */
static int    err_dest = ERR_NONE;
static FILE  *err_fstream = NULL;
static int    err_fd = -1;
static cbuf_t err_cb = NULL;
static FILE  *err_dump_fstream = NULL;
static int    err_dump_fd = -1;

/* Initialize error module.  'prog' is the name of the calling program
 * and will be the prefix of each error message.  Start logging to stderr.
 */
void err_init(char *prog)
{
    char *p = strrchr(prog, '/');       /* store only the basename */

    err_prog = p ? p + 1 : prog;
    err_none();

    openlog(err_prog, LOG_NDELAY | LOG_PID, LOG_DAEMON);
}

void err_none() 
{
    err_dest = ERR_NONE;
    err_fstream = NULL;
    err_fd = -1;
    err_cb = NULL;
}

void err_syslog(int toggle) 
{
    if (toggle)
        err_dest |= ERR_SYSLOG;
    else
        err_dest &= ~ERR_SYSLOG;
}

void err_file_stream(int toggle, FILE *stream) 
{
    if (toggle) {
        err_dest |= ERR_FILE_STREAM;
        err_fstream = stream; 
    }
    else {
        err_dest &= ~ERR_FILE_STREAM;
        err_fstream = NULL;
    }
}

void err_file_descriptor(int toggle, int fd) 
{
    if (toggle) {
        err_dest |= ERR_FILE_DESCRIPTOR;
        err_fd = fd;
    }
    else {
        err_dest &= ~ERR_FILE_DESCRIPTOR;
        err_fd = -1;
    }
}

void err_cbuf_dump_file_stream(int toggle, FILE *stream) 
{
    if (toggle) {
        err_dest |= ERR_CBUF_DUMP_FILE_STREAM;
        err_dump_fstream = stream; 
    }
    else {
        err_dest &= ~ERR_CBUF_DUMP_FILE_STREAM;
        err_dump_fstream = NULL;
    }
}

void err_cbuf_dump_file_descriptor(int toggle, int fd) 
{
    if (toggle) {
        err_dest |= ERR_CBUF_DUMP_FILE_DESCRIPTOR;
        err_dump_fd = fd;
    }
    else {
        err_dest &= ~ERR_CBUF_DUMP_FILE_DESCRIPTOR;
        err_dump_fd = -1;
    }
}

void err_cbuf(int toggle, cbuf_t buf) 
{
    if (toggle) {
        assert(buf != NULL);
        err_dest |= ERR_CBUF;
        err_cb = buf;
    }
    else {
        err_dest &= ~ERR_CBUF;
        err_cb = NULL;
    }
}

/* Common error/debug handing function */
static void _verr(int syslog_level, const char *fmt, va_list ap)
{
    char buf[ERROR_BUFLEN];
    int len;

    if (err_dest == ERR_NONE)
        return;

    len = vsnprintf(buf, ERROR_BUFLEN, fmt, ap);  /* overflow ignored */
    if (err_dest & ERR_SYSLOG) {
        assert(err_prog != NULL);
        syslog(syslog_level, "%s", buf);
    }
    if (err_dest & ERR_FILE_STREAM) {
        assert(err_prog != NULL);
        assert(err_fstream != NULL);
        fprintf(err_fstream, "%s: %s\n", err_prog, buf);
    }
    if (err_dest & ERR_FILE_DESCRIPTOR) {
        char buf2[ERROR_BUFLEN];
        assert(err_prog != NULL);
        assert(err_fd > -1);
        len = snprintf(buf2, ERROR_BUFLEN, "%s: %s\n", err_prog, buf);
        fd_write_n(err_fd, buf2, len); /* ignore errors */
    }
    if (err_dest & ERR_CBUF) {
        assert(err_prog != NULL);
        assert(err_cb != NULL);
        /* may call malloc - inappropriate for out of mem errs */
        cbuf_printf(err_cb, "%s: %s\n", err_prog, buf);
    }
}

/* Report error message and exit.
 */
void err_exit(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    _verr(LOG_ERR, fmt, ap);
    va_end(ap);

    /* Dump on exit if appropriate.  We're exiting, so if
     * something fails, don't bother.
     */
    if ((err_dest & ERR_CBUF_DUMP_FILE_STREAM) > 0)
      {
        int fd = fileno(err_dump_fstream);
        cbuf_peek_to_fd(err_cb, fd, -1);
      }

    if ((err_dest & ERR_CBUF_DUMP_FILE_DESCRIPTOR) > 0)
      cbuf_peek_to_fd(err_cb, err_dump_fd, -1);

    exit(1);
}

/* Report error message.
 */
void err_output(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    _verr(LOG_ERR, fmt, ap);
    va_end(ap);
}

/* Report debug message.
 */
void dbg(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    _verr(LOG_DEBUG, fmt, ap);
    va_end(ap);
}

/* Error routines for cbuf.c, list.c, wrappers.c
 */
void lsd_fatal_error(char *file, int line, char *mesg)
{
    err_exit("ERROR: [%s::%d] %s: %s", file, line, mesg, strerror(errno));
}

void *lsd_nomem_error(char *file, int line, char *mesg)
{
    err_exit("OUT OF MEMORY: [%s::%d] %s", file, line, mesg);
    /*NOTREACHED*/
    return NULL;
}

void cbuf_printf(cbuf_t cbuf, const char *fmt, ...)
{
    va_list ap;
    char *str;
    int written, dropped;

    va_start(ap, fmt);

    str = hvsprintf(fmt, ap);
    if (str == NULL)
        err_exit("cbuf_printf: out of memory");

    written = cbuf_write(cbuf, str, strlen(str), &dropped);
    if (written < 0)
        err_exit("cbuf_printf: cbuf write: %m");

    Free(str);

    va_end(ap);
}

/*
 * vi:tabstop=4 shiftwidth=4 expandtab
 */
