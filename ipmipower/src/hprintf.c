/*****************************************************************************\
 *  $Id: hprintf.c,v 1.1.8.2 2006-02-13 17:59:50 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif
#include <assert.h>

#include "wrappers.h"
#include "hprintf.h"

#define CHUNKSIZE 80
char *hvsprintf(const char *fmt, va_list ap)
{
    int len, size = 0;
    char *str = NULL;

    do {
        str = (size == 0) ? Malloc(CHUNKSIZE) : Realloc(str, size+CHUNKSIZE);
        size += CHUNKSIZE;

        len = vsnprintf(str, size, fmt, ap); /* always null terminates */
    } while (len == -1 || len >= size);
    assert(len == strlen(str));
    return str;
}

char *hsprintf(const char *fmt, ...)
{
    char *str;
    va_list ap;

    va_start(ap, fmt);
    str = hvsprintf(fmt, ap);
    va_end(ap);

    return str;
}

/*
 * vi:tabstop=4 shiftwidth=4 expandtab
 */
