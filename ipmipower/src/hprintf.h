/*****************************************************************************\
 *  $Id: hprintf.h,v 1.1.6.2 2006-02-13 18:29:02 chu11 Exp $
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

#ifndef _HPRINTF_H
#define _HPRINTF_H

#include <stdarg.h>

/* A vsprintf-like function that allocates the string on the heap and ensures
 * that no truncation occurs.  The caller must Free() the resulting string.
 */
char *hvsprintf(const char *fmt, va_list ap);

/* An sprintf-like function that allocates the string on the heap.
 * The caller must Free() the resulting string.
 */
char *hsprintf(const char *fmt, ...);

#endif

/*
 * vi:tabstop=4 shiftwidth=4 expandtab
 */
