/*****************************************************************************\
 *  $Id: debug.c,v 1.9 2010-02-06 01:16:35 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
#if STDC_HEADERS
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmiseld-debug.h"

#include "freeipmi-portability.h"

char *
ipmiseld_debug_msg_create (const char *fmt, ...)
{
  char *buffer;
  va_list ap;

  assert (fmt);

  if (!(buffer = malloc (IPMISELD_DEBUG_BUFFER_LEN)))
    return NULL;

  va_start (ap, fmt);
  vsnprintf (buffer, IPMISELD_DEBUG_BUFFER_LEN, fmt, ap);
  va_end (ap);

  return (buffer);
}
