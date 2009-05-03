/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.

*/
/*****************************************************************************\
 *  $Id: ipmi-fru-parse-common.c,v 1.6 2009-05-03 17:40:36 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>
#include <assert.h>

#include "freeipmi/fru-parse/ipmi-fru-parse.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"

#include "ipmi-fru-parse-common.h"
#include "ipmi-fru-parse-defs.h"
#include "ipmi-fru-parse-trace.h"
#include "ipmi-fru-parse-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

int
ipmi_fru_parse_dump_hex (ipmi_fru_parse_ctx_t ctx,
                         const void *frubuf,
                         unsigned int length_in_bytes,
                         const char *debug_hdr)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (frubuf);
  assert (length_in_bytes);
  assert (debug_hdr);

  if (ctx->flags & IPMI_FRU_PARSE_FLAGS_DEBUG_DUMP)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                     DEBUG_UTIL_DIRECTION_NONE,
                     debug_hdr,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_dump_hex (STDERR_FILENO,
                         ctx->debug_prefix,
                         hdrbuf,
                         NULL,
                         frubuf,
                         length_in_bytes) < 0)
        {
          FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
          return (-1);
        }
    }

  return (0);
}

int
ipmi_fru_parse_dump_obj (ipmi_fru_parse_ctx_t ctx,
                         fiid_obj_t obj,
                         const char *debug_hdr)
{
  assert (ctx);
  assert (ctx->magic == IPMI_FRU_PARSE_CTX_MAGIC);
  assert (obj);
  assert (debug_hdr);

  if (ctx->flags & IPMI_FRU_PARSE_FLAGS_DEBUG_DUMP)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                     DEBUG_UTIL_DIRECTION_NONE,
                     debug_hdr,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_obj_dump (STDERR_FILENO,
                         ctx->debug_prefix,
                         hdrbuf,
                         NULL,
                         obj) < 0)
        {
          FRU_PARSE_ERRNO_TO_FRU_PARSE_ERRNUM (ctx, errno);
          return (-1);
        }
    }

  return (0);
}
