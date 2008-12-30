/*****************************************************************************\
 *  $Id: ipmi-sel-parse-string.c,v 1.1.2.1 2008-12-30 17:59:15 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-string.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

/*
 * %i - record ID in decimal
 * %t - time in format H:M:S using 24 hour clock
 * %d - date in format D-M-YEAR
 * %g - sensor group name
 * %s - sensor name
 * %e - offset from event/reading code string
 * %f - event data 2 string
 * %h - event data 3 string
 * %j - event direction
 * %m - manufacturer id
 * %o - oem data in hex
 * %% - percent sign
 *
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_OUTPUT_NOT_AVAILABLE
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_USE_SLASH
 * IPMI_SEL_PARSE_READ_STRING_FLAGS_DATE_MONTH_STRING
 */
int
sel_parse_format_record_string(ipmi_sel_parse_ctx_t ctx,
                               char *fmt,
                               uint8_t *record_buf,
                               unsigned int record_buflen,
                               uint8_t *buf,
                               unsigned int buflen,
                               unsigned int flags)
{
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(fmt);
  assert(record_buf);
  assert(record_buflen >= IPMI_SEL_RECORD_LENGTH);
  assert(buf);
  assert(buflen);
  assert(!(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));

  return 0;
}
