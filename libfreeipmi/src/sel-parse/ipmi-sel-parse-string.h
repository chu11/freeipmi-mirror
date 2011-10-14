/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef _IPMI_SEL_PARSE_STRING_H
#define _IPMI_SEL_PARSE_STRING_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#if STDC_HEADERS
#include <stdarg.h>
#endif /* STDC_HEADERS */

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "ipmi-sel-parse-defs.h"

/* returns 0 on success, 1 on success but w/ truncation */
int ipmi_sel_parse_string_snprintf (char *buf,
				    unsigned int buflen,
				    unsigned int *wlen,
				    const char *fmt,
				    ...);

int sel_parse_format_record_string (ipmi_sel_parse_ctx_t ctx,
				    const char *fmt,
				    const void *record_buf,
				    unsigned int record_buflen,
				    char *buf,
				    unsigned int buflen,
				    unsigned int flags);

#endif /* _IPMI_SEL_PARSE_STRING_H */
