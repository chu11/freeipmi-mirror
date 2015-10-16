/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifndef IPMI_SEL_STRING_H
#define IPMI_SEL_STRING_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#if STDC_HEADERS
#include <stdarg.h>
#endif /* STDC_HEADERS */

#include "freeipmi/sel/ipmi-sel.h"

#include "ipmi-sel-defs.h"

/* returns 0 on success, 1 on success but w/ truncation */
int sel_string_snprintf (char *buf,
			 unsigned int buflen,
			 unsigned int *wlen,
			 const char *fmt,
			 ...);

/* returns 0 on success, 1 on success but w/ truncation
 * 
 * Just like sel_string_snprintf, but just appends and does non-zero check beforehand
 */
int sel_string_strcat_comma_separate (char *buf,
				      unsigned int buflen,
				      unsigned int *wlen,
				      const char *str);

int sel_format_record_string (ipmi_sel_ctx_t ctx,
			      const char *fmt,
			      const void *sel_record,
			      unsigned int sel_record_len,
			      char *buf,
			      unsigned int buflen,
			      unsigned int flags);

#endif /* IPMI_SEL_STRING_H */
