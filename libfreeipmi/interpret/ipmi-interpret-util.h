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

#ifndef IPMI_INTERPRET_UTIL_H
#define IPMI_INTERPRET_UTIL_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"
#include "freeipmi/sel/ipmi-sel.h"

#include "ipmi-interpret-defs.h"

void interpret_set_interpret_errnum_by_errno (ipmi_interpret_ctx_t ctx, int __errno);

void interpret_set_interpret_errnum_by_sel_ctx (ipmi_interpret_ctx_t ctx, ipmi_sel_ctx_t sel_ctx);

#endif /* IPMI_INTERPRET_UTIL_H */
