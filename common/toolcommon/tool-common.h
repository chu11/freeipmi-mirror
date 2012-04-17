/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef TOOL_COMMON_H
#define TOOL_COMMON_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "parse-common.h"

#define IPMI_OPEN_ERRMSGLEN 1024

int ipmi_is_root ();

void ipmi_disable_coredump (void);

ipmi_ctx_t ipmi_open (const char *progname,
                      const char *hostname,
                      struct common_cmd_args *cmd_args,
                      char *errmsg,
                      unsigned int errmsglen);

/* Check if kg len is decent */
int check_kg_len (const char *in);

/* Turn a 20-byte binary k_g key into an output string */
char *format_kg (char *out, unsigned int outlen, const void *k_g);

#endif /* TOOL_COMMON_H */
