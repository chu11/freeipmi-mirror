/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _TOOL_COMMON_H
#define _TOOL_COMMON_H

#include "freeipmi/api/ipmi-api.h"
#include "tool-cmdline-common.h"

#define IPMI_OPEN_ERRMSGLEN 1024

int ipmi_is_root ();

void ipmi_disable_coredump(void);

ipmi_ctx_t ipmi_open(const char *progname,
                     const char *hostname,
                     struct common_cmd_args *cmd_args,
                     char *errmsg,
                     unsigned int errmsglen);

/* Check if kg len is decent */
int check_kg_len(const char *instr);

/* Turn an input string into a 20-byte binary k_g key */
int parse_kg(unsigned char *outbuf, int outsz, const char *instr);

/* Turn a 20-byte binary k_g key into an output string */
char *format_kg(char *outstr, int outsz, const unsigned char *k_g);

#endif
