/*
argp-common.c: common work for argp for all freeipmi tools.
Copyright (C) 2005 FreeIPMI Core Team

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

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <argp.h>

#include "freeipmi.h"
#include "ipmi-common.h"

int
ipmi_is_root ()
{
  uid_t uid = getuid ();
  if (uid == 0)
    return 1;
  return 0;
}

void
ipmi_error (fiid_obj_t obj_cmd, const char *s)
{
  char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };

  if (obj_cmd == NULL)
    return;

  ipmi_strerror_cmd_r (obj_cmd, errmsg, IPMI_ERR_STR_MAX_LEN);

  fprintf (stderr,
           "%s%s" "ipmi command %02Xh: %s\n",
           (s ? s : ""),
           (s ? ": " : ""),
           obj_cmd[0],
           errmsg);
}
