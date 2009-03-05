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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "tool-fiid-util.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

int
tool_fiid_obj_get (pstdout_state_t pstate,
                   fiid_obj_t obj,
                   char *field,
                   uint64_t *val)
{
  uint64_t lval;
  int ret;

  assert (obj);
  assert (fiid_obj_valid(obj));
  assert (field);
  assert (val);

  if ((ret = fiid_obj_get (obj, field, &lval)) < 0)
    {
      pstdout_fprintf (pstate, 
                       stderr,
                       "fiid_obj_get: '%s': %s\n",
                       field,
                       fiid_obj_errormsg (obj));
      return (-1);
    }

  if (!ret)
    {
      pstdout_fprintf (pstate,
                       stderr,
                       "fiid_obj_get: '%s': not data set\n",
                       field);
      return (-1);
    }

  *val = lval;
  return (1);                   /* return 1 like real call */
}
