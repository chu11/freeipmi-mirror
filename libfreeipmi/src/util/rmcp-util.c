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
#include <errno.h>

#include "freeipmi/util/rmcp-util.h"
#include "freeipmi/cmds/rmcp-cmds.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

int
ipmi_rmcp_check_message_tag (fiid_obj_t pong, uint8_t message_tag)
{
  uint8_t l_message_tag;
  uint64_t val;

  if (!fiid_obj_valid (pong))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (pong, tmpl_cmd_asf_presence_pong) < 0)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_GET (pong, "message_tag", &val) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  l_message_tag = val;

  if (message_tag == l_message_tag)
    return (1);

  return (0);
}

