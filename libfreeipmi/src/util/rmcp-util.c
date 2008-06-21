/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

int8_t
ipmi_rmcp_check_message_tag (fiid_obj_t pong, uint8_t message_tag)
{
  uint64_t val;
  int32_t len;

  ERR_EINVAL (fiid_obj_valid(pong));

  FIID_OBJ_TEMPLATE_COMPARE(pong, tmpl_cmd_asf_presence_pong);

  FIID_OBJ_FIELD_LEN (len, pong, "message_tag");

  ERR_EINVAL (len);

  FIID_OBJ_GET (pong, "message_tag", &val);

  if (message_tag == val)
    return 1;
  else
    return 0;
}

