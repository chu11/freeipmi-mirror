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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/cmds/rmcp-cmds.h"
#include "freeipmi/interface/rmcp-interface.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_asf_presence_ping = 
  {
    {32, "iana_enterprise_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "message_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "data_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_asf_presence_pong =
  {
    {32, "iana_enterprise_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "message_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "data_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "oem_iana_enterprise_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "oem_defined", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "supported_entities.version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "supported_entities.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "supported_entities.ipmi_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "supported_interactions.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "supported_interactions.security_extensions", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {48, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0,  "", 0}
  };

int8_t
fill_cmd_asf_presence_ping(uint8_t message_tag, fiid_obj_t obj_cmd)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_cmd_asf_presence_ping);

  FIID_OBJ_CLEAR (obj_cmd);
  FIID_OBJ_SET (obj_cmd, "iana_enterprise_number", htonl(RMCP_ASF_IANA_ENTERPRISE_NUM));
  FIID_OBJ_SET (obj_cmd, "message_type", RMCP_ASF_MESSAGE_TYPE_PRESENCE_PING);
  FIID_OBJ_SET (obj_cmd, "message_tag", message_tag);
  FIID_OBJ_SET (obj_cmd, "reserved", 0);
  FIID_OBJ_SET (obj_cmd, "data_length", 0x00);
  return 0;
}

