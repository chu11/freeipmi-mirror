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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <arpa/inet.h>
#include <errno.h>

#include "freeipmi/cmds/rmcp-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/rmcp-interface.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_asf_presence_ping =
  {
    { 32, "iana_enterprise_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "message_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "data_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_asf_presence_pong =
  {
    { 32, "iana_enterprise_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "message_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "data_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "oem_iana_enterprise_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "oem_defined", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "supported_entities.version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "supported_entities.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "supported_entities.ipmi_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "supported_interactions.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "supported_interactions.security_extensions", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 48, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

int
fill_cmd_asf_presence_ping (uint8_t message_tag, fiid_obj_t obj_cmd)
{
  if (!fiid_obj_valid (obj_cmd))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_cmd_asf_presence_ping) < 0)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd);
  FILL_FIID_OBJ_SET (obj_cmd, "iana_enterprise_number", htonl (RMCP_ASF_IANA_ENTERPRISE_NUM));
  FILL_FIID_OBJ_SET (obj_cmd, "message_type", RMCP_ASF_MESSAGE_TYPE_PRESENCE_PING);
  FILL_FIID_OBJ_SET (obj_cmd, "message_tag", message_tag);
  FILL_FIID_OBJ_SET (obj_cmd, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd, "data_length", 0x00);
  return (0);
}

