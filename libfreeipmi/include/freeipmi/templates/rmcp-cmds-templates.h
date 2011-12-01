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

#ifndef _RMCP_CMDS_TEMPLATES_H
#define _RMCP_CMDS_TEMPLATES_H

#ifdef __cplusplus
extern "C" {
#endif

/* This header file is for documentation only */

#if 0

Format = { bits, "field name", field flags }

FIID_FIELD_REQUIRED - field is required for the payload
FIID_FIELD_OPTIONAL - field is optional for the payload

FIID_FIELD_LENGTH_FIXED - field length is fixed at the number of bits listed
FIID_FIELD_LENGTH_VARIABLE - field length is variable for the number of bits listed

ASF Presence Ping
-----------------

fiid_template_t tmpl_cmd_asf_presence_ping =
  {
    { 32, "iana_enterprise_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "message_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "data_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

ASF Presence Pong
-----------------

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

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _RMCP_CMDS_TEMPLATES_H */
