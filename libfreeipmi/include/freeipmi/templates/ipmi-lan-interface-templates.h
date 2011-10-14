/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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

#ifndef _IPMI_LAN_INTERFACE_TEMPLATES_H
#define _IPMI_LAN_INTERFACE_TEMPLATES_H

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

LAN Session Header
------------------

fiid_template_t tmpl_lan_session_hdr =
  {
    { 8, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "ipmi_msg_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };
 
LAN Message Header Request
--------------------------

fiid_template_t tmpl_lan_msg_hdr_rq =
  {
    { 8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

LAN Message Header Response
---------------------------

fiid_template_t tmpl_lan_msg_hdr_rs =
  {
    { 8, "rq_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rq_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "checksum1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "rq_seq", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

LAN Message Trailer
-------------------

fiid_template_t tmpl_lan_msg_trlr =
  {
    { 8, "checksum2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_LAN_INTERFACE_TEMPLATES_H */
