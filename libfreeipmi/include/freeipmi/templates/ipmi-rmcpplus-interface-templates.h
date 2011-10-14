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

#ifndef _IPMI_RMCPPLUS_INTERFACE_TEMPLATES_H
#define _IPMI_RMCPPLUS_INTERFACE_TEMPLATES_H

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

RMCP+ Session Header
--------------------

fiid_template_t tmpl_rmcpplus_session_hdr =
  {
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type.authenticated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type.encrypted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "oem_payload_id", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h outside of a session, seperate #'s if authenticated or unauthenticated session */
    { 32, "session_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* length of just the payload */
    { 16, "ipmi_payload_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

RMCP+ Session Trailer
---------------------

/* note: the ipmi spec wording is terrible.  The integrity pad is to
 * ensure that the data passed to the HMAC is a multiple of 4, not
 * just the integrity field.  Sigh ...
 */
fiid_template_t tmpl_rmcpplus_session_trlr =
  {
    { 32, "integrity_pad", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 8, "pad_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "next_header", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 256, "authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

RMCP+ Payload
-------------

fiid_template_t tmpl_rmcpplus_payload =
  {
    { 512, "confidentiality_header", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "payload_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 512, "confidentiality_trailer", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

RMCP+ Open Session Request
--------------------------

fiid_template_t tmpl_rmcpplus_open_session_request =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "requested_maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "authentication_payload.authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "integrity_payload.integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "confidentiality_payload.confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

RMCP+ Open Session Response
---------------------------

fiid_template_t tmpl_rmcpplus_open_session_response =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h not valid */
    { 32, "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "authentication_payload.authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "integrity_payload.integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "confidentiality_payload.confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

RMCP+ RAKP Message 1
--------------------

fiid_template_t tmpl_rmcpplus_rakp_message_1 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "remote_console_random_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "requested_maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "name_only_lookup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "user_name_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "user_name", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

RMCP+ RAKP Message 2
--------------------

fiid_template_t tmpl_rmcpplus_rakp_message_2 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "managed_system_random_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "managed_system_guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "key_exchange_authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

RMCP+ RAKP Message 3
--------------------

fiid_template_t tmpl_rmcpplus_rakp_message_3 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "key_exchange_authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

RMCP+ RAKP Message 4
--------------------

/* achu: The IPMI 2.0 Spec version 1.0 lists the 4th field as
 * "management_console_session_id", not "managed_system_session_id"
 * or "remote_console_session_id".  I'm assuming this is a typo and
 * that "remote_console_session_id" is what is really meant.
 */
fiid_template_t tmpl_rmcpplus_rakp_message_4 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "integrity_check_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_RMCPPLUS_INTERFACE_TEMPLATES_H */
