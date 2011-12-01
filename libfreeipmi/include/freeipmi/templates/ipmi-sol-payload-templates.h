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

#ifndef _IPMI_SOL_PAYLOAD_TEMPLATES_H
#define _IPMI_SOL_PAYLOAD_TEMPLATES_H

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

FIID_FIELD_MAKES_PACKET_SUFFICIENT - indicates field or fields are "sufficient" to make a valid packet

SOL Payload Data
----------------

fiid_template_t tmpl_sol_payload_data =
  {
    /* 0h ack only packet */
    { 4, "packet_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h information pakcet.  No request packet being ack'd or nack'd */
    { 4, "packet_ack_nack_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "accepted_character_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "operation_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "character_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

SOL Payload Data Remote Console to BMC
--------------------------------------

fiid_template_t tmpl_sol_payload_data_remote_console_to_bmc =
  {
    /* 0h ack only packet */
    { 4, "packet_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h information pakcet.  No request packet being ack'd or nack'd */
    { 4, "packet_ack_nack_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "accepted_character_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "flush_outbound", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "flush_inbound", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "drop_dcd_dsr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cts_pause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "generate_break", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "ring_wor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "nack", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "character_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

SOL Payload Data BMC to Remote Console
--------------------------------------

fiid_template_t tmpl_sol_payload_data_bmc_to_remote_console =
  {
    /* 0h ack only packet */
    { 4, "packet_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h information pakcet.  No request packet being ack'd or nack'd */
    { 4, "packet_ack_nack_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "accepted_character_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "break_condition", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "transmit_overrun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sol_deactivating", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "character_transfer_unavailable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "nack", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "character_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_SOL_CMDS_TEMPLATES_H */
