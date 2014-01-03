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
#include <errno.h>

#include "freeipmi/payload/ipmi-sol-payload.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

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

int
fill_sol_payload_data (uint8_t packet_sequence_number,
                       uint8_t packet_ack_nack_sequence_number,
                       uint8_t accepted_character_count,
                       uint8_t operation_status,
                       const void *character_data,
                       unsigned int character_data_len,
                       fiid_obj_t obj_sol_payload)
{
  if (!fiid_obj_valid (obj_sol_payload))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_sol_payload, tmpl_sol_payload_data) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_sol_payload);
  FILL_FIID_OBJ_SET (obj_sol_payload, "packet_sequence_number", packet_sequence_number);
  FILL_FIID_OBJ_SET (obj_sol_payload, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_sol_payload, "packet_ack_nack_sequence_number", packet_ack_nack_sequence_number);
  FILL_FIID_OBJ_SET (obj_sol_payload, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_sol_payload, "accepted_character_count", accepted_character_count);
  FILL_FIID_OBJ_SET (obj_sol_payload, "operation_status", operation_status);
  if (character_data && character_data_len)
    FILL_FIID_OBJ_SET_DATA (obj_sol_payload,
                            "character_data",
                            character_data,
                            character_data_len);

  return (0);
}

int
fill_sol_payload_data_remote_console_to_bmc (uint8_t packet_sequence_number,
                                             uint8_t packet_ack_nack_sequence_number,
                                             uint8_t accepted_character_count,
                                             uint8_t flush_outbound,
                                             uint8_t flush_inbound,
                                             uint8_t drop_dcd_dsr,
                                             uint8_t cts_pause,
                                             uint8_t generate_break,
                                             uint8_t ring_wor,
                                             uint8_t nack,
                                             const void *character_data,
                                             unsigned int character_data_len,
                                             fiid_obj_t obj_sol_payload)
{
  if (!IPMI_SOL_FLUSH_OUTBOUND_VALID (flush_outbound)
      || !IPMI_SOL_FLUSH_INBOUND_VALID (flush_inbound)
      || !IPMI_SOL_ASSERT_DCD_DSR_VALID (drop_dcd_dsr)
      || !IPMI_SOL_ASSERT_CTS_VALID (cts_pause)
      || !IPMI_SOL_GENERATE_BREAK_VALID (generate_break)
      || !IPMI_SOL_ASSERT_RI_VALID (ring_wor)
      || !IPMI_SOL_NACK_VALID (nack)
      || !fiid_obj_valid (obj_sol_payload))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_sol_payload, tmpl_sol_payload_data_remote_console_to_bmc) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_sol_payload);
  FILL_FIID_OBJ_SET (obj_sol_payload, "packet_sequence_number", packet_sequence_number);
  FILL_FIID_OBJ_SET (obj_sol_payload, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_sol_payload, "packet_ack_nack_sequence_number", packet_ack_nack_sequence_number);
  FILL_FIID_OBJ_SET (obj_sol_payload, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_sol_payload, "accepted_character_count", accepted_character_count);
  FILL_FIID_OBJ_SET (obj_sol_payload, "flush_outbound", flush_outbound);
  FILL_FIID_OBJ_SET (obj_sol_payload, "flush_inbound", flush_inbound);
  FILL_FIID_OBJ_SET (obj_sol_payload, "drop_dcd_dsr", drop_dcd_dsr);
  FILL_FIID_OBJ_SET (obj_sol_payload, "cts_pause", cts_pause);
  FILL_FIID_OBJ_SET (obj_sol_payload, "generate_break", generate_break);
  FILL_FIID_OBJ_SET (obj_sol_payload, "ring_wor", ring_wor);
  FILL_FIID_OBJ_SET (obj_sol_payload, "nack", nack);
  FILL_FIID_OBJ_SET (obj_sol_payload, "reserved3", 0);
  if (character_data && character_data_len)
    FILL_FIID_OBJ_SET_DATA (obj_sol_payload,
                            "character_data",
                            character_data,
                            character_data_len);

  return (0);
}
