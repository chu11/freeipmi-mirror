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

#include "freeipmi/cmds/ipmi-sol-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-sol-parameter-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_sol_payload_data =
  {
    /* 0h ack only packet */
    { 4,      "packet_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,      "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h information pakcet.  No request packet being ack'd or nack'd */
    { 4,      "packet_ack_nack_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,      "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,      "accepted_character_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,      "operation_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "character_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_sol_payload_data_remote_console_to_bmc =
  {
    /* 0h ack only packet */
    { 4,      "packet_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,      "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h information pakcet.  No request packet being ack'd or nack'd */
    { 4,      "packet_ack_nack_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,      "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,      "accepted_character_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "flush_outbound", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "flush_inbound", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "drop_dcd_dsr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "cts_pause", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "generate_break", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "ring_wor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "nack", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "character_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_sol_payload_data_bmc_to_remote_console =
  {
    /* 0h ack only packet */
    { 4,      "packet_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,      "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h information pakcet.  No request packet being ack'd or nack'd */
    { 4,      "packet_ack_nack_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,      "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,      "accepted_character_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2,      "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "break_condition", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "transmit_overrun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "sol_deactivating", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "character_transfer_unavailable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "nack", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,      "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "character_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_rq =
  {
    { 8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,    "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,    "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,    "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_enable_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sol_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_authentication_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "sol_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_encryption", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_accumulate_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_send_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_retry_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "retry_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_payload_port_number_rq =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_rs =
  {
    { 8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4,    "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,    "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_enable_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sol_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0,  "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_authentication_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "sol_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "force_sol_payload_encryption", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0,  "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_accumulate_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "character_send_threshold", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_retry_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "retry_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* Note: Read-Only field, no 'set' equivalent */
fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_channel", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_payload_port_number_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4,  "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
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

int
fill_cmd_set_sol_configuration_parameters (uint8_t channel_number,
                                           uint8_t parameter_selector,
                                           const void *configuration_parameter_data,
                                           unsigned int configuration_parameter_data_len,
                                           fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || (!IPMI_SOL_PARAMETER_SELECTOR_VALID (parameter_selector)
          && !IPMI_SOL_PARAMETER_SELECTOR_IS_OEM (parameter_selector))
      || !configuration_parameter_data
      || !configuration_parameter_data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "configuration_parameter_data",
                          configuration_parameter_data,
                          configuration_parameter_data_len);

  return (0);
}


int
fill_cmd_set_sol_configuration_parameters_sol_enable (uint8_t channel_number,
                                                      uint8_t sol_enable,
                                                      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_SOL_SOL_ENABLE_VALID (sol_enable)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_enable_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SOL_PARAMETER_SOL_ENABLE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sol_enable", sol_enable);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return (0);
}

int
fill_cmd_set_sol_configuration_parameters_sol_authentication (uint8_t channel_number,
                                                              uint8_t sol_privilege_level,
                                                              uint8_t force_sol_payload_authentication,
                                                              uint8_t force_sol_payload_encryption,
                                                              fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_PRIVILEGE_LEVEL_VALID (sol_privilege_level)
      || !IPMI_SOL_FORCE_SOL_PAYLOAD_AUTHENTICATION_VALID (force_sol_payload_authentication)
      || !IPMI_SOL_FORCE_SOL_PAYLOAD_ENCRYPTION_VALID (force_sol_payload_encryption)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_authentication_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SOL_PARAMETER_SOL_AUTHENTICATION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sol_privilege_level", sol_privilege_level);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "force_sol_payload_authentication", force_sol_payload_authentication);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "force_sol_payload_encryption", force_sol_payload_encryption);

  return (0);
}

int
fill_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (uint8_t channel_number,
                                                                                            uint8_t character_accumulate_interval,
                                                                                            uint8_t character_send_threshold,
                                                                                            fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SOL_PARAMETER_CHARACTER_ACCUMULATE_INTERVAL_AND_SEND_THRESHOLD);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "character_accumulate_interval", character_accumulate_interval);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "character_send_threshold", character_send_threshold);

  return (0);
}

int
fill_cmd_set_sol_configuration_parameters_sol_retry (uint8_t channel_number,
                                                     uint8_t retry_count,
                                                     uint8_t retry_interval,
                                                     fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_retry_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SOL_PARAMETER_SOL_RETRY);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "retry_count", retry_count);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "retry_interval", retry_interval);

  return (0);
}

int
fill_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (uint8_t channel_number,
                                                                     uint8_t bit_rate,
                                                                     fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_SOL_BIT_RATE_VALID (bit_rate)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SOL_PARAMETER_SOL_NON_VOLATILE_BIT_RATE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "bit_rate", bit_rate);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return (0);
}

int
fill_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (uint8_t channel_number,
                                                                 uint8_t bit_rate,
                                                                 fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_SOL_BIT_RATE_VALID (bit_rate)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SOL_PARAMETER_SOL_VOLATILE_BIT_RATE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "bit_rate", bit_rate);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return (0);
}

int
fill_cmd_set_sol_configuration_parameters_sol_payload_port_number (uint8_t channel_number,
                                                                   uint16_t port_number,
                                                                   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_sol_configuration_parameters_sol_payload_port_number_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SOL_PARAMETER_SOL_PAYLOAD_PORT_NUMBER);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "port_number", port_number);

  return (0);
}

int
fill_cmd_get_sol_configuration_parameters (uint8_t channel_number,
                                           uint8_t get_parameter,
                                           uint8_t parameter_selector,
                                           uint8_t set_selector,
                                           uint8_t block_selector,
                                           fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_GET_SOL_PARAMETER_VALID (get_parameter)
      || (!IPMI_SOL_PARAMETER_SELECTOR_VALID (parameter_selector)
          && !IPMI_SOL_PARAMETER_SELECTOR_IS_OEM (parameter_selector))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_sol_configuration_parameters_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SOL_CONFIGURATION_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "get_parameter", get_parameter);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "block_selector", block_selector);

  return (0);
}

