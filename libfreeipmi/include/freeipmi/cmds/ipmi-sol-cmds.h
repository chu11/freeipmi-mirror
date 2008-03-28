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

#ifndef _IPMI_SOL_CMDS_H
#define _IPMI_SOL_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_SOL_PACKET_SEQUENCE_NUMBER_MAX    0xF

#define IPMI_SOL_FLUSH_OUTBOUND        0x1
#define IPMI_SOL_DO_NOT_FLUSH_OUTBOUND 0x0

#define IPMI_SOL_FLUSH_OUTBOUND_VALID(__val) \
        (((__val) == IPMI_SOL_FLUSH_OUTBOUND \
          || (__val) == IPMI_SOL_DO_NOT_FLUSH_OUTBOUND) ? 1 : 0)

#define IPMI_SOL_FLUSH_INBOUND        0x1
#define IPMI_SOL_DO_NOT_FLUSH_INBOUND 0x0

#define IPMI_SOL_FLUSH_INBOUND_VALID(__val) \
        (((__val) == IPMI_SOL_FLUSH_INBOUND \
          || (__val) == IPMI_SOL_DO_NOT_FLUSH_INBOUND) ? 1 : 0)

#define IPMI_SOL_ASSERT_DCD_DSR       0x0
#define IPMI_SOL_DEASSERT_DCD_DSR     0x1

#define IPMI_SOL_ASSERT_DCD_DSR_VALID(__val) \
        (((__val) == IPMI_SOL_ASSERT_DCD_DSR \
          || (__val) == IPMI_SOL_DEASSERT_DCD_DSR) ? 1 : 0)

#define IPMI_SOL_ASSERT_CTS       0x0
#define IPMI_SOL_DEASSERT_CTS     0x1

#define IPMI_SOL_ASSERT_CTS_VALID(__val) \
        (((__val) == IPMI_SOL_ASSERT_CTS \
          || (__val) == IPMI_SOL_DEASSERT_CTS) ? 1 : 0)

#define IPMI_SOL_GENERATE_BREAK        0x1
#define IPMI_SOL_DO_NOT_GENERATE_BREAK 0x0

#define IPMI_SOL_GENERATE_BREAK_VALID(__val) \
        (((__val) == IPMI_SOL_GENERATE_BREAK \
          || (__val) == IPMI_SOL_DO_NOT_GENERATE_BREAK) ? 1 : 0)

#define IPMI_SOL_ASSERT_RI       0x0
#define IPMI_SOL_DEASSERT_RI     0x1

#define IPMI_SOL_ASSERT_RI_VALID(__val) \
        (((__val) == IPMI_SOL_ASSERT_RI \
          || (__val) == IPMI_SOL_DEASSERT_RI) ? 1 : 0)

#define IPMI_SOL_ACK                0x0
#define IPMI_SOL_NACK               0x1

#define IPMI_SOL_ACK_VALID(__val) \
        (((__val) == IPMI_SOL_ACK \
          || (__val) == IPMI_SOL_NACK) ? 1 : 0)

#define IPMI_SOL_NACK_VALID(__val) \
        (((__val) == IPMI_SOL_ACK \
          || (__val) == IPMI_SOL_NACK) ? 1 : 0)

#define IPMI_SOL_BREAK_CONDITION_DETECTED 0x1 
#define IPMI_SOL_NO_BREAK_DETECTED        0x0

#define IPMI_SOL_TRANSMIT_OVERRUN_CHARACTERS_DROPPED     0x1
#define IPMI_SOL_TRANSMIT_OVERRUN_NO_CHARACTERS_DROPPED  0x0

#define IPMI_SOL_SOL_DEACTIVATING 0x1
#define IPMI_SOL_SOL_ACTIVE       0x0

#define IPMI_SOL_CHARACTER_TRANSFER_UNAVAILABLE 0x1
#define IPMI_SOL_CHARACTER_TRANSFER_AVAILABLE   0x0

#define IPMI_SOL_SOL_DISABLE    0x0
#define IPMI_SOL_SOL_ENABLE     0x1

#define IPMI_SOL_SOL_ENABLE_VALID(__val) \
        (((__val) == IPMI_SOL_SOL_DISABLE \
          || (__val) == IPMI_SOL_SOL_ENABLE) ? 1 : 0)

#define IPMI_SOL_FORCE_SOL_PAYLOAD_AUTHENTICATION             0x1
#define IPMI_SOL_AUTHENTICATION_CONTROLLED_BY_REMOTE_SOFTWARE 0x0

#define IPMI_SOL_FORCE_SOL_PAYLOAD_AUTHENTICATION_VALID(__val) \
        (((__val) == IPMI_SOL_FORCE_SOL_PAYLOAD_AUTHENTICATION \
          || (__val) == IPMI_SOL_AUTHENTICATION_CONTROLLED_BY_REMOTE_SOFTWARE) ? 1 : 0)

#define IPMI_SOL_FORCE_SOL_PAYLOAD_ENCRYPTION                 0x1
#define IPMI_SOL_ENCRYPTION_CONTROLLED_BY_REMOTE_SOFTWARE     0x0

#define IPMI_SOL_FORCE_SOL_PAYLOAD_ENCRYPTION_VALID(__val) \
        (((__val) == IPMI_SOL_FORCE_SOL_PAYLOAD_ENCRYPTION \
          || (__val) == IPMI_SOL_ENCRYPTION_CONTROLLED_BY_REMOTE_SOFTWARE) ? 1 : 0)

#define IPMI_SOL_BIT_RATE_SERIAL_BIT_RATE 0x0
#define IPMI_SOL_BIT_RATE_96_KBPS         0x6
#define IPMI_SOL_BIT_RATE_192_KBPS        0x7
#define IPMI_SOL_BIT_RATE_384_KBPS        0x8
#define IPMI_SOL_BIT_RATE_576_KBPS        0x9
#define IPMI_SOL_BIT_RATE_1152_KBPS       0xA

#define IPMI_SOL_BIT_RATE_VALID(__val) \
        (((__val) == IPMI_SOL_BIT_RATE_SERIAL_BIT_RATE \
          || (__val) == IPMI_SOL_BIT_RATE_96_KBPS \
          || (__val) == IPMI_SOL_BIT_RATE_192_KBPS \
          || (__val) == IPMI_SOL_BIT_RATE_384_KBPS \
          || (__val) == IPMI_SOL_BIT_RATE_576_KBPS \
          || (__val) == IPMI_SOL_BIT_RATE_1152_KBPS) ? 1 : 0)

#define IPMI_GET_SOL_PARAMETER                          0x0
#define IPMI_GET_SOL_PARAMETER_REVISION_ONLY            0x1

#define IPMI_GET_SOL_PARAMETER_VALID(__val) \
        (((__val) == IPMI_GET_SOL_PARAMETER \
          || (__val) == IPMI_GET_SOL_PARAMETER_REVISION_ONLY) ? 1 : 0)

extern fiid_template_t tmpl_sol_payload_data;
extern fiid_template_t tmpl_sol_payload_data_remote_console_to_bmc;
extern fiid_template_t tmpl_sol_payload_data_bmc_to_remote_console;

extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_rq;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_rs;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_enable_rq;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_authentication_rq;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rq;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_retry_rq;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate_rq;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate_rq;
extern fiid_template_t tmpl_cmd_set_sol_configuration_parameters_sol_payload_port_number_rq;

extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_rq;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_enable_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_authentication_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_retry_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs;
extern fiid_template_t tmpl_cmd_get_sol_configuration_parameters_sol_payload_port_number_rs;

int8_t fill_sol_payload_data (uint8_t packet_sequence_number,
                              uint8_t packet_ack_nack_sequence_number,
                              uint8_t accepted_character_count,
                              uint8_t operation_status,
                              uint8_t *character_data,
                              uint32_t character_data_len,
                              fiid_obj_t obj_sol_payload);

int8_t fill_sol_payload_data_remote_console_to_bmc (uint8_t packet_sequence_number,
                                                    uint8_t packet_ack_nack_sequence_number,
                                                    uint8_t accepted_character_count,
                                                    uint8_t flush_outbound,
                                                    uint8_t flush_inbound,
                                                    uint8_t drop_dcd_dsr,
                                                    uint8_t cts_pause,
                                                    uint8_t generate_break,
                                                    uint8_t ring_wor,
                                                    uint8_t nack,
                                                    uint8_t *character_data,
                                                    uint32_t character_data_len,
                                                    fiid_obj_t obj_sol_payload);

int8_t fill_cmd_set_sol_configuration_parameters (uint8_t channel_number,
						  uint8_t parameter_selector,
						  uint8_t *configuration_parameter_data,
						  uint8_t configuration_parameter_data_len,
						  fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_sol_configuration_parameters_sol_enable (uint8_t channel_number, 
                                                             uint8_t sol_enable,
                                                             fiid_obj_t obj_data_rq);

int8_t fill_cmd_set_sol_configuration_parameters_sol_authentication (uint8_t channel_number,
                                                                     uint8_t sol_privilege_level,
                                                                     uint8_t force_sol_payload_authentication,
                                                                     uint8_t force_sol_payload_encryption,
                                                                     fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (uint8_t channel_number,
                                                                                                   uint8_t character_accumulate_interval,
                                                                                                   uint8_t character_send_threshold,
                                                                                                   fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_sol_configuration_parameters_sol_retry (uint8_t channel_number,
                                                            uint8_t retry_count,
                                                            uint8_t retry_interval,
                                                            fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (uint8_t channel_number,
                                                                            uint8_t bit_rate,
                                                                            fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (uint8_t channel_number,
                                                                        uint8_t bit_rate,
                                                                        fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_set_sol_configuration_parameters_sol_payload_port_number (uint8_t channel_number,
                                                                          uint16_t port_number,
                                                                          fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_sol_configuration_parameters (uint8_t channel_number,
						  uint8_t get_parameter,
						  uint8_t parameter_selector, 
						  uint8_t set_selector,
						  uint8_t block_selector,
						  fiid_obj_t obj_data_rq);

#ifdef __cplusplus
}
#endif

#endif
