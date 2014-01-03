/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

#ifndef IPMI_OEM_COMMON_H
#define IPMI_OEM_COMMON_H

#include "ipmi-oem.h"

#define IPMI_OEM_MAX_BYTES      256
#define IPMI_OEM_ERR_BUFLEN     1024
#define IPMI_OEM_TIME_BUFLEN    1024
#define IPMI_OEM_HEX_BASE       16
#define IPMI_OEM_FMT_BUFLEN     1024
#define IPMI_OEM_STR_BUFLEN     2048

/* returns 1 if found OEM specific error message, 0 if not, -1 on error */
typedef int (*Ipmi_oem_comp_code_strerror)(ipmi_oem_state_data_t *state_data,
                                           uint8_t comp_code,
                                           uint8_t cmd,
                                           uint8_t netfn,
                                           char *errbuf,
                                           unsigned int errbuflen);

int ipmi_oem_check_response_and_completion_code (ipmi_oem_state_data_t *state_data,
                                                 const void *bytes_rs,
                                                 unsigned int bytes_rs_len,
                                                 unsigned int expected_bytes_rs_len,
                                                 uint8_t cmd,
                                                 uint8_t netfn,
                                                 Ipmi_oem_comp_code_strerror comp_code_strerror);

int ipmi_oem_parse_key_value (ipmi_oem_state_data_t *state_data,
                              unsigned int option_num,
                              char **key,
                              char **value);

int ipmi_oem_parse_enable (ipmi_oem_state_data_t *state_data,
                           unsigned int option_num,
                           const char *value,
                           uint8_t *enable);

int ipmi_oem_parse_1_byte_field (ipmi_oem_state_data_t *state_data,
				 unsigned int option_num,
				 const char *value,
				 uint8_t *value_out);

int ipmi_oem_parse_2_byte_field (ipmi_oem_state_data_t *state_data,
				 unsigned int option_num,
				 const char *value,
				 uint16_t *value_out);

int ipmi_oem_parse_4_byte_field (ipmi_oem_state_data_t *state_data,
				 unsigned int option_num,
				 const char *value,
				 uint32_t *value_out);

int ipmi_oem_parse_unsigned_int_range (ipmi_oem_state_data_t *state_data,
                                       unsigned int option_num,
                                       const char *value,
                                       uint32_t *value_out,
                                       unsigned int min,
                                       unsigned int max);

int ipmi_oem_parse_port (ipmi_oem_state_data_t *state_data,
                         unsigned int option_num,
                         const char *value,
                         uint16_t *port);

int ipmi_oem_parse_ip_address (ipmi_oem_state_data_t *state_data,
                               unsigned int option_num,
                               const char *value,
                               uint32_t *ip_address);

int ipmi_oem_parse_string (ipmi_oem_state_data_t *state_data,
                           unsigned int option_num,
                           const char *value,
                           uint8_t *string_length,
                           char *stringbuf,
                           unsigned int stringbuflen);

int ipmi_oem_get_system_info_string (ipmi_oem_state_data_t *state_data,
				     uint8_t parameter_selector,
				     uint8_t set_selector,
				     uint8_t block_selector,
				     char *string,
				     unsigned int string_len,
				     unsigned int *string_len_ret);

#endif /* IPMI_OEM_COMMON_H */
