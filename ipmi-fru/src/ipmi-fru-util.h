/*****************************************************************************\
 *  $Id: ipmi-fru-util.h,v 1.9.4.1 2009-04-16 22:54:50 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_FRU_UTIL_H
#define _IPMI_FRU_UTIL_H

#include <freeipmi/freeipmi.h>


#define IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR(__ipmi_fru_parse_ctx) \
  ((ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_NO_FRU_INFORMATION \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_FRU_AREA_LENGTH_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) ==  IPMI_FRU_PARSE_ERR_COMMON_HEADER_CHECKSUM_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_CHASSIS_INFO_AREA_CHECKSUM_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_BOARD_INFO_AREA_CHECKSUM_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_PRODUCT_INFO_AREA_CHECKSUM_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_MULTIRECORD_AREA_CHECKSUM_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_COMMON_HEADER_FORMAT_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_CHASSIS_INFO_AREA_FORMAT_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_BOARD_INFO_AREA_FORMAT_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_PRODUCT_INFO_AREA_FORMAT_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_MULTIRECORD_AREA_FORMAT_INVALID \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_FRU_INFORMATION_INCONSISTENT \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_FRU_LANGUAGE_CODE_NOT_SUPPORTED \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_FRU_INVALID_BCD_ENCODING \
    || ipmi_fru_parse_ctx_errnum ((__ipmi_fru_parse_ctx)) == IPMI_FRU_PARSE_ERR_FRU_SENTINEL_VALUE_NOT_FOUND) ? 1 : 0)

int ipmi_fru_output_field (ipmi_fru_state_data_t *state_data,
                           uint8_t language_code,
                           ipmi_fru_parse_field_t *field,
                           char *str);

#if 0
fru_err_t ipmi_fru_read_fru_data (ipmi_fru_state_data_t *state_data,
                                  uint8_t device_id,
                                  uint8_t *frubuf,
                                  unsigned int frubuflen,
                                  unsigned int offset_in_bytes,
                                  unsigned int fru_read_bytes);

fru_err_t ipmi_fru_output_type_length_field (ipmi_fru_state_data_t *state_data,
                                             uint8_t *frubuf,
                                             unsigned int frubuflen,
                                             unsigned int offset_in_bytes,
                                             uint8_t *language_code,
                                             unsigned int *len_parsed,
                                             char *str);

fru_err_t ipmi_fru_get_info_area_length (ipmi_fru_state_data_t *state_data,
                                         uint8_t device_id,
                                         unsigned int offset_in_bytes,
                                         char *str,
                                         uint64_t *info_area_length);

fru_err_t ipmi_fru_check_checksum (ipmi_fru_state_data_t *state_data,
                                   uint8_t *frubuf,
                                   uint64_t length_in_bytes,
                                   uint8_t checksum_init,
                                   char *str);

fru_err_t ipmi_fru_dump_hex (ipmi_fru_state_data_t *state_data,
                             uint8_t *frubuf,
                             uint64_t length_in_bytes,
                             char *str);

#endif
#endif
