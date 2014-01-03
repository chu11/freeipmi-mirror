/*****************************************************************************\
 *  $Id: ipmi-fru-output.h,v 1.5 2010-02-08 22:20:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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

#ifndef IPMI_FRU_OUTPUT_H
#define IPMI_FRU_OUTPUT_H

#include <freeipmi/freeipmi.h>

#include "ipmi-fru_.h"

#define IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR(__ipmi_fru_ctx) \
  ((ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_NO_FRU_INFORMATION \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_COMMON_HEADER_CHECKSUM_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_CHASSIS_INFO_AREA_CHECKSUM_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_BOARD_INFO_AREA_CHECKSUM_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_PRODUCT_INFO_AREA_CHECKSUM_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_MULTIRECORD_AREA_CHECKSUM_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_COMMON_HEADER_FORMAT_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_CHASSIS_INFO_AREA_FORMAT_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_BOARD_INFO_AREA_FORMAT_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_PRODUCT_INFO_AREA_FORMAT_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_MULTIRECORD_AREA_FORMAT_INVALID \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_FRU_INFORMATION_INCONSISTENT \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_FRU_LANGUAGE_CODE_NOT_SUPPORTED \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_FRU_INVALID_BCD_ENCODING \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_FRU_SENTINEL_VALUE_NOT_FOUND \
    || ipmi_fru_ctx_errnum ((__ipmi_fru_ctx)) == IPMI_FRU_ERR_DEVICE_BUSY) ? 1 : 0)

int ipmi_fru_output_chassis_info_area (ipmi_fru_state_data_t *state_data,
                                       const void *areabuf,
                                       unsigned int area_length);

int ipmi_fru_output_board_info_area (ipmi_fru_state_data_t *state_data,
                                     const void *areabuf,
                                     unsigned int area_length);

int ipmi_fru_output_product_info_area (ipmi_fru_state_data_t *state_data,
                                       const void *areabuf,
                                       unsigned int area_length);

int ipmi_fru_output_power_supply_information (ipmi_fru_state_data_t *state_data,
                                              const void *areabuf,
                                              unsigned int area_length);

int ipmi_fru_output_dc_output (ipmi_fru_state_data_t *state_data,
			       unsigned int area_type,
                               const void *areabuf,
                               unsigned int area_length);

int ipmi_fru_output_dc_load (ipmi_fru_state_data_t *state_data,
			     unsigned int area_type,
                             const void *areabuf,
                             unsigned int area_length);

int ipmi_fru_output_management_access_record (ipmi_fru_state_data_t *state_data,
                                              const void *areabuf,
                                              unsigned int area_length);

int ipmi_fru_output_base_compatibility_record (ipmi_fru_state_data_t *state_data,
                                               const void *areabuf,
                                               unsigned int area_length);

int ipmi_fru_output_extended_compatibility_record (ipmi_fru_state_data_t *state_data,
                                                   const void *areabuf,
                                                   unsigned int area_length);

int ipmi_fru_output_oem_record (ipmi_fru_state_data_t *state_data,
                                const void *areabuf,
                                unsigned int area_length);

int ipmi_fru_output_dimm (ipmi_fru_state_data_t *state_data,
			  const void *areabuf,
			  unsigned int area_length);

#endif /* IPMI_FRU_OUTPUT_H */
