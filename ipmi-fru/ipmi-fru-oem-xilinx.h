/*
 * Copyright (C) 2022, Advanced Micro Devices, Inc.
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

#ifndef IPMI_FRU_OEM_XILINX_H
#define IPMI_FRU_OEM_XILINX_H

#include <freeipmi/freeipmi.h>

#include "ipmi-fru_.h"

/* Returns 1 on interpretation, 0 if not, -1 on error */
int ipmi_fru_oem_xilinx_oem_record (ipmi_fru_state_data_t *state_data,
                                    uint8_t record_type_id,
                                    uint32_t manufacturer_id,
                                    uint8_t *oem_data,
                                    unsigned int oem_data_len);

#endif /* IPMI_FRU_OEM_XILINX_H */
