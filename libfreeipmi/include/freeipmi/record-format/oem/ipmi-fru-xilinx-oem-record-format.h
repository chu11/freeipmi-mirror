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

#ifndef IPMI_FRU_OEM_XILINX_RECORD_FORMAT_H
#define IPMI_FRU_OEM_XILINX_RECORD_FORMAT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/fiid/fiid.h>

/* OEM multi-record IDs used by Xilinx */
#define IPMI_FRU_OEM_XILINX_THERMAL   0xD0
#define IPMI_FRU_OEM_XILINX_POWER     0xD1
#define IPMI_FRU_OEM_XILINX_MAC_ID    0xD2
#define IPMI_FRU_OEM_XILINX_FREE_FORM 0xD3

/* OEM MAC ID versions used by Xilinx */
#define IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_BOARD        0x01
#define IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_SYSCTL       0x11
#define IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_MODULE       0x21
#define IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_DUT_MAC      0x31
#define IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_DUT_ETHERCAT 0x32

#ifdef __cplusplus
}
#endif

#endif /* IPMI_FRU_OEM_XILINX_RECORD_FORMAT_H */
