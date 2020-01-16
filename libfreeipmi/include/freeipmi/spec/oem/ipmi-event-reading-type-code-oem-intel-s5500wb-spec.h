/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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

#ifndef IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_SPEC_H
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel S5500WB/Penguin Computing Relion 700
 */

/* achu: Similarities to "common" event reading typo codes, but
   different enough to warrant not being in common */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_PCIE_FATAL_SENSOR       0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_PCIE_CORRECTABLE_SENSOR 0x71
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_QPI_CORRECTABLE_SENSOR  0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_QPI_NON_FATAL_SENSOR    0x73
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_QPI_FATAL_SENSOR        0x74

#ifdef __cplusplus
}
#endif

#endif /* IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S5500WB_SPEC_H */
