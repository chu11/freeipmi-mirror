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

#ifndef IPMI_SENSOR_AND_EVENT_CODE_TABLES_OEM_INTEL_COMMON_SPEC_H
#define IPMI_SENSOR_AND_EVENT_CODE_TABLES_OEM_INTEL_COMMON_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel S5500WB/Penguin Computing Relion 700
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard contains Intel manufacturer ID)
 * Intel S2600JF/Appro 512X
 * Intel S2600WP
 */

#define IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_BITMASK   0xF8
#define IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_SHIFT     3

#define IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_BITMASK 0x07
#define IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_SHIFT   0

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_AND_EVENT_CODE_TABLES_OEM_INTEL_COMMON_SPEC_H */
