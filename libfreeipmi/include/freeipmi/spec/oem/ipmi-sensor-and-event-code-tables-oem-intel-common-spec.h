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
 */
/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_RECEIVER_ERROR       0x00
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_BAD_DLLP             0x01
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_BAD_TLLP             0x02
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_REPLAY_NUM_ROLLOVER  0x03
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_REPLAY_TIMER_TIMEOUT 0x04
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_ADVISORY_NON_FATAL   0x05
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_LINK_BW_CHANGED      0x06

/*
 * String arrays for above
 */

extern const char * const ipmi_oem_intel_specific_pci_correctable_sensor[];
extern unsigned int ipmi_oem_intel_specific_pci_correctable_sensor_max_index;

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
