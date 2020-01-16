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

#ifndef IPMI_SENSOR_NUMBERS_OEM_INTEL_COMMON_SPEC_H
#define IPMI_SENSOR_NUMBERS_OEM_INTEL_COMMON_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel S5500WB/Penguin Computing Relion 700
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard contains Intel manufacturer ID)
 */

#define IPMI_SENSOR_NUMBER_OEM_INTEL_PCI_SENSOR              0x03
#define IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_SENSOR       0x04
#define IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_SENSOR 0x05

#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_CORRECTABLE_SENSOR  0x06
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_NON_FATAL_SENSOR    0x07
/* QPI_FATAL_SENSOR_A and QPI_FATAL_SENSOR_B are identical, they are
 * logical extensions to provide additional offset values
 */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_SENSOR_A      0x17
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_SENSOR_B      0x18

#define IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_ERROR         0x06

/* achu: S5500WB was "STATUS" instead of "STATE" */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_MIRRORING_MODE  0x01
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR                                           0x02
/* achu: S5500WB was "STATUS" instead of "STATE" */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_SPARING_MODE    0x11
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_MIRRORING_RAS_CONFIGURATION_INFORMATION             0x12
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_SPARING_RAS_CONFIGURATION_INFORMATION               0x13

/*
 * Intel S2600JF/Appro 512X
 * Intel S2600WP
 */

/* achu: not a typo, memory ras configuration status & memory ecc
 * error both 0x02, not sure why in doc
 */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MIRRORING_REDUNDANCY_STATE      0x01
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_RAS_CONFIGURATION_STATUS 0x02
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR                0x02
#define IPMI_SENSOR_NUMBER_OEM_INTEL_LEGACY_PCI_ERROR                0x03
#define IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR                0x04
#define IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_CORRECTABLE_ERROR          0x05
#define IPMI_SENSOR_NUMBER_OEM_INTEL_BIOS_POST_ERROR                 0x06
/* not a typo, also 0x06 */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_CORRECTABLE_ERRORS          0x06
/* achu: S2600JF earlier code was "OPI", assumed typo */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_ERROR                 0x07
#define IPMI_SENSOR_NUMBER_OEM_INTEL_CHIPSET_PROPRIETARY             0x08
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_LINK_WIDTH_REDUCED          0x09
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ERROR_EXTENSION          0x10
#define IPMI_SENSOR_NUMBER_OEM_INTEL_SPARING_REDUNDANCY_STATE        0x11
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_RAS_MODE_SELECT          0x12
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_PARITY_ERROR             0x13
#define IPMI_SENSOR_NUMBER_OEM_INTEL_PCIE_FATAL_ERROR_2              0x14
/* not a typo, jumps to 0x17 */
/* achu: S2600JF earlier code was "OPI", assumed typo */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QPI_FATAL_ERROR_2               0x17
#define IPMI_SENSOR_NUMBER_OEM_INTEL_SYSTEM_EVENT                    0x83

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_NUMBERS_OEM_INTEL_COMMON_SPEC_H */
