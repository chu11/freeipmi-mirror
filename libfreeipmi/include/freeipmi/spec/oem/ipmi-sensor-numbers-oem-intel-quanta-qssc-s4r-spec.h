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

#ifndef IPMI_SENSOR_NUMBERS_OEM_INTEL_QUANTA_QSSC_S4R_SPEC_H
#define IPMI_SENSOR_NUMBERS_OEM_INTEL_QUANTA_QSSC_S4R_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard contains Intel manufacturer ID)
 */
/* achu: Few same as above, but to clearly differentiate motherboards, duplicate them */

#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCI_SENSOR              0x03
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR       0x04
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR 0x05

#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_CORRECTABLE_SENSOR  0x06
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_NON_FATAL_SENSOR    0x07
/* QPI_FATAL_SENSOR_A and QPI_FATAL_SENSOR_B are identical, they are
 * logical extensions to provide additional offset values
 */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR_A      0x17
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR_B      0x18

#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_BIOS_POST_ERROR         0x06

#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_MIRRORING_MODE 0x01
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_ECC_ERROR                                          0x02
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_MISMATCH_CONFIGURATION_ERROR                       0x03
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_CRC_ERROR_PERSISTENT                             0x05
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_PATROL_SCRUB_ERROR                                        0x0B
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_SMI_LINK_CRC_ERROR_UNCORRECTABLE                          0x0C
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_RAS_STATE_INFORMATION_FOR_MEMORY_MIRRORING_SPARING_MODE   0x11
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_MIRRORING_RAS_CONFIGURATION_INFORMATION            0x12
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_SPARING_RAS_CONFIGURATION_INFORMATION              0x13
#define IPMI_SENSOR_NUMBER_OEM_INTEL_QUANTA_QSSC_S4R_MEMORY_BOARD_STATE                                        0x20

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_NUMBERS_OEM_INTEL_QUANTA_QSSC_S4R_SPEC_H */
