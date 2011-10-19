/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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

#ifndef _IPMI_SENSOR_NUMBERS_OEM_SPEC_H
#define _IPMI_SENSOR_NUMBERS_OEM_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************
 * Dell                                    *
 *******************************************/

/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 */

#define IPMI_SENSOR_NUMBER_OEM_DELL_ECC_CORRECTABLE_ERRORS                              0x01
#define IPMI_SENSOR_NUMBER_OEM_DELL_ECC_UNCORRECTABLE_ERRORS                            0x02
#define IPMI_SENSOR_NUMBER_OEM_DELL_IO_CHANNEL_CHECK                                    0x03
#define IPMI_SENSOR_NUMBER_OEM_DELL_PCI_PARITY_ERROR                                    0x04
#define IPMI_SENSOR_NUMBER_OEM_DELL_PCI_SYSTEM_ERROR                                    0x05
#define IPMI_SENSOR_NUMBER_OEM_DELL_EVENT_LOGGING_FOR_CORRECTABLE_ECC_EVENTS_DISABLED   0x06
#define IPMI_SENSOR_NUMBER_OEM_DELL_EVENT_LOGGING_DISABLED                              0x07
#define IPMI_SENSOR_NUMBER_OEM_DELL_UNKNOWN_ERROR                                       0x08
#define IPMI_SENSOR_NUMBER_OEM_DELL_CPU_INTERNAL_ERROR                                  0x09
#define IPMI_SENSOR_NUMBER_OEM_DELL_CPU_PROTOCOL_ERROR                                  0x0A
#define IPMI_SENSOR_NUMBER_OEM_DELL_CPU_BUSS_PERR                                       0x0B
#define IPMI_SENSOR_NUMBER_OEM_DELL_CPU_BUS_INITIALIZATION_ERROR                        0x0C
#define IPMI_SENSOR_NUMBER_OEM_DELL_CPU_MACHINE_CHECK_ERROR                             0x0D
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_SPARE                                        0x11
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_MIRROR                                       0x12
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_RAID                                         0x13
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_HOT_ADD                                      0x14
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_HOT_REMOVE                                   0x15
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_HOT_FAILURE                                  0x16
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_REDUNDANCY_REGAINED                          0x17
#define IPMI_SENSOR_NUMBER_OEM_DELL_FATAL_PCI_EXPRESS_ERRORS                            0x18
#define IPMI_SENSOR_NUMBER_OEM_DELL_CHIPSET_ERROR                                       0x19
#define IPMI_SENSOR_NUMBER_OEM_DELL_ERROR_REGISTER_POINTER                              0x1A
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEMORY_CORRECTABLE_ECC_WARNING_OR_CRITICAL_EXCEEDED 0x1B
#define IPMI_SENSOR_NUMBER_OEM_DELL_CRC_MEMORY_ERROR                                    0x1C
#define IPMI_SENSOR_NUMBER_OEM_DELL_USB_OVER_CURRENT                                    0x1D
#define IPMI_SENSOR_NUMBER_OEM_DELL_POST_FATAL_ERROR                                    0x1E
#define IPMI_SENSOR_NUMBER_OEM_DELL_INCOMPATIBLE_BMC_FIRMWARE                           0x1F
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEM_OVERTEMP                                        0x20
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEM_FATAL_SB_CRC                                    0x21
#define IPMI_SENSOR_NUMBER_OEM_DELL_MEM_FATAL_NB_CRC                                    0x22
#define IPMI_SENSOR_NUMBER_OEM_DELL_OS_WATCHDOG_TIMER                                   0x23
#define IPMI_SENSOR_NUMBER_OEM_DELL_LINK_TUNING_ERROR                                   0x24
#define IPMI_SENSOR_NUMBER_OEM_DELL_LT_FLEXADDR                                         0x25
#define IPMI_SENSOR_NUMBER_OEM_DELL_NON_FATAL_PCI_EXPRESS_ERRORS                        0x26
#define IPMI_SENSOR_NUMBER_OEM_DELL_FATAL_IO_ERROR                                      0x27
#define IPMI_SENSOR_NUMBER_OEM_DELL_MSR_INFO_LOG                                        0x28

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel S5500WB/Penguin Computing Relion 700
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

#define IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATUS_INFORMATION_FOR_MEMORY_MIRRORING_MIRRORING_MODE  0x01
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_ECC_ERROR                                            0x02
#define IPMI_SENSOR_NUMBER_OEM_INTEL_RAS_STATUS_INFORMATION_FOR_MEMORY_MIRRORING_SPARING_MODE    0x11
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_MIRRORING_RAS_CONFIGURATION_INFORMATION              0x12
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_SPARING_RAS_CONFIGURATION_INFORMATION                0x13
#define IPMI_SENSOR_NUMBER_OEM_INTEL_MEMORY_PARITY_ERROR                                         0x14

/*
 * Intel Node Manager
 *
 * http://download.intel.com/support/motherboards/server/s5500wb/sb/s5500wb_tps_1_0.pdf
 *
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Intel S2600JF/Appro 512X
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 */

#define IPMI_SENSOR_NUMBER_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH 0x17

/*******************************************
 * Inventec                                *
 *******************************************/

/*
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 */
/* achu: not official names, named based on use context */
#define IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_START        0x81
#define IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_OK           0x85
#define IPMI_SENSOR_NUMBER_OEM_INVENTEC_POST_ERROR_CODE   0x06
#define IPMI_SENSOR_NUMBER_OEM_INVENTEC_PORT80_CODE_EVENT 0x55
#define IPMI_SENSOR_NUMBER_OEM_INVENTEC_MEMORY            0x60

/*******************************************
 * Quanta                                  *
 *******************************************/

/*
 * Quanta S99Q/Dell FS12-TY
 */
#define IPMI_SENSOR_NUMBER_OEM_QUANTA_MEMORY              0x60
#define IPMI_SENSOR_NUMBER_OEM_QUANTA_PCI_SENSORID        0x81
#define IPMI_SENSOR_NUMBER_OEM_QUANTA_QPI_SENSORID        0x82
#define IPMI_SENSOR_NUMBER_OEM_QUANTA_INT_SENSORID        0x83
#define IPMI_SENSOR_NUMBER_OEM_QUANTA_SOFTWARE_NMI        0xA5

#ifdef __cplusplus
}
#endif

#endif
