/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifndef IPMI_EVENT_READING_TYPE_CODE_OEM_SPEC_H
#define IPMI_EVENT_READING_TYPE_CODE_OEM_SPEC_H

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

/* achu: not official names, named based on use context */

/* achu: the strings are all over the place, "status" is the best name
 * I can think of
 */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS                    0x70

/* achu: names taken from code, are correct names? */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_OEM_DIAGNOSTIC_EVENT_DATA 0x7E

/*
 * Dell Poweredge R720
 */

#define IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_FAILURE                   0x8A

/*******************************************
 * HP                                      *
 *******************************************/

/*
 * HP Proliant DL160 G8
 */

#define IPMI_EVENT_READING_TYPE_CODE_OEM_HP_UID_LIGHT                  0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_HP_HEALTH_LED                 0x71

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel S5500WB/Penguin Computing Relion 700
 */

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_SENSOR       0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR 0x71
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_CORRECTABLE_SENSOR  0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_NON_FATAL_SENSOR    0x73
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_SENSOR        0x74

/* 
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard maintains Intel manufacturer ID)
 */
/* achu: Similar to above, but some events changed and some new ones,
 * so new macros to differentiate
 */ 
/* achu: not official names, named based on use context */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_FATAL_SENSOR       0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_PCIE_CORRECTABLE_SENSOR 0x71
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_CORRECTABLE_SENSOR  0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_NON_FATAL_SENSOR    0x73
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_QPI_FATAL_SENSOR        0x74
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_CORRECTABLE_ERROR       0x76
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QUANTA_QSSC_S4R_UNCORRECTABLE_ERROR     0x77

/*
 * Intel S2600JF/Appro 512X
 */
/* achu: some are similar to above, but different names, new events,
 * new numbers, so making new macros to differentiate */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR       0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_CORRECTABLE_ERROR 0x71
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_QPI_CORRECTABLE_ERRORS 0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR        0x73
/* continuation for IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR for more offsets */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_OPI_FATAL_ERROR_2      0x74
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_CHIPSET_PROPRIETARY    0x75
/* continuation for IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR for more offsets */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_PCIE_FATAL_ERROR_2     0x76
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_QPI_LINK_WIDTH_REDUCED 0x77
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600JF_MEMORY_ERROR_EXTENSION 0x7F

/*
 * Intel Windmill
 * (Quanta Winterfell)
 * (Wiwynn Windmill)
 */
/* achu: Like Intel Node Manager, but no Intel Node Manager on these nodes, so cut & paste to differentiate */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR    0x75
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_OTHER_IIO_ERROR_SENSOR 0x70

/*
 * Intel S2600KP                                                                                                                                                                                   
 * Intel S2600WT2                                                                                                                                                                                  
 * Intel S2600WTT  
 */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_CORRECTABLE_ERROR          0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR                0x73
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR_2              0x74
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_LINK_WIDTH_REDUCED         0x77

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS       0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS_2     0x76
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_CORRECTABLE_ERRORS 0x71 

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_FIRMWARE_UPDATE_STATUS_SENSOR  0x70

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_START            0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_FINISH           0xF0

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO        0x70

/*
 * Intel Node Manager
 *
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 * Quanta QSSC-S4R/Appro GB812X-CN
 * Intel S2600JF/Appro 512X
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 */

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_EXCEPTION_EVENT                       0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_HEALTH_EVENT                          0x73
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_OPERATIONAL_CAPABILITIES_CHANGE_EVENT 0x74
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_NODE_MANAGER_ALERT_THRESHOLD_EXCEEDED              0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_ME_FIRMWARE_HEALTH_EVENT                           0x75
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SERVER_PLATFORM_SERVICES_FIRMWARE_HEALTH           IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_ME_FIRMWARE_HEALTH_EVENT
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_THERMAL_SENSOR_ON_DIMM                             0x76

/*******************************************
 * Inventec                                *
 *******************************************/

/*
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 */
/* achu: not official names, named based on use context */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INVENTEC_BIOS 0x70

/*******************************************
 * Supermicro                              *
 *******************************************/

/*
 * Supermicro X7DBR-3 (X7DBR_3)
 * Supermicro X7DB8
 * Supermicro X8DTN
 * Supermicro X7SBI-LN4 (X7SBI_LN4)
 * Supermicro X8DTH
 * Supermicro X8DTG
 * Supermicro X8DTU
 * Supermicro X8DT3-LN4F (X8DT3_LN4F)
 * Supermicro X8DTU-6+ (X8DTU_6PLUS)
 * Supermicro X8DTL
 * Supermicro X8DTL-3F (X8DTL_3F)
 * Supermicro X8SIL-F  (X8SIL_F)
 * Supermicro X9SCL
 * Supermicro X9SCM
 * Supermicro X8DTN+-F (X8DTNPLUS_F)
 * Supermicro X8SIE
 * Supermicro X9SCA-F-O (X9SCA_F_O)
 * Supermicro H8DGU-F (H8DGU_F)
 * Supermicro X9DRi-F (X9DRI_F)
 * Supermicro X9DRI-LN4F+ (X9DRI_LN4F_PLUS)
 * Supermicro X9SPU-F-O (X9SPU_F_O)
 * Supermicro X9SCM-iiF (X9SCM_IIF)
 */

/* achu: not official names, named based on use context */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC 0x70

#ifdef __cplusplus
}
#endif

#endif /* IPMI_EVENT_READING_TYPE_CODE_OEM_SPEC_H */
