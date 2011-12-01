/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef _IPMI_SENSOR_TYPES_OEM_SPEC_H
#define _IPMI_SENSOR_TYPES_OEM_SPEC_H

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

/* achu: names taken from code, are correct names? */
#define IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS 0xC0
#define IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING                           0xC1
#define IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR                       0xC2
#define IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR                        0xC3
#define IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE                               0xC4

/*******************************************
 * Fujitsu                                 *
 *******************************************/

/*
 * Fujitsu Siemens Computers
 * Fujitsu Technology Solutions
 * iRMC S1 / iRMC S2
 */

#define IPMI_SENSOR_TYPE_OEM_FUJITSU_I2C_BUS                    0xC0
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_SYSTEM_POWER_CONSUMPTION   0xDD // Events only
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_STATUS              0xDE
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY_CONFIG              0xDF
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_MEMORY                     0xE1 // Events only
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_HW_ERROR                   0xE3 // Events only
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_SYS_ERROR                  0xE4 // Events only
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_FAN_STATUS                 0xE6
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_STATUS                 0xE8
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_PSU_REDUNDANCY             0xE9
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_COMMUNICATION              0xEA // Reserved
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_FLASH                      0xEC // Events only
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_EVENT                      0xEE // Reserved
#define IPMI_SENSOR_TYPE_OEM_FUJITSU_CONFIG_BACKUP              0xEF

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel S5500WB
 * Intel SR1625
 */

#define IPMI_SENSOR_TYPE_OEM_INTEL_SMI_TIMEOUT                  0xF3

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

#define IPMI_SENSOR_TYPE_OEM_INTEL_NODE_MANAGER 0xDC

/*******************************************
 * Inventec                                *
 *******************************************/

/*
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 */
/* achu: not official names, named based on use context */
#define IPMI_SENSOR_TYPE_OEM_INVENTEC_BIOS 0xC1

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
 * Supermicro X9SCA-F-O (X9SCA-F-O)
 */
/* achu: not official names, named based on use context */
#define IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP 0xC0 

#ifdef __cplusplus
}
#endif

#endif
