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

#ifndef IPMI_CMD_OEM_SPEC_H
#define IPMI_CMD_OEM_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/******************************************* 
 * Dell                                    *
 *******************************************/
  
/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 * Dell Poweredge R720
 */

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
#define IPMI_CMD_OEM_DELL_RESERVED_EXTENDED_CONFIGURATION 0x01
#define IPMI_CMD_OEM_DELL_GET_EXTENDED_CONFIGURATION      0x02
#define IPMI_CMD_OEM_DELL_SET_EXTENDED_CONFIGURATION      0x03

/* IPMI_NET_FN_OEM_DELL_GENERIC_RQ / IPMI_NET_FN_OEM_DELL_GENERIC_RS */
#define IPMI_CMD_OEM_DELL_RESET_TO_DEFAULTS                                    0x21
#define IPMI_CMD_OEM_DELL_SET_NIC_SELECTION                                    0x24
#define IPMI_CMD_OEM_DELL_GET_NIC_SELECTION                                    0x25
#define IPMI_CMD_OEM_DELL_QUERY_CHASSIS_IDENTIFY_STATUS                        0x32
#define IPMI_CMD_OEM_DELL_GET_CPLD_VERSION                                     0x33
#define IPMI_CMD_OEM_DELL_GET_POWER_CONSUMPTION_DATA                           0x9C
#define IPMI_CMD_OEM_DELL_RESET_POWER_CONSUMPTION_DATA                         0x9D
#define IPMI_CMD_OEM_DELL_POWER_SUPPLY_INFO                                    0xB0
#define IPMI_CMD_OEM_DELL_POWER_MONITORING_OVER_A_SPECIFIED_AVERAGING_INTERVAL 0xB1
#define IPMI_CMD_OEM_DELL_POWER_MONITORING_AVERAGING_INTERVAL_RANGE            0xB2
#define IPMI_CMD_OEM_DELL_POWER_CONSUMPTION                                    0xB3
#define IPMI_CMD_OEM_DELL_FRONT_PANEL_INFO                                     0xB5
/* renamed "front panel info" in 12g */
#define IPMI_CMD_OEM_DELL_LCD_INFO                                             IPMI_CMD_OEM_DELL_FRONT_PANEL_INFO
/* achu: this one is taken from code, is correct name? */
#define IPMI_CMD_OEM_DELL_POWER_CAPACITY_STATUS                                0xBA
#define IPMI_CMD_OEM_DELL_GET_POWER_HEAD_ROOM                                  0xBB
#define IPMI_CMD_OEM_DELL_ROLLBACK_FIRMWARE_VERSION                            0xBE
#define IPMI_CMD_OEM_DELL_GET_IDRAC_FIRMWARE_VERSION_NUMBER                    0xBF
#define IPMI_CMD_OEM_DELL_GET_ACTIVE_LOM_STATUS                                0xC1
#define IPMI_CMD_OEM_DELL_IDRAC_VIRTUAL_MAC                                    0xC9

/*
 * Dell Poweredge R720
 */

#define IPMI_CMD_OEM_DELL_GET_BLADE_SLOT_ID                                     0x18
#define IPMI_CMD_OEM_DELL_SET_NIC_SELECTION_FAILOVER                            0x28
#define IPMI_CMD_OEM_DELL_GET_NIC_SELECTION_FAILOVER                            0x29
#define IPMI_CMD_OEM_DELL_FRESH_AIR                                             0x35
#define IPMI_CMD_OEM_DELL_GET_LAST_POST_CODE                                    0x99
#define IPMI_CMD_OEM_DELL_POWER_MONITORING_OVER_A_SPECIFIED_AVERAGING_INTERVAL2 0xCC
#define IPMI_CMD_OEM_DELL_POWER_MONITORING_AVERAGING_INTERVAL_RANGE2            0xCD

/*
 * Dell Poweredge C410x
 */

/* IPMI_NET_FN_OEM_DELL_GENERIC_RQ / IPMI_NET_FN_OEM_DELL_GENERIC_RS */
#define IPMI_CMD_OEM_DELL_SLOT_POWER_CONTROL 0xF0

/* IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RQ / IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RS */
#define IPMI_CMD_OEM_DELL_PORT_MAP           0xC8

/******************************************* 
 * Fujitsu                                 *
 *******************************************/

/*
 * Fujitsu iRMC S1 / iRMC S2
 *
 * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
 */
/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
#define IPMI_CMD_OEM_FUJITSU_POWER         0x01
#define IPMI_CMD_OEM_FUJITSU_COMMUNICATION 0x02
#define IPMI_CMD_OEM_FUJITSU_FAN_TEST      0x10
#define IPMI_CMD_OEM_FUJITSU_BIOS          0xF1
#define IPMI_CMD_OEM_FUJITSU_SYSTEM        0xF5

/* IPMI_NET_FN_FIRMWARE_RQ / IPMI_NET_FN_FIRMWARE_RS */
#define IPMI_CMD_OEM_FUJITSU_SET_FIRMWARE_SELECTOR 0x04
#define IPMI_CMD_OEM_FUJITSU_GET_FIRMWARE_SELECTOR 0x05

/* IPMI_NET_FN_OEM_FUJITSU_GENERIC_RQ / IPMI_NET_FN_OEM_FUJITSU_GENERIC_RS */
#define IPMI_CMD_OEM_FUJITSU_GET_REMOTE_STORAGE_CONNECTION_OR_STATUS 0x19
#define IPMI_CMD_OEM_FUJITSU_SET_VIDEO_DISPLAY_ON_OFF                0x1A

/*******************************************
 * IBM                                     *
 *******************************************/

/*
 * IBM x3455/x3755
 */
/* IPMI_NET_FN_OEM_IBM_LED_RQ / IPMI_NET_FN_OEM_IBM_LED_RS */
/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_IBM_GET_LED 0xC0

/*******************************************
 * Intel                                   *
 *******************************************/

/*
 * Intel S5500WB/Penguin Computing Relion 700
 * Intel S2600JF/Appro 512X
 */

/* IPMI_NET_FN_OEM_INTEL_GENERIC_RQ / IPMI_NET_FN_OEM_INTEL_GENERIC_RS */
#define IPMI_CMD_OEM_INTEL_RESTORE_CONFIGURATION 0x02
#define IPMI_CMD_OEM_INTEL_SET_FAULT_INDICATION  0x57

/* IPMI_NET_FN_OEM_INTEL_CONFIG_RQ / IPMI_NET_FN_OEM_INTEL_CONFIG_RS */
#define IPMI_CMD_OEM_INTEL_SET_SMTP_CONFIGURATION 0x37
#define IPMI_CMD_OEM_INTEL_GET_SMTP_CONFIGURATION 0x38

/*
 * Intel S2600JF/Appro 512X
 */

/* IPMI_NET_FN_OEM_INTEL_GENERIC_RQ / IPMI_NET_FN_OEM_INTEL_GENERIC_RS */
#define IPMI_CMD_OEM_INTEL_SET_POWER_RESTORE_DELAY 0x54
#define IPMI_CMD_OEM_INTEL_GET_POWER_RESTORE_DELAY 0x55
#define IPMI_CMD_OEM_INTEL_GET_BMC_SERVICE_STATUS  0xB2
#define IPMI_CMD_OEM_INTEL_CONTROL_BMC_SERVICES    0xB1

/*
 * Intel Node Manager
 *
 * For Intel Chips, not just Intel Motherboards.  Confirmed for:
 *
 * Intel S5500WB/Penguin Computing Relion 700
 * Intel S2600JF/Appro 512X
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 * Quanta S99Q/Dell FS12-TY
 * Quanta QSSC-S4R/Appro GB812X-CN
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 */

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_ENABLE_DISABLE_NODE_MANAGER_POLICY_CONTROL 0xC0
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY                    0xC1
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_POLICY                    0xC2
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_THRESHOLDS          0xC3
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_ALERT_THRESHOLDS          0xC4
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POLICY_SUSPEND_PERIODS    0xC5
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_POLICY_SUSPEND_PERIODS    0xC6
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_RESET_NODE_MANAGER_STATISTICS              0xC7
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_STATISTICS                0xC8
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_CAPABILITIES              0xC9
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_VERSION                   0xCA
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_POWER_DRAW_RANGE          0xCB
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_SET_NODE_MANAGER_ALERT_DESTINATION         0xCE
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_NODE_MANAGER_ALERT_DESTINATION         0xCF
#define IPMI_CMD_OEM_INTEL_NODE_MANAGER_GET_LIMITING_POLICY_ID                     0xF2

/******************************************* 
 * Inventec                                *
 *******************************************/

/*
 * Inventec 5441/Dell Xanadu II
 * Inventec 5442/Dell Xanadu III
 */

/* IPMI_NET_FN_FIRMWARE_RQ / IPMI_NET_FN_FIRMWARE_RS */
#define IPMI_CMD_OEM_INVENTEC_UPDATE_FIRMARE    0x01
#define IPMI_CMD_OEM_INVENTEC_GET_UPDATE_STATUS 0x02
#define IPMI_CMD_OEM_INVENTEC_COPY_IMAGE_DATA   0x03

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_INVENTEC_SET_DEDICATED_MAC_ADDRESS    0x21
#define IPMI_CMD_OEM_INVENTEC_SET_SHARED_MAC_ADDRESS       0x23

/* IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ / IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS */
#define IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGURATION 0x01
#define IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION      0x02
#define IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION      0x03
#define IPMI_CMD_OEM_INVENTEC_RESTORE_TO_DEFAULTS             0x04
#define IPMI_CMD_OEM_INVENTEC_GET_RESTORE_STATUS              0x05
#define IPMI_CMD_OEM_INVENTEC_SET_SYSTEM_GUID                 0xB3

/* IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RQ / IPMI_NET_FN_OEM_INVENTEC_SPECIFIC_RS */ 
#define IPMI_CMD_OEM_INVENTEC_SET_WEB_PORT_NUM                0x02
#define IPMI_CMD_OEM_INVENTEC_GET_WEB_PORT_NUM                0x03
#define IPMI_CMD_OEM_INVENTEC_SET_BOARD_ID                    0x10
#define IPMI_CMD_OEM_INVENTEC_GET_BOARD_ID                    0x11
#define IPMI_CMD_OEM_INVENTEC_SET_ASSET_TAG                   0x12
#define IPMI_CMD_OEM_INVENTEC_SET_LAN_SOURCE                  0x13
#define IPMI_CMD_OEM_INVENTEC_GET_LAN_SOURCE                  0x14
#define IPMI_CMD_OEM_INVENTEC_SET_FCB_FW_VERSION              0x15
#define IPMI_CMD_OEM_INVENTEC_GET_FCB_FW_VERSION              0x16
#define IPMI_CMD_OEM_INVENTEC_SET_FAN_CONTROL                 0x61
#define IPMI_CMD_OEM_INVENTEC_GET_FAN_CONTROL                 0x62
#define IPMI_CMD_OEM_INVENTEC_SET_FSC_TABLE                   0x63
#define IPMI_CMD_OEM_INVENTEC_GET_FSC_TABLE                   0x64
#define IPMI_CMD_OEM_INVENTEC_GET_FCB_SKU_INFO                0x6A
#define IPMI_CMD_OEM_INVENTEC_GET_FCB_POWER_THROTTLING_STATUS 0x6B
#define IPMI_CMD_OEM_INVENTEC_OEM_GET_PIC_MODEL               0x70
#define IPMI_CMD_OEM_INVENTEC_OEM_SET_FLASH_PIN               0x71
#define IPMI_CMD_OEM_INVENTEC_OEM_GET_FLASH_PIN               0x72
#define IPMI_CMD_OEM_INVENTEC_OEM_NEW_MASTER_WRITE_READ       0x73
#define IPMI_CMD_OEM_INVENTEC_SET_POWER_THROTTLING_BEHAVIOR   0xB1
#define IPMI_CMD_OEM_INVENTEC_GET_POWER_THROTTLING_BEHAVIOR   0xB2
#define IPMI_CMD_OEM_INVENTEC_GET_PSU_MISMATCH_AND_TYPE       0xB3

/******************************************* 
 * Quanta                                  *
 *******************************************/

/*
 * Quanta S99Q/Dell FS12-TY
 */

/* IPMI_NET_FN_FIRMWARE_RQ / IPMI_NET_FN_FIRMWARE_RS */
#define IPMI_CMD_OEM_QUANTA_UPDATE_FIRMARE      0x01
#define IPMI_CMD_OEM_QUANTA_GET_UPDATE_STATUS   0x02
#define IPMI_CMD_OEM_QUANTA_COPY_IMAGE_DATA     0x03
#define IPMI_CMD_OEM_QUANTA_USB_FIRMWARE_UPDATE 0x04

/* IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ / IPMI_NET_FN_OEM_QUANTA_GENERIC_RS */
#define IPMI_CMD_OEM_QUANTA_RESERVED_EXTENDED_CONFIGURATION   0x01
#define IPMI_CMD_OEM_QUANTA_GET_EXTENDED_CONFIGURATION        0x02
#define IPMI_CMD_OEM_QUANTA_SET_EXTENDED_CONFIGURATION        0x03
#define IPMI_CMD_OEM_QUANTA_RESET_TO_DEFAULTS                 0x04
#define IPMI_CMD_OEM_QUANTA_GET_RESTORE_STATUS                0x05
#define IPMI_CMD_OEM_QUANTA_GET_SENSOR_TEMPERATURE_READING    0x10
#define IPMI_CMD_OEM_QUANTA_GET_PROCESSOR_INFORMATION         0x18
#define IPMI_CMD_OEM_QUANTA_SET_POWER_ON_BY_WAKE_ON_LAN_EVENT 0x1B

/******************************************* 
 * Sun Microsystems                        *
 *******************************************/
  
/*
 * Sun 4140
 */

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
#define IPMI_CMD_OEM_SUN_GET_LED 0x21
#define IPMI_CMD_OEM_SUN_SET_LED 0x22

/******************************************* 
 * Supermicro                              *
 *******************************************/
  
/*
 * Supermicro H8QME
 */

/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_SUPERMICRO_EXTRA_FIRMWARE_INFO 0x20
#define IPMI_CMD_OEM_SUPERMICRO_RESET_INTRUSION     0x03

/*
 * Supermicro X8DTG
 */

/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_SUPERMICRO_GENERIC_EXTENSION   0x70 

/******************************************* 
 * Wistron                                 *
 *******************************************/

/*
 * Wistron / Dell Poweredge C6220
 */

/* IPMI_NET_FN_FIRMWARE_RQ / IPMI_NET_FN_FIRMWARE_RS */
#define IPMI_CMD_OEM_WISTRON_UPDATE_FIRMARE           0x01
#define IPMI_CMD_OEM_WISTRON_GET_UPDATE_STATUS        0x02
#define IPMI_CMD_OEM_WISTRON_COPY_IMAGE_DATA          0x03
#define IPMI_CMD_OEM_WISTRON_USB_FIRMWARE_UPDATE      0x04
#define IPMI_CMD_OEM_WISTRON_WRITE_PROPRIETARY_STRING 0x0B
#define IPMI_CMD_OEM_WISTRON_READ_PROPRIETARY_STRING  0x0C
#define IPMI_CMD_OEM_WISTRON_CLEAR_PROPRIETARY_STRING 0x0D

/* IPMI_NET_FN_OEM_WISTRON_GENERIC_RQ / IPMI_NET_FN_OEM_WISTRON_GENERIC_RS */
#define IPMI_CMD_OEM_WISTRON_RESERVED_EXTENDED_CONFIGURATION     0x01
#define IPMI_CMD_OEM_WISTRON_GET_EXTENDED_CONFIGURATION          0x02
#define IPMI_CMD_OEM_WISTRON_SET_EXTENDED_CONFIGURATION          0x03
#define IPMI_CMD_OEM_WISTRON_RESET_TO_DEFAULTS                   0x04
#define IPMI_CMD_OEM_WISTRON_GET_RESTORE_STATUS                  0x05
#define IPMI_CMD_OEM_WISTRON_SET_CHASSIS_CONFIGURATION           0x11
#define IPMI_CMD_OEM_WISTRON_GET_CHASSIS_CONFIGURATION           0x12
#define IPMI_CMD_OEM_WISTRON_SET_CHASSIS_NAME                    0x13
#define IPMI_CMD_OEM_WISTRON_GET_CHASSIS_NAME                    0x14
#define IPMI_CMD_OEM_WISTRON_SET_SENSOR_INFO                     0x15
#define IPMI_CMD_OEM_WISTRON_GET_SENSOR_INFO                     0x16
#define IPMI_CMD_OEM_WISTRON_SET_CHASSIS_ENCLOSURE_POWER_CAPPING 0x17
#define IPMI_CMD_OEM_WISTRON_GET_CHASSIS_ENCLOSURE_POWER_CAPPING 0x18
#define IPMI_CMD_OEM_WISTRON_SET_FAN_SPEED_CONTROL               0x19
#define IPMI_CMD_OEM_WISTRON_GET_FAN_SPEED_CONTROL               0x1A
#define IPMI_CMD_OEM_WISTRON_SET_CHASSIS_LED_STATUS              0x1B
#define IPMI_CMD_OEM_WISTRON_GET_CHASSIS_LED_STATUS              0x1C
#define IPMI_CMD_OEM_WISTRON_SET_HDD_INFO                        0x1D
#define IPMI_CMD_OEM_WISTRON_SET_PSU_INFO                        0x1E
#define IPMI_CMD_OEM_WISTRON_GET_PSU_INFO                        0x1F
#define IPMI_CMD_OEM_WISTRON_SET_CHASSIS_SERVICE_TAG             0x20
#define IPMI_CMD_OEM_WISTRON_GET_CHASSIS_SERVICE_TAG             0x25
#define IPMI_CMD_OEM_WISTRON_SYNCHRONIZE CHASSIS_SERVICE_TAG     0x26
#define IPMI_CMD_OEM_WISTRON_SET_PSU_CONFIGURATION               0x27
#define IPMI_CMD_OEM_WISTRON_GET_PSU_CONFIGURATION               0x28
#define IPMI_CMD_OEM_WISTRON_SYNCHRONIZE_CHASSIS_ENCLOSURE_POWER 0x29
#define IPMI_CMD_OEM_WISTRON_SET_THERMAL_THROTTLING              0x2A
#define IPMI_CMD_OEM_WISTRON_GET_THERMAL_THROTTLING              0x2B
#define IPMI_CMD_OEM_WISTRON_GET_BMC_SC_BMC_PROTOCOL_VERSION     0x2C
#define IPMI_CMD_OEM_WISTRON_GET_SC_SC_BMC_PROTOCOL_VERSION      0x2D
#define IPMI_CMD_OEM_WISTRON_GET_CHASSIS_POWER_READINGS          0x2E
#define IPMI_CMD_OEM_WISTRON_SET_CHASSIS_POWER_READINGS          0x2F
#define IPMI_CMD_OEM_WISTRON_SET_PASSWORD_POLICY                 0x51

#ifdef __cplusplus
}
#endif

#endif /* IPMI_CMD_OEM_SPEC_H */
