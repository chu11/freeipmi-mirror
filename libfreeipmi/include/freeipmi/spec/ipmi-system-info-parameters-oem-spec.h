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

#ifndef _IPMI_SYSTEM_INFO_PARAMETERS_OEM_SPEC_H
#define _IPMI_SYSTEM_INFO_PARAMETERS_OEM_SPEC_H

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

#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_LCD_STRING                           0xC1
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_LCD_CONFIGURATION                    0xC2
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_GUID                          0xC3
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_ASSET_TAG                     0xC4
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_SERVICE_TAG                   0xC5
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CHASSIS_SERVICE_TAG                  0xC6
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CHASSIS_RELATED_SERVICE_TAG          0xC7
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_BOARD_REVISION                       0xC8
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_ID                            0xC9
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_BIOS_FEATURE                         0xCA
/* Only for 10G systems */
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_EMBEDDED_NICS_MAC_ADDRESSES          0xCB
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_EMBEDDED_NICS_CAPABILITY             0xCE
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_PLATFORM_MODEL_NAME                  0xD1
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_LOCAL_CONSOLE_LOCKOUT                0xD6
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_POWER_STAGGERING_AC_RECOVERY         0xD8
/* achu: this one is taken from code, is correct name? */
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_11G_MAC_ADDRESSES                    0xDA
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IDRAC_INFO                           0xDD
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IDRAC_IPV4_URL                       0xDE
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CMD_IPV4_URL                         0xE0
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_GUI_WEBSERVER_CONTROL                0xE1
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_PLATFORM_SPECIFIC_DEVICE_INFORMATION 0xE3
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_LCD_STATUS                           0xE7
/* achu: this one is taken from code, is correct name? */
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_POWER_CAPACITY                       0xEA
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_AVERAGE_POWER_CONSUMPTION_STATISTICS 0xEB
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS     0xEC
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS     0xED
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_EMBEDDED_VIDEO_STATUS                0xEE
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_ISCSI_NICS_MAC_ADDRESSES             0xEF
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IPV6_SNMP_TRAP_DESTINATION_ADDRESS   0xF0
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_INTERNAL_STORAGE_SLOT_INFO           0xF1
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CMC_IPV6_INFO                        0xF2
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CMC_IPV6_URL                         0xF3
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_REVISION                      0xF4
#define IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_REDUNDANCY_POLICY                    0xFE

#ifdef __cplusplus
}
#endif
#endif /* _IPMI_SYSTEM_INFO_PARAMETERS_SPEC_H */
