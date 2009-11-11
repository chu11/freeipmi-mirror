/*
   Copyright (C) 2003-2009 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifndef _IPMI_CMD_OEM_SPEC_H
#define _IPMI_CMD_OEM_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* 
 * Dell
 */
  
/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 */

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
#define IPMI_CMD_OEM_DELL_RESERVED_EXTENDED_CONFIGURATION 0x01
#define IPMI_CMD_OEM_DELL_GET_EXTENDED_CONFIGURATION      0x02
#define IPMI_CMD_OEM_DELL_SET_EXTENDED_CONFIGURATION      0x03
#define IPMI_CMD_OEM_DELL_RESET_TO_DEFAULTS               0x21

/* IPMI_NET_FN_OEM_DELL_GENERIC_RQ / IPMI_NET_FN_OEM_DELL_GENERIC_RS */
/* achu: names taken from code, are correct names? */
#define IPMI_CMD_OEM_DELL_GET_NIC_SELECTION                        0x25
#define IPMI_CMD_OEM_DELL_SET_NIC_SELECTION                        0x24
#define IPMI_CMD_OEM_DELL_GET_POWER_INFO                           0x9C
#define IPMI_CMD_OEM_DELL_RESET_POWER_INFO                         0x9D
#define IPMI_CMD_OEM_DELL_GET_POWER_SUPPLY_INFO                    0xB0
#define IPMI_CMD_OEM_DELL_GET_INSTANTANEOUS_POWER_CONSUMPTION_INFO 0xB3
#define IPMI_CMD_OEM_DELL_GET_POWER_HEADROOM_INFO                  0xBB
#define IPMI_CMD_OEM_DELL_POWER_CAPACITY_STATUS                    0xBA

/*
 * Dell Xanadu2
 */

/* IPMI_NET_FN_FIRMWARE_RQ / IPMI_NET_FN_FIRMWARE_RS */
#define IPMI_CMD_OEM_DELL_UPDATE_FIRMWARE   0x01
#define IPMI_CMD_OEM_DELL_GET_UPDATE_STATUS 0x02
#define IPMI_CMD_OEM_DELL_COPY_IMAGE_DATA   0x03

/* IPMI_NET_FN_OEM_DELL_XANADU2_RQ / IPMI_NET_FN_OEM_DELL_XANADU2_RS */ 
/* achu: names taken from code, are correct names? */
#define IPMI_CMD_OEM_DELL_SET_WEB_PORT_NUM 0x02
#define IPMI_CMD_OEM_DELL_GET_WEB_PORT_NUM 0x03
#define IPMI_CMD_OEM_DELL_SET_BOARD_ID     0x10
#define IPMI_CMD_OEM_DELL_GET_BOARD_ID     0x11
#define IPMI_CMD_OEM_DELL_SET_ASSET_TAG    0x12
#define IPMI_CMD_OEM_DELL_SET_LAN_SOURCE   0x13
#define IPMI_CMD_OEM_DELL_GET_LAN_SOURCE   0x14
#define IPMI_CMD_OEM_DELL_SET_FCB_VERSION  0x15
#define IPMI_CMD_OEM_DELL_GET_FCB_VERSION  0x16

/* 
 * Fujitsu
 */

/*
 * Fujitsu RX100 S5
 */
/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
#define IPMI_CMD_OEM_FUJITSU_CONFIG_BMC 0xE0

/* 
 * Inventec
 */

/*
 * Inventec 5441/Dell Xanadu2
 */
/* IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ / IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS */
#define IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGUATION 0x01
#define IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION     0x02
#define IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION     0x03
#define IPMI_CMD_OEM_INVENTEC_RESTORE_TO_DEFAULTS            0x04
#define IPMI_CMD_OEM_INVENTEC_GET_RESTORE_STATUS             0x05
#define IPMI_CMD_OEM_INVENTEC_SET_SYSTEM_GUID                0xB3

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_INVENTEC_SET_DEDICATED_MAC_ADDRESS    0x21
#define IPMI_CMD_OEM_INVENTEC_SET_SHARED_MAC_ADDRESS       0x23

/*
 * Sun
 */
  
/*
 * Sun 4140
 */

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
#define IPMI_CMD_OEM_SUN_GET_LED 0x21
#define IPMI_CMD_OEM_SUN_SET_LED 0x22

/*
 * Supermicro
 */
  
/*
 * Supermicro H8QME
 */

/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_SUPERMICRO_EXTRA_FIRMWARE_INFO 0x20
#define IPMI_CMD_OEM_SUPERMICRO_RESET_INTRUSION     0x03

#ifdef __cplusplus
}
#endif

#endif /* ipmi-cmd-spec.h */
