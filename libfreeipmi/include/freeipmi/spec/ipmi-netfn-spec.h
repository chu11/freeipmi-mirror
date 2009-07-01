/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifndef _IPMI_NETFN_SPEC_H
#define	_IPMI_NETFN_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Notes:
   Refer to IPMIv1_5_rev1_1.pdf Table 5-1, Network Function Codes
   for complete description
*/
#define IPMI_NET_FN_CHASSIS_RQ         0x00
#define IPMI_NET_FN_CHASSIS_RS	       0x01
#define IPMI_NET_FN_BRIDGE_RQ	       0x02
#define IPMI_NET_FN_BRIDGE_RS	       0x03
#define IPMI_NET_FN_SENSOR_EVENT_RQ    0x04
#define IPMI_NET_FN_SENSOR_EVENT_RS    0x05
#define IPMI_NET_FN_APP_RQ	       0x06
#define IPMI_NET_FN_APP_RS	       0x07
#define IPMI_NET_FN_FIRMWARE_RQ	       0x08
#define IPMI_NET_FN_FIRMWARE_RS	       0x09
#define IPMI_NET_FN_STORAGE_RQ	       0x0A
#define IPMI_NET_FN_STORAGE_RS	       0x0B
#define IPMI_NET_FN_TRANSPORT_RQ       0x0C
#define IPMI_NET_FN_TRANSPORT_RS       0x0D
/*
   0x0E to 0x2B RESERVED
   0x2C to 0x2D IPMI_NET_FN_GRP
   0x2E to 0x2F IPMI_NET_FN_OEM_GRP
   0x30 to 0x3F IPMI_NET_FN_CNTRLR_OEM_GRP
*/

/* To avoid gcc warnings, added +1 and -1 in comparison */
/* Include checks for possible oem network functions */
#define IPMI_NET_FN_VALID(__net_fn) \
        ((((__net_fn+1) >= IPMI_NET_FN_CHASSIS_RS \
           && (__net_fn-1) <= IPMI_NET_FN_TRANSPORT_RQ) \
          || ((__net_fn) >= 0x2C \
              && (__net_fn) <= 0x3F)) ? 1 : 0)

#define IPMI_NET_FN_RQ_VALID(__net_fn)           \
  ((IPMI_NET_FN_VALID (__net_fn)                 \
    && (!((__net_fn) & 0x1))) ? 1 : 0)

#define IPMI_NET_FN_RS_VALID(__net_fn)           \
  ((IPMI_NET_FN_VALID (__net_fn)                 \
    && ((__net_fn) & 0x1)) ? 1 : 0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-netfn-spec.h */
