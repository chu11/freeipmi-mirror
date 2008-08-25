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

#ifndef _IPMI_IPMB_LUN_SPEC_H
#define	_IPMI_IPMB_LUN_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* BMC IPMI LUNs */

/* BMC commands and Event Request Messages:
   Event Request Messages received on this LUN are routed to the Event
   Receiver function in the BMC, and automatically logged if SEL
   logging is enabled
*/
#define IPMI_BMC_IPMB_LUN_BMC          0x00

/* OEM LUN 1:
   OEM reserved for BMC implementer / system integrator definition.
*/
#define IPMI_BMC_IPMB_LUN_OEM_LUN1     0x01

/* SMS Message LUN (Intended for messages to System Management
   Software): 
   Messages received on this LUN are routed to the Receive Message
   Queue and retrieved using a Read Message command. The SMS_Avail
   flag is set whenever the Receive Message Queue has valid contents. 
*/
#define IPMI_BMC_IPMB_LUN_SMS_MSG_LUN  0x02

/* OEM LUN 2:
   OEM reserved for BMC implementer / system integrator definition.
   */
#define IPMI_BMC_IPMB_LUN_OEM_LUN2     0x03

#define IPMI_BMC_LUN_VALID(__lun) \
        (((__lun) == IPMI_BMC_IPMB_LUN_BMC \
	  || (__lun) == IPMI_BMC_IPMB_LUN_OEM_LUN1 \
	  || (__lun) == IPMI_BMC_IPMB_LUN_SMS_MSG_LUN \
	  || (__lun) == IPMI_BMC_IPMB_LUN_OEM_LUN2) ? 1 : 0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-ipmb-lun-spec.h */
