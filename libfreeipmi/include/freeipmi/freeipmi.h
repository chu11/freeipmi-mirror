/* 
   freeipmi.h - C library interface to IPMI

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _FREEIPMI_H
#define	_FREEIPMI_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/fiid.h>
#include <freeipmi/ipmi-cmd-spec.h>
#include <freeipmi/ipmi-comp-code-spec.h>
#include <freeipmi/ipmi-ipmb-interface.h>
#include <freeipmi/ipmi-lan-param-spec.h>
#include <freeipmi/ipmi-netfn-spec.h>
#include <freeipmi/ipmi-pef-param-spec.h>
#include <freeipmi/ipmi-sensor-types-spec.h>
#include <freeipmi/ipmi-sensor-units-spec.h>
#include <freeipmi/ipmi-serial-modem-param-spec.h>
#include <freeipmi/ipmi-sol-param-spec.h>
#include <freeipmi/rmcp.h>
#include <freeipmi/ipmi-debug.h>
#include <freeipmi/ipmi-error.h>
#include <freeipmi/ipmi-utils.h>
#include <freeipmi/ipmi-locate.h>
#include <freeipmi/ipmi-kcs-interface.h>
#include <freeipmi/ipmi-lan-interface.h>
#include <freeipmi/ipmi-ssif-interface.h>
#include <freeipmi/ipmi-smic-interface.h>
#include <freeipmi/ipmi-bmc-watchdog-timer-cmds.h>
#include <freeipmi/ipmi-chassis-cmds.h>
#include <freeipmi/ipmi-device-global-cmds.h>
#include <freeipmi/ipmi-lan-cmds.h>
#include <freeipmi/ipmi-messaging-support-cmds.h>
#include <freeipmi/ipmi-pef-and-alerting-cmds.h>
#include <freeipmi/ipmi-sdr-repository-cmds.h>
#include <freeipmi/ipmi-sel-cmds.h>
#include <freeipmi/ipmi-sensor-cmds.h>
#include <freeipmi/ipmi-serial-modem-cmds.h>
#include <freeipmi/ipmi-sol-cmds.h>
#include <freeipmi/ipmi-sdr-record-types.h>
#include <freeipmi/ipmi-sel-record-types.h>
#include <freeipmi/ipmi-sensor-event-messages.h>
#include <freeipmi/ipmi-sensor-utils.h>
#include <freeipmi/udm/ipmi-udm.h>
#include <freeipmi/udm/ipmi-kcs-interface-udm.h>
#include <freeipmi/udm/ipmi-lan-interface-udm.h>
#include <freeipmi/udm/ipmi-ssif-interface-udm.h>
#include <freeipmi/udm/ipmi-chassis-cmds-udm.h>
#include <freeipmi/udm/ipmi-device-global-cmds-udm.h>
#include <freeipmi/udm/ipmi-lan-cmds-udm.h>
#include <freeipmi/udm/ipmi-messaging-support-cmds-udm.h>
#include <freeipmi/udm/ipmi-pef-and-alerting-cmds-udm.h>
#include <freeipmi/udm/ipmi-sdr-repository-cmds-udm.h>
#include <freeipmi/udm/ipmi-sensor-cmds-udm.h>
#include <freeipmi/udm/ipmi-sel-cmds-udm.h>
#include <freeipmi/udm/ipmi-serial-modem-cmds-udm.h>
#include <freeipmi/udm/ipmi-sol-cmds-udm.h>

#ifdef __cplusplus
}
#endif

#endif /* freeipmi.h */

