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
#include <freeipmi/ipmi-authentication-type-spec.h>
#include <freeipmi/ipmi-channel-spec.h>
#include <freeipmi/ipmi-cipher-suite-spec.h>
#include <freeipmi/ipmi-cmd-spec.h>
#include <freeipmi/ipmi-comp-code-spec.h>
#include <freeipmi/ipmi-ipmb-interface.h>
#include <freeipmi/ipmi-lan-parameter-spec.h>
#include <freeipmi/ipmi-netfn-spec.h>
#include <freeipmi/ipmi-pef-parameter-spec.h>
#include <freeipmi/ipmi-privilege-level-spec.h>
#include <freeipmi/ipmi-rmcpplus-status-spec.h>
#include <freeipmi/ipmi-sensor-types-spec.h>
#include <freeipmi/ipmi-sensor-units-spec.h>
#include <freeipmi/ipmi-serial-modem-parameter-spec.h>
#include <freeipmi/ipmi-sol-parameter-spec.h>
#include <freeipmi/ipmi-slave-address-spec.h>
#include <freeipmi/rmcp.h>
#include <freeipmi/ipmi-debug.h>
#include <freeipmi/ipmi-error.h>
#include <freeipmi/ipmi-utils.h>
#include <freeipmi/ipmi-locate.h>
#include <freeipmi/ipmi-kcs.h>
#include <freeipmi/ipmi-kcs-api.h>
#include <freeipmi/ipmi-lan.h>
#include <freeipmi/ipmi-lan-interface.h>
#include <freeipmi/ipmi-lan-utils.h>
#include <freeipmi/ipmi-crypt.h>
#include <freeipmi/ipmi-rmcpplus.h>
#include <freeipmi/ipmi-rmcpplus-interface.h>
#include <freeipmi/ipmi-rmcpplus-utils.h>
#include <freeipmi/ipmi-smic-api.h>
#include <freeipmi/ipmi-ssif-api.h>
#include <freeipmi/ipmi-bmc-watchdog-timer-cmds.h>
#include <freeipmi/ipmi-chassis-cmds.h>
#include <freeipmi/ipmi-chassis-boot-options-parameter-spec.h>
#include <freeipmi/ipmi-device-global-cmds.h>
#include <freeipmi/ipmi-lan-cmds.h>
#include <freeipmi/ipmi-messaging-support-cmds.h>
#include <freeipmi/ipmi-pef-and-alerting-cmds.h>
#include <freeipmi/ipmi-rmcpplus-support-and-payload-cmds.h>
#include <freeipmi/ipmi-sdr-repository-cmds.h>
#include <freeipmi/ipmi-sel-cmds.h>
#include <freeipmi/ipmi-sensor-cmds.h>
#include <freeipmi/ipmi-serial-modem-cmds.h>
#include <freeipmi/ipmi-sol-cmds.h>
#include <freeipmi/ipmi-sdr-record-types.h>
#include <freeipmi/ipmi-sel-record-types.h>
#include <freeipmi/ipmi-sensor-and-event-code-tables.h>
#include <freeipmi/ipmi-sensor-utils.h>

#ifdef __cplusplus
}
#endif

#endif /* freeipmi.h */

