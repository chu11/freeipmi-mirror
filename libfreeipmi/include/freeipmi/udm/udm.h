/* 
   udm.h: IPMI Unified Driver Model (API interface for all IPMI drivers)

   Copyright (C) 2005 FreeIPMI Core Team
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

#ifndef _UDM_H
#define _UDM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/udm/ipmi-udm.h>
#include <freeipmi/udm/ipmi-chassis-cmds-udm.h>
#include <freeipmi/udm/ipmi-device-global-cmds-udm.h>
#include <freeipmi/udm/ipmi-fru-inventory-device-cmds-udm.h>
#include <freeipmi/udm/ipmi-lan-cmds-udm.h>
#include <freeipmi/udm/ipmi-messaging-support-cmds-udm.h>
#include <freeipmi/udm/ipmi-pef-and-alerting-cmds-udm.h>
#include <freeipmi/udm/ipmi-rmcpplus-support-and-payload-cmds-udm.h>
#include <freeipmi/udm/ipmi-sdr-repository-cmds-udm.h>
#include <freeipmi/udm/ipmi-sensor-cmds-udm.h>
#include <freeipmi/udm/ipmi-sel-cmds-udm.h>
#include <freeipmi/udm/ipmi-serial-modem-cmds-udm.h>
#include <freeipmi/udm/ipmi-sol-cmds-udm.h>

#ifdef __cplusplus
}
#endif

#endif /* _UDM_H */
