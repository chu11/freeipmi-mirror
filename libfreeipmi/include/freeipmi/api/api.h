/* 
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

#ifndef _API_H
#define _API_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/api/ipmi-chassis-cmds-api.h>
#include <freeipmi/api/ipmi-device-global-cmds-api.h>
#include <freeipmi/api/ipmi-fru-inventory-device-cmds-api.h>
#include <freeipmi/api/ipmi-lan-cmds-api.h>
#include <freeipmi/api/ipmi-messaging-support-cmds-api.h>
#include <freeipmi/api/ipmi-pef-and-alerting-cmds-api.h>
#include <freeipmi/api/ipmi-rmcpplus-support-and-payload-cmds-api.h>
#include <freeipmi/api/ipmi-sdr-repository-cmds-api.h>
#include <freeipmi/api/ipmi-sensor-cmds-api.h>
#include <freeipmi/api/ipmi-sel-cmds-api.h>
#include <freeipmi/api/ipmi-serial-modem-cmds-api.h>
#include <freeipmi/api/ipmi-sol-cmds-api.h>

#ifdef __cplusplus
}
#endif

#endif /* _API_H */
