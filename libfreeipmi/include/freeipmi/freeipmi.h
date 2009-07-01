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

#ifndef _FREEIPMI_H
#define	_FREEIPMI_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/api/ipmi-chassis-cmds-api.h>
#include <freeipmi/api/ipmi-device-global-cmds-api.h>
#include <freeipmi/api/ipmi-event-cmds-api.h>
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
#include <freeipmi/cmds/ipmi-bmc-watchdog-timer-cmds.h>
#include <freeipmi/cmds/ipmi-chassis-cmds.h>
#include <freeipmi/cmds/ipmi-device-global-cmds.h>
#include <freeipmi/cmds/ipmi-event-cmds.h>
#include <freeipmi/cmds/ipmi-fru-inventory-device-cmds.h>
#include <freeipmi/cmds/ipmi-lan-cmds.h>
#include <freeipmi/cmds/ipmi-messaging-support-cmds.h>
#include <freeipmi/cmds/ipmi-pef-and-alerting-cmds.h>
#include <freeipmi/cmds/ipmi-rmcpplus-support-and-payload-cmds.h>
#include <freeipmi/cmds/ipmi-sdr-repository-cmds.h>
#include <freeipmi/cmds/ipmi-sel-cmds.h>
#include <freeipmi/cmds/ipmi-sensor-cmds.h>
#include <freeipmi/cmds/ipmi-serial-modem-cmds.h>
#include <freeipmi/cmds/ipmi-sol-cmds.h>
#include <freeipmi/cmds/rmcp-cmds.h>
#include <freeipmi/debug/ipmi-debug.h>
#include <freeipmi/driver/ipmi-kcs-driver.h>
#include <freeipmi/driver/ipmi-ssif-driver.h>
#include <freeipmi/driver/ipmi-openipmi-driver.h>
#include <freeipmi/driver/ipmi-sunbmc-driver.h>
#include <freeipmi/fiid/fiid.h>
#include <freeipmi/interface/ipmi-ipmb-interface.h>
#include <freeipmi/interface/ipmi-kcs-interface.h>
#include <freeipmi/interface/ipmi-lan-interface.h>
#include <freeipmi/interface/ipmi-rmcpplus-interface.h>
#include <freeipmi/interface/rmcp-interface.h>
#include <freeipmi/locate/ipmi-locate.h>
#include <freeipmi/record-format/ipmi-cipher-suite-record-format.h>
#include <freeipmi/record-format/ipmi-fru-information-record-format.h>
#include <freeipmi/record-format/ipmi-sdr-record-format.h>
#include <freeipmi/record-format/ipmi-sel-record-format.h>
#include <freeipmi/sdr-cache/ipmi-sdr-cache.h>
#include <freeipmi/spec/ipmi-authentication-type-spec.h>
#include <freeipmi/spec/ipmi-channel-spec.h>
#include <freeipmi/spec/ipmi-chassis-boot-options-parameter-spec.h>
#include <freeipmi/spec/ipmi-cmd-spec.h>
#include <freeipmi/spec/ipmi-comp-code-spec.h>
#include <freeipmi/spec/ipmi-event-reading-type-code-spec.h>
#include <freeipmi/spec/ipmi-fru-chassis-types-spec.h>
#include <freeipmi/spec/ipmi-fru-language-codes-spec.h>
#include <freeipmi/spec/ipmi-ipmb-lun-spec.h>
#include <freeipmi/spec/ipmi-lan-parameter-spec.h>
#include <freeipmi/spec/ipmi-netfn-spec.h>
#include <freeipmi/spec/ipmi-pef-parameter-spec.h>
#include <freeipmi/spec/ipmi-privilege-level-spec.h>
#include <freeipmi/spec/ipmi-rmcpplus-status-spec.h>
#include <freeipmi/spec/ipmi-sensor-types-spec.h>
#include <freeipmi/spec/ipmi-sensor-units-spec.h>
#include <freeipmi/spec/ipmi-serial-modem-parameter-spec.h>
#include <freeipmi/spec/ipmi-slave-address-spec.h>
#include <freeipmi/spec/ipmi-sol-parameter-spec.h>
#include <freeipmi/spec/ipmi-system-info-parameters-spec.h>
#include <freeipmi/spec/ipmi-system-software-id-spec.h>
#include <freeipmi/util/ipmi-cipher-suite-util.h>
#include <freeipmi/util/ipmi-error-util.h>
#include <freeipmi/util/ipmi-ipmb-util.h>
#include <freeipmi/util/ipmi-lan-util.h>
#include <freeipmi/util/ipmi-rmcpplus-util.h>
#include <freeipmi/util/ipmi-sensor-and-event-code-tables-util.h>
#include <freeipmi/util/ipmi-sensor-util.h>
#include <freeipmi/util/ipmi-util.h>
#include <freeipmi/util/rmcp-util.h>

#ifdef __cplusplus
}
#endif

#endif /* freeipmi.h */

