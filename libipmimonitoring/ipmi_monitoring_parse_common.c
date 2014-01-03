/*****************************************************************************\
 *  $Id: ipmi_monitoring_parse_common.c,v 1.1 2010-03-19 22:07:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"

#include "freeipmi-portability.h"

int
ipmi_monitoring_get_sensor_type (ipmi_monitoring_ctx_t c,
                                 uint8_t sensor_type)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  switch (sensor_type)
    {
    case IPMI_SENSOR_TYPE_RESERVED:
      return (IPMI_MONITORING_SENSOR_TYPE_RESERVED);
    case IPMI_SENSOR_TYPE_TEMPERATURE:
      return (IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE);
    case IPMI_SENSOR_TYPE_VOLTAGE:
      return (IPMI_MONITORING_SENSOR_TYPE_VOLTAGE);
    case IPMI_SENSOR_TYPE_CURRENT:
      return (IPMI_MONITORING_SENSOR_TYPE_CURRENT);
    case IPMI_SENSOR_TYPE_FAN:
      return (IPMI_MONITORING_SENSOR_TYPE_FAN);
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (IPMI_MONITORING_SENSOR_TYPE_PHYSICAL_SECURITY);
    case IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return (IPMI_MONITORING_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT);
    case IPMI_SENSOR_TYPE_PROCESSOR:
      return (IPMI_MONITORING_SENSOR_TYPE_PROCESSOR);
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (IPMI_MONITORING_SENSOR_TYPE_POWER_SUPPLY);
    case IPMI_SENSOR_TYPE_POWER_UNIT:
      return (IPMI_MONITORING_SENSOR_TYPE_POWER_UNIT);
    case IPMI_SENSOR_TYPE_COOLING_DEVICE:
      return (IPMI_MONITORING_SENSOR_TYPE_COOLING_DEVICE);
    case IPMI_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR:
      return (IPMI_MONITORING_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR);
    case IPMI_SENSOR_TYPE_MEMORY:
      return (IPMI_MONITORING_SENSOR_TYPE_MEMORY);
    case IPMI_SENSOR_TYPE_DRIVE_SLOT:
      return (IPMI_MONITORING_SENSOR_TYPE_DRIVE_SLOT);
    case IPMI_SENSOR_TYPE_POST_MEMORY_RESIZE:
      return (IPMI_MONITORING_SENSOR_TYPE_POST_MEMORY_RESIZE);
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS);
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (IPMI_MONITORING_SENSOR_TYPE_EVENT_LOGGING_DISABLED);
    case IPMI_SENSOR_TYPE_WATCHDOG1:
      return (IPMI_MONITORING_SENSOR_TYPE_WATCHDOG1);
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_EVENT);
    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return (IPMI_MONITORING_SENSOR_TYPE_CRITICAL_INTERRUPT);
    case IPMI_SENSOR_TYPE_BUTTON_SWITCH:
      return (IPMI_MONITORING_SENSOR_TYPE_BUTTON_SWITCH);
    case IPMI_SENSOR_TYPE_MODULE_BOARD:
      return (IPMI_MONITORING_SENSOR_TYPE_MODULE_BOARD);
    case IPMI_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR:
      return (IPMI_MONITORING_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR);
    case IPMI_SENSOR_TYPE_ADD_IN_CARD:
      return (IPMI_MONITORING_SENSOR_TYPE_ADD_IN_CARD);
    case IPMI_SENSOR_TYPE_CHASSIS:
      return (IPMI_MONITORING_SENSOR_TYPE_CHASSIS);
    case IPMI_SENSOR_TYPE_CHIP_SET:
      return (IPMI_MONITORING_SENSOR_TYPE_CHIP_SET);
    case IPMI_SENSOR_TYPE_OTHER_FRU:
      return (IPMI_MONITORING_SENSOR_TYPE_OTHER_FRU);
    case IPMI_SENSOR_TYPE_CABLE_INTERCONNECT:
      return (IPMI_MONITORING_SENSOR_TYPE_CABLE_INTERCONNECT);
    case IPMI_SENSOR_TYPE_TERMINATOR:
      return (IPMI_MONITORING_SENSOR_TYPE_TERMINATOR);
    case IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED:
      return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_BOOT_INITIATED);
    case IPMI_SENSOR_TYPE_BOOT_ERROR:
      return (IPMI_MONITORING_SENSOR_TYPE_BOOT_ERROR);
    case IPMI_SENSOR_TYPE_OS_BOOT:
      return (IPMI_MONITORING_SENSOR_TYPE_OS_BOOT);
    case IPMI_SENSOR_TYPE_OS_CRITICAL_STOP:
      return (IPMI_MONITORING_SENSOR_TYPE_OS_CRITICAL_STOP);
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (IPMI_MONITORING_SENSOR_TYPE_SLOT_CONNECTOR);
    case IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE:
      return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE);
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (IPMI_MONITORING_SENSOR_TYPE_WATCHDOG2);
    case IPMI_SENSOR_TYPE_PLATFORM_ALERT:
      return (IPMI_MONITORING_SENSOR_TYPE_PLATFORM_ALERT);
    case IPMI_SENSOR_TYPE_ENTITY_PRESENCE:
      return (IPMI_MONITORING_SENSOR_TYPE_ENTITY_PRESENCE);
    case IPMI_SENSOR_TYPE_MONITOR_ASIC_IC:
      return (IPMI_MONITORING_SENSOR_TYPE_MONITOR_ASIC_IC);
    case IPMI_SENSOR_TYPE_LAN:
      return (IPMI_MONITORING_SENSOR_TYPE_LAN);
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (IPMI_MONITORING_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH);
    case IPMI_SENSOR_TYPE_BATTERY:
      return (IPMI_MONITORING_SENSOR_TYPE_BATTERY);
    case IPMI_SENSOR_TYPE_SESSION_AUDIT:
      return (IPMI_MONITORING_SENSOR_TYPE_SESSION_AUDIT);
    case IPMI_SENSOR_TYPE_VERSION_CHANGE:
      return (IPMI_MONITORING_SENSOR_TYPE_VERSION_CHANGE);
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (IPMI_MONITORING_SENSOR_TYPE_FRU_STATE);
    }

  return (sensor_type);
}

