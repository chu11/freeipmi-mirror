/*****************************************************************************\
 *  $Id: ipmi_monitoring_parse_common.c,v 1.1 2010-03-19 22:07:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
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

  if (sensor_type == IPMI_SENSOR_TYPE_RESERVED)
    return (IPMI_MONITORING_SENSOR_TYPE_RESERVED);
  else if (sensor_type == IPMI_SENSOR_TYPE_TEMPERATURE)
    return (IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE);
  else if (sensor_type == IPMI_SENSOR_TYPE_VOLTAGE)
    return (IPMI_MONITORING_SENSOR_TYPE_VOLTAGE);
  else if (sensor_type == IPMI_SENSOR_TYPE_CURRENT)
    return (IPMI_MONITORING_SENSOR_TYPE_CURRENT);
  else if (sensor_type == IPMI_SENSOR_TYPE_FAN)
    return (IPMI_MONITORING_SENSOR_TYPE_FAN);
  else if (sensor_type == IPMI_SENSOR_TYPE_PHYSICAL_SECURITY)
    return (IPMI_MONITORING_SENSOR_TYPE_PHYSICAL_SECURITY);
  else if (sensor_type == IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT)
    return (IPMI_MONITORING_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT);
  else if (sensor_type == IPMI_SENSOR_TYPE_PROCESSOR)
    return (IPMI_MONITORING_SENSOR_TYPE_PROCESSOR);
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    return (IPMI_MONITORING_SENSOR_TYPE_POWER_SUPPLY);
  else if (sensor_type == IPMI_SENSOR_TYPE_POWER_UNIT)
    return (IPMI_MONITORING_SENSOR_TYPE_POWER_UNIT);
  else if (sensor_type == IPMI_SENSOR_TYPE_COOLING_DEVICE)
    return (IPMI_MONITORING_SENSOR_TYPE_COOLING_DEVICE);
  else if (sensor_type == IPMI_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR)
    return (IPMI_MONITORING_SENSOR_TYPE_OTHER_UNITS_BASED_SENSOR);
  else if (sensor_type == IPMI_SENSOR_TYPE_MEMORY)
    return (IPMI_MONITORING_SENSOR_TYPE_MEMORY);
  else if (sensor_type == IPMI_SENSOR_TYPE_DRIVE_SLOT)
    return (IPMI_MONITORING_SENSOR_TYPE_DRIVE_SLOT);
  else if (sensor_type == IPMI_SENSOR_TYPE_POST_MEMORY_RESIZE)
    return (IPMI_MONITORING_SENSOR_TYPE_POST_MEMORY_RESIZE);
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS)
    return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS);
  else if (sensor_type == IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED)
    return (IPMI_MONITORING_SENSOR_TYPE_EVENT_LOGGING_DISABLED);
  else if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG1)
    return (IPMI_MONITORING_SENSOR_TYPE_WATCHDOG1);
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_EVENT)
    return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_EVENT);
  else if (sensor_type == IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT)
    return (IPMI_MONITORING_SENSOR_TYPE_CRITICAL_INTERRUPT);
  else if (sensor_type == IPMI_SENSOR_TYPE_BUTTON_SWITCH)
    return (IPMI_MONITORING_SENSOR_TYPE_BUTTON_SWITCH);
  else if (sensor_type == IPMI_SENSOR_TYPE_MODULE_BOARD)
    return (IPMI_MONITORING_SENSOR_TYPE_MODULE_BOARD);
  else if (sensor_type == IPMI_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR)
    return (IPMI_MONITORING_SENSOR_TYPE_MICROCONTROLLER_COPROCESSOR);
  else if (sensor_type == IPMI_SENSOR_TYPE_ADD_IN_CARD)
    return (IPMI_MONITORING_SENSOR_TYPE_ADD_IN_CARD);
  else if (sensor_type == IPMI_SENSOR_TYPE_CHASSIS)
    return (IPMI_MONITORING_SENSOR_TYPE_CHASSIS);
  else if (sensor_type == IPMI_SENSOR_TYPE_CHIP_SET)
    return (IPMI_MONITORING_SENSOR_TYPE_CHIP_SET);
  else if (sensor_type == IPMI_SENSOR_TYPE_OTHER_FRU)
    return (IPMI_MONITORING_SENSOR_TYPE_OTHER_FRU);
  else if (sensor_type == IPMI_SENSOR_TYPE_CABLE_INTERCONNECT)
    return (IPMI_MONITORING_SENSOR_TYPE_CABLE_INTERCONNECT);
  else if (sensor_type == IPMI_SENSOR_TYPE_TERMINATOR)
    return (IPMI_MONITORING_SENSOR_TYPE_TERMINATOR);
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_BOOT_INITIATED)
    return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_BOOT_INITIATED);
  else if (sensor_type == IPMI_SENSOR_TYPE_BOOT_ERROR)
    return (IPMI_MONITORING_SENSOR_TYPE_BOOT_ERROR);
  else if (sensor_type == IPMI_SENSOR_TYPE_OS_BOOT)
    return (IPMI_MONITORING_SENSOR_TYPE_OS_BOOT);
  else if (sensor_type == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP)
    return (IPMI_MONITORING_SENSOR_TYPE_OS_CRITICAL_STOP);
  else if (sensor_type == IPMI_SENSOR_TYPE_SLOT_CONNECTOR)
    return (IPMI_MONITORING_SENSOR_TYPE_SLOT_CONNECTOR);
  else if (sensor_type == IPMI_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE)
    return (IPMI_MONITORING_SENSOR_TYPE_SYSTEM_ACPI_POWER_STATE);
  else if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
    return (IPMI_MONITORING_SENSOR_TYPE_WATCHDOG2);
  else if (sensor_type == IPMI_SENSOR_TYPE_PLATFORM_ALERT)
    return (IPMI_MONITORING_SENSOR_TYPE_PLATFORM_ALERT);
  else if (sensor_type == IPMI_SENSOR_TYPE_ENTITY_PRESENCE)
    return (IPMI_MONITORING_SENSOR_TYPE_ENTITY_PRESENCE);
  else if (sensor_type == IPMI_SENSOR_TYPE_MONITOR_ASIC_IC)
    return (IPMI_MONITORING_SENSOR_TYPE_MONITOR_ASIC_IC);
  else if (sensor_type == IPMI_SENSOR_TYPE_LAN)
    return (IPMI_MONITORING_SENSOR_TYPE_LAN);
  else if (sensor_type == IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH)
    return (IPMI_MONITORING_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH);
  else if (sensor_type == IPMI_SENSOR_TYPE_BATTERY)
    return (IPMI_MONITORING_SENSOR_TYPE_BATTERY);
  else if (sensor_type == IPMI_SENSOR_TYPE_SESSION_AUDIT)
    return (IPMI_MONITORING_SENSOR_TYPE_SESSION_AUDIT);
  else if (sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE)
    return (IPMI_MONITORING_SENSOR_TYPE_VERSION_CHANGE);
  else if (sensor_type == IPMI_SENSOR_TYPE_FRU_STATE)
    return (IPMI_MONITORING_SENSOR_TYPE_FRU_STATE);

  return (sensor_type);
}

