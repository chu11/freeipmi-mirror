/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-dell-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/*
 * Dell Poweredge R610
 * Dell Poweredge R710
 * Dell Poweredge R720
 */

const char * const ipmi_generic_event_reading_type_code_oem_dell_status[] =
  {
    "Absent",
    "Standby",
    "IPMI Function ready",
    "Fully ready",
    "Offline",
    "Failed",
    "Active",
    "Booting",
    "Write protected",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_oem_dell_status_max_index = 0x08;

/*
 * Dell Poweredge R720
 */

const char * const ipmi_generic_event_reading_type_code_oem_dell_failure[] =
  {
    "undocumented",             /* not known yet */
    "undocumented",             /* not known yet */
    "undocumented",             /* not known yet */
    "Memory failed to transition to Online",
    NULL
  };
unsigned int ipmi_generic_event_reading_type_code_oem_dell_failure_max_index = 0x03;

/* achu:
 *
 * I have a feeling "good" is the random string they choose in some
 * code, it's not the real string.  But that's all I got to go on.
 *
 */

const char * const ipmi_sensor_type_oem_dell_system_performance_degradation_status[] =
  {
    "Good",
    "Degraded, other",
    "Degraded, thermal protection",
    "Degraded, cooling capacity change",
    "Degraded, power capacity change",
    "Degraded, user defined power capacity",
    "Halted, system power exceeds capacity",
    "Degraded, system power exceeds capacity",
    NULL
  };
unsigned int ipmi_sensor_type_oem_dell_system_performance_degradation_status_max_index = 0x07;

const char * const ipmi_sensor_type_oem_dell_link_tuning[] =
  {
    "Good",
    "Failed to program virtual MAC address",
    "Device option ROM failed to support link tuning or flex address",
    "Failed to get link tuning or flex address data",
    NULL
  };
unsigned int ipmi_sensor_type_oem_dell_link_tuning_max_index = 0x03;

const char * const ipmi_sensor_type_oem_dell_non_fatal_error[] =
  {
    "PCIe error",
    "undocumented",             /* not known yet */
    "QPI Link Degrade",
  };
unsigned int ipmi_sensor_type_oem_dell_non_fatal_error_max_index = 0x02;

const char * const ipmi_sensor_type_oem_dell_fatal_io_error[] =
  {
    "Successful",
    "Fatal IO error",
    NULL
  };
unsigned int ipmi_sensor_type_oem_dell_fatal_io_error_max_index = 0x01;

const char * const ipmi_sensor_type_oem_dell_upgrade[] =
  {
    "Successful",
    "Failed",
    NULL
  };
unsigned int ipmi_sensor_type_oem_dell_upgrade_max_index = 0x01;
