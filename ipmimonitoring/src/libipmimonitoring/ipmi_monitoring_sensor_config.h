/*****************************************************************************\
 *  $Id: ipmi_monitoring_sensor_config.h,v 1.7 2008-04-02 22:02:44 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 2 of the License, or (at your
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

#ifndef _IPMI_MONITORING_SENSOR_CONFIG_H
#define _IPMI_MONITORING_SENSOR_CONFIG_H

#include "ipmi_monitoring.h"

extern struct ipmi_sensor_config ipmi_threshold_sensor_config[];

extern struct ipmi_sensor_config ipmi_voltage_performance_config[];
extern struct ipmi_sensor_config ipmi_fan_device_install_config[];
extern struct ipmi_sensor_config ipmi_module_board_state_config[];
extern struct ipmi_sensor_config ipmi_module_board_device_install_config[];
extern struct ipmi_sensor_config ipmi_power_unit_redundancy_config[];
extern struct ipmi_sensor_config ipmi_drive_slot_device_install_config[];

extern struct ipmi_sensor_config ipmi_physical_security_config[];
extern struct ipmi_sensor_config ipmi_platform_security_violation_attempt_config[];
extern struct ipmi_sensor_config ipmi_processor_config[];
extern struct ipmi_sensor_config ipmi_power_supply_config[];
extern struct ipmi_sensor_config ipmi_power_unit_config[];
extern struct ipmi_sensor_config ipmi_memory_config[];
extern struct ipmi_sensor_config ipmi_drive_slot_config[];
extern struct ipmi_sensor_config ipmi_system_firmware_progress_config[];
extern struct ipmi_sensor_config ipmi_event_logging_disabled_config[];
extern struct ipmi_sensor_config ipmi_system_event_config[];
extern struct ipmi_sensor_config ipmi_critical_interrupt_config[];
extern struct ipmi_sensor_config ipmi_cable_interconnect_config[];
extern struct ipmi_sensor_config ipmi_slot_connector_config[];
extern struct ipmi_sensor_config ipmi_watchdog2_config[];

int ipmi_monitoring_sensor_config(int *errnum);

#endif /* _IPMI_MONITORING_SENSOR_CONFIG_H */
