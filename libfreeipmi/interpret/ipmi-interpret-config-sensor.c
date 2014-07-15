/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
/*****************************************************************************\
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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-oem-spec.h"

#include "ipmi-interpret-defs.h"
#include "ipmi-interpret-trace.h"
#include "ipmi-interpret-config-common.h"
#include "ipmi-interpret-config-sensor.h"
#include "ipmi-interpret-util.h"

#include "freeipmi-portability.h"
#include "conffile.h"
#include "hash.h"

/*
 * Standard Sensors
 */

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_threshold_config[] =
  {
    { "IPMI_Threshold_Sensor_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Critical_Threshold", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Critical_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Recoverable_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Critical_Threshold", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Critical_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Recoverable_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_threshold_config_len = 7;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_temperature_state_config[] =
  {
    { "IPMI_Temperature_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Temperature_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Temperature_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_temperature_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_temperature_limit_config[] =
  {
    { "IPMI_Temperature_Limit_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Temperature_Limit_Not_Exceeded", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Temperature_Limit_Exceeded", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_temperature_limit_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_temperature_transition_severity_config[] =
  {
    { "IPMI_Temperature_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Temperature_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Temperature_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Temperature_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Temperature_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Temperature_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Temperature_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Temperature_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Temperature_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Temperature_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_temperature_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_voltage_state_config[] =
  {
    { "IPMI_Voltage_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_voltage_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_voltage_limit_config[] =
  {
    { "IPMI_Voltage_Limit_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_Limit_Not_Exceeded", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_Limit_Exceeded", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_voltage_limit_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_voltage_performance_config[] =
  {
    { "IPMI_Voltage_Performance_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_Performance_Met", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_Performance_Lags", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_voltage_performance_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_voltage_transition_severity_config[] =
  {
    { "IPMI_Voltage_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Voltage_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Voltage_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Voltage_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Voltage_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Voltage_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Voltage_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Voltage_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_voltage_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_current_transition_severity_config[] =
  {
    { "IPMI_Current_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Current_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Current_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Current_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Current_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Current_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Current_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Current_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Current_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Current_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_current_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_fan_state_config[] =
  {
    { "IPMI_Fan_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_fan_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_fan_transition_severity_config[] =
  {
    { "IPMI_Fan_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_fan_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_fan_device_present_config[] =
  {
    { "IPMI_Fan_Device_Present_Device_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_fan_device_present_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_fan_transition_availability_config[] =
  {
    { "IPMI_Fan_Transition_Availability_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Transition_Availability_To_Running", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Transition_Availability_To_In_Test", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Power_Off", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_On_Line", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Off_Line", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Off_Duty", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Degraded", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Transition_Availability_To_Power_Save", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_Install_Error", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_fan_transition_availability_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_fan_redundancy_config[] =
  {
    { "IPMI_Fan_Redundancy_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_fan_redundancy_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_physical_security_config[] =
  {
    { "IPMI_Physical_Security_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Physical_Security_General_Chassis_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_Drive_Bay_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_IO_Card_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_Processor_Area_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_LAN_Leash_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_Unauthorized_Dock_Undock", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_FAN_Area_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_physical_security_config_len = 8;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_platform_security_violation_attempt_config[] =
  {
    { "IPMI_Platform_Security_Violation_Attempt_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Platform_Security_Violation_Attempt_Secure_Mode_Violation_Attempt", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_User_Password", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Attempt_Setup_Password", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Network_Boot_Password", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Other_Pre_Boot_Password_Violation", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Out_Of_Band_Access_Password_Violation", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_platform_security_violation_attempt_config_len = 7;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_processor_config[] =
  {
    { "IPMI_Processor_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Processor_IERR", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_Thermal_Trip", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_FRB1_BIST_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_FRB2_Hang_In_POST_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_FRB3_Processor_Startup_Initialization_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_Configuration_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_SMBIOS_Uncorrectable_CPU_Complex_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_Processor_Presence_Detected", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Processor_Processor_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_Terminator_Presence_Detected", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_Processor_Automatically_Throttled", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Processor_Machine_Check_Exception", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Processor_Correctable_Machine_Check_Error", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_processor_config_len = 14;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_processor_state_config[] =
  {
    { "IPMI_Processor_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Processor_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Processor_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_processor_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_power_supply_config[] =
  {
    { "IPMI_Power_Supply_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Presence_Detected", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Power_Supply_Failure_Detected", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Predictive_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Lost_AC_DC", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Lost_Or_Out_Of_Range", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Out_Of_Range_But_Present", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Configuration_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Inactive", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_power_supply_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_power_supply_state_config[] =
  {
    { "IPMI_Power_Supply_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_power_supply_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_power_supply_transition_severity_config[] =
  {
    { "IPMI_Power_Supply_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Supply_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Supply_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Supply_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_power_supply_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_power_supply_redundancy_config[] =
  {
    { "IPMI_Power_Supply_Redundancy_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_power_supply_redundancy_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_power_unit_config[] =
  {
    { "IPMI_Power_Unit_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_Power_Off_Power_Down", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_Power_Cycle", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_240VA_Power_Down", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_Interlock_Power_Down", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_AC_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Soft_Power_Control_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Power_Unit_Failure_Detected", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Predictive_Failure", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_power_unit_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_power_unit_device_present_config[] =
  {
    { "IPMI_Power_Unit_Device_Present_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_power_unit_device_present_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_power_unit_redundancy_config[] =
  {
    { "IPMI_Power_Unit_Redundancy_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_power_unit_redundancy_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_cooling_device_redundancy_config[] =
  {
    { "IPMI_Cooling_Device_Redundancy_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Cooling_Device_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Cooling_Device_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cooling_Device_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Cooling_Device_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cooling_Device_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cooling_Device_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cooling_Device_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Cooling_Device_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_cooling_device_redundancy_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_memory_config[] =
  {
    { "IPMI_Memory_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_Correctable_Memory_Error", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Uncorrectable_Memory_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Parity", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Memory_Scrub_Failed", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Memory_Device_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Correctable_Memory_Error_Logging_Limit_Reached", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Presence_Detected", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_Configuration_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Spare", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_Memory_Automatically_Throttled", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Critical_Overtemperature", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_memory_config_len = 12;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_memory_state_config[] =
  {
    { "IPMI_Memory_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_memory_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_memory_transition_severity_config[] =
  {
    { "IPMI_Memory_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_memory_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_memory_redundancy_config[] =
  {
    { "IPMI_Memory_Redundancy_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Memory_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Memory_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Memory_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_memory_redundancy_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_drive_slot_config[] =
  {
    { "IPMI_Drive_Slot_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Drive_Presence", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Drive_Fault", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Predictive_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Hot_Spare", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Consistency_Check_Parity_Check_In_Progress", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_In_Critical_Array", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Drive_Slot_In_Failed_Array", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Rebuild_Remap_In_Progress", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Rebuild_Remap_Aborted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_drive_slot_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_drive_slot_state_config[] =
  {
    { "IPMI_Drive_Slot_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_State_Deasserted", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Drive_Slot_State_Asserted", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_drive_slot_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_drive_slot_predictive_failure_config[] =
  {
    { "IPMI_Drive_Slot_Predictive_Failure_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Predictive_Failure_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Predictive_Failure_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_drive_slot_predictive_failure_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_drive_slot_device_present_config[] =
  {
    { "IPMI_Drive_Slot_Device_Present_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_drive_slot_device_present_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_post_memory_resize_state_config[] =
  {
    { "IPMI_Post_Memory_Resize_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Post_Memory_Resize_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Post_Memory_Resize_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_post_memory_resize_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_system_firmware_progress_config[] =
  {
    { "IPMI_System_Firmware_Progress_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Firmware_Progress_System_Firmware_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_System_Firmware_Hang", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_System_Firmware_Progress", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_system_firmware_progress_config_len = 4;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_system_firmware_progress_transition_severity_config[] =
  {
    { "IPMI_System_Firmware_Progress_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_System_Firmware_Progress_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_system_firmware_progress_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_event_logging_disabled_config[] =
  {
    { "IPMI_Event_Logging_Disabled_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Event_Logging_Disabled_Correctable_Memory_Error_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_Event_Type_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_Log_Area_Reset_Cleared", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Event_Logging_Disabled_All_Event_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_SEL_Full", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_SEL_Almost_Full", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Event_Logging_Disabled_Correctable_Machine_Check_Error_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_event_logging_disabled_config_len = 8;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_system_event_config[] =
  {
    { "IPMI_System_Event_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_System_Reconfigured", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_System_Event_OEM_System_Boot_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_Undetermined_System_Hardware_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Event_Entry_Added_To_Auxiliary_Log", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_PEF_Action", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_Timestamp_Clock_Sync", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_system_event_config_len = 7;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_system_event_transition_state_config[] =
  {
    { "IPMI_System_Event_Transition_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_Transition_State_Idle", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_Transition_State_Active", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_Transition_State_Busy", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_system_event_transition_state_config_len = 4;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_system_event_state_config[] =
  {
    { "IPMI_System_Event_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_system_event_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_critical_interrupt_config[] =
  {
    { "IPMI_Critical_Interrupt_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Critical_Interrupt_Front_Panel_NMI_Diagnostic_Interrupt", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Timeout", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_IO_Channel_Check_NMI", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Software_NMI", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Critical_Interrupt_PCI_PERR", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_PCI_SERR", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_EISA_Fail_Safe_Timeout", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Correctable_Error", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Critical_Interrupt_Bus_Uncorrectable_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Fatal_NMI", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Fatal_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Degraded", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_critical_interrupt_config_len = 13;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_button_switch_config[] =
  {
    { "IPMI_Button_Switch_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_Power_Button_Pressed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_Sleep_Button_Pressed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_Reset_Button_Pressed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_FRU_Latch_Open", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Button_Switch_FRU_Service_Request_Button", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_button_switch_config_len = 6;

/* achu: for a button/switch states, I don't think users really care.
 * So report Nominal for all states.
 */
static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_button_switch_state_config[] =
  {
    { "IPMI_Button_Switch_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_State_Asserted", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_button_switch_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_button_switch_transition_severity_config[] =
  {
    { "IPMI_Button_Switch_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Button_Switch_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Button_Switch_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Button_Switch_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Button_Switch_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Button_Switch_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Button_Switch_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Button_Switch_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_button_switch_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_module_board_state_config[] =
  {
    { "IPMI_Module_Board_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Module_Board_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Module_Board_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_module_board_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_module_board_device_present_config[] =
  {
    { "IPMI_Module_Board_Device_Present_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Module_Board_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Module_Board_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_module_board_device_present_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_chassis_transition_severity_config[] =
  {
    { "IPMI_Chassis_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Chassis_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Chassis_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Chassis_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chassis_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chassis_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Chassis_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chassis_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chassis_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Chassis_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_chassis_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_chip_set_transition_severity_config[] =
  {
    { "IPMI_Chip_Set_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Chip_Set_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Chip_Set_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Chip_Set_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chip_Set_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chip_Set_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Chip_Set_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chip_Set_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chip_Set_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Chip_Set_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_chip_set_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_cable_interconnect_config[] =
  {
    { "IPMI_Cable_Interconnect_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Cable_Interconnect_Is_Connected", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Cable_Interconnect_Configuration_Error", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_cable_interconnect_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_cable_interconnect_transition_severity_config[] =
  {
    { "IPMI_Cable_Interconnect_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Cable_Interconnect_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Cable_Interconnect_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Cable_Interconnect_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cable_Interconnect_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cable_Interconnect_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Cable_Interconnect_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cable_Interconnect_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Cable_Interconnect_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Cable_Interconnect_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_cable_interconnect_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_boot_error_config[] =
  {
    { "IPMI_Boot_Error_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Boot_Error_No_Bootable_Media", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Non_Bootable_Diskette_Left_In_Drive", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_PXE_Server_Not_Found", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Invalid_Boot_Sector", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Timeout_Waiting_For_User_Selection_Of_Boot_Source", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_boot_error_config_len = 6;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_boot_error_state_config[] =
  {
    { "IPMI_Boot_Error_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Boot_Error_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Boot_Error_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_boot_error_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_boot_error_transition_severity_config[] =
  {
    { "IPMI_Boot_Error_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Boot_Error_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Boot_Error_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Boot_Error_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Boot_Error_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Boot_Error_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_boot_error_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_os_boot_config[] =
  {
    { "IPMI_OS_Boot_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_A_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_C_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_PXE_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_Diagnostic_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_CD_ROM_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_ROM_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_Boot_Completed_Boot_Device_Not_Specified", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_OS_Boot_Base_OS_Hypervisor_Installation_Started", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_Base_OS_Hypervisor_Installation_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_Base_OS_Hypervisor_Installation_Aborted", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_OS_Boot_Base_OS_Hypervisor_Installation_Failed", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_os_boot_config_len = 12;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_os_critical_stop_state_config[] =
  {
    { "IPMI_OS_Critical_Stop_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Critical_Stop_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Critical_Stop_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_os_critical_stop_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_slot_connector_config[] =
  {
    { "IPMI_Slot_Connector_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Fault_Status_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Slot_Connector_Identify_Status_Asserted", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Slot_Connector_Slot_Connector_Device_Installed_Attached", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Connector_Ready_For_Device_Installation", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Connector_Ready_For_Device_Removal", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Power_Is_Off", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Connector_Device_Removal_Request", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Slot_Connector_Interlock_Asserted", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Slot_Connector_Slot_Is_Disabled", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Slot_Connector_Slot_Holds_Spare_Device", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_slot_connector_config_len = 11;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_slot_connector_transition_severity_config[] =
  {
    { "IPMI_Slot_Connector_Transition_Severity_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Transition_Severity_Transition_To_OK", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Transition_Severity_Transition_To_Non_Critical_From_OK", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Slot_Connector_Transition_Severity_Transition_To_Critical_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Slot_Connector_Transition_Severity_Transition_To_Non_Recoverable_From_Less_Severe", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Slot_Connector_Transition_Severity_Transition_To_Non_Critical_From_More_Severe", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Slot_Connector_Transition_Severity_Transition_To_Critical_From_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Slot_Connector_Transition_Severity_Transition_To_Non_Recoverable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Slot_Connector_Transition_Severity_Monitor", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Slot_Connector_Transition_Severity_Informational", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_slot_connector_transition_severity_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_system_acpi_power_state_config[] =
  {
    { "IPMI_System_ACPI_Power_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S0_G0", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S1", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S2", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S3", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S4", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S5_G2", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S4_S5_Soft_Off", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_G3_Mechanical_Off", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Sleeping_in_an_S1_S2_or_S3_States", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_G1_Sleeping", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S5_Entered_By_Override", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Legacy_ON_State", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Legacy_OFF_State", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Unspecified", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_ACPI_Power_State_Unknown", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_system_acpi_power_state_config_len = 16;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_watchdog2_config[] =
  {
    { "IPMI_Watchdog2_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Watchdog2_Timer_Expired", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Watchdog2_Hard_Reset", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Watchdog2_Power_Down", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Watchdog2_Power_Cycle", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Watchdog2_Reserved1", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Watchdog2_Reserved2", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Watchdog2_Reserved3", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Watchdog2_Reserved4", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Watchdog2_Timer_Interrupt", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_sensor_watchdog2_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_platform_alert_state_config[] =
  {
    { "IPMI_Platform_Alert_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Platform_Alert_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Platform_Alert_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_platform_alert_state_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_entity_presence_config[] =
  {
    { "IPMI_Entity_Presence_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Entity_Presence_Entity_Present", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Entity_Presence_Entity_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Entity_Presence_Entity_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_entity_presence_config_len = 4;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_entity_presence_device_present_config[] =
  {
    { "IPMI_Entity_Presence_Device_Present_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Entity_Presence_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Entity_Presence_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_entity_presence_device_present_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_management_subsystem_health_config[] =
  {
    { "IPMI_Management_Subsystem_Health_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Management_Subsystem_Health_Sensor_Access_Degraded_Or_Unavailable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Controller_Access_Degraded_Or_Unavailable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Management_Controller_Off_Line", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Management_Controller_Unavailable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Sensor_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_FRU_Failure", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_management_subsystem_health_config_len = 7;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_battery_config[] =
  {
    { "IPMI_Battery_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Battery_Battery_Low", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Battery_Battery_Failed", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Battery_Battery_Presence_Detected", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_battery_config_len = 4;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_session_audit_config[] =
  {
    { "IPMI_Session_Audit_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Session_Audit_Session_Activated", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Session_Audit_Session_Deactivated", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Session_Audit_Invalid_Username_Or_Password", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Session_Audit_Invalid_Password_Disable", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_session_audit_config_len = 5;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_version_change_config[] =
  {
    { "IPMI_Version_Change_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Version_Change_Hardware_Change_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Version_Change_Firmware_Or_Software_Change_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Version_Change_Hardware_Incompatability_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Firmware_Or_Software_Incompatability_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Entity_Is_Of_An_Invalid_Or_Unsupported_Hardware_Version", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Entity_Contains_An_Invalid_Or_Unsupported_Firmware_Or_Software_Version", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Hardware_Change_Detected_With_Associated_Entity_Was_Successful", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Version_Change_Software_Or_FW_Change_Detected_With_Associated_Entity_Was_Successful", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_sensor_version_change_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_sensor_fru_state_config[] =
  {
    { "IPMI_FRU_State_No_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_FRU_State_FRU_Not_Installed", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_FRU_State_FRU_Inactive", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_FRU_State_FRU_Activation_Requested", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Activation_In_Progress", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Active", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_FRU_State_FRU_Deactivation_Requested", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Deactivation_In_Progress", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Communication_Lost", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_sensor_fru_state_config_len = 9;

static int
_interpret_config_sensor_init (ipmi_interpret_ctx_t ctx,
                               struct ipmi_interpret_sensor_config ***config_dest,
                               struct ipmi_interpret_sensor_config *config_src,
                               unsigned int config_len)
{
  unsigned int mlen;
  unsigned int i;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (config_dest);
  assert (config_src);
  assert (config_len);

  /* +1 for storing NULL pointer sentinel value */
  mlen = sizeof (struct ipmi_interpret_sensor_config *) * (config_len + 1);

  if (!((*config_dest) = (struct ipmi_interpret_sensor_config **) malloc (mlen)))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }
  memset ((*config_dest), '\0', mlen);

  mlen = sizeof (struct ipmi_interpret_sensor_config);

  for (i = 0; i < config_len; i++)
    {
      if (!((*config_dest)[i] = (struct ipmi_interpret_sensor_config *) malloc (mlen)))
        {
          INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }
      memset ((*config_dest)[i], '\0', mlen);

      (*config_dest)[i]->option_str = config_src[i].option_str;
      (*config_dest)[i]->state = config_src[i].state;
    }
  (*config_dest)[i] = NULL;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_interpret_sensor_oem_config_create (ipmi_interpret_ctx_t ctx,
				     uint32_t manufacturer_id,
				     uint16_t product_id,
				     uint8_t event_reading_type_code,
				     uint8_t sensor_type,
				     struct ipmi_interpret_sensor_oem_config **oem_conf)
{
  struct ipmi_interpret_sensor_oem_config *tmp_oem_conf = NULL;
  char keybuf[IPMI_OEM_HASH_KEY_BUFLEN + 1];
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);
  assert (oem_conf);

  memset (keybuf, '\0', IPMI_OEM_HASH_KEY_BUFLEN + 1);

  snprintf (keybuf,
	    IPMI_OEM_HASH_KEY_BUFLEN,
	    "%u:%u:%u:%u",
	    manufacturer_id,
	    product_id,
	    event_reading_type_code,
	    sensor_type);

  if (!(tmp_oem_conf = (struct ipmi_interpret_sensor_oem_config *)malloc (sizeof (struct ipmi_interpret_sensor_oem_config))))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  memset (tmp_oem_conf, '\0', sizeof (struct ipmi_interpret_sensor_oem_config));
  
  memcpy (tmp_oem_conf->key, keybuf, IPMI_OEM_HASH_KEY_BUFLEN);
  tmp_oem_conf->manufacturer_id = manufacturer_id;
  tmp_oem_conf->product_id = product_id;
  tmp_oem_conf->event_reading_type_code = event_reading_type_code;
  tmp_oem_conf->sensor_type = sensor_type;
  
  if (!hash_insert (ctx->interpret_sensor.sensor_oem_config,
		    tmp_oem_conf->key,
		    tmp_oem_conf))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_INTERNAL_ERROR);
      goto cleanup;
    }
  
  (*oem_conf) = tmp_oem_conf;
  rv = 0;
 cleanup:
  if (rv < 0)
    free (tmp_oem_conf);
  return (rv);
}

static int
_interpret_sensor_oem_dell_power_optimized (ipmi_interpret_ctx_t ctx)
{
  struct ipmi_interpret_sensor_oem_config *oem_conf;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  /* Dell Poweredge R610/R710 Power Optimized
   *
   * Manufacturer ID = 674 (Dell)
   * Product ID = 256 (Poweredge)
   * Event/Reading Type Code = 6Fh (Sensor Specific)
   * Sensor Type = C0h (OEM)
   * Bitmask 0x0001 = "Good"
   * Bitmask 0x0002 = "Degraded, other"
   * Bitmask 0x0004 = "Degraded, thermal protection"
   * Bitmask 0x0008 = "Degraded, cooling capacity change"
   * Bitmask 0x0010 = "Degraded, power capacity change"
   * Bitmask 0x0020 = "Degraded, user defined power capacity"
   * Bitmask 0x0040 = "Halted, system power exceeds capacity"
   * Bitmask 0x0080 = "Degraded, system power exceeds capacity"
   */
  
  if (_interpret_sensor_oem_config_create (ctx,
					   IPMI_IANA_ENTERPRISE_ID_DELL,
					   IPMI_DELL_PRODUCT_ID_POWEREDGE_R610,
					   IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC,
					   IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS,
					   &oem_conf) < 0)
    return (-1);

  oem_conf->oem_state[0].sensor_event_bitmask = 0;
  oem_conf->oem_state[0].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[0].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[1].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_GOOD);
  oem_conf->oem_state[1].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[1].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[2].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_OTHER);
  oem_conf->oem_state[2].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[2].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[3].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_THERMAL_PROTECTION);
  oem_conf->oem_state[3].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[3].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[4].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_COOLING_CAPACITY_CHANGE);
  oem_conf->oem_state[4].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[4].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[5].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_POWER_CAPACITY_CHANGE);
  oem_conf->oem_state[5].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[5].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[6].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_USER_DEFINED_POWER_CAPACITY);
  oem_conf->oem_state[6].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[6].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[7].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_HALTED_SYSTEM_POWER_EXCEEDS_CAPACITY);
  oem_conf->oem_state[7].sensor_state = IPMI_INTERPRET_STATE_CRITICAL;
  oem_conf->oem_state[7].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[8].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_SYSTEM_POWER_EXCEEDS_CAPACITY);
  oem_conf->oem_state[8].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[8].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state_count = 9;

  return (0);
}

static int
_interpret_sensor_oem_dell_module_board_status (ipmi_interpret_ctx_t ctx)
{
  struct ipmi_interpret_sensor_oem_config *oem_conf;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  /* Dell Poweredge R210 Module/Board OEM
   *
   * Manufacturer ID = 674 (Dell)
   * Product ID = 256 (Poweredge)
   * Event/Reading Type Code = 70h (OEM)
   * Sensor Type = 15h (Module/Board)
   * Bitmask 0x0001 = "Absent"
   * Bitmask 0x0002 = "Standby"
   * Bitmask 0x0004 = "IPMI Function ready"
   * Bitmask 0x0008 = "Fully ready"
   * Bitmask 0x0010 = "Offline"
   * Bitmask 0x0020 = "Failed"
   * Bitmask 0x0040 = "Active"
   * Bitmask 0x0080 = "Booting"
   * Bitmask 0x0100 = "Write protected"
   */
  
  if (_interpret_sensor_oem_config_create (ctx,
					   IPMI_IANA_ENTERPRISE_ID_DELL,
					   IPMI_DELL_PRODUCT_ID_POWEREDGE_R610,
					   IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS,
					   IPMI_SENSOR_TYPE_MODULE_BOARD,
					   &oem_conf) < 0)
    return (-1);

  oem_conf->oem_state[0].sensor_event_bitmask = 0;
  oem_conf->oem_state[0].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[0].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[1].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_ABSENT);
  oem_conf->oem_state[1].sensor_state = IPMI_INTERPRET_STATE_CRITICAL;
  oem_conf->oem_state[1].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[2].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_STANDBY);
  oem_conf->oem_state[2].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[2].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[3].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_IPMI_FUNCTION_READY);
  oem_conf->oem_state[3].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[3].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[4].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_FULLY_READY);
  oem_conf->oem_state[4].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[4].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[5].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_OFFLINE);
  oem_conf->oem_state[5].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[5].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[6].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_FAILED);
  oem_conf->oem_state[6].sensor_state = IPMI_INTERPRET_STATE_CRITICAL;
  oem_conf->oem_state[6].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[7].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_ACTIVE);
  oem_conf->oem_state[7].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[7].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[8].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_BOOTING);
  oem_conf->oem_state[8].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[8].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[9].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS_WRITE_PROTECTED);
  oem_conf->oem_state[9].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[9].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state_count = 10;

  return (0);
}

static int
_interpret_sensor_oem_dell (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  if (_interpret_sensor_oem_dell_power_optimized (ctx) < 0)
    return (-1);

  if (_interpret_sensor_oem_dell_module_board_status (ctx) < 0)
    return (-1);

  return (0);
}

static int
_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ipmi_interpret_ctx_t ctx,
							    uint32_t manufacturer_id,
							    uint16_t product_id)
{
  struct ipmi_interpret_sensor_oem_config *oem_conf;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  if (_interpret_sensor_oem_config_create (ctx,
					   manufacturer_id,
					   product_id,
					   IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC,
					   IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP,
					   &oem_conf) < 0)
    return (-1);

  oem_conf->oem_state[0].sensor_event_bitmask = IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_LOW;
  oem_conf->oem_state[0].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[0].oem_state_type = IPMI_OEM_STATE_TYPE_VALUE;

  oem_conf->oem_state[1].sensor_event_bitmask = IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_MEDIUM;
  oem_conf->oem_state[1].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[1].oem_state_type = IPMI_OEM_STATE_TYPE_VALUE;

  oem_conf->oem_state[2].sensor_event_bitmask = IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_HIGH;
  oem_conf->oem_state[2].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[2].oem_state_type = IPMI_OEM_STATE_TYPE_VALUE;

  oem_conf->oem_state[3].sensor_event_bitmask = IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_OVERHEAT;
  oem_conf->oem_state[3].sensor_state = IPMI_INTERPRET_STATE_CRITICAL;
  oem_conf->oem_state[3].oem_state_type = IPMI_OEM_STATE_TYPE_VALUE;

  oem_conf->oem_state[4].sensor_event_bitmask = IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_NOT_INSTALLED;
  oem_conf->oem_state[4].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[4].oem_state_type = IPMI_OEM_STATE_TYPE_VALUE;

  oem_conf->oem_state_count = 5;

  return (0);
}

static int
_interpret_sensor_oem_supermicro_discrete_cpu_temp (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);
  
  /* Supermicro CPU Temperature Sensor
   * X7DBR-3/X7DB8/X8DTN/X7SBI-LN4/X8DTH/X8DTG/X8DTU/X8DT3-LN4F/X8DTU-6+/X8DTL/X8DTL-3F
   * X8SIL-F/X9SCL/X9SCM/X8DTN+-F/X8SIE/X9SCA-F-O/H8DGU-F/X9DRi-F/X9DRI-LN4F+/X9SPU-F-O/X9SCM-iiF
   *
   * Manufacturer ID = 10876 (Supermicro), 10437 (Peppercon, IPMI card manufacturer),
   *                   47488 (Supermicro, not IANA number, special case)
   *                   5593 (Magnum Technologies, rebranded Supermicro board)
   * Product ID = 4 (X7DBR-3 / X7DBR_3, X7DB8, X8DTN, X7SBI-LN4 / X7SBI_LN4), 43707 (X8DTH, X8DTG, X8DTU, X8DT3-LN4F / X8DT3_LN4F),
   *              1549 (X8DTU-6+ / X8DTU_6PLUS), 6 (X8DTL, X8DTL-3F / X8DTL_3F), 1541 (X8SIL-F / X8SIL_F), 1572 (X9SCL, X9SCM),
   *              1551 (X8DTN+-F / X8DTNPLUS_F), 1037 (X8SIE), 1585 (X9SCA-F-O / X9SCA_F_O), 43025 (H8DGU-F / H8DGU_F),
   *              1576 (X9DRi-F, X9DRI_F), 1574 (X9DRI-LN4F+ / X9DRI_LN4F_PLUS), 1603 (X9SPU-F-O / X9SPU_F_O),
   *              1600 (X9SCM-iiF / X9SCM_IIF)
   * Event/Reading Type Code = 70h (OEM)
   * Sensor Type = C0h (OEM)
   * Value 0x0000 = "Low"
   * Value 0x0001 = "Medium"
   * Value 0x0002 = "High"
   * Value 0x0004 = "Overheat"
   * Value 0x0007 = "Not Installed"
   */

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO,
								  IPMI_SUPERMICRO_PRODUCT_ID_X9SC_BASE) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO,
								  IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_F) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO,
								  IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_LN4F_PLUS) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO,
								  IPMI_SUPERMICRO_PRODUCT_ID_X9SPU_F_O) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO,
								  IPMI_SUPERMICRO_PRODUCT_ID_X9SCM_IIF) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_PEPPERCON,
								  IPMI_SUPERMICRO_PRODUCT_ID_FOUR_BASE) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X8DT_BASE) < 0)
    return (-1);
  
  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X8DTU_6PLUS) < 0)
    return (-1);
  
  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X8DTL_BASE) < 0)
    return (-1);
  
  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X8SIL_F) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X9SC_BASE) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X8DTNPLUS_F) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X8SIE) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_X9SCA_F_O) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_H8DGU_F) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_H8DGU) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
								  IPMI_SUPERMICRO_PRODUCT_ID_H8DG6) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp_wrapper (ctx,
								  IPMI_IANA_ENTERPRISE_ID_MAGNUM_TECHNOLOGIES,
								  IPMI_SUPERMICRO_PRODUCT_ID_X8DTL_BASE) < 0)
    return (-1);

  return (0);
}

static int
_interpret_sensor_oem_supermicro (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  if (_interpret_sensor_oem_supermicro_discrete_cpu_temp (ctx) < 0)
    return (-1);

  return (0);
}

static int
_interpret_sensor_oem_intel_smi_timeout_power_throttled_wrapper (ipmi_interpret_ctx_t ctx,
								 uint32_t manufacturer_id,
								 uint16_t product_id)
{
  struct ipmi_interpret_sensor_oem_config *oem_conf;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  if (_interpret_sensor_oem_config_create (ctx,
					   manufacturer_id,
					   product_id,
					   IPMI_EVENT_READING_TYPE_CODE_STATE,
					   IPMI_SENSOR_TYPE_OEM_INTEL_SMI_TIMEOUT,
					   &oem_conf) < 0)
    return (-1);
  
  oem_conf->oem_state[0].sensor_event_bitmask = 0;
  oem_conf->oem_state[0].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[0].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[1].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_DEASSERTED);
  oem_conf->oem_state[1].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[1].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[2].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED);
  oem_conf->oem_state[2].sensor_state = IPMI_INTERPRET_STATE_CRITICAL;
  oem_conf->oem_state[2].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state_count = 3;
  
  return (0);
}
						 

static int
_interpret_sensor_oem_intel_smi_timeout_power_throttled (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  /* Intel SMI Timeout
   * Intel SR1625
   * Intel S5500WB/Penguin Computing Relion 700
   * Quanta QSSC-S4R/Appro GB812X-CN (Quanta motherboard maintains Intel manufacturer ID)
   * Intel S5000PAL
   *
   * and
   *
   * Intel Power Throttled
   * Quanta QSSC-S4R/Appro GB812X-CN (Quanta motherboard maintains Intel manufacturer ID)
   *
   * Manufacturer ID = 343 (Intel)
   * Product ID = 62 (Intel SR1625, S5500WB), 64 (Quanta QSSC-S4R), 40 (Intel S5000PAL)
   * Event/Reading Type Code = 3h (State Asserted/Deasserted)
   * Sensor Type = F3h (OEM)
   * Bitmask 0x0001 = "State Deasserted"
   * Bitmask 0x0002 = "State Asserted"
   */
  
  /* From Intel
   *
   * The BMC supports an SMI timeout sensor (sensor type OEM (F3h),
   * event type Discrete (03h)) that asserts if the SMI signal has
   * been asserted for more than 90 seconds. A continuously asserted
   * SMI signal is an indication that the BIOS cannot service the
   * condition that caused the SMI. This is usually because that
   * condition prevents the BIOS from running. When an SMI timeout
   * occurs, the BMC asserts the SMI timeout sensor and logs a SEL
   * event for that sensor. The BMC will also reset the system.
   */

  /* Intel SR1625
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (_interpret_sensor_oem_intel_smi_timeout_power_throttled_wrapper (ctx,
								       IPMI_IANA_ENTERPRISE_ID_INTEL,
								       IPMI_INTEL_PRODUCT_ID_SR1625) < 0)
    return (-1);

  /* Quanta QSSC-S4R/Appro GB812X-CN */
  if (_interpret_sensor_oem_intel_smi_timeout_power_throttled_wrapper (ctx,
								       IPMI_IANA_ENTERPRISE_ID_INTEL,
								       IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R) < 0)
    return (-1);

  /* Intel S5000PAL */
  if (_interpret_sensor_oem_intel_smi_timeout_power_throttled_wrapper (ctx,
								       IPMI_IANA_ENTERPRISE_ID_INTEL,
								       IPMI_INTEL_PRODUCT_ID_S5000PAL) < 0)
    return (-1);

  return (0);
}

static int
_interpret_sensor_oem_intel_nmi_state_wrapper (ipmi_interpret_ctx_t ctx,
					       uint32_t manufacturer_id,
					       uint16_t product_id)
{
  struct ipmi_interpret_sensor_oem_config *oem_conf;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  if (_interpret_sensor_oem_config_create (ctx,
					   manufacturer_id,
					   product_id,
					   IPMI_EVENT_READING_TYPE_CODE_STATE,
					   IPMI_SENSOR_TYPE_OEM_INTEL_NMI_STATE,
					   &oem_conf) < 0)
    return (-1);
  
  oem_conf->oem_state[0].sensor_event_bitmask = 0;
  oem_conf->oem_state[0].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[0].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[1].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_DEASSERTED);
  oem_conf->oem_state[1].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[1].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;
  
  oem_conf->oem_state[2].sensor_event_bitmask = (0x1 << IPMI_GENERIC_EVENT_READING_TYPE_CODE_STATE_ASSERTED);
  oem_conf->oem_state[2].sensor_state = IPMI_INTERPRET_STATE_CRITICAL;
  oem_conf->oem_state[2].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state_count = 3;
  
  return (0);
}

static int
_interpret_sensor_oem_intel_nmi_state (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  /* Intel NMI State
   * Intel S5000PAL
   *
   * Manufacturer ID = 343 (Intel)
   * Product ID = 40 (Intel S5000PAL)
   * Event/Reading Type Code = 3h (State Asserted/Deasserted)
   * Sensor Type = C0h (OEM)
   * Bitmask 0x0001 = "State Deasserted"
   * Bitmask 0x0002 = "State Asserted"
   */

  /* Intel S5000PAL */
  if (_interpret_sensor_oem_intel_nmi_state_wrapper (ctx,
						     IPMI_IANA_ENTERPRISE_ID_INTEL,
						     IPMI_INTEL_PRODUCT_ID_S5000PAL) < 0)
    return (-1);
  
  return (0);
}

static int
_interpret_sensor_oem_intel (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  if (_interpret_sensor_oem_intel_smi_timeout_power_throttled (ctx) < 0)
    return (-1);

  if (_interpret_sensor_oem_intel_nmi_state (ctx) < 0)
    return (-1);

  return (0);
}

static int
_interpret_sensor_oem_config_init (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (ctx->interpret_sensor.sensor_oem_config);

  if (_interpret_sensor_oem_dell (ctx) < 0)
    return (-1);

  if (_interpret_sensor_oem_supermicro (ctx) < 0)
    return (-1);

  if (_interpret_sensor_oem_intel (ctx) < 0)
    return (-1);

  return (0);
}

int
interpret_sensor_init (ipmi_interpret_ctx_t ctx)
{
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_threshold_config,
                                     ipmi_interpret_sensor_threshold_config,
                                     ipmi_interpret_sensor_threshold_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_temperature_state_config,
                                     ipmi_interpret_sensor_temperature_state_config,
                                     ipmi_interpret_sensor_temperature_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_temperature_limit_config,
                                     ipmi_interpret_sensor_temperature_limit_config,
                                     ipmi_interpret_sensor_temperature_limit_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_temperature_transition_severity_config,
                                     ipmi_interpret_sensor_temperature_transition_severity_config,
                                     ipmi_interpret_sensor_temperature_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_voltage_state_config,
                                     ipmi_interpret_sensor_voltage_state_config,
                                     ipmi_interpret_sensor_voltage_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_voltage_limit_config,
                                     ipmi_interpret_sensor_voltage_limit_config,
                                     ipmi_interpret_sensor_voltage_limit_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_voltage_performance_config,
                                     ipmi_interpret_sensor_voltage_performance_config,
                                     ipmi_interpret_sensor_voltage_performance_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_voltage_transition_severity_config,
                                     ipmi_interpret_sensor_voltage_transition_severity_config,
                                     ipmi_interpret_sensor_voltage_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_current_transition_severity_config,
                                     ipmi_interpret_sensor_current_transition_severity_config,
                                     ipmi_interpret_sensor_current_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_fan_state_config,
                                     ipmi_interpret_sensor_fan_state_config,
                                     ipmi_interpret_sensor_fan_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_fan_transition_severity_config,
                                     ipmi_interpret_sensor_fan_transition_severity_config,
                                     ipmi_interpret_sensor_fan_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_fan_device_present_config,
                                     ipmi_interpret_sensor_fan_device_present_config,
                                     ipmi_interpret_sensor_fan_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_fan_transition_availability_config,
                                     ipmi_interpret_sensor_fan_transition_availability_config,
                                     ipmi_interpret_sensor_fan_transition_availability_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_fan_redundancy_config,
                                     ipmi_interpret_sensor_fan_redundancy_config,
                                     ipmi_interpret_sensor_fan_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_physical_security_config,
                                     ipmi_interpret_sensor_physical_security_config,
                                     ipmi_interpret_sensor_physical_security_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_platform_security_violation_attempt_config,
                                     ipmi_interpret_sensor_platform_security_violation_attempt_config,
                                     ipmi_interpret_sensor_platform_security_violation_attempt_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_processor_config,
                                     ipmi_interpret_sensor_processor_config,
                                     ipmi_interpret_sensor_processor_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_processor_state_config,
                                     ipmi_interpret_sensor_processor_state_config,
                                     ipmi_interpret_sensor_processor_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_config,
                                     ipmi_interpret_sensor_power_supply_config,
                                     ipmi_interpret_sensor_power_supply_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_state_config,
                                     ipmi_interpret_sensor_power_supply_state_config,
                                     ipmi_interpret_sensor_power_supply_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_transition_severity_config,
                                     ipmi_interpret_sensor_power_supply_transition_severity_config,
                                     ipmi_interpret_sensor_power_supply_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_redundancy_config,
                                     ipmi_interpret_sensor_power_supply_redundancy_config,
                                     ipmi_interpret_sensor_power_supply_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_config,
                                     ipmi_interpret_sensor_power_unit_config,
                                     ipmi_interpret_sensor_power_unit_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_device_present_config,
                                     ipmi_interpret_sensor_power_unit_device_present_config,
                                     ipmi_interpret_sensor_power_unit_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_redundancy_config,
                                     ipmi_interpret_sensor_power_unit_redundancy_config,
                                     ipmi_interpret_sensor_power_unit_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_cooling_device_redundancy_config,
                                     ipmi_interpret_sensor_cooling_device_redundancy_config,
                                     ipmi_interpret_sensor_cooling_device_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_memory_config,
                                     ipmi_interpret_sensor_memory_config,
                                     ipmi_interpret_sensor_memory_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_memory_state_config,
                                     ipmi_interpret_sensor_memory_state_config,
                                     ipmi_interpret_sensor_memory_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_memory_transition_severity_config,
                                     ipmi_interpret_sensor_memory_transition_severity_config,
                                     ipmi_interpret_sensor_memory_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_memory_redundancy_config,
                                     ipmi_interpret_sensor_memory_redundancy_config,
                                     ipmi_interpret_sensor_memory_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_config,
                                     ipmi_interpret_sensor_drive_slot_config,
                                     ipmi_interpret_sensor_drive_slot_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_state_config,
                                     ipmi_interpret_sensor_drive_slot_state_config,
                                     ipmi_interpret_sensor_drive_slot_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_predictive_failure_config,
                                     ipmi_interpret_sensor_drive_slot_predictive_failure_config,
                                     ipmi_interpret_sensor_drive_slot_predictive_failure_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_device_present_config,
                                     ipmi_interpret_sensor_drive_slot_device_present_config,
                                     ipmi_interpret_sensor_drive_slot_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_post_memory_resize_state_config,
                                     ipmi_interpret_sensor_post_memory_resize_state_config,
                                     ipmi_interpret_sensor_post_memory_resize_state_config_len) < 0)
    goto cleanup;


  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_system_firmware_progress_config,
                                     ipmi_interpret_sensor_system_firmware_progress_config,
                                     ipmi_interpret_sensor_system_firmware_progress_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_system_firmware_progress_transition_severity_config,
                                     ipmi_interpret_sensor_system_firmware_progress_transition_severity_config,
                                     ipmi_interpret_sensor_system_firmware_progress_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_event_logging_disabled_config,
                                     ipmi_interpret_sensor_event_logging_disabled_config,
                                     ipmi_interpret_sensor_event_logging_disabled_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_system_event_config,
                                     ipmi_interpret_sensor_system_event_config,
                                     ipmi_interpret_sensor_system_event_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_system_event_transition_state_config,
                                     ipmi_interpret_sensor_system_event_transition_state_config,
                                     ipmi_interpret_sensor_system_event_transition_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_system_event_state_config,
                                     ipmi_interpret_sensor_system_event_state_config,
                                     ipmi_interpret_sensor_system_event_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_critical_interrupt_config,
                                     ipmi_interpret_sensor_critical_interrupt_config,
                                     ipmi_interpret_sensor_critical_interrupt_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_config,
                                     ipmi_interpret_sensor_button_switch_config,
                                     ipmi_interpret_sensor_button_switch_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_state_config,
                                     ipmi_interpret_sensor_button_switch_state_config,
                                     ipmi_interpret_sensor_button_switch_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_transition_severity_config,
                                     ipmi_interpret_sensor_button_switch_transition_severity_config,
                                     ipmi_interpret_sensor_button_switch_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_module_board_state_config,
                                     ipmi_interpret_sensor_module_board_state_config,
                                     ipmi_interpret_sensor_module_board_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_module_board_device_present_config,
                                     ipmi_interpret_sensor_module_board_device_present_config,
                                     ipmi_interpret_sensor_module_board_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_chassis_transition_severity_config,
                                     ipmi_interpret_sensor_chassis_transition_severity_config,
                                     ipmi_interpret_sensor_chassis_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_chip_set_transition_severity_config,
                                     ipmi_interpret_sensor_chip_set_transition_severity_config,
                                     ipmi_interpret_sensor_chip_set_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_cable_interconnect_config,
                                     ipmi_interpret_sensor_cable_interconnect_config,
                                     ipmi_interpret_sensor_cable_interconnect_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_cable_interconnect_transition_severity_config,
                                     ipmi_interpret_sensor_cable_interconnect_transition_severity_config,
                                     ipmi_interpret_sensor_cable_interconnect_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_config,
                                     ipmi_interpret_sensor_boot_error_config,
                                     ipmi_interpret_sensor_boot_error_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_state_config,
                                     ipmi_interpret_sensor_boot_error_state_config,
                                     ipmi_interpret_sensor_boot_error_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_transition_severity_config,
                                     ipmi_interpret_sensor_boot_error_transition_severity_config,
                                     ipmi_interpret_sensor_boot_error_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_os_boot_config,
                                     ipmi_interpret_sensor_os_boot_config,
                                     ipmi_interpret_sensor_os_boot_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_os_critical_stop_state_config,
                                     ipmi_interpret_sensor_os_critical_stop_state_config,
                                     ipmi_interpret_sensor_os_critical_stop_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_slot_connector_config,
                                     ipmi_interpret_sensor_slot_connector_config,
                                     ipmi_interpret_sensor_slot_connector_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_slot_connector_transition_severity_config,
                                     ipmi_interpret_sensor_slot_connector_transition_severity_config,
                                     ipmi_interpret_sensor_slot_connector_transition_severity_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_system_acpi_power_state_config,
                                     ipmi_interpret_sensor_system_acpi_power_state_config,
                                     ipmi_interpret_sensor_system_acpi_power_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_watchdog2_config,
                                     ipmi_interpret_sensor_watchdog2_config,
                                     ipmi_interpret_sensor_watchdog2_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_platform_alert_state_config,
                                     ipmi_interpret_sensor_platform_alert_state_config,
                                     ipmi_interpret_sensor_platform_alert_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_entity_presence_config,
                                     ipmi_interpret_sensor_entity_presence_config,
                                     ipmi_interpret_sensor_entity_presence_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_entity_presence_device_present_config,
                                     ipmi_interpret_sensor_entity_presence_device_present_config,
                                     ipmi_interpret_sensor_entity_presence_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_management_subsystem_health_config,
                                     ipmi_interpret_sensor_management_subsystem_health_config,
                                     ipmi_interpret_sensor_management_subsystem_health_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_battery_config,
                                     ipmi_interpret_sensor_battery_config,
                                     ipmi_interpret_sensor_battery_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_session_audit_config,
                                     ipmi_interpret_sensor_session_audit_config,
                                     ipmi_interpret_sensor_session_audit_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_version_change_config,
                                     ipmi_interpret_sensor_version_change_config,
                                     ipmi_interpret_sensor_version_change_config_len) < 0)
    goto cleanup;

  if (_interpret_config_sensor_init (ctx,
                                     &ctx->interpret_sensor.ipmi_interpret_sensor_fru_state_config,
                                     ipmi_interpret_sensor_fru_state_config,
                                     ipmi_interpret_sensor_fru_state_config_len) < 0)
    goto cleanup;

  if (!(ctx->interpret_sensor.sensor_oem_config = hash_create (IPMI_INTERPRET_SENSOR_HASH_SIZE,
                                                               (hash_key_f)hash_key_string,
                                                               (hash_cmp_f)strcmp,
                                                               (hash_del_f)free)))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  if (_interpret_sensor_oem_config_init (ctx) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static void
_interpret_config_sensor_destroy (ipmi_interpret_ctx_t ctx,
                                  struct ipmi_interpret_sensor_config **config)
{
  unsigned int i = 0;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (config)
    {
      while (config[i])
        {
          free (config[i]);
          i++;
        }
      free (config);
    }
}

void
interpret_sensor_destroy (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_threshold_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_temperature_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_temperature_limit_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_temperature_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_voltage_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_voltage_limit_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_voltage_performance_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_voltage_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_current_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_fan_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_fan_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_fan_device_present_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_fan_transition_availability_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_fan_redundancy_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_physical_security_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_platform_security_violation_attempt_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_processor_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_processor_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_redundancy_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_device_present_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_redundancy_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_cooling_device_redundancy_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_memory_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_memory_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_memory_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_memory_redundancy_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_predictive_failure_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_device_present_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_post_memory_resize_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_system_firmware_progress_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_system_firmware_progress_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_event_logging_disabled_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_system_event_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_system_event_transition_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_system_event_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_critical_interrupt_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_module_board_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_module_board_device_present_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_chassis_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_chip_set_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_cable_interconnect_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_cable_interconnect_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_os_boot_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_os_critical_stop_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_slot_connector_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_slot_connector_transition_severity_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_system_acpi_power_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_watchdog2_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_platform_alert_state_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_entity_presence_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_entity_presence_device_present_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_management_subsystem_health_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_battery_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_session_audit_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_version_change_config);

  _interpret_config_sensor_destroy (ctx,
                                    ctx->interpret_sensor.ipmi_interpret_sensor_fru_state_config);

  if (ctx->interpret_sensor.sensor_oem_config)
    hash_destroy (ctx->interpret_sensor.sensor_oem_config);
}

static int
_cb_sensor_parse (conffile_t cf,
                  struct conffile_data *data,
                  char *optionname,
                  int option_type,
                  void *option_ptr,
                  int option_data,
                  void *app_ptr,
                  int app_data)
{
  struct ipmi_interpret_sensor_config **config;
  int state;
  int i;

  assert (cf);
  assert (data);
  assert (option_type == CONFFILE_OPTION_STRING);
  assert (optionname);
  assert (option_ptr);

  if ((state = interpret_config_parse_state (cf, data->string)) < 0)
    return (-1);

  i = 0;
  config = (struct ipmi_interpret_sensor_config **)option_ptr;
  while (config[i])
    {
      if (!strcasecmp (optionname, config[i]->option_str))
        {
          config[i]->state = state;
          return (0);
        }
      i++;
    }

  conffile_seterrnum (cf, CONFFILE_ERR_INTERNAL);
  return (-1);
}

static int
_cb_sensor_oem_parse (conffile_t cf,
                      struct conffile_data *data,
                      char *optionname,
                      int option_type,
                      void *option_ptr,
                      int option_data,
                      void *app_ptr,
                      int app_data)
{
  hash_t *h = NULL;
  char keybuf[IPMI_OEM_HASH_KEY_BUFLEN + 1];
  struct ipmi_interpret_config_file_ids ids[IPMI_INTERPRET_CONFIG_FILE_MANUFACTURER_ID_MAX];
  unsigned int ids_count = 0;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  uint16_t sensor_event_bitmask;
  int sensor_state;
  int oem_state_type;
  uint32_t tmp;
  struct ipmi_interpret_sensor_oem_config *oem_conf;
  int found = 0;
  unsigned int i, j, k;

  assert (cf);
  assert (data);
  assert (optionname);
  assert (option_ptr);

  h = (hash_t *)option_ptr;

  memset (keybuf, '\0', IPMI_OEM_HASH_KEY_BUFLEN + 1);

  memset (ids,
          '\0',
          sizeof (struct ipmi_interpret_config_file_ids) * IPMI_INTERPRET_CONFIG_FILE_MANUFACTURER_ID_MAX);

  if (data->stringlist_len != 5)
    {
      if (data->stringlist_len < 5)
        conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_MISSING);
      else
        conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_TOOMANY);
      return (-1);
    }
  
  if (interpret_config_parse_manufactuer_id_product_id (cf,
							data->stringlist[0],
							ids,
							&ids_count) < 0)
    return (-1);

  if (interpret_config_parse_strtoul (cf,
				      data->stringlist[1],
				      UCHAR_MAX,
				      &tmp) < 0)
    return (-1);
  event_reading_type_code = tmp;

  if (interpret_config_parse_strtoul (cf,
				      data->stringlist[2],
				      UCHAR_MAX,
				      &tmp) < 0)
    return (-1);
  sensor_type = tmp;
  
  /* achu: sensor event bitmask bit 16 not legal, but we will allow it
   * b/c perhaps some OEM sensors will break legality of events, or
   * perhaps there is a bug and some vendors need to have the 16th bit
   * matched.
   */
  if (interpret_config_parse_strtoul (cf,
				      data->stringlist[3],
				      USHRT_MAX,
				      &tmp) < 0)
    return (-1);
  sensor_event_bitmask = tmp;

  if ((sensor_state = interpret_config_parse_state (cf, data->stringlist[4])) < 0)
    return (-1);
  
  if (!strcasecmp (optionname, "IPMI_OEM_Bitmask"))
    oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;
  else if (!strcasecmp (optionname, "IPMI_OEM_Value"))
    oem_state_type = IPMI_OEM_STATE_TYPE_VALUE;
  else
    {
      conffile_seterrnum (cf, CONFFILE_ERR_INTERNAL);
      return (-1);
    }
  
  for (i = 0; i < ids_count; i++)
    {
      for (j = 0; j < ids[i].product_ids_count; j++)
        {
          snprintf (keybuf,
                    IPMI_OEM_HASH_KEY_BUFLEN,
                    "%u:%u:%u:%u",
                    ids[i].manufacturer_id,
                    ids[i].product_ids[j],
                    event_reading_type_code,
                    sensor_type);
      
          if (!(oem_conf = hash_find ((*h), keybuf)))
            {
              if (!(oem_conf = (struct ipmi_interpret_sensor_oem_config *)malloc (sizeof (struct ipmi_interpret_sensor_oem_config))))
                {
                  conffile_seterrnum (cf, CONFFILE_ERR_OUTMEM);
                  return (-1);
                }
              memset (oem_conf, '\0', sizeof (struct ipmi_interpret_sensor_oem_config));
              
              memcpy (oem_conf->key, keybuf, IPMI_OEM_HASH_KEY_BUFLEN);
              oem_conf->manufacturer_id = ids[i].manufacturer_id;
              oem_conf->product_id = ids[i].product_ids[j];
              oem_conf->event_reading_type_code = event_reading_type_code;
              oem_conf->sensor_type = sensor_type;
              
              if (!hash_insert ((*h), oem_conf->key, oem_conf))
                {
                  conffile_seterrnum (cf, CONFFILE_ERR_INTERNAL);
                  free (oem_conf);
                  return (-1);
                }
            }
          
          if (oem_conf->oem_state_count >= IPMI_INTERPRET_MAX_BITMASKS)
            {
              conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_TOOMANY);
              return (-1);
            }
      
          /* check for duplicates */
          for (k = 0; k < oem_conf->oem_state_count; k++)
            {
              if (oem_conf->oem_state[k].oem_state_type == oem_state_type
                  && oem_conf->oem_state[k].sensor_event_bitmask == sensor_event_bitmask)
                {
                  oem_conf->oem_state[k].sensor_state = sensor_state;
                  found++;
                  break;
                }
            }
      
          if (!found)
            {
              oem_conf->oem_state[oem_conf->oem_state_count].sensor_event_bitmask = sensor_event_bitmask;
              oem_conf->oem_state[oem_conf->oem_state_count].sensor_state = sensor_state;
              oem_conf->oem_state[oem_conf->oem_state_count].oem_state_type = oem_state_type;
              oem_conf->oem_state_count++;
            }
        }
    }

  return (0);
}

static void
_fill_sensor_config_options (struct conffile_option *to_options,
                             unsigned int *to_options_len,
                             struct ipmi_interpret_sensor_config **from_config,
                             int *from_config_flags,
                             unsigned int from_config_len)
{
  unsigned int i;
    
  assert (to_options
          && to_options_len
          && from_config
          && from_config_len);

  memset (from_config_flags, '\0', sizeof (int) * from_config_len);

  /* note: can't memcpy .. sigh .. wish I did this in C++ w/ a copy constructor */
  for (i = 0; i < from_config_len; i++)
    {
      to_options[(*to_options_len) + i].optionname = from_config[i]->option_str;
      to_options[(*to_options_len) + i].option_type = CONFFILE_OPTION_STRING;
      to_options[(*to_options_len) + i].option_type_arg = -1;
      to_options[(*to_options_len) + i].callback_func = _cb_sensor_parse;
      to_options[(*to_options_len) + i].max_count = 1;
      to_options[(*to_options_len) + i].required_count = 0;
      to_options[(*to_options_len) + i].count_ptr = &from_config_flags[i];
      to_options[(*to_options_len) + i].option_ptr = from_config;
      to_options[(*to_options_len) + i].option_data = 0;
    }

  (*to_options_len) += from_config_len;
}

int
interpret_sensor_config_parse (ipmi_interpret_ctx_t ctx,
			       const char *sensor_config_file)
{
  int ipmi_interpret_sensor_threshold_flags[ipmi_interpret_sensor_threshold_config_len];
  int ipmi_interpret_sensor_temperature_state_flags[ipmi_interpret_sensor_temperature_state_config_len];
  int ipmi_interpret_sensor_temperature_limit_flags[ipmi_interpret_sensor_temperature_limit_config_len];
  int ipmi_interpret_sensor_temperature_transition_severity_flags[ipmi_interpret_sensor_temperature_transition_severity_config_len];
  int ipmi_interpret_sensor_voltage_state_flags[ipmi_interpret_sensor_voltage_state_config_len];
  int ipmi_interpret_sensor_voltage_limit_flags[ipmi_interpret_sensor_voltage_limit_config_len];
  int ipmi_interpret_sensor_voltage_performance_flags[ipmi_interpret_sensor_voltage_performance_config_len];
  int ipmi_interpret_sensor_voltage_transition_severity_flags[ipmi_interpret_sensor_voltage_transition_severity_config_len];
  int ipmi_interpret_sensor_current_transition_severity_flags[ipmi_interpret_sensor_current_transition_severity_config_len];
  int ipmi_interpret_sensor_fan_state_flags[ipmi_interpret_sensor_fan_state_config_len];
  int ipmi_interpret_sensor_fan_transition_severity_flags[ipmi_interpret_sensor_fan_transition_severity_config_len];
  int ipmi_interpret_sensor_fan_device_present_flags[ipmi_interpret_sensor_fan_device_present_config_len];
  int ipmi_interpret_sensor_fan_transition_availability_flags[ipmi_interpret_sensor_fan_transition_availability_config_len];
  int ipmi_interpret_sensor_fan_redundancy_flags[ipmi_interpret_sensor_fan_redundancy_config_len];
  int ipmi_interpret_sensor_physical_security_flags[ipmi_interpret_sensor_physical_security_config_len];
  int ipmi_interpret_sensor_platform_security_violation_attempt_flags[ipmi_interpret_sensor_platform_security_violation_attempt_config_len];
  int ipmi_interpret_sensor_processor_flags[ipmi_interpret_sensor_processor_config_len];
  int ipmi_interpret_sensor_processor_state_flags[ipmi_interpret_sensor_processor_state_config_len];
  int ipmi_interpret_sensor_power_supply_flags[ipmi_interpret_sensor_power_supply_config_len];
  int ipmi_interpret_sensor_power_supply_state_flags[ipmi_interpret_sensor_power_supply_state_config_len];
  int ipmi_interpret_sensor_power_supply_transition_severity_flags[ipmi_interpret_sensor_power_supply_transition_severity_config_len];
  int ipmi_interpret_sensor_power_supply_redundancy_flags[ipmi_interpret_sensor_power_supply_redundancy_config_len];
  int ipmi_interpret_sensor_power_unit_flags[ipmi_interpret_sensor_power_unit_config_len];
  int ipmi_interpret_sensor_power_unit_device_present_flags[ipmi_interpret_sensor_power_unit_device_present_config_len];
  int ipmi_interpret_sensor_power_unit_redundancy_flags[ipmi_interpret_sensor_power_unit_redundancy_config_len];
  int ipmi_interpret_sensor_cooling_device_redundancy_flags[ipmi_interpret_sensor_cooling_device_redundancy_config_len];
  int ipmi_interpret_sensor_memory_flags[ipmi_interpret_sensor_memory_config_len];
  int ipmi_interpret_sensor_memory_state_flags[ipmi_interpret_sensor_memory_state_config_len];
  int ipmi_interpret_sensor_memory_transition_severity_flags[ipmi_interpret_sensor_memory_transition_severity_config_len];
  int ipmi_interpret_sensor_memory_redundancy_flags[ipmi_interpret_sensor_memory_redundancy_config_len];
  int ipmi_interpret_sensor_drive_slot_flags[ipmi_interpret_sensor_drive_slot_config_len];
  int ipmi_interpret_sensor_drive_slot_state_flags[ipmi_interpret_sensor_drive_slot_state_config_len];
  int ipmi_interpret_sensor_drive_slot_predictive_failure_flags[ipmi_interpret_sensor_drive_slot_predictive_failure_config_len];
  int ipmi_interpret_sensor_drive_slot_device_present_flags[ipmi_interpret_sensor_drive_slot_device_present_config_len];
  int ipmi_interpret_sensor_post_memory_resize_state_flags[ipmi_interpret_sensor_post_memory_resize_state_config_len];
  int ipmi_interpret_sensor_system_firmware_progress_flags[ipmi_interpret_sensor_system_firmware_progress_config_len];
  int ipmi_interpret_sensor_system_firmware_progress_transition_severity_flags[ipmi_interpret_sensor_system_firmware_progress_transition_severity_config_len];
  int ipmi_interpret_sensor_event_logging_disabled_flags[ipmi_interpret_sensor_event_logging_disabled_config_len];
  int ipmi_interpret_sensor_system_event_flags[ipmi_interpret_sensor_system_event_config_len];
  int ipmi_interpret_sensor_system_event_transition_state_flags[ipmi_interpret_sensor_system_event_transition_state_config_len];
  int ipmi_interpret_sensor_system_event_state_flags[ipmi_interpret_sensor_system_event_state_config_len];
  int ipmi_interpret_sensor_critical_interrupt_flags[ipmi_interpret_sensor_critical_interrupt_config_len];
  int ipmi_interpret_sensor_button_switch_flags[ipmi_interpret_sensor_button_switch_config_len];
  int ipmi_interpret_sensor_button_switch_state_flags[ipmi_interpret_sensor_button_switch_state_config_len];
  int ipmi_interpret_sensor_button_switch_transition_severity_flags[ipmi_interpret_sensor_button_switch_transition_severity_config_len];
  int ipmi_interpret_sensor_module_board_state_flags[ipmi_interpret_sensor_module_board_state_config_len];
  int ipmi_interpret_sensor_module_board_device_present_flags[ipmi_interpret_sensor_module_board_device_present_config_len];
  int ipmi_interpret_sensor_chassis_transition_severity_flags[ipmi_interpret_sensor_chassis_transition_severity_config_len];
  int ipmi_interpret_sensor_chip_set_transition_severity_flags[ipmi_interpret_sensor_chip_set_transition_severity_config_len];
  int ipmi_interpret_sensor_cable_interconnect_flags[ipmi_interpret_sensor_cable_interconnect_config_len];
  int ipmi_interpret_sensor_cable_interconnect_transition_severity_flags[ipmi_interpret_sensor_cable_interconnect_transition_severity_config_len];
  int ipmi_interpret_sensor_boot_error_flags[ipmi_interpret_sensor_boot_error_config_len];
  int ipmi_interpret_sensor_boot_error_state_flags[ipmi_interpret_sensor_boot_error_state_config_len];
  int ipmi_interpret_sensor_boot_error_transition_severity_flags[ipmi_interpret_sensor_boot_error_transition_severity_config_len];
  int ipmi_interpret_sensor_os_boot_flags[ipmi_interpret_sensor_os_boot_config_len];
  int ipmi_interpret_sensor_os_critical_stop_state_flags[ipmi_interpret_sensor_os_critical_stop_state_config_len];
  int ipmi_interpret_sensor_slot_connector_flags[ipmi_interpret_sensor_slot_connector_config_len];
  int ipmi_interpret_sensor_slot_connector_transition_severity_flags[ipmi_interpret_sensor_slot_connector_transition_severity_config_len];
  int ipmi_interpret_sensor_system_acpi_power_state_flags[ipmi_interpret_sensor_system_acpi_power_state_config_len];
  int ipmi_interpret_sensor_watchdog2_flags[ipmi_interpret_sensor_watchdog2_config_len];
  int ipmi_interpret_sensor_platform_alert_state_flags[ipmi_interpret_sensor_platform_alert_state_config_len];
  int ipmi_interpret_sensor_entity_presence_flags[ipmi_interpret_sensor_entity_presence_config_len];
  int ipmi_interpret_sensor_entity_presence_device_present_flags[ipmi_interpret_sensor_entity_presence_device_present_config_len];
  int ipmi_interpret_sensor_management_subsystem_health_flags[ipmi_interpret_sensor_management_subsystem_health_config_len];
  int ipmi_interpret_sensor_battery_flags[ipmi_interpret_sensor_battery_config_len];
  int ipmi_interpret_sensor_session_audit_flags[ipmi_interpret_sensor_session_audit_config_len];
  int ipmi_interpret_sensor_version_change_flags[ipmi_interpret_sensor_version_change_config_len];
  int ipmi_interpret_sensor_fru_state_flags[ipmi_interpret_sensor_fru_state_config_len];
  int sensor_oem_bitmask_flag;
  int sensor_oem_value_flag;
  struct conffile_option config_file_options[IPMI_INTERPRET_CONFIG_FILE_OPTIONS_MAX];
  char *config_file = NULL;
  unsigned int config_file_options_len = 0;
  conffile_t cf = NULL;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (!sensor_config_file)
    config_file = INTERPRET_SENSOR_CONFIG_FILE_DEFAULT;
  else
    config_file = (char *)sensor_config_file;

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_threshold_config,
                               ipmi_interpret_sensor_threshold_flags,
                               ipmi_interpret_sensor_threshold_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_temperature_state_config,
                               ipmi_interpret_sensor_temperature_state_flags,
                               ipmi_interpret_sensor_temperature_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_temperature_limit_config,
                               ipmi_interpret_sensor_temperature_limit_flags,
                               ipmi_interpret_sensor_temperature_limit_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_temperature_transition_severity_config,
                               ipmi_interpret_sensor_temperature_transition_severity_flags,
                               ipmi_interpret_sensor_temperature_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_voltage_state_config,
                               ipmi_interpret_sensor_voltage_state_flags,
                               ipmi_interpret_sensor_voltage_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_voltage_limit_config,
                               ipmi_interpret_sensor_voltage_limit_flags,
                               ipmi_interpret_sensor_voltage_limit_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_voltage_performance_config,
                               ipmi_interpret_sensor_voltage_performance_flags,
                               ipmi_interpret_sensor_voltage_performance_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_voltage_transition_severity_config,
                               ipmi_interpret_sensor_voltage_transition_severity_flags,
                               ipmi_interpret_sensor_voltage_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_current_transition_severity_config,
                               ipmi_interpret_sensor_current_transition_severity_flags,
                               ipmi_interpret_sensor_current_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_fan_state_config,
                               ipmi_interpret_sensor_fan_state_flags,
                               ipmi_interpret_sensor_fan_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_fan_transition_severity_config,
                               ipmi_interpret_sensor_fan_transition_severity_flags,
                               ipmi_interpret_sensor_fan_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_fan_device_present_config,
                               ipmi_interpret_sensor_fan_device_present_flags,
                               ipmi_interpret_sensor_fan_device_present_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_fan_transition_availability_config,
                               ipmi_interpret_sensor_fan_transition_availability_flags,
                               ipmi_interpret_sensor_fan_transition_availability_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_fan_redundancy_config,
                               ipmi_interpret_sensor_fan_redundancy_flags,
                               ipmi_interpret_sensor_fan_redundancy_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_physical_security_config,
                               ipmi_interpret_sensor_physical_security_flags,
                               ipmi_interpret_sensor_physical_security_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_platform_security_violation_attempt_config,
                               ipmi_interpret_sensor_platform_security_violation_attempt_flags,
                               ipmi_interpret_sensor_platform_security_violation_attempt_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_processor_config,
                               ipmi_interpret_sensor_processor_flags,
                               ipmi_interpret_sensor_processor_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_processor_state_config,
                               ipmi_interpret_sensor_processor_state_flags,
                               ipmi_interpret_sensor_processor_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_config,
                               ipmi_interpret_sensor_power_supply_flags,
                               ipmi_interpret_sensor_power_supply_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_state_config,
                               ipmi_interpret_sensor_power_supply_state_flags,
                               ipmi_interpret_sensor_power_supply_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_transition_severity_config,
                               ipmi_interpret_sensor_power_supply_transition_severity_flags,
                               ipmi_interpret_sensor_power_supply_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_power_supply_redundancy_config,
                               ipmi_interpret_sensor_power_supply_redundancy_flags,
                               ipmi_interpret_sensor_power_supply_redundancy_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_config,
                               ipmi_interpret_sensor_power_unit_flags,
                               ipmi_interpret_sensor_power_unit_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_device_present_config,
                               ipmi_interpret_sensor_power_unit_device_present_flags,
                               ipmi_interpret_sensor_power_unit_device_present_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_power_unit_redundancy_config,
                               ipmi_interpret_sensor_power_unit_redundancy_flags,
                               ipmi_interpret_sensor_power_unit_redundancy_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_cooling_device_redundancy_config,
                               ipmi_interpret_sensor_cooling_device_redundancy_flags,
                               ipmi_interpret_sensor_cooling_device_redundancy_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_memory_config,
                               ipmi_interpret_sensor_memory_flags,
                               ipmi_interpret_sensor_memory_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_memory_state_config,
                               ipmi_interpret_sensor_memory_state_flags,
                               ipmi_interpret_sensor_memory_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_memory_transition_severity_config,
                               ipmi_interpret_sensor_memory_transition_severity_flags,
                               ipmi_interpret_sensor_memory_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_memory_redundancy_config,
                               ipmi_interpret_sensor_memory_redundancy_flags,
                               ipmi_interpret_sensor_memory_redundancy_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_config,
                               ipmi_interpret_sensor_drive_slot_flags,
                               ipmi_interpret_sensor_drive_slot_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_state_config,
                               ipmi_interpret_sensor_drive_slot_state_flags,
                               ipmi_interpret_sensor_drive_slot_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_predictive_failure_config,
                               ipmi_interpret_sensor_drive_slot_predictive_failure_flags,
                               ipmi_interpret_sensor_drive_slot_predictive_failure_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_drive_slot_device_present_config,
                               ipmi_interpret_sensor_drive_slot_device_present_flags,
                               ipmi_interpret_sensor_drive_slot_device_present_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_post_memory_resize_state_config,
                               ipmi_interpret_sensor_post_memory_resize_state_flags,
                               ipmi_interpret_sensor_post_memory_resize_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_system_firmware_progress_config,
                               ipmi_interpret_sensor_system_firmware_progress_flags,
                               ipmi_interpret_sensor_system_firmware_progress_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_system_firmware_progress_transition_severity_config,
                               ipmi_interpret_sensor_system_firmware_progress_transition_severity_flags,
                               ipmi_interpret_sensor_system_firmware_progress_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_event_logging_disabled_config,
                               ipmi_interpret_sensor_event_logging_disabled_flags,
                               ipmi_interpret_sensor_event_logging_disabled_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_system_event_config,
                               ipmi_interpret_sensor_system_event_flags,
                               ipmi_interpret_sensor_system_event_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_system_event_transition_state_config,
                               ipmi_interpret_sensor_system_event_transition_state_flags,
                               ipmi_interpret_sensor_system_event_transition_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_system_event_state_config,
                               ipmi_interpret_sensor_system_event_state_flags,
                               ipmi_interpret_sensor_system_event_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_critical_interrupt_config,
                               ipmi_interpret_sensor_critical_interrupt_flags,
                               ipmi_interpret_sensor_critical_interrupt_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_config,
                               ipmi_interpret_sensor_button_switch_flags,
                               ipmi_interpret_sensor_button_switch_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_state_config,
                               ipmi_interpret_sensor_button_switch_state_flags,
                               ipmi_interpret_sensor_button_switch_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_button_switch_transition_severity_config,
                               ipmi_interpret_sensor_button_switch_transition_severity_flags,
                               ipmi_interpret_sensor_button_switch_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_module_board_state_config,
                               ipmi_interpret_sensor_module_board_state_flags,
                               ipmi_interpret_sensor_module_board_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_module_board_device_present_config,
                               ipmi_interpret_sensor_module_board_device_present_flags,
                               ipmi_interpret_sensor_module_board_device_present_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_chassis_transition_severity_config,
                               ipmi_interpret_sensor_chassis_transition_severity_flags,
                               ipmi_interpret_sensor_chassis_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_chip_set_transition_severity_config,
                               ipmi_interpret_sensor_chip_set_transition_severity_flags,
                               ipmi_interpret_sensor_chip_set_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_cable_interconnect_config,
                               ipmi_interpret_sensor_cable_interconnect_flags,
                               ipmi_interpret_sensor_cable_interconnect_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_cable_interconnect_transition_severity_config,
                               ipmi_interpret_sensor_cable_interconnect_transition_severity_flags,
                               ipmi_interpret_sensor_cable_interconnect_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_config,
                               ipmi_interpret_sensor_boot_error_flags,
                               ipmi_interpret_sensor_boot_error_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_state_config,
                               ipmi_interpret_sensor_boot_error_state_flags,
                               ipmi_interpret_sensor_boot_error_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_boot_error_transition_severity_config,
                               ipmi_interpret_sensor_boot_error_transition_severity_flags,
                               ipmi_interpret_sensor_boot_error_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_os_boot_config,
                               ipmi_interpret_sensor_os_boot_flags,
                               ipmi_interpret_sensor_os_boot_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_os_critical_stop_state_config,
                               ipmi_interpret_sensor_os_critical_stop_state_flags,
                               ipmi_interpret_sensor_os_critical_stop_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_slot_connector_config,
                               ipmi_interpret_sensor_slot_connector_flags,
                               ipmi_interpret_sensor_slot_connector_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_slot_connector_transition_severity_config,
                               ipmi_interpret_sensor_slot_connector_transition_severity_flags,
                               ipmi_interpret_sensor_slot_connector_transition_severity_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_system_acpi_power_state_config,
                               ipmi_interpret_sensor_system_acpi_power_state_flags,
                               ipmi_interpret_sensor_system_acpi_power_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_watchdog2_config,
                               ipmi_interpret_sensor_watchdog2_flags,
                               ipmi_interpret_sensor_watchdog2_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_platform_alert_state_config,
                               ipmi_interpret_sensor_platform_alert_state_flags,
                               ipmi_interpret_sensor_platform_alert_state_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_entity_presence_config,
                               ipmi_interpret_sensor_entity_presence_flags,
                               ipmi_interpret_sensor_entity_presence_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_entity_presence_device_present_config,
                               ipmi_interpret_sensor_entity_presence_device_present_flags,
                               ipmi_interpret_sensor_entity_presence_device_present_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_management_subsystem_health_config,
                               ipmi_interpret_sensor_management_subsystem_health_flags,
                               ipmi_interpret_sensor_management_subsystem_health_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_battery_config,
                               ipmi_interpret_sensor_battery_flags,
                               ipmi_interpret_sensor_battery_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_session_audit_config,
                               ipmi_interpret_sensor_session_audit_flags,
                               ipmi_interpret_sensor_session_audit_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_version_change_config,
                               ipmi_interpret_sensor_version_change_flags,
                               ipmi_interpret_sensor_version_change_config_len);

  _fill_sensor_config_options (config_file_options,
                               &config_file_options_len,
                               ctx->interpret_sensor.ipmi_interpret_sensor_fru_state_config,
                               ipmi_interpret_sensor_fru_state_flags,
                               ipmi_interpret_sensor_fru_state_config_len);

  config_file_options[config_file_options_len].optionname = "IPMI_OEM_Bitmask";
  config_file_options[config_file_options_len].option_type = CONFFILE_OPTION_LIST_STRING;
  config_file_options[config_file_options_len].option_type_arg = 5;
  config_file_options[config_file_options_len].callback_func = _cb_sensor_oem_parse;
  config_file_options[config_file_options_len].max_count = -1;
  config_file_options[config_file_options_len].required_count = 0;
  config_file_options[config_file_options_len].count_ptr = &sensor_oem_bitmask_flag;
  config_file_options[config_file_options_len].option_ptr = &ctx->interpret_sensor.sensor_oem_config;
  config_file_options[config_file_options_len].option_data = 0;
  config_file_options_len++;

  config_file_options[config_file_options_len].optionname = "IPMI_OEM_Value";
  config_file_options[config_file_options_len].option_type = CONFFILE_OPTION_LIST_STRING;
  config_file_options[config_file_options_len].option_type_arg = 5;
  config_file_options[config_file_options_len].callback_func = _cb_sensor_oem_parse;
  config_file_options[config_file_options_len].max_count = -1;
  config_file_options[config_file_options_len].required_count = 0;
  config_file_options[config_file_options_len].count_ptr = &sensor_oem_value_flag;
  config_file_options[config_file_options_len].option_ptr = &ctx->interpret_sensor.sensor_oem_config;
  config_file_options[config_file_options_len].option_data = 0;
  config_file_options_len++;

  if (!(cf = conffile_handle_create ()))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  if (conffile_parse (cf,
                      config_file,
                      config_file_options,
                      config_file_options_len,
                      NULL,
                      0,
                      0) < 0)
    {
      if (conffile_errnum (cf) == CONFFILE_ERR_EXIST)
        {
          /* Its not an error if the default configuration file doesn't exist */
          if (!config_file)
            {
              rv = 0;
              goto cleanup;
            }
          else
            {
              INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST);
              goto cleanup;
            }
        }
      
      if (CONFFILE_IS_PARSE_ERR (conffile_errnum (cf)))
        INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_PARSE);
      else if (conffile_errnum (cf) == CONFFILE_ERR_OUTMEM)
        INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      else
        INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_INTERNAL_ERROR);
      
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  conffile_handle_destroy (cf);
  return (rv);
}
