/*****************************************************************************\
 *  $Id: ipmi_monitoring_sensor_config.c,v 1.13 2008-04-07 20:49:56 chu11 Exp $
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

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_sensor_config.h"

#include "conffile.h"

struct ipmi_sensor_config ipmi_threshold_sensor_config[] =
  {
    {"IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Critical_Threshold", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Threshold_Sensor_At_Or_Below_Lower_Critical_Threshold", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Recoverable_Threshold", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Critical_Threshold", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Threshold_Sensor_At_Or_Above_Upper_Critical_Threshold", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Recoverable_Threshold", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_voltage_performance_config[] =
  {
    {"IPMI_Voltage_Performance_Met", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Voltage_Performance_Lags", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_fan_device_install_config[] =
  {
    {"IPMI_Fan_Device_Install_Device_Removed_Device_Absent", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Fan_Device_Install_Device_Inserted_Device_Present", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_module_board_state_config[] = 
  {
    {"IPMI_Module_Board_State_Deasserted", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Module_Board_State_Asserted", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_module_board_device_install_config[] =
  {
    {"IPMI_Module_Board_Device_Install_Device_Removed_Device_Absent", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Module_Board_Device_Install_Device_Inserted_Device_Present", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_power_unit_redundancy_config[] =
  {
    {"IPMI_Power_Unit_Redundancy_Fully_Redundant", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Power_Unit_Redundancy_Redundancy_Lost", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Unit_Redundancy_Redundancy_Degraded", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Unit_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_drive_slot_device_install_config[] =
  {
    {"IPMI_Drive_Slot_Device_Removed_Device_Absent", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Drive_Slot_Device_Inserted_Device_Present", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_physical_security_config[] =
  {
    {"IPMI_Physical_Security_General_Chassis_Intrusion", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Physical_Security_Drive_Bay_Intrusion", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Physical_Security_IO_Card_Intrusion", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Physical_Security_Processor_Area_Intrusion", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Physical_Security_LAN_Leash_Lost", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Physical_Security_Unauthorized_Dock_Undock", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Physical_Security_FAN_Area_Intrusion", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_platform_security_violation_attempt_config[] =
  {
    {"IPMI_Platform_Security_Violation_Attempt_Secure_Mode_Violation_Attempt", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_User_Password", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Setup_Password", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Network_Boot_Password", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Platform_Security_Violation_Attempt_Other_Pre_Boot_Password_Violation", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Platform_Security_Violation_Attempt_Out_Of_Band_Access_Password_Violation", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_processor_config[] =
  {
    {"IPMI_Processor_IERR", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_Thermal_Trip", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_FRB1_BIST_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_FRB2_Hang_In_POST_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_FRB3_Processor_Startup_Initialization_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_Configuration_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_SMBIOS_Uncorrectable_CPU_Complex_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_Processor_Presence_Detected", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Processor_Processor_Disabled", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_Terminator_Presence_Detected", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Processor_Processor_Automatically_Throttled", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_power_supply_config[] =
  {
    {"IPMI_Power_Supply_Presence_Detected", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Power_Supply_Power_Supply_Failure_Detected", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Supply_Predictive_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Supply_Power_Supply_Input_Lost_AC_DC", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Supply_Power_Supply_Input_Lost_Or_Out_Of_Range", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Supply_Power_Supply_Input_Out_Of_Range_But_Present", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Supply_Configuration_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_power_unit_config[] =
  {
    {"IPMI_Power_Unit_Power_Off_Power_Down", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Power_Unit_Power_Cycle", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Power_Unit_240VA_Power_Down", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Power_Unit_Interlock_Power_Down", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Power_Unit_AC_Lost", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Unit_Soft_Power_Control_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Unit_Power_Unit_Failure_Detected", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Power_Unit_Predictive_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_memory_config[] =
  {
    {"IPMI_Memory_Correctable_ECC_Memory_Error", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Memory_Uncorrectable_ECC_Memory_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Memory_Parity", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Memory_Memory_Scrub_Failed", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Memory_Memory_Device_Disabled", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Memory_Correctable_ECC_Memory_Error_Logging_Limit_Reached", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Memory_Presence_Detected", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Memory_Configuration_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Memory_Spare", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Memory_Memory_Automatically_Throttled", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_drive_slot_config[] =
  {
    {"IPMI_Drive_Slot_Drive_Presence", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Drive_Slot_Drive_Fault", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Drive_Slot_Predictive_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Drive_Slot_Hot_Spare", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Drive_Slot_Consistency_Check_Parity_Check_In_Progress", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Drive_Slot_In_Critical_Array", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Drive_Slot_In_Failed_Array", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Drive_Slot_Rebuild_Remap_In_Progress", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Drive_Slot_Rebuild_Remap_Aborted", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_system_firmware_progress_config[] =
  {
    {"IPMI_System_Firmware_Progress_System_Firmware_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_System_Firmware_Progress_System_Firmware_Hang", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_System_Firmware_Progress_System_Firmware_Progress", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_event_logging_disabled_config[] =
  {
    {"IPMI_Event_Logging_Disabled_Correctable_Memory_Logging_Disabled", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Event_Logging_Disabled_Event_Type_Logging_Disabled", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Event_Logging_Disabled_Log_Area_Reset_Cleared", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Event_Logging_Disabled_All_Event_Logging_Disabled", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Event_Logging_Disabled_SEL_Full", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Event_Logging_Disabled_SEL_Almost_Full", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_system_event_config[] =
  {
    {"IPMI_System_Event_System_Reconfigured", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_System_Event_OEM_System_Boot_Event", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_System_Event_Undetermined_System_Hardware_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_System_Event_Entry_Added_To_Auxiliary_Log", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_System_Event_PEF_Action", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_System_Event_Timestamp_Clock_Sync", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_critical_interrupt_config[] =
  {
    {"IPMI_Critical_Interrupt_Front_Panel_NMI_Diagnostic_Interrupt", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_Bus_Timeout", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_I_O_Channel_Check_NMI", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_Software_NMI", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Critical_Interrupt_PCI_PERR", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_PCI_SERR", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_EISA_Fail_Safe_Timeout", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_Bus_Correctable_Error", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Critical_Interrupt_Bus_Uncorrectable_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_Fatal_NMI", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Critical_Interrupt_Bus_Fatal_Error", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_cable_interconnect_config[] =
  {
    {"IPMI_Cable_Interconnect_Is_Connected", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Cable_Interconnect_Incorrect_Cable_Connected_Incorrect_Interconnection", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_slot_connector_config[] =
  {
    {"IPMI_Slot_Connector_Fault_Status_Asserted", IPMI_MONITORING_SENSOR_STATE_CRITICAL}, 
    {"IPMI_Slot_Connector_Identify_Status_Asserted", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Slot_Connector_Slot_Connector_Device_Installed_Attached", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Slot_Connector_Slot_Connector_Ready_For_Device_Installation", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Slot_Connector_Slot_Connector_Ready_For_Device_Removal", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Slot_Connector_Slot_Power_Is_Off", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Slot_Connector_Slot_Connector_Device_Removal_Request", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Slot_Connector_Interlock_Asserted", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Slot_Connector_Slot_Is_Disabled", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Slot_Connector_Slot_Holds_Spare_Device", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_watchdog2_config[] =
  {
    {"IPMI_Watchdog2_Timer_Expired", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Watchdog2_Hard_Reset", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Watchdog2_Power_Down", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Watchdog2_Power_Cycle", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Watchdog2_Reserved1", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Watchdog2_Reserved2", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Watchdog2_Reserved3", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Watchdog2_Reserved4", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Watchdog2_Timer_Interrupt", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {NULL, -1},
  };

struct ipmi_sensor_config ipmi_entity_presence_config[] =
  {
    {"IPMI_Entity_Presence_Entity_Present", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_Entity_Presence_Entity_Absent", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Entity_Presence_Entity_Disabled", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},   
  };

struct ipmi_sensor_config ipmi_management_subsystem_health_config[] =
  {
    {"IPMI_Management_Subsystem_Health_Sensor_Access_Degraded_Or_Unavailable", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Management_Subsystem_Health_Controller_Access_Degraded_Or_Unavailable", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Management_Subsystem_Health_Management_Controller_Off_Line", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Management_Subsystem_Health_Management_Controller_Unavailable", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Management_Subsystem_Health_Sensor_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Management_Subsystem_Health_FRU_Failure", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},   
  };

struct ipmi_sensor_config ipmi_battery_config[] =
  {
    {"IPMI_Battery_Battery_Low", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_Battery_Battery_Failed", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_Battery_Battery_Presence_Detected", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {NULL, -1},   
  };

struct ipmi_sensor_config ipmi_fru_state_config[] =
  {
    {"IPMI_FRU_State_FRU_Not_Installed", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_FRU_State_FRU_Inactive", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {"IPMI_FRU_State_FRU_Activation_Requested", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_FRU_State_FRU_Activation_In_Progress", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_FRU_State_FRU_Active", IPMI_MONITORING_SENSOR_STATE_NOMINAL},
    {"IPMI_FRU_State_FRU_Deactivation_Requested", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_FRU_State_FRU_Deactivation_In_Progress", IPMI_MONITORING_SENSOR_STATE_WARNING},
    {"IPMI_FRU_State_FRU_Communication_Lost", IPMI_MONITORING_SENSOR_STATE_CRITICAL},
    {NULL, -1},   
  };

static int _ipmi_monitoring_sensor_config_loaded = 0;

static int
_sensor_state(conffile_t cf, 
              int option_type,
              char *option_string)
{
  assert(cf);
  assert(option_string);

  if (option_type != CONFFILE_OPTION_STRING)
    conffile_seterrnum(cf, CONFFILE_ERR_INTERNAL);

  if (!strcasecmp(option_string, "Nominal"))
    return IPMI_MONITORING_SENSOR_STATE_NOMINAL;
  else if (!strcasecmp(option_string, "Warning"))
    return IPMI_MONITORING_SENSOR_STATE_WARNING;
  else if (!strcasecmp(option_string, "Critical"))
    return IPMI_MONITORING_SENSOR_STATE_CRITICAL;

  conffile_seterrnum(cf, CONFFILE_ERR_PARSE_ARG_INVALID);
  return -1;
}

static int
_cb_sensor_state_parse(conffile_t cf,
                       struct conffile_data *data,
                       char *optionname,
                       int option_type,
                       void *option_ptr,
                       int option_data,
                       void *app_ptr,
                       int app_data)
{
  struct ipmi_sensor_config *sensor_config;
  int sensor_state;
  int i;

  assert(cf);
  assert(data);
  assert(optionname);
  assert(option_ptr);

  if ((sensor_state = _sensor_state(cf, option_type, data->string)) < 0)
    return -1;
  
  i = 0;
  sensor_config = (struct ipmi_sensor_config *)option_ptr;
  while (sensor_config[i].option_str)
    {
      if (!strcasecmp(optionname, sensor_config[i].option_str))
        {
          sensor_config[i].sensor_state = sensor_state;
          return 0;
        }
    }

  conffile_seterrnum(cf, CONFFILE_ERR_INTERNAL);
  return -1;
}

int
ipmi_monitoring_sensor_config(int *errnum)
{
  int threshold_sensor_flag0, threshold_sensor_flag1, threshold_sensor_flag2,
    threshold_sensor_flag3, threshold_sensor_flag4, threshold_sensor_flag5;
  int voltage_performance_flag0, voltage_performance_flag1;
  int fan_device_install_flag0, fan_device_install_flag1;
  int module_board_state_flag0, module_board_state_flag1;
  int module_board_device_install_flag0, module_board_device_install_flag1;
  int power_unit_redundancy_flag0, power_unit_redundancy_flag1, power_unit_redundancy_flag2, 
    power_unit_redundancy_flag3, power_unit_redundancy_flag4, power_unit_redundancy_flag5, 
    power_unit_redundancy_flag6, power_unit_redundancy_flag7;
  int drive_slot_device_install_flag0, drive_slot_device_install_flag1; 
  int physical_security_flag0, physical_security_flag1, physical_security_flag2,
    physical_security_flag3, physical_security_flag4, physical_security_flag5, 
    physical_security_flag6;
  int platform_security_violation_attempt_flag0, platform_security_violation_attempt_flag1,
    platform_security_violation_attempt_flag2, platform_security_violation_attempt_flag3,
    platform_security_violation_attempt_flag4, platform_security_violation_attempt_flag5;
  int processor_flag0, processor_flag1, processor_flag2, processor_flag3, 
    processor_flag4, processor_flag5, processor_flag6, processor_flag7, 
    processor_flag8, processor_flag9, processor_flag10;
  int power_supply_flag0, power_supply_flag1, power_supply_flag2, power_supply_flag3,
    power_supply_flag4, power_supply_flag5, power_supply_flag6;
  int power_unit_flag0, power_unit_flag1, power_unit_flag2, power_unit_flag3, 
    power_unit_flag4, power_unit_flag5, power_unit_flag6, power_unit_flag7;
  int memory_flag0, memory_flag1, memory_flag2, memory_flag3, memory_flag4,
    memory_flag5, memory_flag6, memory_flag7, memory_flag8, memory_flag9;
  int drive_slot_flag0, drive_slot_flag1, drive_slot_flag2, drive_slot_flag3, 
    drive_slot_flag4, drive_slot_flag5, drive_slot_flag6, drive_slot_flag7, 
    drive_slot_flag8;
  int system_firmware_progress_flag0, system_firmware_progress_flag1, system_firmware_progress_flag2;
  int event_logging_disabled_flag0, event_logging_disabled_flag1, event_logging_disabled_flag2,
    event_logging_disabled_flag3, event_logging_disabled_flag4, event_logging_disabled_flag5;
  int system_event_flag0, system_event_flag1, system_event_flag2,
    system_event_flag3, system_event_flag4, system_event_flag5;
  int critical_interrupt_flag0, critical_interrupt_flag1, critical_interrupt_flag2, 
    critical_interrupt_flag3, critical_interrupt_flag4, critical_interrupt_flag5, 
    critical_interrupt_flag6, critical_interrupt_flag7, critical_interrupt_flag8, 
    critical_interrupt_flag9, critical_interrupt_flag10;
  int cable_interconnect_flag0, cable_interconnect_flag1;
  int slot_connector_flag0, slot_connector_flag1, slot_connector_flag2, 
    slot_connector_flag3, slot_connector_flag4, slot_connector_flag5, 
    slot_connector_flag6, slot_connector_flag7, slot_connector_flag8, 
    slot_connector_flag9;
  int watchdog2_flag0, watchdog2_flag1, watchdog2_flag2, watchdog2_flag3, 
    watchdog2_flag4, watchdog2_flag5, watchdog2_flag6, watchdog2_flag7, 
    watchdog2_flag8;
  int entity_presence_flag0, entity_presence_flag1, entity_presence_flag2;
  int management_subsystem_health_flag0, management_subsystem_health_flag1,
    management_subsystem_health_flag2, management_subsystem_health_flag3,
    management_subsystem_health_flag4, management_subsystem_health_flag5;
  int battery_flag0, battery_flag1, battery_flag2;
  int fru_state_flag0, fru_state_flag1, fru_state_flag2, fru_state_flag3,
    fru_state_flag4, fru_state_flag5, fru_state_flag6, fru_state_flag7;
  conffile_t cf = NULL;
  int num;
  int rv = -1;
  struct conffile_option options[] =
    {
      /*
       * IPMI_Threshold_Sensor
       */
      {ipmi_threshold_sensor_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &threshold_sensor_flag0,
       ipmi_threshold_sensor_config,
       0
      },
      {ipmi_threshold_sensor_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &threshold_sensor_flag1,
       ipmi_threshold_sensor_config,
       0
      },
      {ipmi_threshold_sensor_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &threshold_sensor_flag2,
       ipmi_threshold_sensor_config,
       0
      },
      {ipmi_threshold_sensor_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &threshold_sensor_flag3,
       ipmi_threshold_sensor_config,
       0
      },
      {ipmi_threshold_sensor_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &threshold_sensor_flag4,
       ipmi_threshold_sensor_config,
       0
      },
      {ipmi_threshold_sensor_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &threshold_sensor_flag5,
       ipmi_threshold_sensor_config,
       0
      },
      /* 
       * IPMI_Voltage_Performance
       */
      {ipmi_voltage_performance_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &voltage_performance_flag0,
       ipmi_voltage_performance_config,
       0
      },
      {ipmi_voltage_performance_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &voltage_performance_flag1,
       ipmi_voltage_performance_config,
       0
      },
      /* 
       * IPMI_Fan_Device_Install
       */
      {ipmi_fan_device_install_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fan_device_install_flag0,
       ipmi_fan_device_install_config,
       0
      },
      {ipmi_fan_device_install_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fan_device_install_flag1,
       ipmi_fan_device_install_config,
       0
      },
      /* 
       * IPMI_Module_Board_State
       */
      {ipmi_module_board_state_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &module_board_state_flag0,
       ipmi_module_board_state_config,
       0
      },
      {ipmi_module_board_state_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &module_board_state_flag1,
       ipmi_module_board_state_config,
       0
      },
      /* 
       * IPMI_Module_Board_Device_Install
       */
      {ipmi_module_board_device_install_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &module_board_device_install_flag0,
       ipmi_module_board_device_install_config,
       0
      },
      {ipmi_module_board_device_install_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &module_board_device_install_flag1,
       ipmi_module_board_device_install_config,
       0
      },
      /* 
       * IPMI_Power_Unit_Redundancy
       */
      {ipmi_power_unit_redundancy_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag0,
       ipmi_power_unit_redundancy_config,
       0
      },
      {ipmi_power_unit_redundancy_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag1,
       ipmi_power_unit_redundancy_config,
       0
      },
      {ipmi_power_unit_redundancy_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag2,
       ipmi_power_unit_redundancy_config,
       0
      },
      {ipmi_power_unit_redundancy_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag3,
       ipmi_power_unit_redundancy_config,
       0
      },
      {ipmi_power_unit_redundancy_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag4,
       ipmi_power_unit_redundancy_config,
       0
      },
      {ipmi_power_unit_redundancy_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag5,
       ipmi_power_unit_redundancy_config,
       0
      },
      {ipmi_power_unit_redundancy_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag6,
       ipmi_power_unit_redundancy_config,
       0
      },
      {ipmi_power_unit_redundancy_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_redundancy_flag7,
       ipmi_power_unit_redundancy_config,
       0
      },
      /* 
       * IPMI_Drive_Slot_Device_Install
       */
      {
        ipmi_drive_slot_device_install_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_device_install_flag0,
       ipmi_drive_slot_device_install_config,
       0
      },
      {
        ipmi_drive_slot_device_install_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_device_install_flag1,
       ipmi_drive_slot_device_install_config,
       0
      },
      /* 
       * IPMI_Physical_Security
       */
      {ipmi_physical_security_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &physical_security_flag0,
       ipmi_physical_security_config,
       0
      },
      {ipmi_physical_security_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &physical_security_flag1,
       ipmi_physical_security_config,
       0
      },
      {ipmi_physical_security_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &physical_security_flag2,
       ipmi_physical_security_config,
       0
      },
      {ipmi_physical_security_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &physical_security_flag3,
       ipmi_physical_security_config,
       0
      },
      {ipmi_physical_security_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &physical_security_flag4,
       ipmi_physical_security_config,
       0
      },
      {ipmi_physical_security_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &physical_security_flag5,
       ipmi_physical_security_config,
       0
      },
      {ipmi_physical_security_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &physical_security_flag6,
       ipmi_physical_security_config,
       0
      },
      /* 
       * IPMI_Platform_Security_Violation_Attempt
       */
      {ipmi_platform_security_violation_attempt_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &platform_security_violation_attempt_flag0,
       ipmi_platform_security_violation_attempt_config,
       0
      },
      {ipmi_platform_security_violation_attempt_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &platform_security_violation_attempt_flag1,
       ipmi_platform_security_violation_attempt_config,
       0
      },
      {ipmi_platform_security_violation_attempt_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &platform_security_violation_attempt_flag2,
       ipmi_platform_security_violation_attempt_config,
       0
      },
      {ipmi_platform_security_violation_attempt_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &platform_security_violation_attempt_flag3,
       ipmi_platform_security_violation_attempt_config,
       0
      },
      {ipmi_platform_security_violation_attempt_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &platform_security_violation_attempt_flag4,
       ipmi_platform_security_violation_attempt_config,
       0
      },
      {ipmi_platform_security_violation_attempt_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &platform_security_violation_attempt_flag5,
       ipmi_platform_security_violation_attempt_config,
       0
      },
      /* 
       * IPMI_Processor
       */
      {ipmi_processor_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag0,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag1,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag2,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag3,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag4,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag5,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag6,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag7,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[8].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag8,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[9].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag9,
       ipmi_processor_config,
       0
      },
      {ipmi_processor_config[10].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &processor_flag10,
       ipmi_processor_config,
       0
      },
      /* 
       * IPMI_Power_Supply
       */
      {ipmi_power_supply_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_supply_flag0,
       ipmi_power_supply_config,
       0
      },
      {ipmi_power_supply_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_supply_flag1,
       ipmi_power_supply_config,
       0
      },
      {ipmi_power_supply_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_supply_flag2,
       ipmi_power_supply_config,
       0
      },
      {ipmi_power_supply_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_supply_flag3,
       ipmi_power_supply_config,
       0
      },
      {ipmi_power_supply_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_supply_flag4,
       ipmi_power_supply_config,
       0
      },
      {ipmi_power_supply_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_supply_flag5,
       ipmi_power_supply_config,
       0
      },
      {ipmi_power_supply_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_supply_flag6,
       ipmi_power_supply_config,
       0
      },
      /* 
       * IPMI_Power_Unit
       */
      {ipmi_power_unit_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag0,
       ipmi_power_unit_config,
       0
      },
      {ipmi_power_unit_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag1,
       ipmi_power_unit_config,
       0
      },
      {ipmi_power_unit_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag2,
       ipmi_power_unit_config,
       0
      },
      {ipmi_power_unit_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag3,
       ipmi_power_unit_config,
       0
      },
      {ipmi_power_unit_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag4,
       ipmi_power_unit_config,
       0
      },
      {ipmi_power_unit_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag5,
       ipmi_power_unit_config,
       0
      },
      {ipmi_power_unit_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag6,
       ipmi_power_unit_config,
       0
      },
      {ipmi_power_unit_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &power_unit_flag7,
       ipmi_power_unit_config,
       0
      },
      /* 
       * IPMI_Memory
       */
      {ipmi_memory_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag0,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag1,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag2,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag3,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag4,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag5,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag6,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag7,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[8].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag8,
       ipmi_memory_config,
       0
      },
      {ipmi_memory_config[9].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &memory_flag9,
       ipmi_memory_config,
       0
      },
      /* 
       * IPMI_Drive_Slot
       */
      {ipmi_drive_slot_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag0,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag1,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag2,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag3,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag4,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag5,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag6,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag7,
       ipmi_drive_slot_config,
       0
      },
      {ipmi_drive_slot_config[8].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &drive_slot_flag8,
       ipmi_drive_slot_config,
       0
      },
      /* 
       * IPMI_System_Firmware_Progress
       */
      {ipmi_system_firmware_progress_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_firmware_progress_flag0,
       ipmi_system_firmware_progress_config,
       0
      },
      {ipmi_system_firmware_progress_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_firmware_progress_flag1,
       ipmi_system_firmware_progress_config,
       0
      },
      {ipmi_system_firmware_progress_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_firmware_progress_flag2,
       ipmi_system_firmware_progress_config,
       0
      },
      /* 
       * IPMI_Event_Logging_Disabled
       */
      {ipmi_event_logging_disabled_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &event_logging_disabled_flag0,
       ipmi_event_logging_disabled_config,
       0
      },
      {ipmi_event_logging_disabled_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &event_logging_disabled_flag1,
       ipmi_event_logging_disabled_config,
       0
      },
      {ipmi_event_logging_disabled_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &event_logging_disabled_flag2,
       ipmi_event_logging_disabled_config,
       0
      },
      {ipmi_event_logging_disabled_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &event_logging_disabled_flag3,
       ipmi_event_logging_disabled_config,
       0
      },
      {ipmi_event_logging_disabled_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &event_logging_disabled_flag4,
       ipmi_event_logging_disabled_config,
       0
      },
      {ipmi_event_logging_disabled_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &event_logging_disabled_flag5,
       ipmi_event_logging_disabled_config,
       0
      },
      /* 
       * IPMI_System_Event
       */
      {ipmi_system_event_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_event_flag0,
       ipmi_system_event_config,
       0
      },
      {ipmi_system_event_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_event_flag1,
       ipmi_system_event_config,
       0
      },
      {ipmi_system_event_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_event_flag2,
       ipmi_system_event_config,
       0
      },
      {ipmi_system_event_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_event_flag3,
       ipmi_system_event_config,
       0
      },
      {ipmi_system_event_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_event_flag4,
       ipmi_system_event_config,
       0
      },
      {ipmi_system_event_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &system_event_flag5,
       ipmi_system_event_config,
       0
      },
      /* 
       * IPMI_Critical_Interrupt
       */
      {ipmi_critical_interrupt_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag0,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag1,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag2,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag3,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag4,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag5,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag6,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag7,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[8].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag8,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[9].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag9,
       ipmi_critical_interrupt_config,
       0
      },
      {ipmi_critical_interrupt_config[10].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &critical_interrupt_flag10,
       ipmi_critical_interrupt_config,
       0
      },
      /* 
       * IPMI_Cable_Interconnect
       */
      {ipmi_cable_interconnect_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &cable_interconnect_flag0,
       ipmi_cable_interconnect_config,
       0
      },
      {ipmi_cable_interconnect_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &cable_interconnect_flag1,
       ipmi_cable_interconnect_config,
       0
      },
      /* 
       * IPMI_Slot_Connector
       */
      {ipmi_slot_connector_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag0,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag1,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag2,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag3,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag4,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag5,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag6,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag7,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[8].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag8,
       ipmi_slot_connector_config,
       0
      },
      {ipmi_slot_connector_config[9].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &slot_connector_flag9,
       ipmi_slot_connector_config,
       0
      },
      /* 
       * IPMI_Watchdog2
       */
      {ipmi_watchdog2_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag0,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag1,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag2,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag3,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag4,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag5,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag6,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag7,
       ipmi_watchdog2_config,
       0
      },
      {ipmi_watchdog2_config[8].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &watchdog2_flag8,
       ipmi_watchdog2_config,
       0
      },
      /* 
       * IPMI_Entity_Presence
       */
      {ipmi_entity_presence_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &entity_presence_flag0,
       ipmi_entity_presence_config,
       0
      },
      {ipmi_entity_presence_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &entity_presence_flag1,
       ipmi_entity_presence_config,
       0
      },
      {ipmi_entity_presence_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &entity_presence_flag2,
       ipmi_entity_presence_config,
       0
      },
      /* 
       * IPMI_Management_Subsystem_Health
       */
      {ipmi_management_subsystem_health_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &management_subsystem_health_flag0,
       ipmi_management_subsystem_health_config,
       0
      },
      {ipmi_management_subsystem_health_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &management_subsystem_health_flag1,
       ipmi_management_subsystem_health_config,
       0
      },
      {ipmi_management_subsystem_health_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &management_subsystem_health_flag2,
       ipmi_management_subsystem_health_config,
       0
      },
      {ipmi_management_subsystem_health_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &management_subsystem_health_flag3,
       ipmi_management_subsystem_health_config,
       0
      },
      {ipmi_management_subsystem_health_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &management_subsystem_health_flag4,
       ipmi_management_subsystem_health_config,
       0
      },
      {ipmi_management_subsystem_health_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &management_subsystem_health_flag5,
       ipmi_management_subsystem_health_config,
       0
      },
      /* 
       * IPMI_Battery
       */
      {ipmi_battery_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &battery_flag0,
       ipmi_battery_config,
       0
      },
      {ipmi_battery_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &battery_flag1,
       ipmi_battery_config,
       0
      },
      {ipmi_battery_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &battery_flag2,
       ipmi_battery_config,
       0
      },
      /* 
       * IPMI_FRU_State
       */
      {ipmi_fru_state_config[0].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag0,
       ipmi_fru_state_config,
       0
      },
      {ipmi_fru_state_config[1].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag1,
       ipmi_fru_state_config,
       0
      },
      {ipmi_fru_state_config[2].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag2,
       ipmi_fru_state_config,
       0
      },
      {ipmi_fru_state_config[3].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag3,
       ipmi_fru_state_config,
       0
      },
      {ipmi_fru_state_config[4].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag4,
       ipmi_fru_state_config,
       0
      },
      {ipmi_fru_state_config[5].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag5,
       ipmi_fru_state_config,
       0
      },
      {ipmi_fru_state_config[6].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag6,
       ipmi_fru_state_config,
       0
      },
      {ipmi_fru_state_config[7].option_str,
       CONFFILE_OPTION_STRING,
       -1,
       _cb_sensor_state_parse,
       1,
       0,
       &fru_state_flag7,
       ipmi_fru_state_config,
       0
      },
    };

  if (_ipmi_monitoring_sensor_config_loaded)
    return 0;

  if (!(cf = conffile_handle_create()))
    {
      IPMI_MONITORING_DEBUG(("conffile_handle_create: %s", strerror(errno)));
      return -1;
    }

  num = sizeof(options)/sizeof(struct conffile_option);
  if (conffile_parse(cf, 
                     IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT, 
                     options, 
                     num,
                     NULL, 
                     0, 
                     0) < 0)
    {
      char buf[CONFFILE_MAX_ERRMSGLEN];

      /* Its not an error if the default configuration file doesn't exist */
      if (conffile_errnum(cf) == CONFFILE_ERR_EXIST)
        {
          rv = 0;
          goto cleanup;
        }

      if (CONFFILE_IS_PARSE_ERR(conffile_errnum(cf)))
        {
          if (errnum)
            *errnum = IPMI_MONITORING_ERR_SENSOR_CONFIG_FILE_PARSE;
        }
      else if (conffile_errnum(cf) == CONFFILE_ERR_OUTMEM)
        {
          if (errnum)
            *errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
        }
      else
        {
          if (errnum)
            *errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
        }

      if (conffile_errmsg(cf, buf, CONFFILE_MAX_ERRMSGLEN) < 0)
        IPMI_MONITORING_DEBUG(("conffile_parse: %d", conffile_errnum(cf)));
      else
        IPMI_MONITORING_DEBUG(("conffile_parse: %s", buf));

      goto cleanup;
    }
  
  rv = 0;
  _ipmi_monitoring_sensor_config_loaded++;
 cleanup:
  if (cf)
    conffile_handle_destroy(cf);
  return rv;
}
