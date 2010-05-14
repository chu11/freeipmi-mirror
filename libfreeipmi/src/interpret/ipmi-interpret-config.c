/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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
 *  Copyright (C) 2007-2010 Lawrence Livermore National Security, LLC.
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
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-oem-spec.h"

#include "ipmi-interpret-defs.h"
#include "ipmi-interpret-trace.h"
#include "ipmi-interpret-config.h"
#include "ipmi-interpret-util.h"

#include "freeipmi-portability.h"
#include "conffile.h"
#include "hash.h"

#define IPMI_INTERPRET_CONFIG_FILE_OPTIONS_MAX 1024

/*
 * Standard Sensors
 */

static struct ipmi_interpret_config ipmi_interpret_threshold_sel_config[] =
  {
    { "IPMI_Threshold_Sensor_Lower_Non_Critical_Going_Low", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Threshold_Sensor_Lower_Non_Critical_Going_High", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_Lower_Critical_Going_Low", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_Lower_Critical_Going_High", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Threshold_Sensor_Lower_Non_Recoverable_Going_Low", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_Lower_Non_Recoverable_Going_High", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_Upper_Non_Critical_Going_Low", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_Upper_Non_Critical_Going_High", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Threshold_Sensor_Upper_Critical_Going_Low", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Threshold_Sensor_Upper_Critical_Going_High", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_Upper_Non_Recoverable_Going_Low", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_Upper_Non_Recoverable_Going_High", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_threshold_sel_config_len = 12;

static struct ipmi_interpret_config ipmi_interpret_threshold_sensor_config[] =
  {
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Critical_Threshold", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Critical_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Recoverable_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Critical_Threshold", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Critical_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Recoverable_Threshold", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_threshold_sensor_config_len = 6;

static struct ipmi_interpret_config ipmi_interpret_voltage_state_config[] =
  {
    { "IPMI_Voltage_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_voltage_state_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_voltage_performance_config[] =
  {
    { "IPMI_Voltage_Performance_Met", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Voltage_Performance_Lags", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_voltage_performance_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_fan_device_present_config[] =
  {
    { "IPMI_Fan_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_fan_device_present_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_fan_transition_availability_config[] =
  {
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
static unsigned int ipmi_interpret_fan_transition_availability_config_len = 9;

static struct ipmi_interpret_config ipmi_interpret_fan_redundancy_config[] =
  {
    { "IPMI_Fan_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Fan_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_fan_redundancy_config_len = 8;

static struct ipmi_interpret_config ipmi_interpret_physical_security_config[] =
  {
    { "IPMI_Physical_Security_General_Chassis_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_Drive_Bay_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_IO_Card_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_Processor_Area_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_LAN_Leash_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_Unauthorized_Dock_Undock", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Physical_Security_FAN_Area_Intrusion", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_physical_security_config_len = 7;

static struct ipmi_interpret_config ipmi_interpret_platform_security_violation_attempt_config[] =
  {
    { "IPMI_Platform_Security_Violation_Attempt_Secure_Mode_Violation_Attempt", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_User_Password", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Attempt_Setup_Password", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Network_Boot_Password", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Other_Pre_Boot_Password_Violation", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Out_Of_Band_Access_Password_Violation", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_platform_security_violation_attempt_config_len = 6;

static struct ipmi_interpret_config ipmi_interpret_processor_config[] =
  {
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
static unsigned int ipmi_interpret_processor_config_len = 13;

static struct ipmi_interpret_config ipmi_interpret_processor_state_config[] =
  {
    { "IPMI_Processor_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Processor_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_processor_state_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_power_supply_config[] =
  {
    { "IPMI_Power_Supply_Presence_Detected", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Power_Supply_Failure_Detected", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Predictive_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Lost_AC_DC", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Lost_Or_Out_Of_Range", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Out_Of_Range_But_Present", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Configuration_Error", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_power_supply_config_len = 7;

static struct ipmi_interpret_config ipmi_interpret_power_supply_state_config[] =
  {
    { "IPMI_Power_Supply_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_State_Asserted", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_power_supply_state_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_power_supply_redundancy_config[] =
  {
    { "IPMI_Power_Supply_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_power_supply_redundancy_config_len = 8;

static struct ipmi_interpret_config ipmi_interpret_power_unit_config[] =
  {
    { "IPMI_Power_Unit_Power_Off_Power_Down", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_Power_Cycle", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_240VA_Power_Down", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_Interlock_Power_Down", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_AC_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Soft_Power_Control_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Power_Unit_Failure_Detected", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Predictive_Failure", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_power_unit_config_len = 8;

static struct ipmi_interpret_config ipmi_interpret_power_unit_device_present_config[] =
  {
    { "IPMI_Power_Unit_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_power_unit_device_present_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_power_unit_redundancy_config[] =
  {
    { "IPMI_Power_Unit_Redundancy_Fully_Redundant", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Lost", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_power_unit_redundancy_config_len = 8;

static struct ipmi_interpret_config ipmi_interpret_memory_config[] =
  {
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
static unsigned int ipmi_interpret_memory_config_len = 11;

static struct ipmi_interpret_config ipmi_interpret_drive_slot_config[] =
  {
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
static unsigned int ipmi_interpret_drive_slot_config_len = 9;

static struct ipmi_interpret_config ipmi_interpret_drive_slot_state_config[] =
  {
    { "IPMI_Drive_Slot_State_Deasserted", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Drive_Slot_State_Asserted", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_drive_slot_state_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_drive_slot_predictive_failure_config[] =
  {
    { "IPMI_Drive_Slot_Predictive_Failure_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Predictive_Failure_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_drive_slot_predictive_failure_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_drive_slot_device_present_config[] =
  {
    { "IPMI_Drive_Slot_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_drive_slot_device_present_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_system_firmware_progress_config[] =
  {
    { "IPMI_System_Firmware_Progress_System_Firmware_Error", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_System_Firmware_Hang", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_System_Firmware_Progress", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_system_firmware_progress_config_len = 3;

static struct ipmi_interpret_config ipmi_interpret_event_logging_disabled_config[] =
  {
    { "IPMI_Event_Logging_Disabled_Correctable_Memory_Error_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_Event_Type_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_Log_Area_Reset_Cleared", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Event_Logging_Disabled_All_Event_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_SEL_Full", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_SEL_Almost_Full", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Event_Logging_Disabled_Correctable_Machine_Check_Error_Logging_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_event_logging_disabled_config_len = 7;

static struct ipmi_interpret_config ipmi_interpret_system_event_config[] =
  {
    { "IPMI_System_Event_System_Reconfigured", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_System_Event_OEM_System_Boot_Event", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_Undetermined_System_Hardware_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_System_Event_Entry_Added_To_Auxiliary_Log", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_PEF_Action", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Event_Timestamp_Clock_Sync", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_system_event_config_len = 6;

static struct ipmi_interpret_config ipmi_interpret_critical_interrupt_config[] =
  {
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
static unsigned int ipmi_interpret_critical_interrupt_config_len = 12;

static struct ipmi_interpret_config ipmi_interpret_button_switch_config[] =
  {
    { "IPMI_Button_Switch_Power_Button_Pressed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_Sleep_Button_Pressed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_Reset_Button_Pressed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_FRU_Latch_Open", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Button_Switch_FRU_Service_Request_Button", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_button_switch_config_len = 5;

/* achu: for a button/switch states, I don't think users really care.
 * So report Nominal for all states.
 */
static struct ipmi_interpret_config ipmi_interpret_button_switch_state_config[] =
  {
    { "IPMI_Button_Switch_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Button_Switch_State_Asserted", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_button_switch_state_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_chip_set_config[] =
  {
    { "IPMI_Chip_Set_Soft_Power_Control_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Chip_Set_Thermal_Trip", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_chip_set_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_module_board_state_config[] =
  {
    { "IPMI_Module_Board_State_Deasserted", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Module_Board_State_Asserted", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_module_board_state_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_module_board_device_present_config[] =
  {
    { "IPMI_Module_Board_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Module_Board_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_module_board_device_present_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_cable_interconnect_config[] =
  {
    { "IPMI_Cable_Interconnect_Is_Connected", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Cable_Interconnect_Configuration_Error", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_cable_interconnect_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_system_boot_initiated_config[] =
  {
    { "IPMI_System_Boot_Initiated_Initiated_By_Power_Up", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Boot_Initiated_Initiated_By_Hard_Reset", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Boot_Initiated_Initiated_By_Warm_Reset", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Boot_Initiated_User_Requested_PXE_Boot", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Boot_Initiated_Automatic_Boot_To_Diagnostic", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_System_Boot_Initiated_OS_Run_Time_Software_Initiated_Hard_Reset", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_System_Boot_Initiated_OS_Run_Time_Software_Initiated_Warm_Reset", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_System_Boot_Initiated_System_Restart", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_system_boot_initiated_config_len = 8;

static struct ipmi_interpret_config ipmi_interpret_boot_error_config[] =
  {
    { "IPMI_Boot_Error_No_Bootable_Media", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Non_Bootable_Diskette_Left_In_Drive", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_PXE_Server_Not_Found", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Invalid_Boot_Sector", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Boot_Error_Timeout_Waiting_For_User_Selection_Of_Boot_Source", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_boot_error_config_len = 5;


static struct ipmi_interpret_config ipmi_interpret_os_boot_config[] =
  {
    { "IPMI_OS_Boot_A_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_C_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_PXE_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_Diagnostic_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_CD_ROM_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_ROM_Boot_Completed", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_OS_Boot_Boot_Completed_Boot_Device_Not_Specified", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_os_boot_config_len = 7;

static struct ipmi_interpret_config ipmi_interpret_os_critical_stop_config[] =
  {
    { "IPMI_OS_Critical_Stop_Critical_Stop_During_OS_Load", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_OS_Critical_Stop_Run_Time_Critical_Stop", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_OS_Critical_Stop_OS_Graceful_Stop", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_OS_Critical_Stop_OS_Graceful_Shutdown", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_OS_Critical_Stop_Soft_Shutdown_Initiated_By_PEF", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_OS_Critical_Stop_Agent_Not_Responding", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_os_critical_stop_config_len = 6;

static struct ipmi_interpret_config ipmi_interpret_slot_connector_config[] =
  {
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
static unsigned int ipmi_interpret_slot_connector_config_len = 10;

static struct ipmi_interpret_config ipmi_interpret_system_acpi_power_state_config[] =
  {
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
static unsigned int ipmi_interpret_system_acpi_power_state_config_len = 15;

static struct ipmi_interpret_config ipmi_interpret_watchdog2_config[] =
  {
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
static unsigned int ipmi_interpret_watchdog2_config_len = 9;

static struct ipmi_interpret_config ipmi_interpret_platform_alert_config[] =
  {
    { "IPMI_Platform_Alert_Platform_Generated_Page", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Platform_Alert_Platform_Generated_LAN_Alert", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Platform_Alert_Platform_Event_Trap_Generated", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Platform_Alert_Platform_Generated_SNMP_Trap", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_platform_alert_config_len = 4;

static struct ipmi_interpret_config ipmi_interpret_entity_presence_config[] =
  {
    { "IPMI_Entity_Presence_Entity_Present", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Entity_Presence_Entity_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Entity_Presence_Entity_Disabled", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_entity_presence_config_len = 3;

static struct ipmi_interpret_config ipmi_interpret_entity_presence_device_present_config[] =
  {
    { "IPMI_Entity_Presence_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Entity_Presence_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_entity_presence_device_present_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_lan_config[] =
  {
    { "IPMI_LAN_Heartbeat", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_LAN_Heartbeat_Lost", IPMI_INTERPRET_STATE_WARNING},
  };
static unsigned int ipmi_interpret_lan_config_len = 2;

static struct ipmi_interpret_config ipmi_interpret_management_subsystem_health_config[] =
  {
    { "IPMI_Management_Subsystem_Health_Sensor_Access_Degraded_Or_Unavailable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Controller_Access_Degraded_Or_Unavailable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Management_Controller_Off_Line", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Management_Controller_Unavailable", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Sensor_Failure", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_FRU_Failure", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_management_subsystem_health_config_len = 6;

static struct ipmi_interpret_config ipmi_interpret_battery_config[] =
  {
    { "IPMI_Battery_Battery_Low", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Battery_Battery_Failed", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Battery_Battery_Presence_Detected", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_battery_config_len = 3;

static struct ipmi_interpret_config ipmi_interpret_session_audit_config[] =
  {
    { "IPMI_Session_Audit_Session_Activated", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Session_Audit_Session_Deactivated", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Session_Audit_Invalid_Username_Or_Password", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Session_Audit_Invalid_Password_Disable", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_session_audit_config_len = 4;

static struct ipmi_interpret_config ipmi_interpret_version_change_config[] =
  {
    { "IPMI_Version_Change_Hardware_Change_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Version_Change_Firmware_Or_Software_Change_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_Version_Change_Hardware_Incompatability_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Firmware_Or_Software_Incompatability_Detected_With_Associated_Entity", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Entity_Is_Of_An_Invalid_Or_Unsupported_Hardware_Version", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Entity_Contains_An_Invalid_Or_Unsupported_Firmware_Or_Software_Version", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_Version_Change_Hardware_Change_Detected_With_Associated_Entity_Was_Successful", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_Version_Change_Software_Or_FW_Change_Detected_With_Associated_Entity_Was_Successful", IPMI_INTERPRET_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_version_change_config_len = 8;

static struct ipmi_interpret_config ipmi_interpret_fru_state_config[] =
  {
    { "IPMI_FRU_State_FRU_Not_Installed", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_FRU_State_FRU_Inactive", IPMI_INTERPRET_STATE_CRITICAL},
    { "IPMI_FRU_State_FRU_Activation_Requested", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Activation_In_Progress", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Active", IPMI_INTERPRET_STATE_NOMINAL},
    { "IPMI_FRU_State_FRU_Deactivation_Requested", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Deactivation_In_Progress", IPMI_INTERPRET_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Communication_Lost", IPMI_INTERPRET_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_fru_state_config_len = 8;

static int
_interpret_config_init (ipmi_interpret_ctx_t ctx,
                        struct ipmi_interpret_config ***config_dest,
                        struct ipmi_interpret_config *config_src,
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
  mlen = sizeof (struct ipmi_interpret_config *) * (config_len + 1);

  if (!((*config_dest) = (struct ipmi_interpret_config **) malloc (mlen)))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }
  memset ((*config_dest), '\0', mlen);

  mlen = sizeof (struct ipmi_interpret_config);

  for (i = 0; i < config_len; i++)
    {
      if (!((*config_dest)[i] = (struct ipmi_interpret_config *) malloc (mlen)))
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

int
ipmi_interpret_sel_init (ipmi_interpret_ctx_t ctx)
{
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_threshold_sel_config,
                              ipmi_interpret_threshold_sel_config,
                              ipmi_interpret_threshold_sel_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_voltage_state_config,
                              ipmi_interpret_voltage_state_config,
                              ipmi_interpret_voltage_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_voltage_performance_config,
                              ipmi_interpret_voltage_performance_config,
                              ipmi_interpret_voltage_performance_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_fan_device_present_config,
                              ipmi_interpret_fan_device_present_config,
                              ipmi_interpret_fan_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_fan_transition_availability_config,
                              ipmi_interpret_fan_transition_availability_config,
                              ipmi_interpret_fan_transition_availability_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_fan_redundancy_config,
                              ipmi_interpret_fan_redundancy_config,
                              ipmi_interpret_fan_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_physical_security_config,
                              ipmi_interpret_physical_security_config,
                              ipmi_interpret_physical_security_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_platform_security_violation_attempt_config,
                              ipmi_interpret_platform_security_violation_attempt_config,
                              ipmi_interpret_platform_security_violation_attempt_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_processor_config,
                              ipmi_interpret_processor_config,
                              ipmi_interpret_processor_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_processor_state_config,
                              ipmi_interpret_processor_state_config,
                              ipmi_interpret_processor_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_power_supply_config,
                              ipmi_interpret_power_supply_config,
                              ipmi_interpret_power_supply_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_power_supply_state_config,
                              ipmi_interpret_power_supply_state_config,
                              ipmi_interpret_power_supply_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_power_supply_redundancy_config,
                              ipmi_interpret_power_supply_redundancy_config,
                              ipmi_interpret_power_supply_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_power_unit_config,
                              ipmi_interpret_power_unit_config,
                              ipmi_interpret_power_unit_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_power_unit_device_present_config,
                              ipmi_interpret_power_unit_device_present_config,
                              ipmi_interpret_power_unit_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_power_unit_redundancy_config,
                              ipmi_interpret_power_unit_redundancy_config,
                              ipmi_interpret_power_unit_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_memory_config,
                              ipmi_interpret_memory_config,
                              ipmi_interpret_memory_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_drive_slot_config,
                              ipmi_interpret_drive_slot_config,
                              ipmi_interpret_drive_slot_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_drive_slot_state_config,
                              ipmi_interpret_drive_slot_state_config,
                              ipmi_interpret_drive_slot_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_drive_slot_predictive_failure_config,
                              ipmi_interpret_drive_slot_predictive_failure_config,
                              ipmi_interpret_drive_slot_predictive_failure_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_drive_slot_device_present_config,
                              ipmi_interpret_drive_slot_device_present_config,
                              ipmi_interpret_drive_slot_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_system_firmware_progress_config,
                              ipmi_interpret_system_firmware_progress_config,
                              ipmi_interpret_system_firmware_progress_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_event_logging_disabled_config,
                              ipmi_interpret_event_logging_disabled_config,
                              ipmi_interpret_event_logging_disabled_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_system_event_config,
                              ipmi_interpret_system_event_config,
                              ipmi_interpret_system_event_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_critical_interrupt_config,
                              ipmi_interpret_critical_interrupt_config,
                              ipmi_interpret_critical_interrupt_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_button_switch_config,
                              ipmi_interpret_button_switch_config,
                              ipmi_interpret_button_switch_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_button_switch_state_config,
                              ipmi_interpret_button_switch_state_config,
                              ipmi_interpret_button_switch_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_chip_set_config,
                              ipmi_interpret_chip_set_config,
                              ipmi_interpret_chip_set_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_module_board_state_config,
                              ipmi_interpret_module_board_state_config,
                              ipmi_interpret_module_board_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_module_board_device_present_config,
                              ipmi_interpret_module_board_device_present_config,
                              ipmi_interpret_module_board_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_cable_interconnect_config,
                              ipmi_interpret_cable_interconnect_config,
                              ipmi_interpret_cable_interconnect_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_system_boot_initiated_config,
                              ipmi_interpret_system_boot_initiated_config,
                              ipmi_interpret_system_boot_initiated_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_boot_error_config,
                              ipmi_interpret_boot_error_config,
                              ipmi_interpret_boot_error_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_os_boot_config,
                              ipmi_interpret_os_boot_config,
                              ipmi_interpret_os_boot_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_os_critical_stop_config,
                              ipmi_interpret_os_critical_stop_config,
                              ipmi_interpret_os_critical_stop_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_slot_connector_config,
                              ipmi_interpret_slot_connector_config,
                              ipmi_interpret_slot_connector_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_system_acpi_power_state_config,
                              ipmi_interpret_system_acpi_power_state_config,
                              ipmi_interpret_system_acpi_power_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_watchdog2_config,
                              ipmi_interpret_watchdog2_config,
                              ipmi_interpret_watchdog2_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_platform_alert_config,
                              ipmi_interpret_platform_alert_config,
                              ipmi_interpret_platform_alert_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_entity_presence_config,
                              ipmi_interpret_entity_presence_config,
                              ipmi_interpret_entity_presence_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_entity_presence_device_present_config,
                              ipmi_interpret_entity_presence_device_present_config,
                              ipmi_interpret_entity_presence_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_lan_config,
                              ipmi_interpret_lan_config,
                              ipmi_interpret_lan_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_management_subsystem_health_config,
                              ipmi_interpret_management_subsystem_health_config,
                              ipmi_interpret_management_subsystem_health_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_battery_config,
                              ipmi_interpret_battery_config,
                              ipmi_interpret_battery_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_session_audit_config,
                              ipmi_interpret_session_audit_config,
                              ipmi_interpret_session_audit_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_version_change_config,
                              ipmi_interpret_version_change_config,
                              ipmi_interpret_version_change_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sel.ipmi_interpret_fru_state_config,
                              ipmi_interpret_fru_state_config,
                              ipmi_interpret_fru_state_config_len) < 0)
    goto cleanup;

  if (!(ctx->interpret_sel.sel_oem_sensor_config = hash_create (IPMI_INTERPRET_SEL_HASH_SIZE,
                                                                (hash_key_f)hash_key_string,
                                                                (hash_cmp_f)strcmp,
                                                                (hash_del_f)free)))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  if (!(ctx->interpret_sel.sel_oem_record_config = hash_create (IPMI_INTERPRET_SEL_HASH_SIZE,
                                                                (hash_key_f)hash_key_string,
                                                                (hash_cmp_f)strcmp,
                                                                (hash_del_f)free)))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  /* No OEM SEL Records to Initialize right now */

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
                                     hash_t oem_config,
				     struct ipmi_interpret_sensor_oem_config **oem_conf)
{
  struct ipmi_interpret_sensor_oem_config *tmp_oem_conf = NULL;
  char keybuf[IPMI_OEM_HASH_KEY_BUFLEN + 1];
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (oem_config);
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
  
  if (!hash_insert (oem_config, tmp_oem_conf->key, tmp_oem_conf))
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
_interpret_sensor_oem_config_init (ipmi_interpret_ctx_t ctx, hash_t oem_config)
{
  struct ipmi_interpret_sensor_oem_config *oem_conf;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (oem_config);

  /* Dell Poweredge R610/R710 Power Optimized
   *
   * Manufacturer ID = 674
   * Product ID = 256
   * Event/Reading Type Code = 6Fh (Sensor Specific)
   * Sensor Type = C0h
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
					   IPMI_DELL_PRODUCT_ID_POWEREDGE,
					   IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC,
					   IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS,
                                           oem_config,
					   &oem_conf) < 0)
    return (-1);

  oem_conf->oem_state[0].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_GOOD);
  oem_conf->oem_state[0].sensor_state = IPMI_INTERPRET_STATE_NOMINAL;
  oem_conf->oem_state[0].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[1].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_OTHER);
  oem_conf->oem_state[1].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[1].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[2].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_THERMAL_PROTECTION);
  oem_conf->oem_state[2].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[2].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[3].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_COOLING_CAPACITY_CHANGE);
  oem_conf->oem_state[3].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[3].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[4].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_POWER_CAPACITY_CHANGE);
  oem_conf->oem_state[4].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[4].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[5].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_USER_DEFINED_POWER_CAPACITY);
  oem_conf->oem_state[5].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[5].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[6].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_HALTED_SYSTEM_POWER_EXCEEDS_CAPACITY);
  oem_conf->oem_state[6].sensor_state = IPMI_INTERPRET_STATE_CRITICAL;
  oem_conf->oem_state[6].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state[7].sensor_event_bitmask = (0x1 << IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS_DEGRADED_SYSTEM_POWER_EXCEEDS_CAPACITY);
  oem_conf->oem_state[7].sensor_state = IPMI_INTERPRET_STATE_WARNING;
  oem_conf->oem_state[7].oem_state_type = IPMI_OEM_STATE_TYPE_BITMASK;

  oem_conf->oem_state_count = 8;

  /* Supermicro X8DTH CPU Temperature Sensor
   *
   * Manufacturer ID = 47488 (not IANA number, special case)
   * Product ID = 43707
   * Event/Reading Type Code = 70h
   * Sensor Type = C0h
   * Value 0x0000 = "Low"
   * Value 0x0001 = "Medium"
   * Value 0x0002 = "High"
   * Value 0x0004 = "Overheat"
   * Value 0x0007 = "Not Installed"
   *
   * IPMI_OEM_Value 47488 43707 0x70 0xC0 0x0000 Nominal
   * IPMI_OEM_Value 47488 43707 0x70 0xC0 0x0001 Warning
   * IPMI_OEM_Value 47488 43707 0x70 0xC0 0x0002 Warning
   * IPMI_OEM_Value 47488 43707 0x70 0xC0 0x0004 Critical
   * IPMI_OEM_Value 47488 43707 0x70 0xC0 0x0007 Warning
   */

  if (_interpret_sensor_oem_config_create (ctx,
					   IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND,
					   IPMI_SUPERMICRO_PRODUCT_ID_X8DTH,
					   IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC,
					   IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP,
                                           oem_config,
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

int
ipmi_interpret_sensor_init (ipmi_interpret_ctx_t ctx)
{
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_threshold_sensor_config,
                              ipmi_interpret_threshold_sensor_config,
                              ipmi_interpret_threshold_sensor_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_voltage_state_config,
                              ipmi_interpret_voltage_state_config,
                              ipmi_interpret_voltage_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_voltage_performance_config,
                              ipmi_interpret_voltage_performance_config,
                              ipmi_interpret_voltage_performance_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_fan_device_present_config,
                              ipmi_interpret_fan_device_present_config,
                              ipmi_interpret_fan_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_fan_transition_availability_config,
                              ipmi_interpret_fan_transition_availability_config,
                              ipmi_interpret_fan_transition_availability_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_fan_redundancy_config,
                              ipmi_interpret_fan_redundancy_config,
                              ipmi_interpret_fan_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_physical_security_config,
                              ipmi_interpret_physical_security_config,
                              ipmi_interpret_physical_security_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_platform_security_violation_attempt_config,
                              ipmi_interpret_platform_security_violation_attempt_config,
                              ipmi_interpret_platform_security_violation_attempt_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_processor_config,
                              ipmi_interpret_processor_config,
                              ipmi_interpret_processor_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_processor_state_config,
                              ipmi_interpret_processor_state_config,
                              ipmi_interpret_processor_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_power_supply_config,
                              ipmi_interpret_power_supply_config,
                              ipmi_interpret_power_supply_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_power_supply_state_config,
                              ipmi_interpret_power_supply_state_config,
                              ipmi_interpret_power_supply_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_power_supply_redundancy_config,
                              ipmi_interpret_power_supply_redundancy_config,
                              ipmi_interpret_power_supply_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_power_unit_config,
                              ipmi_interpret_power_unit_config,
                              ipmi_interpret_power_unit_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_power_unit_device_present_config,
                              ipmi_interpret_power_unit_device_present_config,
                              ipmi_interpret_power_unit_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_power_unit_redundancy_config,
                              ipmi_interpret_power_unit_redundancy_config,
                              ipmi_interpret_power_unit_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_memory_config,
                              ipmi_interpret_memory_config,
                              ipmi_interpret_memory_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_drive_slot_config,
                              ipmi_interpret_drive_slot_config,
                              ipmi_interpret_drive_slot_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_drive_slot_state_config,
                              ipmi_interpret_drive_slot_state_config,
                              ipmi_interpret_drive_slot_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_drive_slot_predictive_failure_config,
                              ipmi_interpret_drive_slot_predictive_failure_config,
                              ipmi_interpret_drive_slot_predictive_failure_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_drive_slot_device_present_config,
                              ipmi_interpret_drive_slot_device_present_config,
                              ipmi_interpret_drive_slot_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_system_firmware_progress_config,
                              ipmi_interpret_system_firmware_progress_config,
                              ipmi_interpret_system_firmware_progress_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_event_logging_disabled_config,
                              ipmi_interpret_event_logging_disabled_config,
                              ipmi_interpret_event_logging_disabled_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_system_event_config,
                              ipmi_interpret_system_event_config,
                              ipmi_interpret_system_event_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_critical_interrupt_config,
                              ipmi_interpret_critical_interrupt_config,
                              ipmi_interpret_critical_interrupt_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_button_switch_config,
                              ipmi_interpret_button_switch_config,
                              ipmi_interpret_button_switch_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_button_switch_state_config,
                              ipmi_interpret_button_switch_state_config,
                              ipmi_interpret_button_switch_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_module_board_state_config,
                              ipmi_interpret_module_board_state_config,
                              ipmi_interpret_module_board_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_module_board_device_present_config,
                              ipmi_interpret_module_board_device_present_config,
                              ipmi_interpret_module_board_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_cable_interconnect_config,
                              ipmi_interpret_cable_interconnect_config,
                              ipmi_interpret_cable_interconnect_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_boot_error_config,
                              ipmi_interpret_boot_error_config,
                              ipmi_interpret_boot_error_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_slot_connector_config,
                              ipmi_interpret_slot_connector_config,
                              ipmi_interpret_slot_connector_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_system_acpi_power_state_config,
                              ipmi_interpret_system_acpi_power_state_config,
                              ipmi_interpret_system_acpi_power_state_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_watchdog2_config,
                              ipmi_interpret_watchdog2_config,
                              ipmi_interpret_watchdog2_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_entity_presence_config,
                              ipmi_interpret_entity_presence_config,
                              ipmi_interpret_entity_presence_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_entity_presence_device_present_config,
                              ipmi_interpret_entity_presence_device_present_config,
                              ipmi_interpret_entity_presence_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_management_subsystem_health_config,
                              ipmi_interpret_management_subsystem_health_config,
                              ipmi_interpret_management_subsystem_health_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_battery_config,
                              ipmi_interpret_battery_config,
                              ipmi_interpret_battery_config_len) < 0)
    goto cleanup;

  if (_interpret_config_init (ctx,
                              &ctx->interpret_sensor.ipmi_interpret_fru_state_config,
                              ipmi_interpret_fru_state_config,
                              ipmi_interpret_fru_state_config_len) < 0)
    goto cleanup;

  if (!(ctx->interpret_sensor.sensor_oem_config = hash_create (IPMI_INTERPRET_SENSOR_HASH_SIZE,
                                                               (hash_key_f)hash_key_string,
                                                               (hash_cmp_f)strcmp,
                                                               (hash_del_f)free)))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  if (_interpret_sensor_oem_config_init (ctx, ctx->interpret_sensor.sensor_oem_config) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static void
_interpret_config_destroy (ipmi_interpret_ctx_t ctx,
                           struct ipmi_interpret_config **config)
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
ipmi_interpret_sel_destroy (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_threshold_sel_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_voltage_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_voltage_performance_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_fan_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_fan_transition_availability_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_fan_redundancy_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_physical_security_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_platform_security_violation_attempt_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_processor_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_processor_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_power_supply_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_power_supply_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_power_supply_redundancy_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_power_unit_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_power_unit_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_power_unit_redundancy_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_memory_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_drive_slot_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_drive_slot_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_drive_slot_predictive_failure_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_drive_slot_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_system_firmware_progress_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_event_logging_disabled_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_system_event_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_critical_interrupt_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_button_switch_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_button_switch_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_module_board_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_module_board_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_cable_interconnect_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_system_boot_initiated_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_boot_error_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_os_boot_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_os_critical_stop_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_slot_connector_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_system_acpi_power_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_watchdog2_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_platform_alert_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_entity_presence_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_entity_presence_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_lan_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_management_subsystem_health_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_battery_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_session_audit_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_version_change_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sel.ipmi_interpret_fru_state_config);

  if (ctx->interpret_sel.sel_oem_sensor_config)
    hash_destroy (ctx->interpret_sel.sel_oem_sensor_config);

  if (ctx->interpret_sel.sel_oem_record_config)
    hash_destroy (ctx->interpret_sel.sel_oem_record_config);
}

void
ipmi_interpret_sensor_destroy (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_threshold_sensor_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_voltage_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_voltage_performance_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_fan_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_fan_transition_availability_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_fan_redundancy_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_physical_security_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_platform_security_violation_attempt_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_processor_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_processor_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_power_supply_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_power_supply_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_power_supply_redundancy_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_power_unit_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_power_unit_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_power_unit_redundancy_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_memory_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_drive_slot_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_drive_slot_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_drive_slot_predictive_failure_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_drive_slot_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_system_firmware_progress_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_event_logging_disabled_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_system_event_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_critical_interrupt_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_button_switch_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_button_switch_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_module_board_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_module_board_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_cable_interconnect_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_boot_error_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_slot_connector_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_system_acpi_power_state_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_watchdog2_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_entity_presence_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_entity_presence_device_present_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_management_subsystem_health_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_battery_config);

  _interpret_config_destroy (ctx,
                             ctx->interpret_sensor.ipmi_interpret_fru_state_config);

  if (ctx->interpret_sensor.sensor_oem_config)
    hash_destroy (ctx->interpret_sensor.sensor_oem_config);
}

static int
_state (conffile_t cf,
        char *option_string)
{
  assert (cf);
  assert (option_string);

  if (!strcasecmp (option_string, "Nominal"))
    return (IPMI_INTERPRET_STATE_NOMINAL);
  else if (!strcasecmp (option_string, "Warning"))
    return (IPMI_INTERPRET_STATE_WARNING);
  else if (!strcasecmp (option_string, "Critical"))
    return (IPMI_INTERPRET_STATE_CRITICAL);

  conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
  return (-1);
}

static int
_cb_state_parse (conffile_t cf,
                 struct conffile_data *data,
                 char *optionname,
                 int option_type,
                 void *option_ptr,
                 int option_data,
                 void *app_ptr,
                 int app_data)
{
  struct ipmi_interpret_config **config;
  int state;
  int i;

  assert (cf);
  assert (data);
  assert (option_type == CONFFILE_OPTION_STRING);
  assert (optionname);
  assert (option_ptr);

  if ((state = _state (cf, data->string)) < 0)
    return (-1);

  i = 0;
  config = (struct ipmi_interpret_config **)option_ptr;
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
_strtoul (conffile_t cf,
	  const char *str,
	  uint32_t max,
	  uint32_t *value)
{
  char *ptr = NULL;

  assert (str);
  assert (value);

  errno = 0;

  (*value) = strtoul (str, &ptr, 0);

  if (errno
      || (*ptr) != '\0'
      || (*value) > max)
    {
      conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
      return (-1);
    }
  
  return (0);
}

static int
_cb_sel_oem_sensor_parse (conffile_t cf,
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
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  unsigned int event_direction_any_flag = 0;
  uint8_t event_direction = 0;
  unsigned int event_data1_any_flag = 0;
  uint8_t event_data1 = 0;
  unsigned int event_data2_any_flag = 0;
  uint8_t event_data2 = 0;
  unsigned int event_data3_any_flag = 0;
  uint8_t event_data3 = 0;
  int sel_state;
  uint32_t tmp;
  struct ipmi_interpret_sel_oem_sensor_config *oem_conf;
  int found = 0;
  unsigned int i;

  assert (cf);
  assert (data);
  assert (optionname);
  assert (option_ptr);

  h = (hash_t *)option_ptr;

  memset (keybuf, '\0', IPMI_OEM_HASH_KEY_BUFLEN + 1);

  if (data->stringlist_len != 9)
    conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_MISSING);
  
  if (_strtoul (cf,
		data->stringlist[0],
		0x00FFFFFF,	/* 24 bit manufacturer ID */
		&tmp) < 0)
    return (-1);
  manufacturer_id = tmp;

  if (_strtoul (cf,
		data->stringlist[1],
		USHRT_MAX,
		&tmp) < 0)
    return (-1);
  product_id = tmp;

  if (_strtoul (cf,
		data->stringlist[2],
		UCHAR_MAX,
		&tmp) < 0)
    return (-1);
  event_reading_type_code = tmp;

  if (_strtoul (cf,
		data->stringlist[3],
		UCHAR_MAX,
		&tmp) < 0)
    return (-1);
  sensor_type = tmp;
  
  if (!strcasecmp (data->stringlist[4], IPMI_SEL_OEM_DATA_HEX_BYTE_ANY))
    event_direction_any_flag = 1;
  else if (!strcasecmp (data->stringlist[4], "assertion"))
    event_direction = IPMI_SEL_RECORD_ASSERTION_EVENT;
  else if (!strcasecmp (data->stringlist[4], "deassertion"))
    event_direction = IPMI_SEL_RECORD_DEASSERTION_EVENT;
  else
    {
      conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
      return (-1);
    }

  if (!strcasecmp (data->stringlist[5], IPMI_SEL_OEM_DATA_HEX_BYTE_ANY))
    event_data1_any_flag = 1;
  else
    {
      if (_strtoul (cf,
                    data->stringlist[5],
                    UCHAR_MAX,
                    &tmp) < 0)
        return (-1);

      event_data1 = tmp;
    }

  if (!strcasecmp (data->stringlist[6], IPMI_SEL_OEM_DATA_HEX_BYTE_ANY))
    event_data2_any_flag = 1;
  else
    {
      if (_strtoul (cf,
                    data->stringlist[6],
                    UCHAR_MAX,
                    &tmp) < 0)
        return (-1);

      event_data2 = tmp;
    }

  if (!strcasecmp (data->stringlist[7], IPMI_SEL_OEM_DATA_HEX_BYTE_ANY))
    event_data3_any_flag = 1;
  else
    {
      if (_strtoul (cf,
                    data->stringlist[7],
                    UCHAR_MAX,
                    &tmp) < 0)
        return (-1);

      event_data3 = tmp;
    }

  if ((sel_state = _state (cf, data->stringlist[8])) < 0)
    return (-1);
  
  snprintf (keybuf,
	    IPMI_OEM_HASH_KEY_BUFLEN,
	    "%u:%u:%u:%u",
	    manufacturer_id,
	    product_id,
	    event_reading_type_code,
	    sensor_type);

  if (!(oem_conf = hash_find ((*h), keybuf)))
    {
      if (!(oem_conf = (struct ipmi_interpret_sel_oem_sensor_config *)malloc (sizeof (struct ipmi_interpret_sel_oem_sensor_config))))
	{
	  conffile_seterrnum (cf, CONFFILE_ERR_OUTMEM);
	  return (-1);
	}
      memset (oem_conf, '\0', sizeof (struct ipmi_interpret_sel_oem_sensor_config));
      
      memcpy (oem_conf->key, keybuf, IPMI_OEM_HASH_KEY_BUFLEN);
      oem_conf->manufacturer_id = manufacturer_id;
      oem_conf->product_id = product_id;
      oem_conf->event_reading_type_code = event_reading_type_code;
      oem_conf->sensor_type = sensor_type;

      if (!hash_insert ((*h), oem_conf->key, oem_conf))
	{
	  conffile_seterrnum (cf, CONFFILE_ERR_INTERNAL);
	  free (oem_conf);
	  return (-1);
	}
    }

  if (oem_conf->oem_sensor_data_count >= IPMI_SEL_OEM_SENSOR_MAX)
    {
      conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_TOOMANY);
      return (-1);
    }

  /* check for duplicates */
  for (i = 0; i < oem_conf->oem_sensor_data_count; i++)
    {
      if (oem_conf->oem_sensor_data[i].event_direction_any_flag == event_direction_any_flag
          && oem_conf->oem_sensor_data[i].event_direction == event_direction
          && oem_conf->oem_sensor_data[i].event_data1_any_flag == event_data1_any_flag
          && oem_conf->oem_sensor_data[i].event_data1 == event_data1
          && oem_conf->oem_sensor_data[i].event_data2_any_flag == event_data2_any_flag
          && oem_conf->oem_sensor_data[i].event_data2 == event_data2
          && oem_conf->oem_sensor_data[i].event_data3_any_flag == event_data3_any_flag
          && oem_conf->oem_sensor_data[i].event_data3 == event_data3)
	{
	  oem_conf->oem_sensor_data[i].sel_state = sel_state;
	  found++;
	  break;
	}
    }

  if (!found)
    {
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_direction_any_flag = event_direction_any_flag;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_direction = event_direction;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_data1_any_flag = event_data1_any_flag;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_data1 = event_data1;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_data2_any_flag = event_data2_any_flag;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_data2 = event_data2;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_data3_any_flag = event_data3_any_flag;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].event_data3 = event_data3;
      oem_conf->oem_sensor_data[oem_conf->oem_sensor_data_count].sel_state = sel_state;
      oem_conf->oem_sensor_data_count++;
    }

  return (0);
}

static int
_cb_sel_oem_record_parse (conffile_t cf,
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
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t record_type;
  int sel_state;
  uint32_t tmp;
  struct ipmi_interpret_sel_oem_record_config *oem_conf;
  struct ipmi_interpret_sel_oem_data_byte oem_bytes[IPMI_SEL_OEM_DATA_MAX];
  unsigned int oem_data_count = 0;
  int found = 0;
  unsigned int i;

  assert (cf);
  assert (data);
  assert (optionname);
  assert (option_ptr);

  h = (hash_t *)option_ptr;

  memset (keybuf, '\0', IPMI_OEM_HASH_KEY_BUFLEN + 1);
  memset (oem_bytes, '\0', sizeof (struct ipmi_interpret_sel_oem_data_byte) * IPMI_SEL_OEM_DATA_MAX);

  if (!strcasecmp (optionname, "IPMI_OEM_Timestamped_Record"))
    {
      if (data->stringlist_len != 9)
        conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_MISSING);
      oem_data_count = IPMI_SEL_OEM_DATA_TIMESTAMPED_BYTES;
    }
  else
    {
      if (data->stringlist_len != 17)
        conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_MISSING);
      oem_data_count = IPMI_SEL_OEM_DATA_NON_TIMESTAMPED_BYTES;
    }
  
  if (_strtoul (cf,
		data->stringlist[0],
		0x00FFFFFF,	/* 24 bit manufacturer ID */
		&tmp) < 0)
    return (-1);
  manufacturer_id = tmp;

  if (_strtoul (cf,
		data->stringlist[1],
		USHRT_MAX,
		&tmp) < 0)
    return (-1);
  product_id = tmp;

  if (_strtoul (cf,
		data->stringlist[2],
		UCHAR_MAX,
		&tmp) < 0)
    return (-1);
  record_type = tmp;

  if (oem_data_count == IPMI_SEL_OEM_DATA_TIMESTAMPED_BYTES)
    {
      if (!IPMI_SEL_RECORD_TYPE_IS_TIMESTAMPED_OEM (record_type))
        {
          conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
          return (-1);
        }
    }
  else
    {
      if (!IPMI_SEL_RECORD_TYPE_IS_NON_TIMESTAMPED_OEM (record_type))
        {
          conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
          return (-1);
        }
    }

  for (i = 0; i < oem_data_count; i++)
    {
      if (!strcasecmp (data->stringlist[3 + i], IPMI_SEL_OEM_DATA_HEX_BYTE_ANY))
        {
          oem_bytes[i].any_flag = 1;
          oem_bytes[i].oem_data_byte = 0;
        }
      else
        {
          if (_strtoul (cf,
                        data->stringlist[3 + i],
                        UCHAR_MAX,
                        &tmp) < 0)
            return (-1);

          oem_bytes[i].any_flag = 0;
          oem_bytes[i].oem_data_byte = tmp;
        }
    }

  if ((sel_state = _state (cf, data->stringlist[3 + oem_data_count])) < 0)
    return (-1);
  
  snprintf (keybuf,
	    IPMI_OEM_HASH_KEY_BUFLEN,
	    "%u:%u:%u",
	    manufacturer_id,
	    product_id,
            record_type);

  if (!(oem_conf = hash_find ((*h), keybuf)))
    {
      if (!(oem_conf = (struct ipmi_interpret_sel_oem_record_config *)malloc (sizeof (struct ipmi_interpret_sel_oem_record_config))))
	{
	  conffile_seterrnum (cf, CONFFILE_ERR_OUTMEM);
	  return (-1);
	}
      memset (oem_conf, '\0', sizeof (struct ipmi_interpret_sel_oem_record_config));

      memcpy (oem_conf->key, keybuf, IPMI_OEM_HASH_KEY_BUFLEN);
      oem_conf->manufacturer_id = manufacturer_id;
      oem_conf->product_id = product_id;
      oem_conf->record_type = record_type;

      if (!hash_insert ((*h), oem_conf->key, oem_conf))
	{
	  conffile_seterrnum (cf, CONFFILE_ERR_INTERNAL);
	  free (oem_conf);
	  return (-1);
	}
    }

  if (oem_conf->oem_record_count >= IPMI_SEL_OEM_RECORD_MAX)
    {
      conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_TOOMANY);
      return (-1);
    }

  /* check for duplicates */
  for (i = 0; i < oem_conf->oem_record_count; i++)
    {
      if (!memcmp (oem_bytes, oem_conf->oem_record[i].oem_bytes, sizeof (struct ipmi_interpret_sel_oem_data_byte) * IPMI_SEL_OEM_DATA_MAX))
        {
          oem_conf->oem_record[i].sel_state = sel_state;
          found++;
          break;
        }
    }

  if (!found)
    {
      memcpy (oem_conf->oem_record[oem_conf->oem_record_count].oem_bytes,
              oem_bytes,
              sizeof (struct ipmi_interpret_sel_oem_data_byte) * IPMI_SEL_OEM_DATA_MAX);
      oem_conf->oem_record[oem_conf->oem_record_count].oem_bytes_count = oem_data_count;
      oem_conf->oem_record[oem_conf->oem_record_count].sel_state = sel_state;
      oem_conf->oem_record_count++;
    }

  return (0);
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
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  uint16_t sensor_event_bitmask;
  int sensor_state;
  int oem_state_type;
  uint32_t tmp;
  struct ipmi_interpret_sensor_oem_config *oem_conf;
  int found = 0;
  unsigned int i;

  assert (cf);
  assert (data);
  assert (optionname);
  assert (option_ptr);

  h = (hash_t *)option_ptr;

  memset (keybuf, '\0', IPMI_OEM_HASH_KEY_BUFLEN + 1);

  if (data->stringlist_len != 6)
    conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_MISSING);
  
  if (_strtoul (cf,
		data->stringlist[0],
		0x00FFFFFF,	/* 24 bit manufacturer ID */
		&tmp) < 0)
    return (-1);
  manufacturer_id = tmp;

  if (_strtoul (cf,
		data->stringlist[1],
		USHRT_MAX,
		&tmp) < 0)
    return (-1);
  product_id = tmp;

  if (_strtoul (cf,
		data->stringlist[2],
		UCHAR_MAX,
		&tmp) < 0)
    return (-1);
  event_reading_type_code = tmp;

  if (_strtoul (cf,
		data->stringlist[3],
		UCHAR_MAX,
		&tmp) < 0)
    return (-1);
  sensor_type = tmp;
  
  /* achu: sensor event bitmask bit 16 not legal, but we will allow it
   * b/c perhaps some OEM sensors will breat legality of events, or
   * perhaps there is a bug and some vendors need to have the 16th bit
   * matched.
   */
  if (_strtoul (cf,
                data->stringlist[4],
                USHRT_MAX,
                &tmp) < 0)
    return (-1);
  sensor_event_bitmask = tmp;

  if ((sensor_state = _state (cf, data->stringlist[5])) < 0)
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
  
  snprintf (keybuf,
	    IPMI_OEM_HASH_KEY_BUFLEN,
	    "%u:%u:%u:%u",
	    manufacturer_id,
	    product_id,
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
      oem_conf->manufacturer_id = manufacturer_id;
      oem_conf->product_id = product_id;
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
  for (i = 0; i < oem_conf->oem_state_count; i++)
    {
      if (oem_conf->oem_state[i].oem_state_type == oem_state_type
	  && oem_conf->oem_state[i].sensor_event_bitmask == sensor_event_bitmask)
	{
	  oem_conf->oem_state[i].sensor_state = sensor_state;
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

  return (0);
}

static void
_copy_options_and_fill_option_ptr (struct conffile_option *to_options,
                                   unsigned int to_options_len,
                                   struct conffile_option *from_options,
                                   unsigned int from_options_len,
                                   void *option_ptr)
{
  unsigned int i;

  assert (to_options
          && from_options
          && from_options_len
          && option_ptr);

  /* note: can't memcpy .. sigh .. wish I did this in C++ w/ a copy constructor */
  for (i = 0; i < from_options_len; i++)
    {
      to_options[to_options_len + i].optionname = from_options[i].optionname;
      to_options[to_options_len + i].option_type = from_options[i].option_type;
      to_options[to_options_len + i].option_type_arg = from_options[i].option_type_arg;
      to_options[to_options_len + i].callback_func = from_options[i].callback_func;
      to_options[to_options_len + i].max_count = from_options[i].max_count;
      to_options[to_options_len + i].required_count = from_options[i].required_count;
      to_options[to_options_len + i].count_ptr = from_options[i].count_ptr;
      to_options[to_options_len + i].option_ptr = option_ptr;
      to_options[to_options_len + i].option_data = from_options[i].option_data;
    }
}


static int
_ipmi_interpret_config_parse (ipmi_interpret_ctx_t ctx,
                              const char *config_file,
                              unsigned int sel_config)
{
  struct conffile_option config_file_options[IPMI_INTERPRET_CONFIG_FILE_OPTIONS_MAX];
  unsigned int config_file_options_len = 0;

  int threshold_sel_flag0, threshold_sel_flag1, threshold_sel_flag2,
    threshold_sel_flag3, threshold_sel_flag4, threshold_sel_flag5, threshold_sel_flag6,
    threshold_sel_flag7, threshold_sel_flag8, threshold_sel_flag9, threshold_sel_flag10,
    threshold_sel_flag11;
  int threshold_sensor_flag0, threshold_sensor_flag1, threshold_sensor_flag2,
    threshold_sensor_flag3, threshold_sensor_flag4, threshold_sensor_flag5;
  int voltage_state_flag0, voltage_state_flag1;
  int voltage_performance_flag0, voltage_performance_flag1;
  int fan_device_present_flag0, fan_device_present_flag1;
  int fan_transition_availability_flag0, fan_transition_availability_flag1,
    fan_transition_availability_flag2, fan_transition_availability_flag3,
    fan_transition_availability_flag4, fan_transition_availability_flag5,
    fan_transition_availability_flag6, fan_transition_availability_flag7,
    fan_transition_availability_flag8;
  int fan_redundancy_flag0, fan_redundancy_flag1, fan_redundancy_flag2,
    fan_redundancy_flag3, fan_redundancy_flag4, fan_redundancy_flag5,
    fan_redundancy_flag6, fan_redundancy_flag7;
  int physical_security_flag0, physical_security_flag1, physical_security_flag2,
    physical_security_flag3, physical_security_flag4, physical_security_flag5,
    physical_security_flag6;
  int platform_security_violation_attempt_flag0, platform_security_violation_attempt_flag1,
    platform_security_violation_attempt_flag2, platform_security_violation_attempt_flag3,
    platform_security_violation_attempt_flag4, platform_security_violation_attempt_flag5;
  int processor_flag0, processor_flag1, processor_flag2, processor_flag3,
    processor_flag4, processor_flag5, processor_flag6, processor_flag7,
    processor_flag8, processor_flag9, processor_flag10, processor_flag11;
  int processor_state_flag0, processor_state_flag1;
  int power_supply_flag0, power_supply_flag1, power_supply_flag2, power_supply_flag3,
    power_supply_flag4, power_supply_flag5, power_supply_flag6;
  int power_supply_state_flag0, power_supply_state_flag1;
  int power_supply_redundancy_flag0, power_supply_redundancy_flag1, power_supply_redundancy_flag2,
    power_supply_redundancy_flag3, power_supply_redundancy_flag4, power_supply_redundancy_flag5,
    power_supply_redundancy_flag6, power_supply_redundancy_flag7;
  int power_unit_flag0, power_unit_flag1, power_unit_flag2, power_unit_flag3,
    power_unit_flag4, power_unit_flag5, power_unit_flag6, power_unit_flag7;
  int power_unit_device_present_flag0, power_unit_device_present_flag1;
  int power_unit_redundancy_flag0, power_unit_redundancy_flag1, power_unit_redundancy_flag2,
    power_unit_redundancy_flag3, power_unit_redundancy_flag4, power_unit_redundancy_flag5,
    power_unit_redundancy_flag6, power_unit_redundancy_flag7;
  int memory_flag0, memory_flag1, memory_flag2, memory_flag3, memory_flag4,
    memory_flag5, memory_flag6, memory_flag7, memory_flag8, memory_flag9,
    memory_flag10;
  int drive_slot_flag0, drive_slot_flag1, drive_slot_flag2, drive_slot_flag3,
    drive_slot_flag4, drive_slot_flag5, drive_slot_flag6, drive_slot_flag7,
    drive_slot_flag8;
  int drive_slot_state_flag0, drive_slot_state_flag1;
  int drive_slot_predictive_failure_flag0, drive_slot_predictive_failure_flag1;
  int drive_slot_device_present_flag0, drive_slot_device_present_flag1;
  int system_firmware_progress_flag0, system_firmware_progress_flag1, system_firmware_progress_flag2;
  int event_logging_disabled_flag0, event_logging_disabled_flag1, event_logging_disabled_flag2,
    event_logging_disabled_flag3, event_logging_disabled_flag4, event_logging_disabled_flag5,
    event_logging_disabled_flag6;
  int system_event_flag0, system_event_flag1, system_event_flag2,
    system_event_flag3, system_event_flag4, system_event_flag5;
  int critical_interrupt_flag0, critical_interrupt_flag1, critical_interrupt_flag2,
    critical_interrupt_flag3, critical_interrupt_flag4, critical_interrupt_flag5,
    critical_interrupt_flag6, critical_interrupt_flag7, critical_interrupt_flag8,
    critical_interrupt_flag9, critical_interrupt_flag10, critical_interrupt_flag11;
  int button_switch_flag0, button_switch_flag1, button_switch_flag2,
    button_switch_flag3, button_switch_flag4;
  int button_switch_state_flag0, button_switch_state_flag1;
  int chip_set_flag0, chip_set_flag1;
  int module_board_state_flag0, module_board_state_flag1;
  int module_board_device_present_flag0, module_board_device_present_flag1;
  int cable_interconnect_flag0, cable_interconnect_flag1;
  int system_boot_initiated_flag0, system_boot_initiated_flag1, system_boot_initiated_flag2,
    system_boot_initiated_flag3, system_boot_initiated_flag4, system_boot_initiated_flag5;
  int boot_error_flag0, boot_error_flag1, boot_error_flag2,
    boot_error_flag3, boot_error_flag4;
  int os_boot_flag0, os_boot_flag1, os_boot_flag2, os_boot_flag3, os_boot_flag4,
    os_boot_flag5;
  int os_critical_stop_flag0, os_critical_stop_flag1, os_critical_stop_flag2, os_critical_stop_flag3,
    os_critical_stop_flag4, os_critical_stop_flag5;
  int slot_connector_flag0, slot_connector_flag1, slot_connector_flag2,
    slot_connector_flag3, slot_connector_flag4, slot_connector_flag5,
    slot_connector_flag6, slot_connector_flag7, slot_connector_flag8,
    slot_connector_flag9;
  int system_acpi_power_state_flag0, system_acpi_power_state_flag1,
    system_acpi_power_state_flag2, system_acpi_power_state_flag3,
    system_acpi_power_state_flag4, system_acpi_power_state_flag5,
    system_acpi_power_state_flag6, system_acpi_power_state_flag7,
    system_acpi_power_state_flag8, system_acpi_power_state_flag9,
    system_acpi_power_state_flag10, system_acpi_power_state_flag11,
    system_acpi_power_state_flag12, system_acpi_power_state_flag13,
    system_acpi_power_state_flag14;
  int watchdog2_flag0, watchdog2_flag1, watchdog2_flag2, watchdog2_flag3,
    watchdog2_flag4, watchdog2_flag5, watchdog2_flag6, watchdog2_flag7,
    watchdog2_flag8;
  int platform_alert_flag0, platform_alert_flag1, platform_alert_flag2, platform_alert_flag3;
  int entity_presence_flag0, entity_presence_flag1, entity_presence_flag2;
  int entity_presence_device_present_flag0, entity_presence_device_present_flag1;
  int lan_flag0, lan_flag1;
  int management_subsystem_health_flag0, management_subsystem_health_flag1,
    management_subsystem_health_flag2, management_subsystem_health_flag3,
    management_subsystem_health_flag4, management_subsystem_health_flag5;
  int battery_flag0, battery_flag1, battery_flag2;
  int session_audit_flag0, session_audit_flag1, session_audit_flag2, session_audit_flag3;
  int version_change_flag0, version_change_flag1, version_change_flag2, version_change_flag3,
    version_change_flag4, version_change_flag5, version_change_flag6, version_change_flag7;
  int fru_state_flag0, fru_state_flag1, fru_state_flag2, fru_state_flag3,
    fru_state_flag4, fru_state_flag5, fru_state_flag6, fru_state_flag7;
  int sel_oem_sensor_flag;
  int sel_oem_timestamped_flag;
  int sel_oem_non_timestamped_flag;
  int sensor_oem_bitmask_flag;
  int sensor_oem_value_flag;

  /*
   * IPMI_Threshold_Sel
   */
  struct conffile_option threshold_sel_options[] =
    {
      {
        ipmi_interpret_threshold_sel_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag8,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[9].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag9,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[10].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag10,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sel_config[11].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sel_flag11,
        NULL,
        0
      },
    };

  /*
   * IPMI_Threshold_Sensor
   */
  struct conffile_option threshold_sensor_options[] =
    {
      {
        ipmi_interpret_threshold_sensor_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sensor_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sensor_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sensor_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sensor_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sensor_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sensor_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sensor_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sensor_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sensor_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_threshold_sensor_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &threshold_sensor_flag5,
        NULL,
        0
      },
    };

  /*
   * IPMI_Voltage_State
   */
  struct conffile_option voltage_state_options[] =
    {
      {
        ipmi_interpret_voltage_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &voltage_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_voltage_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &voltage_state_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Voltage_Performance
   */
  struct conffile_option voltage_performance_options[] =
    {
      {
        ipmi_interpret_voltage_performance_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &voltage_performance_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_voltage_performance_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &voltage_performance_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Fan_Device_Present
   */
  struct conffile_option fan_device_present_options[] =
    {
      {
        ipmi_interpret_fan_device_present_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_device_present_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_device_present_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_device_present_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Fan_Transition_Availability
   */
  struct conffile_option fan_transition_availability_options[] =
    {
      {
        ipmi_interpret_fan_transition_availability_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_transition_availability_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_transition_availability_flag8,
        NULL,
        0
      },
    };

  /*
   * IPMI_Fan_Redundancy
   */
  struct conffile_option fan_redundancy_options[] =
    {
      {
        ipmi_interpret_fan_redundancy_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_redundancy_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_redundancy_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_redundancy_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_redundancy_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_redundancy_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_redundancy_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_fan_redundancy_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fan_redundancy_flag7,
        NULL,
        0
      },
    };

  /*
   * IPMI_Physical_Security
   */
  struct conffile_option physical_security_options[] =
    {
      {
        ipmi_interpret_physical_security_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &physical_security_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_physical_security_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &physical_security_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_physical_security_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &physical_security_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_physical_security_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &physical_security_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_physical_security_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &physical_security_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_physical_security_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &physical_security_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_physical_security_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &physical_security_flag6,
        NULL,
        0
      },
    };

  /*
   * IPMI_Platform_Security_Violation_Attempt
   */
  struct conffile_option platform_security_violation_attempt_options[] =
    {
      {
        ipmi_interpret_platform_security_violation_attempt_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_security_violation_attempt_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_security_violation_attempt_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_security_violation_attempt_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_security_violation_attempt_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_security_violation_attempt_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_security_violation_attempt_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_security_violation_attempt_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_security_violation_attempt_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_security_violation_attempt_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_security_violation_attempt_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_security_violation_attempt_flag5,
        NULL,
        0
      },
    };

  /*
   * IPMI_Processor
   */
  struct conffile_option processor_options[] =
    {
      {
        ipmi_interpret_processor_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag8,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[9].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag9,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[10].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag10,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_config[11].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_flag11,
        NULL,
        0
      },
    };

  /*
   * IPMI_Processor_State
   */
  struct conffile_option processor_state_options[] =
    {
      {
        ipmi_interpret_processor_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_processor_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &processor_state_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Power_Supply
   */
  struct conffile_option power_supply_options[] =
    {
      {
        ipmi_interpret_power_supply_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_flag6,
        NULL,
        0
      },
    };

  /*
   * IPMI_Power_Supply_State
   */
  struct conffile_option power_supply_state_options[] =
    {
      {
        ipmi_interpret_power_supply_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_state_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Power_Supply_Redundancy
   */
  struct conffile_option power_supply_redundancy_options[] =
    {
      {
        ipmi_interpret_power_supply_redundancy_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_redundancy_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_redundancy_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_redundancy_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_redundancy_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_redundancy_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_redundancy_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_power_supply_redundancy_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_supply_redundancy_flag7,
        NULL,
        0
      },
    };

  /*
   * IPMI_Power_Unit
   */
  struct conffile_option power_unit_options[] =
    {
      {
        ipmi_interpret_power_unit_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_flag7,
        NULL,
        0
      },
    };

  /*
   * IPMI_Power_Unit_Device_Present
   */
  struct conffile_option power_unit_device_present_options[] =
    {
      {
        ipmi_interpret_power_unit_device_present_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_device_present_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_device_present_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_device_present_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Power_Unit_Redundancy
   */
  struct conffile_option power_unit_redundancy_options[] =
    {
      {
        ipmi_interpret_power_unit_redundancy_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_redundancy_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_redundancy_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_redundancy_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_redundancy_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_redundancy_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_redundancy_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_power_unit_redundancy_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &power_unit_redundancy_flag7,
        NULL,
        0
      },
    };

  /*
   * IPMI_Memory
   */
  struct conffile_option memory_options[] =
    {
      {
        ipmi_interpret_memory_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag8,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[9].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag9,
        NULL,
        0
      },
      {
        ipmi_interpret_memory_config[10].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &memory_flag10,
        NULL,
        0
      },
    };

  /*
   * IPMI_Drive_Slot
   */
  struct conffile_option drive_slot_options[] =
    {
      {
        ipmi_interpret_drive_slot_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_flag8,
        NULL,
        0
      },
    };

  /*
   * IPMI_Drive_Slot_State
   */
  struct conffile_option drive_slot_state_options[] =
    {
      {
        ipmi_interpret_drive_slot_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_state_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Drive_Slot_Predictive_Failure
   */
  struct conffile_option drive_slot_predictive_failure_options[] =
    {
      {
        ipmi_interpret_drive_slot_predictive_failure_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_predictive_failure_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_predictive_failure_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_predictive_failure_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Drive_Slot_Device_Present
   */
  struct conffile_option drive_slot_device_present_options[] =
    {
      {
        ipmi_interpret_drive_slot_device_present_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_device_present_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_drive_slot_device_present_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &drive_slot_device_present_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_System_Firmware_Progress
   */
  struct conffile_option system_firmware_progress_options[] =
    {
      {
        ipmi_interpret_system_firmware_progress_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_firmware_progress_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_system_firmware_progress_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_firmware_progress_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_system_firmware_progress_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_firmware_progress_flag2,
        NULL,
        0
      },
    };

  /*
   * IPMI_Event_Logging_Disabled
   */
  struct conffile_option event_logging_disabled_options[] =
    {
      {
        ipmi_interpret_event_logging_disabled_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &event_logging_disabled_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_event_logging_disabled_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &event_logging_disabled_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_event_logging_disabled_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &event_logging_disabled_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_event_logging_disabled_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &event_logging_disabled_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_event_logging_disabled_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &event_logging_disabled_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_event_logging_disabled_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &event_logging_disabled_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_event_logging_disabled_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &event_logging_disabled_flag6,
        NULL,
        0
      },
    };

  /*
   * IPMI_System_Event
   */
  struct conffile_option system_event_options[] =
    {
      {
        ipmi_interpret_system_event_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_event_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_system_event_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_event_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_system_event_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_event_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_system_event_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_event_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_system_event_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_event_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_system_event_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_event_flag5,
        NULL,
        0
      },
    };

  /*
   * IPMI_Critical_Interrupt
   */
  struct conffile_option critical_interrupt_options[] =
    {
      {
        ipmi_interpret_critical_interrupt_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag8,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[9].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag9,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[10].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag10,
        NULL,
        0
      },
      {
        ipmi_interpret_critical_interrupt_config[11].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &critical_interrupt_flag11,
        NULL,
        0
      },
    };

  /*
   * IPMI_Button_Switch
   */
  struct conffile_option button_switch_options[] =
    {
      {
        ipmi_interpret_button_switch_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &button_switch_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_button_switch_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &button_switch_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_button_switch_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &button_switch_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_button_switch_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &button_switch_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_button_switch_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &button_switch_flag4,
        NULL,
        0
      },
    };

  /*
   * IPMI_Button_Switch_State
   */
  struct conffile_option button_switch_state_options[] =
    {
      {
        ipmi_interpret_button_switch_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &button_switch_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_button_switch_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &button_switch_state_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Chip_Set
   */
  struct conffile_option chip_set_options[] =
    {
      {
        ipmi_interpret_chip_set_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &chip_set_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_chip_set_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &chip_set_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Module_Board_State
   */
  struct conffile_option module_board_state_options[] =
    {
      {
        ipmi_interpret_module_board_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &module_board_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_module_board_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &module_board_state_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Module_Board_Device_Present
   */
  struct conffile_option module_board_device_present_options[] =
    {
      {
        ipmi_interpret_module_board_device_present_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &module_board_device_present_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_module_board_device_present_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &module_board_device_present_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Cable_Interconnect
   */
  struct conffile_option cable_interconnect_options[] =
    {
      {
        ipmi_interpret_cable_interconnect_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &cable_interconnect_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_cable_interconnect_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &cable_interconnect_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_System_Boot_Initiated
   */
  struct conffile_option system_boot_initiated_options[] =
    {
      {
        ipmi_interpret_system_boot_initiated_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_boot_initiated_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_system_boot_initiated_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_boot_initiated_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_system_boot_initiated_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_boot_initiated_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_system_boot_initiated_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_boot_initiated_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_system_boot_initiated_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_boot_initiated_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_system_boot_initiated_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_boot_initiated_flag5,
        NULL,
        0
      },
    };  

  /*
   * IPMI_Boot_Error
   */
  struct conffile_option boot_error_options[] =
    {
      {
        ipmi_interpret_boot_error_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &boot_error_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_boot_error_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &boot_error_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_boot_error_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &boot_error_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_boot_error_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &boot_error_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_boot_error_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &boot_error_flag4,
        NULL,
        0
      },
    };

  /*
   * IPMI_OS_Boot
   */
  struct conffile_option os_boot_options[] =
    {
      {
        ipmi_interpret_os_boot_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_boot_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_os_boot_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_boot_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_os_boot_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_boot_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_os_boot_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_boot_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_os_boot_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_boot_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_os_boot_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_boot_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_os_boot_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_boot_flag5,
        NULL,
        0
      },
    };  

  /*
   * IPMI_OS_Critical_Stop
   */
  struct conffile_option os_critical_stop_options[] =
    {
      {
        ipmi_interpret_os_critical_stop_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_critical_stop_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_os_critical_stop_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_critical_stop_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_os_critical_stop_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_critical_stop_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_os_critical_stop_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_critical_stop_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_os_critical_stop_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_critical_stop_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_os_critical_stop_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &os_critical_stop_flag5,
        NULL,
        0
      },
    };  

  /*
   * IPMI_Slot_Connector
   */
  struct conffile_option slot_connector_options[] =
    {
      {
        ipmi_interpret_slot_connector_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag8,
        NULL,
        0
      },
      {
        ipmi_interpret_slot_connector_config[9].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &slot_connector_flag9,
        NULL,
        0
      },
    };

  /*
   * IPMI_System_Acpi_Power_State
   */
  struct conffile_option system_acpi_power_state_options[] =
    {
      {
        ipmi_interpret_system_acpi_power_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag8,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[9].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag9,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[10].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag10,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[11].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag11,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[12].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag12,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[13].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag13,
        NULL,
        0
      },
      {
        ipmi_interpret_system_acpi_power_state_config[14].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &system_acpi_power_state_flag14,
        NULL,
        0
      },
    };

  /*
   * IPMI_Watchdog2
   */
  struct conffile_option watchdog2_options[] =
    {
      {
        ipmi_interpret_watchdog2_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag7,
        NULL,
        0
      },
      {
        ipmi_interpret_watchdog2_config[8].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &watchdog2_flag8,
        NULL,
        0
      },
    };

  /*
   * IPMI_Platform_Alert
   */
  struct conffile_option platform_alert_options[] =
    {
      {
        ipmi_interpret_platform_alert_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_alert_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_alert_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_alert_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_alert_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_alert_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_platform_alert_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &platform_alert_flag3,
        NULL,
        0
      },
    };

  /*
   * IPMI_Entity_Presence
   */
  struct conffile_option entity_presence_options[] =
    {
      {
        ipmi_interpret_entity_presence_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &entity_presence_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_entity_presence_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &entity_presence_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_entity_presence_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &entity_presence_flag2,
        NULL,
        0
      },
    };

  /*
   * IPMI_Entity_Presence_Device_Present
   */
  struct conffile_option entity_presence_device_present_options[] =
    {
      {
        ipmi_interpret_entity_presence_device_present_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &entity_presence_device_present_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_entity_presence_device_present_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &entity_presence_device_present_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Lan
   */
  struct conffile_option lan_options[] =
    {
      {
        ipmi_interpret_lan_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &lan_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_lan_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &lan_flag1,
        NULL,
        0
      },
    };

  /*
   * IPMI_Management_Subsystem_Health
   */
  struct conffile_option management_subsystem_health_options[] =
    {
      {
        ipmi_interpret_management_subsystem_health_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &management_subsystem_health_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_management_subsystem_health_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &management_subsystem_health_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_management_subsystem_health_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &management_subsystem_health_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_management_subsystem_health_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &management_subsystem_health_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_management_subsystem_health_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &management_subsystem_health_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_management_subsystem_health_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &management_subsystem_health_flag5,
        NULL,
        0
      },
    };

  /*
   * IPMI_Battery
   */
  struct conffile_option battery_options[] =
    {
      {
        ipmi_interpret_battery_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &battery_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_battery_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &battery_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_battery_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &battery_flag2,
        NULL,
        0
      },
    };

  /*
   * IPMI_Session_Audit
   */
  struct conffile_option session_audit_options[] =
    {
      {
        ipmi_interpret_session_audit_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &session_audit_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_session_audit_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &session_audit_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_session_audit_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &session_audit_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_session_audit_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &session_audit_flag3,
        NULL,
        0
      },
    };

  /*
   * IPMI_Version_Change
   */
  struct conffile_option version_change_options[] =
    {
      {
        ipmi_interpret_version_change_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_version_change_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_version_change_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_version_change_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_version_change_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_version_change_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_version_change_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_version_change_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &version_change_flag7,
        NULL,
        0
      },
    };

  /*
   * IPMI_FRU_State
   */
  struct conffile_option fru_state_options[] =
    {
      {
        ipmi_interpret_fru_state_config[0].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag0,
        NULL,
        0
      },
      {
        ipmi_interpret_fru_state_config[1].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag1,
        NULL,
        0
      },
      {
        ipmi_interpret_fru_state_config[2].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag2,
        NULL,
        0
      },
      {
        ipmi_interpret_fru_state_config[3].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag3,
        NULL,
        0
      },
      {
        ipmi_interpret_fru_state_config[4].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag4,
        NULL,
        0
      },
      {
        ipmi_interpret_fru_state_config[5].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag5,
        NULL,
        0
      },
      {
        ipmi_interpret_fru_state_config[6].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag6,
        NULL,
        0
      },
      {
        ipmi_interpret_fru_state_config[7].option_str,
        CONFFILE_OPTION_STRING,
        -1,
        _cb_state_parse,
        1,
        0,
        &fru_state_flag7,
        NULL,
        0
      },
    };

  struct conffile_option sel_oem_sensor_config_options[] =
    {
      /* OEM Config */
      {
        "IPMI_OEM_Sensor_System_Event_Record",
        CONFFILE_OPTION_LIST_STRING,
        9,
        _cb_sel_oem_sensor_parse,
        -1,
        0,
        &sel_oem_sensor_flag,
        NULL,
        0,
      },
    };

  struct conffile_option sel_oem_record_config_options[] =
    {
      /* OEM Config */
      {
	"IPMI_OEM_Timestamped_Record",
	CONFFILE_OPTION_LIST_STRING,
	10,
	_cb_sel_oem_record_parse,
	-1,
	0,
	&sel_oem_timestamped_flag,
	NULL,
	0
      },
      {
	"IPMI_OEM_Non_Timestamped_Record",
	CONFFILE_OPTION_LIST_STRING,
	17,
	_cb_sel_oem_record_parse,
	-1,
	0,
	&sel_oem_non_timestamped_flag,
	NULL,
	0
      },
    };

  struct conffile_option sensor_oem_config_options[] =
    {
      /* OEM Config */
      {
	"IPMI_OEM_Bitmask",
	CONFFILE_OPTION_LIST_STRING,
	6,
	_cb_sensor_oem_parse,
	-1,
	0,
	&sensor_oem_bitmask_flag,
	NULL,
	0
      },
      {
	"IPMI_OEM_Value",
	CONFFILE_OPTION_LIST_STRING,
	6,
	_cb_sensor_oem_parse,
	-1,
	0,
	&sensor_oem_value_flag,
	NULL,
	0
      },
    };

  conffile_t cf = NULL;
  int options_len;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (config_file);

  if (!(cf = conffile_handle_create ()))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  if (sel_config)
    {
      options_len = sizeof (threshold_sel_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         threshold_sel_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_threshold_sel_config);
      config_file_options_len += options_len;

      options_len = sizeof (voltage_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         voltage_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_voltage_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (voltage_performance_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         voltage_performance_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_voltage_performance_config);
      config_file_options_len += options_len;

      options_len = sizeof (fan_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fan_device_present_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_fan_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (fan_transition_availability_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fan_transition_availability_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_fan_transition_availability_config);
      config_file_options_len += options_len;

      options_len = sizeof (fan_redundancy_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fan_redundancy_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_fan_redundancy_config);
      config_file_options_len += options_len;

      options_len = sizeof (physical_security_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         physical_security_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_physical_security_config);
      config_file_options_len += options_len;

      options_len = sizeof (platform_security_violation_attempt_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         platform_security_violation_attempt_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_platform_security_violation_attempt_config);
      config_file_options_len += options_len;

      options_len = sizeof (processor_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         processor_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_processor_config);
      config_file_options_len += options_len;

      options_len = sizeof (processor_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         processor_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_processor_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_supply_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_supply_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_power_supply_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_supply_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_supply_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_power_supply_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_supply_redundancy_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_supply_redundancy_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_power_supply_redundancy_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_unit_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_unit_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_power_unit_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_unit_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_unit_device_present_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_power_unit_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_unit_redundancy_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_unit_redundancy_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_power_unit_redundancy_config);
      config_file_options_len += options_len;

      options_len = sizeof (memory_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         memory_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_memory_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_drive_slot_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_drive_slot_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_predictive_failure_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_predictive_failure_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_drive_slot_predictive_failure_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_device_present_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_drive_slot_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (system_firmware_progress_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         system_firmware_progress_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_system_firmware_progress_config);
      config_file_options_len += options_len;

      options_len = sizeof (event_logging_disabled_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         event_logging_disabled_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_event_logging_disabled_config);
      config_file_options_len += options_len;

      options_len = sizeof (system_event_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         system_event_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_system_event_config);
      config_file_options_len += options_len;

      options_len = sizeof (critical_interrupt_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         critical_interrupt_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_critical_interrupt_config);
      config_file_options_len += options_len;

      options_len = sizeof (button_switch_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         button_switch_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_button_switch_config);
      config_file_options_len += options_len;

      options_len = sizeof (button_switch_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         button_switch_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_button_switch_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (chip_set_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         chip_set_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_chip_set_config);
      config_file_options_len += options_len;

      options_len = sizeof (module_board_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         module_board_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_module_board_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (module_board_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         module_board_device_present_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_module_board_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (cable_interconnect_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         cable_interconnect_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_cable_interconnect_config);
      config_file_options_len += options_len;

      options_len = sizeof (system_boot_initiated_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         system_boot_initiated_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_system_boot_initiated_config);
      config_file_options_len += options_len;

      options_len = sizeof (boot_error_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         boot_error_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_boot_error_config);
      config_file_options_len += options_len;

      options_len = sizeof (os_boot_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         os_boot_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_os_boot_config);
      config_file_options_len += options_len;

      options_len = sizeof (os_critical_stop_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         os_critical_stop_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_os_critical_stop_config);
      config_file_options_len += options_len;

      options_len = sizeof (slot_connector_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         slot_connector_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_slot_connector_config);
      config_file_options_len += options_len;

      options_len = sizeof (system_acpi_power_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         system_acpi_power_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_system_acpi_power_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (watchdog2_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         watchdog2_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_watchdog2_config);
      config_file_options_len += options_len;

      options_len = sizeof (platform_alert_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         platform_alert_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_platform_alert_config);
      config_file_options_len += options_len;

      options_len = sizeof (entity_presence_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         entity_presence_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_entity_presence_config);
      config_file_options_len += options_len;

      options_len = sizeof (entity_presence_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         entity_presence_device_present_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_entity_presence_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (lan_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         lan_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_lan_config);
      config_file_options_len += options_len;

      options_len = sizeof (management_subsystem_health_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         management_subsystem_health_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_management_subsystem_health_config);
      config_file_options_len += options_len;

      options_len = sizeof (battery_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         battery_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_battery_config);
      config_file_options_len += options_len;

      options_len = sizeof (session_audit_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         session_audit_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_session_audit_config);
      config_file_options_len += options_len;

      options_len = sizeof (version_change_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         version_change_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_version_change_config);
      config_file_options_len += options_len;

      options_len = sizeof (fru_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fru_state_options,
                                         options_len,
                                         ctx->interpret_sel.ipmi_interpret_fru_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (sel_oem_sensor_config_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         sel_oem_sensor_config_options,
                                         options_len,
                                         &ctx->interpret_sel.sel_oem_sensor_config);
      config_file_options_len += options_len;

      options_len = sizeof (sel_oem_record_config_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         sel_oem_record_config_options,
                                         options_len,
                                         &ctx->interpret_sel.sel_oem_record_config);
      config_file_options_len += options_len;
    }
  else
    {
      options_len = sizeof (threshold_sensor_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         threshold_sensor_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_threshold_sensor_config);
      config_file_options_len += options_len;

      options_len = sizeof (voltage_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         voltage_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_voltage_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (voltage_performance_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         voltage_performance_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_voltage_performance_config);
      config_file_options_len += options_len;

      options_len = sizeof (fan_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fan_device_present_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_fan_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (fan_transition_availability_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fan_transition_availability_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_fan_transition_availability_config);
      config_file_options_len += options_len;

      options_len = sizeof (fan_redundancy_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fan_redundancy_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_fan_redundancy_config);
      config_file_options_len += options_len;

      options_len = sizeof (physical_security_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         physical_security_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_physical_security_config);
      config_file_options_len += options_len;

      options_len = sizeof (platform_security_violation_attempt_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         platform_security_violation_attempt_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_platform_security_violation_attempt_config);
      config_file_options_len += options_len;

      options_len = sizeof (processor_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         processor_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_processor_config);
      config_file_options_len += options_len;

      options_len = sizeof (processor_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         processor_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_processor_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_supply_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_supply_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_power_supply_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_supply_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_supply_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_power_supply_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_supply_redundancy_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_supply_redundancy_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_power_supply_redundancy_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_unit_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_unit_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_power_unit_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_unit_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_unit_device_present_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_power_unit_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (power_unit_redundancy_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         power_unit_redundancy_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_power_unit_redundancy_config);
      config_file_options_len += options_len;

      options_len = sizeof (memory_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         memory_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_memory_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_drive_slot_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_drive_slot_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_predictive_failure_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_predictive_failure_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_drive_slot_predictive_failure_config);
      config_file_options_len += options_len;

      options_len = sizeof (drive_slot_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         drive_slot_device_present_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_drive_slot_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (system_firmware_progress_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         system_firmware_progress_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_system_firmware_progress_config);
      config_file_options_len += options_len;

      options_len = sizeof (event_logging_disabled_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         event_logging_disabled_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_event_logging_disabled_config);
      config_file_options_len += options_len;

      options_len = sizeof (system_event_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         system_event_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_system_event_config);
      config_file_options_len += options_len;

      options_len = sizeof (critical_interrupt_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         critical_interrupt_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_critical_interrupt_config);
      config_file_options_len += options_len;

      options_len = sizeof (button_switch_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         button_switch_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_button_switch_config);
      config_file_options_len += options_len;

      options_len = sizeof (button_switch_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         button_switch_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_button_switch_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (module_board_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         module_board_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_module_board_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (module_board_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         module_board_device_present_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_module_board_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (cable_interconnect_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         cable_interconnect_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_cable_interconnect_config);
      config_file_options_len += options_len;

      options_len = sizeof (boot_error_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         boot_error_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_boot_error_config);
      config_file_options_len += options_len;

      options_len = sizeof (slot_connector_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         slot_connector_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_slot_connector_config);
      config_file_options_len += options_len;

      options_len = sizeof (system_acpi_power_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         system_acpi_power_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_system_acpi_power_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (watchdog2_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         watchdog2_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_watchdog2_config);
      config_file_options_len += options_len;

      options_len = sizeof (entity_presence_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         entity_presence_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_entity_presence_config);
      config_file_options_len += options_len;

      options_len = sizeof (entity_presence_device_present_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         entity_presence_device_present_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_entity_presence_device_present_config);
      config_file_options_len += options_len;

      options_len = sizeof (management_subsystem_health_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         management_subsystem_health_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_management_subsystem_health_config);
      config_file_options_len += options_len;

      options_len = sizeof (battery_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         battery_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_battery_config);
      config_file_options_len += options_len;

      options_len = sizeof (fru_state_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         fru_state_options,
                                         options_len,
                                         ctx->interpret_sensor.ipmi_interpret_fru_state_config);
      config_file_options_len += options_len;

      options_len = sizeof (sensor_oem_config_options)/sizeof (struct conffile_option);
      _copy_options_and_fill_option_ptr (config_file_options,
                                         config_file_options_len,
                                         sensor_oem_config_options,
                                         options_len,
                                         &ctx->interpret_sensor.sensor_oem_config);
      config_file_options_len += options_len;
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
              if (sel_config)
                INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST);
              else
                INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST);
              goto cleanup;
            }
        }
      
      if (CONFFILE_IS_PARSE_ERR (conffile_errnum (cf)))
        {
          if (sel_config)
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE);
            else
            INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_PARSE);
        }
      else if (conffile_errnum (cf) == CONFFILE_ERR_OUTMEM)
        INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      else
        INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_INTERNAL_ERROR);
      
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  if (cf)
    conffile_handle_destroy (cf);
  return (rv);
}

int
ipmi_interpret_sel_config_parse (ipmi_interpret_ctx_t ctx,
                                 const char *sel_config_file)
{
  char *config_file = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (!sel_config_file)
    config_file = INTERPRET_SEL_CONFIG_FILE_DEFAULT;
  else
    config_file = (char *)sel_config_file;

  return (_ipmi_interpret_config_parse (ctx, config_file, 1));
}

int
ipmi_interpret_sensor_config_parse (ipmi_interpret_ctx_t ctx,
                                    const char *sensor_config_file)
{
  char *config_file = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (!sensor_config_file)
    config_file = INTERPRET_SENSOR_CONFIG_FILE_DEFAULT;
  else
    config_file = (char *)sensor_config_file;

  return (_ipmi_interpret_config_parse (ctx, config_file, 0));
}
