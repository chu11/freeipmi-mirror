/*
  Copyright (C) 2003-2010 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"

#include "ipmi-interpret-defs.h"
#include "ipmi-interpret-trace.h"
#include "ipmi-interpret-sensor-config.h"
#include "ipmi-interpret-util.h"

#include "freeipmi-portability.h"

static struct ipmi_interpret_sensor_config ipmi_interpret_threshold_sensor_config[] =
  {
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Critical_Threshold", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Critical_Threshold", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Below_Lower_Non_Recoverable_Threshold", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Critical_Threshold", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Critical_Threshold", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Threshold_Sensor_At_Or_Above_Upper_Non_Recoverable_Threshold", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_threshold_sensor_config_len = 6;

static struct ipmi_interpret_sensor_config ipmi_interpret_voltage_state_config[] =
  {
    { "IPMI_Voltage_State_Deasserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Voltage_State_Asserted", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_voltage_state_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_voltage_performance_config[] =
  {
    { "IPMI_Voltage_Performance_Met", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Voltage_Performance_Lags", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_voltage_performance_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_fan_device_present_config[] =
  {
    { "IPMI_Fan_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Fan_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_fan_device_present_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_fan_transition_availability_config[] =
  {
    { "IPMI_Fan_Transition_Availability_To_Running", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Fan_Transition_Availability_To_In_Test", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Power_Off", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_On_Line", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Off_Line", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Off_Duty", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_To_Degraded", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Fan_Transition_Availability_To_Power_Save", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Transition_Availability_Install_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_fan_transition_availability_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_fan_redundancy_config[] =
  {
    { "IPMI_Fan_Redundancy_Fully_Redundant", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Fan_Redundancy_Redundancy_Lost", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Fan_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_fan_redundancy_config_len = 8;

static struct ipmi_interpret_sensor_config ipmi_interpret_physical_security_config[] =
  {
    { "IPMI_Physical_Security_General_Chassis_Intrusion", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Physical_Security_Drive_Bay_Intrusion", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Physical_Security_IO_Card_Intrusion", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Physical_Security_Processor_Area_Intrusion", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Physical_Security_LAN_Leash_Lost", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Physical_Security_Unauthorized_Dock_Undock", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Physical_Security_FAN_Area_Intrusion", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_physical_security_config_len = 7;

static struct ipmi_interpret_sensor_config ipmi_interpret_platform_security_violation_attempt_config[] =
  {
    { "IPMI_Platform_Security_Violation_Attempt_Secure_Mode_Violation_Attempt", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_User_Password", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Attempt_Setup_Password", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Pre_Boot_Password_Violation_Network_Boot_Password", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Other_Pre_Boot_Password_Violation", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Platform_Security_Violation_Attempt_Out_Of_Band_Access_Password_Violation", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_platform_security_violation_attempt_config_len = 6;

static struct ipmi_interpret_sensor_config ipmi_interpret_processor_config[] =
  {
    { "IPMI_Processor_IERR", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_Thermal_Trip", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_FRB1_BIST_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_FRB2_Hang_In_POST_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_FRB3_Processor_Startup_Initialization_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_Configuration_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_SMBIOS_Uncorrectable_CPU_Complex_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_Processor_Presence_Detected", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Processor_Processor_Disabled", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_Terminator_Presence_Detected", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_Processor_Automatically_Throttled", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Processor_Machine_Check_Exception", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Processor_Correctable_Machine_Check_Error", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_processor_config_len = 13;

static struct ipmi_interpret_sensor_config ipmi_interpret_processor_state_config[] =
  {
    { "IPMI_Processor_State_Deasserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Processor_State_Asserted", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_processor_state_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_power_supply_config[] =
  {
    { "IPMI_Power_Supply_Presence_Detected", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Power_Supply_Power_Supply_Failure_Detected", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Predictive_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Lost_AC_DC", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Lost_Or_Out_Of_Range", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Power_Supply_Input_Out_Of_Range_But_Present", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Configuration_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_power_supply_config_len = 7;

static struct ipmi_interpret_sensor_config ipmi_interpret_power_supply_state_config[] =
  {
    { "IPMI_Power_Supply_State_Deasserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Power_Supply_State_Asserted", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_power_supply_state_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_power_supply_redundancy_config[] =
  {
    { "IPMI_Power_Supply_Redundancy_Fully_Redundant", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Lost", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Power_Supply_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_power_supply_redundancy_config_len = 8;

static struct ipmi_interpret_sensor_config ipmi_interpret_power_unit_config[] =
  {
    { "IPMI_Power_Unit_Power_Off_Power_Down", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Power_Unit_Power_Cycle", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Power_Unit_240VA_Power_Down", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Power_Unit_Interlock_Power_Down", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Power_Unit_AC_Lost", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Soft_Power_Control_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Power_Unit_Failure_Detected", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Predictive_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_power_unit_config_len = 8;

static struct ipmi_interpret_sensor_config ipmi_interpret_power_unit_device_present_config[] =
  {
    { "IPMI_Power_Unit_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_power_unit_device_present_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_power_unit_redundancy_config[] =
  {
    { "IPMI_Power_Unit_Redundancy_Fully_Redundant", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Lost", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Redundant", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Sufficient_Resources_From_Insufficient_Redundancy", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Non_Redundant_Insufficient_Resources", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Fully_Redundant", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Power_Unit_Redundancy_Redundancy_Degraded_From_Non_Redundant", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_power_unit_redundancy_config_len = 8;

static struct ipmi_interpret_sensor_config ipmi_interpret_memory_config[] =
  {
    { "IPMI_Memory_Correctable_Memory_Error", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Memory_Uncorrectable_Memory_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Memory_Parity", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Memory_Memory_Scrub_Failed", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Memory_Memory_Device_Disabled", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Memory_Correctable_Memory_Error_Logging_Limit_Reached", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Memory_Presence_Detected", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Memory_Configuration_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Memory_Spare", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Memory_Memory_Automatically_Throttled", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Memory_Critical_Overtemperature", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_memory_config_len = 11;

static struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_config[] =
  {
    { "IPMI_Drive_Slot_Drive_Presence", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Drive_Fault", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Predictive_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Hot_Spare", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Consistency_Check_Parity_Check_In_Progress", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Drive_Slot_In_Critical_Array", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Drive_Slot_In_Failed_Array", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Rebuild_Remap_In_Progress", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Rebuild_Remap_Aborted", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_drive_slot_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_state_config[] =
  {
    { "IPMI_Drive_Slot_State_Deasserted", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Drive_Slot_State_Asserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_drive_slot_state_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_predictive_failure_config[] =
  {
    { "IPMI_Drive_Slot_Predictive_Failure_Deasserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Drive_Slot_Predictive_Failure_Asserted", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_drive_slot_predictive_failure_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_drive_slot_device_present_config[] =
  {
    { "IPMI_Drive_Slot_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Drive_Slot_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_drive_slot_device_present_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_system_firmware_progress_config[] =
  {
    { "IPMI_System_Firmware_Progress_System_Firmware_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_System_Firmware_Hang", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_System_Firmware_Progress_System_Firmware_Progress", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_system_firmware_progress_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_event_logging_disabled_config[] =
  {
    { "IPMI_Event_Logging_Disabled_Correctable_Memory_Error_Logging_Disabled", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_Event_Type_Logging_Disabled", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_Log_Area_Reset_Cleared", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Event_Logging_Disabled_All_Event_Logging_Disabled", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_SEL_Full", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Event_Logging_Disabled_SEL_Almost_Full", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Event_Logging_Disabled_Correctable_Machine_Check_Error_Logging_Disabled", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_event_logging_disabled_config_len = 7;

static struct ipmi_interpret_sensor_config ipmi_interpret_system_event_config[] =
  {
    { "IPMI_System_Event_System_Reconfigured", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_System_Event_OEM_System_Boot_Event", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_Event_Undetermined_System_Hardware_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_System_Event_Entry_Added_To_Auxiliary_Log", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_Event_PEF_Action", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_Event_Timestamp_Clock_Sync", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_system_event_config_len = 6;

static struct ipmi_interpret_sensor_config ipmi_interpret_critical_interrupt_config[] =
  {
    { "IPMI_Critical_Interrupt_Front_Panel_NMI_Diagnostic_Interrupt", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Timeout", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_IO_Channel_Check_NMI", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Software_NMI", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Critical_Interrupt_PCI_PERR", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_PCI_SERR", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_EISA_Fail_Safe_Timeout", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Correctable_Error", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Critical_Interrupt_Bus_Uncorrectable_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Fatal_NMI", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Fatal_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Critical_Interrupt_Bus_Degraded", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_critical_interrupt_config_len = 12;

static struct ipmi_interpret_sensor_config ipmi_interpret_button_switch_config[] =
  {
    { "IPMI_Button_Switch_Power_Button_Pressed", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Button_Switch_Sleep_Button_Pressed", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Button_Switch_Reset_Button_Pressed", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Button_Switch_FRU_Latch_Open", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Button_Switch_FRU_Service_Request_Button", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_button_switch_config_len = 5;

/* achu: for a button/switch states, I don't think users really care.
 * So report Nominal for all states.
 */
static struct ipmi_interpret_sensor_config ipmi_interpret_button_switch_state_config[] =
  {
    { "IPMI_Button_Switch_State_Deasserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Button_Switch_State_Asserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_button_switch_state_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_module_board_state_config[] =
  {
    { "IPMI_Module_Board_State_Deasserted", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Module_Board_State_Asserted", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_module_board_state_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_module_board_device_present_config[] =
  {
    { "IPMI_Module_Board_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Module_Board_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_module_board_device_present_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_cable_interconnect_config[] =
  {
    { "IPMI_Cable_Interconnect_Is_Connected", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Cable_Interconnect_Configuration_Error", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_cable_interconnect_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_boot_error_config[] =
  {
    { "IPMI_Boot_Error_No_Bootable_Media", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Boot_Error_Non_Bootable_Diskette_Left_In_Drive", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Boot_Error_PXE_Server_Not_Found", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Boot_Error_Invalid_Boot_Sector", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Boot_Error_Timeout_Waiting_For_User_Selection_Of_Boot_Source", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_boot_error_config_len = 5;

static struct ipmi_interpret_sensor_config ipmi_interpret_slot_connector_config[] =
  {
    { "IPMI_Slot_Connector_Fault_Status_Asserted", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Slot_Connector_Identify_Status_Asserted", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Slot_Connector_Slot_Connector_Device_Installed_Attached", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Connector_Ready_For_Device_Installation", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Connector_Ready_For_Device_Removal", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Power_Is_Off", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Slot_Connector_Slot_Connector_Device_Removal_Request", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Slot_Connector_Interlock_Asserted", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Slot_Connector_Slot_Is_Disabled", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Slot_Connector_Slot_Holds_Spare_Device", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_slot_connector_config_len = 10;

static struct ipmi_interpret_sensor_config ipmi_interpret_system_acpi_power_state_config[] =
  {
    { "IPMI_System_ACPI_Power_State_S0_G0", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S1", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S2", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S3", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S4", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S5_G2", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S4_S5_Soft_Off", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_G3_Mechanical_Off", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Sleeping_in_an_S1_S2_or_S3_States", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_G1_Sleeping", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_S5_Entered_By_Override", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Legacy_ON_State", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Legacy_OFF_State", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_System_ACPI_Power_State_Unspecified", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_System_ACPI_Power_State_Unknown", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_system_acpi_power_state_config_len = 15;

static struct ipmi_interpret_sensor_config ipmi_interpret_watchdog2_config[] =
  {
    { "IPMI_Watchdog2_Timer_Expired", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Watchdog2_Hard_Reset", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Watchdog2_Power_Down", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Watchdog2_Power_Cycle", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Watchdog2_Reserved1", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Watchdog2_Reserved2", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Watchdog2_Reserved3", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Watchdog2_Reserved4", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Watchdog2_Timer_Interrupt", IPMI_INTERPRET_SENSOR_STATE_WARNING},
  };
static unsigned int ipmi_interpret_watchdog2_config_len = 9;

static struct ipmi_interpret_sensor_config ipmi_interpret_entity_presence_config[] =
  {
    { "IPMI_Entity_Presence_Entity_Present", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_Entity_Presence_Entity_Absent", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Entity_Presence_Entity_Disabled", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_entity_presence_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_entity_presence_device_present_config[] =
  {
    { "IPMI_Entity_Presence_Device_Present_Device_Removed_Device_Absent", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Entity_Presence_Device_Present_Device_Inserted_Device_Present", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_entity_presence_device_present_config_len = 2;

static struct ipmi_interpret_sensor_config ipmi_interpret_management_subsystem_health_config[] =
  {
    { "IPMI_Management_Subsystem_Health_Sensor_Access_Degraded_Or_Unavailable", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Controller_Access_Degraded_Or_Unavailable", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Management_Controller_Off_Line", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Management_Controller_Unavailable", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_Sensor_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Management_Subsystem_Health_FRU_Failure", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_management_subsystem_health_config_len = 6;

static struct ipmi_interpret_sensor_config ipmi_interpret_battery_config[] =
  {
    { "IPMI_Battery_Battery_Low", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_Battery_Battery_Failed", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_Battery_Battery_Presence_Detected", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
  };
static unsigned int ipmi_interpret_battery_config_len = 3;

static struct ipmi_interpret_sensor_config ipmi_interpret_fru_state_config[] =
  {
    { "IPMI_FRU_State_FRU_Not_Installed", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_FRU_State_FRU_Inactive", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
    { "IPMI_FRU_State_FRU_Activation_Requested", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Activation_In_Progress", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Active", IPMI_INTERPRET_SENSOR_STATE_NOMINAL},
    { "IPMI_FRU_State_FRU_Deactivation_Requested", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Deactivation_In_Progress", IPMI_INTERPRET_SENSOR_STATE_WARNING},
    { "IPMI_FRU_State_FRU_Communication_Lost", IPMI_INTERPRET_SENSOR_STATE_CRITICAL},
  };
static unsigned int ipmi_interpret_fru_state_config_len = 8;

static int
_interpret_sensor_config_init (ipmi_interpret_ctx_t ctx,
                               struct ipmi_interpret_sensor_config ***sensor_config_dest,
                               struct ipmi_interpret_sensor_config *sensor_config_src,
                               unsigned int sensor_config_len)
{
  unsigned int mlen;
  unsigned int i;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);
  assert (sensor_config_dest);
  assert (sensor_config_src);
  assert (sensor_config_len);

  /* +1 for storing NULL pointer sentinel value */
  mlen = sizeof (struct ipmi_interpret_sensor_config *) * (sensor_config_len + 1);

  if (!((*sensor_config_dest) = (struct ipmi_interpret_sensor_config **) malloc (mlen)))
    {
      INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }
  memset ((*sensor_config_dest), '\0', mlen);

  mlen = sizeof (struct ipmi_interpret_sensor_config);

  for (i = 0; i < sensor_config_len; i++)
    {
      if (!((*sensor_config_dest)[i] = (struct ipmi_interpret_sensor_config *) malloc (mlen)))
        {
          INTERPRET_SET_ERRNUM (ctx, IPMI_INTERPRET_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }
      memset ((*sensor_config_dest)[i], '\0', mlen);

      (*sensor_config_dest)[i]->option_str = sensor_config_src[i].option_str;
      (*sensor_config_dest)[i]->sensor_state = sensor_config_src[i].sensor_state;
    }
  (*sensor_config_dest)[i] = NULL;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_interpret_sensors_init (ipmi_interpret_ctx_t ctx)
{
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_threshold_sensor_config,
                                     ipmi_interpret_threshold_sensor_config,
                                     ipmi_interpret_threshold_sensor_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_voltage_state_config,
                                     ipmi_interpret_voltage_state_config,
                                     ipmi_interpret_voltage_state_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_voltage_performance_config,
                                     ipmi_interpret_voltage_performance_config,
                                     ipmi_interpret_voltage_performance_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_fan_device_present_config,
                                     ipmi_interpret_fan_device_present_config,
                                     ipmi_interpret_fan_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_fan_transition_availability_config,
                                     ipmi_interpret_fan_transition_availability_config,
                                     ipmi_interpret_fan_transition_availability_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_fan_redundancy_config,
                                     ipmi_interpret_fan_redundancy_config,
                                     ipmi_interpret_fan_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_physical_security_config,
                                     ipmi_interpret_physical_security_config,
                                     ipmi_interpret_physical_security_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_platform_security_violation_attempt_config,
                                     ipmi_interpret_platform_security_violation_attempt_config,
                                     ipmi_interpret_platform_security_violation_attempt_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_processor_config,
                                     ipmi_interpret_processor_config,
                                     ipmi_interpret_processor_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_processor_state_config,
                                     ipmi_interpret_processor_state_config,
                                     ipmi_interpret_processor_state_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_power_supply_config,
                                     ipmi_interpret_power_supply_config,
                                     ipmi_interpret_power_supply_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_power_supply_state_config,
                                     ipmi_interpret_power_supply_state_config,
                                     ipmi_interpret_power_supply_state_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_power_supply_redundancy_config,
                                     ipmi_interpret_power_supply_redundancy_config,
                                     ipmi_interpret_power_supply_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_power_unit_config,
                                     ipmi_interpret_power_unit_config,
                                     ipmi_interpret_power_unit_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_power_unit_device_present_config,
                                     ipmi_interpret_power_unit_device_present_config,
                                     ipmi_interpret_power_unit_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_power_unit_redundancy_config,
                                     ipmi_interpret_power_unit_redundancy_config,
                                     ipmi_interpret_power_unit_redundancy_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_memory_config,
                                     ipmi_interpret_memory_config,
                                     ipmi_interpret_memory_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_drive_slot_config,
                                     ipmi_interpret_drive_slot_config,
                                     ipmi_interpret_drive_slot_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_drive_slot_state_config,
                                     ipmi_interpret_drive_slot_state_config,
                                     ipmi_interpret_drive_slot_state_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_drive_slot_predictive_failure_config,
                                     ipmi_interpret_drive_slot_predictive_failure_config,
                                     ipmi_interpret_drive_slot_predictive_failure_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_drive_slot_device_present_config,
                                     ipmi_interpret_drive_slot_device_present_config,
                                     ipmi_interpret_drive_slot_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_system_firmware_progress_config,
                                     ipmi_interpret_system_firmware_progress_config,
                                     ipmi_interpret_system_firmware_progress_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_event_logging_disabled_config,
                                     ipmi_interpret_event_logging_disabled_config,
                                     ipmi_interpret_event_logging_disabled_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_system_event_config,
                                     ipmi_interpret_system_event_config,
                                     ipmi_interpret_system_event_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_critical_interrupt_config,
                                     ipmi_interpret_critical_interrupt_config,
                                     ipmi_interpret_critical_interrupt_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_button_switch_config,
                                     ipmi_interpret_button_switch_config,
                                     ipmi_interpret_button_switch_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_button_switch_state_config,
                                     ipmi_interpret_button_switch_state_config,
                                     ipmi_interpret_button_switch_state_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_module_board_state_config,
                                     ipmi_interpret_module_board_state_config,
                                     ipmi_interpret_module_board_state_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_module_board_device_present_config,
                                     ipmi_interpret_module_board_device_present_config,
                                     ipmi_interpret_module_board_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_cable_interconnect_config,
                                     ipmi_interpret_cable_interconnect_config,
                                     ipmi_interpret_cable_interconnect_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_boot_error_config,
                                     ipmi_interpret_boot_error_config,
                                     ipmi_interpret_boot_error_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_slot_connector_config,
                                     ipmi_interpret_slot_connector_config,
                                     ipmi_interpret_slot_connector_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_system_acpi_power_state_config,
                                     ipmi_interpret_system_acpi_power_state_config,
                                     ipmi_interpret_system_acpi_power_state_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_watchdog2_config,
                                     ipmi_interpret_watchdog2_config,
                                     ipmi_interpret_watchdog2_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_entity_presence_config,
                                     ipmi_interpret_entity_presence_config,
                                     ipmi_interpret_entity_presence_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_entity_presence_device_present_config,
                                     ipmi_interpret_entity_presence_device_present_config,
                                     ipmi_interpret_entity_presence_device_present_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_management_subsystem_health_config,
                                     ipmi_interpret_management_subsystem_health_config,
                                     ipmi_interpret_management_subsystem_health_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_battery_config,
                                     ipmi_interpret_battery_config,
                                     ipmi_interpret_battery_config_len) < 0)
    goto cleanup;

  if (_interpret_sensor_config_init (ctx,
                                     &ctx->interpret_sensors.ipmi_interpret_fru_state_config,
                                     ipmi_interpret_fru_state_config,
                                     ipmi_interpret_fru_state_config_len) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static void
_interpret_sensor_config_destroy (ipmi_interpret_ctx_t ctx,
                                  struct ipmi_interpret_sensor_config **sensor_config)
{
  unsigned int i;

  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  if (sensor_config)
    {
      while (sensor_config[i])
        {
          free (sensor_config[i]);
          i++;
        }
      free (sensor_config);
    }
}

void
ipmi_interpret_sensors_destroy (ipmi_interpret_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_INTERPRET_CTX_MAGIC);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_threshold_sensor_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_voltage_state_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_voltage_performance_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_fan_device_present_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_fan_transition_availability_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_fan_redundancy_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_physical_security_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_platform_security_violation_attempt_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_processor_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_processor_state_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_power_supply_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_power_supply_state_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_power_supply_redundancy_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_power_unit_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_power_unit_device_present_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_power_unit_redundancy_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_memory_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_drive_slot_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_drive_slot_state_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_drive_slot_predictive_failure_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_drive_slot_device_present_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_system_firmware_progress_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_event_logging_disabled_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_system_event_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_critical_interrupt_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_button_switch_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_button_switch_state_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_module_board_state_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_module_board_device_present_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_cable_interconnect_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_boot_error_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_slot_connector_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_system_acpi_power_state_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_watchdog2_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_entity_presence_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_entity_presence_device_present_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_management_subsystem_health_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_battery_config);

  _interpret_sensor_config_destroy (ctx,
                                    ctx->interpret_sensors.ipmi_interpret_fru_state_config);
}
