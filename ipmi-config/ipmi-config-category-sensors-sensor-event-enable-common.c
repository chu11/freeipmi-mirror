/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-section.h"
#include "ipmi-config-category-sensors-utils.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

/* event field strings */

/* achu: making a generic "convert event message string into
 * key_name" for this tool was difficult.  There are too many
 * conversions and exceptions to the rules (abbreviations, slashes,
 * spaces, hypens, spaces w/ hyphens, single quotes, double quotes,
 * examples, parentheses, strings that are too long, wierd
 * capitalization, need lower cases, etc.)  that strings never
 * turned out right.  I decided to just hard code names in at the
 * end of the day.
 */

char *threshold_event_strings[] =
  {
    "Lower_Non_Critical_Going_Low",
    "Lower_Non_Critical_Going_High",
    "Lower_Critical_Going_Low",
    "Lower_Critical_Going_High",
    "Lower_Non_Recoverable_Going_Low",
    "Lower_Non_Recoverable_Going_High",
    "Upper_Non_Critical_Going_Low",
    "Upper_Non_Critical_Going_High",
    "Upper_Critical_Going_Low",
    "Upper_Critical_Going_High",
    "Upper_Non_Recoverable_Going_Low",
    "Upper_Non_Recoverable_Going_High",
    NULL,
  };

char *generic_event_strings_0x02[] =
  {
    "Transition_to_Idle",
    "Transition_to_Active",
    "Transition_to_Busy",
    NULL,
  };

char *generic_event_strings_0x03[] =
  {
    "State_Deasserted",
    "State_Asserted",
    NULL,
  };

char *generic_event_strings_0x04[] =
  {
    "Predictive_Failure_Deasserted",
    "Predictive_Failure_Asserted",
    NULL,
  };

char *generic_event_strings_0x05[] =
  {
    "Limit_Not_Exceeded",
    "Limit_Exceeded",
    NULL,
  };

char *generic_event_strings_0x06[] =
  {
    "Performance_Met",
    "Performance_Lags",
    NULL,
  };

char *generic_event_strings_0x07[] =
  {
    "Transition_to_OK",
    "Transition_to_Non_Critical_from_OK",
    "Transition_to_Critical_from_Less_Severe",
    "Transition_to_Non_Recoverable_from_Less_Severe",
    "Transition_to_Non_Critical_from_More_Severe",
    "Transition_to_Critical_from_Non_Recoverable",
    "Transition_to_Non_Recoverable",
    "Monitor",
    "Informational",
    NULL,
  };

char *generic_event_strings_0x08[] =
  {
    "Device_Removed_or_Device_Absent",
    "Device_Inserted_or_Device_Present",
    NULL,
  };

char *generic_event_strings_0x09[] =
  {
    "Device_Disabled",
    "Device_Enabled",
    NULL,
  };

char *generic_event_strings_0x0A[] =
  {
    "Transition_to_Running",
    "Transition_to_In_Test",
    "Transition_to_Power_Off",
    "Transition_to_On_Line",
    "Transition_to_Off_Line",
    "Transition_to_Off_Duty",
    "Transition_to_Degraded",
    "Transition_to_Power_Save",
    "Install_Error",
    NULL,
  };

char *generic_event_strings_0x0B[] =
  {
    "Fully_Redundant",
    "Redundancy_Lost",
    "Redundancy_Degraded",
    "Entered_from_Redundancy_Degraded_or_Fully_Redundant",
    "Entered_from_Non_Redundant_Insufficient_Resources",
    "Non_Redundant_Insufficient_Resources",
    "Redundancy_Degraded_from_Fully_Redundant",
    "Redundancy_Degraded_from_Non_Redundant",
    NULL,
  };

char *generic_event_strings_0x0C[] =
  {
    "D0_Power_State",
    "D1_Power_State",
    "D2_Power_State",
    "D3_Power_State",
    NULL,
  };

char *sensor_specific_event_strings_physical_security_chassis_intrusion[] =
  {
    "General_Chassis_Intrusion",
    "Drive_Bay_Intrusion",
    "IO_Card_Area_Intrusion",
    "Processor_Area_Intrusion",
    "LAN_Leash_Lost",
    "Unauthorized_Dock",
    "FAN_Area_Intrusion",
    NULL,
  };

char *sensor_specific_event_strings_platform_security_violation_attempt[] =
  {
    "Secure_Mode_Violation_Attempt",
    "Pre_Boot_Password_Violation_User_Password",
    "Pre_Boot_Password_Violation_Attempt_Setup_Password",
    "Pre_Boot_Password_Violation_Network_Boot_Password",
    "Other_Pre_Boot_Password_Violation",
    "Out_of_Band_Access_Password_Violation",
    NULL,
  };

char *sensor_specific_event_strings_processor[] =
  {
    "IERR",
    "Thermal_Trip",
    "FRB1_BIST_failure",
    "FRB2_Hang_In_POST_Failure",
    "FRB3_Processor_Startup_Initialization_Failure",
    "Configuration_Error",
    "SM_BIOS_Uncorrectable_CPU_Complex_Error",
    "Processor_Presence_detected",
    "Processor_Disabled",
    "Terminator_Presence_Detected",
    "Processor_Automatically_Throttled",
    NULL,
  };

char *sensor_specific_event_strings_power_supply[] =
  {
    "Presence_Detected",
    "Power_Supply_Failure_Detected",
    "Predictive_Failure",
    "Power_Supply_Input_Lost_AC_DC",
    "Power_Supply_Input_Lost_or_Out_of_Range",
    "Power_Supply_Input_Out_of_Range_but_Present",
    "Configuration_Error",
    NULL,
  };

char *sensor_specific_event_strings_power_unit[] =
  {
    "Power_Off_or_Power_Down",
    "Power_Cycle",
    "240VA_Power_Down",
    "Interlock_Power_Down",
    "AC_Lost",
    "Soft_Power_Control_Failure",
    "Power_Unit_Failure_Detected",
    "Predictive_Failure",
    NULL,
  };

char *sensor_specific_event_strings_memory[] =
  {
    "Correctable_ECC",
    "Uncorrectable_ECC",
    "Parity",
    "Memory_Scrub_Failed",
    "Memory_Device_Disabled",
    "Correctable_ECC_Logging_Limit_Reached",
    "Presence_Detected",
    "Configuration_Error",
    "Spare",
    "Memory_Automatically_Throttled",
    "Critical_Overtemperature",
    NULL,
  };

char *sensor_specific_event_strings_drive_slot[] =
  {
    "Drive_Presence",
    "Drive_Fault",
    "Predictive_Failure",
    "Hot_Spare",
    "Consistency_Check_In_Progress",
    "In_Critical_Array",
    "In_Failed_Array",
    "Rebuild_or_Remap_In_Progress",
    "Rebuild_or_Remap_Aborted",
    NULL,
  };

char *sensor_specific_event_strings_system_firmware_progress[] =
  {
    "System_Firmware_Error",
    "System_Firmware_Hang",
    "System_Firmware_Progress",
    NULL,
  };

char *sensor_specific_event_strings_event_logging_disabled[] =
  {
    "Correctable_Memory_Error_Logging_Disabled",
    "Event_Type_Logging_Disabled",
    "Log_Area_Reset_or_Cleared",
    "All_Event_Logging_Disabled",
    "SEL_Full",
    "SEL_Almost_Full",
    NULL,
  };

char *sensor_specific_event_strings_system_event[] =
  {
    "System_Reconfigured",
    "OEM_System_Boot_Event",
    "Undetermined_System_Hardware_Failure",
    "Entry_Added_to_Auxiliary_Log",
    "PEF_Action",
    "Timestamp_Clock_Synch",
    NULL,
  };

char *sensor_specific_event_strings_critical_interrupt[] =
  {
    "Front_Panel_NMI_or_Diagnostic_Interrupt",
    "Bus_Timeout",
    "IO_Channel_Check_NMI",
    "Software_NMI",
    "PCI_PERR",
    "PCI_SERR",
    "EISA_Fail_Safe_Timeout",
    "Bus_Correctable_Error",
    "Bus_Uncorrectable_Error",
    "Fatal_NMI",
    "Bus_Fatal_Error",
    "Bus_Degraded",
    NULL,
  };

char *sensor_specific_event_strings_cable_interconnect[] =
  {
    "Cable_Interconnect_Is_Connected",
    "Configuration_Error_Incorrect_Cable_Connected_or_Incorrect_Interconnection",
    NULL,
  };

char *sensor_specific_event_strings_boot_error[] =
  {
    "No_Bootable_Media",
    "Non_Bootable_Diskette_Left_In_Drive",
    "PXE_Server_Not_Found",
    "Invalid_Boot_Sector",
    "Timeout_Waiting_For_User_Selection_Of_Boot_Source",
    NULL,
  };

char *sensor_specific_event_strings_slot_connector[] =
  {
    "Fault_Status_Asserted",
    "Identify_Status_Asserted",
    "Slot_Connector_Device_Installed_or_Attached",
    "Slot_Connector_Ready_For_Device_Installation",
    "Slot_Connector_Ready_For_Device_Removal",
    "Slot_Power_Is_Off",
    "Slot_Connector_Device_Removal_Request",
    "Interlock_Asserted",
    "Slot_Is_Disabled",
    "Slot_Holds_Spare_Device",
    NULL,
  };

/* event state 4-7 are "reserved" and useless */
char *sensor_specific_event_strings_watchdog2[] =
  {
    "Timer_Expired",
    "Hard_Reset",
    "Power_Down",
    "Power_Cycle",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Timer_Interrupt",
    NULL,
  };
int sensor_specific_event_strings_watchdog2_indexes[] = { 0, 1, 2, 3, 8, -1};

char *sensor_specific_event_strings_entity_presence[] =
  {
    "Entity_Present",
    "Entity_Absent",
    "Entity_Disabled",
    NULL,
  };

char *sensor_specific_event_strings_management_subsystem_health[] =
  {
    "Sensor_Access_Degraded_or_Unavailable",
    "Controller_Access_Degraded_or_Unavailable",
    "Management_Controller_Off_Line",
    "Management_Controller_Unavailable",
    "Sensor_Failure",
    "FRU_Failure",
    NULL,
  };

char *sensor_specific_event_strings_battery[] =
  {
    "Battery_Low",
    "Battery_Failed",
    "Battery_Presence_Detected",
    NULL,
  };

char *sensor_specific_event_strings_fru_state[] =
  {
    "FRU_Not_Installed",
    "FRU_Inactive",
    "FRU_Activation_Requested",
    "FRU_Activation_In_Progress",
    "FRU_Active",
    "FRU_Deactivation_Requested",
    "FRU_Deactivation_In_Progress",
    "FRU_Communication_Lost",
    NULL,
  };

/* convenience structs */

struct sensor_event_enable_data {
  uint8_t all_event_messages;
  uint8_t scanning_on_this_sensor;
  uint16_t assertion_bits;
  uint16_t deassertion_bits;
};

#define KEY_NAME_MAX_LEN 1024

typedef int (*Sdr_event_flags_func)(ipmi_sdr_ctx_t ctx,
				    const void *sdr_record,
				    unsigned int sdr_record_len,
                                    uint8_t *event_state_0,
                                    uint8_t *event_state_1,
                                    uint8_t *event_state_2,
                                    uint8_t *event_state_3,
                                    uint8_t *event_state_4,
                                    uint8_t *event_state_5,
                                    uint8_t *event_state_6,
                                    uint8_t *event_state_7,
                                    uint8_t *event_state_8,
                                    uint8_t *event_state_9,
                                    uint8_t *event_state_10,
                                    uint8_t *event_state_11,
                                    uint8_t *event_state_12,
                                    uint8_t *event_state_13,
                                    uint8_t *event_state_14);

typedef int (*Sdr_threshold_event_flags_func)(ipmi_sdr_ctx_t ctx,
					      const void *sdr_record,
					      unsigned int sdr_record_len,
                                              uint8_t *lower_non_critical_going_low,
                                              uint8_t *lower_non_critical_going_high,
                                              uint8_t *lower_critical_going_low,
                                              uint8_t *lower_critical_going_high,
                                              uint8_t *lower_non_recoverable_going_low,
                                              uint8_t *lower_non_recoverable_going_high,
                                              uint8_t *upper_non_critical_going_low,
                                              uint8_t *upper_non_critical_going_high,
                                              uint8_t *upper_critical_going_low,
                                              uint8_t *upper_critical_going_high,
                                              uint8_t *upper_non_recoverable_going_low,
                                              uint8_t *upper_non_recoverable_going_high);

static ipmi_config_err_t
_get_sensor_event_enable (ipmi_config_state_data_t *state_data,
                          const char *section_name,
                          struct sensor_event_enable_data *data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t sensor_number;
  int field_flag;
  uint64_t val;

  assert (state_data);
  assert (section_name);
  assert (data);

  memset (data, '\0', sizeof (struct sensor_event_enable_data));

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sensor_event_enable_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sensor_event_enable (state_data->ipmi_ctx,
                                        sensor_number,
                                        obj_cmd_rs) < 0)
    {
      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_sensor_event_enable: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "all_event_messages", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'all_event_messages': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->all_event_messages = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "scanning_on_this_sensor", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'scanning_on_this_sensor': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  data->scanning_on_this_sensor = val;

  if (data->all_event_messages == IPMI_SENSOR_ALL_EVENT_MESSAGES_DISABLE)
    {
      data->assertion_bits = 0;
      data->deassertion_bits = 0;
      goto out;
    }

  if ((field_flag = fiid_obj_get (obj_cmd_rs,
                                  "assertion_event_bitmask",
                                  &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'assertion_event_bitmask': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  /* assertion event bitmask need not be returned */
  if (field_flag)
    data->assertion_bits = val;
  else
    data->assertion_bits = 0;

  if ((field_flag = fiid_obj_get (obj_cmd_rs,
                                  "deassertion_event_bitmask",
                                  &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'deassertion_event_bitmask': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  /* deassertion event bitmask need not be returned */
  if (field_flag)
    data->deassertion_bits = val;
  else
    data->deassertion_bits = 0;

 out:
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_sensor_event_enable (ipmi_config_state_data_t *state_data,
                          const char *section_name,
                          struct sensor_event_enable_data *data,
                          uint8_t event_message_action)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t sensor_number;

  assert (state_data);
  assert (section_name);
  assert (data);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sensor_event_enable_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_sensor_event_enable (state_data->ipmi_ctx,
                                        sensor_number,
                                        event_message_action,
                                        data->scanning_on_this_sensor,
                                        data->all_event_messages,
                                        data->assertion_bits,
                                        data->deassertion_bits,
                                        obj_cmd_rs) < 0)
    {
      if (ipmi_errnum_is_non_fatal (state_data,
				    obj_cmd_rs,
				    &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_sensor_event_enable: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

ipmi_config_err_t
sensor_event_enable_enable_all_event_messages_checkout (ipmi_config_state_data_t *state_data,
							const char *section_name,
                                                        struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.all_event_messages ? "Yes" : "No") < 0)
    goto cleanup;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
sensor_event_enable_enable_all_event_messages_commit (ipmi_config_state_data_t *state_data,
						      const char *section_name,
                                                      const struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  /* clear bits just in case, we're not setting anything here */
  data.assertion_bits = 0;
  data.deassertion_bits = 0;

  data.all_event_messages = same (kv->value_input, "yes");

  if ((ret = _set_sensor_event_enable (state_data,
                                       section_name,
                                       &data,
                                       IPMI_SENSOR_EVENT_MESSAGE_ACTION_DO_NOT_CHANGE_INDIVIDUAL_ENABLES)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
sensor_event_enable_enable_scanning_on_this_sensor_checkout (ipmi_config_state_data_t *state_data,
							     const char *section_name,
                                                             struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  data.scanning_on_this_sensor ? "Yes" : "No") < 0)
    goto cleanup;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
sensor_event_enable_enable_scanning_on_this_sensor_commit (ipmi_config_state_data_t *state_data,
							   const char *section_name,
                                                           const struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       &data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  /* clear bits just in case, we're not setting anything here */
  data.assertion_bits = 0;
  data.deassertion_bits = 0;

  data.scanning_on_this_sensor = same (kv->value_input, "yes");

  if ((ret = _set_sensor_event_enable (state_data,
                                       section_name,
                                       &data,
                                       IPMI_SENSOR_EVENT_MESSAGE_ACTION_DO_NOT_CHANGE_INDIVIDUAL_ENABLES)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

int
_setup_event_enable_key (ipmi_config_state_data_t *state_data,
                         struct ipmi_config_section *section,
                         const char *key_name,
                         uint8_t event_supported,
                         Key_Checkout checkout_func,
                         Key_Commit commit_func)
{
  unsigned int flags = 0;

  assert (state_data);
  assert (section);
  assert (key_name);
  assert (checkout_func);
  assert (commit_func);

  if (event_supported
      || state_data->prog_data->args->verbose_count)
    {
      if (!event_supported)
        flags |= IPMI_CONFIG_UNDEFINED;

      if (ipmi_config_section_add_key (state_data,
				       section,
				       key_name,
				       "Possible values: Yes/No",
				       flags,
				       checkout_func,
				       commit_func,
				       yes_no_validate) < 0)
        goto cleanup;
    }

  return (0);

 cleanup:
  return (-1);
}

static ipmi_config_err_t
_threshold_event_enable_verify (ipmi_config_state_data_t *state_data,
                                const char *section_name)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t event_reading_type_code;
  int event_reading_type_code_class;

  assert (state_data);
  assert (section_name);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class != IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Attempting to access threshold event in non-threshold sensor\n");

      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
_threshold_event_enable_get_data (ipmi_config_state_data_t *state_data,
                                  const char *section_name,
                                  const struct ipmi_config_keyvalue *kv,
                                  struct sensor_event_enable_data *data,
                                  uint16_t *bits,
                                  uint8_t *bitposition)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  int found = 0;
  int i;

  assert (state_data);
  assert (section_name);
  assert (kv);
  assert (data);
  assert (bits);
  assert (bitposition);

  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (stristr (kv->key->key_name, "Deassertion"))
    (*bits) = data->deassertion_bits;
  else
    (*bits) = data->assertion_bits;

  i = 0;
  while (threshold_event_strings[i])
    {
      if (stristr (kv->key->key_name, threshold_event_strings[i]))
        {
          (*bitposition) = i;
          found++;
          break;
        }
      i++;
    }

  if (!found)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
threshold_event_enable_checkout (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  uint16_t bits = 0;
  uint8_t bitposition = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _threshold_event_enable_verify (state_data,
                                             section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = _threshold_event_enable_get_data (state_data,
                                               section_name,
                                               kv,
                                               &data,
                                               &bits,
                                               &bitposition)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  ((bits >> bitposition) & 0x1) ? "Yes" : "No") < 0)
    goto cleanup;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
threshold_event_enable_commit (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  uint16_t bits = 0;
  uint8_t bitposition = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t event_message_action;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _threshold_event_enable_verify (state_data,
                                             section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = _threshold_event_enable_get_data (state_data,
                                               section_name,
                                               kv,
                                               &data,
                                               &bits,
                                               &bitposition)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  /* due to set sensor event enable mechanics, we need to clear the bits */
  data.assertion_bits = 0;
  data.deassertion_bits = 0;

  if (stristr (kv->key->key_name, "Deassertion"))
    data.deassertion_bits = (0x1 << bitposition);
  else
    data.assertion_bits = (0x1 << bitposition);

  if (same (kv->value_input, "yes"))
    event_message_action = IPMI_SENSOR_EVENT_MESSAGE_ACTION_ENABLE_SELECTED_EVENT_MESSAGES;
  else
    event_message_action = IPMI_SENSOR_EVENT_MESSAGE_ACTION_DISABLE_SELECTED_EVENT_MESSAGES;

  if ((ret = _set_sensor_event_enable (state_data,
                                       section_name,
                                       &data,
                                       event_message_action)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static int
_setup_threshold_event_enable_wrapper (ipmi_config_state_data_t *state_data,
                                       struct ipmi_config_section *section,
                                       const char *type_str,
                                       Sdr_threshold_event_flags_func sdr_call)
{
  uint8_t lower_non_critical_going_low = 0;
  uint8_t lower_non_critical_going_high = 0;
  uint8_t lower_critical_going_low = 0;
  uint8_t lower_critical_going_high = 0;
  uint8_t lower_non_recoverable_going_low = 0;
  uint8_t lower_non_recoverable_going_high = 0;
  uint8_t upper_non_critical_going_low = 0;
  uint8_t upper_non_critical_going_high = 0;
  uint8_t upper_critical_going_low = 0;
  uint8_t upper_critical_going_high = 0;
  uint8_t upper_non_recoverable_going_low = 0;
  uint8_t upper_non_recoverable_going_high = 0;
  char key_name[KEY_NAME_MAX_LEN];
  uint16_t bitmask = 0;
  int rv = -1;
  int i;

  assert (state_data);
  assert (section);
  assert (type_str);
  assert (sdr_call);

  if (((*sdr_call)(state_data->sdr_ctx,
		   NULL,
		   0,
                   &lower_non_critical_going_low,
                   &lower_non_critical_going_high,
                   &lower_critical_going_low,
                   &lower_critical_going_high,
                   &lower_non_recoverable_going_low,
                   &lower_non_recoverable_going_high,
                   &upper_non_critical_going_low,
                   &upper_non_critical_going_high,
                   &upper_critical_going_low,
                   &upper_critical_going_high,
                   &upper_non_recoverable_going_low,
                   &upper_non_recoverable_going_high)) < 0)
    goto cleanup;

  /* create bitmask to make things easier */
  bitmask |= (lower_non_critical_going_low & 0x1);
  bitmask |= ((lower_non_critical_going_high & 0x1) << 1);
  bitmask |= ((lower_critical_going_low & 0x1) << 2);
  bitmask |= ((lower_critical_going_high & 0x1) << 3);
  bitmask |= ((lower_non_recoverable_going_low & 0x1) << 4);
  bitmask |= ((lower_non_recoverable_going_high & 0x1) << 5);
  bitmask |= ((upper_non_critical_going_low & 0x1) << 6);
  bitmask |= ((upper_non_critical_going_high & 0x1) << 7);
  bitmask |= ((upper_critical_going_low & 0x1) << 8);
  bitmask |= ((upper_critical_going_high & 0x1) << 9);
  bitmask |= ((upper_non_recoverable_going_low & 0x1) << 10);
  bitmask |= ((upper_non_recoverable_going_high & 0x1) << 11);

  i = 0;
  while (threshold_event_strings[i])
    {
      snprintf (key_name,
                KEY_NAME_MAX_LEN,
                "Enable_%s_Event_%s",
                type_str,
                threshold_event_strings[i]);

      if (_setup_event_enable_key (state_data,
                                   section,
                                   key_name,
                                   ((bitmask >> i) & 0x1),
                                   threshold_event_enable_checkout,
                                   threshold_event_enable_commit) < 0)
        goto cleanup;

      i++;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
_setup_threshold_event_enable (ipmi_config_state_data_t *state_data,
                               struct ipmi_config_section *section)
{
  int rv = -1;

  assert (state_data);
  assert (section);

  if (_setup_threshold_event_enable_wrapper (state_data,
                                             section,
                                             "Assertion",
                                             &ipmi_sdr_parse_threshold_assertion_supported) < 0)
    goto cleanup;

  if (_setup_threshold_event_enable_wrapper (state_data,
                                             section,
                                             "Deassertion",
                                             &ipmi_sdr_parse_threshold_deassertion_supported) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_get_event_state_bitmask (ipmi_config_state_data_t *state_data,
                          Sdr_event_flags_func sdr_call,
                          uint16_t *bitmask)
{
  uint8_t event_state_0 = 0;
  uint8_t event_state_1 = 0;
  uint8_t event_state_2 = 0;
  uint8_t event_state_3 = 0;
  uint8_t event_state_4 = 0;
  uint8_t event_state_5 = 0;
  uint8_t event_state_6 = 0;
  uint8_t event_state_7 = 0;
  uint8_t event_state_8 = 0;
  uint8_t event_state_9 = 0;
  uint8_t event_state_10 = 0;
  uint8_t event_state_11 = 0;
  uint8_t event_state_12 = 0;
  uint8_t event_state_13 = 0;
  uint8_t event_state_14 = 0;
  int rv = -1;

  assert (state_data);
  assert (sdr_call);
  assert (bitmask);

  if (((*sdr_call)(state_data->sdr_ctx,
		   NULL,
		   0,
                   &event_state_0,
                   &event_state_1,
                   &event_state_2,
                   &event_state_3,
                   &event_state_4,
                   &event_state_5,
                   &event_state_6,
                   &event_state_7,
                   &event_state_8,
                   &event_state_9,
                   &event_state_10,
                   &event_state_11,
                   &event_state_12,
                   &event_state_13,
                   &event_state_14)) < 0)
    goto cleanup;

  (*bitmask) = 0;
  (*bitmask) |= (event_state_0 & 0x1);
  (*bitmask) |= ((event_state_1 & 0x1) << 1);
  (*bitmask) |= ((event_state_2 & 0x1) << 2);
  (*bitmask) |= ((event_state_3 & 0x1) << 3);
  (*bitmask) |= ((event_state_4 & 0x1) << 4);
  (*bitmask) |= ((event_state_5 & 0x1) << 5);
  (*bitmask) |= ((event_state_6 & 0x1) << 6);
  (*bitmask) |= ((event_state_7 & 0x1) << 7);
  (*bitmask) |= ((event_state_8 & 0x1) << 8);
  (*bitmask) |= ((event_state_9 & 0x1) << 9);
  (*bitmask) |= ((event_state_10 & 0x1) << 10);
  (*bitmask) |= ((event_state_11 & 0x1) << 11);
  (*bitmask) |= ((event_state_12 & 0x1) << 12);
  (*bitmask) |= ((event_state_13 & 0x1) << 13);
  (*bitmask) |= ((event_state_14 & 0x1) << 14);

  rv = 0;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
_generic_event_enable_verify (ipmi_config_state_data_t *state_data,
                              const char *section_name,
                              uint8_t *event_reading_type_code_ptr)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t event_reading_type_code;
  int event_reading_type_code_class;

  assert (state_data);
  assert (section_name);
  assert (event_reading_type_code_ptr);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class != IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Attempting to access generic event in non-generic sensor\n");

      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  (*event_reading_type_code_ptr) = event_reading_type_code;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static char **
_generic_event_enable_get_event_strings (ipmi_config_state_data_t *state_data,
                                         uint8_t event_reading_type_code)
{
  assert (state_data);

  /* achu: I'm sorry.  But these fields have no description in the
   * IPMI spec, so there is no macro for them.  So I'm hard coding
   * hex in.  Please see see Table 42-2 in the IPMI spec.
   */

  switch (event_reading_type_code)
    {
    case 0x02:
      return (&generic_event_strings_0x02[0]);
    case 0x03:
      return (&generic_event_strings_0x03[0]);
    case 0x04:
      return (&generic_event_strings_0x04[0]);
    case 0x05:
      return (&generic_event_strings_0x05[0]);
    case 0x06:
      return (&generic_event_strings_0x06[0]);
    case 0x07:
      return (&generic_event_strings_0x07[0]);
    case 0x08:
      return (&generic_event_strings_0x08[0]);
    case 0x09:
      return (&generic_event_strings_0x09[0]);
    case 0x0A:
      return (&generic_event_strings_0x0A[0]);
    case 0x0B:
      return (&generic_event_strings_0x0B[0]);
    case 0x0C:
      return (&generic_event_strings_0x0C[0]);
    }

  return (NULL);
}

static ipmi_config_err_t
_generic_event_enable_get_data (ipmi_config_state_data_t *state_data,
                                const char *section_name,
                                const struct ipmi_config_keyvalue *kv,
                                uint8_t event_reading_type_code,
                                struct sensor_event_enable_data *data,
                                uint16_t *bits,
                                uint8_t *bitposition)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  char **event_strings = NULL;
  int found = 0;
  int i;

  assert (state_data);
  assert (section_name);
  assert (kv);
  assert (data);
  assert (bits);
  assert (bitposition);

  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (stristr (kv->key->key_name, "Deassertion"))
    (*bits) = data->deassertion_bits;
  else
    (*bits) = data->assertion_bits;

  event_strings = _generic_event_enable_get_event_strings (state_data,
                                                           event_reading_type_code);
  if (!event_strings)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unable to handle event flags for event reading type code 0x%X\n",
                        event_reading_type_code);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  i = 0;
  while (event_strings[i])
    {
      if (stristr (kv->key->key_name, event_strings[i]))
        {
          (*bitposition) = i;
          found++;
          break;
        }
      i++;
    }

  if (!found)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
generic_event_enable_checkout (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  uint16_t bits = 0;
  uint8_t bitposition = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t event_reading_type_code;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _generic_event_enable_verify (state_data,
                                           section_name,
                                           &event_reading_type_code)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = _generic_event_enable_get_data (state_data,
                                             section_name,
                                             kv,
                                             event_reading_type_code,
                                             &data,
                                             &bits,
                                             &bitposition)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  ((bits >> bitposition) & 0x1) ? "Yes" : "No") < 0)
    goto cleanup;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
generic_event_enable_commit (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  uint16_t bits = 0;
  uint8_t bitposition = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t event_reading_type_code;
  uint8_t event_message_action;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _generic_event_enable_verify (state_data,
                                           section_name,
                                           &event_reading_type_code)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = _generic_event_enable_get_data (state_data,
                                             section_name,
                                             kv,
                                             event_reading_type_code,
                                             &data,
                                             &bits,
                                             &bitposition)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  /* due to set sensor event enable mechanics, we need to clear the bits */
  data.assertion_bits = 0;
  data.deassertion_bits = 0;

  if (stristr (kv->key->key_name, "Deassertion"))
    data.deassertion_bits = (0x1 << bitposition);
  else
    data.assertion_bits = (0x1 << bitposition);

  if (same (kv->value_input, "yes"))
    event_message_action = IPMI_SENSOR_EVENT_MESSAGE_ACTION_ENABLE_SELECTED_EVENT_MESSAGES;
  else
    event_message_action = IPMI_SENSOR_EVENT_MESSAGE_ACTION_DISABLE_SELECTED_EVENT_MESSAGES;

  if ((ret = _set_sensor_event_enable (state_data,
                                       section_name,
                                       &data,
                                       event_message_action)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static int
_setup_generic_event_enable_wrapper (ipmi_config_state_data_t *state_data,
                                     struct ipmi_config_section *section,
                                     const char *type_str,
                                     Sdr_event_flags_func sdr_call,
                                     uint8_t event_reading_type_code)
{
  char key_name[KEY_NAME_MAX_LEN];
  char **event_strings = NULL;
  uint16_t bitmask = 0;
  int rv = -1;
  int i;

  assert (state_data);
  assert (section);
  assert (type_str);
  assert (sdr_call);

  if (_get_event_state_bitmask (state_data,
                                sdr_call,
                                &bitmask) < 0)
    goto cleanup;

  event_strings = _generic_event_enable_get_event_strings (state_data,
                                                           event_reading_type_code);
  if (!event_strings)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unable to handle event flags for event reading type code 0x%X\n",
                        event_reading_type_code);
      rv = 0;
      goto cleanup;
    }

  i = 0;
  while (event_strings[i])
    {
      snprintf (key_name,
                KEY_NAME_MAX_LEN,
                "Enable_%s_Event_%s",
                type_str,
                event_strings[i]);

      if (_setup_event_enable_key (state_data,
                                   section,
                                   key_name,
                                   ((bitmask >> i) & 0x1),
                                   generic_event_enable_checkout,
                                   generic_event_enable_commit) < 0)
        goto cleanup;

      i++;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
_setup_generic_event_enable (ipmi_config_state_data_t *state_data,
                             struct ipmi_config_section *section,
                             uint8_t event_reading_type_code)
{
  int rv = -1;

  assert (state_data);
  assert (section);

  if (_setup_generic_event_enable_wrapper (state_data,
                                           section,
                                           "Assertion",
                                           &ipmi_sdr_parse_assertion_supported,
                                           event_reading_type_code) < 0)
    goto cleanup;

  if (_setup_generic_event_enable_wrapper (state_data,
                                           section,
                                           "Deassertion",
                                           &ipmi_sdr_parse_deassertion_supported,
                                           event_reading_type_code) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static ipmi_config_err_t
_sensor_specific_event_enable_verify (ipmi_config_state_data_t *state_data,
                                      const char *section_name,
                                      uint8_t *sensor_type_ptr)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t event_reading_type_code;
  uint8_t sensor_type;
  int event_reading_type_code_class;

  assert (state_data);
  assert (section_name);
  assert (sensor_type_ptr);

  if ((ret = ipmi_config_sensors_seek_to_sdr_record (state_data,
						     section_name)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
					      NULL,
					      0,
                                              &event_reading_type_code) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_event_reading_type_code: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class != IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Attempting to access sensor-specific event in non-sensor-specific sensor\n");

      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				  NULL,
				  0,
                                  &sensor_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  (*sensor_type_ptr) = sensor_type;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static char **
_sensor_specific_event_enable_get_event_strings (ipmi_config_state_data_t *state_data,
                                                 uint8_t sensor_type)
{
  assert (state_data);

  switch (sensor_type)
    {
    case IPMI_SENSOR_TYPE_PHYSICAL_SECURITY:
      return (&sensor_specific_event_strings_physical_security_chassis_intrusion[0]);
    case IPMI_SENSOR_TYPE_PLATFORM_SECURITY_VIOLATION_ATTEMPT:
      return (&sensor_specific_event_strings_platform_security_violation_attempt[0]);
    case IPMI_SENSOR_TYPE_PROCESSOR:
      return (&sensor_specific_event_strings_processor[0]);
    case IPMI_SENSOR_TYPE_POWER_SUPPLY:
      return (&sensor_specific_event_strings_power_supply[0]);
    case IPMI_SENSOR_TYPE_POWER_UNIT:
      return (&sensor_specific_event_strings_power_unit[0]);
    case IPMI_SENSOR_TYPE_MEMORY:
      return (&sensor_specific_event_strings_memory[0]);
    case IPMI_SENSOR_TYPE_DRIVE_SLOT:
      return (&sensor_specific_event_strings_drive_slot[0]);
    case IPMI_SENSOR_TYPE_SYSTEM_FIRMWARE_PROGRESS:
      return (&sensor_specific_event_strings_system_firmware_progress[0]);
    case IPMI_SENSOR_TYPE_EVENT_LOGGING_DISABLED:
      return (&sensor_specific_event_strings_event_logging_disabled[0]);
    case IPMI_SENSOR_TYPE_SYSTEM_EVENT:
      return (&sensor_specific_event_strings_system_event[0]);
    case IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT:
      return (&sensor_specific_event_strings_critical_interrupt[0]);
    case IPMI_SENSOR_TYPE_CABLE_INTERCONNECT:
      return (&sensor_specific_event_strings_cable_interconnect[0]);
    case IPMI_SENSOR_TYPE_BOOT_ERROR:
      return (&sensor_specific_event_strings_boot_error[0]);
    case IPMI_SENSOR_TYPE_SLOT_CONNECTOR:
      return (&sensor_specific_event_strings_slot_connector[0]);
    case IPMI_SENSOR_TYPE_WATCHDOG2:
      return (&sensor_specific_event_strings_watchdog2[0]);
    case IPMI_SENSOR_TYPE_ENTITY_PRESENCE:
      return (&sensor_specific_event_strings_entity_presence[0]);
    case IPMI_SENSOR_TYPE_MANAGEMENT_SUBSYSTEM_HEALTH:
      return (&sensor_specific_event_strings_management_subsystem_health[0]);
    case IPMI_SENSOR_TYPE_BATTERY:
      return (&sensor_specific_event_strings_battery[0]);
    case IPMI_SENSOR_TYPE_FRU_STATE:
      return (&sensor_specific_event_strings_fru_state[0]);
    }

  return (NULL);
}

static ipmi_config_err_t
_sensor_specific_event_enable_get_data (ipmi_config_state_data_t *state_data,
                                        const char *section_name,
                                        const struct ipmi_config_keyvalue *kv,
                                        uint8_t sensor_type,
                                        struct sensor_event_enable_data *data,
                                        uint16_t *bits,
                                        uint8_t *bitposition)
{
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  char **event_strings = NULL;
  int found = 0;
  int i;

  assert (state_data);
  assert (section_name);
  assert (kv);
  assert (data);
  assert (bits);
  assert (bitposition);

  if ((ret = _get_sensor_event_enable (state_data,
                                       section_name,
                                       data)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (stristr (kv->key->key_name, "Deassertion"))
    (*bits) = data->deassertion_bits;
  else
    (*bits) = data->assertion_bits;

  event_strings = _sensor_specific_event_enable_get_event_strings (state_data,
                                                                   sensor_type);
  if (!event_strings)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unable to handle event flags for sensor type 0x%X\n",
                        sensor_type);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  i = 0;

  /* special case - event state 4-7 are "reserved" and useless */
  if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
    {
      while (sensor_specific_event_strings_watchdog2_indexes[i] >= 0)
        {
          if (stristr (kv->key->key_name,
                       event_strings[sensor_specific_event_strings_watchdog2_indexes[i]]))
            {
              (*bitposition) = sensor_specific_event_strings_watchdog2_indexes[i];
              found++;
              break;
            }
          i++;
        }
    }
  else
    {
      while (event_strings[i])
        {
          if (stristr (kv->key->key_name, event_strings[i]))
            {
              (*bitposition) = i;
              found++;
              break;
            }
          i++;
        }
    }

  if (!found)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unrecognized section:key_name: %s:%s\n",
                        section_name,
                        kv->key->key_name);
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
sensor_specific_event_enable_checkout (ipmi_config_state_data_t *state_data,
				       const char *section_name,
                                       struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  uint16_t bits = 0;
  uint8_t bitposition = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t sensor_type;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _sensor_specific_event_enable_verify (state_data,
                                                   section_name,
                                                   &sensor_type)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = _sensor_specific_event_enable_get_data (state_data,
                                                     section_name,
                                                     kv,
                                                     sensor_type,
                                                     &data,
                                                     &bits,
                                                     &bitposition)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
						  kv,
						  ((bits >> bitposition) & 0x1) ? "Yes" : "No") < 0)
    goto cleanup;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

ipmi_config_err_t
sensor_specific_event_enable_commit (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     const struct ipmi_config_keyvalue *kv)
{
  struct sensor_event_enable_data data;
  uint16_t bits = 0;
  uint8_t bitposition = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t sensor_type;
  uint8_t event_message_action;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _sensor_specific_event_enable_verify (state_data,
                                                   section_name,
                                                   &sensor_type)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = _sensor_specific_event_enable_get_data (state_data,
                                                     section_name,
                                                     kv,
                                                     sensor_type,
                                                     &data,
                                                     &bits,
                                                     &bitposition)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  /* due to set sensor event enable mechanics, we need to clear the bits */
  data.assertion_bits = 0;
  data.deassertion_bits = 0;

  if (stristr (kv->key->key_name, "Deassertion"))
    data.deassertion_bits = (0x1 << bitposition);
  else
    data.assertion_bits = (0x1 << bitposition);

  if (same (kv->value_input, "yes"))
    event_message_action = IPMI_SENSOR_EVENT_MESSAGE_ACTION_ENABLE_SELECTED_EVENT_MESSAGES;
  else
    event_message_action = IPMI_SENSOR_EVENT_MESSAGE_ACTION_DISABLE_SELECTED_EVENT_MESSAGES;

  if ((ret = _set_sensor_event_enable (state_data,
                                       section_name,
                                       &data,
                                       event_message_action)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}

static int
_setup_sensor_specific_event_enable_wrapper (ipmi_config_state_data_t *state_data,
                                             struct ipmi_config_section *section,
                                             const char *type_str,
                                             Sdr_event_flags_func sdr_call)
{
  char key_name[KEY_NAME_MAX_LEN];
  char **event_strings = NULL;
  uint16_t bitmask = 0;
  uint8_t sensor_type;
  int rv = -1;
  int i;

  assert (state_data);
  assert (section);
  assert (type_str);
  assert (sdr_call);

  if (_get_event_state_bitmask (state_data,
                                sdr_call,
                                &bitmask) < 0)
    goto cleanup;

  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				  NULL,
				  0,
                                  &sensor_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  event_strings = _sensor_specific_event_enable_get_event_strings (state_data,
                                                                   sensor_type);
  if (!event_strings)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_printf (state_data->pstate,
                        "## Unable to handle event flags for sensor type 0x%X\n",
                        sensor_type);
      rv = 0;
      goto cleanup;
    }

  i = 0;

  /* special case - event state 4-7 are "reserved" and useless */
  if (sensor_type == IPMI_SENSOR_TYPE_WATCHDOG2)
    {
      while (sensor_specific_event_strings_watchdog2_indexes[i] >= 0)
        {
          snprintf (key_name,
                    KEY_NAME_MAX_LEN,
                    "Enable_%s_Event_%s",
                    type_str,
                    event_strings[sensor_specific_event_strings_watchdog2_indexes[i]]);

          if (_setup_event_enable_key (state_data,
                                       section,
                                       key_name,
                                       ((bitmask >> sensor_specific_event_strings_watchdog2_indexes[i]) & 0x1),
                                       sensor_specific_event_enable_checkout,
                                       sensor_specific_event_enable_commit) < 0)
            goto cleanup;

          i++;
        }
    }
  else
    {
      i = 0;
      while (event_strings[i])
        {
          snprintf (key_name,
                    KEY_NAME_MAX_LEN,
                    "Enable_%s_Event_%s",
                    type_str,
                    event_strings[i]);

          if (_setup_event_enable_key (state_data,
                                       section,
                                       key_name,
                                       ((bitmask >> i) & 0x1),
                                       sensor_specific_event_enable_checkout,
                                       sensor_specific_event_enable_commit) < 0)
            goto cleanup;

          i++;
        }
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
_setup_sensor_specific_event_enable (ipmi_config_state_data_t *state_data,
                                     struct ipmi_config_section *section,
                                     uint8_t event_reading_type_code)
{
  int rv = -1;

  assert (state_data);
  assert (section);

  if (_setup_sensor_specific_event_enable_wrapper (state_data,
                                                   section,
                                                   "Assertion",
                                                   &ipmi_sdr_parse_assertion_supported) < 0)
    goto cleanup;

  if (_setup_sensor_specific_event_enable_wrapper (state_data,
                                                   section,
                                                   "Deassertion",
                                                   &ipmi_sdr_parse_deassertion_supported) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
setup_sensor_event_enable_fields (ipmi_config_state_data_t *state_data,
                                  struct ipmi_config_section *section)
{
  uint8_t event_message_control_support = 0;
  int rv = -1;

  assert (state_data);
  assert (section);

  if (ipmi_sdr_parse_sensor_capabilities (state_data->sdr_ctx,
					  NULL,
					  0,
                                          &event_message_control_support,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_capabilities: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  /* achu: I'm not quite sure what IPMI_SDR_GLOBAL_DISABLE_ONLY means.
   * Does it mean all sensors on the motherboard can be disabled
   * together? But one alone cannot?  I'm going to assume that's what
   * it means. I'll have to come back to this if that's not the case.
   */
  if (event_message_control_support == IPMI_SDR_PER_EVENT_ENABLE_DISABLE_SUPPORT
      || event_message_control_support == IPMI_SDR_ENTIRE_SENSOR_ONLY)
    {
      if (ipmi_config_section_add_key (state_data,
				       section,
				       "Enable_All_Event_Messages",
				       "Possible values: Yes/No",
				       0,
				       sensor_event_enable_enable_all_event_messages_checkout,
				       sensor_event_enable_enable_all_event_messages_commit,
				       yes_no_validate) < 0)
        goto cleanup;

      if (ipmi_config_section_add_key (state_data,
				       section,
				       "Enable_Scanning_On_This_Sensor",
				       "Possible values: Yes/No",
				       0,
				       sensor_event_enable_enable_scanning_on_this_sensor_checkout,
				       sensor_event_enable_enable_scanning_on_this_sensor_commit,
				       yes_no_validate) < 0)
        goto cleanup;
    }

  if (event_message_control_support == IPMI_SDR_PER_EVENT_ENABLE_DISABLE_SUPPORT)
    {
      uint8_t event_reading_type_code;
      int event_reading_type_code_class;

      if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
						  NULL,
						  0,
                                                  &event_reading_type_code) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_event_reading_type_code: %s\n",
                           ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          goto cleanup;
        }

      event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

      if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
        {
          if (_setup_threshold_event_enable (state_data,
                                             section) < 0)
            goto cleanup;
        }
      else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE)
        {
          if (_setup_generic_event_enable (state_data,
                                           section,
                                           event_reading_type_code) < 0)
            goto cleanup;
        }
      else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
        {
          if (_setup_sensor_specific_event_enable (state_data,
                                                   section,
                                                   event_reading_type_code) < 0)
            goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  return (rv);
}
