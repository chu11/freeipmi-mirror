#ifndef _IPMI_PEF_UTILS_H
#define _IPMI_PEF_UTILS_H

#define FILTER_NUMBER_KEY_STRING    "Filter_Number"
#define FILTER_TYPE_KEY_STRING      "Filter_Type"
#define ENBALE_FILTER_KEY_STRING    "Enable_Filter"
#define EVENT_FILTER_ACTION_ALERT_KEY_STRING          "Event_filter_Action_Alert"
#define EVENT_FILTER_ACTION_POWER_OFF_KEY_STRING      "Event_Filter_Action_Power_Off"
#define EVENT_FILTER_ACTION_RESET_KEY_STRING          "Event_Filter_Action_Reset"
#define EVENT_FILTER_ACTION_POWER_CYCLE_KEY_STRING    "Event_Filter_Action_Power_Cycle"
#define EVENT_FILTER_ACTION_OEM_KEY_STRING            "Event_Filter_Action_OEM"
#define EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_KEY_STRING    "Event_Filter_Action_Diagnostic_Interrupt"
#define EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_KEY_STRING "Event_Filter_Action_Group_Control_Operation"
#define ALERT_POLICY_NUMBER_KEY_STRING        "Alert_Policy_Number"
#define GROUP_CONTROL_SELECTOR_KEY_STRING     "Group_Control_Selector"
#define EVENT_SEVERITY_KEY_STRING             "Event_Severity"
#define GENERATOR_ID_BYTE1_KEY_STRING         "Generator_ID_Byte1"
#define GENERATOR_ID_BYTE2_KEY_STRING         "Generator_ID_Byte2"
#define SENSOR_TYPE_KEY_STRING                "Sensor_Type"
#define SENSOR_NUMBER_KEY_STRING              "Sensor_Number"
#define EVENT_TRIGGER_KEY_STRING              "Event_Trigger"
#define EVENT_DATA1_OFFSET_MASK_KEY_STRING    "Event_Data1_Offset_Mask"
#define EVENT_DATA1_AND_MASK_KEY_STRING       "Event_Data1_AND_Mask"
#define EVENT_DATA1_COMPARE1_KEY_STRING       "Event_Data1_Compare1"
#define EVENT_DATA1_COMPARE2_KEY_STRING       "Event_Data1_Compare2"
#define EVENT_DATA2_AND_MASK_KEY_STRING       "Event_Data2_AND_Mask"
#define EVENT_DATA2_COMPARE1_KEY_STRING       "Event_Data2_Compare1"
#define EVENT_DATA2_COMPARE2_KEY_STRING       "Event_Data2_Compare2"
#define EVENT_DATA3_AND_MASK_KEY_STRING       "Event_Data3_AND_Mask"
#define EVENT_DATA3_COMPARE1_KEY_STRING       "Event_Data3_Compare1"
#define EVENT_DATA3_COMPARE2_KEY_STRING       "Event_Data3_Compare2"

int strchr_replace (char *str, char ch, char nch);
int filter_number_to_string (int filter_number, char **filter_number_string);
int string_to_filter_number (const char *filter_number_string, int *filter_number);
int filter_type_to_string (int filter_type, char **filter_type_string);
int string_to_filter_type (const char *filter_type_string, int *filter_type);
int enable_filter_to_string (int enable_filter, char **enable_filter_string);
int string_to_enable_filter (const char *enable_filter_string, int *enable_filter);
int event_filter_action_alert_to_string (int event_filter_action_alert, 
					 char **event_filter_action_alert_string);
int string_to_event_filter_action_alert (const char *event_filter_action_alert_string, 
					 int *event_filter_action_alert);
int event_filter_action_power_off_to_string (int event_filter_action_power_off, 
					     char **event_filter_action_power_off_string);
int string_to_event_filter_action_power_off (const char *event_filter_action_power_off_string, 
					     int *event_filter_action_power_off);
int event_filter_action_reset_to_string (int event_filter_action_reset, 
					 char **event_filter_action_reset_string);
int string_to_event_filter_action_reset (const char *event_filter_action_reset_string, 
					 int *event_filter_action_reset);
int event_filter_action_power_cycle_to_string (int event_filter_action_power_cycle, 
					       char **event_filter_action_power_cycle_string);
int string_to_event_filter_action_power_cycle (const char *event_filter_action_power_cycle_string, 
					       int *event_filter_action_power_cycle);
int event_filter_action_oem_to_string (int event_filter_action_oem, 
				       char **event_filter_action_oem_string);
int string_to_event_filter_action_oem (const char *event_filter_action_oem_string, 
				       int *event_filter_action_oem);
int event_filter_action_diagnostic_interrupt_to_string (int event_filter_action_diagnostic_interrupt, 
							char **event_filter_action_diagnostic_interrupt_string);
int string_to_event_filter_action_diagnostic_interrupt (const char *event_filter_action_diagnostic_interrupt_string, 
							int *event_filter_action_diagnostic_interrupt);
int event_filter_action_group_control_operation_to_string (int event_filter_action_group_control_operation, 
							   char **event_filter_action_group_control_operation_string);
int string_to_event_filter_action_group_control_operation (const char *event_filter_action_group_control_operation_string, 
							   int *event_filter_action_group_control_operation);
int alert_policy_number_to_string (int alert_policy_number, 
				   char **alert_policy_number_string);
int string_to_alert_policy_number (const char *alert_policy_number_string, 
				   int *alert_policy_number);
int group_control_selector_to_string (int group_control_selector, 
				      char **group_control_selector_string);
int string_to_group_control_selector (const char *group_control_selector_string, 
				      int *group_control_selector);
int event_severity_to_string (int event_severity, char **event_severity_string);
int string_to_event_severity (const char *event_severity_string, int *event_severity);
int generator_id_byte1_to_string (int generator_id_byte1, 
				  char **generator_id_byte1_string);
int string_to_generator_id_byte1 (const char *generator_id_byte1_string, 
				  int *generator_id_byte1);
int generator_id_byte2_to_string (int generator_id_byte2, 
				  char **generator_id_byte2_string);
int string_to_generator_id_byte2 (const char *generator_id_byte2_string, 
				  int *generator_id_byte2);
int sensor_type_to_string (int sensor_type, char **sensor_type_string);
int string_to_sensor_type (const char *sensor_type_string, int *sensor_type);
int sensor_number_to_string (int sensor_number, char **sensor_number_string);
int string_to_sensor_number (const char *sensor_number_string, int *sensor_number);
int event_trigger_to_string (int event_trigger, char **event_trigger_string);
int string_to_event_trigger (const char *event_trigger_string, int *event_trigger);
int event_data1_offset_mask_to_string (int event_data1_offset_mask, 
				       char **event_data1_offset_mask_string);
int string_to_event_data1_offset_mask (const char *event_data1_offset_mask_string, 
				       int *event_data1_offset_mask);
int event_data1_AND_mask_to_string (int event_data1_AND_mask, 
				    char **event_data1_AND_mask_string);
int string_to_event_data1_AND_mask (const char *event_data1_AND_mask_string, 
				    int *event_data1_AND_mask);
int event_data1_compare1_to_string (int event_data1_compare1, 
				    char **event_data1_compare1_string);
int string_to_event_data1_compare1 (const char *event_data1_compare1_string, 
				    int *event_data1_compare1);
int event_data1_compare2_to_string (int event_data1_compare2, 
				    char **event_data1_compare2_string);
int string_to_event_data1_compare2 (const char *event_data1_compare2_string, 
				    int *event_data1_compare2);
int event_data2_AND_mask_to_string (int event_data2_AND_mask, 
				    char **event_data2_AND_mask_string);
int string_to_event_data2_AND_mask (const char *event_data2_AND_mask_string, 
				    int *event_data2_AND_mask);
int event_data2_compare1_to_string (int event_data2_compare1, 
				    char **event_data2_compare1_string);
int string_to_event_data2_compare1 (const char *event_data2_compare1_string, 
				    int *event_data2_compare1);
int event_data2_compare2_to_string (int event_data2_compare2, 
				    char **event_data2_compare2_string);
int string_to_event_data2_compare2 (const char *event_data2_compare2_string, 
				    int *event_data2_compare2);
int event_data3_AND_mask_to_string (int event_data3_AND_mask, 
				    char **event_data3_AND_mask_string);
int string_to_event_data3_AND_mask (const char *event_data3_AND_mask_string, 
				    int *event_data3_AND_mask);
int event_data3_compare1_to_string (int event_data3_compare1, 
				    char **event_data3_compare1_string);
int string_to_event_data3_compare1 (const char *event_data3_compare1_string, 
				    int *event_data3_compare1);
int event_data3_compare2_to_string (int event_data3_compare2, 
				    char **event_data3_compare2_string);
int string_to_event_data3_compare2 (const char *event_data3_compare2_string, 
				    int *event_data3_compare2);

#endif
