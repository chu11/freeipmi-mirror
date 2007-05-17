#ifndef _IPMI_PEF_UTILS_H
#define _IPMI_PEF_UTILS_H

#define FILTER_NUMBER_KEY_STRING    "Filter_Number"
#define FILTER_TYPE_KEY_STRING      "Filter_Type"
#define ENABLE_FILTER_KEY_STRING    "Enable_Filter"
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

#define APT_ALERT_POLICY_NUMBER_KEY_STRING            "Alert_Policy_Number"
#define APT_POLICY_TYPE_KEY_STRING                    "Policy_Type"
#define APT_POLICY_ENABLED_KEY_STRING                 "Policy_Enabled"
#define APT_POLICY_NUMBER_KEY_STRING                  "Policy_Number"
#define APT_DESTINATION_SELECTOR_KEY_STRING           "Destination_Selector"
#define APT_CHANNEL_NUMBER_KEY_STRING                 "Channel_Number"
#define APT_ALERT_STRING_SET_SELECTOR_KEY_STRING      "Alert_String_Set_Selector"
#define APT_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_KEY_STRING    "Event_Specific_Alert_String"

#define LAD_ALERT_DESTINATION_SELECTOR_KEY_STRING       "Alert_Destination_Selector"
#define LAD_ALERT_DESTINATION_TYPE_KEY_STRING       "Alert_Destination_Type"
#define LAD_ALERT_ACKNOWLEDGE_KEY_STRING            "Alert_Acknowledge"
#define LAD_ALERT_ACKNOWLEDGE_TIMEOUT_KEY_STRING    "Alert_Acknowledge_Timeout"
#define LAD_ALERT_RETRIES_KEY_STRING                "Alert_Retries"
#define LAD_ALERT_GATEWAY_KEY_STRING                "Alert_Gateway"
#define LAD_ALERT_IP_ADDRESS_KEY_STRING             "Alert_IP_Address"
#define LAD_ALERT_MAC_ADDRESS_KEY_STRING            "Alert_MAC_Address"

#define COMMUNITY_STRING_KEY_STRING                 "Community_String"

int strchr_replace (char *str, char ch, char nch);
char *filter_number_to_string (int filter_number);
int string_to_filter_number (const char *filter_number_string);
char *filter_type_to_string (int filter_type);
int string_to_filter_type (const char *filter_type_string);
char *enable_filter_to_string (int enable_filter);
int string_to_enable_filter (const char *enable_filter_string);
char *event_filter_action_alert_to_string (int event_filter_action_alert);
int string_to_event_filter_action_alert (const char *event_filter_action_alert_string);
char *event_filter_action_power_off_to_string (int event_filter_action_power_off);
int string_to_event_filter_action_power_off (const char *event_filter_action_power_off_string);
char *event_filter_action_reset_to_string (int event_filter_action_reset);
int string_to_event_filter_action_reset (const char *event_filter_action_reset_string);
char *event_filter_action_power_cycle_to_string (int event_filter_action_power_cycle);
int string_to_event_filter_action_power_cycle (const char *event_filter_action_power_cycle_string);
char *event_filter_action_oem_to_string (int event_filter_action_oem);
int string_to_event_filter_action_oem (const char *event_filter_action_oem_string);
char *event_filter_action_diagnostic_interrupt_to_string (int event_filter_action_diagnostic_interrupt);
int string_to_event_filter_action_diagnostic_interrupt (const char *event_filter_action_diagnostic_interrupt_string);
char *event_filter_action_group_control_operation_to_string (int event_filter_action_group_control_operation);
int string_to_event_filter_action_group_control_operation (const char *event_filter_action_group_control_operation_string);
char *alert_policy_number_to_string (int alert_policy_number);
int string_to_alert_policy_number (const char *alert_policy_number_string);
char *group_control_selector_to_string (int group_control_selector);
int string_to_group_control_selector (const char *group_control_selector_string);
char *event_severity_to_string (int event_severity);
int string_to_event_severity (const char *event_severity_string);
char *generator_id_byte1_to_string (int generator_id_byte1);
int string_to_generator_id_byte1 (const char *generator_id_byte1_string);
char *generator_id_byte2_to_string (int generator_id_byte2);
int string_to_generator_id_byte2 (const char *generator_id_byte2_string);
char *sensor_type_to_string (int sensor_type);
int string_to_sensor_type (const char *sensor_type_string);
char *sensor_number_to_string (int sensor_number);
int string_to_sensor_number (const char *sensor_number_string);
char *event_trigger_to_string (int event_trigger);
int string_to_event_trigger (const char *event_trigger_string);
char *event_data1_offset_mask_to_string (int event_data1_offset_mask);
int string_to_event_data1_offset_mask (const char *event_data1_offset_mask_string);
char *event_data1_AND_mask_to_string (int event_data1_AND_mask);
int string_to_event_data1_AND_mask (const char *event_data1_AND_mask_string);
char *event_data1_compare1_to_string (int event_data1_compare1);
int string_to_event_data1_compare1 (const char *event_data1_compare1_string);
char *event_data1_compare2_to_string (int event_data1_compare2);
int string_to_event_data1_compare2 (const char *event_data1_compare2_string);
char *event_data2_AND_mask_to_string (int event_data2_AND_mask);
int string_to_event_data2_AND_mask (const char *event_data2_AND_mask_string);
char *event_data2_compare1_to_string (int event_data2_compare1);
int string_to_event_data2_compare1 (const char *event_data2_compare1_string);
char *event_data2_compare2_to_string (int event_data2_compare2);
int string_to_event_data2_compare2 (const char *event_data2_compare2_string);
char *event_data3_AND_mask_to_string (int event_data3_AND_mask);
int string_to_event_data3_AND_mask (const char *event_data3_AND_mask_string);
char *event_data3_compare1_to_string (int event_data3_compare1);
int string_to_event_data3_compare1 (const char *event_data3_compare1_string);
char *event_data3_compare2_to_string (int event_data3_compare2);
int string_to_event_data3_compare2 (const char *event_data3_compare2_string);

char *policy_type_to_string (int policy_type);
int string_to_policy_type (const char *policy_type_string);
char *policy_enabled_to_string (int policy_enabled);
int string_to_policy_enabled (const char *policy_enabled_string);
char *policy_number_to_string (int policy_number);
int string_to_policy_number (const char *policy_number_string);
char *destination_selector_to_string (int destination_selector);
int string_to_destination_selector (const char *destination_selector_string);
char *channel_number_to_string (int channel_number);
int string_to_channel_number (const char *channel_number_string);
char *alert_string_set_selector_to_string (int alert_string_set_selector);
int string_to_alert_string_set_selector (const char *alert_string_set_selector_string);
char *event_specific_alert_string_lookup_to_string (int event_specific_alert_string_lookup);
int string_to_event_specific_alert_string_lookup (const char *event_specific_alert_string_lookup_string);

char *destination_type_to_string (int alert_destination_type);
int string_to_destination_type (const char *alert_destination_type_string);
char *alert_acknowledge_to_string (int alert_acknowledge);
int string_to_alert_acknowledge (const char *alert_acknowledge_string);
char *alert_acknowledge_timeout_to_string (int alert_acknowledge_timeout);
int string_to_alert_acknowledge_timeout (const char *alert_acknowledge_timeout_string);
char *alert_retries_to_string (int alert_retries);
int string_to_alert_retries (const char *alert_retries_string);
char *gateway_selector_to_string (int alert_gateway);
int string_to_gateway_selector (const char *alert_gateway_string);
char *alert_ip_address_to_string (const char *alert_ip_address);
int string_to_alert_ip_address (const char *alert_ip_address_string, char **alert_ip_address);
char *alert_mac_address_to_string (const char *alert_mac_address);
int string_to_alert_mac_address (const char *alert_mac_address_string, char **alert_mac_address);

#endif
