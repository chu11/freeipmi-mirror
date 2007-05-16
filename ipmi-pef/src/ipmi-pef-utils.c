
#include <string.h>
#include <arpa/inet.h>

#include "freeipmi/ipmi-sensor-types-spec.h"
#include "freeipmi/ipmi-pef-and-alerting-cmds.h"
#include "freeipmi/ipmi-lan-cmds.h"
#include "freeipmi-portability.h"

#include "common-utils.h"
#include "ipmi-sensor-api.h"

#include "ipmi-pef-utils.h"

#define IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER_STRING    "Manufacturer_Pre_Configured"
#define IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER_STRING          "Software_Configurable"
#define IPMI_FILTER_CONFIGURATION_RESERVED_STRING                              "Reserved"

#define IPMI_EVENT_SEVERITY_UNSPECIFIED_STRING                "Unspecified"
#define IPMI_EVENT_SEVERITY_MONITOR_STRING                    "Monitor"
#define IPMI_EVENT_SEVERITY_INFORMATION_STRING                "Information"
#define IPMI_EVENT_SEVERITY_OK_STRING                         "OK"
#define IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION_STRING     "Non_Critical"
#define IPMI_EVENT_SEVERITY_CRITICAL_CONDITION_STRING         "Critical"
#define IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION_STRING  "Non_Recoverable"

#define GENERATOR_ID_BYTE1_MATCH_ANY_STRING    "Match_Any"
#define GENERATOR_ID_BYTE1_MATCH_ANY           0xFF

#define GENERATOR_ID_BYTE2_MATCH_ANY_STRING    "Match_Any"
#define GENERATOR_ID_BYTE2_MATCH_ANY           0xFF

#define SENSOR_TYPE_MATCH_ANY_STRING    "Match_Any"
#define SENSOR_TYPE_MATCH_ANY           0xFF

#define SENSOR_NUMBER_MATCH_ANY_STRING    "Match_Any"
#define SENSOR_NUMBER_MATCH_ANY           0xFF

#define EVENT_TRIGGER_MATCH_ANY_STRING    "Match_Any"
#define EVENT_TRIGGER_MATCH_ANY           0xFF

#define IPMI_SENSOR_CLASS_UNSPECIFIED_STRING                 "Unspecified"
#define IPMI_SENSOR_CLASS_THRESHOLD_STRING                   "Threshold"
#define IPMI_SENSOR_CLASS_GENERIC_DISCRETE_STRING            "Generic_Discrete"
#define IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE_STRING    "Sensor_Specific_Discrete"
#define IPMI_SENSOR_CLASS_OEM_STRING                         "OEM"

#define YES_VALUE_STRING    "Yes"
#define YES_VALUE           1
#define NO_VALUE_STRING     "No"
#define NO_VALUE            0

#define IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION_STRING    "Always_Send_To_This_Destination"
#define IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_STRING              "Proceed_To_Next_Entry"
#define IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES_STRING    "Do_Not_Proceed_Any_More_Entries"
#define IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL_STRING    "Proceed_To_Next_Entry_Different_Channel"
#define IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE_STRING    "Proceed_To_Next_Entry_Different_Destination_Type"
#define IPMI_ALERT_POLICY_DISABLED_STRING    "No"
#define IPMI_ALERT_POLICY_ENABLED_STRING     "Yes"
#define IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO_STRING     "No"
#define IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES_STRING    "Yes"

#define IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION_STRING      "PET_Trap"
#define IPMI_DESTINATION_TYPE_OEM1_STRING                      "OEM1"
#define IPMI_DESTINATION_TYPE_OEM2_STRING                      "OEM2"
#define IPMI_ALERT_UNACKNOWLEDGED_STRING    "No"
#define IPMI_ALERT_ACKNOWLEDGED_STRING      "Yes"
#define IPMI_GATEWAY_SELECTOR_DEFAULT_STRING    "Default"
#define IPMI_GATEWAY_SELECTOR_BACKUP_STRING     "Backup"

int 
strchr_replace (char *str, char ch, char nch)
{
  int i = 0;
  
  if (!str)
    return -1;
  
  for (i = 0; str[i]; i++)
    {
      if (str[i] == ch)
	str[i] = nch;
    }
  
  return 0;
}

int 
filter_number_to_string (int filter_number, char **filter_number_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", filter_number);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *filter_number_string = str;
  return 0;
}

int 
string_to_filter_number (const char *filter_number_string, int *filter_number)
{
  int i = 0;
  
  if (str2int ((char *) filter_number_string, 16, &i) == 0)
    {
      *filter_number = i;
      return 0;
    }
  
  return -1;
}

int 
filter_type_to_string (int filter_type, char **filter_type_string)
{
  switch (filter_type)
    {
    case IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER:
      if (!(*filter_type_string = strdup (IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER:
      if (!(*filter_type_string = strdup (IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER_STRING))) 
        {
          perror("strdup");
          return -1;
        }
      return 0;
    default:
      if (!(*filter_type_string = strdup (IPMI_FILTER_CONFIGURATION_RESERVED_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

int 
string_to_filter_type (const char *filter_type_string, int *filter_type)
{
  if (strcasecmp (filter_type_string, 
		  IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER_STRING) == 0)
    {
      *filter_type = IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER;
      return 0;
    }
  
  if (strcasecmp (filter_type_string, 
		  IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER_STRING) == 0)
    {
      *filter_type = IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER;
      return 0;
    }
  
  return -1;
}

int 
enable_filter_to_string (int enable_filter, char **enable_filter_string)
{
  if (enable_filter)
    {
      if (!(*enable_filter_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*enable_filter_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_enable_filter (const char *enable_filter_string, int *enable_filter)
{
  if (strcasecmp (enable_filter_string, YES_VALUE_STRING) == 0)
    {
      *enable_filter = 1;
      return 0;
    }
  
  if (strcasecmp (enable_filter_string, NO_VALUE_STRING) == 0)
    {
      *enable_filter = 0;
      return 0;
    }
  
  return -1;
}

int 
event_filter_action_alert_to_string (int event_filter_action_alert, 
				     char **event_filter_action_alert_string)
{
  if (event_filter_action_alert)
    {
      if (!(*event_filter_action_alert_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*event_filter_action_alert_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_event_filter_action_alert (const char *event_filter_action_alert_string, 
				     int *event_filter_action_alert)
{
  if (strcasecmp (event_filter_action_alert_string, YES_VALUE_STRING) == 0)
    {
      *event_filter_action_alert = 1;
      return 0;
    }
  
  if (strcasecmp (event_filter_action_alert_string, NO_VALUE_STRING) == 0)
    {
      *event_filter_action_alert = 0;
      return 0;
    }
  
  return -1;
}

int 
event_filter_action_power_off_to_string (int event_filter_action_power_off, 
					 char **event_filter_action_power_off_string)
{
  if (event_filter_action_power_off)
    {
      if (!(*event_filter_action_power_off_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*event_filter_action_power_off_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_event_filter_action_power_off (const char *event_filter_action_power_off_string, 
					 int *event_filter_action_power_off)
{
  if (strcasecmp (event_filter_action_power_off_string, YES_VALUE_STRING) == 0)
    {
      *event_filter_action_power_off = 1;
      return 0;
    }
  
  if (strcasecmp (event_filter_action_power_off_string, NO_VALUE_STRING) == 0)
    {
      *event_filter_action_power_off = 0;
      return 0;
    }
  
  return -1;
}

int 
event_filter_action_reset_to_string (int event_filter_action_reset, 
				     char **event_filter_action_reset_string)
{
  if (event_filter_action_reset)
    {
      if (!(*event_filter_action_reset_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*event_filter_action_reset_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_event_filter_action_reset (const char *event_filter_action_reset_string, 
				     int *event_filter_action_reset)
{
  if (strcasecmp (event_filter_action_reset_string, YES_VALUE_STRING) == 0)
    {
      *event_filter_action_reset = 1;
      return 0;
    }
  
  if (strcasecmp (event_filter_action_reset_string, NO_VALUE_STRING) == 0)
    {
      *event_filter_action_reset = 0;
      return 0;
    }
  
  return -1;
}

int 
event_filter_action_power_cycle_to_string (int event_filter_action_power_cycle, 
					   char **event_filter_action_power_cycle_string)
{
  if (event_filter_action_power_cycle)
    {
      if (!(*event_filter_action_power_cycle_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*event_filter_action_power_cycle_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_event_filter_action_power_cycle (const char *event_filter_action_power_cycle_string, 
					   int *event_filter_action_power_cycle)
{
  if (strcasecmp (event_filter_action_power_cycle_string, YES_VALUE_STRING) == 0)
    {
      *event_filter_action_power_cycle = 1;
      return 0;
    }
  
  if (strcasecmp (event_filter_action_power_cycle_string, NO_VALUE_STRING) == 0)
    {
      *event_filter_action_power_cycle = 0;
      return 0;
    }
  
  return -1;
}

int 
event_filter_action_oem_to_string (int event_filter_action_oem, 
				   char **event_filter_action_oem_string)
{
  if (event_filter_action_oem)
    {
      if (!(*event_filter_action_oem_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*event_filter_action_oem_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_event_filter_action_oem (const char *event_filter_action_oem_string, 
				   int *event_filter_action_oem)
{
  if (strcasecmp (event_filter_action_oem_string, YES_VALUE_STRING) == 0)
    {
      *event_filter_action_oem = 1;
      return 0;
    }
  
  if (strcasecmp (event_filter_action_oem_string, NO_VALUE_STRING) == 0)
    {
      *event_filter_action_oem = 0;
      return 0;
    }
  
  return -1;
}

int 
event_filter_action_diagnostic_interrupt_to_string (int event_filter_action_diagnostic_interrupt, 
						    char **event_filter_action_diagnostic_interrupt_string)
{
  if (event_filter_action_diagnostic_interrupt)
    {
      if (!(*event_filter_action_diagnostic_interrupt_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*event_filter_action_diagnostic_interrupt_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_event_filter_action_diagnostic_interrupt (const char *event_filter_action_diagnostic_interrupt_string, 
						    int *event_filter_action_diagnostic_interrupt)
{
  if (strcasecmp (event_filter_action_diagnostic_interrupt_string, YES_VALUE_STRING) == 0)
    {
      *event_filter_action_diagnostic_interrupt = 1;
      return 0;
    }
  
  if (strcasecmp (event_filter_action_diagnostic_interrupt_string, NO_VALUE_STRING) == 0)
    {
      *event_filter_action_diagnostic_interrupt = 0;
      return 0;
    }
  
  return -1;
}

int 
event_filter_action_group_control_operation_to_string (int event_filter_action_group_control_operation, 
						       char **event_filter_action_group_control_operation_string)
{
  if (event_filter_action_group_control_operation)
    {
      if (!(*event_filter_action_group_control_operation_string = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  else 
    {
      if (!(*event_filter_action_group_control_operation_string = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return -1;
        }
    }
  
  return 0;
}

int 
string_to_event_filter_action_group_control_operation (const char *event_filter_action_group_control_operation_string, 
						       int *event_filter_action_group_control_operation)
{
  if (strcasecmp (event_filter_action_group_control_operation_string, YES_VALUE_STRING) == 0)
    {
      *event_filter_action_group_control_operation = 1;
      return 0;
    }
  
  if (strcasecmp (event_filter_action_group_control_operation_string, NO_VALUE_STRING) == 0)
    {
      *event_filter_action_group_control_operation = 0;
      return 0;
    }
  
  return -1;
}

int 
alert_policy_number_to_string (int alert_policy_number, 
			       char **alert_policy_number_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_policy_number);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *alert_policy_number_string = str;
  return 0;
}

int 
string_to_alert_policy_number (const char *alert_policy_number_string, 
			       int *alert_policy_number)
{
  int i = 0;

  if (str2int ((char *) alert_policy_number_string, 16, &i) == 0)
    {
      *alert_policy_number = i;
      return 0;
    }
  
  return -1;
}

int 
group_control_selector_to_string (int group_control_selector, 
				  char **group_control_selector_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", group_control_selector);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *group_control_selector_string = str;
  return 0;
}

int 
string_to_group_control_selector (const char *group_control_selector_string, 
				  int *group_control_selector)
{
  int i = 0;
  
  if (str2int ((char *) group_control_selector_string, 16, &i) == 0)
    {
      *group_control_selector = i;
      return 0;
    }
  
  return -1;
}

int 
event_severity_to_string (int event_severity, char **event_severity_string)
{
  switch (event_severity)
    {
    case IPMI_EVENT_SEVERITY_UNSPECIFIED:
      if (!(*event_severity_string = strdup (IPMI_EVENT_SEVERITY_UNSPECIFIED_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_EVENT_SEVERITY_MONITOR:
      if (!(*event_severity_string = strdup (IPMI_EVENT_SEVERITY_MONITOR_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_EVENT_SEVERITY_INFORMATION:
      if (!(*event_severity_string = strdup (IPMI_EVENT_SEVERITY_INFORMATION_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_EVENT_SEVERITY_OK:
      if (!(*event_severity_string = strdup (IPMI_EVENT_SEVERITY_OK_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION:
      if (!(*event_severity_string = strdup (IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_EVENT_SEVERITY_CRITICAL_CONDITION:
      if (!(*event_severity_string = strdup (IPMI_EVENT_SEVERITY_CRITICAL_CONDITION_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION:
      if (!(*event_severity_string = strdup (IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

int 
string_to_event_severity (const char *event_severity_string, int *event_severity)
{
  if (strcasecmp (event_severity_string, 
		  IPMI_EVENT_SEVERITY_UNSPECIFIED_STRING) == 0)
    {
      *event_severity = IPMI_EVENT_SEVERITY_UNSPECIFIED;
      return 0;
    }
  
  if (strcasecmp (event_severity_string, 
		  IPMI_EVENT_SEVERITY_MONITOR_STRING) == 0)
    {
      *event_severity = IPMI_EVENT_SEVERITY_MONITOR;
      return 0;
    }
  
  if (strcasecmp (event_severity_string, 
		  IPMI_EVENT_SEVERITY_INFORMATION_STRING) == 0)
    {
      *event_severity = IPMI_EVENT_SEVERITY_INFORMATION;
      return 0;
    }
  
  if (strcasecmp (event_severity_string, 
		  IPMI_EVENT_SEVERITY_OK_STRING) == 0)
    {
      *event_severity = IPMI_EVENT_SEVERITY_OK;
      return 0;
    }
  
  if (strcasecmp (event_severity_string, 
		  IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION_STRING) == 0)
    {
      *event_severity = IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION;
      return 0;
    }
  
  if (strcasecmp (event_severity_string, 
		  IPMI_EVENT_SEVERITY_CRITICAL_CONDITION_STRING) == 0)
    {
      *event_severity = IPMI_EVENT_SEVERITY_CRITICAL_CONDITION;
      return 0;
    }
  
  if (strcasecmp (event_severity_string, 
		  IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION_STRING) == 0)
    {
      *event_severity = IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION;
      return 0;
    }
  
  return -1;
}

int 
generator_id_byte1_to_string (int generator_id_byte1, char **generator_id_byte1_string)
{
  char *str = NULL;
  
  if (generator_id_byte1 == GENERATOR_ID_BYTE1_MATCH_ANY)
    {
      if (!(*generator_id_byte1_string = strdup (GENERATOR_ID_BYTE1_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  asprintf (&str, "0x%0X", generator_id_byte1);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *generator_id_byte1_string = str;
  return 0;
}

int 
string_to_generator_id_byte1 (const char *generator_id_byte1_string, int *generator_id_byte1)
{
  int i = 0;
  
  if (strcasecmp (generator_id_byte1_string, 
		  GENERATOR_ID_BYTE1_MATCH_ANY_STRING) == 0)
    {
      *generator_id_byte1 = GENERATOR_ID_BYTE1_MATCH_ANY;
      return 0;
    }
  
  if (str2int ((char *) generator_id_byte1_string, 16, &i) == 0)
    {
      *generator_id_byte1 = i;
      return 0;
    }
  
  return -1;
}

int 
generator_id_byte2_to_string (int generator_id_byte2, char **generator_id_byte2_string)
{
  char *str = NULL;
  
  if (generator_id_byte2 == GENERATOR_ID_BYTE2_MATCH_ANY)
    {
      if (!(*generator_id_byte2_string = strdup (GENERATOR_ID_BYTE2_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  asprintf (&str, "0x%0X", generator_id_byte2);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *generator_id_byte2_string = str;
  return 0;
}

int 
string_to_generator_id_byte2 (const char *generator_id_byte2_string, int *generator_id_byte2)
{
  int i = 0;
  
  if (strcasecmp (generator_id_byte2_string, 
		  GENERATOR_ID_BYTE2_MATCH_ANY_STRING) == 0)
    {
      *generator_id_byte2 = GENERATOR_ID_BYTE2_MATCH_ANY;
      return 0;
    }
  
  if (str2int ((char *) generator_id_byte2_string, 16, &i) == 0)
    {
      *generator_id_byte2 = i;
      return 0;
    }
  
  return -1;
}

int 
sensor_type_to_string (int sensor_type, char **sensor_type_string)
{
  const char *sensor_str = NULL;
  char *str = NULL;
  
  if (sensor_type == SENSOR_TYPE_MATCH_ANY)
    {
      if (!(*sensor_type_string = strdup (SENSOR_TYPE_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
    {
      asprintf (&str, "%s_0x%0X", sensor_str, sensor_type);
      if (!str)
        {
          perror("strdup");
          return -1;
        }
      strchr_replace (*sensor_type_string, ' ', '_');
      return 0;
    }
  
  /* XXX this is wrong - check range */
  if ((sensor_str = ipmi_get_sensor_group (sensor_type)) == NULL)
    return -1;

  if (!(*sensor_type_string = strdup (sensor_str)))
    {
      perror("strdup");
      return -1;
    }
  strchr_replace (*sensor_type_string, ' ', '_');
  return 0;
}

int 
string_to_sensor_type (const char *sensor_type_string, int *sensor_type)
{
  int i = 0;
  char *lstr = NULL;
  
  if (strcasecmp (sensor_type_string, 
		  SENSOR_TYPE_MATCH_ANY_STRING) == 0)
    {
      *sensor_type = SENSOR_TYPE_MATCH_ANY;
      return 0;
    }
  
  if (!(lstr = strdupa (sensor_type_string)))
    {
      perror("strdupa");
      return -1;
    }

  strchr_replace (lstr, '_', ' ');
  
  if (strcasestr (lstr, ipmi_oem_sensor_type) == lstr)
    {
      int stype = 0;
      char *token = strrchr (lstr, ' ');
      
      if (token)
	{
	  if (str2int (token, 16, &stype) == 0)
	    {
	      *sensor_type = stype;
	      return 0;
	    }
	}
      
      return -1;
    }
  
  for (i = 0; ipmi_sensor_types[i]; i++)
    {
      if (strcasecmp (ipmi_sensor_types[i], lstr) == 0)
	{
	  *sensor_type = i;
	  return 0;
	}
    }
  
  return -1;
}

int 
sensor_number_to_string (int sensor_number, char **sensor_number_string)
{
  char *str = NULL;
  
  if (sensor_number == SENSOR_NUMBER_MATCH_ANY)
    {
      if (!(*sensor_number_string = strdup (SENSOR_NUMBER_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  asprintf (&str, "0x%0X", sensor_number);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *sensor_number_string = str;
  return 0;
}

int 
string_to_sensor_number (const char *sensor_number_string, int *sensor_number)
{
  int i = 0;
  
  if (strcasecmp (sensor_number_string, 
		  SENSOR_NUMBER_MATCH_ANY_STRING) == 0)
    {
      *sensor_number = SENSOR_NUMBER_MATCH_ANY;
      return 0;
    }
  
  if (str2int ((char *) sensor_number_string, 16, &i) == 0)
    {
      *sensor_number = i;
      return 0;
    }
  
  return -1;
}

int 
event_trigger_to_string (int event_trigger, char **event_trigger_string)
{
  char *str;

  if (event_trigger == EVENT_TRIGGER_MATCH_ANY)
    {
      if (!(*event_trigger_string = strdup (EVENT_TRIGGER_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  switch (ipmi_sensor_classify (event_trigger))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      if (!(*event_trigger_string = strdup (IPMI_SENSOR_CLASS_THRESHOLD_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      asprintf (&str, "%s_0x%0X", IPMI_SENSOR_CLASS_GENERIC_DISCRETE_STRING, event_trigger);
      if (!str)
        {
          perror("strdup");
          return -1;
        }
      *event_trigger_string = str;
      return 0;
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      if (!(*event_trigger_string = strdup (IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    case IPMI_SENSOR_CLASS_OEM:
      asprintf (&str, "%s_0x%0X", IPMI_SENSOR_CLASS_OEM_STRING, event_trigger);
      if (!str)
        {
          perror("strdup");
          return -1;
        }
      *event_trigger_string = str;
      return 0;
    case IPMI_SENSOR_CLASS_NOT_AVAILABLE:
      if (!(*event_trigger_string = strdup (IPMI_SENSOR_CLASS_UNSPECIFIED_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    default:
      return -1;
    }
  
  return -1;
}

int 
string_to_event_trigger (const char *event_trigger_string, int *event_trigger)
{
  char *lstr = NULL;
  
  if (strcasecmp (event_trigger_string, 
		  EVENT_TRIGGER_MATCH_ANY_STRING) == 0)
    {
      *event_trigger = EVENT_TRIGGER_MATCH_ANY;
      return 0;
    }
  
  if (!(lstr = strdupa (event_trigger_string)))
    {
      perror("strdupa");
      return -1;
    }
  
  if (strcasecmp (lstr, IPMI_SENSOR_CLASS_UNSPECIFIED_STRING) == 0)
    {
      *event_trigger = 0;
      return 0;
    }

  if (strcasestr (lstr, IPMI_SENSOR_CLASS_THRESHOLD_STRING) == lstr)
    {
      int et = 0;
      char *token = strrchr (lstr, '_');
      
      if (token)
	{
	  if (str2int ((token + 1), 16, &et) == 0)
	    {
	      *event_trigger = et;
	      return 0;
	    }
	}
      
      return -1;
    }
  
  if (strcasecmp (lstr, IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE_STRING) == 0)
    {
      *event_trigger = IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE;
      return 0;
    }
  
  if (strcasestr (lstr, IPMI_SENSOR_CLASS_OEM_STRING) == lstr)
    {
      int et = 0;
      char *token = strrchr (lstr, '_');
      
      if (token)
	{
	  if (str2int ((token + 1), 16, &et) == 0)
	    {
	      *event_trigger = et;
	      return 0;
	    }
	}
      
      return -1;
    }
  
  return -1;
}

int 
event_data1_offset_mask_to_string (int event_data1_offset_mask, 
				   char **event_data1_offset_mask_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_offset_mask);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data1_offset_mask_string = str;
  return 0;
}

int 
string_to_event_data1_offset_mask (const char *event_data1_offset_mask_string, 
				   int *event_data1_offset_mask)
{
  int i = 0;
  
  if (str2int ((char *) event_data1_offset_mask_string, 16, &i) == 0)
    {
      *event_data1_offset_mask = i;
      return 0;
    }
  
  return -1;
}

int 
event_data1_AND_mask_to_string (int event_data1_AND_mask, 
				char **event_data1_AND_mask_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_AND_mask);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data1_AND_mask_string = str;
  return 0;
}

int 
string_to_event_data1_AND_mask (const char *event_data1_AND_mask_string, 
				int *event_data1_AND_mask)
{
  int i = 0;
  
  if (str2int ((char *) event_data1_AND_mask_string, 16, &i) == 0)
    {
      *event_data1_AND_mask = i;
      return 0;
    }
  
  return -1;
}

int 
event_data1_compare1_to_string (int event_data1_compare1, 
				char **event_data1_compare1_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_compare1);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data1_compare1_string = str;
  return 0;
}

int 
string_to_event_data1_compare1 (const char *event_data1_compare1_string, 
				int *event_data1_compare1)
{
  int i = 0;
  
  if (str2int ((char *) event_data1_compare1_string, 16, &i) == 0)
    {
      *event_data1_compare1 = i;
      return 0;
    }
  
  return -1;
}

int 
event_data1_compare2_to_string (int event_data1_compare2, 
				char **event_data1_compare2_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_compare2);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data1_compare2_string = str;
  return 0;
}

int 
string_to_event_data1_compare2 (const char *event_data1_compare2_string, 
				int *event_data1_compare2)
{
  int i = 0;
  
  if (str2int ((char *) event_data1_compare2_string, 16, &i) == 0)
    {
      *event_data1_compare2 = i;
      return 0;
    }
  
  return -1;
}

int 
event_data2_AND_mask_to_string (int event_data2_AND_mask, 
				char **event_data2_AND_mask_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data2_AND_mask);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data2_AND_mask_string = str;
  return 0;
}

int 
string_to_event_data2_AND_mask (const char *event_data2_AND_mask_string, 
				int *event_data2_AND_mask)
{
  int i = 0;
  
  if (str2int ((char *) event_data2_AND_mask_string, 16, &i) == 0)
    {
      *event_data2_AND_mask = i;
      return 0;
    }
  
  return -1;
}

int 
event_data2_compare1_to_string (int event_data2_compare1, 
				char **event_data2_compare1_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data2_compare1);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data2_compare1_string = str;
  return 0;
}

int 
string_to_event_data2_compare1 (const char *event_data2_compare1_string, 
				int *event_data2_compare1)
{
  int i = 0;
  
  if (str2int ((char *) event_data2_compare1_string, 16, &i) == 0)
    {
      *event_data2_compare1 = i;
      return 0;
    }
  
  return -1;
}

int 
event_data2_compare2_to_string (int event_data2_compare2, 
				char **event_data2_compare2_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data2_compare2);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data2_compare2_string = str;
  return 0;
}

int 
string_to_event_data2_compare2 (const char *event_data2_compare2_string, 
				int *event_data2_compare2)
{
  int i = 0;
  
  if (str2int ((char *) event_data2_compare2_string, 16, &i) == 0)
    {
      *event_data2_compare2 = i;
      return 0;
    }
  
  return -1;
}

int 
event_data3_AND_mask_to_string (int event_data3_AND_mask, 
				char **event_data3_AND_mask_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data3_AND_mask);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data3_AND_mask_string = str;
  return 0;
}

int 
string_to_event_data3_AND_mask (const char *event_data3_AND_mask_string, 
				int *event_data3_AND_mask)
{
  int i = 0;
  
  if (str2int ((char *) event_data3_AND_mask_string, 16, &i) == 0)
    {
      *event_data3_AND_mask = i;
      return 0;
    }
  
  return -1;
}

int 
event_data3_compare1_to_string (int event_data3_compare1, 
				char **event_data3_compare1_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data3_compare1);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data3_compare1_string = str;
  return 0;
}

int 
string_to_event_data3_compare1 (const char *event_data3_compare1_string, 
				int *event_data3_compare1)
{
  int i = 0;
  
  if (str2int ((char *) event_data3_compare1_string, 16, &i) == 0)
    {
      *event_data3_compare1 = i;
      return 0;
    }
  
  return -1;
}

int 
event_data3_compare2_to_string (int event_data3_compare2, 
				char **event_data3_compare2_string)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data3_compare2);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *event_data3_compare2_string = str;
  return 0;
}

int 
string_to_event_data3_compare2 (const char *event_data3_compare2_string, 
				int *event_data3_compare2)
{
  int i = 0;
  
  if (str2int ((char *) event_data3_compare2_string, 16, &i) == 0)
    {
      *event_data3_compare2 = i;
      return 0;
    }
  
  return -1;
}

int 
policy_type_to_string (int policy_type, char **policy_type_string)
{
  if (policy_type == IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION)
    {
      if (!(*policy_type_string = strdup (IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (policy_type == IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY)
    {
      if (!(*policy_type_string = strdup (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (policy_type == IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES)
    {
      if (!(*policy_type_string = strdup (IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (policy_type == IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL)
    {
      if (!(*policy_type_string = strdup (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (policy_type == IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE)
    {
      if (!(*policy_type_string = strdup (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

int 
string_to_policy_type (const char *policy_type_string, int *policy_type)
{
  if (strcasecmp (policy_type_string, 
		  IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION_STRING) == 0)
    {
      *policy_type = IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION;
      return 0;
    }
  if (strcasecmp (policy_type_string, 
		  IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_STRING) == 0)
    {
      *policy_type = IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY;
      return 0;
    }
  if (strcasecmp (policy_type_string, 
		  IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES_STRING) == 0)
    {
      *policy_type = IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES;
      return 0;
    }
  if (strcasecmp (policy_type_string, 
		  IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL_STRING) == 0)
    {
      *policy_type = IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL;
      return 0;
    }
  if (strcasecmp (policy_type_string, 
		  IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE_STRING) == 0)
    {
      *policy_type = IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE;
      return 0;
    }
  
  return -1;
}

int 
policy_enabled_to_string (int policy_enabled, char **policy_enabled_string)
{
  if (policy_enabled == IPMI_ALERT_POLICY_DISABLED)
    {
      if (!(*policy_enabled_string = strdup (IPMI_ALERT_POLICY_DISABLED_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (policy_enabled == IPMI_ALERT_POLICY_ENABLED)
    {
      if (!(*policy_enabled_string = strdup (IPMI_ALERT_POLICY_ENABLED_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

int 
string_to_policy_enabled (const char *policy_enabled_string, int *policy_enabled)
{
  if (strcasecmp (policy_enabled_string, IPMI_ALERT_POLICY_DISABLED_STRING) == 0)
    {
      *policy_enabled = IPMI_ALERT_POLICY_DISABLED;
      return 0;
    }
  if (strcasecmp (policy_enabled_string, IPMI_ALERT_POLICY_ENABLED_STRING) == 0)
    {
      *policy_enabled = IPMI_ALERT_POLICY_ENABLED;
      return 0;
    }
  return -1;
}

int 
policy_number_to_string (int policy_number, char **policy_number_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", policy_number);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *policy_number_string = str;
  return 0;
}

int 
string_to_policy_number (const char *policy_number_string, int *policy_number)
{
  int i = 0;
  
  if (str2int ((char *) policy_number_string, 0, &i) == 0)
    {
      *policy_number = i;
      return 0;
    }
  
  return -1;
}

int 
destination_selector_to_string (int destination_selector, 
				char **destination_selector_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", destination_selector);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *destination_selector_string = str;
  return 0;
}

int 
string_to_destination_selector (const char *destination_selector_string, 
				int *destination_selector)
{
  int i = 0;
  
  if (str2int ((char *) destination_selector_string, 0, &i) == 0)
    {
      *destination_selector = i;
      return 0;
    }
  
  return -1;
}

int 
channel_number_to_string (int channel_number, char **channel_number_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", channel_number);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *channel_number_string = str;
  return 0;
}

int 
string_to_channel_number (const char *channel_number_string, int *channel_number)
{
  int i = 0;
  
  if (str2int ((char *) channel_number_string, 0, &i) == 0)
    {
      *channel_number = i;
      return 0;
    }
  
  return -1;
}

int 
alert_string_set_selector_to_string (int alert_string_set_selector, 
				     char **alert_string_set_selector_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_string_set_selector);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *alert_string_set_selector_string = str;
  return 0;
}

int 
string_to_alert_string_set_selector (const char *alert_string_set_selector_string, 
				     int *alert_string_set_selector)
{
  int i = 0;
  
  if (str2int ((char *) alert_string_set_selector_string, 0, &i) == 0)
    {
      *alert_string_set_selector = i;
      return 0;
    }
  
  return -1;
}

int 
event_specific_alert_string_lookup_to_string (int event_specific_alert_string_lookup, 
					      char **event_specific_alert_string_lookup_string)
{
  if (event_specific_alert_string_lookup == 
      IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO)
    {
      if (!(*event_specific_alert_string_lookup_string = 
            strdup (IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (event_specific_alert_string_lookup == 
      IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES)
    {
      if (!(*event_specific_alert_string_lookup_string = 
            strdup (IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

int 
string_to_event_specific_alert_string_lookup (const char *event_specific_alert_string_lookup_string, 
					      int *event_specific_alert_string_lookup)
{
  if (strcasecmp (event_specific_alert_string_lookup_string, 
		  IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO_STRING) == 0)
    {
      *event_specific_alert_string_lookup = 
	IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO;
      return 0;
    }
  if (strcasecmp (event_specific_alert_string_lookup_string, 
		  IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES_STRING) == 0)
    {
      *event_specific_alert_string_lookup = 
	IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES;
      return 0;
    }
  
  return -1;
}

int 
destination_type_to_string (int alert_destination_type, 
			    char **alert_destination_type_string)
{
  if (alert_destination_type == IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION)
    {
      if (!(*alert_destination_type_string = 
            strdup (IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (alert_destination_type == IPMI_DESTINATION_TYPE_OEM1)
    {
      if (!(*alert_destination_type_string = 
            strdup (IPMI_DESTINATION_TYPE_OEM1_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (alert_destination_type == IPMI_DESTINATION_TYPE_OEM2)
    {
      if (!(*alert_destination_type_string = 
            strdup (IPMI_DESTINATION_TYPE_OEM2_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

int 
string_to_destination_type (const char *alert_destination_type_string, 
			    int *alert_destination_type)
{
  if (strcasecmp (alert_destination_type_string, 
		  IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION_STRING) == 0)
    {
      *alert_destination_type = IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION;
      return 0;
    }
  if (strcasecmp (alert_destination_type_string, 
		  IPMI_DESTINATION_TYPE_OEM1_STRING) == 0)
    {
      *alert_destination_type = IPMI_DESTINATION_TYPE_OEM1;
      return 0;
    }
  if (strcasecmp (alert_destination_type_string, 
		  IPMI_DESTINATION_TYPE_OEM2_STRING) == 0)
    {
      *alert_destination_type = IPMI_DESTINATION_TYPE_OEM2;
      return 0;
    }
  
  return -1;
}

int 
alert_acknowledge_to_string (int alert_acknowledge, 
			     char **alert_acknowledge_string)
{
  if (alert_acknowledge == IPMI_ALERT_UNACKNOWLEDGED)
    {
      if (!(*alert_acknowledge_string = 
            strdup (IPMI_ALERT_UNACKNOWLEDGED_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (alert_acknowledge == IPMI_ALERT_ACKNOWLEDGED)
    {
      if (!(*alert_acknowledge_string = 
            strdup (IPMI_ALERT_ACKNOWLEDGED_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  return -1;
}

int 
string_to_alert_acknowledge (const char *alert_acknowledge_string, 
			     int *alert_acknowledge)
{
  if (strcasecmp (alert_acknowledge_string, 
		  IPMI_ALERT_UNACKNOWLEDGED_STRING) == 0)
    {
      *alert_acknowledge = IPMI_ALERT_UNACKNOWLEDGED;
      return 0;
    }
  if (strcasecmp (alert_acknowledge_string, 
		  IPMI_ALERT_ACKNOWLEDGED_STRING) == 0)
    {
      *alert_acknowledge = IPMI_ALERT_ACKNOWLEDGED;
      return 0;
    }
  return -1;
}

int 
alert_acknowledge_timeout_to_string (int alert_acknowledge_timeout, 
				     char **alert_acknowledge_timeout_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_acknowledge_timeout);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *alert_acknowledge_timeout_string = str;
  return 0;
}

int 
string_to_alert_acknowledge_timeout (const char *alert_acknowledge_timeout_string, 
				     int *alert_acknowledge_timeout)
{
  int i = 0;
  
  if (str2int ((char *) alert_acknowledge_timeout_string, 0, &i) == 0)
    {
      *alert_acknowledge_timeout = i;
      return 0;
    }
  
  return -1;
}

int 
alert_retries_to_string (int alert_retries, char **alert_retries_string)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_retries);
  if (!str)
    {
      perror("strdup");
      return -1;
    }
  *alert_retries_string = str;
  return 0;
}

int 
string_to_alert_retries (const char *alert_retries_string, int *alert_retries)
{
  int i = 0;
  
  if (str2int ((char *) alert_retries_string, 0, &i) == 0)
    {
      *alert_retries = i;
      return 0;
    }
  
  return -1;
}

int 
gateway_selector_to_string (int alert_gateway, char **alert_gateway_string)
{
  if (alert_gateway == IPMI_GATEWAY_SELECTOR_DEFAULT)
    {
      if (!(*alert_gateway_string = strdup (IPMI_GATEWAY_SELECTOR_DEFAULT_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  if (alert_gateway == IPMI_GATEWAY_SELECTOR_BACKUP)
    {
      if (!(*alert_gateway_string = strdup (IPMI_GATEWAY_SELECTOR_BACKUP_STRING)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  return -1;
}

int 
string_to_gateway_selector (const char *alert_gateway_string, int *alert_gateway)
{
  if (strcasecmp (alert_gateway_string, 
		  IPMI_GATEWAY_SELECTOR_DEFAULT_STRING) == 0)
    {
      *alert_gateway = IPMI_GATEWAY_SELECTOR_DEFAULT;
      return 0;
    }
  if (strcasecmp (alert_gateway_string, 
		  IPMI_GATEWAY_SELECTOR_BACKUP_STRING) == 0)
    {
      *alert_gateway = IPMI_GATEWAY_SELECTOR_BACKUP;
      return 0;
    }
  return -1;
}

int 
alert_ip_address_to_string (const char *alert_ip_address, 
			    char **alert_ip_address_string)
{
  if (!(*alert_ip_address_string = strdup (alert_ip_address)))
    {
      perror("strdup");
      return -1;
    }
  
  return 0;
}

int 
string_to_alert_ip_address (const char *alert_ip_address_string, 
			    char **alert_ip_address)
{
  struct in_addr addr;
  
  if (inet_aton (alert_ip_address_string, &addr) != 0)
    {
      if (!(*alert_ip_address = strdup (alert_ip_address_string)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

int 
alert_mac_address_to_string (const char *alert_mac_address, 
			     char **alert_mac_address_string)
{
  if (!(*alert_mac_address_string = strdup (alert_mac_address)))
    {
      perror("strdup");
      return -1;
    }
  
  return 0;
}

int 
string_to_alert_mac_address (const char *alert_mac_address_string, 
			     char **alert_mac_address)
{
  unsigned int foo;
  
  if (sscanf (alert_mac_address_string, 
              "%02x:%02x:%02x:%02x:%02x:%02x", 
              &foo, 
              &foo, 
              &foo, 
              &foo, 
              &foo, 
              &foo) == 6)
    {
      if (!(*alert_mac_address = strdup (alert_mac_address_string)))
        {
          perror("strdup");
          return -1;
        }
      return 0;
    }
  
  return -1;
}

