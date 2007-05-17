#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

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

static int 
_strchr_replace (char *str, char ch, char nch)
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

char * 
filter_number_to_string (int filter_number)
{
  char *str = NULL;
  
  asprintf (&str, "%d", filter_number);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_filter_number (const char *filter_number_string)
{
  int n = 0;
  
  if (!str2int ((char *) filter_number_string, 16, &n))
    return n;
  
  return -1;
}

char *
filter_type_to_string (int filter_type)
{
  char *str = NULL;

  switch (filter_type)
    {
    case IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER:
      if (!(str = strdup (IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER:
      if (!(str = strdup (IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER_STRING))) 
        {
          perror("strdup");
          return NULL;
        }
      return str;
    default:
      if (!(str = strdup (IPMI_FILTER_CONFIGURATION_RESERVED_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  if (!(str = strdup("")))
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_filter_type (const char *filter_type_string)
{
  if (!strcasecmp (filter_type_string, IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER_STRING))
    return IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER;
  if (!strcasecmp (filter_type_string, IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER_STRING))
    return IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER;
  
  return -1;
}

char * 
enable_filter_to_string (int enable_filter)
{
  char *str = NULL;

  if (enable_filter)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_enable_filter (const char *enable_filter_string)
{
  if (!strcasecmp (enable_filter_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (enable_filter_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
event_filter_action_alert_to_string (int event_filter_action_alert)
{
  char *str = NULL;

  if (event_filter_action_alert)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_filter_action_alert (const char *event_filter_action_alert_string)
{
  if (!strcasecmp (event_filter_action_alert_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (event_filter_action_alert_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
event_filter_action_power_off_to_string (int event_filter_action_power_off)
{
  char *str = NULL;

  if (event_filter_action_power_off)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_filter_action_power_off (const char *event_filter_action_power_off_string)
{
  if (!strcasecmp (event_filter_action_power_off_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (event_filter_action_power_off_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
event_filter_action_reset_to_string (int event_filter_action_reset)
{
  char *str = NULL;

  if (event_filter_action_reset)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_filter_action_reset (const char *event_filter_action_reset_string)
{
  if (!strcasecmp (event_filter_action_reset_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (event_filter_action_reset_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
event_filter_action_power_cycle_to_string (int event_filter_action_power_cycle)
{
  char *str = NULL;

  if (event_filter_action_power_cycle)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_filter_action_power_cycle (const char *event_filter_action_power_cycle_string)
{
  if (!strcasecmp (event_filter_action_power_cycle_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (event_filter_action_power_cycle_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
event_filter_action_oem_to_string (int event_filter_action_oem)
{
  char *str = NULL;

  if (event_filter_action_oem)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_filter_action_oem (const char *event_filter_action_oem_string)
{
  if (!strcasecmp (event_filter_action_oem_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (event_filter_action_oem_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
event_filter_action_diagnostic_interrupt_to_string (int event_filter_action_diagnostic_interrupt)
{
  char *str = NULL;

  if (event_filter_action_diagnostic_interrupt)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_filter_action_diagnostic_interrupt (const char *event_filter_action_diagnostic_interrupt_string)
{
  if (!strcasecmp (event_filter_action_diagnostic_interrupt_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (event_filter_action_diagnostic_interrupt_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
event_filter_action_group_control_operation_to_string (int event_filter_action_group_control_operation)
{
  char *str = NULL;

  if (event_filter_action_group_control_operation)
    {
      if (!(str = strdup (YES_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else 
    {
      if (!(str = strdup (NO_VALUE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_filter_action_group_control_operation (const char *event_filter_action_group_control_operation_string)
{
  if (!strcasecmp (event_filter_action_group_control_operation_string, YES_VALUE_STRING))
    return 1;
  
  if (!strcasecmp (event_filter_action_group_control_operation_string, NO_VALUE_STRING))
    return 0;
  
  return -1;
}

char *
alert_policy_number_to_string (int alert_policy_number)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_policy_number);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_alert_policy_number (const char *alert_policy_number_string)
{
  int n = 0;

  if (!str2int ((char *) alert_policy_number_string, 16, &n))
    return n;
  
  return -1;
}

char *
group_control_selector_to_string (int group_control_selector)
{
  char *str = NULL;
  
  asprintf (&str, "%d", group_control_selector);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_group_control_selector (const char *group_control_selector_string)
{
  int n = 0;
  
  if (!str2int ((char *) group_control_selector_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_severity_to_string (int event_severity)
{
  char *str = NULL;

  switch (event_severity)
    {
    case IPMI_EVENT_SEVERITY_UNSPECIFIED:
      if (!(str = strdup (IPMI_EVENT_SEVERITY_UNSPECIFIED_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_EVENT_SEVERITY_MONITOR:
      if (!(str = strdup (IPMI_EVENT_SEVERITY_MONITOR_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_EVENT_SEVERITY_INFORMATION:
      if (!(str = strdup (IPMI_EVENT_SEVERITY_INFORMATION_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_EVENT_SEVERITY_OK:
      if (!(str = strdup (IPMI_EVENT_SEVERITY_OK_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION:
      if (!(str = strdup (IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_EVENT_SEVERITY_CRITICAL_CONDITION:
      if (!(str = strdup (IPMI_EVENT_SEVERITY_CRITICAL_CONDITION_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION:
      if (!(str = strdup (IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  if (!(str = strdup("")))
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_severity (const char *event_severity_string)
{
  if (!strcasecmp (event_severity_string, IPMI_EVENT_SEVERITY_UNSPECIFIED_STRING))
    return IPMI_EVENT_SEVERITY_UNSPECIFIED;
  
  if (!strcasecmp (event_severity_string, IPMI_EVENT_SEVERITY_MONITOR_STRING))
    return IPMI_EVENT_SEVERITY_MONITOR;
  
  if (!strcasecmp (event_severity_string, IPMI_EVENT_SEVERITY_INFORMATION_STRING))
    return IPMI_EVENT_SEVERITY_INFORMATION;
  
  if (!strcasecmp (event_severity_string, IPMI_EVENT_SEVERITY_OK_STRING))
    return IPMI_EVENT_SEVERITY_OK;

  if (!strcasecmp (event_severity_string, IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION_STRING))
    return IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION;
  
  if (!strcasecmp (event_severity_string, IPMI_EVENT_SEVERITY_CRITICAL_CONDITION_STRING))
    return IPMI_EVENT_SEVERITY_CRITICAL_CONDITION;
  
  if (!strcasecmp (event_severity_string, IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION_STRING))
    return IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION;
  
  return -1;
}

char *
generator_id_byte1_to_string (int generator_id_byte1)
{
  char *str = NULL;
  
  if (generator_id_byte1 == GENERATOR_ID_BYTE1_MATCH_ANY)
    {
      if (!(str = strdup (GENERATOR_ID_BYTE1_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  asprintf (&str, "0x%0X", generator_id_byte1);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_generator_id_byte1 (const char *generator_id_byte1_string)
{
  int n = 0;
  
  if (!strcasecmp (generator_id_byte1_string, GENERATOR_ID_BYTE1_MATCH_ANY_STRING))
    return GENERATOR_ID_BYTE1_MATCH_ANY;
  
  if (!str2int ((char *) generator_id_byte1_string, 16, &n))
    return n;
  
  return -1;
}

char *
generator_id_byte2_to_string (int generator_id_byte2)
{
  char *str = NULL;
  
  if (generator_id_byte2 == GENERATOR_ID_BYTE2_MATCH_ANY)
    {
      if (!(str = strdup (GENERATOR_ID_BYTE2_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  asprintf (&str, "0x%0X", generator_id_byte2);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_generator_id_byte2 (const char *generator_id_byte2_string)
{
  int n = 0;
  
  if (!strcasecmp (generator_id_byte2_string, GENERATOR_ID_BYTE2_MATCH_ANY_STRING))
    return GENERATOR_ID_BYTE2_MATCH_ANY;
  
  if (!str2int ((char *) generator_id_byte2_string, 16, &n))
    return n;
  
  return -1;
}

char *
sensor_type_to_string (int sensor_type)
{
  const char *sensor_str = NULL;
  char *str = NULL;
  
  if (sensor_type == SENSOR_TYPE_MATCH_ANY)
    {
      if (!(str = strdup (SENSOR_TYPE_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  if (!(sensor_str = ipmi_get_sensor_group (sensor_type)))
    sensor_str = "";
      
  if (!(str = strdup (sensor_str)))
    {
      perror("strdup");
      return NULL;
    }

  _strchr_replace (str, ' ', '_');
  return str;
}

int 
string_to_sensor_type (const char *sensor_type_string)
{
  int i = 0;
  char *lstr = NULL;
  
  if (!strcasecmp (sensor_type_string, SENSOR_TYPE_MATCH_ANY_STRING))
    return SENSOR_TYPE_MATCH_ANY;
  
  if (!(lstr = strdupa (sensor_type_string)))
    {
      perror("strdupa");
      return -1;
    }

  _strchr_replace (lstr, '_', ' ');
  
  if (strcasestr (lstr, ipmi_oem_sensor_type) == lstr)
    {
      int n = 0;
      char *token = strrchr (lstr, ' ');
      
      if (token)
	{
	  if (!str2int (token, 16, &n))
            return n;
	}
      
      return -1;
    }
  
  for (i = 0; ipmi_sensor_types[i]; i++)
    {
      if (!strcasecmp (ipmi_sensor_types[i], lstr))
        return i;
    }
  
  return -1;
}

char *
sensor_number_to_string (int sensor_number)
{
  char *str = NULL;
  
  if (sensor_number == SENSOR_NUMBER_MATCH_ANY)
    {
      if (!(str = strdup (SENSOR_NUMBER_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  asprintf (&str, "0x%0X", sensor_number);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_sensor_number (const char *sensor_number_string)
{
  int n = 0;
  
  if (!strcasecmp (sensor_number_string, SENSOR_NUMBER_MATCH_ANY_STRING))
    return SENSOR_NUMBER_MATCH_ANY;
  
  if (!str2int ((char *) sensor_number_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_trigger_to_string (int event_trigger)
{
  char *str = NULL;

  if (event_trigger == EVENT_TRIGGER_MATCH_ANY)
    {
      if (!(str = strdup (EVENT_TRIGGER_MATCH_ANY_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  switch (ipmi_sensor_classify (event_trigger))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      if (!(str = strdup (IPMI_SENSOR_CLASS_THRESHOLD_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      asprintf (&str, "%s_0x%0X", IPMI_SENSOR_CLASS_GENERIC_DISCRETE_STRING, event_trigger);
      if (!str)
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      if (!(str = strdup (IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_SENSOR_CLASS_OEM:
      asprintf (&str, "%s_0x%0X", IPMI_SENSOR_CLASS_OEM_STRING, event_trigger);
      if (!str)
        {
          perror("strdup");
          return NULL;
        }
      return str;
    case IPMI_SENSOR_CLASS_NOT_AVAILABLE:
      if (!(str = strdup (IPMI_SENSOR_CLASS_UNSPECIFIED_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  if (!(str = strdup("")))
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_trigger (const char *event_trigger_string)
{
  char *lstr = NULL;
  
  if (!strcasecmp (event_trigger_string, EVENT_TRIGGER_MATCH_ANY_STRING))
    return EVENT_TRIGGER_MATCH_ANY;
  
  if (!(lstr = strdupa (event_trigger_string)))
    {
      perror("strdupa");
      return -1;
    }
  
  if (!strcasecmp (lstr, IPMI_SENSOR_CLASS_UNSPECIFIED_STRING))
    return 0;

  if (strcasestr (lstr, IPMI_SENSOR_CLASS_THRESHOLD_STRING) == lstr)
    {
      int n = 0;
      char *token = strrchr (lstr, '_');
      
      if (token)
	{
	  if (!str2int ((token + 1), 16, &n))
            return n;
	}
      
      return -1;
    }
  
  if (!strcasecmp (lstr, IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE_STRING))
    return IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE;
  
  if (strcasestr (lstr, IPMI_SENSOR_CLASS_OEM_STRING) == lstr)
    {
      int n = 0;
      char *token = strrchr (lstr, '_');
      
      if (token)
	{
	  if (!str2int ((token + 1), 16, &n))
            return n;
	}
      
      return -1;
    }
  
  return -1;
}

char *
event_data1_offset_mask_to_string (int event_data1_offset_mask)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_offset_mask);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data1_offset_mask (const char *event_data1_offset_mask_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data1_offset_mask_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data1_AND_mask_to_string (int event_data1_AND_mask)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_AND_mask);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data1_AND_mask (const char *event_data1_AND_mask_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data1_AND_mask_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data1_compare1_to_string (int event_data1_compare1)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_compare1);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data1_compare1 (const char *event_data1_compare1_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data1_compare1_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data1_compare2_to_string (int event_data1_compare2)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data1_compare2);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data1_compare2 (const char *event_data1_compare2_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data1_compare2_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data2_AND_mask_to_string (int event_data2_AND_mask)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data2_AND_mask);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data2_AND_mask (const char *event_data2_AND_mask_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data2_AND_mask_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data2_compare1_to_string (int event_data2_compare1)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data2_compare1);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data2_compare1 (const char *event_data2_compare1_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data2_compare1_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data2_compare2_to_string (int event_data2_compare2)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data2_compare2);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data2_compare2 (const char *event_data2_compare2_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data2_compare2_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data3_AND_mask_to_string (int event_data3_AND_mask)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data3_AND_mask);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data3_AND_mask (const char *event_data3_AND_mask_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data3_AND_mask_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data3_compare1_to_string (int event_data3_compare1)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data3_compare1);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data3_compare1 (const char *event_data3_compare1_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data3_compare1_string, 16, &n))
    return n;
  
  return -1;
}

char *
event_data3_compare2_to_string (int event_data3_compare2)
{
  char *str = NULL;
  
  asprintf (&str, "0x%X", event_data3_compare2);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_event_data3_compare2 (const char *event_data3_compare2_string)
{
  int n = 0;
  
  if (!str2int ((char *) event_data3_compare2_string, 16, &n))
    return n;
  
  return -1;
}

char *
policy_type_to_string (int policy_type)
{
  char *str = NULL;

  if (policy_type == IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION)
    {
      if (!(str = strdup (IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  else if (policy_type == IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY)
    {
      if (!(str = strdup (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  else if (policy_type == IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES)
    {
      if (!(str = strdup (IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  else if (policy_type == IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL)
    {
      if (!(str = strdup (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  else if (policy_type == IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE)
    {
      if (!(str = strdup (IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  
  if (!(str = strdup("")))
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_policy_type (const char *policy_type_string)
{
  if (!strcasecmp (policy_type_string, IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION_STRING))
    return IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION;
  if (!strcasecmp (policy_type_string, IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_STRING))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY;
  if (!strcasecmp (policy_type_string, IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES_STRING))
    return IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES;
  if (!strcasecmp (policy_type_string, IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL_STRING))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL;
  if (!strcasecmp (policy_type_string, IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE_STRING))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE;
  
  return -1;
}

char *
policy_enabled_to_string (int policy_enabled)
{
  char *str = NULL;

  if (policy_enabled == IPMI_ALERT_POLICY_ENABLED)
    {
      if (!(str = strdup (IPMI_ALERT_POLICY_ENABLED_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else
    {
      if (!(str = strdup (IPMI_ALERT_POLICY_DISABLED_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_policy_enabled (const char *policy_enabled_string)
{
  if (!strcasecmp (policy_enabled_string, IPMI_ALERT_POLICY_DISABLED_STRING))
    return IPMI_ALERT_POLICY_DISABLED;

  if (!strcasecmp (policy_enabled_string, IPMI_ALERT_POLICY_ENABLED_STRING))
    return IPMI_ALERT_POLICY_ENABLED;

  return -1;
}

char *
policy_number_to_string (int policy_number)
{
  char *str = NULL;
  
  asprintf (&str, "%d", policy_number);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_policy_number (const char *policy_number_string)
{
  int n = 0;
  
  if (!str2int ((char *) policy_number_string, 0, &n))
    return n;
  
  return -1;
}

char *
destination_selector_to_string (int destination_selector)
{
  char *str = NULL;
  
  asprintf (&str, "%d", destination_selector);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_destination_selector (const char *destination_selector_string)
{
  int n = 0;
  
  if (!str2int ((char *) destination_selector_string, 0, &n))
    return n;
  
  return -1;
}

char *
channel_number_to_string (int channel_number)
{
  char *str = NULL;
  
  asprintf (&str, "%d", channel_number);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_channel_number (const char *channel_number_string)
{
  int n = 0;
  
  if (!str2int ((char *) channel_number_string, 0, &n))
    return n;
  
  return -1;
}

char *
alert_string_set_selector_to_string (int alert_string_set_selector)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_string_set_selector);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_alert_string_set_selector (const char *alert_string_set_selector_string)
{
  int n = 0;
  
  if (!str2int ((char *) alert_string_set_selector_string, 0, &n))
    return n;
  
  return -1;
}

char *
event_specific_alert_string_lookup_to_string (int event_specific_alert_string_lookup)
{
  char *str = NULL;

  if (event_specific_alert_string_lookup == IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES)
    {
      if (!(str = strdup (IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else
    {
      if (!(str = strdup (IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  
  return str;
}

int 
string_to_event_specific_alert_string_lookup (const char *event_specific_alert_string_lookup_string)
{
  if (!strcasecmp (event_specific_alert_string_lookup_string, IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO_STRING))
    return IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_NO;

  if (!strcasecmp (event_specific_alert_string_lookup_string, IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES_STRING))
    return IPMI_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_YES;
  
  return -1;
}

char *
destination_type_to_string (int alert_destination_type)
{
  char *str = NULL;

  if (alert_destination_type == IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION)
    {
      if (!(str = strdup (IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  else if (alert_destination_type == IPMI_DESTINATION_TYPE_OEM1)
    {
      if (!(str = strdup (IPMI_DESTINATION_TYPE_OEM1_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }
  else if (alert_destination_type == IPMI_DESTINATION_TYPE_OEM2)
    {
      if (!(str = strdup (IPMI_DESTINATION_TYPE_OEM2_STRING)))
        {
          perror("strdup");
          return NULL;
        }
      return str;
    }

  if (!(str = strdup("")))
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_destination_type (const char *alert_destination_type_string)
{
  if (!strcasecmp (alert_destination_type_string, IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION_STRING))
    return IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION;
  if (!strcasecmp (alert_destination_type_string, IPMI_DESTINATION_TYPE_OEM1_STRING))
    return IPMI_DESTINATION_TYPE_OEM1;
  if (!strcasecmp (alert_destination_type_string, IPMI_DESTINATION_TYPE_OEM2_STRING))
    return IPMI_DESTINATION_TYPE_OEM2;
  
  return -1;
}

char *
alert_acknowledge_to_string (int alert_acknowledge)
{
  char *str = NULL;

  if (alert_acknowledge == IPMI_ALERT_ACKNOWLEDGED)
    {
      if (!(str = strdup (IPMI_ALERT_ACKNOWLEDGED_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else
    {
      if (!(str = strdup (IPMI_ALERT_UNACKNOWLEDGED_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }

  return str;
}

int 
string_to_alert_acknowledge (const char *alert_acknowledge_string)
{
  if (!strcasecmp (alert_acknowledge_string, IPMI_ALERT_UNACKNOWLEDGED_STRING))
    return IPMI_ALERT_UNACKNOWLEDGED;
  if (!strcasecmp (alert_acknowledge_string, IPMI_ALERT_ACKNOWLEDGED_STRING))
    return IPMI_ALERT_ACKNOWLEDGED;
  return -1;
}

char *
alert_acknowledge_timeout_to_string (int alert_acknowledge_timeout)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_acknowledge_timeout);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_alert_acknowledge_timeout (const char *alert_acknowledge_timeout_string)
{
  int n = 0;
  
  if (!str2int ((char *) alert_acknowledge_timeout_string, 0, &n))
    return n;
  
  return -1;
}

char *
alert_retries_to_string (int alert_retries)
{
  char *str = NULL;
  
  asprintf (&str, "%d", alert_retries);
  if (!str)
    {
      perror("strdup");
      return NULL;
    }
  return str;
}

int 
string_to_alert_retries (const char *alert_retries_string)
{
  int n  = 0;
  
  if (!str2int ((char *) alert_retries_string, 0, &n))
    return n;
  
  return -1;
}

char *
gateway_selector_to_string (int alert_gateway)
{
  char *str = NULL;

  if (alert_gateway == IPMI_GATEWAY_SELECTOR_DEFAULT)
    {
      if (!(str = strdup (IPMI_GATEWAY_SELECTOR_DEFAULT_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }
  else
    {
      if (!(str = strdup (IPMI_GATEWAY_SELECTOR_BACKUP_STRING)))
        {
          perror("strdup");
          return NULL;
        }
    }

  return str;
}

int 
string_to_gateway_selector (const char *alert_gateway_string)
{
  if (!strcasecmp (alert_gateway_string, IPMI_GATEWAY_SELECTOR_DEFAULT_STRING))
    return IPMI_GATEWAY_SELECTOR_DEFAULT;
  if (!strcasecmp (alert_gateway_string, IPMI_GATEWAY_SELECTOR_BACKUP_STRING))
    return IPMI_GATEWAY_SELECTOR_BACKUP;
  return -1;
}

char *
alert_ip_address_to_string (const char *alert_ip_address)
{
  char *str = NULL;

  if (!(str = strdup (alert_ip_address)))
    {
      perror("strdup");
      return NULL;
    }
  return str;
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

char *
alert_mac_address_to_string (const char *alert_mac_address)
{
  char *str = NULL;

  if (!(str = strdup (alert_mac_address)))
    {
      perror("strdup");
      return NULL;
    }
  
  return str;
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

