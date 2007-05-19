#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-sensor-api.h"

#include "pef-config.h"
#include "pef-config-common.h"
#include "pef-config-map.h"

int
alert_destination_type_number (const char *source)
{
  if (same (source, "pet_trap"))
    return IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION;
  if (same (source, "oem1"))
    return IPMI_DESTINATION_TYPE_OEM1;
  if (same (source, "oem2"))
    return IPMI_DESTINATION_TYPE_OEM2;
  return -1;
}

char *
alert_destination_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION:
      return "PET_Trap";
    case IPMI_DESTINATION_TYPE_OEM1:
      return "OEM1";
    case IPMI_DESTINATION_TYPE_OEM2:
      return "OEM2";
    }
  return "";
}

int
alert_gateway_number (const char *source)
{
  if (same (source, "default"))
    return IPMI_GATEWAY_SELECTOR_DEFAULT;
  if (same (source, "backup"))
    return IPMI_GATEWAY_SELECTOR_BACKUP;
  return -1;
}

char *
alert_gateway_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_GATEWAY_SELECTOR_DEFAULT:
      return "Default";
    case IPMI_GATEWAY_SELECTOR_BACKUP:
      return "Backup";
    }
  return "";
}

int
policy_type_number (const char *source)
{
  if (same (source, "always_send_to_this_destination"))
    return IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION;
  if (same (source, "proceed_to_next_entry"))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY;
  if (same (source, "do_not_proceed_any_more_entries"))
    return IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES;
  if (same (source, "proceed_to_next_entry_different_channel"))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL;
  if (same (source, "proceed_to_next_entry_different_destination_type"))
    return IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE;
  return -1;
}

char *
policy_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_ALERT_POLICY_ALWAYS_SEND_TO_THIS_DESTINATION:
      return "Always_Send_To_This_Destination";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY:
      return "Proceed_To_Next_Entry";
      break;
    case IPMI_ALERT_POLICY_DO_NOT_PROCEED_ANY_MORE_ENTRIES:
      return "Do_Not_Proceed_Any_More_Entries";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_CHANNEL:
      return "Proceed_To_Next_Entry_Different_Channel";
      break;
    case IPMI_ALERT_POLICY_PROCEED_TO_NEXT_ENTRY_DIFFERENT_DESTINATION_TYPE:
      return "Proceed_To_Next_Entry_Different_Destination_Type";
      break;
    }

  return "";
}

int 
filter_type_number (const char *source)
{
  if (same (source, "manufacturer_pre_configured"))
    return IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER;
  if (same (source, "software_configurable"))
    return IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER;
  if (same (source, "reserved1"))
    return 0x1;
  if (same (source, "reserved3"))
    return 0x3;

  return -1;
}

char *
filter_type_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_FILTER_CONFIGURATION_MANUFACTURER_PRE_CONFIGURED_FILTER:
      return "Manufacturer_Pre_Configured";
    case IPMI_FILTER_CONFIGURATION_SOFTWARE_CONFIGURABLE_FILTER:
      return "Software_Configurable";
    case 0x1:
      return "Reserved1";
    case 0x3:
      return "Reserved3";
    }
  return "";
}

int 
event_severity_number (const char *source)
{
  if (same (source, "unspecified"))
    return IPMI_EVENT_SEVERITY_UNSPECIFIED;
  if (same (source, "monitor"))
    return IPMI_EVENT_SEVERITY_MONITOR;
  if (same (source, "information"))
    return IPMI_EVENT_SEVERITY_INFORMATION;
  if (same (source, "ok"))
    return IPMI_EVENT_SEVERITY_OK;
  if (same (source, "non_critical"))
    return IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION;
  if (same (source, "critical"))
    return IPMI_EVENT_SEVERITY_CRITICAL_CONDITION;
  if (same (source, "non_recoverable"))
    return IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION;
  
  return -1;
}

char *
event_severity_string (uint8_t source)
{
  switch (source)
    {
    case IPMI_EVENT_SEVERITY_UNSPECIFIED:
      return "Unspecified";
    case IPMI_EVENT_SEVERITY_MONITOR:
      return "Monitor";
    case IPMI_EVENT_SEVERITY_INFORMATION:
      return "Information";
    case IPMI_EVENT_SEVERITY_OK:
      return "OK";
    case IPMI_EVENT_SEVERITY_NON_CRITICAL_CONDITION:
      return "Non_Critical";
    case IPMI_EVENT_SEVERITY_CRITICAL_CONDITION:
      return "Critical";
    case IPMI_EVENT_SEVERITY_NON_RECOVERABLE_CONDITION:
      return "Non_Recoverable";
    }
  return "";
}

