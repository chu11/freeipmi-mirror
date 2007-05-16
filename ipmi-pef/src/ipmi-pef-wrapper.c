#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <argp.h>

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-pef-and-alerting-cmds.h"
#include "freeipmi/ipmi-pef-param-spec.h"
#include "freeipmi/udm/ipmi-lan-cmds-udm.h"
#include "freeipmi/udm/ipmi-pef-and-alerting-cmds-udm.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"

#include "common-utils.h"

#include "ipmi-pef.h"
#include "ipmi-pef-utils.h"
#include "ipmi-pef-wrapper.h"

#define COMMENT_CHAR    '#'

#define GET_VALUE_STRING_BY_KEY_RETURN(__cache_record, __key, __value)  \
  do									\
    {									\
      char *local_value_string = NULL;					\
      if (_get_value_string_by_key (__cache_record,			\
				    __key,				\
				    &local_value_string) == -1)		\
	{								\
	  return (-1);							\
	}								\
      __value = strdupa (value_string);					\
      free (value_string);						\
    }									\
  while (0)

static int8_t lan_channel_number;
static int lan_channel_number_initialized = 0;

int
get_lan_channel_number (ipmi_device_t dev, int8_t *channel_num)
{
  if (lan_channel_number_initialized)
    {
      *channel_num = lan_channel_number;
      return 0;
    }

  if ((lan_channel_number = ipmi_get_channel_number (dev,
						     IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    return -1;

  lan_channel_number_initialized = 1;
  *channel_num = lan_channel_number;
  return 0;
}


static int 
_get_value_string_by_key (const char *cache_record, 
			  const char *key, 
			  char **value)
{
  const char *buf = NULL;
  
  buf = cache_record;
  
  while (buf)
    {
      char *line_pos = NULL;
      char *line = NULL;
      char *token = NULL;
      
      if ((line_pos = strchr (buf, '\n')) == NULL)
	{
	  line = strdupa (buf);
	  buf = NULL;
	}
      else 
	{
	  int len = line_pos - buf;
	  line = strndupa (buf, len);
	  buf += (len + 1);
	}
      
      if ((token = strsep_noempty (&line, STRING_WHITESPACES)) == NULL)
	continue;
      
      if (strcasecmp (token, key) == 0)
	{
	  *value = strdup (stripwhite (line));
	  return 0;
	}
    }
  
  return -1;
}

static int 
_record_string_to_evt (const char *record_string, 
		       pef_event_filter_table_t *evt)
{
  int int_value = 0;
  
  char *value_string = NULL;
  
  ERR_EINVAL (record_string && evt);
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  FILTER_NUMBER_KEY_STRING, 
				  value_string);
  if (string_to_filter_number (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       FILTER_NUMBER_KEY_STRING);
      return -1;
    }
  evt->filter_number = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  FILTER_TYPE_KEY_STRING, 
				  value_string);
  if (string_to_filter_type (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       FILTER_TYPE_KEY_STRING);
      return -1;
    }
  evt->filter_type = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  ENABLE_FILTER_KEY_STRING, 
				  value_string);
  if (string_to_enable_filter (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       ENABLE_FILTER_KEY_STRING);
      return -1;
    }
  evt->enable_filter = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_ALERT_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_alert (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_ALERT_KEY_STRING);
      return -1;
    }
  evt->event_filter_action_alert = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_POWER_OFF_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_power_off (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_POWER_OFF_KEY_STRING);
      return -1;
    }
  evt->event_filter_action_power_off = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_RESET_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_reset (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_RESET_KEY_STRING);
      return -1;
    }
  evt->event_filter_action_reset = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_POWER_CYCLE_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_power_cycle (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_POWER_CYCLE_KEY_STRING);
      return -1;
    }
  evt->event_filter_action_power_cycle = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_OEM_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_oem (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_OEM_KEY_STRING);
      return -1;
    }
  evt->event_filter_action_oem = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_diagnostic_interrupt (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_KEY_STRING);
      return -1;
    }
  evt->event_filter_action_diagnostic_interrupt = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_group_control_operation (value_string, 
							     &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_KEY_STRING);
      return -1;
    }
  evt->event_filter_action_group_control_operation = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  ALERT_POLICY_NUMBER_KEY_STRING, 
				  value_string);
  if (string_to_alert_policy_number (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       ALERT_POLICY_NUMBER_KEY_STRING);
      return -1;
    }
  evt->alert_policy_number = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  GROUP_CONTROL_SELECTOR_KEY_STRING, 
				  value_string);
  if (string_to_group_control_selector (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       GROUP_CONTROL_SELECTOR_KEY_STRING);
      return -1;
    }
  evt->group_control_selector = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_SEVERITY_KEY_STRING, 
				  value_string);
  if (string_to_event_severity (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_SEVERITY_KEY_STRING);
      return -1;
    }
  evt->event_severity = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  GENERATOR_ID_BYTE1_KEY_STRING, 
				  value_string);
  if (string_to_generator_id_byte1 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       GENERATOR_ID_BYTE1_KEY_STRING);
      return -1;
    }
  evt->generator_id_byte1 = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  GENERATOR_ID_BYTE2_KEY_STRING, 
				  value_string);
  if (string_to_generator_id_byte2 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       GENERATOR_ID_BYTE2_KEY_STRING);
      return -1;
    }
  evt->generator_id_byte2 = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  SENSOR_TYPE_KEY_STRING, 
				  value_string);
  if (string_to_sensor_type (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       SENSOR_TYPE_KEY_STRING);
      return -1;
    }
  evt->sensor_type = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  SENSOR_NUMBER_KEY_STRING, 
				  value_string);
  if (string_to_sensor_number (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       SENSOR_NUMBER_KEY_STRING);
      return -1;
    }
  evt->sensor_number = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_TRIGGER_KEY_STRING, 
				  value_string);
  if (string_to_event_trigger (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_TRIGGER_KEY_STRING);
      return -1;
    }
  evt->event_trigger = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_OFFSET_MASK_KEY_STRING, 
				  value_string);
  if (string_to_event_data1_offset_mask (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_OFFSET_MASK_KEY_STRING);
      return -1;
    }
  evt->event_data1_offset_mask = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_AND_MASK_KEY_STRING, 
				  value_string);
  if (string_to_event_data1_AND_mask (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_AND_MASK_KEY_STRING);
      return -1;
    }
  evt->event_data1_AND_mask = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_COMPARE1_KEY_STRING, 
				  value_string);
  if (string_to_event_data1_compare1 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_COMPARE1_KEY_STRING);
      return -1;
    }
  evt->event_data1_compare1 = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_COMPARE2_KEY_STRING, 
				  value_string);
  if (string_to_event_data1_compare2 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_COMPARE2_KEY_STRING);
      return -1;
    }
  evt->event_data1_compare2 = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA2_AND_MASK_KEY_STRING, 
				  value_string);
  if (string_to_event_data2_AND_mask (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA2_AND_MASK_KEY_STRING);
      return -1;
    }
  evt->event_data2_AND_mask = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA2_COMPARE1_KEY_STRING, 
				  value_string);
  if (string_to_event_data2_compare1 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA2_COMPARE1_KEY_STRING);
      return -1;
    }
  evt->event_data2_compare1 = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA2_COMPARE2_KEY_STRING, 
				  value_string);
  if (string_to_event_data2_compare2 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA2_COMPARE2_KEY_STRING);
      return -1;
    }
  evt->event_data2_compare2 = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA3_AND_MASK_KEY_STRING, 
				  value_string);
  if (string_to_event_data3_AND_mask (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA3_AND_MASK_KEY_STRING);
      return -1;
    }
  evt->event_data3_AND_mask = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA3_COMPARE1_KEY_STRING, 
				  value_string);
  if (string_to_event_data3_compare1 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA3_COMPARE1_KEY_STRING);
      return -1;
    }
  evt->event_data3_compare1 = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA3_COMPARE2_KEY_STRING, 
				  value_string);
  if (string_to_event_data3_compare2 (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA3_COMPARE2_KEY_STRING);
      return -1;
    }
  evt->event_data3_compare2 = int_value;
  
  return 0;
}

static int 
_record_string_to_apt (const char *record_string, 
		       pef_alert_policy_table_t *apt)
{
  int int_value = 0;
  
  char *value_string = NULL;
  
  ERR_EINVAL (record_string && apt);
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_ALERT_POLICY_NUMBER_KEY_STRING, 
				  value_string);
  if (string_to_policy_type (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_ALERT_POLICY_NUMBER_KEY_STRING);
      return -1;
    }
  apt->alert_policy_number = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_POLICY_TYPE_KEY_STRING, 
				  value_string);
  if (string_to_policy_type (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_POLICY_TYPE_KEY_STRING);
      return -1;
    }
  apt->policy_type = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_POLICY_ENABLED_KEY_STRING, 
				  value_string);
  if (string_to_policy_enabled (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_POLICY_ENABLED_KEY_STRING);
      return -1;
    }
  apt->policy_enabled = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_POLICY_NUMBER_KEY_STRING, 
				  value_string);
  if (string_to_enable_filter (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_POLICY_NUMBER_KEY_STRING);
      return -1;
    }
  apt->policy_number = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_DESTINATION_SELECTOR_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_alert (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_DESTINATION_SELECTOR_KEY_STRING);
      return -1;
    }
  apt->destination_selector = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_CHANNEL_NUMBER_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_power_off (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_CHANNEL_NUMBER_KEY_STRING);
      return -1;
    }
  apt->channel_number = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_ALERT_STRING_SET_SELECTOR_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_reset (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_ALERT_STRING_SET_SELECTOR_KEY_STRING);
      return -1;
    }
  apt->alert_string_set_selector = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_KEY_STRING, 
				  value_string);
  if (string_to_event_filter_action_power_cycle (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       APT_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_KEY_STRING);
      return -1;
    }
  apt->event_specific_alert_string_lookup = int_value;
  
  return 0;
}

static int 
_record_string_to_lad (const char *record_string, 
		       lan_alert_destination_t *lad)
{
  int int_value = 0;
  
  char *value_string = NULL;
  
  ERR_EINVAL (record_string && lad);
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_DESTINATION_SELECTOR_KEY_STRING, 
				  value_string);
  if (string_to_destination_selector (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_DESTINATION_SELECTOR_KEY_STRING);
      return -1;
    }
  lad->destination_selector = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_DESTINATION_TYPE_KEY_STRING, 
				  value_string);
  if (string_to_destination_type (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_DESTINATION_TYPE_KEY_STRING);
      return -1;
    }
  lad->destination_type = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_ACKNOWLEDGE_KEY_STRING, 
				  value_string);
  if (string_to_alert_acknowledge (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_ACKNOWLEDGE_KEY_STRING);
      return -1;
    }
  lad->alert_acknowledge = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_ACKNOWLEDGE_TIMEOUT_KEY_STRING, 
				  value_string);
  if (string_to_alert_acknowledge_timeout (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_ACKNOWLEDGE_TIMEOUT_KEY_STRING);
      return -1;
    }
  lad->alert_acknowledge_timeout = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_RETRIES_KEY_STRING, 
				  value_string);
  if (string_to_alert_retries (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_RETRIES_KEY_STRING);
      return -1;
    }
  lad->alert_retries = int_value;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_GATEWAY_KEY_STRING, 
				  value_string);
  if (string_to_gateway_selector (value_string, &int_value) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_GATEWAY_KEY_STRING);
      return -1;
    }
  lad->gateway_selector = int_value;
  
  {
    char *str = NULL;
    GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				    LAD_ALERT_IP_ADDRESS_KEY_STRING, 
				    value_string);
    if (string_to_alert_ip_address (value_string, &str) != 0)
      {
	fprintf (stderr, 
		 "invalid value %s for %s\n", 
		 value_string, 
		 LAD_ALERT_IP_ADDRESS_KEY_STRING);
	return -1;
      }
    strncpy (lad->alert_ip_address, str, 15);
    free (str);
    
    GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				    LAD_ALERT_MAC_ADDRESS_KEY_STRING, 
				    value_string);
    if (string_to_alert_mac_address (value_string, &str) != 0)
    {
      fprintf (stderr, 
	       "invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_MAC_ADDRESS_KEY_STRING);
      return -1;
    }
    strncpy (lad->alert_mac_address, str, 17);
    free (str);
  }
  
  return 0;
}

static int 
_fread_record (FILE *fp, char **cache_record)
{
  char *record;
  
  record = NULL;
  
  while (1)
    {
      char *line = NULL;
      size_t n = 0;
      
      if (getline (&line, &n, fp) == -1)
	{
	  return (-1);
	}
      else 
	{
	  char *lineptr = line;
	  char *tmp_lineptr = line;
	  line = strdupa (stripwhite (tmp_lineptr));
	  free (lineptr);
	}
      if (strlen (line) == 0)
	{
	  *cache_record = NULL;
	  if (record)
	    {
	      *cache_record = strdup (record);
	    }
	  return 0;
	}
      
      if (line[0] == COMMENT_CHAR)
	{
	  continue;
	}
      
      {
	char *l_record = NULL;
	int len = 0;
	
	if (record)
	  {
	    len = strlen (record) + strlen (line) + 2;
	    l_record = alloca (len);
	    strcpy (l_record, record);
	    strcat (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
	else 
	  {
	    len = strlen (line) + 2;
	    l_record = alloca (len);
	    strcpy (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
      }
    }
  
  return 0;
}

static int 
_get_record_count (FILE *fp, int *count)
{
  char *cache_record = NULL;
  int rcount = 0;
  
  while (1)
    {
      cache_record = NULL;
      if (_fread_record (fp, &cache_record) == -1)
	{
	  *count = rcount;
	  return 0;
	}
      rcount++;
      free (cache_record);
    }
  
  return (-1);
}

int 
get_pef_info (ipmi_device_t dev, pef_info_t *pef_info)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  ERR_EINVAL (dev && pef_info);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_pef_capabilities_rs);
  
  if (ipmi_cmd_get_pef_capabilities (dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "pef_version_major", &val);
  pef_info->pef_version_major = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "pef_version_minor", &val);
  pef_info->pef_version_minor = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.alert", &val);
  pef_info->alert_action_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.power_down", &val);
  pef_info->power_down_action_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.reset", &val);
  pef_info->reset_action_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.power_cycle", &val);
  pef_info->power_cycle_action_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.oem_action", &val);
  pef_info->oem_action_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.diagnostic_interrupt", &val);
  pef_info->diagnostic_interrupt_action_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "oem_event_record_filtering_supported", &val);
  pef_info->oem_event_record_filtering_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_event_filter_table_entries", &val);
  pef_info->eft_entries_count = val;
  
  if (pef_info->alert_action_support)
    {
      FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
      
      FIID_OBJ_CREATE (obj_cmd_rs, 
		       tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs);
      if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (dev, 
									     IPMI_GET_PEF_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) != 0)
	goto cleanup;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_event_filters", &val);
      pef_info->num_event_filters = val;
      FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
      
      FIID_OBJ_CREATE (obj_cmd_rs, 
		       tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs);
      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (dev, 
										    IPMI_GET_PEF_PARAMETER, 
										    SET_SELECTOR, 
										    BLOCK_SELECTOR, 
										    obj_cmd_rs) != 0)
	goto cleanup;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_alert_policy_entries", &val);
      pef_info->num_alert_policies = val;
      FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
      
      FIID_OBJ_CREATE (obj_cmd_rs, 
		       tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs);
      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings (dev, 
									     IPMI_GET_PEF_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) != 0)
	goto cleanup;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_alert_strings", &val);
      pef_info->num_alert_strings = val;
    }
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

int 
get_event_filter_table (ipmi_device_t dev, int filter, pef_event_filter_table_t *evt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  ERR_EINVAL (dev && evt);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_event_filter_table_rs);
  
  if (ipmi_cmd_get_pef_configuration_parameters_event_filter_table (dev,
								    IPMI_GET_PEF_PARAMETER,
								    filter,
								    BLOCK_SELECTOR,
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "filter_number", &val);
  evt->filter_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "filter_configuration.type", &val);
  evt->filter_type = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "filter_configuration.filter", &val);
  evt->enable_filter = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.alert", &val);
  evt->event_filter_action_alert = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.power_off", &val);
  evt->event_filter_action_power_off = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.reset", &val);
  evt->event_filter_action_reset = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.power_cycle", &val);
  evt->event_filter_action_power_cycle = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.oem", &val);
  evt->event_filter_action_oem = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.diagnostic_interrupt", &val);
  evt->event_filter_action_diagnostic_interrupt = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.group_control_operation", &val);
  evt->event_filter_action_group_control_operation = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_policy_number.policy_number", &val);
  evt->alert_policy_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_policy_number.group_control_selector", &val);
  evt->group_control_selector = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_severity", &val);
  evt->event_severity = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "generator_id_byte1", &val);
  evt->generator_id_byte1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "generator_id_byte2", &val);
  evt->generator_id_byte2 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sensor_type", &val);
  evt->sensor_type = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sensor_number", &val);
  evt->sensor_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_trigger", &val);
  evt->event_trigger = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_offset_mask", &val);
  evt->event_data1_offset_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_AND_mask", &val);
  evt->event_data1_AND_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_compare1", &val);
  evt->event_data1_compare1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_compare2", &val);
  evt->event_data1_compare2 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data2_AND_mask", &val);
  evt->event_data2_AND_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data2_compare1", &val);
  evt->event_data2_compare1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data2_compare2", &val);
  evt->event_data2_compare2 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data3_AND_mask", &val);
  evt->event_data3_AND_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data3_compare1", &val);
  evt->event_data3_compare1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data3_compare2", &val);
  evt->event_data3_compare2 = val;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
set_event_filter_table (ipmi_device_t dev, pef_event_filter_table_t *evt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  
  ERR_EINVAL (dev && evt);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  
  if (ipmi_cmd_set_pef_configuration_parameters_event_filter_table (dev, 
								    evt->filter_number, 
								    evt->filter_type, 
								    evt->enable_filter, 
								    evt->event_filter_action_alert, 
								    evt->event_filter_action_power_off, 
								    evt->event_filter_action_reset, 
								    evt->event_filter_action_power_cycle, 
								    evt->event_filter_action_oem, 
								    evt->event_filter_action_diagnostic_interrupt, 
								    evt->event_filter_action_group_control_operation, 
								    evt->alert_policy_number, 
								    evt->group_control_selector, 
								    evt->event_severity, 
								    evt->generator_id_byte1, 
								    evt->generator_id_byte2, 
								    evt->sensor_type, 
								    evt->sensor_number, 
								    evt->event_trigger, 
								    evt->event_data1_offset_mask, 
								    evt->event_data1_AND_mask, 
								    evt->event_data1_compare1, 
								    evt->event_data1_compare2, 
								    evt->event_data2_AND_mask, 
								    evt->event_data2_compare1, 
								    evt->event_data2_compare2, 
								    evt->event_data3_AND_mask, 
								    evt->event_data3_compare1, 
								    evt->event_data3_compare2, 
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
get_number_of_event_filters (ipmi_device_t dev, int *num_event_filters)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs);
  if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (dev, 
									 IPMI_GET_PEF_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) != 0)
    goto cleanup;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_event_filters", &val);
  *num_event_filters = val;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
get_evt_list (FILE *fp, pef_event_filter_table_t **evt_list, int *count)
{
  pef_event_filter_table_t *l_evt_list = NULL;
  int l_count;
  char *record = NULL;
  int i;
  int rv;
  
  if (!(fp && evt_list && count))
    return (-1);
  
  if (_get_record_count (fp, &l_count) != 0)
    return (-1);
  
  l_evt_list = (pef_event_filter_table_t *) calloc (l_count, 
						    sizeof (pef_event_filter_table_t));
  
  fseek (fp, 0, SEEK_SET);
  
  for (i = 0; i < l_count; i++)
    {
      _fread_record (fp, &record);
      rv = _record_string_to_evt (record, &l_evt_list[i]);
      free (record);
      if (rv != 0)
	{
	  free (l_evt_list);
	  return (-1);
	}
    }
  
  *evt_list = l_evt_list;
  *count = l_count;
  
  return 0;
}

int 
get_number_of_alert_policy_entries (ipmi_device_t dev, int *num_alert_policy_entries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs);
  if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (dev, 
										IPMI_GET_PEF_PARAMETER, 
										SET_SELECTOR, 
										BLOCK_SELECTOR, 
										obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_alert_policy_entries", &val);
  *num_alert_policy_entries = val;
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
get_alert_policy_table (ipmi_device_t dev, 
			int policy_number, 
			pef_alert_policy_table_t *apt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  ERR_EINVAL (dev && apt);
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_pef_configuration_parameters_alert_policy_table_rs);
  
  if (ipmi_cmd_get_pef_configuration_parameters_alert_policy_table (dev, 
								    IPMI_GET_PEF_PARAMETER,
								    policy_number, 
								    BLOCK_SELECTOR, 
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_policy_entry_number", &val);
  apt->alert_policy_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "policy_number.policy_type", &val);
  apt->policy_type = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "policy_number.enabled", &val);
  apt->policy_enabled = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "policy_number.policy_number", &val);
  apt->policy_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "channel_destination.destination_selector", &val);
  apt->destination_selector = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "channel_destination.channel_number", &val);
  apt->channel_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_string_key.alert_string_set_selector", &val);
  apt->alert_string_set_selector = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_string_key.event_specific_alert_string_lookup", &val);
  apt->event_specific_alert_string_lookup = val;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
get_apt_list (FILE *fp, pef_alert_policy_table_t **apt_list, int *count)
{
  pef_alert_policy_table_t *l_apt_list = NULL;
  int l_count;
  char *record = NULL;
  int i;
  int rv;
  
  if (!(fp && apt_list && count))
    return (-1);
  
  if (_get_record_count (fp, &l_count) != 0)
    return (-1);
  
  l_apt_list = (pef_alert_policy_table_t *) calloc (l_count, 
						    sizeof (pef_alert_policy_table_t));
  
  fseek (fp, 0, SEEK_SET);
  
  for (i = 0; i < l_count; i++)
    {
      _fread_record (fp, &record);
      rv = _record_string_to_apt (record, &l_apt_list[i]);
      free (record);
      if (rv != 0)
	{
	  free (l_apt_list);
	  return (-1);
	}
    }
  
  *apt_list = l_apt_list;
  *count = l_count;
  
  return 0;
}

int 
set_alert_policy_table (ipmi_device_t dev, pef_alert_policy_table_t *apt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  
  ERR_EINVAL (dev && apt);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  if (ipmi_cmd_set_pef_configuration_parameters_alert_policy_table (dev, 
								    apt->alert_policy_number, 
								    apt->policy_type, 
								    apt->policy_enabled, 
								    apt->policy_number, 
								    apt->destination_selector, 
								    apt->channel_number, 
								    apt->alert_string_set_selector, 
								    apt->event_specific_alert_string_lookup, 
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int
get_bmc_community_string (ipmi_device_t dev,
			  uint8_t *community_string, 
			  uint32_t community_string_len) 
{ 
  fiid_obj_t obj_cmd_rs = NULL; 
  int8_t channel_number; 
  int rv = -1;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_community_string_rs))) 
    goto cleanup; 

  if (get_lan_channel_number (dev, &channel_number) < 0)
    goto cleanup; 
 
  if (ipmi_cmd_get_lan_configuration_parameters_community_string (dev,  
								  channel_number,  
								  IPMI_GET_LAN_PARAMETER,  
								  SET_SELECTOR,  
								  BLOCK_SELECTOR,  
								  obj_cmd_rs) < 0)  
    goto cleanup; 
  
  if (fiid_obj_get_data (obj_cmd_rs,  
			 "community_string",  
			 community_string, 
			 community_string_len) < 0) 
    goto cleanup; 
  
  rv = 0;
 cleanup: 
  if (obj_cmd_rs) 
    fiid_obj_destroy(obj_cmd_rs); 
  return (rv); 
} 

int
set_bmc_community_string (ipmi_device_t dev,
			  uint8_t *community_string) 
{ 
  fiid_obj_t obj_cmd_rs = NULL; 
  int8_t channel_number; 
  int rv = -1;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs))) 
    goto cleanup; 

  if (get_lan_channel_number (dev, &channel_number) < 0)
    goto cleanup; 

  if (ipmi_cmd_set_lan_configuration_parameters_community_string (dev,  
								  channel_number, 
								  community_string,  
								  (community_string) ? strlen((char *)community_string) : 0, 
								  obj_cmd_rs) < 0) 
    goto cleanup; 

  rv = 0;
 cleanup: 
  if (obj_cmd_rs) 
    fiid_obj_destroy(obj_cmd_rs); 
  return (rv); 
} 

/********************************************************************************/

int 
get_number_of_lan_destinations (ipmi_device_t dev, int *number_of_lan_destinations)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  uint64_t val;
  int8_t channel_number;
  
  ERR_EINVAL (dev && number_of_lan_destinations);
  
  if (get_lan_channel_number (dev, &channel_number) < 0)
    goto cleanup; 
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs);
  
  if (ipmi_cmd_get_lan_configuration_parameters_number_of_destinations (dev, 
									channel_number, 
									IPMI_GET_LAN_PARAMETER, 
									SET_SELECTOR, 
									BLOCK_SELECTOR, 
									obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_lan_destinations", &val);
  *number_of_lan_destinations = val;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return rv;
}

int 
get_lan_alert_destination (ipmi_device_t dev, 
			   int destination_selector, 
			   lan_alert_destination_t *lad)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  int8_t channel_number = 7;
  uint8_t alert_ip_address_bytes[4];
  uint8_t alert_mac_address_bytes[6];
  
  ERR_EINVAL (dev && lad);
  
  if (get_lan_channel_number (dev, &channel_number) < 0)
    goto cleanup; 
  
  lad->destination_selector = destination_selector;
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_lan_configuration_parameters_destination_type_rs);
  
  if (ipmi_cmd_get_lan_configuration_parameters_destination_type (dev, 
                                                                  channel_number, 
                                                                  IPMI_GET_LAN_PARAMETER, 
                                                                  destination_selector, 
                                                                  BLOCK_SELECTOR, 
                                                                  obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "destination_type", &val);
  lad->destination_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_acknowledge", &val);
  lad->alert_acknowledge = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_acknowledge_timeout", &val);
  lad->alert_acknowledge_timeout = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "retries", &val);
  lad->alert_retries = val;
  
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs);
  
  if (ipmi_cmd_get_lan_configuration_parameters_destination_addresses (dev, 
                                                                       channel_number, 
                                                                       IPMI_GET_LAN_PARAMETER, 
                                                                       destination_selector, 
                                                                       BLOCK_SELECTOR, 
                                                                       obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "gateway_selector", &val);
  lad->gateway_selector = val;
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, 
			     "alerting_ip_address", 
			     alert_ip_address_bytes, 
			     4);
  snprintf (lad->alert_ip_address, 
	    15, 
	    "%u.%u.%u.%u", 
	    alert_ip_address_bytes[0], 
	    alert_ip_address_bytes[1], 
	    alert_ip_address_bytes[2], 
	    alert_ip_address_bytes[3]);
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, 
                             "alerting_mac_address", 
                             alert_mac_address_bytes, 
                             6);
  snprintf (lad->alert_mac_address, 
	    17, 
	    "%02X:%02X:%02X:%02X:%02X:%02X", 
	    alert_mac_address_bytes[0],
	    alert_mac_address_bytes[1],
	    alert_mac_address_bytes[2],
	    alert_mac_address_bytes[3],
	    alert_mac_address_bytes[4],
	    alert_mac_address_bytes[5]);
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
get_lad_list (FILE *fp, lan_alert_destination_t **lad_list, int *count)
{
  lan_alert_destination_t *l_lad_list = NULL;
  int l_count;
  char *record = NULL;
  int i;
  int rv;
  
  if (!(fp && lad_list && count))
    return (-1);
  
  if (_get_record_count (fp, &l_count) != 0)
    return (-1);
  
  l_lad_list = (lan_alert_destination_t *) calloc (l_count, 
						   sizeof (lan_alert_destination_t));
  
  fseek (fp, 0, SEEK_SET);
  
  for (i = 0; i < l_count; i++)
    {
      _fread_record (fp, &record);
      rv = _record_string_to_lad (record, &l_lad_list[i]);
      free (record);
      if (rv != 0)
	{
	  free (l_lad_list);
	  return (-1);
	}
    }
  
  *lad_list = l_lad_list;
  *count = l_count;
  
  return 0;
}

int 
set_lan_alert_destination (ipmi_device_t dev, lan_alert_destination_t *lad)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  int8_t channel_number = 7;
  uint32_t alert_ip_address_bytes = 0;
  uint64_t alert_mac_address_bytes = 0;
  
  ERR_EINVAL (dev && lad);
  
  if (get_lan_channel_number (dev, &channel_number) < 0)
    goto cleanup; 
  
  if (ipmi_ipv4_address_string2int (lad->alert_ip_address, &alert_ip_address_bytes) < 0)
    goto cleanup;
  
  if (ipmi_mac_address_string2int (lad->alert_mac_address, &alert_mac_address_bytes) < 0)
    goto cleanup;
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);
  if (ipmi_cmd_set_lan_configuration_parameters_destination_type (dev,
                                                                  channel_number,
                                                                  lad->destination_selector,
                                                                  lad->destination_type,
                                                                  lad->alert_acknowledge,
                                                                  lad->alert_acknowledge_timeout,
                                                                  lad->alert_retries,
                                                                  obj_cmd_rs) != 0)
    goto cleanup;
  
  if (ipmi_cmd_set_lan_configuration_parameters_destination_addresses (dev, 
                                                                       channel_number,
                                                                       lad->destination_selector,
                                                                       lad->gateway_selector,
                                                                       alert_ip_address_bytes,
                                                                       alert_mac_address_bytes,
                                                                       obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}
