#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "fiid-wrappers.h"

#include "ipmi-pef.h"
#include "ipmi-pef-checkout.h"
#include "ipmi-pef-keys.h"
#include "ipmi-pef-map.h"
#include "ipmi-pef-utils.h"
#include "ipmi-pef-wrapper.h"

int 
checkout_pef_community_string (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };

  if (get_bmc_community_string (state_data,
				community_string,
				IPMI_MAX_COMMUNITY_STRING_LENGTH+1) < 0)
    {
      if (state_data->prog_data->args->verbose_wanted)
        fprintf (fp, "## FATAL: Unable to get community string\n");
      return -1;
    }
  
  fprintf (fp, 
	   "## Give valid string\n");
  fprintf (fp, 
	   "%-30s %s\n", 
	   COMMUNITY_STRING_KEY_STRING,
	   community_string);
  fprintf (fp,
           "\n");

  return 0;
}

int 
checkout_pef_lan_alert_destination (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  int rv = 0;
  int8_t num_of_lan_alert_destinations;
  int d;

  if (get_number_of_lan_alert_destinations (state_data, &num_of_lan_alert_destinations) != PEF_ERR_SUCCESS)
    return (-1);

  for (d = 1; d <= num_of_lan_alert_destinations; d++)
    {
      lan_alert_destination_t lad;
      char *str = NULL;
      
      memset (&lad, 0, sizeof (lan_alert_destination_t));
      
      if (get_lan_alert_destination (state_data, d, &lad) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to get LAN alert destination #%d\n", d);
	  rv = -1;
	  continue;
	}
      
      if (!(str = destination_selector_to_string (lad.destination_selector)))
        {
          return -1;
        }

      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_DESTINATION_SELECTOR_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = destination_type_to_string (lad.destination_type)))
        return -1;

      fprintf (fp, 
               "## Possible values: PET_Trap/OEM1/OEM2\n");
      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_DESTINATION_TYPE_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = alert_acknowledge_to_string (lad.alert_acknowledge)))
        return -1;

      fprintf (fp, 
               "## Possible values: Yes/No\n");
      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_ACKNOWLEDGE_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = alert_acknowledge_timeout_to_string (lad.alert_acknowledge_timeout)))
        return -1;

      fprintf (fp, 
               "## Give valid unsigned number in seconds\n");
      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_ACKNOWLEDGE_TIMEOUT_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = alert_retries_to_string (lad.alert_retries)))
        return -1;

      fprintf (fp, 
               "## Give valid unsigned number\n");
      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_RETRIES_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = gateway_selector_to_string (lad.gateway_selector)))
        return -1;

      fprintf (fp, 
               "## Possible values: Default/Backup\n");
      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_GATEWAY_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = alert_ip_address_to_string (lad.alert_ip_address)))
        return -1;

      fprintf (fp, 
               "## Give valid IP address\n");
      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_IP_ADDRESS_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = alert_mac_address_to_string (lad.alert_mac_address)))
        return -1;
      
      fprintf (fp, 
               "## Give valid MAC address\n");
      fprintf (fp, 
               "%-30s %s\n", 
               LAD_ALERT_MAC_ADDRESS_KEY_STRING, 
               str);
      free (str);
      
      fprintf (fp, "\n");
    }
  
  return rv;
}

int 
checkout_pef_alert_policy_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  int rv = 0;
  int8_t number_of_alert_policy_entries;
  int entry;
  
  if (get_number_of_alert_policy_entries (state_data, &number_of_alert_policy_entries) != PEF_ERR_SUCCESS)
    return (-1);
  
  for (entry = 1; entry <= number_of_alert_policy_entries; entry++)
    {
      pef_alert_policy_table_t apt;
      char *str = NULL;
      
      memset (&apt, 0, sizeof (pef_alert_policy_table_t));
      
      if (get_alert_policy_table (state_data, entry, &apt) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to get alert policy table #%d\n", entry);
	  rv = -1;
	  continue;
	}
      
      if (!(str = alert_policy_entry_number_to_string (apt.alert_policy_entry_number)))
        return -1;

      fprintf (fp, 
               "%-30s %s\n", 
               APT_ALERT_POLICY_ENTRY_NUMBER_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = policy_type_to_string (apt.policy_type)))
        return -1;

      fprintf (fp, 
               "## Possible values: Always_Send_To_This_Destination/Proceed_To_Next_Entry/Do_Not_Proceed_Any_More_Entries/Proceed_To_Next_Entry_Different_Channel/Proceed_To_Next_Entry_Different_Destina\n");
      fprintf (fp, 
               "%-30s %s\n", 
               APT_POLICY_TYPE_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = policy_enabled_to_string (apt.policy_enabled)))
        return -1;

      fprintf (fp, 
               "%-30s %s\n", 
               APT_POLICY_ENABLED_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = policy_number_to_string (apt.policy_number)))
        return -1;
      
      fprintf (fp, 
               "%-30s %s\n", 
               APT_POLICY_NUMBER_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = destination_selector_to_string (apt.destination_selector)))
        return -1;

      fprintf (fp, 
               "%-30s %s\n", 
               APT_DESTINATION_SELECTOR_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = channel_number_to_string (apt.channel_number)))
        return -1;

      fprintf (fp, 
               "%-30s %s\n", 
               APT_CHANNEL_NUMBER_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = alert_string_set_selector_to_string (apt.alert_string_set_selector)))
        return -1;

      fprintf (fp, 
               "%-30s %s\n", 
               APT_ALERT_STRING_SET_SELECTOR_KEY_STRING, 
               str);
      free (str);
      
      if (!(str = event_specific_alert_string_lookup_to_string (apt.event_specific_alert_string_lookup)))
        return -1;

      fprintf (fp, 
               "%-30s %s\n", 
               APT_EVENT_SPECIFIC_ALERT_STRING_LOOKUP_KEY_STRING, 
               str);
      free (str);
      
      fprintf (fp, "\n");
    }
  
  return rv;
}

int 
checkout_pef_event_filter_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  int rv = 0;
  int8_t num_event_filters;
  int filter;
  
  if (get_number_of_event_filters (state_data, &num_event_filters) != PEF_ERR_SUCCESS)
    return (-1);
  
  for (filter = 1; filter <= num_event_filters; filter++)
    {
      pef_event_filter_table_t eft;
      char *str = NULL;
      
      memset (&eft, 0, sizeof (pef_event_filter_table_t));
      
      if (get_event_filter_table (state_data, filter, &eft) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to get event filter table #%d\n", filter);
	  rv = -1;
	  continue;
	}
      
      if (!(str = filter_number_to_string (eft.filter_number)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               FILTER_NUMBER_KEY_STRING, 
               str);
      free (str);

      if (!(str = filter_type_to_string (eft.filter_type)))
        return -1;

      fprintf (fp, 
               "## Possible values: Manufacturer_Pre_Configured/Software_Configurable\n");
      fprintf (fp, 
               "%-50s %s\n", 
               FILTER_TYPE_KEY_STRING, 
               str);
      free (str);

      if (!(str = enable_filter_to_string (eft.enable_filter)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               ENABLE_FILTER_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_filter_action_alert_to_string (eft.event_filter_action_alert)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_FILTER_ACTION_ALERT_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_filter_action_power_off_to_string (eft.event_filter_action_power_off)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_FILTER_ACTION_POWER_OFF_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_filter_action_reset_to_string (eft.event_filter_action_reset)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_FILTER_ACTION_RESET_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_filter_action_power_cycle_to_string (eft.event_filter_action_power_cycle)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_FILTER_ACTION_POWER_CYCLE_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_filter_action_oem_to_string (eft.event_filter_action_oem)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_FILTER_ACTION_OEM_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_filter_action_diagnostic_interrupt_to_string (eft.event_filter_action_diagnostic_interrupt)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_filter_action_group_control_operation_to_string (eft.event_filter_action_group_control_operation)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_KEY_STRING, 
               str);
      free (str);

      if (!(str = alert_policy_number_to_string (eft.alert_policy_number)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               ALERT_POLICY_NUMBER_KEY_STRING, 
               str);
      free (str);

      if (!(str = group_control_selector_to_string (eft.group_control_selector)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               GROUP_CONTROL_SELECTOR_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_severity_to_string (eft.event_severity)))
        return -1;

      fprintf (fp, 
               "## Possible values: Unspecified/Monitor/Information/OK/Non_Critical/Critical/Non_Recoverable\n");
      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_SEVERITY_KEY_STRING, 
               str);
      free (str);

      if (!(str = generator_id_byte1_to_string (eft.generator_id_byte1)))
        return -1;

      fprintf (fp, 
               "## Possible values: Hex value or Match_Any\n");
      fprintf (fp, 
               "%-50s %s\n", 
               GENERATOR_ID_BYTE1_KEY_STRING, 
               str);
      free (str);

      if (!(str = generator_id_byte2_to_string (eft.generator_id_byte2)))
        return -1;

      fprintf (fp, 
               "## Possible values: Hex value or Match_Any\n");
      fprintf (fp, 
               "%-50s %s\n", 
               GENERATOR_ID_BYTE2_KEY_STRING, 
               str);
      free (str);

      if (!(str = sensor_type_to_string (eft.sensor_type)))
        return -1;

      fprintf (fp, 
               "## Possible values: Sensor type or Match_Any\n");
      fprintf (fp, 
               "%-50s %s\n", 
               SENSOR_TYPE_KEY_STRING, 
               str);
      free (str);

      if (!(str = sensor_number_to_string (eft.sensor_number)))
        return -1;

      fprintf (fp, 
               "## Possible values: Sensor number or Match_Any\n");
      fprintf (fp, 
               "%-50s %s\n", 
               SENSOR_NUMBER_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_trigger_to_string (eft.event_trigger)))
        return -1;

      fprintf (fp, 
               "## Possible values: Unspecified/Threshold/Generic_Discrete_0xXX/Sensor_Specific_Discrete/OEM_0xXX/Match_Any\n");
      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_TRIGGER_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data1_offset_mask_to_string (eft.event_data1_offset_mask)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA1_OFFSET_MASK_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data1_AND_mask_to_string (eft.event_data1_AND_mask)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA1_AND_MASK_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data1_compare1_to_string (eft.event_data1_compare1)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA1_COMPARE1_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data1_compare2_to_string (eft.event_data1_compare2)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA1_COMPARE2_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data2_AND_mask_to_string (eft.event_data2_AND_mask)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA2_AND_MASK_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data2_compare1_to_string (eft.event_data2_compare1)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA2_COMPARE1_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data2_compare2_to_string (eft.event_data2_compare2)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA2_COMPARE2_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data3_AND_mask_to_string (eft.event_data3_AND_mask)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA3_AND_MASK_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data3_compare1_to_string (eft.event_data3_compare1)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA3_COMPARE1_KEY_STRING, 
               str);
      free (str);

      if (!(str = event_data3_compare2_to_string (eft.event_data3_compare2)))
        return -1;

      fprintf (fp, 
               "%-50s %s\n", 
               EVENT_DATA3_COMPARE2_KEY_STRING, 
               str);
      free (str);

      fprintf (fp, "\n");
    }
  
  return rv;
}

pef_err_t
pef_checkout (ipmi_pef_state_data_t *state_data)
{
  FILE *fp = NULL;
  int file_opened = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  struct ipmi_pef_arguments *args;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      fp = fopen (args->filename, "w");
      if (fp == NULL)
        {
          fprintf (stderr, "Unable to open file [%s] for writing: %s\n", 
                   args->filename,
                   strerror(errno));
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdout;
      
#if 0
  /* XXX come back to this later when the sectional stuff works */
  /* By default, output everything if nothing was requested */
  if (!args->community_string_wanted
      && !args->lan_alert_destinations_wanted
      && !args->alert_policy_table_wanted
      && !args->event_filter_table_wanted)
    {
      args->community_string_wanted = 1;
      args->lan_alert_destinations_wanted = 1;
      args->alert_policy_table_wanted = 1;
      args->event_filter_table_wanted = 1;
    }
#endif
      
  if (args->community_string_wanted)
    {
      if (checkout_pef_community_string (state_data, fp) < 0)
        goto cleanup;
    }
  if (args->lan_alert_destinations_wanted)
    {
      if (checkout_pef_lan_alert_destination (state_data, fp) < 0)
        goto cleanup;
    }
  if (args->alert_policy_table_wanted)
    {
      if (checkout_pef_alert_policy_table (state_data, fp) < 0)
        goto cleanup;
    }
  if (args->event_filter_table_wanted)
    {
      if (checkout_pef_event_filter_table (state_data, fp) < 0)
        goto cleanup;
    }
      
  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (file_opened)
    fclose (fp);
  return rv;
}
