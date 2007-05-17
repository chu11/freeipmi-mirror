/*
ipmi-pef.c: Platform Event Filtering utility.
Copyright (C) 2005-2007 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <argp.h>
#include <assert.h>

#include "argp-common.h"
#include "ipmi-common.h"
#include "ipmi-pef.h"
#include "ipmi-pef-utils.h"
#include "ipmi-pef-argp.h"
#include "ipmi-pef-keys.h"
#include "ipmi-pef-map.h"
#include "ipmi-pef-wrapper.h"

#include "freeipmi-portability.h"

void
_ipmi_pef_state_data_init(ipmi_pef_state_data_t *state_data)
{
  assert (state_data);

  memset(state_data, '\0', sizeof(ipmi_pef_state_data_t));
  state_data->prog_data = NULL;
  state_data->dev = NULL;

  state_data->lan_channel_number_initialized = 0;
  state_data->number_of_lan_destinations_initialized = 0;
  state_data->number_of_alert_policy_entries_initialized = 0;
  state_data->number_of_event_filters_initialized = 0;
}

int 
display_pef_info (ipmi_pef_state_data_t *state_data)
{
  pef_info_t pef_info;
  
  memset (&pef_info, 0, sizeof (pef_info_t));
  
  if (get_pef_info (state_data, &pef_info) != 0)
    {
      fprintf (stderr, "%s: Unable to get PEF information\n", 
	       program_invocation_short_name);
      return (-1);
    }

  printf ("PEF version:                            %d.%d\n", 
	  pef_info.pef_version_major, 
	  pef_info.pef_version_minor);
  printf ("Alert action support:                   %s\n", 
	  (pef_info.alert_action_support ? "Yes" : "No"));
  printf ("Power down action support:              %s\n", 
	  (pef_info.power_down_action_support ? "Yes" : "No"));
  printf ("Power reset action support:             %s\n", 
	  (pef_info.reset_action_support ? "Yes" : "No"));
  printf ("Power cycle action support:             %s\n", 
	  (pef_info.power_cycle_action_support ? "Yes" : "No"));
  printf ("OEM action support:                     %s\n", 
	  (pef_info.oem_action_support ? "Yes" : "No"));
  printf ("Diagnostic interrupt action support:    %s\n", 
	  (pef_info.diagnostic_interrupt_action_support ? "Yes" : "No"));
  printf ("OEM event record filtering support:     %s\n", 
	  (pef_info.oem_event_record_filtering_support ? "Yes" : "No"));
  printf ("Number of Event Filter Table entries:   %d\n", 
	  pef_info.number_of_event_filter_table_entries);
  if (pef_info.alert_action_support)
    {
      printf ("Number of Event Filters:                %d\n", 
	      pef_info.number_of_event_filters);
      printf ("Number of Alert Policy entries:         %d\n", 
	      pef_info.number_of_alert_policies);
      printf ("Number of Alert Strings:                %d\n", 
	      pef_info.number_of_alert_strings);
    }
  
  return (0);
}

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
commit_pef_community_string (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  int rv = -1;

  if (get_community_string (state_data,
			    fp,
			    community_string,
			    IPMI_MAX_COMMUNITY_STRING_LENGTH+1) < 0)
    {
      if (state_data->prog_data->args->verbose_wanted)
	fprintf (fp, "## FATAL: Unable to set community string\n");
      goto cleanup;
    }
  
  if (set_bmc_community_string (state_data, 
				community_string) < 0)
    {
      if (state_data->prog_data->args->verbose_wanted)
	fprintf (fp, "## FATAL: Unable to set community string\n");
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return rv;
}

int 
checkout_pef_lan_alert_destination (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  int rv = 0;
  int8_t num_of_lan_destinations;
  int d;

  if (get_number_of_lan_destinations (state_data, &num_of_lan_destinations) != 0)
    return (-1);

  for (d = 1; d <= num_of_lan_destinations; d++)
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
commit_pef_lan_alert_destination (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  lan_alert_destination_t *lad_list = NULL;
  int count = 0;
  int i = 0;
  int rv = 0;
  
  get_lan_alert_destination_list (fp, &lad_list, &count);
  
  for (i = 0; i < count; i++)
    {
      if (set_lan_alert_destination (state_data, &lad_list[i]) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to set LAN Alert Destination #%d\n", 
                     lad_list[i].destination_selector);
	  rv = -1;
	  continue;
	}
    }
  
  free(lad_list);
  return rv;
}

int 
checkout_pef_alert_policy_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  int rv = 0;
  int8_t number_of_alert_policy_entries;
  int entry;
  
  if (get_number_of_alert_policy_entries (state_data, 
					  &number_of_alert_policy_entries) != 0)
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
      
      if (!(str = alert_policy_number_to_string (apt.alert_policy_number)))
        return -1;

      fprintf (fp, 
               "%-30s %s\n", 
               APT_ALERT_POLICY_NUMBER_KEY_STRING, 
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
commit_pef_alert_policy_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  pef_alert_policy_table_t *apt_list = NULL;
  int count = 0;
  int i = 0;
  int rv = 0;
  
  get_alert_policy_table_list (fp, &apt_list, &count);
  
  for (i = 0; i < count; i++)
    {
      if (set_alert_policy_table (state_data, &apt_list[i]) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to set alert policy table #%d\n", 
                     apt_list[i].alert_policy_number);
	  rv = -1;
	  continue;
	}
    }
  
  free(apt_list);
  return rv;
}

int 
checkout_pef_event_filter_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  int rv = 0;
  int8_t num_event_filters;
  int filter;
  
  if (get_number_of_event_filters (state_data, &num_event_filters) != 0)
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

int 
commit_pef_event_filter_table (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  pef_event_filter_table_t *eft_list = NULL;
  int count = 0;
  int i = 0;
  int rv = 0;
  
  get_event_filter_table_list (fp, &eft_list, &count);
  
  for (i = 0; i < count; i++)
    {
      if (set_event_filter_table (state_data, &eft_list[i]) != 0)
	{
          if (state_data->prog_data->args->verbose_wanted)
            fprintf (fp, "## FATAL: Unable to set event filter table #%d\n", 
                     eft_list[i].filter_number);
	  rv = -1;
	  continue;
	}
    }
  
  free(eft_list);
  return rv;
}

int 
run_cmd_args (ipmi_pef_state_data_t *state_data)
{
  struct ipmi_pef_arguments *args;
  int rv = -1;
  
  assert (state_data);

  args = state_data->prog_data->args;

  if (args->info_wanted)
    {
      rv = display_pef_info (state_data);
      return rv;
    }
  
  if (args->checkout_wanted)
    {
      FILE *fp = NULL;
      int file_opened = 0;

      if (args->filename && strcmp (args->filename, "-"))
	{
	  fp = fopen (args->filename, "w");
	  if (fp == NULL)
	    {
              fprintf (stderr, "Unable to open file [%s] for writing: %s\n", 
                       args->filename,
                       strerror(errno));
              rv = -1;
              return rv;
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
          if ((rv = checkout_pef_community_string (state_data, fp)) < 0)
            goto checkout_cleanup;
        }
      if (args->lan_alert_destinations_wanted)
        {
          if ((rv = checkout_pef_lan_alert_destination (state_data, fp)) < 0)
            goto checkout_cleanup;
        }
      if (args->alert_policy_table_wanted)
        {
          if ((rv = checkout_pef_alert_policy_table (state_data, fp)) < 0)
            goto checkout_cleanup;
        }
      if (args->event_filter_table_wanted)
        {
          if ((rv = checkout_pef_event_filter_table (state_data, fp)) < 0)
            goto checkout_cleanup;
        }
      
    checkout_cleanup:
      if (file_opened)
	fclose (fp);
      return rv;
    }
  
  if (args->commit_wanted)
    {
      FILE *fp = NULL;
      int file_opened = 0;

      if (args->filename && strcmp (args->filename, "-"))
        {
          if ((fp = fopen (args->filename, "r")) == NULL)
            {
              fprintf (stderr, "Unable to open file [%s] for reading: %s\n", 
                       args->filename,
                       strerror(errno));
              rv = -1;
              return rv;
            }
          file_opened++;
        }
      else
        fp = stdin;

      /* XXX come back to this later when the sectional stuff works */
      
      if (args->community_string_wanted)
        {
          if ((rv = commit_pef_community_string (state_data, fp)) < 0)
            goto commit_cleanup;
        }
      else if (args->lan_alert_destinations_wanted)
        {
          if ((rv = commit_pef_lan_alert_destination (state_data, fp)) < 0)
            goto commit_cleanup;
        }
      else if (args->alert_policy_table_wanted)
	{
	  if ((rv = commit_pef_alert_policy_table (state_data, fp)) < 0)
            goto commit_cleanup;
	}
      else if (args->event_filter_table_wanted)
	{
          if ((rv = commit_pef_event_filter_table (state_data, fp)) < 0)
            goto commit_cleanup;
        }
      
    commit_cleanup:
      if (file_opened)
        fclose (fp);
      return rv;
    }
  
  return rv;
}

static int 
_ipmi_pef (void *arg)
{
  ipmi_pef_state_data_t state_data;
  ipmi_pef_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  int exit_code = -1;
  
  prog_data = (ipmi_pef_prog_data_t *) arg;
  
  if (prog_data->args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
				       prog_data->args->common.host,
                                       prog_data->args->common.username,
                                       prog_data->args->common.password,
                                       prog_data->args->common.authentication_type, 
                                       prog_data->args->common.privilege_level,
                                       prog_data->args->common.session_timeout, 
                                       prog_data->args->common.retry_timeout, 
				       prog_data->debug_flags)))
	{
	  perror ("ipmi_open_outofband()");
	  exit_code = EXIT_FAILURE;
	  goto cleanup;
	}
    }
  else
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", prog_data->progname);
	  exit_code = EXIT_FAILURE;
	  goto cleanup;
        }

      if (prog_data->args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI, 
					prog_data->args->common.disable_auto_probe, 
                                        prog_data->args->common.driver_address, 
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device, 
                                        prog_data->debug_flags)))
	    {
	      if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS,
					    prog_data->args->common.disable_auto_probe,
					    prog_data->args->common.driver_address,
					    prog_data->args->common.register_spacing,
					    prog_data->args->common.driver_device,
					    prog_data->debug_flags)))
		{
		  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
						prog_data->args->common.disable_auto_probe,
						prog_data->args->common.driver_address,
						prog_data->args->common.register_spacing,
						prog_data->args->common.driver_device,
						prog_data->debug_flags)))
		    {
		      perror ("ipmi_open_inband()");
		      exit_code = EXIT_FAILURE;
		      goto cleanup;
		    }
		}
	    }
	}
      else
	{
	  if (!(dev = ipmi_open_inband (prog_data->args->common.driver_type,
					prog_data->args->common.disable_auto_probe,
					prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
	    {
	      perror ("ipmi_open_inband()");
	      exit_code = EXIT_FAILURE;
	      goto cleanup;
	    }
	}
    }
  
  memset (&state_data, '\0', sizeof (ipmi_pef_state_data_t));
  state_data.dev = dev;
  state_data.prog_data = prog_data;
  
  if (run_cmd_args (&state_data) < 0)
    { 
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = 0;
 cleanup:
  if (dev)
    ipmi_close_device (dev);
  return exit_code;
}

int 
main (int argc, char **argv)
{
  ipmi_pef_prog_data_t prog_data;
  struct ipmi_pef_arguments cmd_args;
  int exit_code;
  
  ipmi_disable_coredump();

  prog_data.progname = argv[0];
  ipmi_pef_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

#ifndef NDEBUG
  if (prog_data.args->common.debug)
    prog_data.debug_flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */
  
  exit_code = _ipmi_pef (&prog_data);
  
  return exit_code;
}

