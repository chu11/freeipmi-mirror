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

#include "pef-config.h"
#include "pef-config-sections.h"
#include "pef-config-utils.h"

#include "pef-config-pef-conf-section.h"
#include "pef-config-community-string.h"
#include "pef-config-lan-alert-destination.h"
#include "pef-config-alert-string.h"
#include "pef-config-alert-policy-table.h"
#include "pef-config-event-filter-table.h"

struct config_section *
pef_config_sections_create (pef_config_state_data_t *state_data)
{
  struct config_section *sections = NULL;
  struct config_section *section = NULL;
  uint8_t number_of_lan_alert_destinations,
    number_of_alert_strings,
    number_of_alert_policy_entries,
    number_of_event_filters;
  int i;

  if (get_number_of_lan_alert_destinations(state_data, 
                                           &number_of_lan_alert_destinations) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.verbose)
        fprintf (stderr, "## FATAL: Unable to get Number of Alert Destinations\n");
      return NULL;
    }

  if (get_number_of_alert_policy_entries(state_data,
                                         &number_of_alert_policy_entries) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.verbose)
        fprintf (stderr, "## FATAL: Unable to get Number of Alert Policy Entries\n");
      return NULL;
    }

  if (get_number_of_alert_strings(state_data,
                                  &number_of_alert_strings) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.verbose)
        fprintf (stderr, "## FATAL: Unable to get Number of Alert Strings\n");
      return NULL;
    }
  
  if (get_number_of_event_filters(state_data, 
                                  &number_of_event_filters) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.verbose)
        fprintf (stderr, "## FATAL: Unable to get Number of Event Filters\n");
      return NULL;
    }

  if (!(section = pef_config_pef_conf_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  if (!(section = pef_config_community_string_section_get (state_data)))
    goto cleanup;
  if (config_section_append (&sections, section) < 0)
    goto cleanup;

  for (i = 0; i < number_of_lan_alert_destinations; i++)
    {
      if (!(section = pef_config_lan_alert_destination_section_get(state_data, i+1)))
	goto cleanup;
      if (config_section_append (&sections, section) < 0)
	goto cleanup;
    }

  for (i = 0; i < number_of_alert_strings; i++)
    {
      if (!(section = pef_config_alert_string_section_get(state_data, i+1)))
	goto cleanup;
      if (config_section_append (&sections, section) < 0)
	goto cleanup;
    }

  for (i = 0; i < number_of_alert_policy_entries; i++)
    {
      if (!(section = pef_config_alert_policy_table_section_get(state_data, i+1)))
	goto cleanup;
      if (config_section_append (&sections, section) < 0)
	goto cleanup;
    }

  for (i = 0; i < number_of_event_filters; i++)
    {
      if (!(section = pef_config_event_filter_table_section_get(state_data, i+1)))
	goto cleanup;
      if (config_section_append (&sections, section) < 0)
	goto cleanup;
    }
  
  return sections;

 cleanup:
  config_sections_destroy(sections);
  return NULL;
}
