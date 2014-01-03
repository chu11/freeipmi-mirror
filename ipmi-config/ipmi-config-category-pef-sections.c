/*
 * Copyright (C) 2007-2014 FreeIPMI Core Team
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
#include <errno.h>
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "ipmi-config-category-pef-alert-policy-table.h"
#include "ipmi-config-category-pef-alert-string.h"
#include "ipmi-config-category-pef-community-string.h"
#include "ipmi-config-category-pef-event-filter-table.h"
#include "ipmi-config-category-pef-lan-alert-destination.h"
#include "ipmi-config-category-pef-pef-conf-section.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

static ipmi_config_err_t
_get_number_of_lan_alert_destinations (struct ipmi_config_state_data *state_data, uint8_t *number_of_lan_alert_destinations)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint64_t val;
  uint8_t channel_number;

  assert (state_data);
  assert (number_of_lan_alert_destinations);

  /* for the time being, we assume equal destinations per channel, so NULL for section_name */
  if ((ret = get_lan_channel_number (state_data, NULL, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_number_of_destinations (state_data->ipmi_ctx,
                                                                        channel_number,
                                                                        IPMI_GET_LAN_PARAMETER,
                                                                        IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                        IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_lan_configuration_parameters_number_of_destinations: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "number_of_lan_destinations",
                    &val) < 0)
    {
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  (*number_of_lan_alert_destinations) = val;
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_number_of_alert_strings (struct ipmi_config_state_data *state_data, uint8_t *number_of_alert_strings)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (number_of_alert_strings);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings (state_data->ipmi_ctx,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                         IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "number_of_alert_strings",
                    &val) < 0)
    {
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  (*number_of_alert_strings) = val;
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_number_of_alert_policy_entries (struct ipmi_config_state_data *state_data, uint8_t *number_of_alert_policy_entries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (number_of_alert_policy_entries);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (state_data->ipmi_ctx,
                                                                                IPMI_GET_PEF_PARAMETER,
                                                                                IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "number_of_alert_policy_entries",
                    &val) < 0)
    {
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  (*number_of_alert_policy_entries) = val;
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_number_of_event_filters (struct ipmi_config_state_data *state_data, uint8_t *number_of_event_filters)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (number_of_event_filters);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (state_data->ipmi_ctx,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                         IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "number_of_event_filters",
                    &val) < 0)
    {
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  (*number_of_event_filters) = val;
  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

struct ipmi_config_section *
ipmi_config_pef_sections_create (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *sections = NULL;
  struct ipmi_config_section *section = NULL;
  uint8_t number_of_lan_alert_destinations = 0;
  uint8_t number_of_alert_strings = 0;
  uint8_t number_of_alert_policy_entries = 0;
  uint8_t number_of_event_filters = 0;
  unsigned int i;
  int channelindex;

  assert (state_data);

  if (load_lan_channel_numbers (state_data) == IPMI_CONFIG_ERR_FATAL_ERROR) 
    return (NULL);

  if (_get_number_of_lan_alert_destinations (state_data,
					     &number_of_lan_alert_destinations) != IPMI_CONFIG_ERR_SUCCESS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Unable to get Number of Alert Destinations\n");
      return (NULL);
    }

  if (_get_number_of_alert_policy_entries (state_data,
					   &number_of_alert_policy_entries) != IPMI_CONFIG_ERR_SUCCESS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Unable to get Number of Alert Policy Entries\n");
      return (NULL);
    }

  if (_get_number_of_alert_strings (state_data,
				    &number_of_alert_strings) != IPMI_CONFIG_ERR_SUCCESS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Unable to get Number of Alert Strings\n");
      return (NULL);
    }

  if (_get_number_of_event_filters (state_data,
				    &number_of_event_filters) != IPMI_CONFIG_ERR_SUCCESS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Unable to get Number of Event Filters\n");
      return (NULL);
    }

  if (state_data->prog_data->args->verbose_count
      && state_data->lan_channel_numbers_count > 1)
    {
      state_data->lan_base_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
      state_data->lan_channel_config_flags = 0;
    }
  else
    {
      state_data->lan_base_config_flags = 0;
      state_data->lan_channel_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
    }

  /* PEF_Conf Section */

  if (!(section = ipmi_config_pef_pef_conf_section_get (state_data)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  /* Community_String Section(s) */

  if (!(section = ipmi_config_pef_community_string_section_get (state_data,
								state_data->lan_base_config_flags,
								-1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_pef_community_string_section_get (state_data,
									state_data->lan_channel_config_flags,
									channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Lan_Alert_Destination Section(s) */


  for (i = 0; i <= number_of_lan_alert_destinations; i++)
    {
      if (!(section = ipmi_config_pef_lan_alert_destination_section_get (state_data,
									 i,
									 state_data->lan_base_config_flags,
									 -1)))
        goto cleanup;
      if (ipmi_config_section_append (&sections, section) < 0)
        goto cleanup;
    }

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
	  for (i = 0; i < number_of_lan_alert_destinations; i++)
	    {
	      if (!(section = ipmi_config_pef_lan_alert_destination_section_get (state_data,
										 i + 1,
										 state_data->lan_channel_config_flags,
										 channelindex)))
		goto cleanup;
	      if (ipmi_config_section_append (&sections, section) < 0)
		goto cleanup;
	    }
        }
    }

  /* Alert_String Section(s) */

  for (i = 0; i <= number_of_alert_strings; i++)
    {
      if (!(section = ipmi_config_pef_alert_string_section_get (state_data, i)))
        goto cleanup;
      if (ipmi_config_section_append (&sections, section) < 0)
        goto cleanup;
    }

  /* Alert_Policy Section(s) */

  for (i = 0; i < number_of_alert_policy_entries; i++)
    {
      if (!(section = ipmi_config_pef_alert_policy_table_section_get (state_data, i + 1)))
        goto cleanup;
      if (ipmi_config_section_append (&sections, section) < 0)
        goto cleanup;
    }

  /* Event_Filter Section(s) */

  for (i = 0; i < number_of_event_filters; i++)
    {
      if (!(section = ipmi_config_pef_event_filter_table_section_get (state_data, i + 1)))
        goto cleanup;
      if (ipmi_config_section_append (&sections, section) < 0)
        goto cleanup;
    }

  return (sections);

 cleanup:
  ipmi_config_sections_destroy (sections);
  return (NULL);
}
