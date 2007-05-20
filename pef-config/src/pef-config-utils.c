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

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"

#include "common-utils.h"

#include "pef-config.h"
#include "pef-config-common.h"
#include "pef-config-utils.h"

pef_err_t
get_lan_channel_number (struct pef_config_state_data *state_data, int8_t *channel_number)
{
  assert(state_data);
  assert(channel_number);

  if (state_data->lan_channel_number_initialized)
    {
      *channel_number = state_data->lan_channel_number;
      return 0;
    }

  if ((state_data->lan_channel_number = ipmi_get_channel_number (state_data->dev,
                                                                 IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0) 
    return PEF_ERR_NON_FATAL_ERROR;
  
  state_data->lan_channel_number_initialized = 1;
  *channel_number = state_data->lan_channel_number;
  return PEF_ERR_SUCCESS;
}

pef_err_t
get_number_of_lan_alert_destinations (struct pef_config_state_data *state_data, int8_t *number_of_lan_alert_destinations)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t rc;
  uint64_t val;
  int8_t channel_number;
  
  assert(state_data);
  assert(number_of_lan_alert_destinations);
  
  if (state_data->number_of_lan_alert_destinations_initialized)
    {
      *number_of_lan_alert_destinations = state_data->number_of_lan_alert_destinations;
      return PEF_ERR_SUCCESS;
    }

  if ((rc = get_lan_channel_number (state_data, &channel_number)) != PEF_ERR_SUCCESS)
    {
      rv = rc;
      goto cleanup; 
    }

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs)))
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_number_of_destinations (state_data->dev, 
									channel_number, 
									IPMI_GET_LAN_PARAMETER, 
									SET_SELECTOR, 
									BLOCK_SELECTOR, 
									obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get(obj_cmd_rs,
                   "number_of_lan_destinations",
                   &val) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  state_data->number_of_lan_alert_destinations_initialized = 1;
  state_data->number_of_lan_alert_destinations = val;

  *number_of_lan_alert_destinations = state_data->number_of_lan_alert_destinations;
  rv = PEF_ERR_SUCCESS;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return rv;
}

pef_err_t
get_number_of_alert_strings (struct pef_config_state_data *state_data, int8_t *number_of_alert_strings)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(number_of_alert_strings);

  if (state_data->number_of_alert_strings_initialized)
    {
      *number_of_alert_strings = state_data->number_of_alert_strings;
      return PEF_ERR_SUCCESS;
    }

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings (state_data->dev, 
										IPMI_GET_PEF_PARAMETER, 
										SET_SELECTOR, 
										BLOCK_SELECTOR, 
										obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get(obj_cmd_rs,
                   "number_of_alert_strings",
                   &val) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  state_data->number_of_alert_strings_initialized = 1;
  state_data->number_of_alert_strings = val;

  *number_of_alert_strings = state_data->number_of_alert_strings;
  rv = PEF_ERR_SUCCESS;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

pef_err_t
get_number_of_alert_policy_entries (struct pef_config_state_data *state_data, int8_t *number_of_alert_policy_entries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(number_of_alert_policy_entries);

  if (state_data->number_of_alert_policy_entries_initialized)
    {
      *number_of_alert_policy_entries = state_data->number_of_alert_policy_entries;
      return PEF_ERR_SUCCESS;
    }

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (state_data->dev, 
										IPMI_GET_PEF_PARAMETER, 
										SET_SELECTOR, 
										BLOCK_SELECTOR, 
										obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get(obj_cmd_rs,
                   "number_of_alert_policy_entries",
                   &val) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  state_data->number_of_alert_policy_entries_initialized = 1;
  state_data->number_of_alert_policy_entries = val;

  *number_of_alert_policy_entries = state_data->number_of_alert_policy_entries;
  rv = PEF_ERR_SUCCESS;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

pef_err_t
get_number_of_event_filters (struct pef_config_state_data *state_data, int8_t *number_of_event_filters)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(number_of_event_filters);

  if (state_data->number_of_event_filters_initialized)
    {
      *number_of_event_filters = state_data->number_of_event_filters;
      return PEF_ERR_SUCCESS;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (state_data->dev, 
									 IPMI_GET_PEF_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get(obj_cmd_rs,
                   "number_of_event_filters",
                   &val) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  state_data->number_of_event_filters_initialized = 1;
  state_data->number_of_event_filters = val;

  *number_of_event_filters = state_data->number_of_event_filters;
  rv = PEF_ERR_SUCCESS;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}


