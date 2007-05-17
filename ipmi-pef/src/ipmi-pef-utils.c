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

#include "ipmi-pef.h"
#include "ipmi-pef-common.h"
#include "ipmi-pef-utils.h"

int
get_lan_channel_number (struct ipmi_pef_state_data *state_data, int8_t *channel_number)
{
  int num;

  assert(state_data);
  assert(channel_number);

  if ((num = ipmi_get_channel_number (state_data->dev,
                                      IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    return -1;
  
  *channel_number = num;
  return 0;
}

int 
get_number_of_lan_destinations (struct ipmi_pef_state_data *state_data, int8_t *number_of_lan_destinations)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  uint64_t val;
  int8_t channel_number;
  
  assert(state_data);
  assert(number_of_lan_destinations);
  
  if (get_lan_channel_number (state_data, &channel_number) < 0)
    goto cleanup; 
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_lan_configuration_parameters_number_of_destinations_rs);
  
  if (ipmi_cmd_get_lan_configuration_parameters_number_of_destinations (state_data->dev, 
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
get_number_of_alert_policy_entries (struct ipmi_pef_state_data *state_data, int8_t *num_alert_policy_entries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(state_data);
  assert(num_alert_policy_entries);

  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs);
  if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (state_data->dev, 
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
get_number_of_event_filters (struct ipmi_pef_state_data *state_data, int8_t *num_event_filters)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(state_data);
  assert(num_event_filters);

  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs);
  if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (state_data->dev, 
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


