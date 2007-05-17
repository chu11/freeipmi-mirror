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
#include "ipmi-pef-info.h"
#include "ipmi-pef-common.h"

pef_err_t
pef_info (ipmi_pef_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  uint64_t val, val1, val2;
  int alert_action_support = 0;

  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_pef_capabilities_rs);
  
  if (ipmi_cmd_get_pef_capabilities (state_data->dev, obj_cmd_rs) < 0)
    {
      fprintf (stderr, "Failure Retrieving PEF info\n");
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "pef_version_major", &val1);
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "pef_version_minor", &val2);
  printf ("PEF version:                            %d.%d\n", 
	  (int)val1, 
	  (int)val2);
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.alert", &val);
  printf ("Alert action support:                   %s\n", 
	  (val ? "Yes" : "No"));
  alert_action_support = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.power_down", &val);
  printf ("Power down action support:              %s\n", 
	  (val ? "Yes" : "No"));
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.reset", &val);
  printf ("Power reset action support:             %s\n", 
	  (val? "Yes" : "No"));
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.power_cycle", &val);
  printf ("Power cycle action support:             %s\n", 
	  (val ? "Yes" : "No"));
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.oem_action", &val);
  printf ("OEM action support:                     %s\n", 
	  (val ? "Yes" : "No"));
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "action_support.diagnostic_interrupt", &val);
  printf ("Diagnostic interrupt action support:    %s\n", 
	  (val ? "Yes" : "No"));
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "oem_event_record_filtering_supported", &val);
  printf ("OEM event record filtering support:     %s\n", 
	  (val ? "Yes" : "No"));
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_event_filter_table_entries", &val);
  printf ("Number of Event Filter Table entries:   %d\n", 
	  (int)val);

  if (alert_action_support)
    {
      FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);

      FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs,
                               tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs);

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (state_data->dev,
                                                                             IPMI_GET_PEF_PARAMETER,
                                                                             SET_SELECTOR,
                                                                             BLOCK_SELECTOR,
                                                                             obj_cmd_rs) < 0)
        {
          fprintf (stderr, "Failure Retrieving PEF info\n");
          rv = PEF_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_event_filters", &val);

      printf ("Number of Event Filters:                %d\n", 
	      (int)val);

      FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);

      FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs,
                               tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs);

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (state_data->dev,
                                                                                    IPMI_GET_PEF_PARAMETER,
                                                                                    SET_SELECTOR,
                                                                                    BLOCK_SELECTOR,
                                                                                    obj_cmd_rs) < 0)
        {
          fprintf (stderr, "Failure Retrieving PEF info\n");
          rv = PEF_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_alert_policy_entries", &val);

      printf ("Number of Alert Policy entries:         %d\n", 
	      (int)val);
      
      FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);

      FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs,
                               tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs);

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings (state_data->dev,
                                                                             IPMI_GET_PEF_PARAMETER,
                                                                             SET_SELECTOR,
                                                                             BLOCK_SELECTOR,
                                                                             obj_cmd_rs) < 0)
        {
          fprintf (stderr, "Failure Retrieving PEF info\n");
          rv = PEF_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_alert_strings", &val);
      
      printf ("Number of Alert Strings:                %d\n", 
	      (int)val);
    }
  
  rv = PEF_ERR_SUCCESS;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}
