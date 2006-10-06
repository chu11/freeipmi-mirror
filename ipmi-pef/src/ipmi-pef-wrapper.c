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
#include "freeipmi/udm/ipmi-pef-and-alerting-cmds-udm.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"

#include "ipmi-pef-wrapper.h"

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
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "number_of_eft_entries", &val);
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
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

