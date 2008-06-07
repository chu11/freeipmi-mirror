/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

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
#include <errno.h>
#include <assert.h>

#include "freeipmi-portability.h"

#include "pef-config.h"
#include "pef-config-info.h"

config_err_t
pef_info (pef_config_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  uint64_t val, val1, val2;
  int alert_action_support = 0;

  if (!(obj_cmd_rs = Fiid_obj_create (tmpl_cmd_get_pef_capabilities_rs)))
    goto cleanup;
  
  if (ipmi_cmd_get_pef_capabilities (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        fprintf(stderr,
                "ipmi_cmd_get_pef_capabilities: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      fprintf (stderr, "Failure Retrieving PEF info\n");
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "pef_version_major", &val1) < 0)
    goto cleanup;
  if (Fiid_obj_get (obj_cmd_rs, "pef_version_minor", &val2) < 0)
    goto cleanup;
  /* achu: ipmi version is BCD encoded, but major/minor are only 4 bits */
  printf ("PEF version:                            %d.%d\n", 
	  (int)val1, 
	  (int)val2);
  if (Fiid_obj_get (obj_cmd_rs, "action_support.alert", &val) < 0)
    goto cleanup;
  printf ("Alert action support:                   %s\n", 
	  (val ? "Yes" : "No"));
  alert_action_support = val;
  if (Fiid_obj_get (obj_cmd_rs, "action_support.power_down", &val) < 0)
    goto cleanup;
  printf ("Power down action support:              %s\n", 
	  (val ? "Yes" : "No"));
  if (Fiid_obj_get (obj_cmd_rs, "action_support.reset", &val) < 0)
    goto cleanup;
  printf ("Power reset action support:             %s\n", 
	  (val? "Yes" : "No"));
  if (Fiid_obj_get (obj_cmd_rs, "action_support.power_cycle", &val) < 0)
    goto cleanup;
  printf ("Power cycle action support:             %s\n", 
	  (val ? "Yes" : "No"));
  if (Fiid_obj_get (obj_cmd_rs, "action_support.oem_action", &val) < 0)
    goto cleanup;
  printf ("OEM action support:                     %s\n", 
	  (val ? "Yes" : "No"));
  if (Fiid_obj_get (obj_cmd_rs, "action_support.diagnostic_interrupt", &val) < 0)
    goto cleanup;
  printf ("Diagnostic interrupt action support:    %s\n", 
	  (val ? "Yes" : "No"));
  if (Fiid_obj_get (obj_cmd_rs, "oem_event_record_filtering_supported", &val) < 0)
    goto cleanup;
  printf ("OEM event record filtering support:     %s\n", 
	  (val ? "Yes" : "No"));
  if (Fiid_obj_get (obj_cmd_rs, "number_of_event_filter_table_entries", &val) < 0)
    goto cleanup;
  printf ("Number of Event Filter Table entries:   %d\n", 
	  (int)val);

  if (alert_action_support)
    {
      Fiid_obj_destroy (obj_cmd_rs);

      if (!(obj_cmd_rs = Fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_number_of_event_filters_rs)))
        goto cleanup;

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters (state_data->ipmi_ctx,
                                                                             IPMI_GET_PEF_PARAMETER,
                                                                             SET_SELECTOR,
                                                                             BLOCK_SELECTOR,
                                                                             obj_cmd_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            fprintf(stderr,
                    "ipmi_cmd_get_pef_configuration_parameters_number_of_event_filters: %s\n",
                    ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          fprintf (stderr, "Failure Retrieving PEF info\n");
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      if (Fiid_obj_get (obj_cmd_rs, "number_of_event_filters", &val) < 0)
        goto cleanup;

      printf ("Number of Event Filters:                %d\n", 
	      (int)val);

      Fiid_obj_destroy (obj_cmd_rs);

      if (!(obj_cmd_rs = Fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries_rs)))
        goto cleanup;

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries (state_data->ipmi_ctx,
                                                                                    IPMI_GET_PEF_PARAMETER,
                                                                                    SET_SELECTOR,
                                                                                    BLOCK_SELECTOR,
                                                                                    obj_cmd_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            fprintf(stderr,
                    "ipmi_cmd_get_pef_configuration_parameters_number_of_alert_policy_entries: %s\n",
                    ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          fprintf (stderr, "Failure Retrieving PEF info\n");
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      if (Fiid_obj_get (obj_cmd_rs, "number_of_alert_policy_entries", &val) < 0)
        goto cleanup;

      printf ("Number of Alert Policy entries:         %d\n", 
	      (int)val);
      
      Fiid_obj_destroy (obj_cmd_rs);

      if (!(obj_cmd_rs = Fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_number_of_alert_strings_rs)))
        goto cleanup;

      if (ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings (state_data->ipmi_ctx,
                                                                             IPMI_GET_PEF_PARAMETER,
                                                                             SET_SELECTOR,
                                                                             BLOCK_SELECTOR,
                                                                             obj_cmd_rs) < 0)
        {
          if (state_data->prog_data->args->config_args.common.debug)
            fprintf(stderr,
                    "ipmi_cmd_get_pef_configuration_parameters_number_of_alert_strings: %s\n",
                    ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
          fprintf (stderr, "Failure Retrieving PEF info\n");
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      if (Fiid_obj_get (obj_cmd_rs, "number_of_alert_strings", &val) < 0)
        goto cleanup;
      
      printf ("Number of Alert Strings:                %d\n", 
	      (int)val);
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}
