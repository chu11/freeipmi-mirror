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
#include "ipmi-pef-argp.h"
#include "ipmi-pef-wrapper.h"
#include "ipmi-pef.h"

#include "freeipmi-portability.h"

int 
display_pef_info (ipmi_pef_state_data_t *state_data)
{
  pef_info_t pef_info;
  
  memset (&pef_info, 0, sizeof (pef_info_t));
  
  if (get_pef_info (state_data->dev, &pef_info) != 0)
    {
      fprintf (stderr, "%s: unable to get PEF information\n", 
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
	  pef_info.eft_entries_count);
  if (pef_info.alert_action_support)
    {
      printf ("Number of Event Filters:                %d\n", 
	      pef_info.num_event_filters);
      printf ("Number of Alert Policy entries:         %d\n", 
	      pef_info.num_alert_policies);
      printf ("Number of Alert Strings:                %d\n", 
	      pef_info.num_alert_strings);
    }
  
  return (0);
}

int 
checkout_pef_evt (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  int rv = 0;
  int num_event_filters;
  int filter;
  
  if (get_number_of_event_filters (state_data->dev, &num_event_filters) != 0)
    return (-1);
  
  for (filter = 1; filter < num_event_filters; filter++)
    {
      pef_event_filter_table_t evt;
      
      memset (&evt, 0, sizeof (pef_event_filter_table_t));
      
      if (get_event_filter_table (state_data->dev, filter, &evt) != 0)
	{
	  fprintf (stderr, "unable to get event filter table #%d\n", filter);
	  rv = -1;
	  continue;
	}
      fprintf (fp, "Filter_Number = %d\n", evt.filter_number);
      /* fprintf (fp, "# manufacturer pre-configured filter = 2, software configurable filter = 0, reserved = 1 or 3.\n"); */
      fprintf (fp, "Filter_Type = %d\n", evt.filter_type);
      fprintf (fp, "Enable_Filter = %s\n", 
	       (evt.enable_filter ? "Yes" : "No"));
      fprintf (fp, "Event_filter_Action_Alert = %s\n", 
	       (evt.event_filter_action_alert ? "Yes" : "No"));
      fprintf (fp, "Event_Filter_Action_Power_Off = %s\n", 
	       (evt.event_filter_action_power_off ? "Yes" : "No"));
      fprintf (fp, "Event_Filter_Action_Reset = %s\n", 
	       (evt.event_filter_action_reset ? "Yes" : "No"));
      fprintf (fp, "Event_Filter_Action_Power_Cycle = %s\n", 
	       (evt.event_filter_action_power_cycle ? "Yes" : "No"));
      fprintf (fp, "Event_Filter_Action_OEM = %s\n", 
	       (evt.event_filter_action_oem ? "Yes" : "No"));
      fprintf (fp, "Event_Filter_Action_Diagnostic_Interrupt = %s\n", 
	       (evt.event_filter_action_diagnostic_interrupt ? "Yes" : "No"));
      fprintf (fp, "Event_Filter_Action_Group_Control_Operation = %s\n", 
	       (evt.event_filter_action_group_control_operation ? "Yes" : "No"));
      fprintf (fp, "Alert_Policy_Number = %d\n", evt.alert_policy_number);
      fprintf (fp, "Group_Control_Selector = %d\n", evt.group_control_selector);
      /* fprintf (fp, "# 0x0 = unspecified, 0x1 = Monitor, 0x2 = Information, 0x4 = OK (return to OK condition), 0x8 = Non-critical condition, 0x10 = Critical condition, 0x20 = Non-recoverable condition\n"); */
      fprintf (fp, "Event_Severity = 0x%X\n", evt.event_severity);
      /* fprintf (fp, "# 0xFF = Match any\n"); */
      fprintf (fp, "Generator_ID_Byte1 = 0x%X\n", evt.generator_id_byte1);
      /* fprintf (fp, "# 0xFF = Match any\n"); */
      fprintf (fp, "Generator_ID_Byte2 = 0x%X\n", evt.generator_id_byte2);
      /* fprintf (fp, "# 0xFF = Match any\n"); */
      fprintf (fp, "Sensor_Type = 0x%X\n", evt.sensor_type);
      /* fprintf (fp, "# 0xFF = Match any\n"); */
      fprintf (fp, "Sensor_Number = 0x%X\n", evt.sensor_number);
      /* fprintf (fp, "# 0xFF = Match any\n"); */
      fprintf (fp, "Event_Trigger = 0x%X\n", evt.event_trigger);
      fprintf (fp, "Event_Data1_Offset_Mask = 0x%X\n", evt.event_data1_offset_mask);
      fprintf (fp, "Event_Data1_AND_Mask = 0x%X\n", evt.event_data1_AND_mask);
      fprintf (fp, "Event_Data1_Compare1 = 0x%X\n", evt.event_data1_compare1);
      fprintf (fp, "Event_Data1_Compare2 = 0x%X\n", evt.event_data1_compare2);
      fprintf (fp, "Event_Data2_AND_Mask = 0x%X\n", evt.event_data2_AND_mask);
      fprintf (fp, "Event_Data2_Compare1 = 0x%X\n", evt.event_data2_compare1);
      fprintf (fp, "Event_Data2_Compare2 = 0x%X\n", evt.event_data2_compare2);
      fprintf (fp, "Event_Data3_AND_Mask = 0x%X\n", evt.event_data3_AND_mask);
      fprintf (fp, "Event_Data3_Compare1 = 0x%X\n", evt.event_data3_compare1);
      fprintf (fp, "Event_Data3_Compare2 = 0x%X\n", evt.event_data3_compare2);
      fprintf (fp, "\n");
    }
  
  return rv;
}

int 
commit_pef_evt (ipmi_pef_state_data_t *state_data, FILE *fp)
{
  pef_event_filter_table_t *evt_list = NULL;
  int count = 0;
  int i = 0;
  int rv = 0;
  
  get_evt_list (fp, &evt_list, &count);
  
  for (i = 0; i < count; i++)
    {
      if (set_event_filter_table (state_data->dev, &evt_list[i]) != 0)
	{
	  fprintf (stderr, "unable to set event filter table #%d\n", 
		   evt_list[i].filter_number);
	  rv = -1;
	  continue;
	}
    }
  
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
      FILE *fp = stdout;
      
      if (args->checkout_filename)
	{
	  fp = fopen (args->checkout_filename, "w");
	  if (fp == NULL)
	    {
	      fprintf (stderr, "unable to open file [%s] for writing.  using stdout\n", 
		       args->checkout_filename);
	      fp = stdout;
	    }
	}
      
      rv = checkout_pef_evt (state_data, fp);
      
      if (fp != stdout)
	fclose (fp);
      
      return rv;
    }
  
  if (args->commit_wanted)
    {
      FILE *fp = NULL;
      
      fp = fopen (args->commit_filename, "r");
      if (fp)
	{
	  rv = commit_pef_evt (state_data, fp);
	  fclose (fp);
	}
      else 
	{
	  fprintf (stderr, "unable to open file [%s] for reading.  aborting...\n", 
		   args->commit_filename);
	  rv = -1;
	}
      
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
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */
  
  ipmi_disable_coredump();

  prog_data.progname = argv[0];
  ipmi_pef_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
  
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

