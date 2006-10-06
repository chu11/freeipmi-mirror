/*
ipmi-pef.c: Platform Event Filtering utility.
Copyright (C) 2005 FreeIPMI Core Team

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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
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

#include "argp-common.h"
#include "ipmi-common.h"
#include "ipmi-pef-argp.h"
#include "ipmi-pef-wrapper.h"

#include "freeipmi-portability.h"

int exit_status = 0;

int 
display_pef_info (ipmi_device_t dev)
{
  pef_info_t pef_info;
  
  memset (&pef_info, 0, sizeof (pef_info_t));
  
  if (get_pef_info (dev, &pef_info) != 0)
    {
      fprintf (stderr, "%s: unable to get PEF information\n", 
	       program_invocation_short_name);
      exit_status = -1;
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
run_cmd_args (ipmi_device_t dev, struct arguments *args)
{
  int retval = 0;
  
  if (args == NULL)
    return (-1);
  
  if (args->info_wanted)
    {
      retval = display_pef_info (dev);
      return retval;
    }
  
  return retval;
}

void
_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

int 
main (int argc, char **argv)
{
  struct arguments *args = NULL;
  ipmi_device_t dev = NULL;
  int retval = 0;
  uint32_t flags;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */
  
  _disable_coredump();

  ipmi_pef_argp_parse (argc, argv);
  args = ipmi_pef_get_arguments ();

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
  
#ifndef NDEBUG
  if (args->common.debug)
    flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */

  if (args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
				       args->common.host,
                                       args->common.username,
                                       args->common.password,
                                       args->common.authentication_type, 
                                       args->common.privilege_level,
                                       args->common.session_timeout, 
                                       args->common.retry_timeout, 
				       flags)))
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", argv[0]);
          exit(EXIT_FAILURE);
        }

      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI, 
					args->common.disable_auto_probe, 
                                        args->common.driver_address, 
                                        args->common.register_spacing,
                                        args->common.driver_device, 
                                        flags)))
	    {
	      if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS,
					    args->common.disable_auto_probe,
					    args->common.driver_address,
					    args->common.register_spacing,
					    args->common.driver_device,
					    flags)))
		{
		  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
						args->common.disable_auto_probe,
						args->common.driver_address,
						args->common.register_spacing,
						args->common.driver_device,
						flags)))
		    {
		      perror ("ipmi_open_inband()");
		      return (-1);
		    }
		}
	    }
	}
      else
	{
	  if (!(dev = ipmi_open_inband (args->common.driver_type,
					args->common.disable_auto_probe,
					args->common.driver_address,
                                        args->common.register_spacing,
                                        args->common.driver_device,
                                        flags)))
	    {
	      perror ("ipmi_open_inband()");
	      return (-1);
	    }
	}
    }
  
  retval = run_cmd_args (dev, args);
  
  ipmi_close_device (dev);
  
  return (retval);
}
