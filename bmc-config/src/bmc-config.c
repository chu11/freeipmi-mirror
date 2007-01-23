/* 

   bmc-config - tool to configure bmc

   Copyright (C) 2006 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <argp.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#ifdef HAVE_SYS_IO_H
#include <sys/io.h>
#endif
#include <syslog.h>
#include <assert.h>
#include <stdarg.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <errno.h>
#if HAVE_GETOPT_H
#include <getopt.h>
#endif
#include <stdint.h>
#include <sys/stat.h>
#include <sys/select.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else  /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <string.h>
#include <sys/types.h>
#include <sys/resource.h>

#include "bmc-config.h"
#include "bmc-argp.h"
#include "bmc-commit.h"
#include "bmc-checkout.h"
#include "bmc-diff.h"
#include "bmc-sections.h"

#include "ipmi-common.h"

static int
ipmi_core_init (char *progname, struct bmc_config_arguments *args)
{
  uint32_t flags;

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
      if (!(args->dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
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
          fprintf(stderr, "%s: Permission Denied\n", progname);
          exit(EXIT_FAILURE);
        }
      
      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN) 
        {
	  if (!(args->dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI, 
					      args->common.disable_auto_probe, 
					      args->common.driver_address, 
					      args->common.register_spacing,
					      args->common.driver_device, 
					      flags)))
	    {
	      if (!(args->dev = ipmi_open_inband (IPMI_DEVICE_KCS,
						  args->common.disable_auto_probe,
						  args->common.driver_address,
						  args->common.register_spacing,
						  args->common.driver_device,
						  flags)))
		{
		  if (!(args->dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
						      args->common.disable_auto_probe,
						      args->common.driver_address,
						      args->common.register_spacing,
						      args->common.driver_device,
						      flags)))
		    {
		      perror ("ipmi_open_inband()");
		      exit (EXIT_FAILURE);
		    }
		}
            }
        } 
      else 
        {
          if (!(args->dev = ipmi_open_inband (args->common.driver_type,
					      args->common.disable_auto_probe,
					      args->common.driver_address,
                                              args->common.register_spacing,
                                              args->common.driver_device,
                                              flags)))
            {
              perror ("ipmi_open_inband()");
              exit (EXIT_FAILURE);
            }
        }
    }

  return 0;
}

static void
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
main (int argc, char *argv[])
{
  struct bmc_config_arguments cmd_args;
  struct section *sections;
  int ret = 0;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */

  _disable_coredump();

  if (bmc_argp (argc, argv,  &cmd_args) < 0)
    return (EXIT_FAILURE);

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */

  ipmi_core_init (argv[0], &cmd_args);

  /* this should be after ipmi_core_init since
     user section refers to ipmi calls to get
     number of user profiles in this ipmi instance
  */
  sections = bmc_sections_init (&cmd_args);

  switch (cmd_args.action) {
  case BMC_ACTION_CHECKOUT:
    ret = bmc_checkout (&cmd_args, sections);
    break;
  case BMC_ACTION_COMMIT:
    ret = bmc_commit (&cmd_args, sections);
    break;
  case BMC_ACTION_DIFF:
    ret = bmc_diff (&cmd_args, sections);
    break;
  case BMC_ACTION_LIST_SECTIONS:
    ret = bmc_sections_list (&cmd_args, sections);
    break;
  }
  
  ipmi_close_device(cmd_args.dev);
  return (ret);
}
