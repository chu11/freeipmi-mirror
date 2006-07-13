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
#ifndef __FreeBSD__
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
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>

#include "bmc-types.h"
#include "bmc-config.h"
#include "bmc-sections.h"

#include "ipmi-common.h"

extern ipmi_device_t *global_dev;

static int
ipmi_core_init (struct arguments *args)
{
  struct sockaddr_in host;
  struct hostent *hostinfo;

  if (args->common.host != NULL) {
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (args->common.host);
      if (hostinfo == NULL) {
          perror ("gethostbyname()");
          exit (EXIT_FAILURE);
      }
      host.sin_addr = *(struct in_addr *) hostinfo->h_addr;

      memset (&args->dev, 0, sizeof (ipmi_device_t));
      if (ipmi_open_outofband (&args->dev,
                               IPMI_DEVICE_LAN,
                               IPMI_MODE_DEFAULT,
                               args->common.packet_retry_timeout,
                               args->common.packet_retry_max,
                               (struct sockaddr *) &host,
                               sizeof (struct sockaddr),
                               args->common.authentication_type,
                               args->common.username,
                               args->common.password,
                               args->common.privilege_level) != 0) {
          perror ("ipmi_open_outofband()");
          exit (EXIT_FAILURE);
        }
  } else {
    memset (&args->dev, 0, sizeof (ipmi_device_t));
    if (args->common.driver_type == IPMI_DEVICE_UNKNOWN) {
      if (ipmi_open_inband (&args->dev,
			    args->common.disable_auto_probe,
			    IPMI_DEVICE_KCS,
			    args->common.driver_address,
			    0,
			    args->common.driver_device,
			    IPMI_MODE_DEFAULT) != 0) {
	if (ipmi_open_inband (&args->dev,
			      args->common.disable_auto_probe,
			      IPMI_DEVICE_SSIF,
			      args->common.driver_address,
			      0,
			      args->common.driver_device,
			      IPMI_MODE_DEFAULT) != 0) {
	  perror ("ipmi_open_inband()");
	  return (-1);
	}
      }
    } else {
      if (ipmi_open_inband (&args->dev,
			    args->common.disable_auto_probe,
			    args->common.driver_type,
			    args->common.driver_address,
			    0,
			    args->common.driver_device,
			    IPMI_MODE_DEFAULT) != 0) {
	perror ("ipmi_open_inband()");
	return (-1);
      }
    }
  }

  global_dev = &args->dev;
  return 0;
}

int
main (int argc, char *argv[])
{
  struct arguments arguments;
  struct section *sections;
  struct rlimit resource_limit;
  int ret = 0;

  /* generate core dump on seg-fault */
  if (ipmi_is_root ()) {
      resource_limit.rlim_cur =
        resource_limit.rlim_max = RLIM_INFINITY;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
  }

  /* Default values. */
  memset (&arguments, 0, sizeof (arguments));
  arguments.common.disable_auto_probe = 0;
  arguments.common.driver_type = IPMI_DEVICE_UNKNOWN;
  arguments.common.driver_address = 0;
  arguments.common.driver_device = NULL;
  arguments.common.packet_retry_timeout = 1000;
  arguments.common.packet_retry_max = 10;
  arguments.common.host = NULL;
  arguments.common.username = NULL;
  arguments.common.password = NULL;
  arguments.common.authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
  arguments.common.privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;

  if (bmc_argp (argc, argv,  &arguments) != 0)
    return (1);

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */

  ipmi_core_init (&arguments);

  /* this should be after ipmi_core_init since
     user section refers to ipmi calls to get
     number of user profiles in this ipmi instance
  */
  sections = bmc_sections_init (&arguments);

  switch (arguments.action) {
  case BMC_ACTION_CHECKOUT:
    ret = bmc_checkout (&arguments, sections);
    break;
  case BMC_ACTION_COMMIT:
    ret = bmc_commit (&arguments, sections);
    break;
  case BMC_ACTION_DIFF:
    ret = bmc_diff (&arguments, sections);
    break;
  }
  return (ret);
}
