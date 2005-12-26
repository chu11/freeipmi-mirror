/*
udm-test.c: an example program about usage of libfreeipmi.
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

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <argp.h>
#include <freeipmi/freeipmi.h>

#include "argp-common.h"

const char *argp_program_version = "udm-test 0.1.0";
const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = 
"UDM Test -- an example program about usage of libfreeipmi";

/* A description of the arguments we accept. */
static char args_doc[] = "";

/* The options we understand. */
static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS, 
    { 0 }
  };

/* Used by `main' to communicate with `parse_opt'. */
struct arguments
{
  struct common_cmd_args common;
};

/* Parse a single option. */
static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  /* Get the INPUT argument from `argp_parse', which we
     know is a pointer to our arguments structure. */
  struct arguments *cmd_args = state->input;
  
  return common_parse_opt (key, arg, state, 
			   &(cmd_args->common));
}

/* Our argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc };

int 
main (int argc, char **argv)
{
  struct arguments cmd_args;
  
  ipmi_device_t dev;
  
  struct hostent *hostinfo;
  struct sockaddr_in host;
  
  fiid_obj_t obj_cmd_rs;
  
  init_common_cmd_args (&(cmd_args.common));
  
  /* Parse our arguments; every option seen by `parse_opt' will
     be reflected in `arguments'. */
  argp_parse (&argp, argc, argv, 0, 0, &cmd_args);
  
  if (cmd_args.common.host != NULL)
    {
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (cmd_args.common.host);
      if (hostinfo == NULL)
	{
	  perror ("gethostbyname()");
	  exit (EXIT_FAILURE);
	}
      host.sin_addr = *(struct in_addr *) hostinfo->h_addr;
      
      if (ipmi_open_outofband (&dev, 
			       IPMI_DEVICE_LAN, 
			       IPMI_MODE_DEFAULT, 
			       (struct sockaddr *) &host, 
			       sizeof (struct sockaddr), 
			       cmd_args.common.auth_type, 
			       cmd_args.common.username, 
			       cmd_args.common.password, 
			       cmd_args.common.priv_level) != 0)
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else 
    {
      if (ipmi_open_inband (&dev, 
			    cmd_args.common.driver_type, 
			    IPMI_MODE_DEFAULT) != 0)
	{
	  perror ("ipmi_open_inband()");
	  exit (EXIT_FAILURE);
	}
    }
  
  fiid_obj_alloca (obj_cmd_rs, tmpl_cmd_get_dev_id_rs);
  if (ipmi_cmd_get_dev_id (&dev, obj_cmd_rs) != 0)
    {
      perror ("ipmi_cmd()");
    }
  fiid_obj_dump (fileno (stdout), obj_cmd_rs, tmpl_cmd_get_dev_id_rs);
  
  if (ipmi_close (&dev) != 0)
    {
      perror ("ipmi_close()");
      exit (EXIT_FAILURE);
    }
  
  return (0);
}
