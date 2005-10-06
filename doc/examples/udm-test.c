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

const char *argp_program_version = "udm-test 0.1.0";
const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = 
"UDM Test -- an example program about usage of libfreeipmi";

/* A description of the arguments we accept. */
static char args_doc[] = "";

/* The options we understand. */
static struct argp_option options[] = 
  {
    {"hostname",   'h', "IPMIHOST", 0, 
     "Connect to IPMIHOST"},
    {"username",   'u', "USERNAME", 0, 
     "Use USERNAME instead of NULL.  Maximum USERNAME length is 16"},
    {"password",   'p', "PASSWORD", 0, 
     "Use PASSWORD instead of NULL.  Maximum PASSWORD length is 16"},
    {"auth-type",  'a', "AUTHTYPE", 0, 
     "Use AUTHTYPE instead of NONE.  "
     "Allowed values are NONE, MD2, MD5, PLAIN and OEM"},
    {"priv-level", 'l', "PRIVILEGE-LEVEL", 0, 
     "Use this PRIVILEGE-LEVEL instead of USER.  "
     "Allowed values are CALLBACK, USER, OPERATOR, ADMIN and OEM"},
    { 0 }
  };

/* Used by `main' to communicate with `parse_opt'. */
struct arguments
{
  char *hostname;
  char *username;
  char *password;
  int auth_type;
  int priv_level;
};

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  /* Get the INPUT argument from `argp_parse', which we
     know is a pointer to our arguments structure. */
  struct arguments *arguments = state->input;
  
  switch (key)
    {
    case 'h':
      arguments->hostname = arg;
      break;
      
    case 'u': /* 16 chars max */
      if (strlen (arg) > 16)
	argp_usage (state);
      else 
	arguments->username = arg;
      
      break;
      
    case 'p': /* 16 chars max */
      if (strlen (arg) > 16)
	argp_usage (state);
      else 
	arguments->password = arg;
      
      break;
      
    case 'a': /* values 0,1,2,4,5 = none,md2,md5,straight,oem */
      if (strcasecmp (arg, "none") == 0)
	{
	  arguments->auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
	}
      else 
	if (strcasecmp (arg, "md2") == 0)
	  {
	    arguments->auth_type = IPMI_SESSION_AUTH_TYPE_MD2;
	  }
	else 
	  if (strcasecmp (arg, "md5") == 0)
	    {
	      arguments->auth_type = IPMI_SESSION_AUTH_TYPE_MD5;
	    }
	  else 
	    if (strcasecmp (arg, "plain") == 0)
	      {
		arguments->auth_type = IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY;
	      }
	    else 
	      if (strcasecmp (arg, "oem") == 0)
		{
		  arguments->auth_type = IPMI_SESSION_AUTH_TYPE_OEM_PROP;
		}
	      else 
		{
		  argp_usage (state);
		}
      
      break;
      
    case 'l': /* range 1 to 5 = callback,user,operator,admin,oem */
      if (strcasecmp (arg, "callback") == 0)
	{
	  arguments->priv_level = IPMI_PRIV_LEVEL_CALLBACK;
	}
      else 
	if (strcasecmp (arg, "user") == 0)
	  {
	    arguments->priv_level = IPMI_PRIV_LEVEL_USER;
	  }
	else 
	  if (strcasecmp (arg, "operator") == 0)
	    {
	      arguments->priv_level = IPMI_PRIV_LEVEL_OPERATOR;
	    }
	  else 
	    if (strcasecmp (arg, "admin") == 0)
	      {
		arguments->priv_level = IPMI_PRIV_LEVEL_ADMIN;
	      }
	    else 
	      if (strcasecmp (arg, "oem") == 0)
		{
		  arguments->priv_level = IPMI_PRIV_LEVEL_OEM;
		}
	      else 
		{
		  argp_usage (state);
		}
      
      break;
      
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
      
    case ARGP_KEY_END:
      break;
      
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

/* Our argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc };

int 
main (int argc, char **argv)
{
  struct arguments arguments;
  
  ipmi_device_t dev;
  
  struct hostent *hostinfo;
  struct sockaddr_in host;
  
  fiid_obj_t obj_cmd_rs;
  
  /* Default values. */
  arguments.hostname = NULL;
  arguments.username = NULL;
  arguments.password = NULL;
  arguments.auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
  arguments.priv_level = IPMI_PRIV_LEVEL_USER;
  
  /* Parse our arguments; every option seen by `parse_opt' will
     be reflected in `arguments'. */
  argp_parse (&argp, argc, argv, 0, 0, &arguments);
  
  if (arguments.hostname != NULL)
    {
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (arguments.hostname);
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
			       arguments.auth_type, 
			       arguments.username, 
			       arguments.password, 
			       arguments.priv_level) != 0)
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else 
    {
      if (ipmi_open_inband (&dev, 
			    IPMI_DEVICE_KCS, 
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
