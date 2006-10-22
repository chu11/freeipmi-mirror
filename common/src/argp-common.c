/*
   argp-common.c: common work for argp for all freeipmi tools.
   Copyright (C) 2005 FreeIPMI Core Team

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

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <argp.h>

#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-privilege-level-spec.h"

#include "argp-common.h"
#include "freeipmi-portability.h"

error_t 
common_parse_opt (int key, 
		  char *arg, 
		  struct argp_state *state, 
		  struct common_cmd_args *cmd_args)
{
  switch (key)
    {
    case NO_PROBING_KEY:
      cmd_args->disable_auto_probe = 1;
      break;
    case DRIVER_TYPE_KEY: /* values 1,2,3,4,5 = lan,kcs,smic,bt.ssif */
      if (strcasecmp (arg, "lan") == 0)
	{
	  cmd_args->driver_type = IPMI_DEVICE_LAN;
	}
      else 
	if (strcasecmp (arg, "kcs") == 0)
	  {
	    cmd_args->driver_type = IPMI_DEVICE_KCS;
	  }
	else 
	  if (strcasecmp (arg, "smic") == 0)
	    {
	      cmd_args->driver_type = IPMI_DEVICE_SMIC;
	    }
	  else 
	    if (strcasecmp (arg, "bt") == 0)
	      {
		cmd_args->driver_type = IPMI_DEVICE_BT;
	      }
	    else 
	      if (strcasecmp (arg, "ssif") == 0)
		{
		  cmd_args->driver_type = IPMI_DEVICE_SSIF;
		}
	      else 
		if (strcasecmp (arg, "openipmi") == 0)
		  {
		    cmd_args->driver_type = IPMI_DEVICE_OPENIPMI;
		  }
		else
		  {
		    fprintf(stderr, "invalid driver type specified\n");
		    argp_usage (state);
		  }
      break;
    case DRIVER_ADDRESS_KEY:
      {
	int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	errno = 0;
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid driver address\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid driver address\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid driver address\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->driver_address = value;
      }
      break;
    case DRIVER_DEVICE_KEY:
      if (cmd_args->driver_device != NULL)
	free (cmd_args->driver_device);
      cmd_args->driver_device = strdup (arg);
      break;
    case REGISTER_SPACING_KEY:
      {
        int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid register spacing\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid register spacing\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid register spacing\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->register_spacing = value;
      }
      break;
    case HOSTNAME_KEY:
      if (cmd_args->host != NULL)
	free (cmd_args->host);
      cmd_args->host = strdup (arg);
      break;
    case USERNAME_KEY:
      if (strlen (arg) > IPMI_MAX_USER_NAME_LENGTH)
	argp_usage (state);
      else 
	{
	  if (cmd_args->username != NULL)
	    free (cmd_args->username);
	  cmd_args->username = strdup (arg);
	}
      break;
    case PASSWORD_KEY:
      if (arg && strlen (arg) > IPMI_MAX_AUTHENTICATION_CODE_LENGTH)
	argp_usage (state);
      else 
	{
	  if (cmd_args->password != NULL)
	    free (cmd_args->password);
	  if (!arg || !strlen (arg))
	    arg = getpass ("Password: ");
	  cmd_args->password = strdup (arg);
	}
      break;
    case RETRY_TIMEOUT_KEY:
      {
	int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	errno = 0;
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid packet retry timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid packet retry timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid packet retry timeout value\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->retry_timeout = value;
      }
      break;
    case SESSION_TIMEOUT_KEY:
      {
	int value = 0;
	char *str = NULL;
	char *tail = NULL;
	int errnum = 0;
	
	str = strdupa (arg);
	errno = 0;
	value = strtol (str, &tail, 0);
	errnum = errno;
	
	if (errnum)
	  {
	    // overflow
	    fprintf (stderr, "invalid packet session timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (tail[0] != '\0')
	  {
	    // invalid integer format
	    fprintf (stderr, "invalid packet session timeout value\n");
	    argp_usage (state);
	    break;
	  }
	
	if (value < 0)
	  {
	    // negative number
	    fprintf (stderr, "invalid packet session timeout value\n");
	    argp_usage (state);
	    break;
	  }
	cmd_args->session_timeout = value;
      }
      break;
    case AUTHENTICATION_TYPE_KEY: /* values 0,1,2,4 = none,md2,md5,straight */
      if (strcasecmp (arg, "none") == 0)
	{
	  cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;
	}
      else 
	if (strcasecmp (arg, "md2") == 0)
	  {
	    cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD2;
	  }
	else 
	  if (strcasecmp (arg, "md5") == 0)
	    {
	      cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
	    }
	  else 
	    if (strcasecmp (arg, "plain") == 0)
	      {
		cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
	      }
            else 
              {
                fprintf(stderr, "invalid authentication type specified\n");
                argp_usage (state);
              }
      break;
    case PRIVILEGE_LEVEL_KEY: /* range 1 to 5 = callback,user,operator,admin,oem */
      if (strcasecmp (arg, "callback") == 0)
	{
	  cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_CALLBACK;
	}
      else 
	if (strcasecmp (arg, "user") == 0)
	  {
	    cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
	  }
	else 
	  if (strcasecmp (arg, "operator") == 0)
	    {
	      cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_OPERATOR;
	    }
	  else 
	    if (strcasecmp (arg, "admin") == 0)
	      {
		cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
	      }
	    else 
	      if (strcasecmp (arg, "oem") == 0)
		{
		  cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_OEM;
		}
	      else 
		{
                  fprintf(stderr, "invalid privilege level specified\n");
                  argp_usage (state);
		}
      break;
#ifndef NDEBUG
    case DEBUG_KEY:
      cmd_args->debug = 1;
      break;
#endif /* NDEBUG */
    default:
      return ARGP_ERR_UNKNOWN;
    }
  
  return 0;
}

void 
init_common_cmd_args (struct common_cmd_args *cmd_args)
{
  cmd_args->disable_auto_probe = 0;
  cmd_args->driver_type = IPMI_DEVICE_UNKNOWN;
  cmd_args->driver_address = 0;
  cmd_args->driver_device = NULL;
  cmd_args->register_spacing = 0;
  cmd_args->session_timeout = 0;
  cmd_args->retry_timeout = 0;
  cmd_args->host = NULL;
  cmd_args->username = NULL;
  cmd_args->password = NULL;
  cmd_args->authentication_type = IPMI_AUTHENTICATION_TYPE_MD5;
  cmd_args->privilege_level = IPMI_PRIVILEGE_LEVEL_USER;
#ifndef NDEBUG
  cmd_args->debug = 0;
#endif /* NDEBUG */
}

void 
free_common_cmd_args (struct common_cmd_args *cmd_args)
{
  cmd_args->disable_auto_probe = 0;
  cmd_args->driver_type = IPMI_DEVICE_UNKNOWN;
  cmd_args->driver_address = 0;
  if (cmd_args->driver_device != NULL)
    {
      free (cmd_args->driver_device);
      cmd_args->driver_device = NULL;
    }
  if (cmd_args->host != NULL)
    {
      free (cmd_args->host);
      cmd_args->host = NULL;
    }
  if (cmd_args->username != NULL)
    {
      free (cmd_args->username);
      cmd_args->username = NULL;
    }
  if (cmd_args->password != NULL)
    {
      free (cmd_args->password);
      cmd_args->password = NULL;
    }
  cmd_args->authentication_type = 0;
  cmd_args->privilege_level = 0;
}

