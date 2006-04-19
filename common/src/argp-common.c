/*
argp-common.c: common work for argp for all freeipmi tools.
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
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <error.h>
#include <argp.h>

#include "freeipmi.h"
#include "argp-common.h"

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
		{
		  argp_usage (state);
		}
      break;
    case DRIVER_ADDRESS_KEY:
      cmd_args->driver_address = atol (arg);
      break;
    case DRIVER_DEVICE_KEY:
      if (cmd_args->driver_device != NULL)
	free (cmd_args->driver_device);
      cmd_args->driver_device = strdup (arg);
      break;
    case PACKET_RETRY_TIMEOUT_KEY:
      cmd_args->packet_retry_timeout = atol (arg);
      break;
    case PACKET_RETRY_MAX_KEY:
      cmd_args->packet_retry_max = atol (arg);
      break;
    case HOSTNAME_KEY:
      if (cmd_args->host != NULL)
	free (cmd_args->host);
      cmd_args->host = strdup (arg);
      break;
    case USERNAME_KEY:
      if (strlen (arg) > IPMI_SESSION_MAX_USERNAME_LEN)
	argp_usage (state);
      else 
	{
	  if (cmd_args->username != NULL)
	    free (cmd_args->username);
	  cmd_args->username = strdup (arg);
	}
      break;
    case PASSWORD_KEY:
      if (strlen (arg) > IPMI_SESSION_MAX_AUTH_CODE_LEN)
	argp_usage (state);
      else 
	{
	  if (cmd_args->password != NULL)
	    free (cmd_args->password);
	  cmd_args->password = strdup (arg);
	}
      break;
    case AUTH_TYPE_KEY: /* values 0,1,2,4,5 = none,md2,md5,straight,oem */
      if (strcasecmp (arg, "none") == 0)
	{
	  cmd_args->auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
	}
      else 
	if (strcasecmp (arg, "md2") == 0)
	  {
	    cmd_args->auth_type = IPMI_SESSION_AUTH_TYPE_MD2;
	  }
	else 
	  if (strcasecmp (arg, "md5") == 0)
	    {
	      cmd_args->auth_type = IPMI_SESSION_AUTH_TYPE_MD5;
	    }
	  else 
	    if (strcasecmp (arg, "plain") == 0)
	      {
		cmd_args->auth_type = IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY;
	      }
	    else 
	      if (strcasecmp (arg, "oem") == 0)
		{
		  cmd_args->auth_type = IPMI_SESSION_AUTH_TYPE_OEM_PROP;
		}
	      else 
		{
		  argp_usage (state);
		}
      break;
    case PRIV_LEVEL_KEY: /* range 1 to 5 = callback,user,operator,admin,oem */
      if (strcasecmp (arg, "callback") == 0)
	{
	  cmd_args->priv_level = IPMI_PRIV_LEVEL_CALLBACK;
	}
      else 
	if (strcasecmp (arg, "user") == 0)
	  {
	    cmd_args->priv_level = IPMI_PRIV_LEVEL_USER;
	  }
	else 
	  if (strcasecmp (arg, "operator") == 0)
	    {
	      cmd_args->priv_level = IPMI_PRIV_LEVEL_OPERATOR;
	    }
	  else 
	    if (strcasecmp (arg, "admin") == 0)
	      {
		cmd_args->priv_level = IPMI_PRIV_LEVEL_ADMIN;
	      }
	    else 
	      if (strcasecmp (arg, "oem") == 0)
		{
		  cmd_args->priv_level = IPMI_PRIV_LEVEL_OEM;
		}
	      else 
		{
		  argp_usage (state);
		}
      break;
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
  cmd_args->packet_retry_timeout = 0;
  cmd_args->packet_retry_max = 0;
  cmd_args->host = NULL;
  cmd_args->username = NULL;
  cmd_args->password = NULL;
  cmd_args->auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
  cmd_args->priv_level = IPMI_PRIV_LEVEL_USER;
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
  cmd_args->auth_type = 0;
  cmd_args->priv_level = 0;
}

