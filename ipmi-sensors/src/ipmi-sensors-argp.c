/* 
   $Id: ipmi-sensors-argp.c,v 1.5 2006-12-15 22:23:19 chu11 Exp $ 
   
   ipmi-sensors-argp.c - IPMI Sensors utility.
   
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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <argp.h>
#include <ctype.h>
#include <errno.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "argp-common.h"
#include "ipmi-sensor-api.h"
#include "ipmi-sensors-argp.h"
#include "ipmi-sensors-utils.h"

#include "freeipmi-portability.h"

static struct arguments cmd_args;

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI Sensors [ipmi-sensors-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI Sensors - displays current readings of sensor chips through BMC.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS, 
    {"verbose",     VERBOSE_KEY,      0, 0, 
     "Increase verbosity in output.  More -v adds more verbosity.", 9}, 
    {"sdr-info",    SDR_INFO_KEY,     0, 0, 
     "Show SDR Information."}, 
    {"flush-cache", FLUSH_CACHE_KEY,  0, 0, 
     "Flush sensor cache."}, 
    {"quiet-cache", QUIET_CACHE_KEY,  0, 0,
     "Do not output cache creation information."},
    {"list-groups", LIST_GROUPS_KEY,  0, 0, 
     "List sensor groups."}, 
    {"group",       GROUP_KEY,        "GROUP", 0, 
     "Show sensors belongs to this GROUP."}, 
    {"sensors",     SENSORS_LIST_KEY, "SENSORS-LIST", 0, 
     "Show listed sensors."}, 
    {"sdr-cache-directory", SDR_CACHE_DIR_KEY, "DIRECTORY", 0, 
     "Use DIRECTORY for sensor cache."}, 
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static char *
stripwhite (char *string)
{
  register char *s, *t;

  for (s = string; isspace (*s); s++)
    ;

  if (*s == 0)
    return s;

  t = s + strlen (s) - 1;
  while (t > s && isspace (*t))
    t--;
  *++t = '\0';

  return s;
}

static char *
get_token (char **line)
{
  char *command;
  while (1)
    {
      command = (char *) strsep (line, " ,");
      if (!command)
        break;
      if (*(command))
        break;
    }
  
  if (command)
    return strdup (stripwhite (command));
  return command;
}

static int 
validate_sensor_list_string (char *sensor_list_string)
{
  char *dlist = NULL;
  
  if (sensor_list_string == NULL)
    return (-1);
  
  dlist = strdupa (sensor_list_string);
  while (sensor_list_string)
    {
      unsigned int value = 0;
      char *token = NULL;
      char *str = NULL;
      
      token = get_token (&dlist);
      if (token == NULL)
	break;
      str = strdupa (token);
      free (token);
      
      if (str2uint (str, 10, &value))
        return (-1);
    }
  
  return 0;
}

static int 
get_sensor_list_count (char *sensor_list_string)
{
  int count = 0;
  char *dlist = NULL;
  
  if (sensor_list_string == NULL)
    return (-1);
  
  dlist = strdupa (sensor_list_string);
  while (sensor_list_string)
    {
      char *str = NULL;
      
      str = get_token (&dlist);
      if (str == NULL)
	break;
      free (str);
      
      count++;
    }
  
  return count;
}

static int 
get_sensor_list (char *sensor_list_string, unsigned int *records, int count)
{
  int i = 0;
  char *dlist = NULL;
  
  if (sensor_list_string == NULL || 
      records == NULL)
    return (-1);
  
  dlist = strdupa (sensor_list_string);
  for (i = 0; i < count; i++)
    {
      unsigned int value = 0;
      char *str = NULL;
      
      str = get_token (&dlist);
      if (str == NULL)
	break;
      
      str2uint (str, 10, &value);
      records[i] = value;
      
      free (str);
    }
  
  return 0;
}

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct arguments *cmd_args = state->input;
  
  switch (key)
    {
    case VERBOSE_KEY:
      cmd_args->verbose_wanted = 1;
      cmd_args->verbose_count++;
      break;
    case SDR_INFO_KEY:
      cmd_args->sdr_info_wanted = 1;
      break;
    case FLUSH_CACHE_KEY:
      cmd_args->flush_cache_wanted = 1;
      break;
    case QUIET_CACHE_KEY:
      cmd_args->quiet_cache_wanted = 1;
      break;
    case LIST_GROUPS_KEY:
      cmd_args->list_groups_wanted = 1;
      break;
    case GROUP_KEY:
      cmd_args->group_wanted = 1;
      cmd_args->group = strdup (arg);
      break;
    case SENSORS_LIST_KEY:
      cmd_args->sensors_list_wanted = 1;
      {
	char *sensors_list_arg = strdupa (arg);
	
	if (validate_sensor_list_string (sensors_list_arg) == -1)
	  {
	    fprintf (stderr, "invalid integer in sensors list\n");
	    argp_usage (state);
	    break;
	  }
	
	if (cmd_args->sensors_list)
	  free (cmd_args->sensors_list);
	
	cmd_args->sensors_list_length = 
	  get_sensor_list_count (sensors_list_arg);
	cmd_args->sensors_list = 
	  calloc (cmd_args->sensors_list_length, sizeof (int));
	get_sensor_list (sensors_list_arg, 
			 cmd_args->sensors_list, 
			 cmd_args->sensors_list_length);
      }
      break;
    case SDR_CACHE_DIR_KEY:
      cmd_args->sdr_cache_dir_wanted = 1;
      cmd_args->sdr_cache_dir = strdup (arg);
      if (access (cmd_args->sdr_cache_dir, R_OK|W_OK|X_OK) != 0)
	{
	  fprintf (stderr, "insufficient permission on sensor cache directory [%s]\n", 
		   cmd_args->sdr_cache_dir);
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
      return common_parse_opt (key, arg, state, 
			       &(cmd_args->common));
    }
  
  return 0;
}

void 
ipmi_sensors_argp_parse (int argc, char **argv)
{
  init_common_cmd_args (&(cmd_args.common));
  cmd_args.verbose_wanted = 0;
  cmd_args.verbose_count = 0;
  cmd_args.sdr_info_wanted = 0;
  cmd_args.flush_cache_wanted = 0;
  cmd_args.quiet_cache_wanted = 0;
  cmd_args.list_groups_wanted = 0;
  cmd_args.group_wanted = 0;
  cmd_args.group = NULL;
  cmd_args.sensors_list_wanted = 0;
  cmd_args.sensors_list = NULL;
  cmd_args.sensors_list_length = 0;
  cmd_args.sdr_cache_dir_wanted = 0;
  cmd_args.sdr_cache_dir = NULL;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, &cmd_args);
}

struct arguments *
ipmi_sensors_get_arguments ()
{
  return &cmd_args;
}

