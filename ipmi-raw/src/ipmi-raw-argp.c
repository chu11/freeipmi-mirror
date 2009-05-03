/* 
   Copyright (C) 2005-2008 FreeIPMI Core Team
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */
#include <limits.h>
#include <errno.h>

#include "ipmi-raw.h"
#include "ipmi-raw-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version = 
  "ipmi-raw - " PACKAGE_VERSION "\n"
  "Copyright (C) 2005-2008 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = 
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] = 
  "ipmi-raw - execute IPMI commands by hex values";

static char cmdline_args_doc[] = 
  "[<lun> <netfn> COMMAND-HEX-BYTES]";

static struct argp_option cmdline_options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_USER,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"file", CMD_FILE_KEY, "CMD-FILE", 0, 
     "Specify a file to read command requests from.", 30}, 
    { 0 }
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse, 
                                    cmdline_args_doc, 
                                    cmdline_doc };

static struct argp cmdline_config_file_argp = { cmdline_options,
                                                cmdline_config_file_parse,
                                                cmdline_args_doc,
                                                cmdline_doc };

static error_t 
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmi_raw_arguments *cmd_args = state->input;
  error_t ret;
  
  switch (key)
    {
    case CMD_FILE_KEY:
      if (!(cmd_args->cmd_file = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case ARGP_KEY_ARG:
      {
	int i;
	long value;
	
	if (strlen(arg) >= 2)
	  {
	    if (strncmp(arg, "0x", 2) == 0)
	      arg+=2;
	  }

        if (*arg == '\0')
          {
            fprintf (stderr, "invalid hex byte argument\n");
            exit(1);
          }

	for (i = 0; arg[i] != (char) NULL; i++)
	  {
	    if (i >= 2)
	      {
		fprintf (stderr, "invalid hex byte argument\n");
                exit(1);
	      }
	    
	    if (isxdigit (arg[i]) == 0)
	      {
		fprintf (stderr, "invalid hex byte argument\n");
                exit(1);
	      }
	  }
	
	value = strtol (arg, (char **) NULL, 16);
	cmd_args->cmd[cmd_args->cmd_length++] = (uint8_t) value;
	
	break;
      }
    case ARGP_KEY_END:
      break;
    default:
      ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->hostrange));
      return ret;
    }
  
  return 0;
}

static void
_ipmi_raw_config_file_parse(struct ipmi_raw_arguments *cmd_args)
{
  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         NULL,
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_RAW,
                         NULL) < 0)
    {
      fprintf(stderr, "config_file_parse: %s\n", strerror(errno));
      exit(1);
    }
}

void 
ipmi_raw_argp_parse (int argc, char **argv, struct ipmi_raw_arguments *cmd_args)
{
  init_common_cmd_args_user (&(cmd_args->common));
  init_hostrange_cmd_args (&(cmd_args->hostrange));

  cmd_args->cmd_file = NULL;
  errno = 0;
  if ((cmd_args->arg_max = sysconf(_SC_ARG_MAX)) <= 0)
    {
      if (errno)
        {
          perror("sysconf");
          exit(1);
        }
      cmd_args->arg_max = LONG_MAX;
    }
  if (!(cmd_args->cmd = calloc(cmd_args->arg_max, sizeof(uint8_t))))
    {
      perror("calloc");
      exit(1);
    }
  cmd_args->cmd_length = 0;

  argp_parse (&cmdline_config_file_argp, argc, argv, ARGP_IN_ORDER, NULL, &(cmd_args->common));

  _ipmi_raw_config_file_parse(cmd_args);

  argp_parse (&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}


