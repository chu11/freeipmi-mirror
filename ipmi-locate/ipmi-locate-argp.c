/*
 * Copyright (C) 2005-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */
#include <assert.h>

#include "ipmi-locate_.h"
#include "ipmi-locate-argp.h"

#include "freeipmi-portability.h"

const char *argp_program_version =
  "ipmi-locate - " PACKAGE_VERSION "\n"
  "Copyright (C) 2005-2014 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-locate - IPMI probing utility";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    { "defaults", DEFAULTS_KEY, NULL, 0,
      "Display system defaults.", 40},
    { NULL, 0, NULL, 0, NULL, 0}
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc };

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmi_locate_arguments *cmd_args;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case DEFAULTS_KEY:
      cmd_args->defaults++;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return (ARGP_ERR_UNKNOWN);
    }

  return (0);
}

void
ipmi_locate_argp_parse (int argc, char **argv, struct ipmi_locate_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  cmd_args->defaults = 0;

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);
}

