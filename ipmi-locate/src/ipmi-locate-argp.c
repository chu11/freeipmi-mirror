/* 
   $Id: ipmi-locate-argp.c,v 1.1.2.1 2005-12-20 19:05:00 chu11 Exp $ 
   
   ipmi-locate-argp.c - command line argument parser.
   
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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include "common.h"

static struct arguments arguments;

static error_t parse_opt (int key, char *arg, struct argp_state *state);

static char version_doc[] = 
N_("IPMI Locate [ipmi-locate-" PACKAGE_VERSION "]\n"
   "Copyright (C) 2003-2005 FreeIPMI Core Team\n"
   "This program is free software; you may redistribute it under the terms of\n"
   "the GNU General Public License.  This program has absolutely no warranty.");

void (*argp_program_version_hook) (FILE *, struct argp_state *) = fi_show_version;
const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = N_("IPMI Locate - Probes and displays IPMI devices.");

static char args_doc[] = N_("");

static struct argp_option options[] = 
  {
    { "quiet",                QUIET_KEY,         NULL,          0, 
      N_("Inhibit usual output."), 0 },
    { "silent",               0,                 NULL,          OPTION_ALIAS, 
      NULL, 0 },
    { "brief",                BRIEF_KEY,         NULL,          0, 
      N_("Shorten output."), 0 },
    { "verbose",              VERBOSE_KEY,       NULL,          0, 
      N_("Print more information."), 0 },
    { NULL, 0, NULL, 0, NULL, 0 }
  };

static struct argp argp = 
{
  options, parse_opt, args_doc, doc, NULL, NULL, NULL
};

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct arguments *arguments = state->input;
  
  switch (key)
    {
    case QUIET_KEY:
      arguments->quiet = 1;
      break;
    case BRIEF_KEY:
      arguments->brief = 1;
      break;
    case VERBOSE_KEY:
      arguments->verbose = 1;
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

void 
fi_show_version (FILE *stream, struct argp_state *state)
{
  (void) state;
  
  fprintf (stream, "%s\n", version_doc);
}

void 
fi_argp_parse (int argc, char **argv)
{
  arguments.quiet = 0;
  arguments.brief = 0;
  arguments.verbose = 0;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, &arguments);
}

struct arguments *
fi_get_arguments ()
{
  return &arguments;
}

int 
fi_set_arguments (struct arguments *args)
{
  arguments.quiet = args->quiet;
  arguments.brief = args->brief;
  arguments.verbose = args->verbose;
  
  return (0);
}
