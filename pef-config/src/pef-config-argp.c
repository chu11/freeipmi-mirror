/* 
   $Id: pef-config-argp.c,v 1.6 2007-09-05 20:13:46 chu11 Exp $ 
   
   pef-config-argp.c - Platform Event Filtering utility.
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <errno.h>

#include "cmdline-parse-common.h"
#include "pef-config.h"
#include "pef-config-argp.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
"IPMI PEF [pef-config-" PACKAGE_VERSION "]\n"
"Copyright (C) 2003-2005 FreeIPMI Core Team\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI PEF - PEF configuration utility.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"info",       INFO_KEY,       0, 0, 
     "Show general information about PEF configuration.", 25},
    {"checkout",   CHECKOUT_KEY,   0, 0,
     "Fetch configuration information from the BMC.", 26},
    {"commit",     COMMIT_KEY,     0, 0,
     "Update configuration information to the BMC from a config file.", 27},
    {"diff",       DIFF_KEY,       0, 0,
     "Show differences between the BMC and a config file.", 28},
    {"listsections", LIST_SECTIONS_KEY, 0, 0,
     "List available sections for checkout.", 29},
    {"verbose", VERBOSE_KEY, 0, 0,  
     "Print additional detailed information.", 30},
    {"filename", FILENAME_KEY, "FILENAME", 0,
     "Specify a PEF config file for PEF checkout/commit/diff.", 31},
    {"section", SECTIONS_KEY, "SECTION", 0,
     "Specify a SECTION for checkout.", 32},
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static struct sectionstr *
_create_sectionstr(char *arg)
{
  struct sectionstr *s;

  if (!(s = (struct sectionstr *)malloc(sizeof(struct sectionstr))))
    {
      perror("malloc");
      exit(1);
    }
  if (!(s->sectionstr = strdup(arg)))
    {
      perror("strdup");
      exit(1);
    }
  s->next = NULL;

  return s;
}

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct pef_config_arguments *cmd_args = state->input;
  struct sectionstr *sstr;
  
  switch (key)
    {
    case INFO_KEY:
      cmd_args->action = PEF_ACTION_INFO;
      break;
    case CHECKOUT_KEY:
      cmd_args->action = PEF_ACTION_CHECKOUT;
      break;
    case COMMIT_KEY:
      cmd_args->action = PEF_ACTION_COMMIT;
      break;
    case DIFF_KEY:
      cmd_args->action = PEF_ACTION_DIFF;
      break;
    case LIST_SECTIONS_KEY:
      cmd_args->action = PEF_ACTION_LIST_SECTIONS;
      break;
    case VERBOSE_KEY:
      cmd_args->verbose = 1;
      break;
    case FILENAME_KEY:
      if (cmd_args->filename) /* If specified more than once */
        free (cmd_args->filename);
      if (!(cmd_args->filename = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case SECTIONS_KEY:
      sstr = _create_sectionstr(arg);
      if (cmd_args->sectionstrs)
        {
          struct sectionstr *p = NULL;

          p = cmd_args->sectionstrs;
          while (p->next)
            p = p->next;

          p->next = sstr;
        }
      else
        cmd_args->sectionstrs = sstr;
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
pef_config_argp_parse (int argc, char **argv, struct pef_config_arguments *cmd_args)
{
  init_common_cmd_args (&(cmd_args->common));
  cmd_args->action = 0;
  cmd_args->verbose = 0;
  cmd_args->filename = NULL;
  cmd_args->sectionstrs = NULL;

  /* ADMIN is minimum for pef-config b/c its needed for many of the
   * ipmi cmds used
   */
  cmd_args->common.privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
}

int
pef_config_args_validate (struct pef_config_arguments *args)
{
  int ret = 0;

  // action is non 0 and -1
  if (! args->action || args->action == -1)
    {
      fprintf (stderr,
               "Exactly one of --info, --checkout, --commit, --diff, or --listsections MUST be given\n");
      return -1;
    }

  if (args->filename)
    {
      switch (args->action)
        {
        case PEF_ACTION_COMMIT: case PEF_ACTION_DIFF:
          if (access (args->filename, R_OK) != 0)
            {
              perror (args->filename);
              return -1;
            }
          break;
        case PEF_ACTION_CHECKOUT:
          if (access (args->filename, F_OK) == 0)
            {
              if (access (args->filename, W_OK) != 0)
                {
                  perror (args->filename);
                  return -1;
                }
            }
          else
            {
              int fd;
              fd = open (args->filename, O_CREAT);
              if (fd == -1)
                {
                  perror (args->filename);
                  return -1;
                }
              else
                {
                  close (fd);
                  unlink (args->filename);
                }
            }
          break;
        case PEF_ACTION_INFO:
        case PEF_ACTION_LIST_SECTIONS:
          /* do nothing - here to remove compile warning */
          break;
        }
    }

  return ret;
}

