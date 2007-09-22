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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <argp.h>
#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#ifdef HAVE_SYS_IO_H
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

#include "cmdline-parse-common.h"

#include "bmc-config-argp.h"

#include "bmc-config.h"
#include "bmc-config-sections.h"

const char *argp_program_version = PACKAGE_VERSION;
const char *argp_program_bug_address = "<" PACKAGE_BUGREPORT ">";
/* Program documentation. */
static char doc[] =  "GNU FreeIPMI (bmc-config) -- BMC config tool";
/* A description of the arguments we accept. */
static char args_doc[] = "";

/* The options we understand. */
static struct argp_option options[] = {
  ARGP_COMMON_OPTIONS_DRIVER,
  ARGP_COMMON_OPTIONS_INBAND,
  ARGP_COMMON_OPTIONS_OUTOFBAND,
  ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
  ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
  ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
  ARGP_COMMON_OPTIONS_DEBUG,
  {"checkout", CHECKOUT_KEY, 0, 0, 
   "Fetch configuration information from the BMC.", 25},
  {"commit", COMMIT_KEY, 0, 0, 
   "Update configuration information to the BMC from a config file or key pairs.", 26},
  {"diff", DIFF_KEY, 0, 0, 
   "Show differences between the BMC and a config file or key pairs.", 27},
  {"filename", FILENAME_KEY, "FILENAME", 0, 
   "Specify a BMC config file for BMC checkout/commit/diff.", 28},
  {"key-pair", KEYPAIR_KEY, "KEY-PAIR", 0, 
   "Specify KEY=VALUE pairs for checkout/commit/diff.", 29},
  {"section", SECTIONS_KEY, "SECTION", 0,
   "Specify a SECTION for checkout.", 30},
  {"listsections", LIST_SECTIONS_KEY, 0, 0,
   "List available sections for checkout.", 31},
  {"verbose", VERBOSE_KEY, 0, 0,  
   "Print additional detailed information.", 32},
  { 0, }
};

static error_t parse_opt (int key, char *arg, struct argp_state *state);

/* Our argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc};

static struct keypair *
_create_keypair(char *arg)
{
  struct keypair *kp;
  
  if (!(kp = (struct keypair *)malloc(sizeof(struct keypair))))
    {
      perror("malloc");
      exit(1);
    }
  if (!(kp->keypair = strdup(arg)))
    {
      perror("strdup");
      exit(1);
    }
  kp->next = NULL;

  return kp;
}

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

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct bmc_config_arguments *cmd_args = state->input;
  struct keypair *kp;
  struct sectionstr *sstr;

  switch (key)
    {
    case CHECKOUT_KEY:
      if (!cmd_args->action)
	cmd_args->action = CONFIG_ACTION_CHECKOUT;
      else
	cmd_args->action = -1;
      break;
    case COMMIT_KEY:
      if (!cmd_args->action)
	cmd_args->action = CONFIG_ACTION_COMMIT;
      else
	cmd_args->action = -1;
      break;
    case DIFF_KEY:
      if (!cmd_args->action)
	cmd_args->action = CONFIG_ACTION_DIFF;
      else
	cmd_args->action = -1;
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
    case KEYPAIR_KEY:
      kp = _create_keypair(arg);
      if (cmd_args->keypairs)
        {
          struct keypair *p = NULL;
          
          p = cmd_args->keypairs;
          while (p->next)
            p = p->next;
          
          p->next = kp;
        }
      else
        cmd_args->keypairs = kp;
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
    case LIST_SECTIONS_KEY:
      if (!cmd_args->action)
	cmd_args->action = CONFIG_ACTION_LIST_SECTIONS;
      else
	cmd_args->action = -1;
      break;
    case VERBOSE_KEY:
      cmd_args->verbose = 1;
      break;
    case ARGP_KEY_ARG:
      argp_usage (state);
      break;
    default:
      return common_parse_opt (key, arg, state, &cmd_args->common);
    }
  return 0;
}

void
bmc_config_argp_parse (int argc, char *argv[], struct bmc_config_arguments *cmd_args)
{
  init_common_cmd_args (&(cmd_args->common));
  cmd_args->verbose = 0;
  cmd_args->filename = NULL;
  cmd_args->keypairs = NULL;
  cmd_args->sectionstrs = NULL;
  cmd_args->action = 0;

  /* ADMIN is minimum for bmc-config b/c its needed for many of the
   * ipmi cmds used
   */
  cmd_args->common.privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
}

int
bmc_config_args_validate (struct bmc_config_arguments *cmd_args)
{
  int ret = 0;

  // action is non 0 and -1
  if (! cmd_args->action || cmd_args->action == -1) 
    {
      fprintf (stderr, 
               "Exactly one of --checkout, --commit, --diff, or --listsections MUST be given\n");
      return -1;
    }

  // filename and keypair both given for checkout or diff
  if (cmd_args->filename && cmd_args->keypairs 
      && (cmd_args->action == CONFIG_ACTION_CHECKOUT
          || cmd_args->action == CONFIG_ACTION_DIFF))
    {
      fprintf (stderr, 
               "Both --filename or --keypair cannot be used\n");
      return -1;
    }

  // only one of keypairs or section can be given for checkout
  if (cmd_args->action == CONFIG_ACTION_CHECKOUT
      && (cmd_args->keypairs && cmd_args->sectionstrs))
    {
      fprintf (stderr, 
               "Only one of --filename, --keypair, and --section can be used\n");
      return -1;
    }

  // filename is readable if commit, writable/creatable if checkout

  if (cmd_args->filename) 
    {
      switch (cmd_args->action) 
        {
        case CONFIG_ACTION_COMMIT: case CONFIG_ACTION_DIFF:
          if (access (cmd_args->filename, R_OK) != 0) 
            {
              perror (cmd_args->filename);
              return -1;
            }
          break;
        case CONFIG_ACTION_CHECKOUT:
          if (access (cmd_args->filename, F_OK) == 0) 
            {
              if (access (cmd_args->filename, W_OK) != 0) 
                {
                  perror (cmd_args->filename);
                  return -1;
                }
            } 
          else 
            {
              int fd;
              fd = open (cmd_args->filename, O_CREAT);
              if (fd == -1) 
                {
                  perror (cmd_args->filename);
                  return -1;
                } 
              else 
                {
                  close (fd);
                  unlink (cmd_args->filename);
                }
            }
          break;
        case CONFIG_ACTION_LIST_SECTIONS:
        case CONFIG_ACTION_INFO:
          /* do nothing - here to remove compile warning */
          break;
        }
    }
  
  return ret;
}
