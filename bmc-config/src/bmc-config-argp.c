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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
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

#include "argp-common.h"

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
  ARGP_COMMON_OPTIONS_AUTHTYPE,
  ARGP_COMMON_OPTIONS_PRIVLEVEL_ADMIN,
#ifndef NDEBUG
  ARGP_COMMON_OPTIONS_DEBUG,
#endif /* NDEBUG */
  {"checkout", 'o', 0, 0, 
   "Action is to GET the BMC configuration", 24},
  {"commit", 'i', 0, 0, 
   "Action is to UPDATE the BMC configuration", 25},
  {"diff", 'd', 0, 0, 
   "Action is to SHOW THE DIFFERENCES with BMC", 26},

  {"filename", 'f', "FILENAME", 0, 
   "use FILENAME in checkout, commit or diff", 27},
  {"key-pair", 'k', "KEY-PAIR", 0, 
   "use KEY-PAIR in checkout, commit or diff", 28},
  {"section", 'S', "SECTION", 0,
   "use SECTION in checkout", 29},
  {"listsections", 'L', 0, 0,
   "List available sections for checkout", 30},

  {"verbose",   'v', 0, 0,  "Produce verbose output", 31},
  {"quiet",     'q', 0, 0,  "Do not produce any output", 32},
  {"silent",    's', 0, OPTION_ALIAS },
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
  struct bmc_config_arguments *args = state->input;
  struct keypair *kp;
  struct sectionstr *sstr;

  switch (key)
    {
    case 'q': 
    case 's':
      args->silent = 1;
      break;
    case 'v':
      args->verbose = 1;
      break;
    case 'f':
      if (args->filename) /* If specified more than once */
	free (args->filename);
      if (!(args->filename = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;

    case 'k':

      kp = _create_keypair(arg);
      if (args->keypairs)
        {
          struct keypair *p = NULL;
          
          p = args->keypairs;
          while (p->next)
            p = p->next;
          
          p->next = kp;
        }
      else
        args->keypairs = kp;
      break;

    case 'S':
      sstr = _create_sectionstr(arg);
      if (args->sectionstrs)
        {
          struct sectionstr *p = NULL;
          
          p = args->sectionstrs;
          while (p->next)
            p = p->next;
          
          p->next = sstr;
        }
      else
        args->sectionstrs = sstr;
      break;

    case 'L':
      if (! args->action)
	args->action = BMC_ACTION_LIST_SECTIONS;
      else
	args->action = -1;
      break;

    case 'o':
      if (! args->action)
	args->action = BMC_ACTION_CHECKOUT;
      else
	args->action = -1;
      break;

    case 'i':
      if (! args->action)
	args->action = BMC_ACTION_COMMIT;
      else
	args->action = -1;
      break;

    case 'd':
      if (! args->action)
	args->action = BMC_ACTION_DIFF;
      else
	args->action = -1;
      break;

    case ARGP_KEY_ARG:
      argp_usage (state);
      break;

    default:
      return common_parse_opt (key, arg, state, &args->common);
    }
  return 0;
}

void
bmc_config_argp (int argc, char *argv[], struct bmc_config_arguments *args)
{
  init_common_cmd_args (&(args->common));
  args->silent = 0;
  args->verbose = 0;
  args->filename = NULL;
  args->keypairs = NULL;
  args->sectionstrs = NULL;
  args->action = 0;

  /* ADMIN is minimum for bmc-config b/c its needed for many of the
   * ipmi cmds used
   */
  args->common.privilege_level = IPMI_PRIVILEGE_LEVEL_ADMIN;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, args);
}

int
bmc_config_args_validate (struct bmc_config_arguments *args)
{
  int ret = 0;

  // action is non 0 and -1
  if (! args->action || args->action == -1) 
    {
      fprintf (stderr, 
               "Exactly one of --checkout, --commit, --diff, or --listsections MUST be given\n");
      return -1;
    }

  // filename and keypair both given for checkout or diff
  if (args->filename && args->keypairs 
      && (args->action == BMC_ACTION_CHECKOUT
          || args->action == BMC_ACTION_DIFF))
    {
      fprintf (stderr, 
               "Both --filename or --keypair cannot be used\n");
      return -1;
    }

  // only one of keypairs or section can be given for checkout
  if (args->action == BMC_ACTION_CHECKOUT
      && (args->keypairs && args->sectionstrs))
    {
      fprintf (stderr, 
               "Only one of --filename, --keypair, and --section can be used\n");
      return -1;
    }

  // filename is readable if commit, writable/creatable if checkout

  if (args->filename) 
    {
      switch (args->action) 
        {
        case BMC_ACTION_COMMIT: case BMC_ACTION_DIFF:
          if (access (args->filename, R_OK) != 0) 
            {
              perror (args->filename);
              return -1;
            }
          break;
        case BMC_ACTION_CHECKOUT:
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
        case BMC_ACTION_LIST_SECTIONS:
          /* do nothing - here to remove compile warning */
          break;
        }
    }
  
  return ret;
}
