/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-argp.h"
#include "ipmi-config-tool-utils.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-config - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2013 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-config - configure BMC values";

static char cmdline_args_doc[] = "";

/* The options we understand. */
static struct argp_option cmdline_options[] = {
  ARGP_COMMON_OPTIONS_DRIVER,
  ARGP_COMMON_OPTIONS_INBAND,
  ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED,
  ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
  ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
  ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
  ARGP_COMMON_OPTIONS_CONFIG_FILE,
  ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
  ARGP_COMMON_HOSTRANGED_OPTIONS,
  ARGP_COMMON_OPTIONS_DEBUG,
  { "checkout", IPMI_CONFIG_ARGP_CHECKOUT_KEY, 0, 0,
    "Fetch configuration information.", 40},
  { "commit", IPMI_CONFIG_ARGP_COMMIT_KEY, 0, 0,
    "Update configuration information from a config file or key pairs.", 41},
  { "diff", IPMI_CONFIG_ARGP_DIFF_KEY, 0, 0,
    "Show differences between stored information and a config file or key pairs.", 42},
  { "filename", IPMI_CONFIG_ARGP_FILENAME_KEY, "FILENAME", 0,
    "Specify a config file for checkout/commit/diff.", 42},
  { "key-pair", IPMI_CONFIG_ARGP_KEYPAIR_KEY, "KEY-PAIR", 0,
    "Specify KEY=VALUE pairs for checkout/commit/diff.", 43},
  { "section", IPMI_CONFIG_ARGP_SECTIONS_KEY, "SECTION", 0,
    "Specify a SECTION for checkout.", 44},
  { "listsections", IPMI_CONFIG_ARGP_LIST_SECTIONS_KEY, 0, 0,
    "List available sections for checkout.", 45},
  { "verbose", IPMI_CONFIG_ARGP_VERBOSE_KEY, 0, 0,
    "Print additional detailed information.", 46},
  { "lan-channel-number", IPMI_CONFIG_ARGP_LAN_CHANNEL_NUMBER_KEY, "NUMBER", 0,
    "Use a specific LAN Channel Number.", 47},
  { "serial-channel-number", IPMI_CONFIG_ARGP_SERIAL_CHANNEL_NUMBER_KEY, "NUMBER", 0,
    "Use a specific Serial Channel Number.", 48},
  { "sol-channel-number", IPMI_CONFIG_ARGP_SOL_CHANNEL_NUMBER_KEY, "NUMBER", 0,
    "Use a specific SOL Channel Number.", 49},
  { "foobar", IPMI_CONFIG_ARGP_FILENAME_KEY_LEGACY, "FILENAME", OPTION_HIDDEN,
    "Specify a config file for checkout/commit/diff.", 50},
  { NULL, 0, NULL, 0, NULL, 0}
};

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc};

static struct argp cmdline_config_file_argp = { cmdline_options,
                                                cmdline_config_file_parse,
                                                cmdline_args_doc,
                                                cmdline_doc };

static void
_ipmi_config_parse_channel_number (char *arg,
				   uint8_t *channel_number,
				   int *channel_number_set)
{
  char *endptr;
  int tmp;

  assert (arg);
  assert (channel_number);
  assert (channel_number_set);

  tmp = strtol (arg, &endptr, 0);
  if (errno
      || endptr[0] != '\0')
    {
      fprintf (stderr, "invalid channel number\n");
      exit (EXIT_FAILURE);
    }
  if (!IPMI_CHANNEL_NUMBER_VALID (tmp))
    {
      fprintf (stderr, "invalid channel number\n");
      exit(1);
    }
  (*channel_number) = (uint8_t)tmp;
  (*channel_number_set)++;
}

/* Parse a single option. */
static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmi_config_arguments *cmd_args;
  struct ipmi_config_keypair *kp = NULL;
  struct ipmi_config_section_str *sstr = NULL;
  char *section_name = NULL;
  char *key_name = NULL;
  char *value = NULL;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case IPMI_CONFIG_ARGP_CHECKOUT_KEY:
      if (!cmd_args->action)
        cmd_args->action = IPMI_CONFIG_ACTION_CHECKOUT;
      else
        cmd_args->action = -1;
      break;
    case IPMI_CONFIG_ARGP_COMMIT_KEY:
      if (!cmd_args->action)
        cmd_args->action = IPMI_CONFIG_ACTION_COMMIT;
      else
        cmd_args->action = -1;
      break;
    case IPMI_CONFIG_ARGP_DIFF_KEY:
      if (!cmd_args->action)
        cmd_args->action = IPMI_CONFIG_ACTION_DIFF;
      else
        cmd_args->action = -1;
      break;
    case IPMI_CONFIG_ARGP_FILENAME_KEY:
    case IPMI_CONFIG_ARGP_FILENAME_KEY_LEGACY:
      free (cmd_args->filename);
      if (!(cmd_args->filename = strdup (arg)))
        {
          perror ("strdup");
          exit (EXIT_FAILURE);
        }
      break;
    case IPMI_CONFIG_ARGP_KEYPAIR_KEY:
      if (ipmi_config_keypair_parse_string (arg,
					    &section_name,
					    &key_name,
					    &value) < 0)
        {
          /* error printed in function call */
          exit (EXIT_FAILURE);
        }
      if (!(kp = ipmi_config_keypair_create (section_name,
					     key_name,
					     value)))
        {
          fprintf (stderr,
                   "ipmi_config_keypair_create error\n");
          exit (EXIT_FAILURE);
        }
      if (ipmi_config_keypair_append (&(cmd_args->keypairs),
				      kp) < 0)
        {
          /* error printed in function call */
          exit (EXIT_FAILURE);
        }
      free (section_name);
      free (key_name);
      free (value);
      break;
    case IPMI_CONFIG_ARGP_SECTIONS_KEY:
      if (!(sstr = ipmi_config_section_str_create (arg)))
        {
          fprintf (stderr,
                   "ipmi_config_section_str_create error\n");
          exit (EXIT_FAILURE);
        }
      if (ipmi_config_section_str_append (&(cmd_args->section_strs),
                                     sstr) < 0)
        {
          /* error printed in function call */
          exit (EXIT_FAILURE);
        }
      sstr = NULL;
      break;
    case IPMI_CONFIG_ARGP_LIST_SECTIONS_KEY:
      if (!cmd_args->action)
        cmd_args->action = IPMI_CONFIG_ACTION_LIST_SECTIONS;
      else
        cmd_args->action = -1;
      break;
    case IPMI_CONFIG_ARGP_VERBOSE_KEY:
      cmd_args->verbose_count++;
      break;
    case IPMI_CONFIG_ARGP_LAN_CHANNEL_NUMBER_KEY:
      _ipmi_config_parse_channel_number (arg,
					 &(cmd_args->lan_channel_number),
					 &(cmd_args->lan_channel_number_set));
      break;
    case IPMI_CONFIG_ARGP_SERIAL_CHANNEL_NUMBER_KEY:
      _ipmi_config_parse_channel_number (arg,
					 &(cmd_args->serial_channel_number),
					 &(cmd_args->serial_channel_number_set));
      break;
    case IPMI_CONFIG_ARGP_SOL_CHANNEL_NUMBER_KEY:
      _ipmi_config_parse_channel_number (arg,
					 &(cmd_args->sol_channel_number),
					 &(cmd_args->sol_channel_number_set));
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return (common_parse_opt (key, arg, &cmd_args->common_args));
    }
  return (0);
}

static void
_ipmi_config_config_file_parse (struct ipmi_config_arguments *cmd_args)
{
  struct config_file_data_ipmi_config config_file_data;
  
  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmi_config));
  
  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_CONFIG,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

  if (config_file_data.verbose_count_count)
    cmd_args->verbose_count = config_file_data.verbose_count;
}

static void
_ipmi_config_args_validate (struct ipmi_config_arguments *cmd_args)
{
  assert (cmd_args);

  if (!cmd_args->action || cmd_args->action == -1)
    {
      fprintf (stderr,
               "Exactly one of --checkout, --commit, --diff, or --listsections MUST be given\n");
      exit (EXIT_FAILURE);
    }

  /* filename and keypair both given for diff */
  if (cmd_args->filename && cmd_args->keypairs
      && cmd_args->action == IPMI_CONFIG_ACTION_DIFF)
    {
      fprintf (stderr,
               "Both --filename or --keypair cannot be used\n");
      exit (EXIT_FAILURE);
    }

  /* only one of keypairs or section can be given for checkout */
  if (cmd_args->action == IPMI_CONFIG_ACTION_CHECKOUT
      && (cmd_args->keypairs && cmd_args->section_strs))
    {
      fprintf (stderr,
               "Only one of --filename, --keypair, and --section can be used\n");
      exit (EXIT_FAILURE);
    }

  /* filename is readable if commit, writable/creatable if checkout */
  if (cmd_args->filename)
    {
      switch (cmd_args->action)
        {
        case IPMI_CONFIG_ACTION_COMMIT:
        case IPMI_CONFIG_ACTION_DIFF:
          if (access (cmd_args->filename, R_OK) < 0)
            {
              fprintf (stderr,
                       "Cannot read '%s': %s\n",
                       cmd_args->filename,
                       strerror (errno));
              exit (EXIT_FAILURE);
            }
          break;
        case IPMI_CONFIG_ACTION_CHECKOUT:
          if (access (cmd_args->filename, F_OK) == 0)
            {
              if (access (cmd_args->filename, W_OK) < 0)
                {
                  fprintf (stderr,
                           "Cannot write to '%s': %s\n",
                           cmd_args->filename,
                           strerror (errno));
                  exit (EXIT_FAILURE);
                }
            }
          else
            {
              int fd;
              
              if ((fd = open (cmd_args->filename, O_CREAT, 0644)) < 0)
                {
                  fprintf (stderr,
                           "Cannot open '%s': %s\n",
                           cmd_args->filename,
                           strerror (errno));
                  exit (EXIT_FAILURE);
                }
              else
                {
                  /* ignore close error, don't care right now */
                  close (fd);

                  if (unlink (cmd_args->filename) < 0)
                    {
                      fprintf (stderr,
                               "Cannot remove '%s': %s\n",
                               cmd_args->filename,
                               strerror (errno));
                      exit (EXIT_FAILURE);
                    }
                }
            }
          break;
        case IPMI_CONFIG_ACTION_LIST_SECTIONS:
          /* do nothing - here to remove compile warning */
          break;
        }
    }
}


void
ipmi_config_argp_parse (int argc, char *argv[], struct ipmi_config_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_admin (&(cmd_args->common_args));

  cmd_args->action = 0;
  cmd_args->verbose_count = 0;
  cmd_args->filename = NULL;
  cmd_args->lan_channel_number = 0;
  cmd_args->lan_channel_number_set = 0;
  cmd_args->serial_channel_number = 0;
  cmd_args->serial_channel_number_set = 0;
  cmd_args->sol_channel_number = 0;
  cmd_args->sol_channel_number_set = 0;
  cmd_args->keypairs = NULL;
  cmd_args->section_strs = NULL;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _ipmi_config_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _ipmi_config_args_validate (cmd_args);
}
