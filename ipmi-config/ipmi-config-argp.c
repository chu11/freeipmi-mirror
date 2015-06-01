/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-config - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2014 FreeIPMI Core Team\n"
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
  { "category", IPMI_CONFIG_ARGP_CATEGORY_KEY, "CATEGORY", 0,
    "Specify category (categories) to configure.  Defaults to 'core'.", 40},
  { "checkout", IPMI_CONFIG_ARGP_CHECKOUT_KEY, 0, 0,
    "Fetch configuration information.", 41},
  { "commit", IPMI_CONFIG_ARGP_COMMIT_KEY, 0, 0,
    "Update configuration information from a config file or key pairs.", 42},
  { "diff", IPMI_CONFIG_ARGP_DIFF_KEY, 0, 0,
    "Show differences between stored information and a config file or key pairs.", 43},
  { "filename", IPMI_CONFIG_ARGP_FILENAME_KEY, "FILENAME", 0,
    "Specify a config file for checkout/commit/diff.", 44},
  { "key-pair", IPMI_CONFIG_ARGP_KEYPAIR_KEY, "KEY-PAIR", 0,
    "Specify KEY=VALUE pairs for checkout/commit/diff.", 45},
  { "section", IPMI_CONFIG_ARGP_SECTIONS_KEY, "SECTION", 0,
    "Specify a SECTION for checkout.", 46},
  { "listsections", IPMI_CONFIG_ARGP_LIST_SECTIONS_KEY, 0, 0,
    "List available sections for checkout.", 47},
  { "verbose", IPMI_CONFIG_ARGP_VERBOSE_KEY, 0, 0,
    "Print additional detailed information.", 48},
  { "lan-channel-number", IPMI_CONFIG_ARGP_LAN_CHANNEL_NUMBER_KEY, "NUMBER", 0,
    "Use a specific LAN Channel Number.", 49},
  { "serial-channel-number", IPMI_CONFIG_ARGP_SERIAL_CHANNEL_NUMBER_KEY, "NUMBER", 0,
    "Use a specific Serial Channel Number.", 50},
  { "sol-channel-number", IPMI_CONFIG_ARGP_SOL_CHANNEL_NUMBER_KEY, "NUMBER", 0,
    "Use a specific SOL Channel Number.", 51},
  { "foobar", IPMI_CONFIG_ARGP_FILENAME_KEY_LEGACY, "FILENAME", OPTION_HIDDEN,
    "Specify a config file for checkout/commit/diff.", 52},
  { "info", IPMI_CONFIG_ARGP_PEF_INFO_KEY, 0, OPTION_HIDDEN,
    "Show general information about PEF configuration.", 53},
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

static int
_ipmi_config_category (char *arg, unsigned int *category_mask)
{
  char *argtmp;
  char *tok;
  int rv = -1;

  assert (arg);
  assert (category_mask);

  (*category_mask) = 0;

  if (!(argtmp = strdup (arg)))
    {
      perror (arg);
      return (-1);
    }

  tok = strtok (argtmp, " ,");
  while (tok)
    {
      if (!strcasecmp (tok, "core")
	  || !strcasecmp (tok, "bmc"))
	(*category_mask) |= IPMI_CONFIG_CATEGORY_MASK_CORE;
      else if (!strcasecmp (tok, "chassis"))
	(*category_mask) |= IPMI_CONFIG_CATEGORY_MASK_CHASSIS;
      else if (!strcasecmp (tok, "sensors")
	       || !strcasecmp (tok, "sensor")) /* handle common typo */
	(*category_mask) |= IPMI_CONFIG_CATEGORY_MASK_SENSORS;
      else if (!strcasecmp (tok, "pef"))
	(*category_mask) |= IPMI_CONFIG_CATEGORY_MASK_PEF;
      else if (!strcasecmp (tok, "dcmi"))
	(*category_mask) |= IPMI_CONFIG_CATEGORY_MASK_DCMI;
      else
	{
	  fprintf (stderr, "invalid category '%s' specified\n", tok);
	  goto cleanup;
	}

      tok = strtok (NULL, " ,");
    }

  rv = 0;
 cleanup:
  free (argtmp);
  return (0);
}

static int
_ipmi_config_parse_channel_number (char *arg,
                                   uint8_t *channel_number,
                                   int *channel_number_set)
{
  char *endptr;
  int tmp;

  assert (arg);
  assert (channel_number);
  assert (channel_number_set);

  errno = 0;
  tmp = strtol (arg, &endptr, 0);
  if (errno
      || endptr[0] != '\0')
    {
      fprintf (stderr, "invalid channel number\n");
      return (-1);
    }
  if (!IPMI_CHANNEL_NUMBER_VALID (tmp))
    {
      fprintf (stderr, "invalid channel number\n");
      return (-1);
    }
  (*channel_number) = (uint8_t)tmp;
  (*channel_number_set)++;
  return (0);
}

static int
_ipmi_config_keypair_parse_string (const char *str,
                                   char **section_name,
                                   char **key_name,
                                   char **value)
{
  char *str_temp = NULL;
  char *section_name_tok = NULL;
  char *key_name_tok = NULL;
  char *value_tok = NULL;
  char *ptr;
  char *buf;
  int rv = -1;

  assert (str);
  assert (section_name);
  assert (key_name);
  assert (value);

  *section_name = NULL;
  *key_name = NULL;
  *value = NULL;

  if (!(str_temp = strdup (str)))
    {
      perror ("strdup");
      goto cleanup;
    }

  section_name_tok = strtok_r (str_temp, ":", &buf);
  key_name_tok = strtok_r (NULL, "=", &buf);
  value_tok = strtok_r (NULL, "\0", &buf);

  if (!(section_name_tok && key_name_tok))
    {
      fprintf (stderr,
               "Improperly input keypair '%s'\n",
               str);
      goto cleanup;
    }

  /* get rid of spaces stuck in the string */
  if (section_name_tok)
    section_name_tok = strtok_r (section_name_tok, " \t", &buf);
  if (key_name_tok)
    key_name_tok = strtok_r (key_name_tok, " \t", &buf);
  if (value_tok)
    value_tok = strtok_r (value_tok, " \t", &buf);

  if (section_name_tok)
    {
      if (!(ptr = strdup (section_name_tok)))
        {
          perror ("strdup");
          goto cleanup;
        }
      *section_name = ptr;
    }
  if (key_name_tok)
    {
      if (!(ptr = strdup (key_name_tok)))
        {
          perror ("strdup");
          goto cleanup;
        }
      *key_name = ptr;
    }
  if (value_tok)
    {
      if (!(ptr = strdup (value_tok)))
        {
          perror ("strdup");
          goto cleanup;
        }
      *value = ptr;
    }
  else
    {
      /* values can be empty strings */
      if (!(ptr = strdup ("")))
        {
          perror ("strdup");
          goto cleanup;
        }
      *value = ptr;
    }

  rv = 0;
 cleanup:
  free (str_temp);
  if (rv < 0)
    {
      free (*section_name);
      *section_name = NULL;
      free (*key_name);
      *key_name = NULL;
      free (*value);
      *value = NULL;
    }
  return (rv);
}

static int
_ipmi_config_keypair_append (struct ipmi_config_keypair **keypairs,
                             struct ipmi_config_keypair *keypair)
{
  assert (keypairs);
  assert (keypair);

  if (*keypairs)
    {
      struct ipmi_config_keypair *kp;

      kp = *keypairs;
      while (kp)
        {
          if (!strcasecmp (kp->section_name, keypair->section_name)
              && !strcasecmp (kp->key_name, keypair->key_name))
            {
              fprintf (stderr,
                       "Duplicate section:key pair '%s:%s' specified\n",
                       kp->section_name, kp->key_name);
              return (-1);
            }
          kp = kp->next;
        }

      kp = *keypairs;
      while (kp->next)
        kp = kp->next;
      kp->next = keypair;
    }
  else
    *keypairs = keypair;

  return (0);
}

static struct ipmi_config_keypair *
_ipmi_config_keypair_create (const char *section_name,
                             const char *key_name,
                             const char *value_input)
{
  struct ipmi_config_keypair *keypair = NULL;

  assert (section_name);
  assert (key_name);

  if (!(keypair = (struct ipmi_config_keypair *)malloc (sizeof (struct ipmi_config_keypair))))
    {
      perror ("malloc");
      goto cleanup;
    }
  memset (keypair, '\0', sizeof (struct ipmi_config_keypair));

  if (!(keypair->section_name = strdup (section_name)))
    {
      perror ("strdup");
      goto cleanup;
    }

  if (!(keypair->key_name = strdup (key_name)))
    {
      perror ("strdup");
      goto cleanup;
    }

  if (value_input)
    {
      if (!(keypair->value_input = strdup (value_input)))
        {
          perror ("strdup");
          goto cleanup;
        }
    }

  return (keypair);

 cleanup:
  if (keypair)
    {
      free (keypair->section_name);
      free (keypair->key_name);
      free (keypair->value_input);
      free (keypair);
    }
  return (NULL);
}

static struct ipmi_config_section_str *
_ipmi_config_section_str_create (const char *section_name)
{
  struct ipmi_config_section_str *sstr = NULL;

  if (!(sstr = (struct ipmi_config_section_str *)malloc (sizeof (struct ipmi_config_section_str))))
    {
      perror ("malloc");
      goto cleanup;
    }

  if (!(sstr->section_name = strdup (section_name)))
    {
      perror ("strdup");
      goto cleanup;
    }
  sstr->next = NULL;

  return (sstr);

 cleanup:
  if (sstr)
    {
      free (sstr->section_name);
      free (sstr);
    }
  return (NULL);
}

static int
_ipmi_config_section_str_append (struct ipmi_config_section_str **section_strs,
                                 struct ipmi_config_section_str *section_str)
{
  assert (section_strs);
  assert (section_str);

  if (*section_strs)
    {
      struct ipmi_config_section_str *sstr;

      sstr = *section_strs;
      while (sstr)
        {
          if (!strcasecmp (sstr->section_name, section_str->section_name))
            {
              fprintf (stderr,
                       "Duplicate section '%s' specified\n",
                       sstr->section_name);
              return (-1);
            }
          sstr = sstr->next;
        }

      sstr = *section_strs;
      while (sstr->next)
        sstr = sstr->next;
      sstr->next = section_str;
    }
  else
    *section_strs = section_str;

  return (0);
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
    case IPMI_CONFIG_ARGP_CATEGORY_KEY:
      if (_ipmi_config_category (arg, &(cmd_args->category_mask)) < 0)
        exit (EXIT_FAILURE);
      break;
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
      if (_ipmi_config_keypair_parse_string (arg,
                                             &section_name,
                                             &key_name,
                                             &value) < 0)
        exit (EXIT_FAILURE);

      if (!(kp = _ipmi_config_keypair_create (section_name,
                                              key_name,
                                              value)))
        exit (EXIT_FAILURE);

      if (_ipmi_config_keypair_append (&(cmd_args->keypairs),
                                       kp) < 0)
        exit (EXIT_FAILURE);

      free (section_name);
      free (key_name);
      free (value);
      break;
    case IPMI_CONFIG_ARGP_SECTIONS_KEY:
      if (!(sstr = _ipmi_config_section_str_create (arg)))
        exit (EXIT_FAILURE);

      if (_ipmi_config_section_str_append (&(cmd_args->section_strs),
                                           sstr) < 0)
        exit (EXIT_FAILURE);

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
      if (_ipmi_config_parse_channel_number (arg,
                                             &(cmd_args->lan_channel_number),
                                             &(cmd_args->lan_channel_number_set)) < 0)
        exit (EXIT_FAILURE);
      break;
    case IPMI_CONFIG_ARGP_SERIAL_CHANNEL_NUMBER_KEY:
      if (_ipmi_config_parse_channel_number (arg,
                                             &(cmd_args->serial_channel_number),
                                             &(cmd_args->serial_channel_number_set)) < 0)
        exit (EXIT_FAILURE);
      break;
    case IPMI_CONFIG_ARGP_SOL_CHANNEL_NUMBER_KEY:
      if (_ipmi_config_parse_channel_number (arg,
                                             &(cmd_args->sol_channel_number),
                                             &(cmd_args->sol_channel_number_set)) < 0)
        exit (EXIT_FAILURE);
      break;
    case IPMI_CONFIG_ARGP_PEF_INFO_KEY: /* legacy */
      cmd_args->info++;
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

  if ((!cmd_args->action || cmd_args->action == -1)
      && !cmd_args->info)
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

  cmd_args->category_mask = IPMI_CONFIG_CATEGORY_MASK_CORE;
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

  cmd_args->info = 0;

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
