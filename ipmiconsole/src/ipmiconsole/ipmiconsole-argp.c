/*****************************************************************************\
 *  $Id: ipmiconsole-argp.c,v 1.8 2008-05-17 00:51:20 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *  
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/param.h>
#include <assert.h>
#include <errno.h>

#include <ipmiconsole.h>        /* lib ipmiconsole.h */

#include "ipmiconsole_.h"       /* tool ipmiconsole.h */
#include "ipmiconsole-argp.h"
#include "tool-cmdline-common.h"
#include "conffile.h"
#include "error.h"
#include "secure.h"

#include "freeipmi-portability.h"
#include "tool-common.h"

#define IPMICONSOLE_CONFIG_FILE_DEFAULT "/etc/ipmiconsole.conf"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

static error_t parse_opt (int key, char *arg, struct argp_state *state);
static error_t parse_opt_conf (int key, char *arg, struct argp_state *state);

const char *argp_program_version =
"Ipmiconsole [ipmiconsole-" PACKAGE_VERSION "]\n"
"Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.\n"
"Copyright (C) 2006-2007 The Regents of the University of California.\n"
"This program is free software; you may redistribute it under the terms of\n"
"the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMIConsole - IPMI SOL Console Utility";

static char args_doc[] = "";

static struct argp_option options[] =
  {
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    /* legacy */
    {"cipher-suite-id", 'c', "CIPHER-SUITE-ID", OPTION_HIDDEN,                         
     "Specify the IPMI 2.0 cipher suite ID to use.", 14},
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"config-file", CONFIG_FILE_KEY, "FILE", 0,
     "Specify an alternate configuration file.", 30},
    {"escape-char", ESCAPE_CHAR_KEY, "CHAR", 0,
     "Specify an alternate escape character (default char '&')", 31},
    {"dont-steal", DONT_STEAL_KEY, 0, 0,
     "Do not steal an SOL session if one is already detected as being in use.", 32},
    {"deactivate", DEACTIVATE_KEY, 0, 0,
     "Deactivate a SOL session if one is detected as being in use and exit.", 33},
    {"lock-memory", LOCK_MEMORY_KEY, 0, 0,
     "Lock sensitive information (such as usernames and passwords) in memory.", 34},
#ifndef NDEBUG
    {"debugfile", DEBUGFILE_KEY, 0, 0,
     "Output debugging to the debugfile rather than to standard output.", 36},
    {"noraw", NORAW_KEY, 0, 0,
     "Don't enter terminal raw mode.", 37},
#endif
    { 0 }
  };

static struct argp argp = { options, parse_opt, args_doc, doc };

static struct argp argp_conf = { options, parse_opt_conf, args_doc, doc };

static error_t
parse_opt_conf (int key, char *arg, struct argp_state *state)
{
  struct ipmiconsole_arguments *cmd_args = state->input;

  switch (key)
    {
    case CONFIG_FILE_KEY:	/* --config-file */
      if (!(cmd_args->config_file = strdup(arg)))
        err_exit("strdup: %s", strerror(errno));
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      /* don't parse anything else, fall to return 0 */
      break;
    }

  return 0;
}

static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct ipmiconsole_arguments *cmd_args = state->input;
  error_t ret;

  switch (key)
    {
      /* 'I' is advertised option, 'c' is for backwards compatability */
    case 'c':	/* --cipher-suite-id */
      ret = common_parse_opt ('I', arg, state, &(cmd_args->common));
      return ret;
      break;
    case ESCAPE_CHAR_KEY:          /* --escape-char */
      cmd_args->escape_char = *arg;
      break;
    case DONT_STEAL_KEY:       /* --dont-steal */
      cmd_args->dont_steal++;
      break;
    case DEACTIVATE_KEY:       /* --deactivate */
      cmd_args->deactivate++;
      break;
    case LOCK_MEMORY_KEY:       /* --lock-memory */
      cmd_args->lock_memory++;
      break;
    case CONFIG_FILE_KEY:	/* --config-file */
      /* ignore */
      break;
#ifndef NDEBUG
    case DEBUGFILE_KEY:	/* --debugfile */
      cmd_args->debugfile++;
      break;
    case NORAW_KEY:	/* --noraw */
      cmd_args->noraw++;
      break;
#endif /* NDEBUG */
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      return ret;
    }

  return 0;
}

static int
_cb_hostname(conffile_t cf, 
	     struct conffile_data *data,
             char *optionname, 
	     int option_type,
	     void *option_ptr,
             int option_data,
	     void *app_ptr,
	     int app_data)
{
  struct ipmiconsole_arguments *cmd_args;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  if (strlen(data->string) > MAXHOSTNAMELEN)
    err_exit("Config File Error: hostname too long");

  if (!(cmd_args->common.hostname = strdup(data->string)))
    err_exit("strdup: %s", strerror(errno));

  return 0;
}

static int
_cb_username(conffile_t cf, 
	     struct conffile_data *data,
             char *optionname, 
	     int option_type,
	     void *option_ptr,
             int option_data,
	     void *app_ptr,
	     int app_data)
{
  struct ipmiconsole_arguments *cmd_args;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    err_exit("Config File Error: username too long");

  if (!(cmd_args->common.username = strdup(data->string)))
    err_exit("strdup: %s", strerror(errno));

  return 0;
}

static int
_cb_password(conffile_t cf, 
	     struct conffile_data *data,
             char *optionname,
	     int option_type,
	     void *option_ptr,
             int option_data, 
	     void *app_ptr,
	     int app_data)
{
  struct ipmiconsole_arguments *cmd_args;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  if (strlen(data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    err_exit("Config File Error: password too long");

  if (!(cmd_args->common.password = strdup(data->string)))
    err_exit("strdup: %s", strerror(errno));

  return 0;
}

static int
_cb_k_g(conffile_t cf, 
	struct conffile_data *data,
	char *optionname, 
	int option_type, 
	void *option_ptr,
	int option_data,
	void *app_ptr,
	int app_data)
{
  struct ipmiconsole_arguments *cmd_args;
  int rv;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  if ((rv = parse_kg(cmd_args->common.k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    err_exit("Config File Error: K_g invalid");

  if (rv > 0)
    cmd_args->common.k_g_len = rv;

  return 0;
}

static int
_cb_privilege_level(conffile_t cf, 
		    struct conffile_data *data,
		    char *optionname,
		    int option_type,
		    void *option_ptr,
		    int option_data, 
		    void *app_ptr,
		    int app_data)
{
  struct ipmiconsole_arguments *cmd_args;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  cmd_args->common.privilege_level = parse_privilege_level(data->string);

  return 0;
}

static int
_cb_cipher_suite_id(conffile_t cf, 
		    struct conffile_data *data,
                    char *optionname,
		    int option_type,
		    void *option_ptr,
                    int option_data, 
		    void *app_ptr, 
		    int app_data)
{
  struct ipmiconsole_arguments *cmd_args;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  cmd_args->common.cipher_suite_id = data->intval;
  if (cmd_args->common.cipher_suite_id < IPMI_CIPHER_SUITE_ID_MIN
      || cmd_args->common.cipher_suite_id > IPMI_CIPHER_SUITE_ID_MAX)
    err_exit("Config File Error: cipher suite id invalid\n");
  if (!IPMI_CIPHER_SUITE_ID_SUPPORTED (cmd_args->common.cipher_suite_id))
    err_exit("Config File Error: cipher suite id unsupported\n");
  return 0;
}

static int
_cb_workaround_flags(conffile_t cf, 
                     struct conffile_data *data,
                     char *optionname,
                     int option_type,
                     void *option_ptr,
                     int option_data, 
                     void *app_ptr, 
                     int app_data)
{
  struct ipmiconsole_arguments *cmd_args;
  int tmp;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  if ((tmp = parse_workaround_flags(data->string)) < 0)
    err_exit("Config File Error: invalid workaround flags\n");
  cmd_args->common.workaround_flags = tmp;
  return 0;
}

static int
_cb_escape_char(conffile_t cf, 
                struct conffile_data *data,
                char *optionname,
                int option_type,
                void *option_ptr,
                int option_data, 
                void *app_ptr, 
                int app_data)
{
  struct ipmiconsole_arguments *cmd_args;

  cmd_args = (struct ipmiconsole_arguments *)app_ptr;

  cmd_args->escape_char = data->string[0];
  return 0;
}

static int
_cb_bool(conffile_t cf, 
         struct conffile_data *data,
         char *optionname,
         int option_type,
         void *option_ptr,
         int option_data, 
         void *app_ptr, 
         int app_data)
{
  int *boolval = (int *)option_ptr;
  int cmdlineset = option_data;

  if (cmdlineset)
    return 0;
  
  *boolval = data->boolval;
  return 0;
}

static void
_config_file_parse(struct ipmiconsole_arguments *cmd_args)
{
  int hostname_flag,
    username_flag,
    password_flag, 
    k_g_flag,
    cipher_suite_id_flag,
    privilege_flag, 
    privilege_level_flag, 
    escape_char_flag,
    dont_steal_flag,
    lock_memory_flag,
    workaround_flags_flag;
  
  /* Notes:
   *
   * -T/--deactivate option is not useful for config files.  It is
   * excluded here.
   */

  struct conffile_option config_file_options[] =
    {
      {
        "hostname", 
        CONFFILE_OPTION_STRING, 
        -1, 
        _cb_hostname,
        1, 
        0, 
        &hostname_flag,
        NULL, 
        0
      },
      {
        "username", 
        CONFFILE_OPTION_STRING, 
        -1, 
        _cb_username,
        1, 
        0, 
        &username_flag,
        NULL, 
        0
      },
      {
        "password", 
        CONFFILE_OPTION_STRING, 
        -1, 
        _cb_password,
        1, 
        0, 
        &password_flag, 
        NULL, 
        0
      },
      {
        "k_g", 
        CONFFILE_OPTION_STRING, 
        -1, 
        _cb_k_g,
        1, 
        0, 
        &k_g_flag, 
        NULL, 
        0
      },
      /* privilege maintained for backwards compatability */
      {
        "privilege", 
        CONFFILE_OPTION_STRING, 
        -1,
        _cb_privilege_level,
        1, 
        0, 
        &privilege_flag,
        NULL, 
        0
      },
      {
        "cipher-suite-id", 
        CONFFILE_OPTION_INT, 
        -1, 
        _cb_cipher_suite_id,
        1,
        0, 
        &cipher_suite_id_flag,
        NULL, 
        0
      },
      {
        "privilege-level", 
        CONFFILE_OPTION_STRING, 
        -1,
        _cb_privilege_level,
        1, 
        0, 
        &privilege_level_flag,
        NULL, 
        0
      },
      {
        "escape-char",
        CONFFILE_OPTION_STRING,
        -1,
        _cb_escape_char,
        1,
        0,
        &escape_char_flag,
        NULL,
        0
      },
      {
        "dont-steal", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        _cb_bool,
        1, 
        0, 
        &dont_steal_flag, 
        &(cmd_args->dont_steal),
        0,
      },
      {
        "lock-memory", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        _cb_bool,
        1, 
        0, 
        &lock_memory_flag, 
        &(cmd_args->lock_memory),
        0,
      },
      {
        "workaround-flags",
        CONFFILE_OPTION_STRING, 
        -1, 
        _cb_workaround_flags,
        1, 
        0, 
        &workaround_flags_flag,
        NULL,
        0
      },
    };
  conffile_t cf = NULL;
  int num;

  if (!(cf = conffile_handle_create()))
    {
      err_exit("conffile_handle_create");
      goto cleanup;
    }

  num = sizeof(config_file_options)/sizeof(struct conffile_option);
  if (conffile_parse(cf, cmd_args->config_file, config_file_options, num, cmd_args, 0, 0) < 0)
    {
      char buf[CONFFILE_MAX_ERRMSGLEN];

      /* Its not an error if the default configuration file doesn't exist */
      if (!strcmp(cmd_args->config_file, IPMICONSOLE_CONFIG_FILE_DEFAULT)
          && conffile_errnum(cf) == CONFFILE_ERR_EXIST)
	goto cleanup;

      if (conffile_errmsg(cf, buf, CONFFILE_MAX_ERRMSGLEN) < 0)
        err_exit("conffile_parse: %d", conffile_errnum(cf));
      else
        err_exit("conffile_parse: %s", buf);
    }

 cleanup:
  if (cf)
    conffile_handle_destroy(cf);
}

void
_ipmiconsole_args_validate (struct ipmiconsole_arguments *cmd_args)
{
  if (!cmd_args->common.hostname)
    err_exit("hostname input required");
}

void
ipmiconsole_argp_parse (int argc, char **argv, struct ipmiconsole_arguments *cmd_args)
{
  init_common_cmd_args_admin (&(cmd_args->common));
  cmd_args->escape_char = '&';
  cmd_args->dont_steal = 0;
  cmd_args->deactivate = 0;
  cmd_args->lock_memory = 0;
  cmd_args->config_file = IPMICONSOLE_CONFIG_FILE_DEFAULT;
#ifndef NDEBUG
  cmd_args->debugfile = 0;
  cmd_args->noraw = 0;
#endif /* NDEBUG */

  /* special case to ipmiconsole, different timeout defaults */
  cmd_args->common.session_timeout = 60000;
  cmd_args->common.retransmission_timeout = 500;

  argp_parse (&argp_conf, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  /* change defaults to whatever is configured, run 2nd b/c
   * user may have specified config file on the command line.
   */
  _config_file_parse(cmd_args);
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  _ipmiconsole_args_validate (cmd_args);
}

