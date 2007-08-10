/*****************************************************************************\
 *  $Id: ipmiconsole_config.c,v 1.20 2007-08-10 16:22:17 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
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
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
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
#include <assert.h>
#include <errno.h>

#include <argp.h>

#include <ipmiconsole.h>

#include "ipmiconsole_config.h"
#include "cmdline-parse-common.h"
#include "conffile.h"
#include "error.h"
#include "secure.h"

#include "freeipmi-portability.h"
#include "tool-common.h"

#define IPMICONSOLE_CONFIG_FILE_KEY 'C'
#define IPMICONSOLE_DONT_STEAL_KEY  'N'
#define IPMICONSOLE_DEACTIVATE_KEY  'T'
#define IPMICONSOLE_LOCK_MEMORY_KEY 'L'

#define IPMICONSOLE_DEBUG_KEY       160
#define IPMICONSOLE_DEBUGFILE_KEY   161
#define IPMICONSOLE_NORAW_KEY       162

const char *argp_program_version = "ipmiconsole " VERSION "\n";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_OUTOFBAND_NO_TIMEOUT,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    {"config-file", IPMICONSOLE_CONFIG_FILE_KEY, "FILE", 0,
     "Specify an alternate configuration file.", 24},
    {"dont-steal", IPMICONSOLE_DONT_STEAL_KEY, 0, 0,
     "Do not steal an SOL session if one is already detected as being in use.", 25},
    {"deactivate", IPMICONSOLE_DEACTIVATE_KEY, 0, 0,
     "Deactivate a SOL session if one is detected as being in use and exit.", 26},
    {"lock-memory", IPMICONSOLE_LOCK_MEMORY_KEY, 0, 0,
     "Lock sensitive information (such as usernames and passwords) in memory.", 27},
    {"debug", IPMICONSOLE_DEBUG_KEY, 0, 0,
     "Turn on debugging.", 28},
#ifndef NDEBUG
    {"debugfile", IPMICONSOLE_DEBUGFILE_KEY, 0, 0,
     "Output debugging to the debugfile rather than to standard output.", 29},
    {"noraw", IPMICONSOLE_NORAW_KEY, 0, 0,
     "Don't enter terminal raw mode.", 31},
#endif
    { 0 }
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static char cmdline_args_doc[] = "";

static char cmdline_doc[] = "IPMIConsole - IPMI Serial-over-LAN (SOL) Utility";

static struct argp cmdline_argp = {cmdline_options, 
                                   cmdline_parse, 
                                   cmdline_args_doc, 
                                   cmdline_doc};

extern struct ipmiconsole_config *conf;

static void
_config_default(void)
{
  assert(conf);

  memset(conf, '\0', sizeof(struct ipmiconsole_config));

  conf->debug = 0;
#ifndef NDEBUG
  conf->debugfile = 0;
  conf->noraw = 0;
#endif /* NDEBUG */
  conf->config_file = IPMICONSOLE_CONFIG_FILE_DEFAULT;

  conf->privilege = -1;
  conf->cipher_suite_id = -1;

  memset(conf->k_g, '\0', IPMI_MAX_K_G_LENGTH + 1);
  conf->k_g_len = 0;
}

static error_t
cmdline_parse (int key,
               char *arg,
               struct argp_state *state)
{ 
  char *pw;
  char *kg;
  char *ptr;
  int rv;
  int tmp;

  assert(conf);

  switch(key)
    {
    case ARGP_HOSTNAME_KEY:       /* --hostname */
      if (strlen(arg) > MAXHOSTNAMELEN)
        err_exit("Command Line Error: hostname too long");
      strcpy(conf->hostname, arg);
      conf->hostname_set_on_cmdline++;
      break;
    case ARGP_USERNAME_KEY:       /* --username */
      if (strlen(arg) > IPMI_MAX_USER_NAME_LENGTH)
        err_exit("Command Line Error: username too long");
      strcpy(conf->username, arg);
      conf->username_set_on_cmdline++;
      if (arg)
        {
          int n;
          n = strlen(arg);
          secure_memset(arg, '\0', n);
        }
      break;
    case ARGP_PASSWORD_KEY:       /* --password */
      if (strlen(arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        err_exit("Command Line Error: password too long");
      strcpy(conf->password, arg);
      conf->password_set_on_cmdline++;
      if (arg)
        {
          int n;
          n = strlen(arg);
          secure_memset(arg, '\0', n);
        }
      break;
    case ARGP_PASSWORD_PROMPT_KEY:       /* --password-prompt */
      if (!(pw = getpass("Password: ")))
            err_exit("getpass: %s", strerror(errno));
      if (strlen(pw) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        err_exit("password too long");
      strcpy(conf->password, pw);
      conf->password_set_on_cmdline++;
      break;
    case ARGP_K_G_KEY:       /* --k-g */
      if ((rv = check_kg_len(arg)) < 0)
        err_exit("Command Line Error: k_g too long");
      if ((rv = parse_kg(conf->k_g, IPMI_MAX_K_G_LENGTH + 1, arg)) < 0)
        err_exit("Command Line Error: k_g input formatted incorrectly");
      if (rv > 0)
        {
          conf->k_g_len = rv;
          conf->k_g_set_on_cmdline++;
        }
      if (arg)
        {
          int n;
          n = strlen(arg);
          secure_memset(arg, '\0', n);
        }
      break;
    case ARGP_K_G_PROMPT_KEY:       /* --k-g-prompt */
      if (!(kg = getpass("K_g: ")))
        err_exit("getpass: %s", strerror(errno));
      if ((rv = check_kg_len(kg)) < 0)
        err_exit("Command Line Error: k_g too long");
      if ((rv = parse_kg(conf->k_g, IPMI_MAX_K_G_LENGTH + 1, kg)) < 0)
        err_exit("Command Line Error: k_g input formatted incorrectly");
      if (rv > 0)
        {
          conf->k_g_len = rv;
          conf->k_g_set_on_cmdline++;
        }
      break;
    case ARGP_PRIVILEGE_LEVEL_KEY:	/* --privilege-level */
      tmp = parse_privilege_level(arg);
      if (tmp == IPMI_PRIVILEGE_LEVEL_USER)
        conf->privilege = IPMICONSOLE_PRIVILEGE_USER;
      else if (tmp == IPMI_PRIVILEGE_LEVEL_OPERATOR)
        conf->privilege = IPMICONSOLE_PRIVILEGE_OPERATOR;
      else if (tmp == IPMI_PRIVILEGE_LEVEL_ADMIN)
        conf->privilege = IPMICONSOLE_PRIVILEGE_ADMIN;
      else
        err_exit("Command Line Error: Invalid privilege level");
      conf->privilege_set_on_cmdline++;
      break;
      /* 'I' is advertised option, 'c' is for backwards compatability */
    case 'c':	/* --cipher-suite-id */
    case ARGP_CIPHER_SUITE_ID_KEY:	/* --cipher-suite-id */
      conf->cipher_suite_id = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: cipher suite id invalid\n");
      if (conf->cipher_suite_id < IPMI_CIPHER_SUITE_ID_MIN
          || conf->cipher_suite_id > IPMI_CIPHER_SUITE_ID_MAX)
        err_exit("Command Line Error: cipher suite id invalid\n");
      if (!IPMI_CIPHER_SUITE_ID_SUPPORTED (conf->cipher_suite_id))
        err_exit("Command Line Error: cipher suite id unsupported\n");
      conf->cipher_suite_id_set_on_cmdline++;
      break;
    case IPMICONSOLE_CONFIG_FILE_KEY:	/* --config-file */
      if (!(conf->config_file = strdup(arg)))
        err_exit("strdup: %s", strerror(errno));
      break;
    case IPMICONSOLE_DONT_STEAL_KEY:       /* --dont-steal */
      conf->dont_steal++;
      conf->dont_steal_set_on_cmdline++;
      break;
    case IPMICONSOLE_DEACTIVATE_KEY:       /* --deactivate */
      conf->deactivate++;
      conf->deactivate_set_on_cmdline++;
      break;
    case IPMICONSOLE_LOCK_MEMORY_KEY:       /* --lock-memory */
      conf->lock_memory++;
      conf->lock_memory_set_on_cmdline++;
      break;
    case ARGP_WORKAROUND_FLAGS_KEY:
      if ((tmp = parse_outofband_2_0_workaround_flags(arg)) < 0)
        err_exit("Command Line Error: invalid workaround flags\n");
      /* convert to ipmiconsole flags */
      if (tmp & IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
        conf->workaround_flags |= IPMICONSOLE_WORKAROUND_INTEL_2_0;
      if (tmp & IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
        conf->workaround_flags |= IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0;
      if (tmp & IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUN_2_0_SESSION)
        conf->workaround_flags |= IPMICONSOLE_WORKAROUND_SUN_2_0;
      conf->workaround_flags_set_on_cmdline++;
      break;
    case IPMICONSOLE_DEBUG_KEY:	/* --debug */
      conf->debug++;
      break;
#ifndef NDEBUG
    case IPMICONSOLE_DEBUGFILE_KEY:	/* --debugfile */
      conf->debugfile++;
      break;
    case IPMICONSOLE_NORAW_KEY:	/* --noraw */
      conf->noraw++;
      break;
#endif /* NDEBUG */
    default:
      return ARGP_ERR_UNKNOWN;
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
  if (conf->hostname_set_on_cmdline)
    return 0;

  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    err_exit("Config File Error: hostname too long");

  strcpy(conf->hostname, data->string);
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
  if (conf->username_set_on_cmdline)
    return 0;

  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    err_exit("Config File Error: username too long");

  strcpy(conf->username, data->string);
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
  if (conf->password_set_on_cmdline)
    return 0;

  if (strlen(data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    err_exit("Config File Error: password too long");

  strcpy(conf->password, data->string);
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
  int rv;

  if (conf->k_g_set_on_cmdline)
    return 0;

  if ((rv = parse_kg(conf->k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    err_exit("Config File Error: K_g invalid");
  if (rv > 0)
    conf->k_g_len = rv;

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
  int tmp;

  if (conf->privilege_set_on_cmdline)
    return 0;
  
  tmp = parse_privilege_level(data->string);
  if (tmp == IPMI_PRIVILEGE_LEVEL_USER)
    conf->privilege = IPMICONSOLE_PRIVILEGE_USER;
  else if (tmp == IPMI_PRIVILEGE_LEVEL_OPERATOR)
    conf->privilege = IPMICONSOLE_PRIVILEGE_OPERATOR;
  else if (tmp == IPMI_PRIVILEGE_LEVEL_ADMIN)
    conf->privilege = IPMICONSOLE_PRIVILEGE_ADMIN;
  else
    err_exit("Config File Error: Invalid privilege level");

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
  if (conf->cipher_suite_id_set_on_cmdline)
    return 0;

  conf->cipher_suite_id = data->intval;
  if (conf->cipher_suite_id < IPMI_CIPHER_SUITE_ID_MIN
      || conf->cipher_suite_id > IPMI_CIPHER_SUITE_ID_MAX)
    err_exit("Config File Error: cipher suite id invalid\n");
  if (!IPMI_CIPHER_SUITE_ID_SUPPORTED (conf->cipher_suite_id))
    err_exit("Config File Error: cipher suite id unsupported\n");
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
  int tmp;

  if (conf->workaround_flags_set_on_cmdline)
    return 0;

  if ((tmp = parse_outofband_2_0_workaround_flags(data->string)) < 0)
    err_exit("Config File Error: invalid workaround flags\n");
  /* convert to ipmiconsole flags */
  if (tmp & IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_INTEL_2_0_SESSION)
    conf->workaround_flags |= IPMICONSOLE_WORKAROUND_INTEL_2_0;
  else if (tmp & IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION)
    conf->workaround_flags |= IPMICONSOLE_WORKAROUND_SUPERMICRO_2_0;
  else if (tmp & IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUN_2_0_SESSION)
    conf->workaround_flags |= IPMICONSOLE_WORKAROUND_SUN_2_0;
  return 0;
}

static void
_config_file_parse(void)
{
  int hostname_flag,
    username_flag,
    password_flag, 
    k_g_flag,
    privilege_flag, 
    privilege_level_flag, 
    cipher_suite_id_flag,
    dont_steal_flag,
    lock_memory_flag,
    workaround_flags_flag;
  
  /* Notes:
   *
   * -T/--deactivate option is not useful for config fils.  It is
   * excluded here.
   */

  struct conffile_option options[] =
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
        "dont-steal", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        _cb_bool,
        1, 
        0, 
        &dont_steal_flag, 
        &(conf->dont_steal),
        conf->dont_steal_set_on_cmdline
      },
      {
        "lock-memory", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        _cb_bool,
        1, 
        0, 
        &lock_memory_flag, 
        &(conf->lock_memory),
        conf->lock_memory_set_on_cmdline
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

  num = sizeof(options)/sizeof(struct conffile_option);
  if (conffile_parse(cf, conf->config_file, options, num, NULL, 0, 0) < 0)
    {
      char buf[CONFFILE_MAX_ERRMSGLEN];

      /* Its not an error if the default configuration file doesn't exist */
      if (!strcmp(conf->config_file, IPMICONSOLE_CONFIG_FILE_DEFAULT)
          && conffile_errnum(cf) == CONFFILE_ERR_EXIST)
	goto cleanup;

      if (conffile_errmsg(cf, buf, CONFFILE_MAX_ERRMSGLEN) < 0)
        err_exit("conffile_parse: %d", conffile_errnum(cf));
      else
        err_exit("conffile_parse: %s", buf);
    }

 cleanup:
  conffile_handle_destroy(cf);
}

void
ipmiconsole_config_setup(int argc, char **argv)
{
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */

  assert(argv);

  _config_default();
  argp_parse(&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, NULL);
  _config_file_parse();

  if (!strlen(conf->hostname))
    err_exit("hostname input required");
  
#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1).  This hack
   * does not work on all operating systems.  Start at index 1, since
   * we don't need to clear out the actually command invocation.
   */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
}
