/*****************************************************************************\
 *  $Id: ipmiconsole_config.c,v 1.8 2007-03-28 22:10:32 chu11 Exp $
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

#include <ipmiconsole.h>

#include "ipmiconsole_config.h"
#include "conffile.h"
#include "error.h"
#include "secure.h"

extern struct ipmiconsole_config *conf;

static void
_config_default(void)
{
  assert(conf);

  memset(conf, '\0', sizeof(struct ipmiconsole_config));

#ifndef NDEBUG
  conf->debug = 0;
  conf->debugfile = 0;
  conf->debugdump = 0;
  conf->noraw = 0;
#endif /* NDEBUG */
  conf->config_file = IPMICONSOLE_CONFIG_FILE_DEFAULT;

  conf->privilege = -1;
  conf->cipher_suite_id = -1;
}

static void
_usage(void)
{
  fprintf(stderr, "Usage: cerebrod [OPTIONS]\n"
          "-H --help                     Output Help\n"
          "-V --version                  Output Version\n"
	  "-h --hostname str             Hostname\n"
          "-u --username name            Username\n"
          "-p --password pw              Password\n"
          "-P --password-prompt          Prompt for Password\n"
          "-k --k-g str                  K_g Key\n"
          "-K --k-g-prompt               Prompt for K_g Key\n"
	  "-l --privilege str            Privilege\n"
	  "-c --cipher-suite-id num      Cipher Suite Privilege\n"
          "-C --config                   Select alternate config file\n"
          "-N --dont-steal               Do not steal in use SOL sessions by default\n"
          "-L --lock-memory              Lock memory\n"
          "-I --intel-2-0-session        Workaround Intel IPMI bugs\n"
          "-S --supermicro-2-0-session   Workaround Supermicro IPMI bugs\n");
#ifndef NDEBUG
  fprintf(stderr,
          "-D --debug                    Turn on debugging\n"
	  "-E --debugfile                Output debugging to debugfile\n"
	  "-F --debugdump                Turn on packet dumps\n"
	  "-G --noraw                    Don't enter raw mode\n");
#endif /* NDEBUG */
  exit(0);
}

static void
_version(void)
{
  fprintf(stderr, "ipmiconsole %s\n", VERSION);
  exit(0);
}

static void
_cmdline_parse(int argc, char **argv)
{ 
  char options[100];
  char *pw;
  char *kg;
  char *ptr;
  int c;

#if HAVE_GETOPT_LONG
  struct option long_options[] =
    {
      {"help",                     0, NULL, 'H'},
      {"version",                  0, NULL, 'V'},
      {"hostname",                 1, NULL, 'h'},
      {"username",                 1, NULL, 'u'},
      {"password",                 1, NULL, 'p'},
      {"password-prompt",          1, NULL, 'P'},
      {"k-g",                      1, NULL, 'k'},
      {"k-g-prompt",               1, NULL, 'K'},
      {"privilege",                1, NULL, 'l'},
      {"cipher-suite-id",          1, NULL, 'c'},
      {"config-file",              1, NULL, 'C'}, 
      {"dont-steal",               0, NULL, 'N'},
      {"lock-memory",              0, NULL, 'L'},
      {"intel-2-0-session",        0, NULL, 'I'},
      {"supermicro-2-0-session",   0, NULL, 'S'},
#ifndef NDEBUG
      {"debug",               0, NULL, 'D'},
      {"debugfile",           0, NULL, 'E'},
      {"debugdump",           0, NULL, 'F'},
      {"noraw",               0, NULL, 'G'},
#endif /* NDEBUG */
      {0, 0, 0, 0}
    };
#endif /* HAVE_GETOPT_LONG */

  assert(argv);
  assert(conf);

  memset(options, '\0', sizeof(options));
  strcat(options, "HVh:u:p:Pk:Kl:c:C:NLIS");
#ifndef NDEBUG
  strcat(options, "DEFG");
#endif /* NDEBUG */

  /* turn off output messages */
  opterr = 0;

#if HAVE_GETOPT_LONG
  while ((c = getopt_long(argc, argv, options, long_options, NULL)) != -1)
#else
  while ((c = getopt(argc, argv, options)) != -1)
#endif
    {
      switch (c)
        {
        case 'H':	/* --help */
          _usage();
          break;
        case 'V':
          _version();	/* --version */
          break;
        case 'h':       /* --hostname */
          if (strlen(optarg) > MAXHOSTNAMELEN)
            err_exit("Command Line Error: hostname too long");
          strcpy(conf->hostname, optarg);
          conf->hostname_set++;
          break;
        case 'u':       /* --username */
          if (strlen(optarg) > IPMI_MAX_USER_NAME_LENGTH)
            err_exit("Command Line Error: username too long");
          strcpy(conf->username, optarg);
          conf->username_set++;
          if (optarg)
            {
              int n;
              n = strlen(optarg);
              secure_memset(optarg, '\0', n);
            }
          break;
        case 'p':       /* --password */
          if (strlen(optarg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
            err_exit("Command Line Error: password too long");
          strcpy(conf->password, optarg);
          conf->password_set++;
          if (optarg)
            {
              int n;
              n = strlen(optarg);
              secure_memset(optarg, '\0', n);
            }
          break;
        case 'P':       /* --password-prompt */
          if (!(pw = getpass("Password: ")))
            err_exit("getpass: %s", strerror(errno));
          if (strlen(pw) > IPMI_2_0_MAX_PASSWORD_LENGTH)
            err_exit("password too long");
          strcpy(conf->password, pw);
          conf->password_set++;
          break;
        case 'k':       /* --k-g */
          if (strlen(optarg) > IPMI_MAX_K_G_LENGTH)
            err_exit("Command Line Error: K_g too long");
          strcpy(conf->k_g, optarg);
          conf->k_g_set++;
          if (optarg)
            {
              int n;
              n = strlen(optarg);
              secure_memset(optarg, '\0', n);
            }
          break;
        case 'K':       /* --k-g-prompt */
          if (!(kg = getpass("K_g: ")))
            err_exit("getpass: %s", strerror(errno));
          if (strlen(kg) > IPMI_MAX_K_G_LENGTH)
            err_exit("K_g too long");
          strcpy(conf->k_g, kg);
          conf->k_g_set++;
          break;
	case 'l':	/* --privilege */
	  if (!strcasecmp(optarg, "user"))
	    conf->privilege = IPMICONSOLE_PRIVILEGE_USER;
	  else if (!strcasecmp(optarg, "operator"))
	    conf->privilege = IPMICONSOLE_PRIVILEGE_OPERATOR;
	  else if (!strcasecmp(optarg, "admin")
		   || !strcasecmp(optarg, "administrator"))
	    conf->privilege = IPMICONSOLE_PRIVILEGE_ADMIN;
	  else
	    err_exit("Command Line Error: Invalid privilege level");
	  conf->privilege_set++;
	  break;
	case 'c':	/* --cipher-suite-id */
          conf->cipher_suite_id = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: cipher suite id invalid\n");
	  if (conf->cipher_suite_id < IPMI_CIPHER_SUITE_ID_MIN
	      || conf->cipher_suite_id > IPMI_CIPHER_SUITE_ID_MAX)
            err_exit("Command Line Error: cipher suite id invalid\n");
          conf->cipher_suite_id_set++;
	  break;
	case 'C':	/* --config-file */
	  if (!(conf->config_file = strdup(optarg)))
	    err_exit("strdup: %s", strerror(errno));
	  break;
        case 'N':       /* --dont-steal */
          conf->dont_steal++;
          conf->dont_steal_set++;
          break;
        case 'L':       /* --lock-memory */
          conf->lock_memory++;
          conf->lock_memory_set++;
          break;
        case 'I':       /* --intel-2-0-session */
          conf->intel_2_0_session++;
          conf->intel_2_0_session_set++;
          break;
        case 'S':       /* --supermicro-2-0-session */
          conf->supermicro_2_0_session++;
          conf->supermicro_2_0_session_set++;
          break;
#ifndef NDEBUG
        case 'D':	/* --debug */
          conf->debug++;
          break;
        case 'E':	/* --debugfile */
          conf->debugfile++;
          break;
        case 'F':	/* --debugdump */
          conf->debugdump++;
          break;
	case 'G':	/* --noraw */
	  conf->noraw++;
	  break;
#endif /* NDEBUG */
        case '?':
        default:
          err_exit("unknown command line option '%c'", c);
        }          
    }
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
  if (conf->hostname_set)
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
  if (conf->username_set)
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
  if (conf->password_set)
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
  if (conf->k_g_set)
    return 0;

  if (strlen(data->string) > IPMI_MAX_K_G_LENGTH)
    err_exit("Config File Error: K_g too long");

  strcpy(conf->k_g, data->string);
  return 0;
}

static int
_cb_privilege(conffile_t cf, 
		    struct conffile_data *data,
		    char *optionname,
		    int option_type,
		    void *option_ptr,
		    int option_data, 
		    void *app_ptr,
		    int app_data)
{
  if (conf->privilege_set)
    return 0;

  if (!strcasecmp(data->string, "user"))
    conf->privilege = IPMICONSOLE_PRIVILEGE_USER;
  else if (!strcasecmp(data->string, "operator"))
    conf->privilege = IPMICONSOLE_PRIVILEGE_OPERATOR;
  else if (!strcasecmp(data->string, "admin")
	   || !strcasecmp(data->string, "administrator"))
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
  if (conf->cipher_suite_id_set)
    return 0;

  conf->cipher_suite_id = data->intval;
  if (conf->cipher_suite_id < IPMI_CIPHER_SUITE_ID_MIN
      || conf->cipher_suite_id > IPMI_CIPHER_SUITE_ID_MAX)
    err_exit("Command Line Error: cipher suite id invalid\n");
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
_config_file_parse(void)
{
  int hostname_flag,
    username_flag,
    password_flag, 
    k_g_flag,
    privilege_flag, 
    cipher_suite_id_flag,
    dont_steal_flag,
    lock_memory_flag,
    intel_2_0_session_flag,
    supermicro_2_0_session_flag;
  
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
      {
        "privilege", 
        CONFFILE_OPTION_STRING, 
        -1,
        _cb_privilege,
        1, 
        0, 
        &privilege_flag,
        NULL, 
        0
      },
      {
        "cipher_suite_id", 
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
        conf->dont_steal_set
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
        conf->lock_memory_set
      },
      {
        "intel_2_0_session", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        _cb_bool,
        1, 
        0, 
        &intel_2_0_session_flag, 
        &(conf->intel_2_0_session),
        conf->intel_2_0_session_set
      },
      {
        "supermicro_2_0_session", 
        CONFFILE_OPTION_BOOL, 
        -1, 
        _cb_bool,
        1, 
        0, 
        &supermicro_2_0_session_flag, 
        &(conf->supermicro_2_0_session),
        conf->supermicro_2_0_session_set
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
  _cmdline_parse(argc, argv);
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
