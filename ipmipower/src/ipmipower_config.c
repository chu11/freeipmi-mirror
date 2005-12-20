/*****************************************************************************\
 *  $Id: ipmipower_config.c,v 1.6.2.2 2005-12-20 19:05:00 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
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
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdint.h>
#include <getopt.h>

#include "ipmipower_config.h"
#include "ipmipower_auth.h"
#include "ipmipower_output.h"
#include "ipmipower_privilege.h"
#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"
      
extern struct ipmipower_config *conf;
extern struct ipmipower_connection *ics;

void 
ipmipower_config_default_logfile(char *buf, int buflen)
{
  char buffer[MAXPATHLEN+1];
  pid_t pid;

  assert(buf != NULL && buflen > 0);

  pid = getpid();
  snprintf(buffer, MAXPATHLEN, IPMIPOWER_DEFAULT_LOGFILE, pid);
  if (strlen(buffer) > buflen - 1)
    err_exit("ipmipower_config_default_logfile: internal buffer too small\n");
  strcpy(buf, buffer);
}

void 
ipmipower_config_setup(void) 
{
  assert(conf == NULL);         /* Already initialized */

  conf = (struct ipmipower_config *)Secure_malloc(sizeof(struct ipmipower_config));

  conf->hosts = NULL;
  conf->hosts_count = 0;
  memset(conf->username, '\0', IPMI_SESSION_MAX_USERNAME_LEN+1);
  memset(conf->password, '\0', IPMI_SESSION_MAX_AUTH_CODE_LEN+1);
  conf->powercmd = POWER_CMD_NONE;
  memset(conf->configfile, '\0', MAXPATHLEN+1);

  conf->authtype = AUTH_TYPE_AUTO;
  conf->privilege = PRIVILEGE_TYPE_AUTO;
  conf->on_if_off = IPMIPOWER_FALSE;
  conf->outputtype = OUTPUT_TYPE_NEWLINE;
  conf->force_permsg_auth = IPMIPOWER_FALSE;
#ifndef NDEBUG
  conf->debug = IPMIPOWER_FALSE;
  conf->ipmidump = IPMIPOWER_FALSE;
  conf->rmcpdump = IPMIPOWER_FALSE;
  conf->log = IPMIPOWER_FALSE;
  memset(conf->logfile, '\0', MAXPATHLEN+1);
  ipmipower_config_default_logfile(conf->logfile, MAXPATHLEN);
  conf->logfile_fd = -1;
#endif
  conf->timeout_len = 20000;     /* 20 seconds */
  conf->retry_timeout_len = 400; /* .4 seconds  */
  conf->retry_backoff_count = 8;
  conf->ping_interval_len = 5000; /* 5 seconds */
  conf->ping_timeout_len = 30000; /* 30 seconds */
  conf->ping_packet_count = 10;
  conf->ping_percent = 50;
  conf->ping_consec_count = 5;

  /* Options not found yet, all false */
  conf->hosts_set = IPMIPOWER_FALSE;
  conf->username_set = IPMIPOWER_FALSE;
  conf->password_set = IPMIPOWER_FALSE;
  conf->authtype_set = IPMIPOWER_FALSE;
  conf->privilege_set = IPMIPOWER_FALSE;
  conf->outputtype_set = IPMIPOWER_FALSE;
  conf->force_permsg_auth = IPMIPOWER_FALSE;
  conf->timeout_len_set = IPMIPOWER_FALSE;
  conf->retry_timeout_len_set = IPMIPOWER_FALSE;
  conf->retry_backoff_count_set = IPMIPOWER_FALSE;
  conf->ping_interval_len_set = IPMIPOWER_FALSE;
  conf->ping_timeout_len_set = IPMIPOWER_FALSE;
  conf->ping_packet_count_set = IPMIPOWER_FALSE;
  conf->ping_percent_set = IPMIPOWER_FALSE;
  conf->ping_consec_count_set = IPMIPOWER_FALSE;
}

static void 
_config_common_checks(char *str) 
{
  assert (str != NULL);

  if (conf->hosts != NULL 
      && (conf->hosts_count < IPMIPOWER_MINNODES 
          || conf->hosts_count > IPMIPOWER_MAXNODES))
    err_exit("%s: invalid number of hostnames", str);
    
  if (conf->authtype == AUTH_TYPE_INVALID) 
    err_exit("%s: invalid authtype", str);

  if (conf->privilege == PRIVILEGE_TYPE_INVALID)
    err_exit("%s: invalid privilege", str);

  if (conf->outputtype == OUTPUT_TYPE_INVALID) 
    err_exit("%s: invalid outputtype", str);

  if (conf->timeout_len < IPMIPOWER_TIMEOUT_MIN 
      || conf->timeout_len > IPMIPOWER_TIMEOUT_MAX)
    err_exit("%s: timeout out of range", str);
  
  if (conf->retry_timeout_len != 0 
      && (conf->retry_timeout_len < IPMIPOWER_RETRY_TIMEOUT_MIN 
          || conf->retry_timeout_len > IPMIPOWER_RETRY_TIMEOUT_MAX))
    err_exit("%s: retry timeout out of range", str);
  
  if (conf->retry_backoff_count != 0 
      && (conf->retry_backoff_count < IPMIPOWER_RETRY_BACKOFF_COUNT_MIN 
          || conf->retry_backoff_count > IPMIPOWER_RETRY_BACKOFF_COUNT_MAX))
    err_exit("%s: retry backoff count out of range", str);

  if (conf->ping_interval_len != 0 
      && (conf->ping_interval_len < IPMIPOWER_PING_INTERVAL_MIN 
          || conf->ping_interval_len > IPMIPOWER_PING_INTERVAL_MAX))
    err_exit("%s: ping interval out of range", str);
  
  if (conf->ping_timeout_len != 0 
      && (conf->ping_timeout_len < IPMIPOWER_PING_TIMEOUT_MIN 
          || conf->ping_timeout_len > IPMIPOWER_PING_TIMEOUT_MAX))
    err_exit("%s: ping timeout out of range", str);

  if (conf->ping_packet_count != 0
      && (conf->ping_packet_count < IPMIPOWER_PING_PACKET_COUNT_MIN
          || conf->ping_packet_count > IPMIPOWER_PING_PACKET_COUNT_MAX))
    err_exit("%s: ping packet out of range", str);

  if (conf->ping_percent != 0
      && (conf->ping_percent < IPMIPOWER_PING_PERCENT_MIN
          || conf->ping_percent > IPMIPOWER_PING_PERCENT_MAX))
    err_exit("%s: ping percent out of range", str);
  
  if (conf->ping_consec_count != 0
      && (conf->ping_consec_count < IPMIPOWER_PING_CONSEC_COUNT_MIN
          || conf->ping_consec_count > IPMIPOWER_PING_CONSEC_COUNT_MAX))
    err_exit("%s: ping consec out of range", str);
}

/* _usage
 * - output usage 
 */
static void 
_usage(void) 
{
  fprintf(stderr, "Usage: ipmipower [OPTIONS]\n"
          "-h --hostnames hosts  List of hostnames\n"
          "-u --username name    Username\n"
          "-p --password pw      Password\n" 
          "-n --on               Power On\n"
          "-f --off              Power Off\n"
          "-c --cycle            Power Cycle\n"
          "-r --reset            Power Reset\n"
          "-s --stat             Power Status Query\n"
          "-j --pulse            Pulse Diagnostic Interrupt\n"
          "-k --soft             Soft Shutdown OS via ACPI\n"
          "-H --help             Output help menu\n"
          "-V --version          Output version\n"
          "-C --config           Specify Alternate Config File\n"
          );
  exit(1);
}

/* _version
 * - output version
 */
static void 
_version(void) 
{
  fprintf(stderr, "ipmipower %s\n", VERSION);
  exit(1);
}

void 
ipmipower_config_cmdline_parse(int argc, char **argv) 
{
  char c;
  char *ptr;

#ifndef NDEBUG
  char *options = "h:u:p:nfcrsjkHVC:a:l:go:PDIRLF:t:y:b:i:z:v:w:x:";
#else
  char *options = "h:u:p:nfcrsjkHVC:a:l:go:Pt:y:b:i:z:v:w:x:";
#endif
    
#if HAVE_GETOPT_LONG
  struct option long_options[] = 
    {
      {"hostnames",           1, NULL, 'h'},
      {"username",            1, NULL, 'u'},
      {"password",            1, NULL, 'p'},
      {"on",                  0, NULL, 'n'},
      {"off",                 0, NULL, 'f'},
      {"cycle",               0, NULL, 'c'},
      {"reset",               0, NULL, 'r'},
      {"stat",                0, NULL, 's'},
      {"pulse",               0, NULL, 'j'},
      {"soft",                0, NULL, 'k'},
      {"help",                0, NULL, 'H'},
      {"version",             0, NULL, 'V'},
      {"config",              1, NULL, 'C'}, 

      {"authtype",            1, NULL, 'a'},  
      {"privilege",           1, NULL, 'l'},
      {"on-if-off",           0, NULL, 'g'},
      {"outputtype",          1, NULL, 'o'},
      {"force-permsg-auth",   0, NULL, 'P'},
#ifndef NDEBUG
      {"debug",               0, NULL, 'D'},
      {"ipmidump",            0, NULL, 'I'},
      {"rmcpdump",            0, NULL, 'R'},
      {"log",                 0, NULL, 'L'},
      {"logfile",             1, NULL, 'F'},
#endif
      {"timeout" ,            1, NULL, 't'},
      {"retry-timeout",       1, NULL, 'y'},
      {"retry-backoff-count", 1, NULL, 'b'},
      {"ping-interval",       1, NULL, 'i'},
      {"ping-timeout",        1, NULL, 'z'},
      {"ping-packet-count",   1, NULL, 'v'},
      {"ping-percent",        1, NULL, 'w'},
      {"ping-consec-count",   1, NULL, 'x'},
      {0, 0, 0, 0},
    };
#endif

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
        case 'h':       /* --hostnames */
          if ((conf->hosts = hostlist_create(optarg)) == NULL)
            err_exit("Error: Hostnames incorrectly formatted");
          hostlist_uniq(conf->hosts);
          conf->hosts_count = hostlist_count(conf->hosts);
          conf->hosts_set = IPMIPOWER_TRUE;
          break;
        case 'u':       /* --username */
          if (strlen(optarg) > IPMI_SESSION_MAX_USERNAME_LEN)
            err_exit("Command Line Error: username too long");
          strcpy(conf->username, optarg);
          conf->username_set = IPMIPOWER_TRUE;
          break;
        case 'p':       /* --password */
          if (strlen(optarg) > IPMI_SESSION_MAX_AUTH_CODE_LEN)
            err_exit("Command Line Error: password too long");
          strcpy(conf->password, optarg);
          conf->password_set = IPMIPOWER_TRUE;
          break;
        case 'n':       /* --on */ 
          conf->powercmd = POWER_CMD_POWER_ON;
          break;
        case 'f':       /* --off */ 
          conf->powercmd = POWER_CMD_POWER_OFF;
          break;
        case 'c':       /* --cycle */ 
          conf->powercmd = POWER_CMD_POWER_CYCLE;
          break;
        case 'r':       /* --reset */ 
          conf->powercmd = POWER_CMD_POWER_RESET;
          break;
        case 's':       /* --stat */ 
          conf->powercmd = POWER_CMD_POWER_STATUS;
          break;
        case 'j':       /* --pulse */
          conf->powercmd = POWER_CMD_PULSE_DIAG_INTR;
          break;
        case 'k':       /* --soft */
          conf->powercmd = POWER_CMD_SOFT_SHUTDOWN_OS;
          break;
        case 'H':       /* --help */
          _usage();
          break;
        case 'V':       /* --version */
          _version();
          break;
        case 'C':
          if (strlen(optarg) > MAXPATHLEN)
            err_exit("Command Line Error: configuration file pathname too long");
          strcpy(conf->configfile, optarg);
          break;
        case 'a':       /* --authtype */
          conf->authtype = ipmipower_auth_index(optarg);
          conf->authtype_set = IPMIPOWER_TRUE;
          break;
        case 'l':       /* --privilege */
          conf->privilege = ipmipower_privilege_index(optarg);
          conf->privilege_set = IPMIPOWER_TRUE;
          break;
        case 'g':       /* --on-if-off */
          conf->on_if_off = !conf->on_if_off;
          conf->on_if_off_set = IPMIPOWER_TRUE;
          break;
        case 'o':       /* --outputtype */
          conf->outputtype = ipmipower_output_index(optarg);
          conf->outputtype_set = IPMIPOWER_TRUE;
          break;
        case 'P':       /* --force-permsg-auth */
          conf->force_permsg_auth = IPMIPOWER_TRUE;
          conf->force_permsg_auth_set = IPMIPOWER_TRUE;
          break;
#ifndef NDEBUG
        case 'D':       /* --debug */
          conf->debug = !conf->debug;
          break;
        case 'I':       /* --ipmidump */
          conf->ipmidump = !conf->ipmidump;
          break;
        case 'R':       /* --rmcpdump */
          conf->rmcpdump = !conf->rmcpdump;
          break;
	case 'L':       /* --log */
	  conf->log = !conf->log;
	  break;
	case 'F':       /* --logfile */
          if (strlen(optarg) > MAXPATHLEN)
            err_exit("Command Line Error: log file pathname too long");
	  memset(conf->logfile, '\0', MAXPATHLEN+1);
          strcpy(conf->logfile, optarg);
	  break;
#endif
        case 't':       /* --timeout */
          conf->timeout_len = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: timeout length invalid\n");
          conf->timeout_len_set = IPMIPOWER_TRUE;
          break;
        case 'y':       /* --retry-timeout */
          conf->retry_timeout_len = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: retry timeout length invalid\n");
          conf->retry_timeout_len_set = IPMIPOWER_TRUE;
          break;
        case 'b':       /* --retry-backoff-count */
          conf->retry_backoff_count = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: retry backoff count invalid\n");
          conf->retry_backoff_count_set = IPMIPOWER_TRUE;
          break;
        case 'i':       /* --ping-interval */
          conf->ping_interval_len = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: ping interval length invalid\n");
          conf->ping_interval_len_set = IPMIPOWER_TRUE;
          break;
        case 'z':       /* --ping-timeout */
          conf->ping_timeout_len = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: ping timeout length invalid\n");
          conf->ping_timeout_len_set = IPMIPOWER_TRUE;
          break;
        case 'v':       /* --ping-packet-count */
          conf->ping_packet_count = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: ping packet count invalid\n");
          conf->ping_packet_count_set = IPMIPOWER_TRUE;
          break;
        case 'w':       /* --ping-percent */
          conf->ping_percent = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: ping percent invalid\n");
          conf->ping_percent_set = IPMIPOWER_TRUE;
          break;
        case 'x':       /* --ping-consec-count */
          conf->ping_consec_count = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: ping consec count invalid\n");
          conf->ping_consec_count_set = IPMIPOWER_TRUE;
          break;
        default:
          fprintf(stderr, "Error: command line option error\n");
          _usage();
          break;
        }
    }
  
  _config_common_checks("Command Line Error");
  
  if (optind < argc)
    _usage();

  if (conf->powercmd != POWER_CMD_NONE)
    conf->ping_interval_len = 0;     /* force pings to be off */
}

/*
 * Conffile library callback functions
 */
    
static int 
_cb_hostnames(conffile_t cf, struct conffile_data *data,
              char *optionname, int option_type, void *option_ptr, 
              int option_data, void *app_ptr, int app_data) 
{
  int i;
  
  if (conf->hosts_set == IPMIPOWER_TRUE)
    return 0;
  
  if ((conf->hosts = hostlist_create(NULL)) == NULL)
    err_exit("Conf-> File Error: Hostnames incorrectly formatted");
  
  for (i = 0; i < data->stringlist_len; i++) 
    {
      if (hostlist_push(conf->hosts, data->stringlist[i]) == 0)
        err_exit("Conf-> File Error: Hostnames incorrectly formatted");
    }
  
  hostlist_uniq(conf->hosts);
  
  conf->hosts_count = hostlist_count(conf->hosts);
  
  return 0;
}

static int 
_cb_authtype(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr, 
             int option_data, void *app_ptr, int app_data) 
{
  if (conf->authtype_set == IPMIPOWER_TRUE)
      return 0;

  /* Incorrect authtype checked in _config_common_checks */
  conf->authtype = ipmipower_auth_index(data->string);
  return 0;
}

static int 
_cb_privilege(conffile_t cf, struct conffile_data *data,
              char *optionname, int option_type, void *option_ptr, 
              int option_data, void *app_ptr, int app_data) 
{
  if (conf->privilege_set == IPMIPOWER_TRUE)
      return 0;

  /* Incorrect privilege checked in _config_common_checks */
  conf->privilege = ipmipower_privilege_index(data->string);
  return 0;
}

static int 
_cb_outputtype(conffile_t cf, struct conffile_data *data,
               char *optionname, int option_type, void *option_ptr, 
               int option_data, void *app_ptr, int app_data) 
{
  if (conf->outputtype_set == IPMIPOWER_TRUE)
    return 0;

  /* Incorrect outputtype checked in _config_common_checks */
  conf->outputtype = ipmipower_output_index(data->string);
  return 0;
}

static int 
_cb_bool(conffile_t cf, struct conffile_data *data,
         char *optionname, int option_type, void *option_ptr,
         int option_data, void *app_ptr, int app_data) 
{
  ipmipower_bool_t *boolval = (ipmipower_bool_t *)option_ptr;
  ipmipower_bool_t cmdlineset = (ipmipower_bool_t)option_data;

  if (cmdlineset == IPMIPOWER_TRUE)
    return 0;

  *boolval = data->boolval;
  return 0;
}

static int 
_cb_int(conffile_t cf, struct conffile_data *data,
        char *optionname, int option_type, void *option_ptr,
        int option_data, void *app_ptr, int app_data) 
{
  int *temp = (int *)option_ptr;
  ipmipower_bool_t cmdlineset = (ipmipower_bool_t)option_data;

  if (cmdlineset == IPMIPOWER_TRUE)
    return 0;

  *temp = data->intval; 
  return 0;
}

static int 
_cb_username(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr,
             int option_data, void *app_ptr, int app_data) 
{
  if (conf->username_set == IPMIPOWER_TRUE)
    return 0;

  if (strlen(data->string) > IPMI_SESSION_MAX_USERNAME_LEN)
    err_exit("Conf-> File Error: username too long");

  strcpy(conf->username, data->string);
  return 0;
}

static int 
_cb_password(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr,
             int option_data, void *app_ptr, int app_data) 
{
  if (conf->password_set == IPMIPOWER_TRUE)
    return 0;

  if (strlen(data->string) > IPMI_SESSION_MAX_AUTH_CODE_LEN)
    err_exit("Conf-> File Error: password too long");

  strcpy(conf->password, data->string);
  return 0;
}

void 
ipmipower_config_conffile_parse(char *configfile) 
{
  int hostnames_flag, username_flag, password_flag, authtype_flag, 
    privilege_flag, on_if_off_flag, outputtype_flag, force_permsg_auth_flag, 
    timeout_flag, retry_timeout_flag, retry_backoff_count_flag, 
    ping_interval_flag, ping_timeout_flag, ping_packet_count_flag, 
    ping_percent_flag, ping_consec_count_flag;

  struct conffile_option options[] = 
    {
      {"hostnames", CONFFILE_OPTION_LIST_STRING, -1, _cb_hostnames, 
       1, 0, &hostnames_flag, NULL, 0},
      {"username", CONFFILE_OPTION_STRING, -1, _cb_username,
       1, 0, &username_flag, NULL, 0},
      {"password", CONFFILE_OPTION_STRING, -1, _cb_password, 
       1, 0, &password_flag, NULL, 0},
      {"authtype", CONFFILE_OPTION_STRING, -1, _cb_authtype, 
       1, 0, &authtype_flag, NULL, 0},
      {"privilege", CONFFILE_OPTION_STRING, -1, _cb_privilege, 
       1, 0, &privilege_flag, NULL, 0},
      {"on-if-off", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &on_if_off_flag, &(conf->on_if_off), conf->on_if_off_set},
      {"outputtype", CONFFILE_OPTION_STRING, -1, _cb_outputtype, 
       1, 0, &outputtype_flag, NULL, 0},
      {"force_permsg_auth", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &force_permsg_auth_flag, &(conf->force_permsg_auth), 
       conf->force_permsg_auth_set},
      {"timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &timeout_flag, &(conf->timeout_len), conf->timeout_len_set},
      {"retry-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &retry_timeout_flag, &(conf->retry_timeout_len), 
       conf->retry_timeout_len_set},
      {"retry-backoff-count", CONFFILE_OPTION_INT, -1, _cb_int,
       1, 0, &retry_backoff_count_flag, &(conf->retry_backoff_count), 
       conf->retry_backoff_count_set},
      {"ping_interval", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_interval_flag, &(conf->ping_interval_len), 
       conf->ping_interval_len_set},
      {"ping_timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_timeout_flag, &(conf->ping_timeout_len), 
       conf->ping_timeout_len_set},
      {"ping_packet_count", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_packet_count_flag, &(conf->ping_packet_count), 
       conf->ping_packet_count_set},
      {"ping_percent", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_percent_flag, &(conf->ping_percent), 
       conf->ping_percent_set},
      {"ping_consec_count", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_consec_count_flag, &(conf->ping_consec_count), 
       conf->ping_consec_count_set},
    };
  conffile_t cf = NULL;
  char *conffile = NULL;
  int num;

  if ((cf = conffile_handle_create()) == NULL)
    err_exit("Conf-> File Error: cannot create conffile handle");

  conffile = (strlen(configfile)) ? configfile : IPMIPOWER_CONFIGFILE_DEFAULT;
  num = sizeof(options)/sizeof(struct conffile_option);
  if (conffile_parse(cf, conffile, options, num, NULL, 0, 0) < 0) 
    {
      char errbuf[CONFFILE_MAX_ERRMSGLEN];
      
      /* Not an error if default file doesn't exist */ 
      if (!strlen(configfile) && conffile_errnum(cf) == CONFFILE_ERR_EXIST)
        goto done;
      
      if (conffile_errmsg(cf, errbuf, CONFFILE_MAX_ERRMSGLEN) < 0)
        err_exit("Conf-> File Error: Cannot retrieve conffile error message");
      
      err_exit("Conf-> File Error: %s", errbuf);
    }

  _config_common_checks("Conf-> File Error");

 done:
  (void)conffile_handle_destroy(cf);
  return;
}

void 
ipmipower_config_check_values(void) 
{
  if (conf->retry_timeout_len > conf->timeout_len)
    err_exit("Error: Timeout length must be longer than retry timeout length");
  
  if (conf->ping_interval_len > conf->ping_timeout_len)
    err_exit("Error: Ping timeout interval length must be "
             "longer than ping interval length");

  if (conf->ping_consec_count > conf->ping_packet_count)
    err_exit("Error: Ping consec count must be larger than ping packet count");

  if (conf->powercmd != POWER_CMD_NONE && conf->hosts == NULL)
    err_exit("Error: Must specify target hostnames in non-interactive mode");

  if (conf->authtype == AUTH_TYPE_NONE && strlen(conf->password) > 0)
    err_exit("Error: password cannot be set for authentication type \"%s\"",
             ipmipower_auth_string(conf->authtype));
}
