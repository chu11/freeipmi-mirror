/*****************************************************************************\
 *  $Id: ipmipower_config.c,v 1.44 2006-10-21 01:36:27 chu11 Exp $
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
#include <assert.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#if HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#include <errno.h>

#include "ipmipower_config.h"
#include "ipmipower_authentication.h"
#include "ipmipower_cipher_suite.h"
#include "ipmipower_ipmi_version.h"
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

#ifdef NDEBUG
  if (!(conf = (struct ipmipower_config *)secure_malloc(sizeof(struct ipmipower_config))))
    err_exit("secure_malloc: %s", strerror(errno));
#else  /* !NDEBUG */
  if (!(conf = (struct ipmipower_config *)malloc(sizeof(struct ipmipower_config))))
    err_exit("malloc: %s", strerror(errno));
#endif /* !NDEBUG */

  conf->hosts = NULL;
  conf->hosts_count = 0;
  memset(conf->username, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
  memset(conf->password, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH+1);
  memset(conf->k_g, '\0', IPMI_MAX_K_G_LENGTH+1);
  conf->powercmd = POWER_CMD_NONE;
  memset(conf->configfile, '\0', MAXPATHLEN+1);

  conf->authentication_type = AUTHENTICATION_TYPE_AUTO;
  conf->privilege = PRIVILEGE_TYPE_AUTO;
  conf->ipmi_version = IPMI_VERSION_AUTO;
  conf->cipher_suite_id = CIPHER_SUITE_ID_AUTO;
  conf->on_if_off = IPMIPOWER_FALSE;
  conf->wait_until_on = IPMIPOWER_FALSE;
  conf->wait_until_off = IPMIPOWER_FALSE;
  conf->outputtype = OUTPUT_TYPE_NEWLINE;
  conf->force_permsg_authentication = IPMIPOWER_FALSE;
  conf->accept_session_id_zero = IPMIPOWER_FALSE;
  conf->check_unexpected_authcode = IPMIPOWER_FALSE;
  conf->intel_2_0_session = IPMIPOWER_FALSE;
  conf->supermicro_2_0_session = IPMIPOWER_FALSE;
#ifndef NDEBUG
  conf->debug = IPMIPOWER_FALSE;
  conf->ipmidump = IPMIPOWER_FALSE;
  conf->rmcpdump = IPMIPOWER_FALSE;
  conf->log = IPMIPOWER_FALSE;
  memset(conf->logfile, '\0', MAXPATHLEN+1);
  ipmipower_config_default_logfile(conf->logfile, MAXPATHLEN);
  conf->logfile_fd = -1;
#endif /* NDEBUG */
  conf->timeout_len = 20000;     /* 20 seconds */
  conf->retry_timeout_len = 400; /* .4 seconds  */
  conf->retry_wait_timeout_len = 500; /* .5 seconds  */
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
  conf->authentication_type_set = IPMIPOWER_FALSE;
  conf->privilege_set = IPMIPOWER_FALSE;
  conf->ipmi_version_set = IPMIPOWER_FALSE;
  conf->cipher_suite_id_set = IPMIPOWER_FALSE;
  conf->on_if_off_set = IPMIPOWER_FALSE;
  conf->wait_until_on_set = IPMIPOWER_FALSE;
  conf->wait_until_off_set = IPMIPOWER_FALSE;
  conf->outputtype_set = IPMIPOWER_FALSE;
  conf->force_permsg_authentication_set = IPMIPOWER_FALSE;
  conf->accept_session_id_zero_set = IPMIPOWER_FALSE;
  conf->check_unexpected_authcode_set = IPMIPOWER_FALSE;
  conf->intel_2_0_session_set = IPMIPOWER_FALSE;
  conf->supermicro_2_0_session_set = IPMIPOWER_FALSE;
  conf->timeout_len_set = IPMIPOWER_FALSE;
  conf->retry_timeout_len_set = IPMIPOWER_FALSE;
  conf->retry_wait_timeout_len_set = IPMIPOWER_FALSE;
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
    
  if (conf->authentication_type == AUTHENTICATION_TYPE_INVALID) 
    err_exit("%s: invalid authentication_type", str);

  if (conf->privilege == PRIVILEGE_TYPE_INVALID)
    err_exit("%s: invalid privilege", str);

  if (conf->ipmi_version == IPMI_VERSION_INVALID)
    err_exit("%s: invalid ipmi version", str);

  if (conf->cipher_suite_id == CIPHER_SUITE_ID_INVALID)
    err_exit("%s: invalid cipher suite id", str);

  if (conf->outputtype == OUTPUT_TYPE_INVALID) 
    err_exit("%s: invalid outputtype", str);

  if (conf->timeout_len < IPMIPOWER_TIMEOUT_MIN 
      || conf->timeout_len > IPMIPOWER_TIMEOUT_MAX)
    err_exit("%s: timeout out of range", str);
  
  if (conf->retry_timeout_len != 0 
      && (conf->retry_timeout_len < IPMIPOWER_RETRY_TIMEOUT_MIN 
          || conf->retry_timeout_len > IPMIPOWER_RETRY_TIMEOUT_MAX))
    err_exit("%s: retry timeout out of range", str);

  if (conf->retry_wait_timeout_len != 0 
      && (conf->retry_wait_timeout_len < IPMIPOWER_RETRY_WAIT_TIMEOUT_MIN 
          || conf->retry_wait_timeout_len > IPMIPOWER_RETRY_WAIT_TIMEOUT_MAX))
    err_exit("%s: retry wait timeout out of range", str);
  
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
          "-k --k-g str          K_g Key\n"
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

  /* achu: Here's are what options are left and available
     lower case: de
     upper case: EGJKNOQWZ
   */

#ifndef NDEBUG
  char *options = "h:u:p:k:nfcrsjmHVC:a:l:R:T:gABo:PSUXYDIMLF:t:y:q:b:i:z:v:w:x:";
#else  /* !NDEBUG */
  char *options = "h:u:p:k:nfcrsjmHVC:a:l:R:T:gABo:PSUXYt:y:q:b:i:z:v:w:x:";
#endif /* !NDEBUG */
    
#if HAVE_GETOPT_LONG
  struct option long_options[] = 
    {
      {"hostnames",                    1, NULL, 'h'},
      {"username",                     1, NULL, 'u'},
      {"password",                     1, NULL, 'p'},
      {"k-g",                          1, NULL, 'k'},
      {"on",                           0, NULL, 'n'},
      {"off",                          0, NULL, 'f'},
      {"cycle",                        0, NULL, 'c'},
      {"reset",                        0, NULL, 'r'},
      {"stat",                         0, NULL, 's'},
      {"pulse",                        0, NULL, 'j'},
      {"soft",                         0, NULL, 'm'},
      {"help",                         0, NULL, 'H'},
      {"version",                      0, NULL, 'V'},
      {"config",                       1, NULL, 'C'}, 
      {"authentication-type",          1, NULL, 'a'},  
      {"privilege",                    1, NULL, 'l'},
      {"ipmi-version",                 1, NULL, 'R'},
      {"cipher-suite-id",              1, NULL, 'T'},
      {"on-if-off",                    0, NULL, 'g'},
      {"wait-until-on",                0, NULL, 'A'},
      {"wait-until-off",               0, NULL, 'B'},
      {"outputtype",                   1, NULL, 'o'},
      {"force-permsg-authentication",  0, NULL, 'P'},
      {"accept-session-id-zero",       0, NULL, 'S'},
      {"check-unexpected-authcode",    0, NULL, 'U'},
      {"intel-2-0-session",            0, NULL, 'X'},
      {"supermicro-2-0-session",       0, NULL, 'Y'},
#ifndef NDEBUG
      {"debug",                        0, NULL, 'D'},
      {"ipmidump",                     0, NULL, 'I'},
      {"rmcpdump",                     0, NULL, 'M'},
      {"log",                          0, NULL, 'L'},
      {"logfile",                      1, NULL, 'F'},
#endif /* NDEBUG */
      {"timeout" ,                     1, NULL, 't'},
      {"retry-timeout",                1, NULL, 'y'},
      {"retry-wait-timeout",           1, NULL, 'q'},
      {"retry-backoff-count",          1, NULL, 'b'},
      {"ping-interval",                1, NULL, 'i'},
      {"ping-timeout",                 1, NULL, 'z'},
      {"ping-packet-count",            1, NULL, 'v'},
      {"ping-percent",                 1, NULL, 'w'},
      {"ping-consec-count",            1, NULL, 'x'},
      {0, 0, 0, 0},
    };
#endif /* HAVE_GETOPT_LONG */

  /* turn off output messages */
  opterr = 0;

#if HAVE_GETOPT_LONG
  while ((c = getopt_long(argc, argv, options, long_options, NULL)) != -1)
#else  /* !HAVE_GETOPT_LONG */
  while ((c = getopt(argc, argv, options)) != -1)
#endif /* !HAVE_GETOPT_LONG */
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
          if (strlen(optarg) > IPMI_MAX_USER_NAME_LENGTH)
            err_exit("Command Line Error: username too long");
          strcpy(conf->username, optarg);
          conf->username_set = IPMIPOWER_TRUE;
	  /* Args will be cleared out in main() */
          break;
        case 'p':       /* --password */
          if (strlen(optarg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
            err_exit("Command Line Error: password too long");
          strcpy(conf->password, optarg);
          conf->password_set = IPMIPOWER_TRUE;
	  /* Args will be cleared out in main() */
          break;
        case 'k':       /* --k-g */
          if (strlen(optarg) > IPMI_MAX_K_G_LENGTH)
            err_exit("Command Line Error: K_g too long");
          strcpy(conf->k_g, optarg);
          conf->k_g_set = IPMIPOWER_TRUE;
	  /* Args will be cleared out in main() */
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
        case 'm':       /* --soft */
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
        case 'a':       /* --authentication-type */
          conf->authentication_type = ipmipower_authentication_type_index(optarg);
          conf->authentication_type_set = IPMIPOWER_TRUE;
          break;
        case 'l':       /* --privilege */
          conf->privilege = ipmipower_privilege_index(optarg);
          conf->privilege_set = IPMIPOWER_TRUE;
          break;
	case 'R':	/* --ipmi-version */
          conf->ipmi_version = ipmipower_ipmi_version_index(optarg);
	  conf->ipmi_version_set = IPMIPOWER_TRUE;
          break;
        case 'T':       /* --cipher-suite-id */
          conf->cipher_suite_id = ipmipower_cipher_suite_id_index(optarg);
          conf->cipher_suite_id_set = IPMIPOWER_TRUE;
        case 'g':       /* --on-if-off */
          conf->on_if_off = !conf->on_if_off;
          conf->on_if_off_set = IPMIPOWER_TRUE;
          break;
        case 'A':       /* --wait-until-on */
          conf->wait_until_on = !conf->wait_until_on;
          conf->wait_until_on_set = IPMIPOWER_TRUE;
          break;
        case 'B':       /* --wait-until-off */
          conf->wait_until_off = !conf->wait_until_off;
          conf->wait_until_off_set = IPMIPOWER_TRUE;
          break;
        case 'o':       /* --outputtype */
          conf->outputtype = ipmipower_output_index(optarg);
          conf->outputtype_set = IPMIPOWER_TRUE;
          break;
        case 'P':       /* --force-permsg-authentication */
          conf->force_permsg_authentication = IPMIPOWER_TRUE;
          conf->force_permsg_authentication_set = IPMIPOWER_TRUE;
          break;
        case 'S':       /* --accept-session-id-zero */
          conf->accept_session_id_zero = IPMIPOWER_TRUE;
          conf->accept_session_id_zero_set = IPMIPOWER_TRUE;
          break;
        case 'U':       /* --check-unexpected-authcode */
          conf->check_unexpected_authcode = IPMIPOWER_TRUE;
          conf->check_unexpected_authcode_set = IPMIPOWER_TRUE;
          break;
        case 'X':      /* --intel-2-0-session */
          conf->intel_2_0_session = IPMIPOWER_TRUE;
          conf->intel_2_0_session_set = IPMIPOWER_TRUE;
          break;
        case 'Y':      /* --supermicro-2-0-session */
          conf->supermicro_2_0_session = IPMIPOWER_TRUE;
          conf->supermicro_2_0_session_set = IPMIPOWER_TRUE;
          break;
#ifndef NDEBUG
        case 'D':       /* --debug */
          conf->debug = !conf->debug;
          break;
        case 'I':       /* --ipmidump */
          conf->ipmidump = !conf->ipmidump;
          break;
        case 'M':       /* --rmcpdump */
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
#endif /* !NDEBUG */
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
        case 'q':       /* --retry-wait-timeout */
          conf->retry_wait_timeout_len = strtol(optarg, &ptr, 10);
          if (ptr != (optarg + strlen(optarg)))
            err_exit("Command Line Error: retry wait timeout length invalid\n");
          conf->retry_wait_timeout_len_set = IPMIPOWER_TRUE;
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
    err_exit("Config File Error: Hostnames incorrectly formatted");
  
  for (i = 0; i < data->stringlist_len; i++) 
    {
      if (hostlist_push(conf->hosts, data->stringlist[i]) == 0)
        err_exit("Config File Error: Hostnames incorrectly formatted");
    }
  
  hostlist_uniq(conf->hosts);
  
  conf->hosts_count = hostlist_count(conf->hosts);
  
  return 0;
}

static int 
_cb_authentication_type(conffile_t cf, struct conffile_data *data,
			char *optionname, int option_type, void *option_ptr, 
			int option_data, void *app_ptr, int app_data) 
{
  if (conf->authentication_type_set == IPMIPOWER_TRUE)
      return 0;

  /* Incorrect authentication_type checked in _config_common_checks */
  conf->authentication_type = ipmipower_authentication_type_index(data->string);
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
_cb_ipmi_version(conffile_t cf, struct conffile_data *data,
		 char *optionname, int option_type, void *option_ptr, 
		 int option_data, void *app_ptr, int app_data) 
{
  if (conf->ipmi_version_set == IPMIPOWER_TRUE)
    return 0;

  /* Incorrect ipmi_versions checked in _config_common_checks */
  conf->ipmi_version = ipmipower_ipmi_version_index(data->string);
  return 0;
}

static int 
_cb_cipher_suite_id(conffile_t cf, struct conffile_data *data,
                    char *optionname, int option_type, void *option_ptr, 
                    int option_data, void *app_ptr, int app_data) 
{
  if (conf->cipher_suite_id_set == IPMIPOWER_TRUE)
    return 0;

  /* Incorrect cipher_suite_ids checked in _config_common_checks */
  conf->cipher_suite_id = ipmipower_cipher_suite_id_index(data->string);
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

  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    err_exit("Config File Error: username too long");

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

  if (strlen(data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    err_exit("Config File Error: password too long");

  strcpy(conf->password, data->string);
  return 0;
}

static int 
_cb_k_g(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr,
             int option_data, void *app_ptr, int app_data) 
{
  if (conf->k_g_set == IPMIPOWER_TRUE)
    return 0;

  if (strlen(data->string) > IPMI_MAX_K_G_LENGTH)
    err_exit("Config File Error: K_g too long");

  strcpy(conf->k_g, data->string);
  return 0;
}

void 
ipmipower_config_conffile_parse(char *configfile) 
{
  int hostnames_flag, username_flag, password_flag, k_g_flag, authentication_type_flag, 
    privilege_flag, cipher_suite_id_flag, ipmi_version_flag, on_if_off_flag, 
    wait_until_on_flag, wait_until_off_flag, outputtype_flag, force_permsg_authentication_flag, 
    accept_session_id_zero_flag, check_unexpected_authcode_flag, intel_2_0_session_flag, 
    supermicro_2_0_session_flag, timeout_flag, retry_timeout_flag, retry_wait_timeout_flag, 
    retry_backoff_count_flag, ping_interval_flag, ping_timeout_flag, ping_packet_count_flag, 
    ping_percent_flag, ping_consec_count_flag;

  struct conffile_option options[] = 
    {
      {"hostnames", CONFFILE_OPTION_LIST_STRING, -1, _cb_hostnames, 
       1, 0, &hostnames_flag, NULL, 0},
      {"username", CONFFILE_OPTION_STRING, -1, _cb_username,
       1, 0, &username_flag, NULL, 0},
      {"password", CONFFILE_OPTION_STRING, -1, _cb_password, 
       1, 0, &password_flag, NULL, 0},
      {"k_g", CONFFILE_OPTION_STRING, -1, _cb_k_g, 
       1, 0, &k_g_flag, NULL, 0},
      {"authentication-type", CONFFILE_OPTION_STRING, -1, _cb_authentication_type, 
       1, 0, &authentication_type_flag, NULL, 0},
      {"privilege", CONFFILE_OPTION_STRING, -1, _cb_privilege, 
       1, 0, &privilege_flag, NULL, 0},
      {"ipmi_version", CONFFILE_OPTION_STRING, -1, _cb_ipmi_version,
       1, 0, &ipmi_version_flag, NULL, 0},
      {"cipher_suite_id", CONFFILE_OPTION_STRING, -1, _cb_cipher_suite_id,
       1, 0, &cipher_suite_id_flag, NULL, 0},
      {"on-if-off", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &on_if_off_flag, &(conf->on_if_off), conf->on_if_off_set},
      {"wait-until-on", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &wait_until_on_flag, &(conf->wait_until_on), conf->wait_until_on_set},
      {"wait-until-off", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &wait_until_off_flag, &(conf->wait_until_off), conf->wait_until_off_set},
      {"outputtype", CONFFILE_OPTION_STRING, -1, _cb_outputtype, 
       1, 0, &outputtype_flag, NULL, 0},
      {"force_permsg_authentication", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &force_permsg_authentication_flag, &(conf->force_permsg_authentication), 
       conf->force_permsg_authentication_set},
      {"accept_session_id_zero", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &accept_session_id_zero_flag, &(conf->accept_session_id_zero), 
       conf->accept_session_id_zero_set},
      {"check_unexpected_authcode", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &check_unexpected_authcode_flag, &(conf->check_unexpected_authcode), 
       conf->check_unexpected_authcode_set},
      {"intel_2_0_session", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &intel_2_0_session_flag, &(conf->intel_2_0_session), 
       conf->intel_2_0_session_set},
      {"supermicro_2_0_session", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &supermicro_2_0_session_flag, &(conf->supermicro_2_0_session), 
       conf->supermicro_2_0_session_set},
      {"timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &timeout_flag, &(conf->timeout_len), conf->timeout_len_set},
      {"retry-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &retry_timeout_flag, &(conf->retry_timeout_len), 
       conf->retry_timeout_len_set},
      {"retry-wait-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &retry_wait_timeout_flag, &(conf->retry_wait_timeout_len), 
       conf->retry_wait_timeout_len_set},
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
    err_exit("Config File Error: cannot create conffile handle");

  conffile = (strlen(configfile)) ? configfile : IPMIPOWER_CONFIGFILE_DEFAULT;
  num = sizeof(options)/sizeof(struct conffile_option);
  if (conffile_parse(cf, conffile, options, num, NULL, 0, 0) < 0) 
    {
      char errbuf[CONFFILE_MAX_ERRMSGLEN];
      
      /* Not an error if default file doesn't exist */ 
      if (!strlen(configfile) && conffile_errnum(cf) == CONFFILE_ERR_EXIST)
        goto done;
      
      if (conffile_errmsg(cf, errbuf, CONFFILE_MAX_ERRMSGLEN) < 0)
        err_exit("Config File Error: Cannot retrieve conffile error message");
      
      err_exit("Config File Error: %s", errbuf);
    }

  _config_common_checks("Config File Error");

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

  if (conf->authentication_type == AUTHENTICATION_TYPE_NONE 
      && strlen(conf->password) > 0)
    err_exit("Error: password cannot be set for authentication type \"%s\"",
             ipmipower_authentication_type_string(conf->authentication_type));

  if (conf->ipmi_version != IPMI_VERSION_AUTO
      && conf->ipmi_version != IPMI_VERSION_2_0
      && strlen(conf->k_g) > 0)
    err_exit("Error: k_g is only used for IPMI 2.0");

  if (conf->ipmi_version == IPMI_VERSION_1_5
      && strlen(conf->password) >= IPMI_1_5_MAX_PASSWORD_LENGTH)
    err_exit("Error: password too long");
}
