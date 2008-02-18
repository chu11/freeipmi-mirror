/*****************************************************************************\
 *  $Id: ipmipower_config.c,v 1.73 2008-02-18 17:09:03 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
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
#include <assert.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <stdint.h>
#if HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
#include <errno.h>

#include <argp.h>

#include "ipmipower_config.h"
#include "ipmipower_authentication_type.h"
#include "ipmipower_cipher_suite_id.h"
#include "ipmipower_ipmi_version.h"
#include "ipmipower_output.h"
#include "ipmipower_privilege_level.h"
#include "ipmipower_util.h"
#include "ipmipower_workarounds.h"
#include "ipmipower_wrappers.h"

#include "secure.h"
#include "freeipmi-portability.h"
#include "tool-common.h"
      
extern struct ipmipower_config *conf;
extern struct ipmipower_connection *ics;

const char *argp_program_version = "ipmipower " VERSION "\n";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";


#define IPMIPOWER_ON_KEY                       'n'
#define IPMIPOWER_OFF_KEY                      'f'
#define IPMIPOWER_CYCLE_KEY                    'c'
#define IPMIPOWER_RESET_KEY                    'r'
#define IPMIPOWER_STAT_KEY                     's'
#define IPMIPOWER_PULSE_KEY                    'j'
#define IPMIPOWER_SOFT_KEY                     'm'

#define IPMIPOWER_IPMI_VERSION_KEY             'R'
#define IPMIPOWER_ON_IF_OFF_KEY                'g'
#define IPMIPOWER_WAIT_UNTIL_OFF_KEY           'A'
#define IPMIPOWER_WAIT_UNTIL_ON_KEY            'B'

#define IPMIPOWER_RETRY_TIMEOUT_KEY            160
#define IPMIPOWER_RETRANSMISSION_TIMEOUT_KEY   'y'
#define IPMIPOWER_TIMEOUT_KEY                  161
#define IPMIPOWER_SESSION_TIMEOUT_KEY          't'
#define IPMIPOWER_RETRY_WAIT_TIMEOUT           162
#define IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT  'q'
#define IPMIPOWER_RETRY_BACKOFF_COUNT          163
#define IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT 'b'
#define IPMIPOWER_PING_INTERVAL                'i'
#define IPMIPOWER_PING_TIMEOUT                 'z'
#define IPMIPOWER_PING_PACKET_COUNT            'v'
#define IPMIPOWER_PING_PERCENT                 'w'
#define IPMIPOWER_PING_CONSEC_COUNT            'x'

#define IPMIPOWER_CONFIG_KEY                    164
#define IPMIPOWER_DEBUG_KEY                     165
#define IPMIPOWER_IPMIDUMP_KEY                  166
#define IPMIPOWER_RMCPDUMP_KEY                  167
#define IPMIPOWER_LOG_KEY                       168
#define IPMIPOWER_LOGFILE_KEY                   169

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED_NO_TIMEOUT,
    /*
     * achu:
     *
     * argp's help/usage layout has various bugs when the description
     * buffer gets to 150 characters in length.  So we're going to shorten
     * argp usage help output.  I'll keep alot of the original text in #if
     * 0's around for the future (or just documentation).
     *
     * b/c of the text shortening, we could use the cmdline-parse-common.h
     * macros.  But for now, we'll still use our own coded one.
     */
#if 0
    {"authentication-type", ARGP_AUTHENTICATION_TYPE_KEY, "AUTHENTICATION-TYPE", 0,                 
     "Specify the IPMI 1.5 authentication type to use. "                                            
     "The currently available authentication types are NONE, STRAIGHT_PASSWORD_KEY, MD2, and MD5. " 
     "Defaults to MD5 if not specified", 12},
    {"cipher-suite-id",     ARGP_CIPHER_SUITE_ID_KEY, "CIPHER-SUITE-ID", 0,                         
     "Specify the IPMI 2.0 cipher suite ID to use. "                                                
     "The currently supported cipher suite ids are: AUTO, 0, 1, 2, 3, 6, 7, 8, 11, 12. "            
     "Defaults to AUTO if not specified.", 13},
    /* maintain "privilege" for backwards compatability */
    {"privilege",  ARGP_PRIVILEGE_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                            
     "Specify the privilege level to be used. "                                                     
     "The currently available privilege levels are AUTO, USER, OPERATOR, and ADMIN. "               
     "Defaults to AUTO if not specified.", 14},
    {"privilege-level",  ARGP_PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0,                            
     "Specify the privilege level to be used. "                                                     
     "The currently available privilege levels are AUTO, USER, OPERATOR, and ADMIN. "                
     "Defaults to AUTO if not specified.", 14},
#else
    {"authentication-type", ARGP_AUTHENTICATION_TYPE_KEY, "AUTHENTICATION-TYPE", 0,                 
     "Specify the IPMI 1.5 authentication type to use.", 13},
    {"cipher-suite-id",     ARGP_CIPHER_SUITE_ID_KEY, "CIPHER-SUITE-ID", 0,                         
     "Specify the IPMI 2.0 cipher suite ID to use.", 14},
    /* maintain "privilege" for backwards compatability */
    {"privilege",  ARGP_PRIVILEGE_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                            
     "Specify the privilege level to be used.", 15},
    {"privilege-level",  ARGP_PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0,                            
     "Specify the privilege level to be used.", 15},
#endif
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_CONSOLIDATE_OUTPUT,
    ARGP_COMMON_HOSTRANGED_ELIMINATE,
    {"on", IPMIPOWER_ON_KEY, 0, 0,
     "Power on the target hosts.", 30},
    {"off", IPMIPOWER_OFF_KEY, 0, 0,
     "Power off the target hosts.", 31},
    {"cycle", IPMIPOWER_CYCLE_KEY, 0, 0,
     "Power cycle the target hosts.", 32},
    {"reset", IPMIPOWER_RESET_KEY, 0, 0,
     "Reset the target hosts.", 33},
    {"stat", IPMIPOWER_STAT_KEY, 0, 0,
     "Get power status of the target hosts.", 34},
    {"pulse", IPMIPOWER_PULSE_KEY, 0, 0,
     "Send power diagnostic interrupt to target hosts.", 35},
    {"soft", IPMIPOWER_SOFT_KEY, 0, 0,
     "Initiate a soft-shutdown of the OS via ACPI.", 36},
    {"config", IPMIPOWER_CONFIG_KEY, "FILE", 0,
     "Specify an alternate configuration file.", 37},
    {"ipmi-version", IPMIPOWER_IPMI_VERSION_KEY, "IPMIVERSION", 0,
     "Specify the IPMI protocol version to use.", 38},
    {"on-if-off", IPMIPOWER_ON_IF_OFF_KEY, 0, 0,
     "Issue a power on command instead of a power cycle or hard reset "
     "command if the remote machine's power is currently off.", 39},
    {"wait-until-off", IPMIPOWER_WAIT_UNTIL_OFF_KEY, 0, 0,
     "Regularly query the remote BMC and return only after the machine has powered off.", 40},
    {"wait-until-on", IPMIPOWER_WAIT_UNTIL_ON_KEY, 0, 0,
     "Regularly query the remote BMC and return only after the machine has powered on.", 41},
    /* don't use the cmdline-parse-common.headers, we need to support backwards compatible short options */
    /* maintain "retry-timeout" for backwards compatability */
    {"retry-timeout", IPMIPOWER_RETRY_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,
     "Specify the packet retransmission timeout in milliseconds.", 42},
    {"retransmission-timeout", IPMIPOWER_RETRANSMISSION_TIMEOUT_KEY, "MILLISECONDS", 0,
     "Specify the packet retransmission timeout in milliseconds.", 43},
    /* maintain "timeout" for backwards compatability */
    {"timeout", IPMIPOWER_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,
     "Specify the session timeout in milliseconds.", 44},
    {"session-timeout", IPMIPOWER_SESSION_TIMEOUT_KEY, "MILLISECONDS", 0,
     "Specify the session timeout in milliseconds.", 45},
    /* retry-wait-timeout maintained for backwards comptability */
    {"retry-wait-timeout", IPMIPOWER_RETRY_WAIT_TIMEOUT, "MILLISECONDS", OPTION_HIDDEN,
     "Specify the retransmission timeout length in milliseconds.", 46},
    {"retransmission-wait-timeout", IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT, "MILLISECONDS", 0,
     "Specify the retransmission timeout length in milliseconds.", 47},
    /* retry-backoff-count maintained for backwards comptability */
    {"retry-backoff-count", IPMIPOWER_RETRY_BACKOFF_COUNT, "COUNT", OPTION_HIDDEN,
     "Specify the retransmission backoff count for retransmissions.", 48},
    {"retransmission-backoff-count", IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT, "COUNT", 0,
     "Specify the retransmission backoff count for retransmissions.", 49},
    {"ping-interval", IPMIPOWER_PING_INTERVAL, "MILLISECONDS", 0,
     "Specify the ping interval length in milliseconds.", 50},
    {"ping-timeout", IPMIPOWER_PING_TIMEOUT, "MILLISECONDS", 0,
     "Specify the ping timeout length in milliseconds.", 51},
    {"ping-packet-count", IPMIPOWER_PING_PACKET_COUNT, "COUNT", 0,
     "Specify the ping packet count size.", 52},
    {"ping-percent", IPMIPOWER_PING_PERCENT, "PERCENT", 0,
     "Specify the ping percent value.", 53},
    {"ping-consec-count", IPMIPOWER_PING_CONSEC_COUNT, "COUNT", 0,
     "Specify the ping consecutive count.", 54},
    {"debug", IPMIPOWER_DEBUG_KEY, 0, 0,
     "Turn on debugging.", 55},
#ifndef NDEBUG
    {"rmcpdump", IPMIPOWER_RMCPDUMP_KEY, 0, 0,
     "Turn on RMCP packet dump output.", 56},
    {"log", IPMIPOWER_LOG_KEY, 0, 0,
     "Turn on logging.", 57},
    {"logfile", IPMIPOWER_LOGFILE_KEY, "FILE", 0,
     "Specify an alternate logfile.", 58},
#endif
    { 0 }
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static char cmdline_args_doc[] = "";

static char cmdline_doc[] = "ipmipower - IPMI power control utility";

static struct argp cmdline_argp = {cmdline_options,
                                   cmdline_parse,
                                   cmdline_args_doc,
                                   cmdline_doc};


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
  conf->k_g_len = 0;
  conf->powercmd = POWER_CMD_NONE;
  memset(conf->configfile, '\0', MAXPATHLEN+1);

  conf->authentication_type = AUTHENTICATION_TYPE_AUTO;
  conf->privilege_level = PRIVILEGE_LEVEL_AUTO;
  conf->ipmi_version = IPMI_VERSION_AUTO;
  conf->cipher_suite_id = CIPHER_SUITE_ID_AUTO;
  conf->on_if_off = IPMIPOWER_FALSE;
  conf->wait_until_on = IPMIPOWER_FALSE;
  conf->wait_until_off = IPMIPOWER_FALSE;
  conf->consolidate_output = IPMIPOWER_FALSE;
  conf->eliminate = IPMIPOWER_FALSE;
  conf->workaround_flags = 0;
#ifndef NDEBUG
  conf->debug = IPMIPOWER_FALSE;
  conf->rmcpdump = IPMIPOWER_FALSE;
  conf->log = IPMIPOWER_FALSE;
  memset(conf->logfile, '\0', MAXPATHLEN+1);
  ipmipower_config_default_logfile(conf->logfile, MAXPATHLEN);
  conf->logfile_fd = -1;
#endif /* NDEBUG */
  conf->session_timeout_len = 20000;     /* 20 seconds */
  conf->retransmission_timeout_len = 400; /* .4 seconds  */
  conf->retransmission_wait_timeout_len = 500; /* .5 seconds  */
  conf->retransmission_backoff_count = 8;
  conf->ping_interval_len = 5000; /* 5 seconds */
  conf->ping_timeout_len = 30000; /* 30 seconds */
  conf->ping_packet_count = 10;
  conf->ping_percent = 50;
  conf->ping_consec_count = 5;

  /* Options not found yet, all false */
  conf->hosts_set_on_cmdline = IPMIPOWER_FALSE;
  conf->username_set_on_cmdline = IPMIPOWER_FALSE;
  conf->password_set_on_cmdline = IPMIPOWER_FALSE;
  conf->authentication_type_set_on_cmdline = IPMIPOWER_FALSE;
  conf->privilege_level_set_on_cmdline = IPMIPOWER_FALSE;
  conf->ipmi_version_set_on_cmdline = IPMIPOWER_FALSE;
  conf->cipher_suite_id_set_on_cmdline = IPMIPOWER_FALSE;
  conf->on_if_off_set_on_cmdline = IPMIPOWER_FALSE;
  conf->wait_until_on_set_on_cmdline = IPMIPOWER_FALSE;
  conf->wait_until_off_set_on_cmdline = IPMIPOWER_FALSE;
  conf->consolidate_output_set_on_cmdline = IPMIPOWER_FALSE;
  conf->eliminate_set_on_cmdline = IPMIPOWER_FALSE;
  conf->workaround_flags_set_on_cmdline = IPMIPOWER_FALSE;
  conf->session_timeout_len_set_on_cmdline = IPMIPOWER_FALSE;
  conf->retransmission_timeout_len_set_on_cmdline = IPMIPOWER_FALSE;
  conf->retransmission_wait_timeout_len_set_on_cmdline = IPMIPOWER_FALSE;
  conf->retransmission_backoff_count_set_on_cmdline = IPMIPOWER_FALSE;
  conf->ping_interval_len_set_on_cmdline = IPMIPOWER_FALSE;
  conf->ping_timeout_len_set_on_cmdline = IPMIPOWER_FALSE;
  conf->ping_packet_count_set_on_cmdline = IPMIPOWER_FALSE;
  conf->ping_percent_set_on_cmdline = IPMIPOWER_FALSE;
  conf->ping_consec_count_set_on_cmdline = IPMIPOWER_FALSE;
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

  if (conf->privilege_level == PRIVILEGE_LEVEL_INVALID)
    err_exit("%s: invalid privilege level", str);

  if (conf->ipmi_version == IPMI_VERSION_INVALID)
    err_exit("%s: invalid ipmi version", str);

  if (conf->cipher_suite_id == CIPHER_SUITE_ID_INVALID)
    err_exit("%s: invalid cipher suite id", str);

  if (conf->session_timeout_len < IPMIPOWER_SESSION_TIMEOUT_MIN 
      || conf->session_timeout_len > IPMIPOWER_SESSION_TIMEOUT_MAX)
    err_exit("%s: timeout out of range", str);
  
  if (conf->retransmission_timeout_len != 0 
      && (conf->retransmission_timeout_len < IPMIPOWER_RETRANSMISSION_TIMEOUT_MIN 
          || conf->retransmission_timeout_len > IPMIPOWER_RETRANSMISSION_TIMEOUT_MAX))
    err_exit("%s: retransmission timeout out of range", str);

  if (conf->retransmission_wait_timeout_len != 0 
      && (conf->retransmission_wait_timeout_len < IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT_MIN 
          || conf->retransmission_wait_timeout_len > IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT_MAX))
    err_exit("%s: retransmission wait timeout out of range", str);
  
  if (conf->retransmission_backoff_count != 0 
      && (conf->retransmission_backoff_count < IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT_MIN 
          || conf->retransmission_backoff_count > IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT_MAX))
    err_exit("%s: retransmission backoff count out of range", str);

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

static error_t
cmdline_parse (int key,
               char *arg,
               struct argp_state *state)
{
  char *ptr;
  char *pw;
  char *kg;
  int rv;
  uint32_t flags;
  int n;

  switch (key) 
    {
    case ARGP_HOSTNAME_KEY:       /* --hostname */
      if ((conf->hosts = hostlist_create(arg)) == NULL)
        err_exit("Error: Hostname(s) incorrectly formatted");
      hostlist_uniq(conf->hosts);
      conf->hosts_count = hostlist_count(conf->hosts);
      conf->hosts_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case ARGP_USERNAME_KEY:       /* --username */
      if (strlen(arg) > IPMI_MAX_USER_NAME_LENGTH)
        err_exit("Command Line Error: username too long");
      strcpy(conf->username, arg);
      conf->username_set_on_cmdline = IPMIPOWER_TRUE;
      n = strlen(arg);
      secure_memset(arg, '\0', n);
      break;
    case ARGP_PASSWORD_KEY:       /* --password */
      if (strlen(arg) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        err_exit("Command Line Error: password too long");
      strcpy(conf->password, arg);
      conf->password_set_on_cmdline = IPMIPOWER_TRUE;
      n = strlen(arg);
      secure_memset(arg, '\0', n);
      break;
    case ARGP_PASSWORD_PROMPT_KEY:       /* --password-prompt */
      if (!(pw = getpass("Password: ")))
        err_exit("getpass: %s", strerror(errno));
      if (strlen(pw) > IPMI_2_0_MAX_PASSWORD_LENGTH)
        err_exit("password too long");
      strcpy(conf->password, pw);
      conf->password_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case ARGP_K_G_KEY:       /* --k-g */
      if ((rv = check_kg_len(arg)) < 0)
        err_exit("Command Line Error: k_g too long");
      if ((rv = parse_kg(conf->k_g, IPMI_MAX_K_G_LENGTH + 1, arg)) < 0)
        err_exit("Command Line Error: k_g input formatted incorrectly");
      if (rv > 0)
        {
          conf->k_g_len = rv;
          conf->k_g_set_on_cmdline = IPMIPOWER_TRUE;
        }
      n = strlen(arg);
      secure_memset(arg, '\0', n);
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
          conf->k_g_set_on_cmdline = IPMIPOWER_TRUE;
        }
      break;
    case IPMIPOWER_ON_KEY:       /* --on */ 
      conf->powercmd = POWER_CMD_POWER_ON;
      break;
    case IPMIPOWER_OFF_KEY:       /* --off */ 
      conf->powercmd = POWER_CMD_POWER_OFF;
      break;
    case IPMIPOWER_CYCLE_KEY:       /* --cycle */ 
      conf->powercmd = POWER_CMD_POWER_CYCLE;
      break;
    case IPMIPOWER_RESET_KEY:       /* --reset */ 
      conf->powercmd = POWER_CMD_POWER_RESET;
      break;
    case IPMIPOWER_STAT_KEY:       /* --stat */ 
      conf->powercmd = POWER_CMD_POWER_STATUS;
      break;
    case IPMIPOWER_PULSE_KEY:       /* --pulse */
      conf->powercmd = POWER_CMD_PULSE_DIAG_INTR;
      break;
    case IPMIPOWER_SOFT_KEY:       /* --soft */
      conf->powercmd = POWER_CMD_SOFT_SHUTDOWN_OS;
      break;
#ifndef NDEBUG
    case IPMIPOWER_CONFIG_KEY:         /* --config */
      if (strlen(arg) > MAXPATHLEN)
        err_exit("Command Line Error: configuration file pathname too long");
      strcpy(conf->configfile, arg);
      break;
#endif /* !NDEBUG */
    case ARGP_AUTHENTICATION_TYPE_KEY:       /* --authentication-type */
      conf->authentication_type = ipmipower_authentication_type_index(arg);
      conf->authentication_type_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    /* ARGP_PRIVILEGE_KEY for backwards compatability */
    case ARGP_PRIVILEGE_KEY:
    case ARGP_PRIVILEGE_LEVEL_KEY:       /* --privilege-level */
      conf->privilege_level = ipmipower_privilege_level_index(arg);
      conf->privilege_level_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_IPMI_VERSION_KEY:	/* --ipmi-version */
      conf->ipmi_version = ipmipower_ipmi_version_index(arg);
      conf->ipmi_version_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case ARGP_CIPHER_SUITE_ID_KEY:       /* --cipher-suite-id */
      conf->cipher_suite_id = ipmipower_cipher_suite_id_index(arg);
      conf->cipher_suite_id_set_on_cmdline = IPMIPOWER_TRUE;
    case IPMIPOWER_ON_IF_OFF_KEY:       /* --on-if-off */
      conf->on_if_off = !conf->on_if_off;
      conf->on_if_off_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_WAIT_UNTIL_OFF_KEY:       /* --wait-until-on */
      conf->wait_until_on = !conf->wait_until_on;
      conf->wait_until_on_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_WAIT_UNTIL_ON_KEY:       /* --wait-until-off */
      conf->wait_until_off = !conf->wait_until_off;
      conf->wait_until_off_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case ARGP_CONSOLIDATE_OUTPUT_KEY:       /* --consolidate-output */
      conf->consolidate_output = IPMIPOWER_TRUE;
      conf->consolidate_output_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case ARGP_ELIMINATE_KEY:       /* --eliminate */
      conf->eliminate = IPMIPOWER_TRUE;
      conf->eliminate_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case ARGP_WORKAROUND_FLAGS_KEY:       /* --workaround-flags */
      if (ipmipower_workarounds_parse(arg, &flags) < 0)
        err_exit("Command Line Error: invalid workaround specified");
      conf->workaround_flags = flags;
      conf->workaround_flags_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_DEBUG_KEY:          /* --debug */
      conf->debug = !conf->debug;
      break;
#ifndef NDEBUG
    case IPMIPOWER_RMCPDUMP_KEY:       /* --rmcpdump */
      conf->rmcpdump = !conf->rmcpdump;
      break;
    case IPMIPOWER_LOG_KEY:            /* --log */
      conf->log = !conf->log;
      break;
    case IPMIPOWER_LOGFILE_KEY:        /* --logfile */
      if (strlen(arg) > MAXPATHLEN)
        err_exit("Command Line Error: log file pathname too long");
      memset(conf->logfile, '\0', MAXPATHLEN+1);
      strcpy(conf->logfile, arg);
      break;
#endif /* !NDEBUG */
    case IPMIPOWER_SESSION_TIMEOUT_KEY:       /* --session-timeout */
      conf->session_timeout_len = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: session timeout length invalid\n");
      conf->session_timeout_len_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_RETRANSMISSION_TIMEOUT_KEY:       /* --retransmission-timeout */
      conf->retransmission_timeout_len = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: retransmission timeout length invalid\n");
      conf->retransmission_timeout_len_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    /* IPMIPOWER_RETRY_WAIT_TIMEOUT for backwards compatability */
    case IPMIPOWER_RETRY_WAIT_TIMEOUT:
    case IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT:       /* --retransmission-wait-timeout */
      conf->retransmission_wait_timeout_len = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: retransmission wait timeout length invalid\n");
      conf->retransmission_wait_timeout_len_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    /* IPMIPOWER_RETRY_BACKOFF_COUNT for backwards compatability */
    case IPMIPOWER_RETRY_BACKOFF_COUNT:
    case IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT:       /* --retransmission-backoff-count */
      conf->retransmission_backoff_count = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: retransmission backoff count invalid\n");
      conf->retransmission_backoff_count_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_PING_INTERVAL:       /* --ping-interval */
      conf->ping_interval_len = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: ping interval length invalid\n");
      conf->ping_interval_len_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_PING_TIMEOUT:       /* --ping-timeout */
      conf->ping_timeout_len = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: ping timeout length invalid\n");
      conf->ping_timeout_len_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_PING_PACKET_COUNT:       /* --ping-packet-count */
      conf->ping_packet_count = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: ping packet count invalid\n");
      conf->ping_packet_count_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_PING_PERCENT:       /* --ping-percent */
      conf->ping_percent = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: ping percent invalid\n");
      conf->ping_percent_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case IPMIPOWER_PING_CONSEC_COUNT:       /* --ping-consec-count */
      conf->ping_consec_count = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg)))
        err_exit("Command Line Error: ping consec count invalid\n");
      conf->ping_consec_count_set_on_cmdline = IPMIPOWER_TRUE;
      break;
    case '?':
    default:
      return ARGP_ERR_UNKNOWN;
    } 

  return 0;
}

static void
post_cmdline_parse_verify(void)
{
  _config_common_checks("Command Line Error");
  
  if (conf->powercmd != POWER_CMD_NONE)
    conf->ping_interval_len = 0;     /* force pings to be off */
}

void 
ipmipower_config_cmdline_parse(int argc, char **argv)
{
  argp_parse(&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, NULL);
  post_cmdline_parse_verify();
}

/*
 * Conffile library callback functions
 */
    
static int 
_cb_hostname(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr, 
             int option_data, void *app_ptr, int app_data) 
{
  int i;
  
  if (conf->hosts_set_on_cmdline == IPMIPOWER_TRUE)
    return 0;
  
  if ((conf->hosts = hostlist_create(NULL)) == NULL)
    err_exit("Config File Error: Hostname(s) incorrectly formatted");
  
  for (i = 0; i < data->stringlist_len; i++) 
    {
      if (hostlist_push(conf->hosts, data->stringlist[i]) == 0)
        err_exit("Config File Error: Hostname(s) incorrectly formatted");
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
  if (conf->authentication_type_set_on_cmdline == IPMIPOWER_TRUE)
      return 0;

  /* Incorrect authentication_type checked in _config_common_checks */
  conf->authentication_type = ipmipower_authentication_type_index(data->string);
  return 0;
}

static int 
_cb_privilege_level(conffile_t cf, struct conffile_data *data,
                    char *optionname, int option_type, void *option_ptr, 
                    int option_data, void *app_ptr, int app_data) 
{
  if (conf->privilege_level_set_on_cmdline == IPMIPOWER_TRUE)
    return 0;

  /* Incorrect privilege level checked in _config_common_checks */
  conf->privilege_level = ipmipower_privilege_level_index(data->string);
  return 0;
}

static int 
_cb_ipmi_version(conffile_t cf, struct conffile_data *data,
		 char *optionname, int option_type, void *option_ptr, 
		 int option_data, void *app_ptr, int app_data) 
{
  if (conf->ipmi_version_set_on_cmdline == IPMIPOWER_TRUE)
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
  if (conf->cipher_suite_id_set_on_cmdline == IPMIPOWER_TRUE)
    return 0;

  /* Incorrect cipher_suite_ids checked in _config_common_checks */
  conf->cipher_suite_id = ipmipower_cipher_suite_id_index(data->string);
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
  if (conf->username_set_on_cmdline == IPMIPOWER_TRUE)
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
  if (conf->password_set_on_cmdline == IPMIPOWER_TRUE)
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
  int rv;

  if (conf->k_g_set_on_cmdline == IPMIPOWER_TRUE)
    return 0;

  if ((rv = check_kg_len(data->string)) < 0)
    err_exit("Command Line Error: k_g too long");

  if ((rv = parse_kg(conf->k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    err_exit("Config File Error: k_g input formatted incorrectly");

  if (rv > 0)
    conf->k_g_len = rv;

  return 0;
}

static int 
_cb_workaround_flags(conffile_t cf, struct conffile_data *data,
                     char *optionname, int option_type, void *option_ptr,
                     int option_data, void *app_ptr, int app_data) 
{
  uint32_t flags;

  if (conf->workaround_flags_set_on_cmdline == IPMIPOWER_TRUE)
    return 0;

  if (ipmipower_workarounds_parse(data->string, &flags) < 0)
    err_exit("Config File Error: invalid workaround specified");
  conf->workaround_flags = flags;
  return 0;
}

void 
ipmipower_config_conffile_parse(char *configfile) 
{
  int hostname_flag, hostnames_flag, username_flag, password_flag, k_g_flag, 
    authentication_type_flag, privilege_flag, privilege_level_flag, cipher_suite_id_backwards_flag, 
    cipher_suite_id_flag, ipmi_version_flag, on_if_off_flag, wait_until_on_flag, wait_until_off_flag, 
    consolidate_output_flag, eliminate_flag, workaround_flags_flag, timeout_flag, session_timeout_flag, 
    retry_timeout_flag, retransmission_timeout_flag, retry_wait_timeout_flag, 
    retransmission_wait_timeout_flag, retry_backoff_count_flag, retransmission_backoff_count_flag, 
    ping_interval_flag, ping_timeout_flag, ping_packet_count_flag, ping_percent_flag, 
    ping_consec_count_flag;

  struct conffile_option options[] = 
    {
      /* hostnames (plural) maintained for backwards compatability */
      {"hostnames", CONFFILE_OPTION_LIST_STRING, -1, _cb_hostname, 
       1, 0, &hostnames_flag, NULL, 0},
      {"hostname", CONFFILE_OPTION_LIST_STRING, -1, _cb_hostname, 
       1, 0, &hostname_flag, NULL, 0},
      {"username", CONFFILE_OPTION_STRING, -1, _cb_username,
       1, 0, &username_flag, NULL, 0},
      {"password", CONFFILE_OPTION_STRING, -1, _cb_password, 
       1, 0, &password_flag, NULL, 0},
      {"k_g", CONFFILE_OPTION_STRING, -1, _cb_k_g, 
       1, 0, &k_g_flag, NULL, 0},
      {"authentication-type", CONFFILE_OPTION_STRING, -1, _cb_authentication_type, 
       1, 0, &authentication_type_flag, NULL, 0},
      /* "privilege" maintained for backwards compatability */
      {"privilege", CONFFILE_OPTION_STRING, -1, _cb_privilege_level, 
       1, 0, &privilege_flag, NULL, 0},
      {"privilege-level", CONFFILE_OPTION_STRING, -1, _cb_privilege_level, 
       1, 0, &privilege_level_flag, NULL, 0},
      {"ipmi-version", CONFFILE_OPTION_STRING, -1, _cb_ipmi_version,
       1, 0, &ipmi_version_flag, NULL, 0},
      /* cipher suite id w/ underscores maintained for backwards compatability */
      {"cipher_suite_id", CONFFILE_OPTION_STRING, -1, _cb_cipher_suite_id,
       1, 0, &cipher_suite_id_backwards_flag, NULL, 0},
      {"cipher-suite-id", CONFFILE_OPTION_STRING, -1, _cb_cipher_suite_id,
       1, 0, &cipher_suite_id_flag, NULL, 0},
      {"on-if-off", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &on_if_off_flag, &(conf->on_if_off), 
       conf->on_if_off_set_on_cmdline},
      {"wait-until-on", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &wait_until_on_flag, &(conf->wait_until_on), 
       conf->wait_until_on_set_on_cmdline},
      {"wait-until-off", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &wait_until_off_flag, &(conf->wait_until_off), 
       conf->wait_until_off_set_on_cmdline},
      {"consolidate-output", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &consolidate_output_flag, NULL, 0},
      {"eliminate", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &eliminate_flag, NULL, 0},
      {"workaround-flags", CONFFILE_OPTION_STRING, -1, _cb_workaround_flags,
       1, 0, &workaround_flags_flag, NULL, 0},
      /* timeout maintained for backwards compatability */
      {"timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &timeout_flag, &(conf->session_timeout_len), 
       conf->session_timeout_len_set_on_cmdline},
      {"session-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &session_timeout_flag, &(conf->session_timeout_len), 
       conf->session_timeout_len_set_on_cmdline},
      /* retry-timeout for backwards comptability */
      {"retry-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &retry_timeout_flag, &(conf->retransmission_timeout_len), 
       conf->retransmission_timeout_len_set_on_cmdline},
      {"retransmission-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &retransmission_timeout_flag, &(conf->retransmission_timeout_len), 
       conf->retransmission_timeout_len_set_on_cmdline},
      /* retry-wait-timeout for backwards comptability */
      {"retry-wait-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &retry_wait_timeout_flag, &(conf->retransmission_wait_timeout_len), 
       conf->retransmission_wait_timeout_len_set_on_cmdline},
      {"retransmission-wait-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &retransmission_wait_timeout_flag, &(conf->retransmission_wait_timeout_len), 
       conf->retransmission_wait_timeout_len_set_on_cmdline},
      /* retry-backoff-count for backwards compatability */
      {"retry-backoff-count", CONFFILE_OPTION_INT, -1, _cb_int,
       1, 0, &retry_backoff_count_flag, &(conf->retransmission_backoff_count), 
       conf->retransmission_backoff_count_set_on_cmdline},
      {"retransmission-backoff-count", CONFFILE_OPTION_INT, -1, _cb_int,
       1, 0, &retransmission_backoff_count_flag, &(conf->retransmission_backoff_count), 
       conf->retransmission_backoff_count_set_on_cmdline},
      {"ping-interval", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_interval_flag, &(conf->ping_interval_len), 
       conf->ping_interval_len_set_on_cmdline},
      {"ping-timeout", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_timeout_flag, &(conf->ping_timeout_len), 
       conf->ping_timeout_len_set_on_cmdline},
      {"ping-packet-count", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_packet_count_flag, &(conf->ping_packet_count), 
       conf->ping_packet_count_set_on_cmdline},
      {"ping-percent", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_percent_flag, &(conf->ping_percent), 
       conf->ping_percent_set_on_cmdline},
      {"ping-consec-count", CONFFILE_OPTION_INT, -1, _cb_int, 
       1, 0, &ping_consec_count_flag, &(conf->ping_consec_count), 
       conf->ping_consec_count_set_on_cmdline},
    };
  conffile_t cf = NULL;
  char *conffile = NULL;
  int num;

  if ((cf = conffile_handle_create()) == NULL)
    err_exit("Config File Error: cannot create conffile handle");

  conffile = (strlen(configfile)) ? configfile : IPMIPOWER_CONFIG_FILE_DEFAULT;
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
  if (conf->retransmission_timeout_len > conf->session_timeout_len)
    err_exit("Error: Session timeout length must be longer than retransmission  timeout length");
  
  if (conf->ping_interval_len > conf->ping_timeout_len)
    err_exit("Error: Ping timeout interval length must be "
             "longer than ping interval length");

  if (conf->ping_consec_count > conf->ping_packet_count)
    err_exit("Error: Ping consec count must be larger than ping packet count");

  if (conf->powercmd != POWER_CMD_NONE && conf->hosts == NULL)
    err_exit("Error: Must specify target hostname(s) in non-interactive mode");

  if (conf->authentication_type == AUTHENTICATION_TYPE_NONE 
      && strlen(conf->password) > 0)
    err_exit("Error: password cannot be set for authentication type \"%s\"",
             ipmipower_authentication_type_string(conf->authentication_type));

  if (conf->ipmi_version != IPMI_VERSION_AUTO
      && conf->ipmi_version != IPMI_VERSION_2_0
      && conf->k_g_len)
    err_exit("Error: k_g is only used for IPMI 2.0");

  if (conf->ipmi_version == IPMI_VERSION_1_5
      && strlen(conf->password) >= IPMI_1_5_MAX_PASSWORD_LENGTH)
    err_exit("Error: password too long");
}
