/*****************************************************************************\
 *  $Id: ipmipower_config.c,v 1.117 2008-05-19 18:47:18 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
#include "ipmipower_output.h"
#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"

#include "secure.h"
#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
      
extern struct ipmipower_arguments args;
extern struct ipmipower_connection *ics;

const char *argp_program_version = "ipmipower " VERSION "\n";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    /* maintain "ipmi-version" for backwards compatability */
    {"ipmi-version", IPMI_VERSION_KEY, "IPMIVERSION", OPTION_HIDDEN,
     "Specify the IPMI protocol version to use.", 11},
    ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED,
    /* maintain legacy short options to timeout/session-timeout */    
    {"bogus-long-option1", SESSION_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,
     "Specify the session timeout in milliseconds.", 12},
    /* maintain legacy short options to retry-timeout/retransmission-timeout */
    {"bogus-long-option2", RETRANSMISSION_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,
     "Specify the packet retransmission timeout in milliseconds.", 11},
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_OPERATOR,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"config", CONFIG_KEY, "FILE", 0,
     "Specify an alternate configuration file.", 26},
#ifndef NDEBUG
    {"rmcpdump", RMCPDUMP_KEY, 0, 0,
     "Turn on RMCP packet dump output.", 27},
#endif
    {"on", ON_KEY, 0, 0,
     "Power on the target hosts.", 30},
    {"off", OFF_KEY, 0, 0,
     "Power off the target hosts.", 31},
    {"cycle", CYCLE_KEY, 0, 0,
     "Power cycle the target hosts.", 32},
    {"reset", RESET_KEY, 0, 0,
     "Reset the target hosts.", 33},
    {"stat", STAT_KEY, 0, 0,
     "Get power status of the target hosts.", 34},
    {"pulse", PULSE_KEY, 0, 0,
     "Send power diagnostic interrupt to target hosts.", 35},
    {"soft", SOFT_KEY, 0, 0,
     "Initiate a soft-shutdown of the OS via ACPI.", 36},
    {"on-if-off", ON_IF_OFF_KEY, 0, 0,
     "Issue a power on command instead of a power cycle or hard reset "
     "command if the remote machine's power is currently off.", 38},
    {"wait-until-off", WAIT_UNTIL_OFF_KEY, 0, 0,
     "Regularly query the remote BMC and return only after the machine has powered off.", 39},
    {"wait-until-on", WAIT_UNTIL_ON_KEY, 0, 0,
     "Regularly query the remote BMC and return only after the machine has powered on.", 40},
    /* retry-wait-timeout maintained for backwards comptability */
    {"retry-wait-timeout", RETRY_WAIT_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,
     "Specify the retransmission timeout length in milliseconds.", 41},
    {"retransmission-wait-timeout", RETRANSMISSION_WAIT_TIMEOUT_KEY, "MILLISECONDS", 0,
     "Specify the retransmission timeout length in milliseconds.", 41},
    /* retry-backoff-count maintained for backwards comptability */
    {"retry-backoff-count", RETRY_BACKOFF_COUNT_KEY, "COUNT", OPTION_HIDDEN,
     "Specify the retransmission backoff count for retransmissions.", 42},
    {"retransmission-backoff-count", RETRANSMISSION_BACKOFF_COUNT_KEY, "COUNT", 0,
     "Specify the retransmission backoff count for retransmissions.", 42},
    {"ping-interval", PING_INTERVAL_KEY, "MILLISECONDS", 0,
     "Specify the ping interval length in milliseconds.", 43},
    {"ping-timeout", PING_TIMEOUT_KEY, "MILLISECONDS", 0,
     "Specify the ping timeout length in milliseconds.", 44},
    {"ping-packet-count", PING_PACKET_COUNT_KEY, "COUNT", 0,
     "Specify the ping packet count size.", 45},
    {"ping-percent", PING_PERCENT_KEY, "PERCENT", 0,
     "Specify the ping percent value.", 46},
    {"ping-consec-count", PING_CONSEC_COUNT_KEY, "COUNT", 0,
     "Specify the ping consecutive count.", 47},
    { 0 }
  };

static error_t cmdline_parse_config (int key, char *arg, struct argp_state *state);

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static char cmdline_args_doc[] = "";

static char cmdline_doc[] = "ipmipower - IPMI power control utility";

static struct argp cmdline_argp = {cmdline_options,
                                   cmdline_parse,
                                   cmdline_args_doc,
                                   cmdline_doc};

static struct argp cmdline_argp_config = {cmdline_options,
                                          cmdline_parse_config,
                                          cmdline_args_doc,
                                          cmdline_doc};

static error_t
cmdline_parse_config (int key,
                      char *arg,
                      struct argp_state *state)
{
  switch (key) 
    {
    case CONFIG_KEY:         /* --config */
      if (strlen(arg) > MAXPATHLEN)
        ierr_exit("Command Line Error: configuration file pathname too long");
      if (!(args.configfile = strdup(arg)))
        {
          perror("strdup");
          exit(1);
        }
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
cmdline_parse (int key,
               char *arg,
               struct argp_state *state)
{
  char *ptr;
  error_t ret;
  int tmp;

  switch (key) 
    {
    /* IPMI_VERSION_KEY for backwards compatability */
    case IPMI_VERSION_KEY:	/* --ipmi-version */
      if (!strcasecmp(arg, "1.5"))
        tmp = IPMI_DEVICE_LAN;
      else if (!strcasecmp(arg, "2.0"))
        tmp = IPMI_DEVICE_LAN_2_0;
      else
        ierr_exit("Command Line Error: invalid driver type specified");
      args.common.driver_type = tmp;
      break;
    case CONFIG_KEY:         /* --config */
      /* ignore */
      break;
#ifndef NDEBUG
    case RMCPDUMP_KEY:       /* --rmcpdump */
      args.rmcpdump++;
      break;
#endif /* !NDEBUG */
    case ON_KEY:       /* --on */ 
      args.powercmd = POWER_CMD_POWER_ON;
      break;
    case OFF_KEY:       /* --off */ 
      args.powercmd = POWER_CMD_POWER_OFF;
      break;
    case CYCLE_KEY:       /* --cycle */ 
      args.powercmd = POWER_CMD_POWER_CYCLE;
      break;
    case RESET_KEY:       /* --reset */ 
      args.powercmd = POWER_CMD_POWER_RESET;
      break;
    case STAT_KEY:       /* --stat */ 
      args.powercmd = POWER_CMD_POWER_STATUS;
      break;
    case PULSE_KEY:       /* --pulse */
      args.powercmd = POWER_CMD_PULSE_DIAG_INTR;
      break;
    case SOFT_KEY:       /* --soft */
      args.powercmd = POWER_CMD_SOFT_SHUTDOWN_OS;
      break;
    case ON_IF_OFF_KEY:       /* --on-if-off */
      args.on_if_off++;
      break;
    case WAIT_UNTIL_OFF_KEY:       /* --wait-until-on */
      args.wait_until_on++;
      break;
    case WAIT_UNTIL_ON_KEY:       /* --wait-until-off */
      args.wait_until_off++;
      break;
      /* RETRY_WAIT_TIMEOUT for backwards compatability */
    case RETRY_WAIT_TIMEOUT_KEY:
    case RETRANSMISSION_WAIT_TIMEOUT_KEY:       /* --retransmission-wait-timeout */
      tmp = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg))
          || tmp <= 0)
        ierr_exit("Command Line Error: retransmission wait timeout length invalid");
      args.retransmission_wait_timeout = tmp;
      break;
      /* RETRY_BACKOFF_COUNT for backwards compatability */
    case RETRY_BACKOFF_COUNT_KEY:
    case RETRANSMISSION_BACKOFF_COUNT_KEY:       /* --retransmission-backoff-count */
      tmp = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg))
          || tmp <= 0)
        ierr_exit("Command Line Error: retransmission backoff count invalid");
      args.retransmission_backoff_count = tmp;
      break;
    case PING_INTERVAL_KEY:       /* --ping-interval */
      tmp = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg))
          || tmp < 0)
        ierr_exit("Command Line Error: ping interval length invalid");
      args.ping_interval = tmp;
      break;
    case PING_TIMEOUT_KEY:       /* --ping-timeout */
      tmp = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg))
          || tmp < 0)
        ierr_exit("Command Line Error: ping timeout length invalid");
      args.ping_timeout = tmp;
      break;
    case PING_PACKET_COUNT_KEY:       /* --ping-packet-count */
      tmp = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg))
          || tmp < 0)
        ierr_exit("Command Line Error: ping packet count invalid");
      args.ping_packet_count = tmp;
      break;
    case PING_PERCENT_KEY:       /* --ping-percent */
      tmp = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg))
          || tmp < 0)
        ierr_exit("Command Line Error: ping percent invalid");
      args.ping_percent = tmp;
      break;
    case PING_CONSEC_COUNT_KEY:       /* --ping-consec-count */
      tmp = strtol(arg, &ptr, 10);
      if (ptr != (arg + strlen(arg))
          || tmp < 0)
        ierr_exit("Command Line Error: ping consec count invalid");
      args.ping_consec_count = tmp;
      break;
    default:
      ret = common_parse_opt (key, arg, state, &(args.common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(args.hostrange));
      return ret;
    } 

  return 0;
}

/*
 * Conffile library callback functions
 */
    
static int 
_cb_driver_type(conffile_t cf, struct conffile_data *data,
                char *optionname, int option_type, void *option_ptr, 
                int option_data, void *app_ptr, int app_data) 
{
  int tmp;

  if ((tmp = parse_outofband_driver_type(data->string)) < 0)
    ierr_exit("Config File Error: invalid driver type specified");

  args.common.driver_type = tmp;
  return 0;
}

static int 
_cb_hostname(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr, 
             int option_data, void *app_ptr, int app_data) 
{
  char buffer[IPMIPOWER_OUTPUT_BUFLEN];
  hostlist_t hl = NULL;
  int rv;
  int i;

  if (!(hl = hostlist_create(NULL)))
    ierr_exit("hostlist_create: %s", strerror(errno));
  
  for (i = 0; i < data->stringlist_len; i++) 
    {
      if (!hostlist_push(hl, data->stringlist[i]))
        ierr_exit("Config File Error: Hostname(s) incorrectly formatted");
    }

  hostlist_uniq(hl);

  if ((rv = hostlist_ranged_string(hl, IPMIPOWER_OUTPUT_BUFLEN, buffer) < 0))
    ierr_exit("Config File Error: Hostname(s) incorrectly formatted");
    
  if (rv > 0)
    {
      if (!(args.common.hostname = strdup(buffer)))
        {
          perror("strdup");
          exit(1);
        }
    }

  if (hl)
    hostlist_destroy(hl);
  return 0;
}

static int 
_cb_username(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr,
             int option_data, void *app_ptr, int app_data) 
{
  if (strlen(data->string) > IPMI_MAX_USER_NAME_LENGTH)
    ierr_exit("Config File Error: username too long");

  if (!(args.common.username = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }
  return 0;
}

static int 
_cb_password(conffile_t cf, struct conffile_data *data,
             char *optionname, int option_type, void *option_ptr,
             int option_data, void *app_ptr, int app_data) 
{
  if (strlen(data->string) > IPMI_2_0_MAX_PASSWORD_LENGTH)
    ierr_exit("Config File Error: password too long");

  if (!(args.common.password = strdup(data->string)))
    {
      perror("strdup");
      exit(1);
    }
  return 0;
}

static int 
_cb_k_g(conffile_t cf, struct conffile_data *data,
        char *optionname, int option_type, void *option_ptr,
        int option_data, void *app_ptr, int app_data) 
{
  int rv;

  if ((rv = check_kg_len(data->string)) < 0)
    ierr_exit("Command Line Error: k_g too long");

  if ((rv = parse_kg(args.common.k_g, IPMI_MAX_K_G_LENGTH + 1, data->string)) < 0)
    ierr_exit("Config File Error: k_g input formatted incorrectly");

  if (rv > 0)
    args.common.k_g_len = rv;

  return 0;
}

static int 
_cb_authentication_type(conffile_t cf, struct conffile_data *data,
			char *optionname, int option_type, void *option_ptr, 
			int option_data, void *app_ptr, int app_data) 
{
  int tmp;

  if ((tmp = parse_authentication_type(data->string)) < 0)
    ierr_exit("Config File Error: invalid authentication type specified");

  args.common.authentication_type = tmp;
  return 0;
}

static int 
_cb_cipher_suite_id(conffile_t cf, struct conffile_data *data,
                    char *optionname, int option_type, void *option_ptr, 
                    int option_data, void *app_ptr, int app_data) 
{
  if (data->intval < IPMI_CIPHER_SUITE_ID_MIN
      || data->intval > IPMI_CIPHER_SUITE_ID_MAX)
    ierr_exit("Config File Error: invalid cipher suite id");
  if (!IPMI_CIPHER_SUITE_ID_SUPPORTED(data->intval))
    ierr_exit("Config File Error: unsupported cipher suite id");
  args.common.cipher_suite_id = data->intval;
  return 0;
}

static int 
_cb_privilege_level(conffile_t cf, struct conffile_data *data,
                    char *optionname, int option_type, void *option_ptr, 
                    int option_data, void *app_ptr, int app_data) 
{
  int tmp;
  
  if ((tmp = parse_privilege_level(data->string)) < 0)
    ierr_exit("Config File Error: invalid privilege level specified");

  args.common.privilege_level = tmp;
  return 0;
}

static int 
_cb_workaround_flags(conffile_t cf, struct conffile_data *data,
                     char *optionname, int option_type, void *option_ptr,
                     int option_data, void *app_ptr, int app_data) 
{
  int tmp;

  if ((tmp = parse_workaround_flags(data->string)) < 0)
    ierr_exit("Config File Error: invalid workaround flags specified");
  args.common.workaround_flags = tmp;
  return 0;
}

static int 
_cb_fanout(conffile_t cf, struct conffile_data *data,
                    char *optionname, int option_type, void *option_ptr, 
                    int option_data, void *app_ptr, int app_data) 
{
  if (data->intval < PSTDOUT_FANOUT_MIN
      || data->intval > PSTDOUT_FANOUT_MAX)
    ierr_exit("Config File Error: invalid fanout");
  args.hostrange.fanout = data->intval;
  return 0;
}

static int 
_cb_bool(conffile_t cf, struct conffile_data *data,
         char *optionname, int option_type, void *option_ptr,
         int option_data, void *app_ptr, int app_data) 
{
  int *boolval = (int *)option_ptr;
  int cmdlineset = (int)option_data;

  if (cmdlineset)
    return 0;

  *boolval = data->boolval;
  return 0;
}

static int 
_cb_unsigned_int_non_zero(conffile_t cf, struct conffile_data *data,
                          char *optionname, int option_type, void *option_ptr,
                          int option_data, void *app_ptr, int app_data) 
{
  unsigned int *temp = (unsigned int *)option_ptr;
  int cmdlineset = (int)option_data;

  if (cmdlineset)
    return 0;

  if (data->intval <= 0)
    ierr_exit("Config File Error: %s value invalid", optionname);

  *temp = data->intval; 
  return 0;
}

static int 
_cb_unsigned_int(conffile_t cf, struct conffile_data *data,
                 char *optionname, int option_type, void *option_ptr,
                 int option_data, void *app_ptr, int app_data) 
{
  unsigned int *temp = (unsigned int *)option_ptr;
  int cmdlineset = (int)option_data;

  if (cmdlineset)
    return 0;

  if (data->intval < 0)
    ierr_exit("Config File Error: %s value invalid", optionname);

  *temp = data->intval; 
  return 0;
}

void 
ipmipower_config_conffile_parse(char *configfile) 
{
  int driver_type_flag,
    ipmi_version_flag, 
    hostname_flag, 
    hostnames_flag, 
    username_flag, 
    password_flag, 
    k_g_flag, 
    timeout_flag, 
    session_timeout_flag, 
    retry_timeout_flag,
    retransmission_timeout_flag, 
    authentication_type_flag, 
    cipher_suite_id_backwards_flag, 
    cipher_suite_id_flag, 
    privilege_flag, 
    privilege_level_flag, 
    workaround_flags_flag, 
    buffer_output_flag, 
    consolidate_output_flag,
    fanout_flag,
    eliminate_flag, 
    always_prefix_flag,
    on_if_off_flag, 
    wait_until_on_flag,
    wait_until_off_flag, 
    retry_wait_timeout_flag, 
    retransmission_wait_timeout_flag, 
    retry_backoff_count_flag, 
    retransmission_backoff_count_flag, 
    ping_interval_flag, 
    ping_timeout_flag,
    ping_packet_count_flag,
    ping_percent_flag, 
    ping_consec_count_flag;

  struct conffile_option options[] = 
    {
      {"driver-type", CONFFILE_OPTION_STRING, -1, _cb_driver_type,
       1, 0, &driver_type_flag, NULL, 0},
      /* ipmi-version maintained for backwards compatability */
      {"ipmi-version", CONFFILE_OPTION_STRING, -1, _cb_driver_type,
       1, 0, &ipmi_version_flag, NULL, 0},
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
      /* timeout maintained for backwards compatability */
      {"timeout", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero, 
       1, 0, &timeout_flag, &(args.common.session_timeout), 
       0},
      {"session-timeout", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero, 
       1, 0, &session_timeout_flag, &(args.common.session_timeout), 
       0},
      /* retry-timeout for backwards comptability */
      {"retry-timeout", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero, 
       1, 0, &retry_timeout_flag, &(args.common.retransmission_timeout), 
       0},
      {"retransmission-timeout", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero, 
       1, 0, &retransmission_timeout_flag, &(args.common.retransmission_timeout), 
       0},
      {"authentication-type", CONFFILE_OPTION_STRING, -1, _cb_authentication_type, 
       1, 0, &authentication_type_flag, NULL, 0},
      /* cipher suite id w/ underscores maintained for backwards compatability */
      {"cipher_suite_id", CONFFILE_OPTION_STRING, -1, _cb_cipher_suite_id,
       1, 0, &cipher_suite_id_backwards_flag, NULL, 0},
      {"cipher-suite-id", CONFFILE_OPTION_STRING, -1, _cb_cipher_suite_id,
       1, 0, &cipher_suite_id_flag, NULL, 0},
      /* "privilege" maintained for backwards compatability */
      {"privilege", CONFFILE_OPTION_STRING, -1, _cb_privilege_level, 
       1, 0, &privilege_flag, NULL, 0},
      {"privilege-level", CONFFILE_OPTION_STRING, -1, _cb_privilege_level, 
       1, 0, &privilege_level_flag, NULL, 0},
      {"workaround-flags", CONFFILE_OPTION_STRING, -1, _cb_workaround_flags,
       1, 0, &workaround_flags_flag, NULL, 0},
      {"buffer-output", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &buffer_output_flag, NULL, 0},
      {"consolidate-output", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &consolidate_output_flag, NULL, 0},
      {"fanout", CONFFILE_OPTION_INT, -1, _cb_fanout,
       1, 0, &fanout_flag, &(args.hostrange.fanout),
       0},
      {"eliminate", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &eliminate_flag, NULL, 0},
      {"always_prefix", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &always_prefix_flag, NULL, 0},
      {"on-if-off", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &on_if_off_flag, &(args.on_if_off), 
       0},
      {"wait-until-on", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &wait_until_on_flag, &(args.wait_until_on), 
       0},
      {"wait-until-off", CONFFILE_OPTION_BOOL, -1, _cb_bool,
       1, 0, &wait_until_off_flag, &(args.wait_until_off), 
       0},
      /* retry-wait-timeout for backwards comptability */
      {"retry-wait-timeout", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero, 
       1, 0, &retry_wait_timeout_flag, &(args.retransmission_wait_timeout), 
       0},
      {"retransmission-wait-timeout", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero, 
       1, 0, &retransmission_wait_timeout_flag, &(args.retransmission_wait_timeout), 
       0},
      /* retry-backoff-count for backwards compatability */
      {"retry-backoff-count", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero,
       1, 0, &retry_backoff_count_flag, &(args.retransmission_backoff_count), 
       0},
      {"retransmission-backoff-count", CONFFILE_OPTION_INT, -1, _cb_unsigned_int_non_zero,
       1, 0, &retransmission_backoff_count_flag, &(args.retransmission_backoff_count), 
       0},
      {"ping-interval", CONFFILE_OPTION_INT, -1, _cb_unsigned_int, 
       1, 0, &ping_interval_flag, &(args.ping_interval), 
       0},
      {"ping-timeout", CONFFILE_OPTION_INT, -1, _cb_unsigned_int, 
       1, 0, &ping_timeout_flag, &(args.ping_timeout), 
       0},
      {"ping-packet-count", CONFFILE_OPTION_INT, -1, _cb_unsigned_int, 
       1, 0, &ping_packet_count_flag, &(args.ping_packet_count), 
       0},
      {"ping-percent", CONFFILE_OPTION_INT, -1, _cb_unsigned_int, 
       1, 0, &ping_percent_flag, &(args.ping_percent), 
       0},
      {"ping-consec-count", CONFFILE_OPTION_INT, -1, _cb_unsigned_int, 
       1, 0, &ping_consec_count_flag, &(args.ping_consec_count), 
       0},
    };
  conffile_t cf = NULL;
  char *conffile = NULL;
  int num;

  if (!(cf = conffile_handle_create()))
    ierr_exit("Config File Error: cannot create conffile handle");

  conffile = (configfile) ? configfile : IPMIPOWER_CONFIG_FILE_DEFAULT;
  num = sizeof(options)/sizeof(struct conffile_option);
  if (conffile_parse(cf, conffile, options, num, NULL, 0, 0) < 0) 
    {
      char errbuf[CONFFILE_MAX_ERRMSGLEN];
      
      /* Not an error if default file doesn't exist */ 
      if (!configfile && conffile_errnum(cf) == CONFFILE_ERR_EXIST)
        goto done;
      
      if (conffile_errmsg(cf, errbuf, CONFFILE_MAX_ERRMSGLEN) < 0)
        ierr_exit("Config File Error: Cannot retrieve conffile error message");
      
      ierr_exit("Config File Error: %s", errbuf);
    }

 done:
  (void)conffile_handle_destroy(cf);
  return;
}

void 
ipmipower_config_check_values(void) 
{
  if (args.common.driver_type == IPMI_DEVICE_LAN
      && args.common.password
      && strlen(args.common.password) > IPMI_1_5_MAX_PASSWORD_LENGTH)
    ierr_exit("Error: password too long");

  if (args.common.retransmission_timeout > args.common.session_timeout)
    ierr_exit("Error: Session timeout length must be longer than retransmission timeout length");

  if (args.retransmission_wait_timeout > args.common.session_timeout)
    ierr_exit("Error: Session timeout length must be longer than retransmission wait timeout length");
  
  if (args.powercmd != POWER_CMD_NONE && !args.common.hostname)
    ierr_exit("Error: Must specify target hostname(s) in non-interactive mode");

  if (args.ping_interval > args.ping_timeout)
    ierr_exit("Error: Ping timeout interval length must be "
              "longer than ping interval length");

  if (args.ping_consec_count > args.ping_packet_count)
    ierr_exit("Error: Ping consec count must be larger than ping packet count");
}

void
ipmipower_config(int argc, char **argv)
{
  init_common_cmd_args_operator (&(args.common));
  init_hostrange_cmd_args (&(args.hostrange));

  /* ipmipower differences */
  args.common.driver_type = IPMI_DEVICE_LAN;
  args.common.driver_type_outofband_only = 1;
  args.common.session_timeout = 20000; /* 20 seconds */
  args.common.retransmission_timeout = 400; /* .4 seconds */

  args.configfile = NULL;
#ifndef NDEBUG
  args.rmcpdump = 0;
#endif /* NDEBUG */

  args.powercmd = POWER_CMD_NONE;
  args.on_if_off = 0;
  args.wait_until_on = 0;
  args.wait_until_off = 0;
  args.retransmission_wait_timeout = 500; /* .5 seconds  */
  args.retransmission_backoff_count = 8;
  args.ping_interval = 5000; /* 5 seconds */
  args.ping_timeout = 30000; /* 30 seconds */
  args.ping_packet_count = 10;
  args.ping_percent = 50;
  args.ping_consec_count = 5;

  argp_parse(&cmdline_argp_config, argc, argv, ARGP_IN_ORDER, NULL, NULL);

  ipmipower_config_conffile_parse(args.configfile);

  argp_parse(&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, NULL);
  /* achu: don't do these checks, we don't do inband, so checks aren't appropriate 
   * checks will be done in ipmipower_config_check_values().
   */
  /* verify_common_cmd_args (&(args.common)); */
  verify_hostrange_cmd_args (&(args.hostrange));
  ipmipower_config_check_values();
}
