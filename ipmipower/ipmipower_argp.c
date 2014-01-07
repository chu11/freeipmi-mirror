/*****************************************************************************\
 *  $Id: ipmipower_argp.c,v 1.26 2010-07-13 22:09:52 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmipower_argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

extern struct ipmipower_connection *ics;

const char *argp_program_version =
  "ipmipower - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.\n"
  "Copyright (C) 2003-2007 The Regents of the University of California.\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmipower - IPMI power control utility";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    /* maintain "ipmi-version" for backwards compatability */
    { "ipmi-version", IPMI_VERSION_KEY, "IPMIVERSION", OPTION_HIDDEN,
      "Specify the IPMI protocol version to use.", 11},
    ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED,
    /* removed legacy short options */
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "on", ON_KEY, 0, 0,
      "Power on the target hosts.", 40},
    { "off", OFF_KEY, 0, 0,
      "Power off the target hosts.", 41},
    { "cycle", CYCLE_KEY, 0, 0,
      "Power cycle the target hosts.", 42},
    { "reset", RESET_KEY, 0, 0,
      "Reset the target hosts.", 43},
    { "stat", STAT_KEY, 0, 0,
      "Get power status of the target hosts.", 44},
    { "pulse", PULSE_KEY, 0, 0,
      "Send power diagnostic interrupt to target hosts.", 45},
    { "soft", SOFT_KEY, 0, 0,
      "Initiate a soft-shutdown of the OS via ACPI.", 46},
    { "on-if-off", ON_IF_OFF_KEY, 0, 0,
      "Issue a power on command instead of a power cycle or hard reset "
      "command if the remote machine's power is currently off.", 48},
    { "wait-until-off", WAIT_UNTIL_OFF_KEY, 0, 0,
      "Regularly query the remote BMC and return only after the machine has powered off.", 49},
    { "wait-until-on", WAIT_UNTIL_ON_KEY, 0, 0,
      "Regularly query the remote BMC and return only after the machine has powered on.", 50},
    { "oem-power-type", OEM_POWER_TYPE_KEY, "OEM-POWER-TYPE", 0,
      "Specify an OEM power type to be used.", 51},
    /* retry-wait-timeout maintained for backwards comptability */
    { "retry-wait-timeout", RETRY_WAIT_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,
      "Specify the retransmission timeout length in milliseconds.", 52},
    { "retransmission-wait-timeout", RETRANSMISSION_WAIT_TIMEOUT_KEY, "MILLISECONDS", 0,
      "Specify the retransmission timeout length in milliseconds.", 52},
    /* retry-backoff-count maintained for backwards comptability */
    { "retry-backoff-count", RETRY_BACKOFF_COUNT_KEY, "COUNT", OPTION_HIDDEN,
      "Specify the retransmission backoff count for retransmissions.", 53},
    { "retransmission-backoff-count", RETRANSMISSION_BACKOFF_COUNT_KEY, "COUNT", 0,
      "Specify the retransmission backoff count for retransmissions.", 53},
    { "ping-interval", PING_INTERVAL_KEY, "MILLISECONDS", 0,
      "Specify the ping interval length in milliseconds.", 54},
    { "ping-timeout", PING_TIMEOUT_KEY, "MILLISECONDS", 0,
      "Specify the ping timeout length in milliseconds.", 55},
    { "ping-packet-count", PING_PACKET_COUNT_KEY, "COUNT", 0,
      "Specify the ping packet count size.", 56},
    { "ping-percent", PING_PERCENT_KEY, "PERCENT", 0,
      "Specify the ping percent value.", 57},
    { "ping-consec-count", PING_CONSEC_COUNT_KEY, "COUNT", 0,
      "Specify the ping consecutive count.", 58},
#ifndef NDEBUG
    { "rmcpdump", RMCPDUMP_KEY, 0, 0,
      "Turn on RMCP packet dump output.", 59},
#endif
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
                                                cmdline_doc};

void _parse_oem_power_type (struct ipmipower_arguments *cmd_args, const char *oem_power_type_str)
{      
  assert (cmd_args);
  assert (oem_power_type_str);

  if (!strcasecmp (oem_power_type_str, IPMIPOWER_OEM_POWER_TYPE_NONE_STR))
    cmd_args->oem_power_type = IPMIPOWER_OEM_POWER_TYPE_NONE;
  else if (!strcasecmp (oem_power_type_str, IPMIPOWER_OEM_POWER_TYPE_C410X_STR))
    cmd_args->oem_power_type = IPMIPOWER_OEM_POWER_TYPE_C410X;
  else
    cmd_args->oem_power_type = IPMIPOWER_OEM_POWER_TYPE_INVALID;
}

static error_t
cmdline_parse (int key,
               char *arg,
               struct argp_state *state)
{
  struct ipmipower_arguments *cmd_args;
  char *endptr;
  int tmp = 0;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
      /* IPMI_VERSION_KEY for backwards compatability */
    case IPMI_VERSION_KEY:      /* --ipmi-version */
      if (!strcasecmp (arg, "1.5"))
        tmp = IPMI_DEVICE_LAN;
      else if (!strcasecmp (arg, "2.0"))
        tmp = IPMI_DEVICE_LAN_2_0;
      else
        {
          fprintf (stderr, "invalid driver type specified");
          exit (EXIT_FAILURE);
        }
      cmd_args->common_args.driver_type = tmp;
      break;
#ifndef NDEBUG
    case RMCPDUMP_KEY:       /* --rmcpdump */
      cmd_args->rmcpdump++;
      break;
#endif /* !NDEBUG */
    case ON_KEY:       /* --on */
      cmd_args->powercmd = IPMIPOWER_POWER_CMD_POWER_ON;
      break;
    case OFF_KEY:       /* --off */
      cmd_args->powercmd = IPMIPOWER_POWER_CMD_POWER_OFF;
      break;
    case CYCLE_KEY:       /* --cycle */
      cmd_args->powercmd = IPMIPOWER_POWER_CMD_POWER_CYCLE;
      break;
    case RESET_KEY:       /* --reset */
      cmd_args->powercmd = IPMIPOWER_POWER_CMD_POWER_RESET;
      break;
    case STAT_KEY:       /* --stat */
      cmd_args->powercmd = IPMIPOWER_POWER_CMD_POWER_STATUS;
      break;
    case PULSE_KEY:       /* --pulse */
      cmd_args->powercmd = IPMIPOWER_POWER_CMD_PULSE_DIAGNOSTIC_INTERRUPT;
      break;
    case SOFT_KEY:       /* --soft */
      cmd_args->powercmd = IPMIPOWER_POWER_CMD_SOFT_SHUTDOWN_OS;
      break;
    case ON_IF_OFF_KEY:       /* --on-if-off */
      cmd_args->on_if_off++;
      break;
    case WAIT_UNTIL_OFF_KEY:       /* --wait-until-on */
      cmd_args->wait_until_on++;
      break;
    case WAIT_UNTIL_ON_KEY:       /* --wait-until-off */
      cmd_args->wait_until_off++;
      break;
    case OEM_POWER_TYPE_KEY:
      _parse_oem_power_type (cmd_args, arg);
      break;
      /* RETRY_WAIT_TIMEOUT for backwards compatability */
    case RETRY_WAIT_TIMEOUT_KEY:
    case RETRANSMISSION_WAIT_TIMEOUT_KEY:       /* --retransmission-wait-timeout */
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp <= 0)
        {
          fprintf (stderr, "retransmission wait timeout length invalid");
          exit (EXIT_FAILURE);
        }
      cmd_args->retransmission_wait_timeout = tmp;
      break;
      /* RETRY_BACKOFF_COUNT for backwards compatability */
    case RETRY_BACKOFF_COUNT_KEY:
    case RETRANSMISSION_BACKOFF_COUNT_KEY:       /* --retransmission-backoff-count */
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp <= 0)
        {
          fprintf (stderr, "retransmission backoff count invalid");
          exit (EXIT_FAILURE);
        }
      cmd_args->retransmission_backoff_count = tmp;
      break;
    case PING_INTERVAL_KEY:       /* --ping-interval */
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp < 0)
        {
          fprintf (stderr, "ping interval length invalid");
          exit (EXIT_FAILURE);
        }
      cmd_args->ping_interval = tmp;
      break;
    case PING_TIMEOUT_KEY:       /* --ping-timeout */
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp < 0)
        {
          fprintf (stderr, "ping timeout length invalid");
          exit (EXIT_FAILURE);
        }
      cmd_args->ping_timeout = tmp;
      break;
    case PING_PACKET_COUNT_KEY:       /* --ping-packet-count */
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp < 0)
        {
          fprintf (stderr, "ping packet count invalid");
          exit (EXIT_FAILURE);
        }
      cmd_args->ping_packet_count = tmp;
      break;
    case PING_PERCENT_KEY:       /* --ping-percent */
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp < 0)
        {
          fprintf (stderr, "ping percent invalid");
          exit (EXIT_FAILURE);
        }
      cmd_args->ping_percent = tmp;
      break;
    case PING_CONSEC_COUNT_KEY:       /* --ping-consec-count */
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp < 0)
        {
          fprintf (stderr, "ping consec count invalid");
          exit (EXIT_FAILURE);
        }
      cmd_args->ping_consec_count = tmp;
      break;
      /* removed legacy short options */
    default:
      return (common_parse_opt (key, arg, &(cmd_args->common_args)));
    }

  return (0);
}

static void
_ipmipower_config_file_parse (struct ipmipower_arguments *cmd_args)
{
  struct config_file_data_ipmipower config_file_data;

  assert (cmd_args);

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_ipmipower));

  /* try legacy file first */
  if (!cmd_args->common_args.config_file)
    {
      if (!config_file_parse (IPMIPOWER_CONFIG_FILE_LEGACY,
                              1,         /* do not exit if file not found */
                              &(cmd_args->common_args),
                              CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                              CONFIG_FILE_TOOL_IPMIPOWER,
                              &config_file_data))
        goto out;
    }

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMIPOWER,
                         &config_file_data) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

 out:
  if (config_file_data.on_if_off_count)
    cmd_args->on_if_off = config_file_data.on_if_off;
  if (config_file_data.wait_until_on_count)
    cmd_args->wait_until_on = config_file_data.wait_until_on;
  if (config_file_data.wait_until_off_count)
    cmd_args->wait_until_off = config_file_data.wait_until_off;
  /* See comments in config file parsing */
  if (config_file_data.oem_power_type_str_count)
    _parse_oem_power_type (cmd_args, config_file_data.oem_power_type_str);
  if (config_file_data.retransmission_wait_timeout_count)
    cmd_args->retransmission_wait_timeout = config_file_data.retransmission_wait_timeout;
  if (config_file_data.retransmission_backoff_count_count)
    cmd_args->retransmission_backoff_count = config_file_data.retransmission_backoff_count;
  if (config_file_data.ping_interval_count)
    cmd_args->ping_interval = config_file_data.ping_interval;
  if (config_file_data.ping_timeout_count)
    cmd_args->ping_timeout = config_file_data.ping_timeout;
  if (config_file_data.ping_packet_count_count)
    cmd_args->ping_packet_count = config_file_data.ping_packet_count;
  if (config_file_data.ping_percent_count)
    cmd_args->ping_percent = config_file_data.ping_percent;
  if (config_file_data.ping_consec_count_count)
    cmd_args->ping_consec_count = config_file_data.ping_consec_count;
}

static void
_ipmipower_args_validate (struct ipmipower_arguments *cmd_args)
{
  assert (cmd_args);

  if (!IPMIPOWER_OEM_POWER_TYPE_VALID (cmd_args->oem_power_type))
    {
      fprintf (stderr, "invalid oem power type\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->retransmission_wait_timeout > cmd_args->common_args.session_timeout)
    {
      fprintf (stderr, "retransmission wait timeout larger than session timeout\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->powercmd != IPMIPOWER_POWER_CMD_NONE && !cmd_args->common_args.hostname)
    {
      fprintf (stderr, "must specify target hostname(s) in non-interactive mode\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->ping_interval > cmd_args->ping_timeout)
    {
      fprintf (stderr, "ping interval larger than ping timeout\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->ping_consec_count > cmd_args->ping_packet_count)
    {
      fprintf (stderr, "ping consec count larger than ping packet count\n");
      exit (EXIT_FAILURE);
    }
}

void
ipmipower_argp_parse (int argc, char **argv, struct ipmipower_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_operator (&(cmd_args->common_args));

  /* ipmipower differences */
  cmd_args->common_args.driver_type = IPMI_DEVICE_LAN;
  cmd_args->common_args.driver_type_outofband_only = 1;
  cmd_args->common_args.session_timeout = 20000; /* 20 seconds */
  cmd_args->common_args.retransmission_timeout = 400; /* .4 seconds */

#ifndef NDEBUG
  cmd_args->rmcpdump = 0;
#endif /* NDEBUG */

  cmd_args->powercmd = IPMIPOWER_POWER_CMD_NONE;
  cmd_args->on_if_off = 0;
  cmd_args->wait_until_on = 0;
  cmd_args->wait_until_off = 0;
  cmd_args->oem_power_type = IPMIPOWER_OEM_POWER_TYPE_NONE;
  cmd_args->retransmission_wait_timeout = 500; /* .5 seconds  */
  cmd_args->retransmission_backoff_count = 8;
  cmd_args->ping_interval = 5000; /* 5 seconds */
  cmd_args->ping_timeout = 30000; /* 30 seconds */
  cmd_args->ping_packet_count = 10;
  cmd_args->ping_percent = 50;
  cmd_args->ping_consec_count = 5;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _ipmipower_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  /* don't check hostname inputs, ipmipower isn't like most tools */
  verify_common_cmd_args_outofband (&(cmd_args->common_args), 0);
  _ipmipower_args_validate (cmd_args);
}
