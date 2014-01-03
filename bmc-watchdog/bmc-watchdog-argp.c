/*****************************************************************************\
 *  $Id: bmc-watchdog-argp.c,v 1.25 2010-06-30 21:56:36 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2004-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155913
 *
 *  This file is part of Bmc-watchdog, a base management controller
 *  (BMC) watchdog timer management tool. For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Bmc-Watchdog is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Bmc-Watchdog is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Bmc-Watchdog.  If not, see <http://www.gnu.org/licenses/>.
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
#include <assert.h>
#include <errno.h>

#include "bmc-watchdog.h"
#include "bmc-watchdog-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "bmc-watchdog - " PACKAGE_VERSION "\n"
  "Copyright (C) 2003-2014 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "bmc-watchdog - BMC watchdog timer daemon and control utility";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_OPTIONS_DEBUG,
    { "set", SET_KEY, NULL, 0,
      "Set BMC Watchdog Config.", 40},
    { "get", GET_KEY, NULL, 0,
      "Get BMC Watchdog Config.", 41},
    { "reset", RESET_KEY, NULL, 0,
      "Reset BMC Watchdog Timer.", 42},
    { "start", START_KEY, NULL, 0,
      "Start BMC Watchdog Timer.", 43},
    { "stop", STOP_KEY, NULL, 0,
      "Stop BMC Watchdog Timer.", 44},
    { "clear", CLEAR_KEY, NULL, 0,
      "Clear BMC Watchdog Config.", 45},
    { "daemon", DAEMON_KEY, NULL, 0,
      "Run in daemon mode.", 46},
    { "logfile", LOGFILE_KEY, "FILE", OPTION_HIDDEN,
      "Specify an alternate logfile.", 47},
    { "verbose-logging", VERBOSE_LOGGING_KEY, 0, 0,
      "Increase verbosity in logging.", 48},
    { "no-logging", NO_LOGGING_KEY, NULL, 0,
      "Turn off all syslogging.", 49},
    { "timer-use", TIMER_USE_KEY, "INT", 0,
      "Set timer use.", 50},
    { "stop-timer", STOP_TIMER_KEY, "INT", 0,
      "Set Stop Timer Flag.", 51},
    { "log", LOG_KEY, "INT", 0,
      "Set Log Flag.", 52},
    { "timeout-action", TIMEOUT_ACTION_KEY, "INT", 0,
      "Set timeout action.", 53},
    { "pre-timeout-interrupt", PRE_TIMEOUT_INTERRUPT_KEY, "INT", 0,
      "Set pre-timeout interrupt.", 54},
    { "pre-timeout-interval", PRE_TIMEOUT_INTERVAL_KEY, "SECONDS", 0,
      "Set pre-timeout interval in seconds.", 55},
    { "clear-bios-frb2", CLEAR_BIOS_FRB2_KEY, NULL, 0,
      "Clear BIOS FRB2 Timer Use Flag.", 56},
    { "clear-bios-post", CLEAR_BIOS_POST_KEY, NULL, 0,
      "Clear BIOS POST Timer Use Flag.", 57},
    { "clear-os-load", CLEAR_OS_LOAD_KEY, NULL, 0,
      "Clear OS Load Timer Use Flag.", 58},
    { "clear-sms-os", CLEAR_SMS_OS_KEY, NULL, 0,
      "Clear SMS/OS Timer Use Flag.", 59},
    { "clear-oem", CLEAR_OEM_KEY, NULL, 0,
      "Clear OEM Timer Use Flag.", 60},
    { "initial-countdown", INITIAL_COUNTDOWN_KEY, "SECONDS", 0,
      "Set initial countdown in seconds.", 61},
    { "start-after-set", START_AFTER_SET_KEY, NULL, 0,
      "Start timer after set if timer is stopped.", 62},
    { "reset-after-set", RESET_AFTER_SET_KEY, NULL, 0,
      "Reset timer after set if timer is running.", 63},
    { "start-if-stopped", START_IF_STOPPED_KEY, NULL, 0,
      "Don't set if timer is stopped, just start.", 64},
    { "reset-if-running", RESET_IF_RUNNING_KEY, NULL, 0,
      "Don't set if timer is running, just reset.", 65},
    { "gratuitous-arp", GRATUITOUS_ARP_KEY, "INT", 0,
      "Set Gratuitous ARPs Flag.", 66},
    { "arp-response", ARP_RESPONSE_KEY, "INT", 0,
      "Set ARP Responses Flag.", 67},
    { "reset-period", RESET_PERIOD_KEY, "SECONDS", 0,
      "Specify time interval before resetting timer.", 68},
    { "help", HELP_KEY, NULL, 0,
      "Output help.", 69},
    { "help", HELP_KEY, NULL, 0,
      "Output help.", 70},
    { "version", VERSION_KEY, NULL, 0,
      "Output version.", 71},
    { NULL, 0, NULL, 0, NULL, 0}
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc };

static struct argp cmdline_config_file_argp = { cmdline_options,
                                                cmdline_config_file_parse,
                                                cmdline_args_doc,
                                                cmdline_doc };

static char *
_cmd_string (struct bmc_watchdog_arguments *cmd_args)
{
  if (cmd_args->get)
    return "--get";
  else if (cmd_args->set)
    return "--set";
  else if (cmd_args->reset)
    return "--reset";
  else if (cmd_args->start)
    return "--start";
  else if (cmd_args->stop)
    return "--stop";
  else if (cmd_args->clear)
    return "--clear";
  else if (cmd_args->daemon)
    return "--daemon";
  else
    return (NULL);
}

static void
_usage (struct bmc_watchdog_arguments *cmd_args)
{
  char *cmdstr;

  cmdstr = _cmd_string (cmd_args);
  if (!cmdstr)
    {
      fprintf (stderr,
               "Usage: bmc-watchdog <COMMAND> [OPTIONS]... [COMMAND_OPTIONS]...\n\n");
      fprintf (stderr,
               "COMMANDS:\n"
               "  -s         --set                            Set BMC Watchdog Config.\n"
               "  -g         --get                            Get BMC Watchdog Config.\n"
               "  -r         --reset                          Reset BMC Watchdog Timer.\n"
               "  -t         --start                          Start BMC Watchdog Timer.\n"
               "  -y         --stop                           Stop BMC Watchdog Timer.\n"
               "  -c         --clear                          Clear BMC Watchdog Config.\n"
               "  -d         --daemon                         Run in Daemon Mode.\n\n");
    }
  else
    fprintf (stderr,
             "Usage: bmc-watchdog %s [OPTIONS]... \n\n", cmdstr);

  fprintf (stderr,
           "OPTIONS:\n"
           "  -D IPMIDRIVER  --driver-type=IPMIDRIVER             Specify IPMI driver type.\n"
           "                 --disable-auto-probe                 Do not probe driver for default settings.\n"
           "                 --driver-address=DRIVER-ADDRESS      Specify driver address.\n"
           "                 --driver-device=DEVICE               Specify driver device path.\n"
           "                 --register-spacing=REGISTER-SPACING  Specify driver register spacing.\n"
           "                 --config-file=FILE                   Specify an alternate config file\n"
	   "  -v             --verbose-logging                    Turn on verbose logging\n"
           "  -n             --no-logging                         Turn off all logging\n"
           "  -?             --help                               Output help menu.\n"
           "  -V             --version                            Output version.\n");

  fprintf (stderr,
           "                 --debug                              Turn on debugging.\n");

  fprintf (stderr, "\n");
  if (cmd_args->set || cmd_args->start || cmd_args->daemon)
    fprintf (stderr,
             "COMMAND SPECIFIC OPTIONS:\n");

  if (cmd_args->set || cmd_args->daemon)
    fprintf (stderr,
             "  -u INT     --timer-use=INT              Set timer use.\n"
             "             %d = BIOS FRB2\n"
             "             %d = BIOS POST\n"
             "             %d = OS_LOAD\n"
             "             %d = SMS OS\n"
             "             %d = OEM\n"
             "  -m INT     --stop-timer=INT             Set Stop Timer Flag.\n"
             "             %d = Stop Timer\n"
             "             %d = Don't Stop timer\n"
             "  -l INT     --log=INT                    Set Log Flag.\n"
             "             %d = Enable Log\n"
             "             %d = Disable Log\n"
             "  -a INT     --timeout-action=INT         Set timeout action.\n"
             "             %d = No action\n"
             "             %d = Hard Reset\n"
             "             %d = Power Down\n"
             "             %d = Power Cycle\n"
             "  -p INT     --pre-timeout-interrupt=INT  Set pre-timeout interrupt.\n"
             "             %d = None\n"
             "             %d = SMI\n"
             "             %d = NMI\n"
             "             %d = Messaging Interrupt\n"
             "  -z SECS    --pre-timeout-interval=SECS  Set pre-timeout interval in seconds.\n"
             "  -F         --clear-bios-frb2            Clear BIOS FRB2 Timer Use Flag.\n"
             "  -P         --clear-bios-post            Clear BIOS POST Timer Use Flag.\n"
             "  -L         --clear-os-load              Clear OS Load Timer Use Flag.\n"
             "  -S         --clear-sms-os               Clear SMS/OS Timer Use Flag.\n"
             "  -O         --clear-oem                  Clear OEM Timer Use Flag.\n"
             "  -i SECS    --initial-countdown=SECS     Set initial countdown in seconds.\n",
             IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_BIOS_FRB2,
             IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_BIOS_POST,
             IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_OS_LOAD,
             IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_SMS_OS,
             IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_OEM,
             IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_ENABLE,
             IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_DISABLE,
             IPMI_BMC_WATCHDOG_TIMER_LOG_ENABLE,
             IPMI_BMC_WATCHDOG_TIMER_LOG_DISABLE,
             IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_NO_ACTION,
             IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_HARD_RESET,
             IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_POWER_DOWN,
             IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_POWER_CYCLE,
             IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_NONE,
             IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_SMI,
             IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_NMI,
             IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_MESSAGING_INTERRUPT);

  if (cmd_args->set)
    fprintf (stderr,
             "  -w         --start-after-set            Start timer after set if timer is stopped.\n"
	     "  -x         --reset-after-set            Reset timer after set if timer is running.\n"
	     "  -j         --start-if-stopped           Don't set if timer is stopped, just start.\n"
	     "  -k         --reset-if-running           Don't set if timer is running, just reset.\n");

  if (cmd_args->start || cmd_args->daemon)
    fprintf (stderr,
             "  -G INT     --gratuitous-arp=INT         Set Gratuitous ARPs Flag.\n"
             "             %d = Suspend Gratuitous ARPs\n"
             "             %d = Do Not Suspend Gratuitous ARPs\n"
             "  -A INT     --arp-response=INT           Set ARP Responses Flag.\n"
             "             %d = Suspend ARP Responses\n"
             "             %d = Do Not Suspend ARP Responses\n",
             IPMI_BMC_GENERATED_GRATUITOUS_ARP_SUSPEND,
             IPMI_BMC_GENERATED_GRATUITOUS_ARP_DO_NOT_SUSPEND,
             IPMI_BMC_GENERATED_ARP_RESPONSE_SUSPEND,
             IPMI_BMC_GENERATED_ARP_RESPONSE_DO_NOT_SUSPEND);
  if (cmd_args->daemon)
    fprintf (stderr,
             "  -e SECS    --reset-period=SECS          Specify time interval before resetting timer.\n");

  if (cmd_args->set || cmd_args->start || cmd_args->daemon)
    fprintf (stderr, "\n");

  exit (EXIT_FAILURE);
}

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  struct bmc_watchdog_arguments *cmd_args;
  char *endptr;
  int tmp;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case SET_KEY:
      cmd_args->set++;
      break;
    case GET_KEY:
      cmd_args->get++;
      break;
    case RESET_KEY:
      cmd_args->reset++;
      break;
    case START_KEY:
      cmd_args->start++;
      break;
    case STOP_KEY:
      cmd_args->stop++;
      break;
    case CLEAR_KEY:
      cmd_args->clear++;
      break;
    case DAEMON_KEY:
      cmd_args->daemon++;
      break;
    case LOGFILE_KEY:
      /* deprecated */
      break;
    case VERBOSE_LOGGING_KEY:
      cmd_args->verbose_logging++;
      break;
    case NO_LOGGING_KEY:
      cmd_args->no_logging++;
      break;
    case TIMER_USE_KEY:
      cmd_args->timer_use++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || !IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_VALID (tmp))
        {
          fprintf (stderr, "invalid timer use\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->timer_use_arg = tmp;
      break;
    case STOP_TIMER_KEY:
      cmd_args->stop_timer++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || !IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_VALID (tmp))
        {
          fprintf (stderr, "invalid stop timer value\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->stop_timer_arg = tmp;
      break;
    case LOG_KEY:
      cmd_args->log++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || !IPMI_BMC_WATCHDOG_TIMER_LOG_VALID (tmp))
        {
          fprintf (stderr, "invalid log value\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->log_arg = tmp;
      break;
    case TIMEOUT_ACTION_KEY:
      cmd_args->timeout_action++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || !IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_VALID (tmp))
        {
          fprintf (stderr, "invalid timeout action value\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->timeout_action_arg = tmp;
      break;
    case PRE_TIMEOUT_INTERRUPT_KEY:
      cmd_args->pre_timeout_interrupt++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || !IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_VALID (tmp))
        {
          fprintf (stderr, "invalid pre timeout interrupt value\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->pre_timeout_interrupt_arg = tmp;
      break;
    case PRE_TIMEOUT_INTERVAL_KEY:
      cmd_args->pre_timeout_interval++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0')
        {
          fprintf (stderr, "invalid pre timeout interval\n");
          exit (EXIT_FAILURE);
        }
      if (tmp < IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERVAL_MIN
          || tmp > IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERVAL_MAX)
        {
          fprintf (stderr, "pre timeout interval out of range\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->pre_timeout_interval_arg = tmp;
      break;
    case CLEAR_BIOS_FRB2_KEY:
      cmd_args->clear_bios_frb2++;
      break;
    case CLEAR_BIOS_POST_KEY:
      cmd_args->clear_bios_post++;
      break;
    case CLEAR_OS_LOAD_KEY:
      cmd_args->clear_os_load++;
      break;
    case CLEAR_SMS_OS_KEY:
      cmd_args->clear_sms_os++;
      break;
    case CLEAR_OEM_KEY:
      cmd_args->clear_oem++;
      break;
    case INITIAL_COUNTDOWN_KEY:
      cmd_args->initial_countdown_seconds++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0')
        {
          fprintf (stderr, "invalid initial countdown\n");
          exit (EXIT_FAILURE);
        }
      if (tmp < IPMI_BMC_WATCHDOG_TIMER_INITIAL_COUNTDOWN_MIN_SECONDS
          || tmp > IPMI_BMC_WATCHDOG_TIMER_INITIAL_COUNTDOWN_MAX_SECONDS)
        {
          fprintf (stderr, "initial countdown out of range\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->initial_countdown_seconds_arg = tmp;
      break;
    case START_AFTER_SET_KEY:
      cmd_args->start_after_set++;
      break;
    case RESET_AFTER_SET_KEY:
      cmd_args->reset_after_set++;
      break;
    case START_IF_STOPPED_KEY:
      cmd_args->start_if_stopped++;
      break;
    case RESET_IF_RUNNING_KEY:
      cmd_args->reset_if_running++;
      break;
    case GRATUITOUS_ARP_KEY:
      cmd_args->gratuitous_arp++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || !IPMI_BMC_GENERATED_GRATUITOUS_ARP_VALID (tmp))
        {
          fprintf (stderr, "invalid gratuitous arp value\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->gratuitous_arp_arg = tmp;
      break;
    case ARP_RESPONSE_KEY:
      cmd_args->arp_response++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || !IPMI_BMC_GENERATED_ARP_RESPONSE_VALID (tmp))
        {
          fprintf (stderr, "invalid arp response value\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->arp_response_arg = tmp;
      break;
    case RESET_PERIOD_KEY:
      cmd_args->reset_period++;
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0')
        {
          fprintf (stderr, "invalid reset period\n");
          exit (EXIT_FAILURE);
        }
      if (tmp < IPMI_BMC_WATCHDOG_TIMER_INITIAL_COUNTDOWN_MIN_SECONDS
          || tmp > IPMI_BMC_WATCHDOG_TIMER_INITIAL_COUNTDOWN_MAX_SECONDS)
        {
          fprintf (stderr, "reset period out of range\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->reset_period_arg = tmp;
      break;
      /* do not output default argp help, do internal help */
    case HELP_KEY:
      cmd_args->help++;
      break;
    case VERSION_KEY:
      cmd_args->version++;
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return (common_parse_opt (key, arg, &(cmd_args->common_args)));
    }

  return (0);
}

static void
_bmc_watchdog_config_file_parse (struct bmc_watchdog_arguments *cmd_args)
{
  struct config_file_data_bmc_watchdog config_file_data;

  memset (&config_file_data,
          '\0',
          sizeof (struct config_file_data_bmc_watchdog));

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND,
                         CONFIG_FILE_TOOL_BMC_WATCHDOG,
                         &config_file_data) < 0)

    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }

  if (config_file_data.verbose_logging_count)
    cmd_args->verbose_logging = config_file_data.verbose_logging;
  if (config_file_data.no_logging_count)
    cmd_args->no_logging = config_file_data.no_logging;
}

static void
_bmc_watchdog_args_validate (struct bmc_watchdog_arguments *cmd_args)
{
  int count;

  assert (cmd_args);

  if (cmd_args->help)
    _usage (cmd_args);

  if (cmd_args->version)
    {
      fprintf (stderr, "%s\n", argp_program_version);
      exit (EXIT_FAILURE);
    }

  count = cmd_args->set + cmd_args->get + cmd_args->reset +
    cmd_args->start + cmd_args->stop + cmd_args->clear + cmd_args->daemon;
  if (!count)
    _usage (cmd_args);
  if (count > 1)
    {
      fprintf (stderr, "Only one command can be specified\n");
      exit (EXIT_FAILURE);
    }

  if (((cmd_args->get
        || cmd_args->reset
        || cmd_args->start
        || cmd_args->stop
        || cmd_args->clear)
       && (cmd_args->timer_use
           || cmd_args->stop_timer
           || cmd_args->log
           || cmd_args->timeout_action
           || cmd_args->pre_timeout_interrupt
           || cmd_args->pre_timeout_interval
           || cmd_args->clear_bios_frb2
           || cmd_args->clear_bios_post
           || cmd_args->clear_sms_os
           || cmd_args->clear_oem
           || cmd_args->initial_countdown_seconds
           || cmd_args->start_after_set
           || cmd_args->reset_after_set
           || cmd_args->reset_if_running
           || cmd_args->reset_period))
      || (cmd_args->set
          && cmd_args->reset_period)
      || (cmd_args->daemon
          && (cmd_args->stop_timer
              || cmd_args->start_after_set
              || cmd_args->reset_after_set
              || cmd_args->reset_if_running))
      || ((cmd_args->set
           || cmd_args->get
           || cmd_args->reset
           || cmd_args->stop
           || cmd_args->clear)
          && (cmd_args->gratuitous_arp
              || cmd_args->arp_response)))
    {
      char *cmdstr;

      cmdstr = _cmd_string (cmd_args);
      fprintf (stderr, "Invalid command option specified for '%s' command\n", cmdstr);
      exit (EXIT_FAILURE);
    }
}

void
bmc_watchdog_argp_parse (int argc, char **argv, struct bmc_watchdog_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_user (&(cmd_args->common_args));

  cmd_args->set = 0;
  cmd_args->get = 0;
  cmd_args->reset = 0;
  cmd_args->start = 0;
  cmd_args->stop = 0;
  cmd_args->clear = 0;
  cmd_args->daemon = 0;
  cmd_args->verbose_logging = 0;
  cmd_args->no_logging = 0;
  cmd_args->timer_use = 0;
  cmd_args->timer_use_arg = 0;
  cmd_args->stop_timer = 0;
  cmd_args->stop_timer_arg = 0;
  cmd_args->log = 0;
  cmd_args->log_arg = 0;
  cmd_args->timeout_action = 0;
  cmd_args->timeout_action_arg = 0;
  cmd_args->pre_timeout_interrupt = 0;
  cmd_args->pre_timeout_interrupt_arg = 0;
  cmd_args->pre_timeout_interval = 0;
  cmd_args->pre_timeout_interval_arg = 0;
  cmd_args->clear_bios_frb2 = 0;
  cmd_args->clear_bios_post = 0;
  cmd_args->clear_os_load = 0;
  cmd_args->clear_sms_os = 0;
  cmd_args->clear_oem = 0;
  cmd_args->initial_countdown_seconds = 0;
  cmd_args->initial_countdown_seconds_arg = 0;
  cmd_args->start_after_set = 0;
  cmd_args->reset_after_set = 0;
  cmd_args->start_if_stopped = 0;
  cmd_args->reset_if_running = 0;
  cmd_args->gratuitous_arp = 0;
  cmd_args->gratuitous_arp_arg = 0;
  cmd_args->arp_response = 0;
  cmd_args->arp_response_arg = 0;
  cmd_args->reset_period = 0;
  cmd_args->reset_period_arg = 0;
  cmd_args->help = 0;
  cmd_args->version = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER | ARGP_NO_HELP,
              NULL,
              &(cmd_args->common_args));

  _bmc_watchdog_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER | ARGP_NO_HELP,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _bmc_watchdog_args_validate (cmd_args);
}
