/*****************************************************************************\
 *  $Id: bmc-watchdog.h,v 1.10 2010-06-17 20:49:54 chu11 Exp $
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

#ifndef BMC_WATCHDOG_H
#define BMC_WATCHDOG_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"

enum bmc_watchdog_argp_option_keys
  {
    SET_KEY = 's',
    GET_KEY = 'g',
    RESET_KEY = 'r',
    START_KEY = 't',
    STOP_KEY = 'y',
    CLEAR_KEY = 'c',
    DAEMON_KEY = 'd',
    LOGFILE_KEY = 'f',
    VERBOSE_LOGGING_KEY = 'v',
    NO_LOGGING_KEY = 'n',
    TIMER_USE_KEY = 'u',
    STOP_TIMER_KEY = 'm',
    LOG_KEY = 'l',
    TIMEOUT_ACTION_KEY = 'a',
    PRE_TIMEOUT_INTERRUPT_KEY = 'p',
    PRE_TIMEOUT_INTERVAL_KEY = 'z',
    CLEAR_BIOS_FRB2_KEY = 'F',
    CLEAR_BIOS_POST_KEY = 'P',
    CLEAR_OS_LOAD_KEY = 'L',
    CLEAR_SMS_OS_KEY = 'S',
    CLEAR_OEM_KEY = 'O',
    INITIAL_COUNTDOWN_KEY = 'i',
    START_AFTER_SET_KEY = 'w',
    RESET_AFTER_SET_KEY = 'x',
    START_IF_STOPPED_KEY = 'j',
    RESET_IF_RUNNING_KEY = 'k',
    GRATUITOUS_ARP_KEY = 'G',
    ARP_RESPONSE_KEY = 'A',
    RESET_PERIOD_KEY = 'e',
    HELP_KEY = '?',
    VERSION_KEY = 'V',
  };

struct bmc_watchdog_arguments
{
  struct common_cmd_args common_args;
  int set;
  int get;
  int reset;
  int start;
  int stop;
  int clear;
  int daemon;
  int verbose_logging;
  int no_logging;
  int timer_use;
  uint8_t timer_use_arg;
  int stop_timer;
  uint8_t stop_timer_arg;
  int log;
  uint8_t log_arg;
  int timeout_action;
  uint8_t timeout_action_arg;
  int pre_timeout_interrupt;
  uint8_t pre_timeout_interrupt_arg;
  int pre_timeout_interval;
  uint8_t pre_timeout_interval_arg;
  int clear_bios_frb2;
  int clear_bios_post;
  int clear_os_load;
  int clear_sms_os;
  int clear_oem;
  int initial_countdown_seconds;
  uint16_t initial_countdown_seconds_arg;
  int start_after_set;
  int reset_after_set;
  int start_if_stopped;
  int reset_if_running;
  int gratuitous_arp;
  uint8_t gratuitous_arp_arg;
  int arp_response;
  uint8_t arp_response_arg;
  int reset_period;
  uint32_t reset_period_arg;
  int help;
  int version;
};

#endif /* BMC_WATCHDOG_H */
