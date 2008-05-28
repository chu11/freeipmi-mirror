/*****************************************************************************\
 *  $Id: bmc-watchdog.h,v 1.1 2008-05-28 21:09:32 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 2 of the License, or (at your
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

#ifndef _BMC_WATCHDOG_H
#define _BMC_WATCHDOG_H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

/* Pre Timeout Interval is 1 byte */
#define IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERVAL_MIN_SECS  0
#define IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERVAL_MAX_SECS  255

/* Countdown Seconds is 2 bytes of 100 millisecond chunks */
#define IPMI_BMC_WATCHDOG_TIMER_INITIAL_COUNTDOWN_MIN_SECS     0
#define IPMI_BMC_WATCHDOG_TIMER_INITIAL_COUNTDOWN_MAX_SECS     6553

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
    HELP2_KEY = 'h',             /* legacy */
    HELP3_KEY = 'H',             /* legacy */
  };

/* achu
 *
 * many of the fields can be '0', so there are various "_val" to store
 * the actual value.
 */
struct bmc_watchdog_arguments
{
  struct common_cmd_args common;
  int set;
  int get;
  int reset;
  int start;
  int stop;
  int clear;
  int daemon;
  char *logfile;
  int no_logging;
  int timer_use;
  uint8_t timer_use_val;
  int stop_timer;
  uint8_t stop_timer_val;
  int log;
  uint8_t log_val;
  int timeout_action;
  uint8_t timeout_action_val;
  int pre_timeout_interrupt;
  uint8_t pre_timeout_interrupt_val;
  int pre_timeout_interval;
  uint8_t pre_timeout_interval_val;
  int clear_bios_frb2;
  int clear_bios_post;
  int clear_os_load;
  int clear_sms_os;
  int clear_oem;
  int initial_countdown_seconds;
  uint16_t initial_countdown_seconds_val;
  int start_after_set;
  int reset_after_set;
  int start_if_stopped;
  int reset_if_running;
  int gratuitous_arp;
  uint8_t gratuitous_arp_val;
  int arp_response;
  uint8_t arp_response_val;
  int reset_period;
  uint32_t reset_period_val;
  int help;
};

#endif
