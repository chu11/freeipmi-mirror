/*
   ipmi-watchdog.h - IPMI Message Support Commands

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _IPMI_WATCHDOG_H
#define	_IPMI_WATCHDOG_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_WATCHDOG_LOG_ENABLE                                 0
#define IPMI_WATCHDOG_LOG_DISABLE                                1

#define IPMI_WATCHDOG_STOP_TIMER_ENABLE                          0
#define IPMI_WATCHDOG_STOP_TIMER_DISABLE                         1

#define IPMI_WATCHDOG_TIMER_STATE_RUNNING                        1
#define IPMI_WATCHDOG_TIMER_STATE_STOPPED                        0

#define IPMI_WATCHDOG_TIMER_USE_BIOS_FRB2                        0x1
#define IPMI_WATCHDOG_TIMER_USE_BIOS_POST                        0x2
#define IPMI_WATCHDOG_TIMER_USE_OS_LOAD                          0x3
#define IPMI_WATCHDOG_TIMER_USE_SMS_OS                           0x4
#define IPMI_WATCHDOG_TIMER_USE_OEM                              0x5

#define IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NONE                 0x0
#define IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_SMI                  0x1
#define IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NMI                  0x2
#define IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_MESSAGING_INTERRUPT  0x3

#define IPMI_WATCHDOG_TIMEOUT_ACTION_NO_ACTION                   0x0
#define IPMI_WATCHDOG_TIMEOUT_ACTION_HARD_RESET                  0x1
#define IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_DOWN                  0x2
#define IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_CYCLE                 0x3

#define IPMI_WATCHDOG_TIMER_USE_EXP_FLAG_BIOS_FRB2              0x02
#define IPMI_WATCHDOG_TIMER_USE_EXP_FLAG_BIOS_POST              0x04
#define IPMI_WATCHDOG_TIMER_USE_EXP_FLAG_OS_LOAD                0x08
#define IPMI_WATCHDOG_TIMER_USE_EXP_FLAG_SMS_OS                 0x10
#define IPMI_WATCHDOG_TIMER_USE_EXP_FLAG_OEM                    0x20

#define IPMI_WATCHDOG_GRATUITOUS_ARP_NO_SUSPEND                 0
#define IPMI_WATCHDOG_GRATUITOUS_ARP_SUSPEND                    1

#define IPMI_WATCHDOG_ARP_RESPONSE_NO_SUSPEND                   0
#define IPMI_WATCHDOG_ARP_RESPONSE_SUSPEND                      1

#define IPMI_WATCHDOG_LOG_VALID(__x) \
        (((__x) == IPMI_WATCHDOG_LOG_ENABLE \
          || (__x) == IPMI_WATCHDOG_LOG_DISABLE) ? 1 : 0)

#define IPMI_WATCHDOG_STOP_TIMER_VALID(__x) \
        (((__x) == IPMI_WATCHDOG_STOP_TIMER_ENABLE \
          || (__x) == IPMI_WATCHDOG_STOP_TIMER_DISABLE) ? 1 : 0)

#define IPMI_WATCHDOG_TIMER_USE_VALID(__x) \
        (((__x) == IPMI_WATCHDOG_TIMER_USE_BIOS_FRB2 \
          || (__x) == IPMI_WATCHDOG_TIMER_USE_BIOS_POST \
          || (__x) == IPMI_WATCHDOG_TIMER_USE_OS_LOAD \
          || (__x) == IPMI_WATCHDOG_TIMER_USE_SMS_OS \
          || (__x) == IPMI_WATCHDOG_TIMER_USE_OEM) ? 1 : 0)

#define IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_VALID(__x) \
        (((__x) == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NONE \
          || (__x) == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_SMI \
          || (__x) == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_NMI \
          || (__x) == IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_MESSAGING_INTERRUPT) ? 1 : 0)

#define IPMI_WATCHDOG_TIMEOUT_ACTION_VALID(__x) \
        (((__x) == IPMI_WATCHDOG_TIMEOUT_ACTION_NO_ACTION \
          || (__x) == IPMI_WATCHDOG_TIMEOUT_ACTION_HARD_RESET \
          || (__x) == IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_DOWN \
          || (__x) == IPMI_WATCHDOG_TIMEOUT_ACTION_POWER_CYCLE) ? 1 : 0)

#define IPMI_WATCHDOG_GRATUITOUS_ARP_VALID(__x) \
        (((__x) == IPMI_WATCHDOG_GRATUITOUS_ARP_NO_SUSPEND \
          || (__x) == IPMI_WATCHDOG_GRATUITOUS_ARP_SUSPEND) ? 1 : 0)

#define IPMI_WATCHDOG_ARP_RESPONSE_VALID(__x) \
        (((__x) == IPMI_WATCHDOG_ARP_RESPONSE_NO_SUSPEND \
          || (__x) == IPMI_WATCHDOG_ARP_RESPONSE_SUSPEND) ? 1 : 0)

extern fiid_template_t tmpl_cmd_reset_watchdog_timer_rq;
extern fiid_template_t tmpl_cmd_reset_watchdog_timer_rs;
extern fiid_template_t tmpl_cmd_set_watchdog_timer_rq;
extern fiid_template_t tmpl_cmd_set_watchdog_timer_rs;
extern fiid_template_t tmpl_cmd_get_watchdog_timer_rq;
extern fiid_template_t tmpl_cmd_get_watchdog_timer_rs;
extern fiid_template_t tmpl_cmd_suspend_bmc_arps_rq;
extern fiid_template_t tmpl_cmd_suspend_bmc_arps_rs;

int8_t fill_cmd_reset_watchdog_timer (fiid_obj_t obj_cmd);
int8_t fill_cmd_set_watchdog_timer (uint8_t timer_use, uint8_t stop_timer, uint8_t log, uint8_t timeout_action, uint8_t pre_timeout_interrupt, uint8_t pre_timeout_interval, uint8_t timer_use_expiration_flag_bios_frb2, uint8_t timer_use_expiration_flag_bios_post, uint8_t timer_use_expiration_flag_os_load, uint8_t timer_use_expiration_flag_sms_os, uint8_t timer_use_expiration_flag_oem, uint8_t initial_countdown_value_ls_byte, uint8_t initial_countdown_value_ms_byte, fiid_obj_t obj_cmd);
int8_t fill_cmd_get_watchdog_timer (fiid_obj_t obj_cmd);
int8_t fill_cmd_suspend_bmc_arps (uint8_t channel_number, uint8_t gratuitous_arp_suspend, uint8_t arp_response_suspend, fiid_obj_t obj_cmd);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-watchdog.h */


