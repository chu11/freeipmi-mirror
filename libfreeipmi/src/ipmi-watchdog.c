/* 
   ipmi-msg-support-cmds.c - IPMI Message Support Commands

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#include "freeipmi.h"

fiid_template_t tmpl_cmd_reset_watchdog_timer_rq =
  {
    {8, "cmd"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_reset_watchdog_timer_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_set_watchdog_timer_rq =
  {
    {8, "cmd"},
    {3, "timer_use"},
    {3, "reserved1"},
    {1, "stop_timer"},
    {1, "log"},
    {3, "timeout_action"},
    {1, "reserved2"},
    {3, "pre_timeout_interrupt"},
    {1, "reserved3"},
    {8, "pre_timeout_interval"},
    {1, "reserved4"},
    {1, "timer_use_expiration_flag_bios_frb2"},
    {1, "timer_use_expiration_flag_bios_post"},
    {1, "timer_use_expiration_flag_os_load"},
    {1, "timer_use_expiration_flag_sms_os"},
    {1, "timer_use_expiration_flag_oem"},
    {1, "reserved5"},
    {1, "reserved6"},
    {8, "initial_countdown_value_ls_byte"},
    {8, "initial_countdown_value_ms_byte"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_set_watchdog_timer_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_watchdog_timer_rq =
  {
    {8, "cmd"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_watchdog_timer_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {3, "timer_use"},
    {3, "reserved1"},
    {1, "timer_state"},
    {1, "log"},
    {3, "timeout_action"},
    {1, "reserved2"},
    {3, "pre_timeout_interrupt"},
    {1, "reserved3"},
    {8, "pre_timeout_interval"},
    {1, "reserved4"},
    {1, "timer_use_expiration_flag_bios_frb2"},
    {1, "timer_use_expiration_flag_bios_post"},
    {1, "timer_use_expiration_flag_os_load"},
    {1, "timer_use_expiration_flag_sms_os"},
    {1, "timer_use_expiration_flag_oem"},
    {1, "reserved5"},
    {1, "reserved6"},
    {8, "initial_countdown_value_ls_byte"},
    {8, "initial_countdown_value_ms_byte"},
    {8, "present_countdown_value_ls_byte"},
    {8, "present_countdown_value_ms_byte"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_suspend_bmc_arps_rq =
  {
    {8, "cmd"},
    {4, "channel_number"},
    {4, "reserved1"},
    {1, "gratuitous_arp_suspend"},
    {1, "arp_response_suspend"},
    {6, "reserved2"},
    {0, ""}
  };
 
fiid_template_t tmpl_cmd_suspend_bmc_arps_rs =
  {
    {8, "cmd"},
    {8, "comp_code"},
    {1, "gratuitous_arp_status"},
    {1, "arp_response_status"},
    {6, "reserved"},
    {0, ""}
  };

int8_t
fill_cmd_reset_watchdog_timer (fiid_obj_t obj_cmd)
{
  if (!obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_reset_watchdog_timer_rq, (uint8_t *)"cmd", 
		IPMI_CMD_RESET_WATCHDOG_TIMER);
  return (0);
}

int8_t
fill_cmd_set_watchdog_timer (uint8_t timer_use, uint8_t stop_timer, uint8_t log, uint8_t timeout_action, uint8_t pre_timeout_interrupt, uint8_t pre_timeout_interval, uint8_t timer_use_expiration_flag_bios_frb2, uint8_t timer_use_expiration_flag_bios_post, uint8_t timer_use_expiration_flag_os_load, uint8_t timer_use_expiration_flag_sms_os, uint8_t timer_use_expiration_flag_oem, uint8_t initial_countdown_value_ls_byte, uint8_t initial_countdown_value_ms_byte, fiid_obj_t obj_cmd)
{
  if (!obj_cmd
      || !IPMI_WATCHDOG_LOG_VALID(log)
      || !IPMI_WATCHDOG_STOP_TIMER_VALID(stop_timer)
      || !IPMI_WATCHDOG_TIMER_USE_VALID(timer_use)
      || !IPMI_WATCHDOG_PRE_TIMEOUT_INTERRUPT_VALID(pre_timeout_interrupt)
      || !IPMI_WATCHDOG_TIMEOUT_ACTION_VALID(timeout_action))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"cmd", 
		IPMI_CMD_SET_WATCHDOG_TIMER);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"log", log);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"stop_timer", stop_timer);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"timer_use", timer_use);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"pre_timeout_interrupt", pre_timeout_interrupt);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"timeout_action", timeout_action);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"pre_timeout_interval", pre_timeout_interval);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"timer_use_expiration_flag_oem", timer_use_expiration_flag_oem);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"timer_use_expiration_flag_sms_os", timer_use_expiration_flag_sms_os);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"timer_use_expiration_flag_os_load", timer_use_expiration_flag_os_load);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"timer_use_expiration_flag_bios_post", timer_use_expiration_flag_bios_post);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"timer_use_expiration_flag_bios_frb2", timer_use_expiration_flag_bios_frb2);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"initial_countdown_value_ls_byte", initial_countdown_value_ls_byte);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_set_watchdog_timer_rq, (uint8_t *)"initial_countdown_value_ms_byte", initial_countdown_value_ms_byte);

  return (0);
}

int8_t
fill_cmd_get_watchdog_timer (fiid_obj_t obj_cmd)
{
  if (!obj_cmd)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_get_watchdog_timer_rq, (uint8_t *)"cmd", 
		IPMI_CMD_GET_WATCHDOG_TIMER);
  return (0);
}

int8_t
fill_cmd_suspend_bmc_arps (uint8_t channel_number,
                           uint8_t gratuitous_arp_suspend,
                           uint8_t arp_response_suspend,
                           fiid_obj_t obj_cmd)
{
  if (!obj_cmd
      || !IPMI_WATCHDOG_GRATUITOUS_ARP_VALID(gratuitous_arp_suspend)
      || !IPMI_WATCHDOG_ARP_RESPONSE_VALID(arp_response_suspend))
    {
      errno = EINVAL;
      return (-1);
    }
 
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_suspend_bmc_arps_rq, (uint8_t *)"cmd",
                IPMI_CMD_SUSPEND_BMC_ARPS);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_suspend_bmc_arps_rq, (uint8_t *)"channel_number",
                channel_number);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_suspend_bmc_arps_rq, (uint8_t *)"gratuitous_arp_suspend",
                gratuitous_arp_suspend);
  FIID_OBJ_SET (obj_cmd, tmpl_cmd_suspend_bmc_arps_rq, (uint8_t *)"arp_response_suspend",
                arp_response_suspend);
 
  return (0);
}
