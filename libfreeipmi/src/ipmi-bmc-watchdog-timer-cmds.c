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

#include "freeipmi-build.h"
#include "fiid-wrappers.h"

fiid_template_t tmpl_cmd_reset_watchdog_timer_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_reset_watchdog_timer_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_watchdog_timer_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "timer_use", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "stop_timer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "log", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "timeout_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "pre_timeout_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "pre_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.bios_frb2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.bios_post", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.os_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.sms_os", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "initial_countdown_value_ls_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "initial_countdown_value_ms_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_watchdog_timer_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_watchdog_timer_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_watchdog_timer_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "timer_use", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "log", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "timeout_action", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "pre_timeout_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "pre_timeout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.bios_frb2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.bios_post", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.os_load", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.sms_os", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "timer_use_expiration_flag.oem", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "initial_countdown_value_ls_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "initial_countdown_value_ms_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "present_countdown_value_ls_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "present_countdown_value_ms_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
fill_cmd_reset_watchdog_timer (fiid_obj_t obj_cmd)
{
  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_cmd_reset_watchdog_timer_rq);

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_RESET_WATCHDOG_TIMER);
  return (0);
}

int8_t
fill_cmd_set_watchdog_timer (uint8_t timer_use,
                             uint8_t stop_timer, 
                             uint8_t log, 
                             uint8_t timeout_action, 
                             uint8_t pre_timeout_interrupt, 
                             uint8_t pre_timeout_interval, 
                             uint8_t timer_use_expiration_flag_bios_frb2, 
                             uint8_t timer_use_expiration_flag_bios_post, 
                             uint8_t timer_use_expiration_flag_os_load, 
                             uint8_t timer_use_expiration_flag_sms_os, 
                             uint8_t timer_use_expiration_flag_oem, 
                             uint8_t initial_countdown_value_ls_byte, 
                             uint8_t initial_countdown_value_ms_byte, 
                             fiid_obj_t obj_cmd)
{
  if (!fiid_obj_valid(obj_cmd)
      || !IPMI_BMC_WATCHDOG_TIMER_LOG_VALID(log)
      || !IPMI_BMC_WATCHDOG_TIMER_STOP_TIMER_VALID(stop_timer)
      || !IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_VALID(timer_use)
      || !IPMI_BMC_WATCHDOG_TIMER_PRE_TIMEOUT_INTERRUPT_VALID(pre_timeout_interrupt)
      || !IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_EXPIRATION_VALID(timer_use_expiration_flag_bios_frb2)
      || !IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_EXPIRATION_VALID(timer_use_expiration_flag_bios_post)
      || !IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_EXPIRATION_VALID(timer_use_expiration_flag_os_load)
      || !IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_EXPIRATION_VALID(timer_use_expiration_flag_sms_os)
      || !IPMI_BMC_WATCHDOG_TIMER_TIMER_USE_EXPIRATION_VALID(timer_use_expiration_flag_oem)
      || !IPMI_BMC_WATCHDOG_TIMER_TIMEOUT_ACTION_VALID(timeout_action))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_cmd_set_watchdog_timer_rq);

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_SET_WATCHDOG_TIMER);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"timer_use", timer_use);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"stop_timer", stop_timer);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"log", log);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"timeout_action", timeout_action);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"pre_timeout_interrupt", pre_timeout_interrupt);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved3", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"pre_timeout_interval", pre_timeout_interval);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved4", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"timer_use_expiration_flag.bios_frb2", timer_use_expiration_flag_bios_frb2);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"timer_use_expiration_flag.bios_post", timer_use_expiration_flag_bios_post);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"timer_use_expiration_flag.os_load", timer_use_expiration_flag_os_load);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"timer_use_expiration_flag.sms_os", timer_use_expiration_flag_sms_os);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"timer_use_expiration_flag.oem", timer_use_expiration_flag_oem);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved5", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"reserved6", 0);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"initial_countdown_value_ls_byte", initial_countdown_value_ls_byte);
  FIID_OBJ_SET (obj_cmd, (uint8_t *)"initial_countdown_value_ms_byte", initial_countdown_value_ms_byte);

  return (0);
}

int8_t
fill_cmd_get_watchdog_timer (fiid_obj_t obj_cmd)
{
  if (!fiid_obj_valid(obj_cmd))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_cmd_get_watchdog_timer_rq);

  FIID_OBJ_SET (obj_cmd, (uint8_t *)"cmd", IPMI_CMD_GET_WATCHDOG_TIMER);
  return (0);
}
