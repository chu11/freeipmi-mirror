/*
   ipmi-chassis-cmds.h - IPMI Chassis Commands

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

#ifndef _IPMI_CHASSIS_CMDS_H
#define _IPMI_CHASSIS_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
  /* XXX */
#include "fiid.h"

#define IPMI_POWER_RESTORE_POLICY_POWERED_OFF_AFTER_AC_RETURNS  0x00
#define IPMI_POWER_RESTORE_POLICY_POWER_RESTORED_TO_STATE       0x01
#define IPMI_POWER_RESTORE_POLICY_POWERS_UP_AFTER_AC_RETURNS    0x02
#define IPMI_POWER_RESTORE_POLICY_UNKNOWN                       0x03

#define IPMI_CHASSIS_CONTROL_POWER_DOWN                    0x00
#define IPMI_CHASSIS_CONTROL_POWER_UP                      0x01
#define IPMI_CHASSIS_CONTROL_POWER_CYCLE                   0x02
#define IPMI_CHASSIS_CONTROL_HARD_RESET                    0x03
#define IPMI_CHASSIS_CONTROL_PULSE_DIAGNOSTIC_INTERRUPT    0x04
#define IPMI_CHASSIS_CONTROL_INITIATE_SOFT_SHUTDOWN        0x05

#define IPMI_CHASSIS_CONTROL_VALID(__chassis_control) \
        (((__chassis_control) == IPMI_CHASSIS_CONTROL_POWER_DOWN \
          || (__chassis_control) == IPMI_CHASSIS_CONTROL_POWER_UP \
          || (__chassis_control) == IPMI_CHASSIS_CONTROL_POWER_CYCLE \
          || (__chassis_control) == IPMI_CHASSIS_CONTROL_HARD_RESET \
          || (__chassis_control) == IPMI_CHASSIS_CONTROL_PULSE_DIAGNOSTIC_INTERRUPT \
          || (__chassis_control) == IPMI_CHASSIS_CONTROL_INITIATE_SOFT_SHUTDOWN) ? 1 : 0)

#define IPMI_CHASSIS_FORCE_IDENTIFY_OFF      0x0
#define IPMI_CHASSIS_FORCE_IDENTIFY_ON       0x1

#define IPMI_CHASSIS_FORCE_IDENTIFY_VALID(__force_identify) \
        (((__force_identify) == IPMI_CHASSIS_FORCE_IDENTIFY_OFF \
          || (__force_identify) == IPMI_CHASSIS_FORCE_IDENTIFY_ON) ? 1 : 0)

#define IPMI_CHASSIS_IDENTIFY_STATE_OFF            0x0
#define IPMI_CHASSIS_IDENTIFY_STATE_TEMPORARY_ON   0x1
#define IPMI_CHASSIS_IDENTIFY_STATE_INDEFINITE_ON  0x2
#define IPMI_CHASSIS_IDENTIFY_STATE_RESERVED       0x3

#define IPMI_POWER_RESTORE_POLICY_NO_CHANGE                                0x3
#define IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_IS_LOST         0x2
#define IPMI_POWER_RESTORE_POLICY_RESTORE_POWER_TO_STATE_WHEN_AC_WAS_LOST  0x1
#define IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWERED_OFF                  0x0

#define IPMI_POWER_RESTORE_POLICY_VALID(__policy) \
        (((__policy) == IPMI_POWER_RESTORE_POLICY_NO_CHANGE \
          || (__policy) == IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_IS_LOST \
          || (__policy) == IPMI_POWER_RESTORE_POLICY_RESTORE_POWER_TO_STATE_WHEN_AC_WAS_LOST \
          || (__policy) == IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWERED_OFF) ? 1 : 0)

extern fiid_template_t tmpl_set_power_restore_policy_rq;
extern fiid_template_t tmpl_set_power_restore_policy_rs;
extern fiid_template_t tmpl_cmd_chassis_control_rq;
extern fiid_template_t tmpl_cmd_chassis_control_rs;
extern fiid_template_t tmpl_cmd_get_chassis_status_rq;
extern fiid_template_t tmpl_cmd_get_chassis_status_rs;

int8_t fill_cmd_get_chassis_status (fiid_obj_t obj_cmd);

int8_t fill_cmd_chassis_control (uint8_t chassis_control, fiid_obj_t obj_cmd);

int8_t fill_cmd_chassis_identify (uint8_t *identify_interval,
                                  uint8_t *force_identify_flag,
                                  fiid_obj_t obj_cmd);

int8_t fill_cmd_set_power_restore_policy (uint8_t power_restore_policy, fiid_obj_t obj_cmd);

#ifdef __cplusplus
}
#endif



#endif
