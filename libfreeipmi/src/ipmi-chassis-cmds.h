/*
   ipmi-chassis-cmds.h - IPMI Chassis Commands

   Copyright (C) 2003 FreeIPMI Core Team

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

#ifndef _IPMI_CHASSIS_CMDS_H
#define _IPMI_CHASSIS_CMDS_H

#define IPMI_POWER_RESTORE_POLICY_NO_CHANGE                         0x3
#define IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_MAINS    0x2
#define IPMI_POWER_RESTORE_POLICY_RESTORE_POWER                     0x1
#define IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWER_OFF             0x0

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_GET_CHASSIS_STATUS_RESTORE_POLICY_POWER_OFF      0x00
#define IPMI_GET_CHASSIS_STATUS_RESTORE_POLICY_POWER_RESTORE  0x01
#define IPMI_GET_CHASSIS_STATUS_RESTORE_POLICY_POWER_UP       0x02
#define IPMI_GET_CHASSIS_STATUS_RESTORE_POLICY_UNKNOWN        0x03

#define IPMI_CHASSIS_CTRL_POWER_DOWN         0x00
#define IPMI_CHASSIS_CTRL_POWER_UP           0x01
#define IPMI_CHASSIS_CTRL_POWER_CYCLE        0x02
#define IPMI_CHASSIS_CTRL_HARD_RESET         0x03
#define IPMI_CHASSIS_CTRL_PULSE_DIAG_INTR    0x04
#define IPMI_CHASSIS_CTRL_INIT_SOFT_SHUTDOWN 0x05


#if 0
#pragma pack(1)
typedef struct ipmi_cmd_get_chassis_status_rq
{
  u_int8_t cmd;
  struct {
  } data;
} ipmi_cmd_get_chassis_status_rq_t;

typedef struct ipmi_cmd_get_chassis_status_rs
{
  u_int8_t cmd;
  u_int8_t comp_code;
  struct {
    struct {
      u_int8_t power_on:1;
      u_int8_t power_overload:1;
      u_int8_t interlock:1;
      u_int8_t power_fault:1;
      u_int8_t power_control_fault:1;
      u_int8_t power_restore_policy:2;
      u_int8_t reserved:1;
    } power_state;
    struct {
      u_int8_t ac_failed:1;
      u_int8_t power_down_overload:1;
      u_int8_t power_down_interlock:1;
      u_int8_t power_down_fault:1;
      u_int8_t power_on_via_ipmi:1;
      u_int8_t reserved:3;
    } last_power_event;
    struct {
      u_int8_t chassis_intrusion_active:1;
      u_int8_t front_panel_lockout_active:1;
      u_int8_t drive_fault:1;
      u_int8_t cooling_fan_fault:1;
      u_int8_t reserved:4;
    } misc_chassis;
  } data;
} ipmi_cmd_get_chassis_status_rs_t;

typedef struct ipmi_cmd_chassis_ctrl_rq
{
  u_int8_t cmd;
  struct {
    u_int8_t chassis_ctrl:4;
    u_int8_t reserved:4;
  } data;
} ipmi_cmd_chassis_ctrl_rq_t;

/* NOTE: 
   The implementation is allowed to return the completion code prior
   to performing the selected control action if necessary. 
*/
typedef struct ipmi_cmd_chassis_ctrl_rs
{
  u_int8_t cmd;
  u_int8_t comp_code;
  struct {
  } data;
} ipmi_cmd_chassis_ctrl_rs_t;
#pragma pack(0)
#endif

extern fiid_template_t tmpl_set_power_restore_policy_rq;
extern fiid_template_t tmpl_set_power_restore_policy_rs;
extern fiid_template_t tmpl_cmd_chassis_ctrl_rq;
extern fiid_template_t tmpl_cmd_chassis_ctrl_rs;
extern fiid_template_t tmpl_cmd_get_chassis_status_rq;
extern fiid_template_t tmpl_cmd_get_chassis_status_rs;

int8_t fill_cmd_set_power_restore_policy (fiid_obj_t obj_data_rq, 
                                          u_int8_t power_restore_policy);

int8_t ipmi_set_power_restore_policy (u_int8_t power_restore_policy, 
				      fiid_obj_t obj_data_rs);

int8_t fill_cmd_get_chassis_status (fiid_obj_t obj_cmd);
int8_t ipmi_get_chassis_status (fiid_obj_t obj_data_rs);

int8_t fill_cmd_chassis_ctrl (u_int8_t chassis_ctrl, fiid_obj_t obj_cmd);


#ifdef __cplusplus
}
#endif



#endif
