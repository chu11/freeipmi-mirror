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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _IPMI_CHASSIS_CMDS_H
#define _IPMI_CHASSIS_CMDS_H

#define IPMI_POWER_RESTORE_POLICY_NO_CHANGE                         0x3
#define IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_MAINS    0x2
#define IPMI_POWER_RESTORE_POLICY_RESTORE_POWER                     0x1
#define IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWER_OFF             0x0

#define IPMI_POWER_RESTORE_POLICY_VALID(policy)                         \
((policy == IPMI_POWER_RESTORE_POLICY_NO_CHANGE ||                      \
  policy == IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_MAINS || \
  policy == IPMI_POWER_RESTORE_POLICY_RESTORE_POWER ||                  \
  policy == IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWER_OFF) ? 1 : 0)

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

int8_t ipmi_cmd_set_power_restore_policy2 (ipmi_device_t *dev, 
					   u_int8_t power_restore_policy, 
					   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_chassis_status2 (ipmi_device_t *dev, 
				     fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif



#endif
