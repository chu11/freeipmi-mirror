/* 
   ipmi-pef-cmds.h - IPMI System Event Log Commands
   
   Copyright (C) 2003 - 2004 FreeIPMI Core Team

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

/* $Id: ipmi-pef-cmds.h,v 1.1 2004-10-26 22:16:23 itz Exp $ */

#ifndef _IPMI_PEF_CMDS_H
#define _IPMI_PEF_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_get_pef_caps_rq;
extern fiid_template_t tmpl_get_pef_caps_rs;

extern fiid_template_t tmpl_arm_pef_postpone_timer_rq;
extern fiid_template_t tmpl_arm_pef_postpone_timer_rs;

extern fiid_template_t tmpl_set_last_processed_event_rq;
extern fiid_template_t tmpl_set_last_processed_event_rs;

extern fiid_template_t tmpl_get_last_processed_event_rq;
extern fiid_template_t tmpl_get_last_processed_event_rs;

extern fiid_template_t tmpl_pet_ack_rq;
extern fiid_template_t tmpl_pet_ack_rs;

enum which_event
  {
    last_software_event = 0,
    last_bmc_event = 1,
  };
typedef enum which_event which_event_t;

int8_t ipmi_kcs_get_pef_caps (u_int16_t sms_io_base, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_arm_pef_postpone_timer (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int8_t countdown);
int8_t ipmi_kcs_set_last_processed_event (u_int16_t sms_io_base, fiid_obj_t obj_data_rs,
                                          which_event_t which, u_int16_t id);
int8_t ipmi_kcs_get_last_processed_event (u_int16_t sms_io_base, fiid_obj_t obj_data_rs);
int8_t ipmi_pet_ack (u_int16_t sms_io_base, fiid_obj_t obj_data_rs, u_int16_t sequence_number,
                     u_int32_t timestamp, u_int8_t source_type, u_int8_t sensor_device,
                     u_int8_t sensor_number, u_int32_t event_data);

#ifdef __cplusplus
}
#endif

#endif
