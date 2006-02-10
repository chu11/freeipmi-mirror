/* 
   ipmi-serial-cmds.h - IPMI serial port settings commands

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

#ifndef _IPMI_SERIAL_CMDS_H
#define	_IPMI_SERIAL_CMDS_H

#define IPMI_GET_SERIAL_PARAMETER                          0x0
#define IPMI_GET_SERIAL_PARAMETER_REVISION_ONLY            0x1

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_set_serial_conf_param_connmode_rq;
extern fiid_template_t tmpl_set_serial_conf_param_pageblackout_rq;
extern fiid_template_t tmpl_set_serial_conf_param_retry_rq;
extern fiid_template_t tmpl_set_serial_conf_param_commbits_rq;
extern fiid_template_t tmpl_get_serial_conf_param_rq;

extern fiid_template_t tmpl_set_serial_conf_param_rs;
extern fiid_template_t tmpl_get_serial_conf_param_connmode_rs;
extern fiid_template_t tmpl_get_serial_conf_param_pageblackout_rs;
extern fiid_template_t tmpl_get_serial_conf_param_retry_rs;
extern fiid_template_t tmpl_get_serial_conf_param_commbits_rs;

int fill_set_serial_connmode (uint8_t channel_number,
                              uint8_t basic_mode_enable,
                              uint8_t ppp_mode_enable,
                              uint8_t terminal_mode_enable,
                              uint8_t direct,
                              fiid_obj_t obj_data_rq);
int8_t fill_set_serial_page_blackout_interval (uint8_t channel_number,
                                               uint8_t page_blackout_interval,
                                               fiid_obj_t obj_data_rq);
int8_t fill_set_serial_retry_time (uint8_t channel_number,
                                   uint8_t retry_time,
                                   fiid_obj_t obj_data_rq);
int8_t fill_set_serial_comm_bits (uint8_t channel_number,
                                  uint8_t dtr_hangup,
                                  uint8_t flow_control,
                                  uint8_t bit_rate,
                                  fiid_obj_t obj_data_rq);
int8_t fill_get_serial_conf_param (uint8_t parameter_selector,
                                   uint8_t channel_number,
                                   uint8_t parameter_type,
                                   uint8_t set_selector,
                                   uint8_t block_selector,
                                   fiid_obj_t obj_data_rq);

#ifdef __cplusplus
}
#endif

#endif
