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

#define IPMI_SERIAL_CONF_GET_PARAMETER             0x0
#define IPMI_SERIAL_CONF_GET_PARAMETER_REVISION    0x1


#ifdef __cplusplus
extern "C" {
#endif
extern fiid_template_t tmpl_set_serial_conf_param_rs;

extern fiid_template_t tmpl_get_serial_conf_param_connmode_rs;
extern fiid_template_t tmpl_get_serial_conf_param_pageblackout_rs;
extern fiid_template_t tmpl_get_serial_conf_param_retry_rs;
extern fiid_template_t tmpl_get_serial_conf_param_commbits_rs;

int8_t ipmi_set_serial_connmode (u_int8_t channel_number, 
				 u_int8_t basic_mode_enable,
				 u_int8_t ppp_mode_enable,
				 u_int8_t terminal_mode_enable,
				 u_int8_t direct,
				 fiid_obj_t obj_data_rs);

int8_t ipmi_set_serial_page_blackout_interval (u_int8_t channel_number, 
                                               u_int8_t page_blackout_interval, 
                                               fiid_obj_t obj_data_rs);

int8_t ipmi_set_serial_retry_time (u_int8_t channel_number, 
                                   u_int8_t retry_time, 
                                   fiid_obj_t obj_data_rs);

int8_t ipmi_set_serial_comm_bits (u_int8_t channel_number, 
                                  u_int8_t dtr_hangup,
                                  u_int8_t flow_control,
                                  u_int8_t bit_rate,
                                  fiid_obj_t obj_data_rs);

int8_t ipmi_get_serial_connmode (u_int8_t channel_number,
                                 u_int8_t parameter_type,
                                 u_int8_t set_selector,
                                 u_int8_t block_selector,
                                 fiid_obj_t obj_data_rs);

int8_t ipmi_get_serial_page_blackout (u_int8_t channel_number,
                                      u_int8_t parameter_type,
                                      u_int8_t set_selector,
                                      u_int8_t block_selector,
                                      fiid_obj_t obj_data_rs);

int8_t ipmi_get_serial_retry_time (u_int8_t channel_number,
                                   u_int8_t parameter_type,
                                   u_int8_t set_selector,
                                   u_int8_t block_selector,
                                   fiid_obj_t obj_data_rs);

int8_t ipmi_get_serial_comm_bits (u_int8_t channel_number,
                                  u_int8_t parameter_type,
                                  u_int8_t set_selector,
                                  u_int8_t block_selector,
                                  fiid_obj_t obj_data_rs);

int8_t ipmi_cmd_set_serial_connmode2 (ipmi_device_t *dev, 
				      u_int8_t channel_number, 
				      u_int8_t basic_mode_enable,
				      u_int8_t ppp_mode_enable,
				      u_int8_t terminal_mode_enable,
				      u_int8_t direct,
				      fiid_obj_t *obj_data_rs);
int8_t ipmi_cmd_set_serial_page_blackout_interval2 (ipmi_device_t *dev, 
						    u_int8_t channel_number, 
						    u_int8_t page_blackout_interval, 
						    fiid_obj_t *obj_data_rs);
int8_t ipmi_cmd_set_serial_retry_time2 (ipmi_device_t *dev, 
					u_int8_t channel_number, 
					u_int8_t retry_time, 
					fiid_obj_t *obj_data_rs);
int8_t ipmi_cmd_set_serial_comm_bits2 (ipmi_device_t *dev, 
				       u_int8_t channel_number, 
				       u_int8_t dtr_hangup,
				       u_int8_t flow_control,
				       u_int8_t bit_rate,
				       fiid_obj_t *obj_data_rs);

int8_t ipmi_cmd_get_serial_connmode2 (ipmi_device_t *dev, 
				      u_int8_t channel_number,
				      u_int8_t parameter_type,
				      u_int8_t set_selector,
				      u_int8_t block_selector,
				      fiid_obj_t *obj_data_rs);
int8_t ipmi_cmd_get_serial_page_blackout2 (ipmi_device_t *dev, 
					   u_int8_t channel_number,
					   u_int8_t parameter_type,
					   u_int8_t set_selector,
					   u_int8_t block_selector,
					   fiid_obj_t *obj_data_rs);
int8_t ipmi_cmd_get_serial_retry_time2 (ipmi_device_t *dev, 
					u_int8_t channel_number,
					u_int8_t parameter_type,
					u_int8_t set_selector,
					u_int8_t block_selector,
					fiid_obj_t *obj_data_rs);
int8_t ipmi_cmd_get_serial_comm_bits2 (ipmi_device_t *dev, 
				       u_int8_t channel_number,
				       u_int8_t parameter_type,
				       u_int8_t set_selector,
				       u_int8_t block_selector,
				       fiid_obj_t *obj_data_rs);


#ifdef __cplusplus
}
#endif

#endif
