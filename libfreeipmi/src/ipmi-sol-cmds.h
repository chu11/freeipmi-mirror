/*
   ipmi-sol-cmds.h - IPMI SOL Commands

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

#ifndef _IPMI_SOL_CMDS_H
#define _IPMI_SOL_CMDS_H

#define IPMI_SOL_PAYLOAD_DISABLE    0x0
#define IPMI_SOL_PAYLOAD_ENABLE     0x1

#define IPMI_SOL_PAYLOAD_VALID(__val) \
        (((__val) == IPMI_SOL_PAYLOAD_DISABLE \
          || (__val) == IPMI_SOL_PAYLOAD_ENABLE) ? 1 : 0)

#define IPMI_GET_SOL_PARAMETER                          0x0
#define IPMI_GET_SOL_PARAMETER_REVISION_ONLY            0x1

#define IPMI_GET_SOL_PARAMETER_VALID(__val) \
        (((__val) == IPMI_GET_SOL_PARAMETER \
          || (__val) == IPMI_GET_SOL_PARAMETER_REVISION_ONLY) ? 1 : 0)

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_set_sol_configuration_parameters_rq;
extern fiid_template_t tmpl_set_sol_configuration_parameters_rs;
extern fiid_template_t tmpl_set_sol_sol_enable_rq;

extern fiid_template_t tmpl_get_sol_configuration_parameters_rq;
extern fiid_template_t tmpl_get_sol_configuration_parameters_rs;
extern fiid_template_t tmpl_get_sol_enable_rs;

int8_t fill_cmd_set_sol_configuration_parameters (fiid_obj_t obj_data_rq,
						  uint8_t channel_number,
						  uint8_t parameter_selector,
						  uint8_t *configuration_parameter_data,
						  uint8_t configuration_parameter_data_len);

int8_t fill_cmd_set_sol_sol_enable (uint8_t channel_number, 
				    uint8_t sol_payload,
				    fiid_obj_t obj_data_rq);

int8_t fill_cmd_get_sol_configuration_parameters (uint8_t channel_number,
						  uint8_t get_parameter,
						  uint8_t parameter_selector, 
						  uint8_t set_selector,
						  uint8_t block_selector,
						  fiid_obj_t obj_data_rq);

#ifdef __cplusplus
}
#endif


#endif
