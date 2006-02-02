/* 
   ipmi-sdr-repo-cmds.h - IPMI SDR Repository commands

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

#ifndef _IPMI_SDR_REPO_CMDS_H
#define _IPMI_SDR_REPO_CMDS_H

#define IPMI_SDR_MODAL_NON_MODAL_REPO_UPDATE_OP_UNSPECIFIED    0x0
#define IPMI_SDR_NON_MODAL_REPO_UPDATE_OP_SUPPORTED            0x1
#define IPMI_SDR_MODAL_REPO_UPDATE_OP_SUPPORTED                0x2
#define IPMI_SDR_MODAL_NON_MODAL_REPO_UPDATE_OP_SUPPORTED      0x3

#define IPMI_SDR_IPMB_SLAVE_ADDRESS    0x0
#define IPMI_SDR_SYSTEM_SOFTWARE_ID    0x1



#ifdef __cplusplus
extern "C" {
#endif


extern fiid_template_t tmpl_get_sdr_repo_info_rq;
extern fiid_template_t tmpl_get_sdr_repo_info_rs;

extern fiid_template_t tmpl_get_sdr_repo_alloc_info_rq;
extern fiid_template_t tmpl_get_sdr_repo_alloc_info_rs;

extern fiid_template_t tmpl_reserve_sdr_repo_rq;
extern fiid_template_t tmpl_reserve_sdr_repo_rs;

extern fiid_template_t tmpl_get_sdr_rq;
extern fiid_template_t tmpl_get_sdr_rs;

int8_t ipmi_cmd_get_sdr_repo_info2 (ipmi_device_t *dev, 
				    fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_sdr_repo_alloc_info2 (ipmi_device_t *dev, 
					  fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_reserve_sdr_repo2 (ipmi_device_t *dev, 
				   fiid_obj_t obj_cmd_rs);
int8_t ipmi_cmd_get_sdr2 (ipmi_device_t *dev, 
			  uint16_t record_id, 
			  fiid_obj_t obj_cmd_rs, 
			  uint8_t *sensor_record,
			  uint32_t *sensor_record_len);


#ifdef __cplusplus
}
#endif

#endif
