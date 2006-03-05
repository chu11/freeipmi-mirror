/* 
   ipmi-ssif-api-udm.h: IPMI UDM - SMBus System Interface - SMS Api

   Copyright (C) 2005 FreeIPMI Core Team

   Based on ipmitool.c provided by Amitoj Singh <amitoj@fnal.gov> and 
   Don Holmgren <djholm@fnal.gov>

   Under GNU/Linux, requires i2c-dev, i2c-i801, i2c-core drivers version >= 2.8.7

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

#ifndef IPMI_SSIF_API_UDM_H
#define IPMI_SSIF_API_UDM_H

#include <stdint.h>
#include <freeipmi/fiid.h>
#include <freeipmi/udm/ipmi-udm.h>

int ipmi_ssif_cmd (ipmi_device_t *dev, 
		   fiid_obj_t obj_cmd_rq, 
		   fiid_obj_t obj_cmd_rs);
int8_t ipmi_ssif_cmd_raw (ipmi_device_t *dev, 
			  uint8_t *buf_rq, 
			  size_t buf_rq_len, 
			  uint8_t *buf_rs, 
			  size_t *buf_rs_len);

#endif /* IPMI_SSIF_API_UDM_H */
