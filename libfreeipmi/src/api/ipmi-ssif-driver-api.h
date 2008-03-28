/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

#ifndef IPMI_SSIF_DRIVER_API_H
#define IPMI_SSIF_DRIVER_API_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/fiid/fiid.h>

int ipmi_ssif_cmd_api (ipmi_ctx_t ctx, 
		   fiid_obj_t obj_cmd_rq, 
		   fiid_obj_t obj_cmd_rs);

int32_t ipmi_ssif_cmd_raw_api (ipmi_ctx_t ctx, 
                               uint8_t *buf_rq, 
                               size_t buf_rq_len, 
                               uint8_t *buf_rs, 
                               size_t buf_rs_len);

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SSIF_DRIVER_API_H */
