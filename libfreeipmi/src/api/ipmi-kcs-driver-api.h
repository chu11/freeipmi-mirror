/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifndef _IPMI_KCS_DRIVER_API_H
#define _IPMI_KCS_DRIVER_API_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>
#include <freeipmi/api/ipmi-api.h>

/* High level calls */
/* BMC treats "write followed by a read" as one transaction. It is
   highly recommended to use ipmi_kcs_cmd instead. Otherwise make sure
   you check the return status of write before calling read.
*/
int8_t ipmi_kcs_cmd_api (ipmi_ctx_t ctx, 
			 fiid_obj_t obj_cmd_rq, 
			 fiid_obj_t obj_cmd_rs);

int8_t ipmi_kcs_cmd_api_ipmb (ipmi_ctx_t ctx, 
			      fiid_obj_t obj_cmd_rq, 
			      fiid_obj_t obj_cmd_rs);

int32_t ipmi_kcs_cmd_raw_api (ipmi_ctx_t ctx, 
                              uint8_t *buf_rq, 
                              size_t buf_rq_len, 
                              uint8_t *buf_rs, 
                              size_t buf_rs_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-driver-api.h */

