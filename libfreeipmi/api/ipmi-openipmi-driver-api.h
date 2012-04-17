/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef IPMI_OPENIPMI_DRIVER_API_H
#define IPMI_OPENIPMI_DRIVER_API_H

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/fiid/fiid.h>

int ipmi_openipmi_cmd_api (ipmi_ctx_t ctx,
                           fiid_obj_t obj_cmd_rq,
                           fiid_obj_t obj_cmd_rs);

int ipmi_openipmi_cmd_api_ipmb (ipmi_ctx_t ctx,
                                fiid_obj_t obj_cmd_rq,
                                fiid_obj_t obj_cmd_rs);

int ipmi_openipmi_cmd_raw_api (ipmi_ctx_t ctx,
                               const void *buf_rq,
                               unsigned int buf_rq_len,
                               void *buf_rs,
                               unsigned int buf_rs_len);

int ipmi_openipmi_cmd_raw_api_ipmb (ipmi_ctx_t ctx,
				    const void *buf_rq,
				    unsigned int buf_rq_len,
				    void *buf_rs,
				    unsigned int buf_rs_len);

#endif /* IPMI_OPENIPMI_DRIVER_API_H */
