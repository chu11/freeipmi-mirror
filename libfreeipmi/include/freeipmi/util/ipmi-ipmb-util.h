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

#ifndef _IPMI_IPMB_UTIL_H
#define _IPMI_IPMB_UTIL_H       1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

/* returns 1 on pass, 0 on fail, -1 on error */
int ipmi_ipmb_check_rq_seq (fiid_obj_t obj_ipmb_msg_hdr, uint8_t rq_seq);

/* returns 1 on pass, 0 on fail, -1 on error */
int ipmi_ipmb_check_checksum (uint8_t rq_addr,
                              fiid_obj_t obj_ipmb_msg_hdr,
                              fiid_obj_t obj_cmd,
                              fiid_obj_t obj_ipmb_msg_trlr);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-ipmb-util.h */


