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

#ifndef _IPMI_IPMB_INTERFACE_H
#define	_IPMI_IPMB_INTERFACE_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_IPMB_REQUESTER_SEQUENCE_NUMBER_MAX    0x3F /* 111111b */

extern fiid_template_t tmpl_ipmb_msg_hdr_rq;
extern fiid_template_t tmpl_ipmb_msg_hdr_rs;
extern fiid_template_t tmpl_ipmb_msg_trlr;
extern fiid_template_t tmpl_ipmb_msg;

int8_t fill_ipmb_msg_hdr (uint8_t rs_addr,
                          uint8_t net_fn, 
                          uint8_t rs_lun,
                          uint8_t rq_addr,
                          uint8_t rq_lun,
                          uint8_t rq_seq, 
                          fiid_obj_t obj_ipmb_msg_hdr);

int32_t assemble_ipmi_ipmb_msg (fiid_obj_t obj_ipmb_msg_hdr, 
                                fiid_obj_t obj_cmd, 
                                fiid_obj_t obj_ipmb_msg);

int8_t unassemble_ipmi_ipmb_msg (fiid_obj_t obj_ipmb_msg,
                                 fiid_obj_t obj_ipmb_msg_hdr,
                                 fiid_obj_t obj_cmd,
                                 fiid_obj_t obj_ipmb_msg_trlr);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-ipmb-interface.h */


