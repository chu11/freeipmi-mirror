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

#ifndef _IPMI_KCS_INTERFACE_H
#define _IPMI_KCS_INTERFACE_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

extern fiid_template_t tmpl_hdr_kcs;

int8_t fill_hdr_ipmi_kcs (uint8_t lun, 
			  uint8_t fn, 
			  fiid_obj_t obj_kcs_hdr);

int32_t assemble_ipmi_kcs_pkt (fiid_obj_t obj_kcs_hdr, 
                               fiid_obj_t obj_cmd, 
                               uint8_t *pkt, 
                               uint32_t pkt_len);

int32_t unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
                                 uint32_t pkt_len, 
                                 fiid_obj_t obj_kcs_hdr, 
                                 fiid_obj_t obj_cmd);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-interface.h */

