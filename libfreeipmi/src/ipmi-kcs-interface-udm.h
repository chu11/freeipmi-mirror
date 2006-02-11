/* 
   ipmi-kcs-interface-udm.h - IPMI UDM KCS SMS Interface

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

#ifndef _IPMI_KCS_INTERFACE_UDM_H
#define _IPMI_KCS_INTERFACE_UDM_H 1

#ifdef __cplusplus
extern "C" {
#endif

/* High level calls */
/* BMC treats "write followed by a read" as one transaction. It is
   highly recommended to use ipmi_kcs_cmd instead. Otherwise make sure
   you check the return status of write before calling read.
*/
int8_t ipmi_kcs_cmd2 (ipmi_device_t *dev, 
		      fiid_obj_t obj_cmd_rq, 
		      fiid_template_t tmpl_cmd_rq, 
		      fiid_obj_t obj_cmd_rs, 
		      fiid_template_t tmpl_cmd_rs);
int8_t ipmi_kcs_cmd_raw2 (ipmi_device_t *dev, 
			  uint8_t *buf_rq, 
			  size_t buf_rq_len, 
			  uint8_t *buf_rs, 
			  size_t *buf_rs_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-interface.h */

