/*
   ipmi-dev-global-cmds.h - IPMI Device Global Commands

   Copyright (C) 2003 FreeIPMI Core Team

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

#ifndef _IPMI_DEV_GLOBAL_CMDS_H
#define	_IPMI_DEV_GLOBAL_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_DEV_ID_UNSPECIFIED     0x00
#define IPMI_MANF_ID_UNSPECIFIED    0x000000
#define IPMI_MANF_ID_RESERVED       0x0FFFFF
#define IPMI_MANF_ID_INTEL          0x157

#define IPMI_PROD_ID_SR870BN4       0x100
#define IPMI_PROD_ID_CDC6440        0x100

#define IPMI_PROD_ID_SE7501WV2      0x1B
#define IPMI_PROD_ID_CDC1620        0x1B

extern fiid_template_t tmpl_cmd_get_dev_id_rq;
extern fiid_template_t tmpl_cmd_get_dev_id_rs;
extern fiid_template_t tmpl_cmd_get_dev_id_sr870bn4_rs;

int8_t fill_cmd_get_dev_id (fiid_obj_t obj_data_rq);
int8_t ipmi_kcs_get_dev_id (fiid_obj_t obj_data_rs);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-dev-global-cmds.h */
