/* 
   ipmi-sel-cmds.h - IPMI System Event Log Commands
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _IPMI_SEL_CMDS_H
#define _IPMI_SEL_CMDS_H

#include <stdint.h>
#include <freeipmi/fiid.h>

#define IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY 0x0000
#define IPMI_SEL_GET_RECORD_ID_LAST_ENTRY  0xFFFF

#define IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE        0xAA
#define IPMI_SEL_CLEAR_OPERATION_GET_ERASURE_STATUS    0x0

#define IPMI_SEL_CLEAR_OPERATION_VALID(__val) \
        (((__val) == IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE \
          || (__val) == IPMI_SEL_CLEAR_OPERATION_GET_ERASURE_STATUS) ? 1 : 0)

#define IPMI_SEL_CLEAR_ERASURE_IN_PROGRESS    0x0
#define IPMI_SEL_CLEAR_ERASE_COMPLETED        0x1

#define IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ  0xFF

#ifdef __cplusplus
extern "C" {
#endif

extern fiid_template_t tmpl_get_sel_info_rq;
extern fiid_template_t tmpl_get_sel_info_rs;

extern fiid_template_t tmpl_get_sel_allocation_info_rq;
extern fiid_template_t tmpl_get_sel_allocation_info_rs;

extern fiid_template_t tmpl_reserve_sel_rq;
extern fiid_template_t tmpl_reserve_sel_rs;

extern fiid_template_t tmpl_get_sel_entry_rq;
extern fiid_template_t tmpl_get_sel_entry_rs;

extern fiid_template_t tmpl_delete_sel_entry_rq;
extern fiid_template_t tmpl_delete_sel_entry_rs;

extern fiid_template_t tmpl_clear_sel_rq;
extern fiid_template_t tmpl_clear_sel_rs;

int8_t fill_cmd_get_sel_info (fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_sel_allocation_info (fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_reserve_sel (fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_sel_entry (uint16_t reservation_id,
                               uint16_t record_id, 
                               uint8_t offset_into_record,
                               uint8_t bytes_to_read,
                               fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_delete_sel_entry (uint16_t reservation_id,
                                  uint16_t record_id,
                                  fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_clear_sel (uint16_t reservation_id, 
                           uint8_t operation, 
                           fiid_obj_t obj_cmd_rq);

#ifdef __cplusplus
}
#endif

#endif
