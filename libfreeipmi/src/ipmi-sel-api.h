/* 
   ipmi-sel-api.h - IPMI SEL commands API

   Copyright (C) 2005 FreeIPMI Core Team

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

#ifndef _IPMI_SEL_API_H
#define _IPMI_SEL_API_H

#define SEL_RECORD_SIZE 16

struct sel_descriptor
{
  u_int16_t first_record_id;
  u_int16_t next_record_id;
};
typedef struct sel_descriptor sel_descriptor_t;

enum sel_info_flag
  {
    get_sel_alloc_info_cmd_support = 1,
    reserve_sel_cmd_support = 2,
    partial_add_sel_entry_cmd_support = 4,
    delete_sel_cmd_support = 8,
    overflow_flag = 16,
  };
typedef enum sel_info_flag sel_info_flag_t;

struct sel_info
{
  unsigned long version_major;
  unsigned long version_minor;
  unsigned long entry_count;
  unsigned long free_space;
  unsigned long last_add_time;
  unsigned long last_erase_time;
  unsigned long flags;
};
typedef struct sel_info sel_info_t;

int ipmi_sel_get_first_entry (ipmi_device_t *dev, 
			      sel_descriptor_t *seld, 
			      u_int8_t *record_data);
int ipmi_sel_get_next_entry (ipmi_device_t *dev, 
			     sel_descriptor_t *seld, 
			     u_int8_t *record_data);
int get_sel_info (ipmi_device_t *dev, 
		  sel_info_t *pinfo);

#endif 
