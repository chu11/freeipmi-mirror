/* 
   ipmi-sel-api.c - IPMI SEL commands API

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

#include "freeipmi.h"

int 
ipmi_sel_get_first_entry (ipmi_device_t *dev, 
			  sel_descriptor_t *seld, 
			  u_int8_t *record_data)
{
  fiid_obj_t obj_cmd_rs;
  u_int64_t val;
  
  fiid_obj_alloca (obj_cmd_rs, tmpl_get_sel_entry_rs);
  if (ipmi_cmd_get_sel_entry2 (dev, 
			       IPMI_SEL_FIRST_ENTRY, 
			       obj_cmd_rs) != 0)
    {
      ipmi_error (obj_cmd_rs, "ipmi_cmd_get_sel_entry2()");
      return (-1);
    }
  
  seld->first_record_id = IPMI_SEL_FIRST_ENTRY;
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_entry_rs, 
		"next_record_id", 
		&val);
  seld->next_record_id = val;
  
  fiid_obj_get_data (obj_cmd_rs, 
		     tmpl_get_sel_entry_rs, 
		     "record_data", 
		     record_data);
  
  return 0;
}

int 
ipmi_sel_get_next_entry (ipmi_device_t *dev, 
			 sel_descriptor_t *seld, 
			 u_int8_t *record_data)
{
  fiid_obj_t obj_cmd_rs;
  u_int64_t val;
  
  if (seld->next_record_id == IPMI_SEL_LAST_ENTRY)
    return (-1);
  
  fiid_obj_alloca (obj_cmd_rs, tmpl_get_sel_entry_rs);
  if (ipmi_cmd_get_sel_entry2 (dev, 
			       seld->next_record_id, 
			       obj_cmd_rs) != 0)
    {
      ipmi_error (obj_cmd_rs, "ipmi_cmd_get_sel_entry2()");
      return (-1);
    }
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_entry_rs, 
		"next_record_id", 
		&val);
  seld->next_record_id = val;
  
  fiid_obj_get_data (obj_cmd_rs, 
		     tmpl_get_sel_entry_rs, 
		     "record_data", 
		     record_data);
  
  return 0;
}

int 
get_sel_info (ipmi_device_t *dev, sel_info_t *pinfo)
{
  fiid_obj_t obj_cmd_rs;
  u_int64_t val;
  
  fiid_obj_alloca (obj_cmd_rs, tmpl_get_sel_info_rs);
  if (ipmi_cmd_get_sel_info2 (dev, obj_cmd_rs) != 0)
    {
      ipmi_error (obj_cmd_rs, "ipmi_cmd_get_sel_info2()");
      return (-1);
    }
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"sel_version_major", 
		&val);
  pinfo->version_major = val;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"sel_version_minor", 
		&val);
  pinfo->version_minor = val;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"log_entry_count", 
		&val);
  pinfo->entry_count = val;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"free_space", 
		&val);
  pinfo->free_space = val;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"recent_addition_timestamp", 
		&val);
  pinfo->last_add_time = val;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"recent_erase_timestamp", 
		&val);
  pinfo->last_erase_time = val;
  
  pinfo->flags = 0;
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"get_sel_alloc_info_cmd_support", 
		&val);
  if (val) pinfo->flags |= get_sel_alloc_info_cmd_support;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"reserve_sel_cmd_support", 
		&val);
  if (val) pinfo->flags |= reserve_sel_cmd_support;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"partial_add_sel_entry_cmd_support", 
		&val);
  if (val) pinfo->flags |= partial_add_sel_entry_cmd_support;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"delete_sel_cmd_support", 
		&val);
  if (val) pinfo->flags |= delete_sel_cmd_support;
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sel_info_rs, 
		"overflow_flag", 
		&val);
  if (val) pinfo->flags |= overflow_flag;
  
  return 0;
}

