/* 
   ipmi-sel-cmds.c - IPMI System Event Log Commands
   
   Copyright (C) 2003 - 2004 FreeIPMI Core Team

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>
#endif

#include <freeipmi.h>

fiid_template_t tmpl_get_sel_info_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_sel_info_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {4,  "sel_version_major"}, 
    {4,  "sel_version_minor"}, 
    {16, "log_entry_count"}, //LS byte first
    {16, "free_space"}, //LS byte first
    {32, "recent_addition_timestamp"}, //LS byte first
    {32, "recent_erase_timestamp"}, 
    {1,  "get_sel_alloc_info_cmd_support"}, 
    {1,  "reserve_sel_cmd_support"}, 
    {1,  "partial_add_sel_entry_cmd_support"}, 
    {1,  "delete_sel_cmd_support"}, 
    {3,  "reserved"}, 
    {1,  "overflow_flag"}, 
    {0,  ""}
  };


fiid_template_t tmpl_get_sel_alloc_info_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_sel_alloc_info_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "no_of_possible_alloc_units"}, 
    {16, "allocation_unit_size"}, 
    {16, "no_of_free_alloc_units"}, 
    {16, "larget_free_block"}, 
    {8,  "max_record_size"}, 
    {0,  ""}
  };


fiid_template_t tmpl_reserve_sel_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_reserve_sel_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "reservation_id"}, //LS byte first
    {0,  ""}
  };


fiid_template_t tmpl_get_sel_entry_rq =
  {
    {8,  "cmd"}, 
    {16, "reservation_id"}, //LS byte first
    {16, "record_id"}, //LS byte first
    {8,  "record_offset"}, 
    {8,  "bytes_read"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sel_entry_rs =
  {
    {8,   "cmd"}, 
    {8,   "comp_code"}, 
    {16,  "next_record_id"}, //LS byte first
    {128, "record_data"}, 
    {0,   ""}
  };


fiid_template_t tmpl_delete_sel_entry_rq =
  {
    {8,  "cmd"}, 
    {16, "reservation_id"}, //LS byte first
    {16, "record_id"}, //LS byte first
    {0,  ""}
  };


fiid_template_t tmpl_delete_sel_entry_rs =
  {
    {8,   "cmd"}, 
    {8,   "comp_code"}, 
    {16,  "record_id"}, //LS byte first
    {0,   ""}
  };


fiid_template_t tmpl_clear_sel_rq =
  {
    {8,  "cmd"}, 
    {16, "reservation_id"}, //LS byte first
    {8, "C"}, 
    {8, "L"}, 
    {8, "R"}, 
    {8, "opcode"}, 
    {0,  ""}
  };


fiid_template_t tmpl_clear_sel_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {4, "erasure_progress"}, 
    {4, "reserved"}, 
    {0, ""}
  };


int8_t 
fill_kcs_get_sel_info (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sel_info_rq, 
		"cmd", 
		IPMI_CMD_SEL_DEV_CMDS_GET_SEL_INFO);
  return 0;
}

int8_t 
ipmi_kcs_get_sel_info (u_int16_t sms_io_base, fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_sel_info_rq);
  fill_kcs_get_sel_info (obj_data_rq);
  status = ipmi_kcs_cmd (sms_io_base, IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_get_sel_info_rq, 
			 obj_data_rs, tmpl_get_sel_info_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_sel_alloc_info (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sel_alloc_info_rq, 
		"cmd", 
		IPMI_CMD_GET_SEL_ALLOCATION_INFO);
  return 0;
}

int8_t 
ipmi_kcs_get_sel_alloc_info (u_int16_t sms_io_base, fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_sel_alloc_info_rq);
  fill_kcs_get_sel_alloc_info (obj_data_rq);
  status = ipmi_kcs_cmd (sms_io_base, IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_get_sel_alloc_info_rq, 
			 obj_data_rs, tmpl_get_sel_alloc_info_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_reserve_sel (fiid_obj_t obj_data_rq)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_reserve_sel_rq, 
		"cmd", 
		IPMI_CMD_RESERVE_SEL);
  return 0;
}

int8_t 
ipmi_kcs_reserve_sel (u_int16_t sms_io_base, fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_reserve_sel_rq);
  fill_kcs_reserve_sel (obj_data_rq);
  status = ipmi_kcs_cmd (sms_io_base, IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_reserve_sel_rq, 
			 obj_data_rs, tmpl_reserve_sel_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_get_sel_entry (fiid_obj_t obj_data_rq, u_int16_t record_id)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sel_entry_rq, 
		"cmd", 
		IPMI_CMD_GET_SEL_ENTRY);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sel_entry_rq, 
		"record_id", 
		record_id);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sel_entry_rq, 
		"bytes_read", 
		0xff);
  
  return 0;
}

int8_t 
ipmi_kcs_get_sel_entry (u_int16_t sms_io_base, u_int16_t record_id, fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_get_sel_entry_rq);
  fill_kcs_get_sel_entry (obj_data_rq, record_id);
  status = ipmi_kcs_cmd (sms_io_base, IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_get_sel_entry_rq, 
			 obj_data_rs, tmpl_get_sel_entry_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_delete_sel_entry (fiid_obj_t obj_data_rq, 
			   u_int16_t reservation_id, 
			   u_int16_t record_id)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_delete_sel_entry_rq, 
		"cmd", 
		IPMI_CMD_DELETE_SEL_ENTRY);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_delete_sel_entry_rq, 
		"reservation_id", 
		reservation_id);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_delete_sel_entry_rq, 
		"record_id", 
		record_id);
  
  return 0;
}

int8_t 
ipmi_kcs_delete_sel_entry (u_int16_t sms_io_base, 
			   u_int16_t reservation_id, 
			   u_int16_t record_id, 
			   fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_delete_sel_entry_rq);
  fill_kcs_delete_sel_entry (obj_data_rq, reservation_id, record_id);
  status = ipmi_kcs_cmd (sms_io_base, IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_delete_sel_entry_rq, 
			 obj_data_rs, tmpl_delete_sel_entry_rs);
  free (obj_data_rq);
  return status;
}

int8_t 
fill_kcs_clear_sel (fiid_obj_t obj_data_rq, u_int16_t reservation_id, u_int8_t opcode)
{
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_clear_sel_rq, 
		"cmd", 
		IPMI_CMD_CLEAR_SEL);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_clear_sel_rq, 
		"reservation_id", 
		reservation_id);
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_clear_sel_rq, 
		"C", 
		'C');
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_clear_sel_rq, 
		"L", 
		'L');
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_clear_sel_rq, 
		"R", 
		'R');
  
  FIID_OBJ_SET (obj_data_rq, 
		tmpl_clear_sel_rq, 
		"opcode", 
		opcode);
  
  return 0;
}

int8_t 
ipmi_kcs_clear_sel (u_int16_t sms_io_base, 
		    u_int16_t reservation_id, 
		    u_int8_t opcode, 
		    fiid_obj_t obj_data_rs)
{
  fiid_obj_t obj_data_rq; 
  int8_t status;
  
  obj_data_rq = fiid_obj_alloc (tmpl_clear_sel_rq);
  fill_kcs_clear_sel (obj_data_rq, reservation_id, opcode);
  status = ipmi_kcs_cmd (sms_io_base, IPMI_BMC_IPMB_LUN_BMC, IPMI_NET_FN_STORAGE_RQ, 
			 obj_data_rq, tmpl_clear_sel_rq, 
			 obj_data_rs, tmpl_clear_sel_rs);
  free (obj_data_rq);
  return status;
}

