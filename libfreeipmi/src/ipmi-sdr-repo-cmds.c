/* 
   ipmi-sdr-repo-cmds.c - IPMI SDR Repository commands

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

#include "freeipmi.h"

fiid_template_t tmpl_get_sdr_repo_info_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_sdr_repo_info_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {4,  "sdr_version_major"}, 
    {4,  "sdr_version_minor"}, 
    {16, "record_count"}, //LS byte first
    {16, "free_space"}, //LS byte first
    {32, "recent_addition_timestamp"}, //LS byte first
    {32, "recent_erase_timestamp"}, 
    {1,  "get_sdr_repo_alloc_info_cmd_support"}, 
    {1,  "reserve_sdr_repo_cmd_support"}, 
    {1,  "partial_add_sdr_cmd_support"}, 
    {1,  "delete_sdr_cmd_support"}, 
    {1,  "reserved"}, 
    {2,  "modal_non_modal_sdr_repo_update_op_support"}, 
    {1,  "overflow_flag"}, 
    {0,  ""}
  };


fiid_template_t tmpl_get_sdr_repo_alloc_info_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_get_sdr_repo_alloc_info_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "no_of_possible_alloc_units"}, 
    {16, "calloction_unit_size"}, 
    {16, "no_of_free_alloc_units"}, 
    {16, "larget_free_block"}, 
    {8, "max_record_size"}, 
    {0, ""}
  };

fiid_template_t tmpl_reserve_sdr_repo_rq =
  {
    {8, "cmd"}, 
    {0, ""}
  };

fiid_template_t tmpl_reserve_sdr_repo_rs =
  {
    {8, "cmd"}, 
    {8, "comp_code"}, 
    {16, "reservation_id"}, //LS byte first
    {0, ""}
  };

fiid_template_t tmpl_get_sdr_rq =
  {
    {8,  "cmd"}, 
    {16, "reservation_id"}, //LS byte first
    {16, "record_id"}, //LS byte first
    {8,  "record_offset"}, 
    {8,  "bytes_read"}, 
    {0,  ""}
  };

fiid_template_t tmpl_get_sdr_rs =
  {
    {8,  "cmd"}, 
    {8,  "comp_code"}, 
    {16, "next_record_id"}, //LS byte first
    // record data field will be added on the fly
    {0,  ""}
  };

int8_t 
fill_kcs_get_repo_info (fiid_obj_t obj_data_rq)
{
  if (!obj_data_rq)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sdr_repo_info_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SDR_REPOSITORY_INFO);
  return 0;
}

int8_t 
fill_kcs_get_repo_alloc_info (fiid_obj_t obj_data_rq)
{
  if (!obj_data_rq)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_get_sdr_repo_alloc_info_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SDR_REPOSITORY_ALLOC_INFO);
  return 0;
}

int8_t 
fill_kcs_reserve_repo (fiid_obj_t obj_data_rq)
{
  if (!obj_data_rq)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		tmpl_reserve_sdr_repo_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_RESERVE_SDR_REPOSITORY);
  return 0;
}

int8_t 
fill_kcs_get_sensor_record_header (uint16_t record_id, fiid_obj_t obj_data_rq)
{
  if (!obj_data_rq)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"cmd",
		IPMI_CMD_GET_SDR);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"reservation_id",
		0x0);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"record_id",
		record_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"record_offset",
		0x0);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"bytes_read",
		fiid_obj_len_bytes (tmpl_sdr_sensor_record_header));
  return 0;
}

int8_t 
fill_kcs_get_sdr_chunk (uint16_t reservation_id, 
			uint16_t record_id, 
			uint8_t record_offset, 
			uint8_t bytes_read,
                        fiid_obj_t obj_data_rq)
{
  if (!obj_data_rq)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"cmd",
		IPMI_CMD_GET_SDR);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"reservation_id",
		reservation_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"record_id",
		record_id);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"record_offset",
		record_offset);
  
  FIID_OBJ_SET (obj_data_rq,
		tmpl_get_sdr_rq,
		(uint8_t *)"bytes_read",
		bytes_read);
  return 0;
}

