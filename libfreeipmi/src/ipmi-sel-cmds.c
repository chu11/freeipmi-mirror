/* 
   ipmi-sel-cmds.c - IPMI System Event Log Commands
   
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "ipmi-sel-cmds.h"

#include "freeipmi-portability.h"
#include "fiid-wrappers.h"
#include "ipmi-cmd-spec.h"

fiid_template_t tmpl_get_sel_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_sel_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sel_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sel_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "entries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {16, "free_space", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {32, "most_recent_addition_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {32, "most_recent_erase_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "get_sel_allocation_info_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "reserve_sel_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "partial_add_sel_entry_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "delete_sel_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "overflow_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sel_allocation_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_sel_allocation_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "number_of_possible_allocation_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "allocation_unit_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "number_of_free_allocation_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "largest_free_block", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "maximum_record_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_reserve_sel_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_reserve_sel_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {0,  "", 0}
  };


fiid_template_t tmpl_get_sel_entry_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {8,  "offset_into_record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_get_sel_entry_rs =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16,  "next_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {128, "record_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,   "", 0}
  };


fiid_template_t tmpl_delete_sel_entry_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {0,  "", 0}
  };


fiid_template_t tmpl_delete_sel_entry_rs =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16,  "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {0,   "", 0}
  };


fiid_template_t tmpl_clear_sel_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, //LS byte first
    {8,  "C", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "L", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "R", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_clear_sel_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "erasure_progress", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

int8_t 
fill_cmd_get_sel_info (fiid_obj_t obj_data_rq)
{
  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_get_sel_info_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SEL_DEV_CMDS_GET_SEL_INFO);
  return 0;
}

int8_t 
fill_cmd_get_sel_allocation_info (fiid_obj_t obj_data_rq)
{
  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_get_sel_allocation_info_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SEL_ALLOCATION_INFO);
  return 0;
}

int8_t 
fill_cmd_reserve_sel (fiid_obj_t obj_data_rq)
{
  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_reserve_sel_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_RESERVE_SEL);
  return 0;
}

int8_t 
fill_cmd_get_sel_entry (uint16_t reservation_id,
                        uint16_t record_id,
                        uint8_t offset_into_record,
                        uint8_t bytes_to_read,
                        fiid_obj_t obj_data_rq)
{
  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_get_sel_entry_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SEL_ENTRY);

  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reservation_id", reservation_id);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"record_id", record_id);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"offset_into_record", offset_into_record);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"bytes_to_read", bytes_to_read);
  
  return 0;
}

int8_t 
fill_cmd_delete_sel_entry (uint16_t reservation_id, 
			   uint16_t record_id,
                           fiid_obj_t obj_data_rq)
{
  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_delete_sel_entry_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_DELETE_SEL_ENTRY);
  
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reservation_id", reservation_id);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"record_id", record_id);
  
  return 0;
}

int8_t 
fill_cmd_clear_sel (uint16_t reservation_id, 
                    uint8_t operation, 
                    fiid_obj_t obj_data_rq)
{
  if (!IPMI_SEL_CLEAR_OPERATION_VALID(operation)
      ||!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_clear_sel_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_CLEAR_SEL);
  
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reservation_id", reservation_id);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"C", 'C');
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"L", 'L');
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"R", 'R');
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"operation", operation);
  
  return 0;
}
