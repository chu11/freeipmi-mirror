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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-sdr-repository-cmds.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_sdr_repository_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sdr_repository_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4,  "sdr_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "record_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {16, "free_space", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {32, "most_recent_addition_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {32, "most_recent_erase_timestamp", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "get_sdr_repository_allocation_info_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "reserve_sdr_repository_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "partial_add_sdr_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "delete_sdr_command_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,  "modal_non_modal_sdr_repository_update_operation_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "overflow_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_cmd_get_sdr_repository_allocation_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sdr_repository_allocation_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "number_of_possible_allocation_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "allocation_unit_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "number_of_free_allocation_units", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "largest_free_block", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "maximum_record_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_reserve_sdr_repository_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_reserve_sdr_repository_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sdr_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "reservation_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {16, "record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {8,  "offset_into_record", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "bytes_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_cmd_get_sdr_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "next_record_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    /* 2040 = 255 * 8, 255 b/c bytes_to_read field in request is 1 byte long */
    {2040, "record_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0,  "", 0}
  };

fiid_template_t tmpl_cmd_get_sdr_repository_time_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_sdr_repository_time_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {0,   "", 0}
  };

fiid_template_t tmpl_cmd_set_sdr_repository_time_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_sdr_repository_time_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0,   "", 0}
  };

int8_t 
fill_cmd_get_repository_info (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_sdr_repository_info_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SDR_REPOSITORY_INFO);
  return 0;
}

int8_t 
fill_cmd_get_repository_allocation_info (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_sdr_repository_allocation_info_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SDR_REPOSITORY_ALLOCATION_INFO);
  return 0;
}

int8_t 
fill_cmd_reserve_sdr_repository (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_reserve_sdr_repository_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_RESERVE_SDR_REPOSITORY);
  return 0;
}

int8_t 
fill_cmd_get_sdr (uint16_t reservation_id, 
                  uint16_t record_id, 
                  uint8_t offset_into_record, 
                  uint8_t bytes_to_read,
                  fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_sdr_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SDR);
  FIID_OBJ_SET (obj_cmd_rq, "reservation_id", reservation_id);
  FIID_OBJ_SET (obj_cmd_rq, "record_id", record_id);
  FIID_OBJ_SET (obj_cmd_rq, "offset_into_record", offset_into_record);
  FIID_OBJ_SET (obj_cmd_rq, "bytes_to_read", bytes_to_read);
  return 0;
}

int8_t
fill_cmd_get_sdr_repository_time (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_sdr_repository_time_rq);
  
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SDR_REPOSITORY_TIME);
  
  return 0;
}

int8_t
fill_cmd_set_sdr_repository_time (uint32_t time, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_sdr_repository_time_rq);
  
  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SDR_REPOSITORY_TIME);
  FIID_OBJ_SET (obj_cmd_rq, "time", time);
  
  return 0;
}
