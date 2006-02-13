/* 
   ipmi-dev-global-cmds.c - IPMI Device Global Commands

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

#include "freeipmi.h"
  
fiid_template_t tmpl_cmd_get_dev_id_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_dev_id_rs =
{
  {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "dev_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
  {4,  "dev_rev.rev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* binary encoded */
  {3,  "dev_rev.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "dev_rev.sdr_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {7,  "firmware_rev1.major_rev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "firmware_rev1.dev_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "firmware_rev2.minor_rev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
  {4,  "ipmi_ver.ms_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "ipmi_ver.ls_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.sensor_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.sdr_repo_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.sel_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.fru_inventory_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.ipmb_evnt_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.ipmb_evnt_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.chassis_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {20, "manf_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "manf_id.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {16, "prod_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {32, "aux_firmware_rev_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {0,  "", 0}
};

fiid_template_t tmpl_cmd_get_dev_id_sr870bn4_rs =
{
  {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "dev_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
  {4,  "dev_rev.rev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* binary encoded */
  {3,  "dev_rev.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "dev_rev.sdr_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {7,  "firmware_rev1.major_rev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "firmware_rev1.dev_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "firmware_rev2.minor_rev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
  {4,  "ipmi_ver.ms_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "ipmi_ver.ls_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.sensor_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.sdr_repo_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.sel_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.fru_inventory_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.ipmb_evnt_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.ipmb_evnt_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_dev_support.chassis_dev", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {20, "manf_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "manf_id.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {16, "prod_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "aux_firmware_rev_info.boot_code.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "aux_firmware_rev_info.boot_code.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "aux_firmware_rev_info.pia.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "aux_firmware_rev_info.pia.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {0,  "", 0}
};

int8_t 
fill_cmd_get_dev_id (fiid_obj_t obj_data_rq)
{ 
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_cmd_get_dev_id_rq)) < 0)
    return (-1);
  
  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"cmd", IPMI_CMD_GET_DEV_ID);
  return (0);
}

