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
    {8, "cmd"},
    {0, ""}
  };

fiid_template_t tmpl_cmd_get_dev_id_rs =
{
  {8,  "cmd"},
  {8,  "comp_code"},
  {8,  "dev_id"}, 
  {4,  "dev_rev.rev"}, /* binary encoded */
  {3,  "dev_rev.reserved1"},
  {1,  "dev_rev.sdr_support"},
  {7,  "firmware_rev1.major_rev"},
  {1,  "firmware_rev1.dev_available"},
  {8,  "firmware_rev2.minor_rev"}, /* BCD encoded */
  {4,  "ipmi_ver.ms_bits"},
  {4,  "ipmi_ver.ls_bits"},
  {1,  "additional_dev_support.sensor_dev"},
  {1,  "additional_dev_support.sdr_repo_dev"},
  {1,  "additional_dev_support.sel_dev"},
  {1,  "additional_dev_support.fru_inventory_dev"},
  {1,  "additional_dev_support.ipmb_evnt_receiver"},
  {1,  "additional_dev_support.ipmb_evnt_generator"},
  {1,  "additional_dev_support.bridge"},
  {1,  "additional_dev_support.chassis_dev"},
  {20, "manf_id.id"},
  {4,  "manf_id.reserved1"},
  {16, "prod_id"},
  {32, "aux_firmware_rev_info"},
  {0,  ""}
};

fiid_template_t tmpl_cmd_get_dev_id_sr870bn4_rs =
{
  {8,  "cmd"},
  {8,  "comp_code"},
  {8,  "dev_id"}, 
  {4,  "dev_rev.rev"}, /* binary encoded */
  {3,  "dev_rev.reserved1"},
  {1,  "dev_rev.sdr_support"},
  {7,  "firmware_rev1.major_rev"},
  {1,  "firmware_rev1.dev_available"},
  {8,  "firmware_rev2.minor_rev"}, /* BCD encoded */
  {4,  "ipmi_ver.ms_bits"},
  {4,  "ipmi_ver.ls_bits"},
  {1,  "additional_dev_support.sensor_dev"},
  {1,  "additional_dev_support.sdr_repo_dev"},
  {1,  "additional_dev_support.sel_dev"},
  {1,  "additional_dev_support.fru_inventory_dev"},
  {1,  "additional_dev_support.ipmb_evnt_receiver"},
  {1,  "additional_dev_support.ipmb_evnt_generator"},
  {1,  "additional_dev_support.bridge"},
  {1,  "additional_dev_support.chassis_dev"},
  {20, "manf_id.id"},
  {4,  "manf_id.reserved1"},
  {16, "prod_id"},
  {8,  "aux_firmware_rev_info.boot_code.major"},
  {8,  "aux_firmware_rev_info.boot_code.minor"},
  {8,  "aux_firmware_rev_info.pia.major"},
  {8,  "aux_firmware_rev_info.pia.minor"},
  {0,  ""}
};

int8_t 
fill_cmd_get_dev_id (fiid_obj_t obj_data_rq)
{ 
  if (!obj_data_rq)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_data_rq, tmpl_cmd_get_dev_id_rq, (uint8_t *)"cmd", IPMI_CMD_GET_DEV_ID);
  return (0);
}

int8_t 
ipmi_cmd_get_dev_id (ipmi_device_t *dev, fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_get_dev_id_rq);
  ERR (fill_cmd_get_dev_id (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_get_dev_id_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_get_dev_id_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

