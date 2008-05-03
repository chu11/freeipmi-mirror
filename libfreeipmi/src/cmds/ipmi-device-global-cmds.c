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
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_device_id_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_device_id_rs =
{
  {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
  {4,  "device_revision.revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* binary encoded */
  {3,  "device_revision.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "device_revision.sdr_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {7,  "firmware_revision1.major_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "firmware_revision1.device_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "firmware_revision2.minor_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
  {4,  "ipmi_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "ipmi_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.sdr_repository_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.fru_inventory_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.ipmb_event_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.ipmb_event_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.chassis_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {20, "manufacturer_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "manufacturer_id.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {16, "product_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {32, "auxiliary_firmware_revision_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {0,  "", 0}
};

fiid_template_t tmpl_cmd_get_device_id_sr870bn4_rs =
{
  {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
  {4,  "device_revision.revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* binary encoded */
  {3,  "device_revision.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "device_revision.sdr_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {7,  "firmware_revision1.major_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "firmware_revision1.device_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "firmware_revision2.minor_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* BCD encoded */
  {4,  "ipmi_version.ms_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "ipmi_version.ls_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.sdr_repository_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.fru_inventory_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.ipmb_event_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.ipmb_event_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {1,  "additional_device_support.chassis_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {20, "manufacturer_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {4,  "manufacturer_id.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {16, "product_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "auxiliary_firmware_revision_info.boot_code.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "auxiliary_firmware_revision_info.boot_code.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "auxiliary_firmware_revision_info.pia.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,  "auxiliary_firmware_revision_info.pia.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {0,  "", 0}
};

fiid_template_t tmpl_cmd_get_device_guid_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_device_guid_rs =
{
  {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {128, "guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {0, "", 0}
};

/* achu: format in IPMI spec differs from the format in the "Wired for
   Management Baseline" 2.0 document.  In the IPMI spec there is a 2 byte
   field called "clock_seq_and_reserved", and in "Wired for Management
   Baseline" there is a 1 byte field called
   "clock_seq_hi_and_reserved" and a 1 byte field called
   "clock_seq_low".  I am going with the "Wired for Management
   Baseline" format under the assumption the IPMI spec has a typo.
 */
fiid_template_t tmpl_cmd_get_device_guid_format_rs =
{
  {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {48,  "node", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
  {8,   "clock_seq_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {8,   "clock_seq_hi_and_reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {16,  "time_high_and_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
  {16,  "time_mid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
  {32,  "time_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
  {0, "", 0}
};

fiid_template_t tmpl_cmd_cold_reset_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_cold_reset_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_warm_reset_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_warm_reset_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t 
fill_cmd_get_device_id (fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_device_id_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_DEVICE_ID);
  return (0);
}

int8_t
fill_cmd_get_device_guid (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE2(obj_cmd_rq, 
                             tmpl_cmd_get_device_guid_rq,
                             tmpl_cmd_get_device_guid_format_rs);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_DEVICE_GUID);
  return (0);
}

int8_t 
fill_cmd_cold_reset (fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_cold_reset_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_COLD_RESET);
  return (0);
}

int8_t 
fill_cmd_warm_reset (fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_warm_reset_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_WARM_RESET);
  return (0);
}
