/*****************************************************************************\
 *  $Id: ipmi-fru-inventory-device-cmds.c,v 1.1.2.2 2007-06-28 00:21:49 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/ipmi-cmd-spec.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_fru_inventory_area_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_fru_inventory_area_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "fru_inventory_area_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "device_is_accessed", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_read_fru_data_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "fru_inventory_offset_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "count_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_read_fru_data_rs =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "count_returned", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 2040 = 255 * 8, 255 b/c count_returned field in request is 1 byte long */
    {2040, "requested_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_write_fru_data_rq =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,   "fru_inventory_offset_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 2040 = 255 * 8, 255 b/c bytes_to_write field in request is 1 byte long */
    {2040, "data_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_write_fru_data_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "count_written", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
fill_cmd_get_fru_inventory_area_info (uint8_t fru_device_id,
                                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_fru_inventory_area_info_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_FRU_INVENTORY_AREA_INFO);
  FIID_OBJ_SET (obj_cmd_rq, "fru_device_id", fru_device_id);
  return (0);
}

int8_t
fill_cmd_read_fru_data (uint8_t fru_device_id,
                        uint16_t fru_inventory_offset_to_read,
                        uint8_t count_to_read,
                        fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_read_fru_data_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_READ_FRU_DATA);
  FIID_OBJ_SET (obj_cmd_rq, "fru_device_id", fru_device_id);
  FIID_OBJ_SET (obj_cmd_rq, "fru_inventory_offset_to_read", fru_inventory_offset_to_read);
  FIID_OBJ_SET (obj_cmd_rq, "count_to_read", count_to_read);
  return (0);
}

int8_t
fill_cmd_write_fru_data (uint8_t fru_device_id,
                         uint16_t fru_inventory_offset_to_write,
                         uint8_t *data_to_write,
                         unsigned int data_to_write_len,
                         fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (!(data_to_write
                && data_to_write_len > IPMI_FRU_DATA_MAX)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_write_fru_data_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_WRITE_FRU_DATA);
  FIID_OBJ_SET (obj_cmd_rq, "fru_device_id", fru_device_id);
  FIID_OBJ_SET (obj_cmd_rq, "fru_inventory_offset_to_write", fru_inventory_offset_to_write);
  if (data_to_write && data_to_write_len)
    FIID_OBJ_SET_DATA (obj_cmd_rq, "data_to_write", data_to_write, data_to_write_len);
  return (0);
}
