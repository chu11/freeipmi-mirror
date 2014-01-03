/*****************************************************************************\
 *  $Id: ipmi-fru-inventory-device-cmds.c,v 1.22 2010-07-28 21:19:56 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-fru-inventory-device-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_fru_inventory_area_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_fru_inventory_area_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "fru_inventory_area_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "device_is_accessed", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_read_fru_data_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "fru_inventory_offset_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "count_to_read", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* 2040 = 255 * 8, 255 b/c count_returned field in request is 1 byte long */
fiid_template_t tmpl_cmd_read_fru_data_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "count_returned", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2040, "requested_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

/* 2040 = 255 * 8, 255 b/c bytes_to_write field in request is 1 byte long */
fiid_template_t tmpl_cmd_write_fru_data_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "fru_device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "fru_inventory_offset_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2040, "data_to_write", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_write_fru_data_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED  | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "count_written", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

int
fill_cmd_get_fru_inventory_area_info (uint8_t fru_device_id,
                                      fiid_obj_t obj_cmd_rq)
{
  if (fru_device_id == IPMI_FRU_DEVICE_ID_RESERVED
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_fru_inventory_area_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_FRU_INVENTORY_AREA_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "fru_device_id", fru_device_id);
  return (0);
}

int
fill_cmd_read_fru_data (uint8_t fru_device_id,
                        uint16_t fru_inventory_offset_to_read,
                        uint8_t count_to_read,
                        fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_read_fru_data_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_READ_FRU_DATA);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "fru_device_id", fru_device_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "fru_inventory_offset_to_read", fru_inventory_offset_to_read);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "count_to_read", count_to_read);
  return (0);
}

int
fill_cmd_write_fru_data (uint8_t fru_device_id,
                         uint16_t fru_inventory_offset_to_write,
                         const void *data_to_write,
                         unsigned int data_to_write_len,
                         fiid_obj_t obj_cmd_rq)
{
  if ((data_to_write
       && data_to_write_len > IPMI_FRU_DATA_MAX)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_write_fru_data_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_WRITE_FRU_DATA);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "fru_device_id", fru_device_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "fru_inventory_offset_to_write", fru_inventory_offset_to_write);
  if (data_to_write && data_to_write_len)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "data_to_write", data_to_write, data_to_write_len);
  return (0);
}
