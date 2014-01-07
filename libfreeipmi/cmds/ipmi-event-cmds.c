/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-event-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/* event_receiver_slave_address is presumably always a slave address, not a
 * software id, so no "id type" field.
 */
fiid_template_t tmpl_cmd_set_event_receiver_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_receiver_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_receiver_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_event_receiver_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

/* event_receiver_slave_address is presumably always a slave address, not a
 * software id, so no "id type" field.
 */
fiid_template_t tmpl_cmd_get_event_receiver_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_event_receiver_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "event_receiver_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "event_receiver_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_platform_event_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "generator_id", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_message_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* EvMRev in spec */
    { 8, "sensor_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "sensor_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "event_type_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_dir", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "event_data3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_platform_event_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

int
fill_cmd_set_event_receiver (uint8_t event_receiver_slave_address,
                             uint8_t event_receiver_lun,
                             fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_BMC_LUN_VALID (event_receiver_lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_event_receiver_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_EVENT_RECEIVER);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_receiver_slave_address", event_receiver_slave_address);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_receiver_lun", event_receiver_lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  return (0);
}

int
fill_cmd_get_event_receiver (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_event_receiver_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_EVENT_RECEIVER);
  return (0);
}

int
fill_cmd_platform_event (uint8_t *generator_id,
                         uint8_t event_message_format_version,
                         uint8_t sensor_type,
                         uint8_t sensor_number,
                         uint8_t event_type_code,
                         uint8_t event_dir,
                         uint8_t event_data1,
                         uint8_t event_data2,
                         uint8_t event_data3,
                         fiid_obj_t obj_cmd_rq)
{
  /* b/c OEM codes are allowed here, don't really need to check for
   * a lot of correct input.  Anything is allowed in many cases.
   */
  if (!IPMI_SEL_RECORD_EVENT_DIRECTION_VALID (event_dir)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_platform_event_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_PLATFORM_EVENT);
  if (generator_id)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "generator_id", (*generator_id));
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_message_format_version", event_message_format_version);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_type", sensor_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sensor_number", sensor_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_type_code", event_type_code);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_dir", event_dir);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_data1", event_data1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_data2", event_data2);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_data3", event_data3);

  return (0);
}
