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

#include "freeipmi/cmds/ipmi-event-cmds.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

/* event_receiver_slave_address is presumably always a slave address, not a 
 * software id, so no "id type" field.
 */
fiid_template_t tmpl_cmd_set_event_receiver_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "event_receiver_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "event_receiver_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_event_receiver_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

/* event_receiver_slave_address is presumably always a slave address, not a 
 * software id, so no "id type" field.
 */
fiid_template_t tmpl_cmd_get_event_receiver_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_event_receiver_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "event_receiver_slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "event_receiver_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t 
fill_cmd_set_event_receiver (uint8_t event_receiver_slave_address,
                             uint8_t event_receiver_lun,
                             fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (IPMI_BMC_LUN_VALID(lun)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_event_receiver_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_EVENT_RECEIVER);
  FIID_OBJ_SET (obj_cmd_rq, "event_receiver_slave_address", event_receiver_slave_address);
  FIID_OBJ_SET (obj_cmd_rq, "event_receiver_lun", event_receiver_lun);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  return (0);
}

int8_t 
fill_cmd_get_event_receiver (fiid_obj_t obj_cmd_rq)
{ 
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_event_receiver_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_EVENT_RECEIVER);
  return (0);
}
