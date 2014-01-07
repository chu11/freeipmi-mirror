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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/cmds/ipmi-firmware-firewall-command-discovery-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_netfn_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_netfn_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 2, "lun0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun0_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun1_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun2_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "lun3_netfn_support_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "command_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_sub_function_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_sub_function_support_specification_errata_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "errata_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "specification_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_sub_function_support_extension_errata_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "oem_group_defining_body_errata", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "specification_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_configurable_commands_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_configurable_commands_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "command_support_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_configurable_command_sub_functions_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_configurable_command_sub_functions_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_command_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "enable_disable_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_command_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "enable_disable_mask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu:
 *
 * For set_command_sub_function_enables, we cannot do net_fn_data like
 * other templates.  Having an optional/variable length field in the
 * middle of a payload is problematic.  There's no way to know if
 * packet data is meant for which optional/variable length field
 * without flat out length checks.  We have to deal with it with three
 * different templates.
 */

fiid_template_t tmpl_cmd_set_command_sub_function_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_command_sub_function_enables_defining_body_code_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "defining_body_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_command_sub_function_enables_oem_iana_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_command_sub_function_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_sub_function_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "command", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* For defining body code or group IANA depending on net_fn */
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_command_sub_function_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "sub_function_enables1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "sub_function_enables2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_oem_netfn_iana_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "list_index", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_oem_netfn_iana_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 7, "last_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "lun3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "net_fn_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

int
fill_cmd_get_netfn_support (uint8_t channel_number,
			    fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_netfn_support_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_NETFN_SUPPORT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  
  return (0);
}

int
fill_cmd_get_command_support (uint8_t channel_number,
			      uint8_t net_fn,
			      uint8_t operation,
			      uint8_t lun,
			      uint32_t net_fn_data,
			      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || !IPMI_FIRMWARE_FIREWALL_COMMAND_DISCOVERY_OPERATION_VALID (operation)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_command_support_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_COMMAND_SUPPORT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  if (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      uint8_t tmp = net_fn_data;
      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "net_fn_data", &tmp, 1);
    }
  else if (net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	   || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn_data", net_fn_data);
  
  return (0);
}

int
fill_cmd_get_command_sub_function_support (uint8_t channel_number,
					   uint8_t net_fn,
					   uint8_t lun,
					   uint8_t command,
					   uint32_t net_fn_data,
					   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_command_sub_function_support_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_COMMAND_SUB_FUNCTION_SUPPORT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "command", command);
  if (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      uint8_t tmp = net_fn_data;
      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "net_fn_data", &tmp, 1);
    }
  else if (net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	   || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn_data", net_fn_data);
  
  return (0);
}

int
fill_cmd_get_configurable_commands (uint8_t channel_number,
				    uint8_t net_fn,
				    uint8_t operation,
				    uint8_t lun,
				    uint32_t net_fn_data,
				    fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || !IPMI_FIRMWARE_FIREWALL_COMMAND_DISCOVERY_OPERATION_VALID (operation)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_configurable_commands_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CONFIGURABLE_COMMANDS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  if (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      uint8_t tmp = net_fn_data;
      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "net_fn_data", &tmp, 1);
    }
  else if (net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	   || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn_data", net_fn_data);
  
  return (0);
}

int
fill_cmd_get_configurable_command_sub_functions (uint8_t channel_number,
						 uint8_t net_fn,
						 uint8_t lun,
						 uint8_t command,
						 uint32_t net_fn_data,
						 fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_configurable_command_sub_functions_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CONFIGURABLE_COMMAND_SUB_FUNCTIONS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "command", command);
  if (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      uint8_t tmp = net_fn_data;
      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "net_fn_data", &tmp, 1);
    }
  else if (net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	   || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn_data", net_fn_data);
  
  return (0);
}

int
fill_cmd_set_command_enables (uint8_t channel_number,
			      uint8_t net_fn,
			      uint8_t operation,
			      uint8_t lun,
			      uint8_t *enable_disable_bitmask,
			      unsigned int enable_disable_bitmask_len,
			      uint32_t net_fn_data,
			      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || !IPMI_FIRMWARE_FIREWALL_COMMAND_DISCOVERY_OPERATION_VALID (operation)
      || !IPMI_BMC_LUN_VALID (lun)
      || !enable_disable_bitmask
      || enable_disable_bitmask_len < IPMI_FIRMWARE_FIREWALL_COMMAND_DISCOVERY_ENABLE_DISABLE_BITMASK_LEN
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_command_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_COMMAND_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
			  "enable_disable_mask",
			  enable_disable_bitmask,
			  IPMI_FIRMWARE_FIREWALL_COMMAND_DISCOVERY_ENABLE_DISABLE_BITMASK_LEN);
  if (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      uint8_t tmp = net_fn_data;
      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "net_fn_data", &tmp, 1);
    }
  else if (net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	   || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn_data", net_fn_data);
  
  return (0);
}

int
fill_cmd_get_command_enables (uint8_t channel_number,
			      uint8_t net_fn,
			      uint8_t operation,
			      uint8_t lun,
			      uint32_t net_fn_data,
			      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || !IPMI_FIRMWARE_FIREWALL_COMMAND_DISCOVERY_OPERATION_VALID (operation)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_command_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_COMMAND_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  if (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      uint8_t tmp = net_fn_data;
      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "net_fn_data", &tmp, 1);
    }
  else if (net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	   || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn_data", net_fn_data);
  
  return (0);
}

int
fill_cmd_set_command_sub_function_enables (uint8_t channel_number,
					   uint8_t net_fn,
					   uint8_t lun,
					   uint8_t command,
					   uint32_t sub_function_enables1,
					   uint32_t *sub_function_enables2,
					   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
	  || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS
	  || net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	  || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_command_sub_function_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_COMMAND_SUB_FUNCTION_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "command", command);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sub_function_enables1", sub_function_enables1);
  if (sub_function_enables2)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "sub_function_enables2", *sub_function_enables2);
  
  return (0);
}

int
fill_cmd_set_command_sub_function_enables_defining_body_code (uint8_t channel_number,
							      uint8_t net_fn,
							      uint8_t lun,
							      uint8_t command,
							      uint8_t defining_body_code,
							      uint32_t sub_function_enables1,
							      uint32_t *sub_function_enables2,
							      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || (net_fn != IPMI_NET_FN_GROUP_EXTENSION_RQ
	  && net_fn != IPMI_NET_FN_GROUP_EXTENSION_RS)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_command_sub_function_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_COMMAND_SUB_FUNCTION_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "command", command);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "defining_body_code", defining_body_code);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sub_function_enables1", sub_function_enables1);
  if (sub_function_enables2)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "sub_function_enables2", *sub_function_enables2);
  
  return (0);
}

int
fill_cmd_set_command_sub_function_enables_oem_iana (uint8_t channel_number,
						    uint8_t net_fn,
						    uint8_t lun,
						    uint8_t command,
						    uint32_t oem_iana,
						    uint32_t sub_function_enables1,
						    uint32_t *sub_function_enables2,
						    fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || (net_fn != IPMI_NET_FN_OEM_GROUP_RQ
	  && net_fn != IPMI_NET_FN_OEM_GROUP_RS)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_command_sub_function_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_COMMAND_SUB_FUNCTION_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "command", command);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_iana", oem_iana);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sub_function_enables1", sub_function_enables1);
  if (sub_function_enables2)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "sub_function_enables2", *sub_function_enables2);
  
  return (0);
}

int
fill_cmd_get_command_sub_function_enables (uint8_t channel_number,
					   uint8_t net_fn,
					   uint8_t lun,
					   uint8_t command,
					   uint32_t net_fn_data,
					   fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_NET_FN_VALID (net_fn)
      || !IPMI_BMC_LUN_VALID (lun)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_command_sub_function_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_COMMAND_SUB_FUNCTION_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "lun", lun);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "command", command);
  if (net_fn == IPMI_NET_FN_GROUP_EXTENSION_RQ
      || net_fn == IPMI_NET_FN_GROUP_EXTENSION_RS)
    {
      uint8_t tmp = net_fn_data;
      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "net_fn_data", &tmp, 1);
    }
  else if (net_fn == IPMI_NET_FN_OEM_GROUP_RQ
	   || net_fn == IPMI_NET_FN_OEM_GROUP_RS)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn_data", net_fn_data);
  
  return (0);
}

int
fill_cmd_get_oem_netfn_iana_support (uint8_t channel_number,
				     uint8_t net_fn,
				     uint8_t list_index,
				     fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || (net_fn != IPMI_NET_FN_GROUP_EXTENSION_RQ
	  && net_fn != IPMI_NET_FN_OEM_GROUP_RQ)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_oem_netfn_iana_support_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_OEM_NETFN_IANA_SUPPORT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "net_fn", net_fn);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "list_index", list_index);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  
  return (0);
}
