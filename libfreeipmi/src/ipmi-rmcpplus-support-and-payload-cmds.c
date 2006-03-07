/* 
   ipmi-rmcpplus.c - IPMI RMCPPLUS

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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/ipmi-rmcpplus-support-and-payload-cmds.h"
#include "freeipmi/ipmi-rmcpplus.h"
#include "freeipmi/ipmi-cmd-spec.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

fiid_template_t tmpl_activate_payload_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,  "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "auxiliary_request_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_activate_payload_sol_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,  "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "sol_startup_handshake", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "shared_serial_alert_behavior", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "test_mode", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_activation", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1,  "encryption_activation", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {1,  "reserved4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {24, "reserved5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_activate_payload_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "auxiliary_response_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "inbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "outbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "payload_udp_port_number_ls_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "payload_udp_port_number_ms_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "payload_vlan_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_activate_payload_sol_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "test_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {31, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "inbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16, "outbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "payload_udp_port_number_ls_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "payload_udp_port_number_ms_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "payload_vlan_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_deactivate_payload_rq = 
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,  "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "payload_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_deactivate_payload_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_suspend_resume_payload_encryption_rq = 
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,  "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* XXX: The IPMI spec says [4:0] for Operation and [7:2] for
       reserved3.  Needless to say, one is wrong.  Since there are
       only three operations, we'll assume they meant for operation to
       be a 2 bit field
    */
    {2,  "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_suspend_resume_payload_encryption_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
fill_cmd_activate_payload (uint8_t payload_type,
			   uint8_t payload_instance,
			   uint8_t *auxiliary_request_data,
			   uint32_t auxiliary_request_data_len,
			   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL(IPMI_PAYLOAD_TYPE_VALID(payload_type)
	     && auxiliary_request_data
	     && auxiliary_request_data_len
	     && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_activate_payload_rq);

  FIID_OBJ_CLEAR(obj_cmd_rq);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"cmd", IPMI_CMD_ACTIVATE_PAYLOAD);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"payload_type", payload_type);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"payload_instance", payload_instance);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET_DATA(obj_cmd_rq,
		    (uint8_t *)"auxiliary_request_data",
		    auxiliary_request_data,
		    auxiliary_request_data_len);
  
  return (0);
}

int8_t
fill_cmd_activate_payload_sol (uint8_t payload_type,
			       uint8_t payload_instance,
			       uint8_t sol_startup_handshake,
			       uint8_t shared_serial_alert_behavior,
			       uint8_t test_mode,
			       uint8_t authentication_activation,
			       uint8_t encryption_activation,
			       fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL(IPMI_PAYLOAD_TYPE_VALID(payload_type)
	     && IPMI_SOL_STARTUP_HANDSHAKE_CTS_AND_DCD_SDR_VALID(sol_startup_handshake)
	     && IPMI_SERIAL_MODEM_ALERTS_VALID(shared_serial_alert_behavior)
	     && IPMI_TEST_MODE_VALID(test_mode)
	     && IPMI_AUTHENTICATION_ACTIVATION_VALID(authentication_activation)
	     && IPMI_ENCRYPTION_ACTIVATION_VALID(encryption_activation)
	     && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_activate_payload_sol_rq);

  FIID_OBJ_CLEAR(obj_cmd_rq);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"cmd", IPMI_CMD_ACTIVATE_PAYLOAD);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"payload_type", payload_type);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"payload_instance", payload_instance);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"reserved3", 0);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"sol_startup_handshake", sol_startup_handshake);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"shared_serial_alert_behavior", shared_serial_alert_behavior);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"test_mode", test_mode);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"authentication_activation", authentication_activation);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"encryption_activation", encryption_activation);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"reserved4", 0);
  FIID_OBJ_SET(obj_cmd_rq, (uint8_t *)"reserved5", 0);
  
  return (0);
}

