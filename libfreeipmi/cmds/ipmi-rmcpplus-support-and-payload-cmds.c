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

#include "freeipmi/cmds/ipmi-rmcpplus-support-and-payload-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_activate_payload_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "auxiliary_request_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_payload_sol_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sol_startup_handshake", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "shared_serial_alert_behavior", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "test_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_activation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "encryption_activation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_payload_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "auxiliary_response_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "inbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "outbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_udp_port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_vlan_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_payload_sol_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "test_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 31, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "inbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "outbound_payload_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_udp_port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "payload_vlan_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_deactivate_payload_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "payload_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_deactivate_payload_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_suspend_resume_payload_encryption_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* achu: The IPMI spec says [4:0] for Operation and [7:2] for
       reserved3.  Needless to say, one is wrong.  Since there are
       only three operations, we'll assume they meant for operation to
       be a 2 bit field
    */
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_suspend_resume_payload_encryption_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_payload_activation_status_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_payload_activation_status_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "instance_capacity", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_12", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_13", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_14", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_15", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "instance_16", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_payload_instance_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_instance", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_payload_instance_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "port_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 56, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_payload_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_payload_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_payload_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_payload_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_payload_7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_payload_support_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_payload_support_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "standard_payload_type_0_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_1_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_2_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_3_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_4_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_5_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_6_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_7_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_8_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_9_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_10_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_11_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_12_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_13_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_14_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "standard_payload_type_15_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_0_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_1_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_2_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_3_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_4_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_5_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_6_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_7_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_8_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_9_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_10_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_11_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_12_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_13_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_14_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "session_setup_payload_15_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_20h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_21h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_22h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_23h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_24h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_25h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_26h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_27h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_28h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_29h_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Ah_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Bh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Ch_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Dh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Eh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type_2Fh_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_payload_version_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_payload_version_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "minor_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "major_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_oem_payload_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "oem_payload_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_oem_payload_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "oem_payload_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "minor_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "major_format_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

int
fill_cmd_activate_payload (uint8_t payload_type,
                           uint8_t payload_instance,
                           const void *auxiliary_request_data,
                           unsigned int auxiliary_request_data_len,
                           fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !IPMI_PAYLOAD_INSTANCE_VALID (payload_instance)
      || !auxiliary_request_data
      || !auxiliary_request_data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_activate_payload_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_ACTIVATE_PAYLOAD);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_instance", payload_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "auxiliary_request_data",
                          auxiliary_request_data,
                          auxiliary_request_data_len);

  return (0);
}

int
fill_cmd_activate_payload_sol (uint8_t payload_type,
                               uint8_t payload_instance,
                               uint8_t sol_startup_handshake,
                               uint8_t shared_serial_alert_behavior,
                               uint8_t test_mode,
                               uint8_t authentication_activation,
                               uint8_t encryption_activation,
                               fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !IPMI_PAYLOAD_INSTANCE_VALID (payload_instance)
      || !IPMI_SOL_STARTUP_HANDSHAKE_CTS_AND_DCD_SDR_VALID (sol_startup_handshake)
      || !IPMI_SERIAL_MODEM_ALERTS_VALID (shared_serial_alert_behavior)
      || !IPMI_TEST_MODE_VALID (test_mode)
      || !IPMI_AUTHENTICATION_ACTIVATION_VALID (authentication_activation)
      || !IPMI_ENCRYPTION_ACTIVATION_VALID (encryption_activation)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_activate_payload_sol_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_ACTIVATE_PAYLOAD);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_instance", payload_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "sol_startup_handshake", sol_startup_handshake);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "shared_serial_alert_behavior", shared_serial_alert_behavior);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved4", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "test_mode", test_mode);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "authentication_activation", authentication_activation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "encryption_activation", encryption_activation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved5", 0);

  return (0);
}

int
fill_cmd_deactivate_payload (uint8_t payload_type,
                             uint8_t payload_instance,
                             uint32_t payload_auxiliary_data,
                             fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !IPMI_PAYLOAD_INSTANCE_VALID (payload_instance)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_deactivate_payload_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_DEACTIVATE_PAYLOAD);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_instance", payload_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_auxiliary_data", payload_auxiliary_data);

  return (0);
}

int
fill_cmd_suspend_resume_payload_encryption (uint8_t payload_type,
                                            uint8_t payload_instance,
                                            uint8_t operation,
                                            fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !IPMI_PAYLOAD_INSTANCE_VALID (payload_instance)
      || !IPMI_SUSPEND_RESUME_PAYLOAD_ENCRYPTION_OPERATION_VALID (operation)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_suspend_resume_payload_encryption_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SUSPEND_RESUME_PAYLOAD_ENCRYPTION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_instance", payload_instance);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);

  return (0);
}

int
fill_cmd_get_payload_activation_status (uint8_t payload_type,
                                        fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_payload_activation_status_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_PAYLOAD_ACTIVATION_STATUS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);

  return (0);
}

int
fill_cmd_get_payload_instance_info (uint8_t payload_type,
                                    uint8_t payload_instance,
                                    fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !IPMI_PAYLOAD_INSTANCE_VALID (payload_instance)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_payload_instance_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_PAYLOAD_INSTANCE_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_instance", payload_instance);

  return (0);
}

int
fill_cmd_set_user_payload_access (uint8_t channel_number,
                                  uint8_t user_id,
                                  uint8_t operation,
                                  uint8_t standard_payload_1,
                                  uint8_t standard_payload_2,
                                  uint8_t standard_payload_3,
                                  uint8_t standard_payload_4,
                                  uint8_t standard_payload_5,
                                  uint8_t standard_payload_6,
                                  uint8_t standard_payload_7,
                                  uint8_t oem_payload_0,
                                  uint8_t oem_payload_1,
                                  uint8_t oem_payload_2,
                                  uint8_t oem_payload_3,
                                  uint8_t oem_payload_4,
                                  uint8_t oem_payload_5,
                                  uint8_t oem_payload_6,
                                  uint8_t oem_payload_7,
                                  fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_SET_USER_PAYLOAD_OPERATION_VALID (operation)
      || !IPMI_PAYLOAD_ACCESS_VALID (standard_payload_1)
      || !IPMI_PAYLOAD_ACCESS_VALID (standard_payload_2)
      || !IPMI_PAYLOAD_ACCESS_VALID (standard_payload_3)
      || !IPMI_PAYLOAD_ACCESS_VALID (standard_payload_4)
      || !IPMI_PAYLOAD_ACCESS_VALID (standard_payload_5)
      || !IPMI_PAYLOAD_ACCESS_VALID (standard_payload_6)
      || !IPMI_PAYLOAD_ACCESS_VALID (standard_payload_7)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_0)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_1)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_2)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_3)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_4)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_5)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_6)
      || !IPMI_PAYLOAD_ACCESS_VALID (oem_payload_7)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_user_payload_access_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_PAYLOAD_ACCESS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "standard_payload_1", standard_payload_1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "standard_payload_2", standard_payload_2);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "standard_payload_3", standard_payload_3);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "standard_payload_4", standard_payload_4);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "standard_payload_5", standard_payload_5);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "standard_payload_6", standard_payload_6);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "standard_payload_7", standard_payload_7);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_0", oem_payload_0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_1", oem_payload_1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_2", oem_payload_2);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_3", oem_payload_3);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_4", oem_payload_4);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_5", oem_payload_5);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_6", oem_payload_6);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_7", oem_payload_7);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved4", 0);

  return (0);
}

int
fill_cmd_get_user_payload_access (uint8_t channel_number,
                                  uint8_t user_id,
                                  fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_user_payload_access_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_USER_PAYLOAD_ACCESS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return (0);
}

int
fill_cmd_get_channel_payload_support (uint8_t channel_number,
                                      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_channel_payload_support_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_PAYLOAD_SUPPORT);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_get_channel_payload_version (uint8_t channel_number,
                                      uint8_t payload_type,
                                      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_channel_payload_version_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_PAYLOAD_VERSION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);

  return (0);
}

int
fill_cmd_get_channel_oem_payload_info (uint8_t channel_number,
                                       uint8_t payload_type,
                                       uint32_t oem_iana,
                                       uint16_t oem_payload_id,
                                       fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT
          && (!oem_iana || !oem_payload_id))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_channel_oem_payload_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_OEM_PAYLOAD_INFO);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_iana", oem_iana);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_payload_id", oem_payload_id);

  return (0);
}
