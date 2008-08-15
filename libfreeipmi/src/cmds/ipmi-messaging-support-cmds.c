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
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

#define IPMI_MAX_K_LENGTH 64

fiid_template_t tmpl_cmd_get_system_interface_capabilities_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "system_interface", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_interface_capabilities_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "data", FIID_FIELD_OPTIONAL, FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_interface_capabilities_ssif_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "ssif_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "pec_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "transaction_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "input_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "output_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_interface_capabilities_kcs_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "system_interface_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "input_maximum_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_bt_interface_capabilities_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_bt_interface_capabilities_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "number_of_outstanding_requests_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "input_buffer_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* in bytes */
    {8, "output_buffer_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* in bytes */
    {8, "BMC_Request_to_Response_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* in seconds */
    {8, "recommended_retries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rs = 
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.straight_password_key", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_type.oem_prop", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "authentication_type.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.anonymous_login", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.non_null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,  "authentication_status.per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,  "authentication_status.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "oem_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_v20_rq =
 {
   {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1, "get_ipmi_v2.0_extended_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {0, "", 0}
 };

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_v20_rs =
 {
   {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {8,  "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.straight_password_key", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.oem_prop", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_type.ipmi_v2.0_extended_capabilities_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_status.anonymous_login", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_status.null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_status.non_null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_status.user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_status.per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "authentication_status.k_g", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {2,  "authentication_status.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "channel_supports_ipmi_v1.5_connections", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {1,  "channel_supports_ipmi_v2.0_connections", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {6,  "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {24, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {8,  "oem_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
   {0, "", 0}
 };

fiid_template_t tmpl_cmd_get_channel_cipher_suites_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "list_index", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "list_algorithm_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_cipher_suites_rs = 
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "cipher_suite_record_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_cipher_suites_list_algorithms_by_cipher_suite_rs = 
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "start_of_record_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "cipher_suite_id_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_cipher_suites_list_supported_algorithms_rs = 
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_1", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_3", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_4", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_5", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_6", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_7", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_8", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_9", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_10", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_11", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_12", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_13", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_14", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_15", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_15", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {6,   "algorithm_16", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {2,   "tag_bits_16", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rs =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "temp_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* LS byte first */
    {128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "initial_outbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "initial_inbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_privilege_level_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_privilege_level_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rq =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };


fiid_template_t tmpl_cmd_set_channel_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "channel_access_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "channel_privilege_level_limit_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_channel_access_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "channel_access_get", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_access_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_info_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_info_rs =
  {
    {8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "actual_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,  "actual_channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {7,  "channel_medium_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,  "channel_medium_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5,  "channel_protocol_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3,  "channel_protocol_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,  "active_session_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,  "session_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24, "vendor_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {16, "auxiliary_channel_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_channel_security_keys_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,   "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,   "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "key_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {160, "key_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_channel_security_keys_rs =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,   "lock_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,   "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {160, "key_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "change_bits_in_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "user_session_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_access_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_cmd_get_user_access_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_access_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "max_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "current_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "user_id_enable_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "current_channel_fixed_names", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_name_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_name_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_name_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_name_rs =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,   "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

fiid_template_t tmpl_cmd_set_user_password_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "operation.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {128, "password", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

/* achu: Note that the password is variable length, but it must be
 * fixed to 0, 16, or 20 bytes.  We may try and amend this situation
 * in fiid at a later time.
 */
fiid_template_t tmpl_cmd_set_user_password_v20_rq =
  {
    {8,   "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,   "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1,   "password_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "operation.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {160, "password", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_password_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };
    
int8_t
fill_cmd_get_system_interface_capabilities (uint8_t system_interface,
                                            fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_SYSTEM_INTERFACE_VALID(system_interface)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_system_interface_capabilities_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_INTERFACE_CAPABILITIES);
  FIID_OBJ_SET (obj_cmd_rq, "system_interface", system_interface); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  return (0);
}

int8_t
fill_cmd_get_bt_interface_capabilities (fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_bt_interface_capabilities_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_BT_INTERFACE_CAPABILITIES);
  return (0);
}

int8_t 
fill_cmd_get_channel_authentication_capabilities (uint8_t channel_number,
                                                  uint8_t maximum_privilege_level, 
                                                  fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_level", maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  return (0);
}

int8_t 
fill_cmd_get_channel_authentication_capabilities_v20 (uint8_t channel_number,
                                                      uint8_t maximum_privilege_level, 
                                                      uint8_t get_ipmi_v20_extended_data,
                                                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
	      && IPMI_GET_IPMI_DATA_VALID(get_ipmi_v20_extended_data)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_v20_rq);

  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "get_ipmi_v2.0_extended_data", get_ipmi_v20_extended_data);
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_level", maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return (0);
}

int8_t
fill_cmd_get_channel_cipher_suites (uint8_t channel_number,
                                    uint8_t payload_type,
                                    uint8_t list_index,
                                    uint8_t list_algorithm_type,
                                    fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && IPMI_PAYLOAD_TYPE_VALID(payload_type)
              && IPMI_LIST_ALGORITHM_TYPE_VALID(list_algorithm_type)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_channel_cipher_suites_rq);
  
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_CIPHER_SUITES);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "list_index", list_index); 
  FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FIID_OBJ_SET (obj_cmd_rq, "list_algorithm_type", list_algorithm_type);
  return (0);
}

int8_t 
fill_cmd_get_session_challenge (uint8_t authentication_type, 
				char *user_name, 
				uint32_t user_name_len, 
				fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_MAX_USER_NAME_LENGTH];

  /* achu: user_name can be IPMI_MAX_USER_NAME_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  ERR_EINVAL (IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
	      && !(user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_session_challenge_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SESSION_CHALLENGE);
  FIID_OBJ_SET (obj_cmd_rq, "authentication_type", authentication_type);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  /* achu: user_name must be zero extended */
  memset(buf, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (user_name)
    strncpy((char *)buf, user_name, IPMI_MAX_USER_NAME_LENGTH);
  
  FIID_OBJ_SET_DATA (obj_cmd_rq, "user_name", buf, IPMI_MAX_USER_NAME_LENGTH);
  
  return (0);
}

int8_t 
fill_cmd_activate_session (uint8_t authentication_type, 
			   uint8_t maximum_privilege_level, 
			   uint8_t *challenge_string, 
			   uint32_t challenge_string_len, 
			   uint32_t initial_outbound_sequence_number, 
			   fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_CHALLENGE_STRING_LENGTH];

  ERR_EINVAL (IPMI_AUTHENTICATION_TYPE_VALID(authentication_type)
	      && IPMI_PRIVILEGE_LEVEL_VALID(maximum_privilege_level)
	      && challenge_string
	      && !(challenge_string_len > IPMI_CHALLENGE_STRING_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_activate_session_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_ACTIVATE_SESSION);
  FIID_OBJ_SET (obj_cmd_rq, "authentication_type", authentication_type);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_level", maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  
  /* achu: challenge string must be zero extended */
  memset(buf, '\0', IPMI_CHALLENGE_STRING_LENGTH);
  memcpy(buf, challenge_string, challenge_string_len);
  
  FIID_OBJ_SET_DATA (obj_cmd_rq,
		     "challenge_string",
		     buf,
		     IPMI_CHALLENGE_STRING_LENGTH);

  FIID_OBJ_SET (obj_cmd_rq, 
                "initial_outbound_sequence_number", 
                initial_outbound_sequence_number);

  return (0);
}

int8_t 
fill_cmd_set_session_privilege_level (uint8_t privilege_level, 
                                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_PRIVILEGE_LEVEL_VALID(privilege_level)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_session_privilege_level_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL);
  FIID_OBJ_SET (obj_cmd_rq, "privilege_level", privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  return (0);
}  

int8_t 
fill_cmd_close_session (uint32_t close_session_id, 
			fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_close_session_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_CLOSE_SESSION);
  FIID_OBJ_SET (obj_cmd_rq, "session_id", close_session_id);
  return (0);
}  

int8_t 
fill_cmd_set_channel_access (uint8_t channel_number, 
			     uint8_t ipmi_messaging_access_mode, 
			     uint8_t user_level_authentication, 
			     uint8_t per_message_authentication, 
			     uint8_t pef_alerting, 
			     uint8_t channel_access_set, 
			     uint8_t channel_privilege_level_limit, 
			     uint8_t channel_privilege_level_limit_set,
                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_MESSAGING_ACCESS_MODE_VALID(ipmi_messaging_access_mode)
	      && IPMI_USER_LEVEL_AUTHENTICATION_VALID(user_level_authentication)
	      && IPMI_PER_MESSAGE_AUTHENTICATION_VALID(per_message_authentication)
	      && IPMI_PEF_ALERTING_VALID(pef_alerting)
	      && IPMI_CHANNEL_ACCESS_VALID(channel_access_set)
	      && IPMI_PRIVILEGE_LEVEL_LIMIT_VALID(channel_privilege_level_limit)
	      && IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VALID(channel_privilege_level_limit_set)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_channel_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_CHANNEL_ACCESS);
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "ipmi_messaging_access_mode", ipmi_messaging_access_mode);
  FIID_OBJ_SET (obj_cmd_rq, "user_level_authentication", user_level_authentication);
  FIID_OBJ_SET (obj_cmd_rq, "per_message_authentication", per_message_authentication);
  FIID_OBJ_SET (obj_cmd_rq, "pef_alerting", pef_alerting);
  FIID_OBJ_SET (obj_cmd_rq, "channel_access_set", channel_access_set);
  FIID_OBJ_SET (obj_cmd_rq, "channel_privilege_level_limit", channel_privilege_level_limit);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "channel_privilege_level_limit_set", channel_privilege_level_limit_set);
  return 0;
}

int8_t
fill_cmd_get_channel_access (uint8_t channel_number,
			     uint8_t channel_access_get,
                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_CHANNEL_ACCESS_GET_VALID(channel_access_get)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_channel_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_ACCESS);  
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "channel_access_get", channel_access_get);

  return 0;
}

int8_t 
fill_cmd_get_channel_info (uint8_t channel_number, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_channel_info_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_INFO_COMMAND);  
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  
  return 0;
}

int8_t
fill_cmd_set_channel_security_keys(uint8_t channel_number,
                                   uint8_t operation,
                                   uint8_t key_id,
                                   uint8_t *key_value,
                                   uint32_t key_value_len,
                                   fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
              && IPMI_CHANNEL_SECURITY_KEYS_OPERATION_VALID(operation)
              && IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_VALID(key_id)
	      && !((key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R
		    && key_value)
		   && key_value_len > IPMI_MAX_K_R_LENGTH)
	      && !((key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G
		    && key_value)
		   && key_value_len > IPMI_MAX_K_G_LENGTH)
              && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_channel_security_keys_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_CHANNEL_SECURITY_KEYS);  
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "key_id", key_id);
  if (operation == IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY)
    {
      uint8_t buf[IPMI_MAX_K_LENGTH];
      uint32_t buf_len;
      
      memset(buf, '\0', IPMI_MAX_K_LENGTH);
      
      if (key_value && key_value_len)
	memcpy(buf, key_value, key_value_len);
      
      if (key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R)
	buf_len = IPMI_MAX_K_R_LENGTH;
      else
	buf_len = IPMI_MAX_K_G_LENGTH;
      
      FIID_OBJ_SET_DATA (obj_cmd_rq,
			 "key_value",
			 buf,
			 buf_len);
    }
  return (0);
}

int8_t 
fill_cmd_set_user_access (uint8_t channel_number,
			  uint8_t user_ipmi_messaging,
			  uint8_t user_link_authentication,
			  uint8_t user_restricted_to_callback,
			  uint8_t user_id,
			  uint8_t user_privilege_level_limit,
			  uint8_t user_session_limit,
                          fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && IPMI_USER_IPMI_MESSAGING_VALID(user_ipmi_messaging)
	      && IPMI_USER_LINK_AUTHENTICATION_VALID(user_link_authentication)
	      && IPMI_USER_RESTRICTED_TO_CALLBACK_VALID(user_restricted_to_callback)
	      && IPMI_PRIVILEGE_LEVEL_LIMIT_VALID(user_privilege_level_limit)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_user_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_ACCESS_COMMAND);  
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "user_ipmi_messaging", user_ipmi_messaging);
  FIID_OBJ_SET (obj_cmd_rq, "user_link_authentication", user_link_authentication);
  FIID_OBJ_SET (obj_cmd_rq, "user_restricted_to_callback", user_restricted_to_callback);
  FIID_OBJ_SET (obj_cmd_rq, "change_bits_in_byte", 1);
  FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "user_privilege_level_limit", user_privilege_level_limit);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FIID_OBJ_SET (obj_cmd_rq, "user_session_limit", user_session_limit);
  FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);

  return 0;
}

int8_t
fill_cmd_get_user_access (uint8_t channel_number,
			  uint8_t user_id,
                          fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_CHANNEL_NUMBER_VALID(channel_number)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_user_access_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_USER_ACCESS_COMMAND);  
  FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return 0;
}

int8_t 
fill_cmd_set_user_name (uint8_t user_id, 
			char *user_name,
                        unsigned int user_name_len,
                        fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_MAX_USER_NAME_LENGTH];

  /* achu: user_name can be IPMI_MAX_USER_NAME_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  ERR_EINVAL (!(user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_user_name_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_NAME);  
  FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, "user_id.reserved", 0);
  
  /* achu: user_name must be zero extended */
  memset(buf, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (user_name)
    strncpy((char *)buf, user_name, IPMI_MAX_USER_NAME_LENGTH);
      
  FIID_OBJ_SET_DATA (obj_cmd_rq, "user_name", buf, IPMI_MAX_USER_NAME_LENGTH);
  
  return 0;
}

int8_t 
fill_cmd_get_user_name (uint8_t user_id, fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_get_user_name_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_USER_NAME_COMMAND);
  FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, "user_id.reserved", 0);

  return 0;
}

int8_t 
fill_cmd_set_user_password (uint8_t user_id, 
			    uint8_t operation, 
			    char *password,
                            unsigned int password_len,
                            fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_1_5_MAX_PASSWORD_LENGTH];

  /* achu: password can be the max length.  Null termination in IPMI
   * packet not required.
   */
  ERR_EINVAL (IPMI_PASSWORD_OPERATION_VALID(operation)
	      && !(password && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_user_password_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_PASSWORD_COMMAND);  
  FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, "user_id.reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FIID_OBJ_SET (obj_cmd_rq, "operation.reserved", 0);

  if (operation == IPMI_PASSWORD_OPERATION_SET_PASSWORD
      || operation == IPMI_PASSWORD_OPERATION_TEST_PASSWORD)
    {
      /* achu: password must be zero extended */
      memset(buf, '\0', IPMI_1_5_MAX_PASSWORD_LENGTH);
      if (password)
        strncpy((char *)buf, password, IPMI_1_5_MAX_PASSWORD_LENGTH);
      
      FIID_OBJ_SET_DATA (obj_cmd_rq, "password", buf, IPMI_1_5_MAX_PASSWORD_LENGTH);
    }

  return 0;
}

int8_t 
fill_cmd_set_user_password_v20 (uint8_t user_id, 
                                uint8_t password_size,
                                uint8_t operation, 
                                char *password,
                                unsigned int password_len,
                                fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_2_0_MAX_PASSWORD_LENGTH];

  /* achu: password can be the max length.  Null termination in IPMI
   * packet not required.
   *
   * Note that for IPMI 1.5 only machines, > 16 byte passwords will
   * simply fail and return an error.
   */
  ERR_EINVAL (IPMI_PASSWORD_OPERATION_VALID(operation)
              && IPMI_PASSWORD_SIZE_VALID(password_size)
	      && !(password 
                   && password_size == IPMI_PASSWORD_SIZE_16_BYTES
                   && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
	      && !(password 
                   && password_size == IPMI_PASSWORD_SIZE_20_BYTES
                   && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_cmd_set_user_password_v20_rq);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_PASSWORD_COMMAND);  
  FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FIID_OBJ_SET (obj_cmd_rq, "user_id.reserved", 0);
  FIID_OBJ_SET (obj_cmd_rq, "password_size", password_size);
  FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FIID_OBJ_SET (obj_cmd_rq, "operation.reserved", 0);

  if (operation == IPMI_PASSWORD_OPERATION_SET_PASSWORD
      || operation == IPMI_PASSWORD_OPERATION_TEST_PASSWORD)
    {
      uint32_t buf_max_len;

      if (password_size == IPMI_PASSWORD_SIZE_16_BYTES)
        buf_max_len = IPMI_1_5_MAX_PASSWORD_LENGTH;
      else
        buf_max_len = IPMI_2_0_MAX_PASSWORD_LENGTH;

      /* achu: password must be zero extended */
      memset(buf, '\0', buf_max_len);
      if (password)
        strncpy((char *)buf, password, buf_max_len);

      FIID_OBJ_SET_DATA (obj_cmd_rq, "password", buf, buf_max_len);
    }

  return 0;
}
