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
#include <assert.h>

#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-system-info-parameters-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define IPMI_MAX_K_LENGTH 64

fiid_template_t tmpl_cmd_set_bmc_global_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "receive_message_queue_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer_full_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "system_event_logging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_bmc_global_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_bmc_global_enables_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_bmc_global_enables_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "receive_message_queue_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer_full_interrupt", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "system_event_logging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_clear_message_flags_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_receive_message_queue", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_event_message_buffer", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_watchdog_pre_timeout_interrupt_flag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_oem_0", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_oem_1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "clear_oem_2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_clear_message_flags_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_message_flags_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_message_flags_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1, "receive_message_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "event_message_buffer_full", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "watchdog_pre_timeout_interrupt_occurred", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_0_data_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_1_data_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "oem_2_data_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_enable_message_channel_receive_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_enable_message_channel_receive_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_message_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_message_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "inferred_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "message_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_send_message_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "send_message_with_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "send_message_with_encryption", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "tracking_operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "message_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_send_message_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 1024, "response_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_read_event_message_buffer_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_read_event_message_buffer_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "message_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_interface_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "system_interface", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_interface_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_interface_capabilities_ssif_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "ssif_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pec_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "transaction_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "input_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "output_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_interface_capabilities_kcs_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "system_interface_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "input_maximum_message_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_bt_interface_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_bt_interface_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "number_of_outstanding_requests_supported", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "input_buffer_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* in bytes */
    { 8, "output_buffer_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* in bytes */
    { 8, "bmc_request_to_response_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* in seconds */
    { 8, "recommended_retries", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_master_write_read_rq = 
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "bus_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "bus_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "slave_address", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "read_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2040, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_master_write_read_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 2040, "data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_ipmi_v2.0_extended_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_authentication_capabilities_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.none", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.md2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.md5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.straight_password_key", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.oem_prop", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_type.ipmi_v2.0_extended_capabilities_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.anonymous_login", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.non_null_username", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "authentication_status.k_g", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "authentication_status.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_supports_ipmi_v1.5_connections", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_supports_ipmi_v2.0_connections", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "oem_auxiliary_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_guid_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_guid_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu: format in IPMI spec differs from the format in the "Wired for
   Management Baseline" 2.0 document.  In the IPMI spec there is a 2 byte
   field called "clock_seq_and_reserved", and in "Wired for Management
   Baseline" there is a 1 byte field called
   "clock_seq_hi_and_reserved" and a 1 byte field called
   "clock_seq_low".  I am going with the "Wired for Management
   Baseline" format under the assumption the IPMI spec has a typo.
*/
fiid_template_t tmpl_cmd_get_system_guid_format_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 48, "node", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 8, "clock_seq_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "clock_seq_hi_and_reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "time_high_and_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 16, "time_mid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 32, "time_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_set_in_progress_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_system_firmware_version_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_system_firmware_version_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_system_name_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_system_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_primary_operating_system_name_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_primary_operating_system_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_operating_system_name_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_operating_system_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_present_os_version_number_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_present_os_version_number_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_bmc_url_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_bmc_url_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_base_os_hypervisor_url_first_set_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_system_info_parameters_base_os_hypervisor_url_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1024, "configuration_parameter_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_set_in_progress_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_system_firmware_version_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_system_firmware_version_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_system_name_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_system_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_primary_operating_system_name_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_primary_operating_system_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_operating_system_name_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_operating_system_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_present_os_version_number_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_present_os_version_number_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_bmc_url_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_bmc_url_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_base_os_hypervisor_url_first_set_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "encoding", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "string_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 112, "string", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_system_info_parameters_base_os_hypervisor_url_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "parameter_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_cipher_suites_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "list_index", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "list_algorithm_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_cipher_suites_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "cipher_suite_record_data", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_session_challenge_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 32, "temp_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "challenge_string", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "initial_outbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_activate_session_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "initial_inbound_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_privilege_level_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_session_privilege_level_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "session_handle", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_close_session_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };


fiid_template_t tmpl_cmd_set_channel_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_access_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_privilege_level_limit_set", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_channel_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "channel_access_get", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 3, "ipmi_messaging_access_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_level_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "per_message_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "pef_alerting", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_info_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_channel_info_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "actual_channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "actual_channel_number.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "channel_medium_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "channel_medium_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 5, "channel_protocol_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "channel_protocol_type.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "active_session_count", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "session_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "vendor_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "auxiliary_channel_info", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_channel_security_keys_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "key_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 160, "key_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_channel_security_keys_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 2, "lock_status", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 6, "reserved", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 160, "key_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "change_bits_in_byte", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "user_session_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_access_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_access_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 6, "max_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "current_channel_user_ids", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_id_enable_status", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "current_channel_fixed_names", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "user_privilege_level_limit", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_ipmi_messaging", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_link_authentication", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_restricted_to_callback", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_name_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_user_name_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu: Note that the password is variable length, but it must be
 * fixed to 0, 16, or 20 bytes.  We may try and amend this situation
 * in fiid at a later time.
 */
fiid_template_t tmpl_cmd_set_user_password_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "user_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "user_id.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "password_size", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "operation", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "operation.reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 160, "password", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_user_password_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

int
fill_cmd_set_bmc_global_enables (uint8_t receive_message_queue_interrupt,
                                 uint8_t event_message_buffer_full_interrupt,
                                 uint8_t event_message_buffer,
                                 uint8_t system_event_logging,
                                 uint8_t oem_0,
                                 uint8_t oem_1,
                                 uint8_t oem_2,
                                 fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_BMC_GLOBAL_ENABLES_VALID (receive_message_queue_interrupt)
      || !IPMI_BMC_GLOBAL_ENABLES_VALID (event_message_buffer_full_interrupt)
      || !IPMI_BMC_GLOBAL_ENABLES_VALID (event_message_buffer)
      || !IPMI_BMC_GLOBAL_ENABLES_VALID (system_event_logging)
      || !IPMI_BMC_GLOBAL_ENABLES_VALID (oem_0)
      || !IPMI_BMC_GLOBAL_ENABLES_VALID (oem_1)
      || !IPMI_BMC_GLOBAL_ENABLES_VALID (oem_2)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_bmc_global_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_BMC_GLOBAL_ENABLES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "receive_message_queue_interrupt", receive_message_queue_interrupt);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_message_buffer_full_interrupt", event_message_buffer_full_interrupt);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "event_message_buffer", event_message_buffer);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "system_event_logging", system_event_logging);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_0", oem_0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_1", oem_1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "oem_2", oem_2);

  return (0);
}

int
fill_cmd_get_bmc_global_enables (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_bmc_global_enables_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_BMC_GLOBAL_ENABLES);

  return (0);
}

int
fill_cmd_clear_message_flags (uint8_t receive_message_queue,
                              uint8_t event_message_buffer,
                              uint8_t watchdog_pre_timeout_interrupt_flag,
                              uint8_t oem_0,
                              uint8_t oem_1,
                              uint8_t oem_2,
                              fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_MESSAGE_FLAGS_VALID (receive_message_queue)
      || !IPMI_MESSAGE_FLAGS_VALID (event_message_buffer)
      || !IPMI_MESSAGE_FLAGS_VALID (watchdog_pre_timeout_interrupt_flag)
      || !IPMI_MESSAGE_FLAGS_VALID (oem_0)
      || !IPMI_MESSAGE_FLAGS_VALID (oem_1)
      || !IPMI_MESSAGE_FLAGS_VALID (oem_2)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_clear_message_flags_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_CLEAR_MESSAGE_FLAGS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "clear_receive_message_queue", receive_message_queue);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "clear_event_message_buffer", event_message_buffer);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "clear_watchdog_pre_timeout_interrupt_flag", watchdog_pre_timeout_interrupt_flag);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "clear_oem_0", oem_0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "clear_oem_1", oem_1);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "clear_oem_2", oem_2);
  return (0);
}

int
fill_cmd_get_message_flags (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_message_flags_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_MESSAGE_FLAGS);
  return (0);
}

int
fill_cmd_enable_message_channel_receive (uint8_t channel_number,
                                         uint8_t channel_operation,
                                         fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_CHANNEL_OPERATION_VALID (channel_operation)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_enable_message_channel_receive_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_ENABLE_MESSAGE_CHANNEL_RECEIVE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_operation", channel_operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  return (0);
}

int
fill_cmd_get_message (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_message_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_MESSAGE);
  return (0);
}

int
fill_cmd_send_message (uint8_t channel_number,
                       uint8_t message_authentication,
                       uint8_t message_encryption,
                       uint8_t tracking_operation,
                       const void *message_data,
                       unsigned int message_data_len,
                       fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_SEND_MESSAGE_AUTHENTICATION_VALID (message_authentication)
      || !IPMI_SEND_MESSAGE_ENCRYPTION_VALID (message_encryption)
      || !IPMI_SEND_MESSAGE_TRACKING_VALID (tracking_operation)
      || !message_data
      || !message_data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_send_message_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SEND_MESSAGE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "send_message_with_authentication", message_authentication);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "send_message_with_encryption", message_encryption);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "tracking_operation", tracking_operation);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "message_data",
                          message_data,
                          message_data_len);
  return (0);
}

int
fill_cmd_read_event_message_buffer (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_read_event_message_buffer_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_READ_EVENT_MESSAGE_BUFFER);
  return (0);
}

int
fill_cmd_get_system_interface_capabilities (uint8_t system_interface,
                                            fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_INTERFACE_VALID (system_interface)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_system_interface_capabilities_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_INTERFACE_CAPABILITIES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "system_interface", system_interface);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  return (0);
}

int
fill_cmd_get_bt_interface_capabilities (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_bt_interface_capabilities_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_BT_INTERFACE_CAPABILITIES);
  return (0);
}

int
fill_cmd_master_write_read (uint8_t bus_type,
                            uint8_t bus_id,
                            uint8_t channel_number,
                            uint8_t slave_address,
                            uint8_t read_count,
                            const void *data,
                            unsigned int data_len,
                            fiid_obj_t obj_cmd_rq)
{
  /* note, don't check channel number, since this is a master write-read command */
  if (!IPMI_BUS_TYPE_VALID (bus_type)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_master_write_read_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_MASTER_WRITE_READ);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "bus_type", bus_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "bus_id", bus_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "slave_address", slave_address);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "read_count", read_count);
  if (data && data_len)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "data", data, data_len);

  return (0);
}


int
fill_cmd_get_channel_authentication_capabilities (uint8_t channel_number,
                                                  uint8_t maximum_privilege_level,
                                                  uint8_t get_ipmi_v20_extended_data,
                                                  fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_PRIVILEGE_LEVEL_VALID (maximum_privilege_level)
      || !IPMI_GET_IPMI_DATA_VALID (get_ipmi_v20_extended_data)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_AUTHENTICATION_CAPABILITIES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "get_ipmi_v2.0_extended_data", get_ipmi_v20_extended_data);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_level", maximum_privilege_level);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return (0);
}

int
fill_cmd_get_system_guid (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_system_guid_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_GUID);

  return (0);
}

int
fill_cmd_set_system_info_parameters (uint8_t parameter_selector,
                                     const void *configuration_parameter_data,
                                     unsigned int configuration_parameter_data_len,
                                     fiid_obj_t obj_cmd_rq)
{
  if ((!IPMI_SYSTEM_INFO_PARAMETER_SELECTOR_VALID (parameter_selector)
       && !IPMI_SYSTEM_INFO_PARAMETER_SELECTOR_IS_OEM (parameter_selector))
      || !configuration_parameter_data
      || !configuration_parameter_data_len
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_info_parameters_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_INFO_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "configuration_parameter_data",
                          configuration_parameter_data,
                          configuration_parameter_data_len);

  return (0);
}
  
int
fill_cmd_set_system_info_parameters_set_in_progress (uint8_t state,
                                                     fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_SYSTEM_INFO_PARAMETERS_SET_IN_PROGRESS_VALID (state)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_system_info_parameters_set_in_progress_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_INFO_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", IPMI_SYSTEM_INFO_PARAMETER_SYSTEM_FIRMWARE_VERSION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "state", state);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

static int
_fill_cmd_set_system_info_parameters_string_first_set_common (uint8_t set_selector,
							      uint8_t encoding,
							      uint8_t string_length,
							      const void *string_block,
							      unsigned int string_block_length,
							      fiid_obj_t obj_cmd_rq,
							      fiid_field_t *tmpl_cmd_rq_expected,
							      uint8_t parameter_selector)
{
  uint8_t stringbuf[IPMI_SYSTEM_INFO_FIRST_SET_STRING_LEN_MAX];

  assert (tmpl_cmd_rq_expected);

  if (!IPMI_SYSTEM_INFO_ENCODING_VALID (encoding)
      || string_block_length > IPMI_SYSTEM_INFO_FIRST_SET_STRING_LEN_MAX
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_rq_expected) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_INFO_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "encoding", encoding);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "string_length", string_length);

  /* achu: spec is not clear if this data should be 0 extended, we
   * will do it to be on the safe side
   */
  memset (stringbuf, '\0', IPMI_SYSTEM_INFO_FIRST_SET_STRING_LEN_MAX);
  if (string_block && string_block_length)
    memcpy (stringbuf, string_block, string_block_length);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "string", stringbuf, IPMI_SYSTEM_INFO_FIRST_SET_STRING_LEN_MAX);

  return (0);
}

static int
_fill_cmd_set_system_info_parameters_string_set_common (uint8_t set_selector,
							const void *string_block,
							unsigned int string_block_length,
							fiid_obj_t obj_cmd_rq,
							fiid_field_t *tmpl_cmd_rq_expected,
							uint8_t parameter_selector)
{
  uint8_t stringbuf[IPMI_SYSTEM_INFO_SET_STRING_LEN_MAX];

  assert (tmpl_cmd_rq_expected);

  if (string_block_length > IPMI_SYSTEM_INFO_SET_STRING_LEN_MAX
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_rq_expected) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
    
  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SYSTEM_INFO_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
   
  /* achu: spec is not clear if this data should be 0 extended, we
   * will do it to be on the safe side
   */
  memset (stringbuf, '\0', IPMI_SYSTEM_INFO_SET_STRING_LEN_MAX);
  if (string_block && string_block_length)
    memcpy (stringbuf, string_block, string_block_length);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "string", stringbuf, IPMI_SYSTEM_INFO_SET_STRING_LEN_MAX);
   
  return (0);
}

int
fill_cmd_set_system_info_parameters_system_firmware_version_first_set (uint8_t set_selector,
                                                                       uint8_t encoding,
                                                                       uint8_t string_length,
                                                                       const void *string_block,
                                                                       unsigned int string_block_length,
                                                                       fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_first_set_common (set_selector,
								    encoding,
								    string_length,
								    string_block,
								    string_block_length,
								    obj_cmd_rq,
								    tmpl_cmd_set_system_info_parameters_system_firmware_version_first_set_rq,
								    IPMI_SYSTEM_INFO_PARAMETER_SYSTEM_FIRMWARE_VERSION) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_system_firmware_version (uint8_t set_selector,
                                                             const void *string_block,
                                                             unsigned int string_block_length,
                                                             fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_set_common (set_selector,
							      string_block,
							      string_block_length,
							      obj_cmd_rq,
							      tmpl_cmd_set_system_info_parameters_system_firmware_version_rq,
							      IPMI_SYSTEM_INFO_PARAMETER_SYSTEM_FIRMWARE_VERSION) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_system_name_first_set (uint8_t set_selector,
                                                           uint8_t encoding,
                                                           uint8_t string_length,
                                                           const void *string_block,
                                                           unsigned int string_block_length,
                                                           fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_first_set_common (set_selector,
								    encoding,
								    string_length,
								    string_block,
								    string_block_length,
								    obj_cmd_rq,
								    tmpl_cmd_set_system_info_parameters_system_name_first_set_rq,
								    IPMI_SYSTEM_INFO_PARAMETER_SYSTEM_NAME) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}
  
int
fill_cmd_set_system_info_parameters_system_name (uint8_t set_selector,
                                                 const void *string_block,
                                                 unsigned int string_block_length,
                                                 fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_set_common (set_selector,
							      string_block,
							      string_block_length,
							      obj_cmd_rq,
							      tmpl_cmd_set_system_info_parameters_system_name_rq,
							      IPMI_SYSTEM_INFO_PARAMETER_SYSTEM_NAME) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_primary_operating_system_name_first_set (uint8_t set_selector,
                                                                             uint8_t encoding,
                                                                             uint8_t string_length,
                                                                             const void *string_block,
                                                                             unsigned int string_block_length,
                                                                             fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_first_set_common (set_selector,
								    encoding,
								    string_length,
								    string_block,
								    string_block_length,
								    obj_cmd_rq,
								    tmpl_cmd_set_system_info_parameters_primary_operating_system_name_first_set_rq,
								    IPMI_SYSTEM_INFO_PARAMETER_PRIMARY_OPERATING_SYSTEM_NAME) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_primary_operating_system_name (uint8_t set_selector,
                                                                   const void *string_block,
                                                                   unsigned int string_block_length,
                                                                   fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_set_common (set_selector,
							      string_block,
							      string_block_length,
							      obj_cmd_rq,
							      tmpl_cmd_set_system_info_parameters_primary_operating_system_name_rq,
							      IPMI_SYSTEM_INFO_PARAMETER_PRIMARY_OPERATING_SYSTEM_NAME) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_operating_system_name_first_set (uint8_t set_selector,
                                                                     uint8_t encoding,
                                                                     uint8_t string_length,
                                                                     const void *string_block,
                                                                     unsigned int string_block_length,
                                                                     fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_first_set_common (set_selector,
								    encoding,
								    string_length,
								    string_block,
								    string_block_length,
								    obj_cmd_rq,
								    tmpl_cmd_set_system_info_parameters_operating_system_name_first_set_rq,
								    IPMI_SYSTEM_INFO_PARAMETER_OPERATING_SYSTEM_NAME) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_operating_system_name (uint8_t set_selector,
                                                           const void *string_block,
                                                           unsigned int string_block_length,
                                                           fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_set_common (set_selector,
							      string_block,
							      string_block_length,
							      obj_cmd_rq,
							      tmpl_cmd_set_system_info_parameters_operating_system_name_rq,
							      IPMI_SYSTEM_INFO_PARAMETER_OPERATING_SYSTEM_NAME) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_present_os_version_number_first_set (uint8_t set_selector,
									 uint8_t encoding,
									 uint8_t string_length,
									 const void *string_block,
									 unsigned int string_block_length,
									 fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_first_set_common (set_selector,
								    encoding,
								    string_length,
								    string_block,
								    string_block_length,
								    obj_cmd_rq,
								    tmpl_cmd_set_system_info_parameters_present_os_version_number_first_set_rq,
								    IPMI_SYSTEM_INFO_PARAMETER_PRESENT_OS_VERSION_NUMBER) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_present_os_version_number (uint8_t set_selector,
							       const void *string_block,
							       unsigned int string_block_length,
							       fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_set_common (set_selector,
							      string_block,
							      string_block_length,
							      obj_cmd_rq,
							      tmpl_cmd_set_system_info_parameters_present_os_version_number_rq,
							      IPMI_SYSTEM_INFO_PARAMETER_PRESENT_OS_VERSION_NUMBER) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_bmc_url_first_set (uint8_t set_selector,
						       uint8_t encoding,
						       uint8_t string_length,
						       const void *string_block,
						       unsigned int string_block_length,
						       fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_first_set_common (set_selector,
								    encoding,
								    string_length,
								    string_block,
								    string_block_length,
								    obj_cmd_rq,
								    tmpl_cmd_set_system_info_parameters_bmc_url_first_set_rq,
								    IPMI_SYSTEM_INFO_PARAMETER_BMC_URL) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_bmc_url (uint8_t set_selector,
					     const void *string_block,
					     unsigned int string_block_length,
					     fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_set_common (set_selector,
							      string_block,
							      string_block_length,
							      obj_cmd_rq,
							      tmpl_cmd_set_system_info_parameters_bmc_url_rq,
							      IPMI_SYSTEM_INFO_PARAMETER_BMC_URL) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_base_os_hypervisor_url_first_set (uint8_t set_selector,
								      uint8_t encoding,
								      uint8_t string_length,
								      const void *string_block,
								      unsigned int string_block_length,
								      fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_first_set_common (set_selector,
								    encoding,
								    string_length,
								    string_block,
								    string_block_length,
								    obj_cmd_rq,
								    tmpl_cmd_set_system_info_parameters_base_os_hypervisor_url_first_set_rq,
								    IPMI_SYSTEM_INFO_PARAMETER_BASE_OS_HYPERVISOR_URL) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_set_system_info_parameters_base_os_hypervisor_url (uint8_t set_selector,
							    const void *string_block,
							    unsigned int string_block_length,
							    fiid_obj_t obj_cmd_rq)
{
  if (_fill_cmd_set_system_info_parameters_string_set_common (set_selector,
							      string_block,
							      string_block_length,
							      obj_cmd_rq,
							      tmpl_cmd_set_system_info_parameters_base_os_hypervisor_url_rq,
							      IPMI_SYSTEM_INFO_PARAMETER_BASE_OS_HYPERVISOR_URL) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  return (0);
}

int
fill_cmd_get_system_info_parameters (uint8_t get_parameter,
                                     uint8_t parameter_selector,
                                     uint8_t set_selector,
                                     uint8_t block_selector,
                                     fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_GET_SYSTEM_INFO_PARAMETER_VALID (get_parameter)
      || (!IPMI_SYSTEM_INFO_PARAMETER_SELECTOR_VALID (parameter_selector)
          && !IPMI_SYSTEM_INFO_PARAMETER_SELECTOR_IS_OEM (parameter_selector))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_system_info_parameters_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "get_parameter", get_parameter);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "parameter_selector", parameter_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_selector", set_selector);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "block_selector", block_selector);

  return (0);
}

int
fill_cmd_get_channel_cipher_suites (uint8_t channel_number,
                                    uint8_t payload_type,
                                    uint8_t list_index,
                                    uint8_t list_algorithm_type,
                                    fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !IPMI_LIST_ALGORITHM_TYPE_VALID (list_algorithm_type)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_channel_cipher_suites_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_CIPHER_SUITES);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "list_index", list_index);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "list_algorithm_type", list_algorithm_type);
  return (0);
}

int
fill_cmd_get_session_challenge (uint8_t authentication_type,
                                const char *user_name,
                                unsigned int user_name_len,
                                fiid_obj_t obj_cmd_rq)
{
  char buf[IPMI_MAX_USER_NAME_LENGTH];

  /* achu: user_name can be IPMI_MAX_USER_NAME_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  if (!IPMI_AUTHENTICATION_TYPE_VALID (authentication_type)
      || (user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_session_challenge_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SESSION_CHALLENGE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "authentication_type", authentication_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  /* achu: user_name must be zero extended */
  memset (buf, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (user_name)
    strncpy (buf, user_name, IPMI_MAX_USER_NAME_LENGTH);

  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "user_name", buf, IPMI_MAX_USER_NAME_LENGTH);

  return (0);
}

int
fill_cmd_activate_session (uint8_t authentication_type,
                           uint8_t maximum_privilege_level,
                           const void *challenge_string,
                           unsigned int challenge_string_len,
                           uint32_t initial_outbound_sequence_number,
                           fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_CHALLENGE_STRING_LENGTH];

  if (!IPMI_AUTHENTICATION_TYPE_VALID (authentication_type)
      || !IPMI_PRIVILEGE_LEVEL_VALID (maximum_privilege_level)
      || !challenge_string
      || (challenge_string_len > IPMI_CHALLENGE_STRING_LENGTH)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_activate_session_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_ACTIVATE_SESSION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "authentication_type", authentication_type);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "maximum_privilege_level", maximum_privilege_level);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  /* achu: challenge string must be zero extended */
  memset (buf, '\0', IPMI_CHALLENGE_STRING_LENGTH);
  memcpy (buf, challenge_string, challenge_string_len);

  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "challenge_string",
                          buf,
                          IPMI_CHALLENGE_STRING_LENGTH);

  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "initial_outbound_sequence_number",
                     initial_outbound_sequence_number);

  return (0);
}

int
fill_cmd_set_session_privilege_level (uint8_t privilege_level,
                                      fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_PRIVILEGE_LEVEL_VALID (privilege_level)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_session_privilege_level_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_SESSION_PRIVILEGE_LEVEL);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "privilege_level", privilege_level);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  return (0);
}

int
fill_cmd_close_session (uint32_t session_id,
                        uint8_t *session_handle,
                        fiid_obj_t obj_cmd_rq)
{
  /* if session_handle, session_id must be 0, see spec */
  if (!fiid_obj_valid (obj_cmd_rq)
      || (session_handle && session_id))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_close_session_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_CLOSE_SESSION);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "session_id", session_id);
  if (session_handle)
    FILL_FIID_OBJ_SET (obj_cmd_rq, "session_handle", (*session_handle));
  return (0);
}

int
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
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_MESSAGING_ACCESS_MODE_VALID (ipmi_messaging_access_mode)
      || !IPMI_USER_LEVEL_AUTHENTICATION_VALID (user_level_authentication)
      || !IPMI_PER_MESSAGE_AUTHENTICATION_VALID (per_message_authentication)
      || !IPMI_PEF_ALERTING_VALID (pef_alerting)
      || !IPMI_CHANNEL_ACCESS_VALID (channel_access_set)
      || !IPMI_PRIVILEGE_LEVEL_LIMIT_VALID (channel_privilege_level_limit)
      || !IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VALID (channel_privilege_level_limit_set)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_channel_access_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_CHANNEL_ACCESS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "ipmi_messaging_access_mode", ipmi_messaging_access_mode);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_level_authentication", user_level_authentication);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "per_message_authentication", per_message_authentication);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "pef_alerting", pef_alerting);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_access_set", channel_access_set);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_privilege_level_limit", channel_privilege_level_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_privilege_level_limit_set", channel_privilege_level_limit_set);
  return (0);
}

int
fill_cmd_get_channel_access (uint8_t channel_number,
                             uint8_t channel_access_get,
                             fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_CHANNEL_ACCESS_GET_VALID (channel_access_get)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_channel_access_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_ACCESS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_access_get", channel_access_get);

  return (0);
}

int
fill_cmd_get_channel_info (uint8_t channel_number, fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_channel_info_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_CHANNEL_INFO_COMMAND);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved", 0);

  return (0);
}

int
fill_cmd_set_channel_security_keys (uint8_t channel_number,
                                    uint8_t operation,
                                    uint8_t key_id,
                                    const void *key_value,
                                    unsigned int key_value_len,
                                    fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_CHANNEL_SECURITY_KEYS_OPERATION_VALID (operation)
      || !IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_VALID (key_id)
      || ((key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R
           && key_value)
          && key_value_len > IPMI_MAX_K_R_LENGTH)
      || ((key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_G
           && key_value)
          && key_value_len > IPMI_MAX_K_G_LENGTH)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_channel_security_keys_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_CHANNEL_SECURITY_KEYS);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "key_id", key_id);
  if (operation == IPMI_CHANNEL_SECURITY_KEYS_OPERATION_SET_KEY)
    {
      uint8_t buf[IPMI_MAX_K_LENGTH];
      unsigned int buf_len;

      memset (buf, '\0', IPMI_MAX_K_LENGTH);

      if (key_value && key_value_len)
        memcpy (buf, key_value, key_value_len);

      if (key_id == IPMI_CHANNEL_SECURITY_KEYS_KEY_ID_K_R)
        buf_len = IPMI_MAX_K_R_LENGTH;
      else
        buf_len = IPMI_MAX_K_G_LENGTH;

      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                              "key_value",
                              buf,
                              buf_len);
    }
  return (0);
}

int
fill_cmd_set_user_access (uint8_t channel_number,
                          uint8_t user_ipmi_messaging,
                          uint8_t user_link_authentication,
                          uint8_t user_restricted_to_callback,
                          uint8_t change_bits_in_byte,
                          uint8_t user_id,
                          uint8_t user_privilege_level_limit,
                          uint8_t user_session_limit,
                          fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_USER_IPMI_MESSAGING_VALID (user_ipmi_messaging)
      || !IPMI_USER_LINK_AUTHENTICATION_VALID (user_link_authentication)
      || !IPMI_USER_RESTRICTED_TO_CALLBACK_VALID (user_restricted_to_callback)
      || !IPMI_CHANGE_BITS_VALID (change_bits_in_byte)
      || !IPMI_PRIVILEGE_LEVEL_LIMIT_VALID (user_privilege_level_limit)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_user_access_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_ACCESS_COMMAND);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_ipmi_messaging", user_ipmi_messaging);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_link_authentication", user_link_authentication);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_restricted_to_callback", user_restricted_to_callback);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "change_bits_in_byte", change_bits_in_byte);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_privilege_level_limit", user_privilege_level_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_session_limit", user_session_limit);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved3", 0);

  return (0);
}

int
fill_cmd_get_user_access (uint8_t channel_number,
                          uint8_t user_id,
                          fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_user_access_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_USER_ACCESS_COMMAND);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "channel_number", channel_number);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "reserved2", 0);

  return (0);
}

int
fill_cmd_set_user_name (uint8_t user_id,
                        const char *user_name,
                        unsigned int user_name_len,
                        fiid_obj_t obj_cmd_rq)
{
  char buf[IPMI_MAX_USER_NAME_LENGTH];

  /* achu: user_name can be IPMI_MAX_USER_NAME_LENGTH length.  Null
   * termination in IPMI packet not required
   */
  if ((user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_user_name_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_NAME);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id.reserved", 0);

  /* achu: user_name must be zero extended */
  memset (buf, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (user_name)
    strncpy (buf, user_name, IPMI_MAX_USER_NAME_LENGTH);

  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "user_name", buf, IPMI_MAX_USER_NAME_LENGTH);

  return (0);
}

int
fill_cmd_get_user_name (uint8_t user_id, fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_user_name_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_USER_NAME_COMMAND);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id.reserved", 0);

  return (0);
}

int
fill_cmd_set_user_password (uint8_t user_id,
                            uint8_t password_size,
                            uint8_t operation,
                            const char *password,
                            unsigned int password_len,
                            fiid_obj_t obj_cmd_rq)
{
  /* achu: password can be the max length.  Null termination in IPMI
   * packet not required.
   *
   * Note that for IPMI 1.5 only machines, > 16 byte passwords will
   * simply fail and return an error.
   */
  if (!IPMI_PASSWORD_OPERATION_VALID (operation)
      || !IPMI_PASSWORD_SIZE_VALID (password_size)
      || (password
          && password_size == IPMI_PASSWORD_SIZE_16_BYTES
          && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
      || (password
          && password_size == IPMI_PASSWORD_SIZE_20_BYTES
          && password_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_user_password_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_USER_PASSWORD_COMMAND);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id", user_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "user_id.reserved", 0);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "password_size", password_size);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation", operation);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "operation.reserved", 0);

  if (operation == IPMI_PASSWORD_OPERATION_SET_PASSWORD
      || operation == IPMI_PASSWORD_OPERATION_TEST_PASSWORD)
    {
      char buf[IPMI_2_0_MAX_PASSWORD_LENGTH];
      unsigned int buf_max_len;

      if (password_size == IPMI_PASSWORD_SIZE_16_BYTES)
        buf_max_len = IPMI_1_5_MAX_PASSWORD_LENGTH;
      else
        buf_max_len = IPMI_2_0_MAX_PASSWORD_LENGTH;

      /* achu: password must be zero extended */
      memset (buf, '\0', buf_max_len);
      if (password)
        strncpy (buf, password, buf_max_len);

      FILL_FIID_OBJ_SET_DATA (obj_cmd_rq, "password", buf, buf_max_len);
    }

  return (0);
}
