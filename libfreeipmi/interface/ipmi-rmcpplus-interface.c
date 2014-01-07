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
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/cmds/ipmi-sol-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/payload/ipmi-sol-payload.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/util/ipmi-rmcpplus-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-network.h"
#include "libcommon/ipmi-crypt.h"
#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-md5.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"
#include "secure.h"

#define IPMI_MAX_RMCPPLUS_AUTHENTICATION_CODE_LENGTH      64
#define IPMI_MAX_CONFIDENTIALITY_HEADER_LENGTH            64
#define IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH           64
#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH  64

#define IPMI_MAX_INTEGRITY_DATA_LENGTH                    32

fiid_template_t tmpl_rmcpplus_session_hdr =
  {
    { 4, "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type.authenticated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "payload_type.encrypted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 16, "oem_payload_id", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 32, "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h outside of a session, seperate #'s if authenticated or unauthenticated session */
    { 32, "session_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* length of just the payload */
    { 16, "ipmi_payload_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* doesn't exist if session_id = 0h */
fiid_template_t tmpl_rmcpplus_session_trlr =
  {
    { 32, "integrity_pad", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 8, "pad_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "next_header", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 256, "authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };
/* note: the ipmi spec wording is terrible.  The integrity pad is to
 * ensure that the data passed to the HMAC is a multiple of 4, not
 * just the integrity field.  Sigh ...
 */

fiid_template_t tmpl_rmcpplus_payload =
  {
    { 512, "confidentiality_header", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    { 524288, "payload_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    { 512, "confidentiality_trailer", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_open_session_request =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "requested_maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "authentication_payload.authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "integrity_payload.integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "confidentiality_payload.confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_open_session_response =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 4, "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h not valid */
    { 32, "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "authentication_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "authentication_payload.authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "integrity_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "integrity_payload.integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "confidentiality_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "confidentiality_payload.confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_1 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "remote_console_random_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "requested_maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "name_only_lookup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 3, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "user_name_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "user_name", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_2 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "managed_system_random_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 128, "managed_system_guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "key_exchange_authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_3 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "key_exchange_authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

/* achu: The IPMI 2.0 Spec version 1.0 lists the 4th field as
 * "management_console_session_id", not "managed_system_session_id"
 * or "remote_console_session_id".  I'm assuming this is a typo and
 * that "remote_console_session_id" is what is really meant.
 */
fiid_template_t tmpl_rmcpplus_rakp_message_4 =
  {
    { 8, "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 16, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 512, "integrity_check_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

int
ipmi_rmcpplus_init (void)
{
  if (crypt_init ())
    return (-1);
  return (0);
}

int
fill_rmcpplus_session_hdr (uint8_t payload_type,
                           uint8_t payload_authenticated,
                           uint8_t payload_encrypted,
                           uint32_t oem_iana,
                           uint16_t oem_payload_id,
                           uint32_t session_id,
                           uint32_t session_sequence_number,
                           fiid_obj_t obj_rmcpplus_session_hdr)
{
  if (!IPMI_PAYLOAD_TYPE_VALID (payload_type)
      || !IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID (payload_authenticated)
      || !IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID (payload_encrypted)
      || (IPMI_PAYLOAD_TYPE_SESSION_SETUP (payload_type)
          && (payload_authenticated
              || payload_encrypted
              || session_id
              || session_sequence_number))
      || !fiid_obj_valid (obj_rmcpplus_session_hdr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_rmcpplus_session_hdr);

  FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "authentication_type", IPMI_AUTHENTICATION_TYPE_RMCPPLUS);
  FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "reserved1", 0);
  FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type", payload_type);
  FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type.authenticated", payload_authenticated);
  FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type.encrypted", payload_encrypted);
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "oem_iana", oem_iana);
      FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "reserved2", 0);
      FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "oem_payload_id", oem_payload_id);
    }
  FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "session_id", session_id);
  FILL_FIID_OBJ_SET (obj_rmcpplus_session_hdr, "session_sequence_number", session_sequence_number);

  /* ipmi_payload_len will be calculated during packet assembly */

  return (0);
}

int
fill_rmcpplus_session_trlr (fiid_obj_t obj_rmcpplus_session_trlr)
{
  if (!fiid_obj_valid (obj_rmcpplus_session_trlr))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_rmcpplus_session_trlr);

  /* Computing hashes and checking for correct input is done during
   * the packet assembly.  Padding calculations will also be done
   * during packet assembly.
   */

  FILL_FIID_OBJ_SET (obj_rmcpplus_session_trlr, "next_header", IPMI_NEXT_HEADER);

  return (0);
}

int
fill_rmcpplus_payload (const void *confidentiality_header,
                       unsigned int confidentiality_header_len,
                       const void *payload_data,
                       unsigned int payload_data_len,
                       const void *confidentiality_trailer,
                       unsigned int confidentiality_trailer_len,
                       fiid_obj_t obj_cmd_rq)
{
  if ((confidentiality_header && confidentiality_header_len > IPMI_MAX_CONFIDENTIALITY_HEADER_LENGTH)
      || (payload_data && payload_data_len > IPMI_MAX_PAYLOAD_LENGTH)
      || (confidentiality_trailer && confidentiality_trailer_len > IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_rmcpplus_payload) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  if (confidentiality_header)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                            "confidentiality_header",
                            confidentiality_header,
                            confidentiality_header_len);

  if (payload_data)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                            "payload_data",
                            payload_data,
                            payload_data_len);

  if (confidentiality_trailer)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                            "confidentiality_trailer",
                            confidentiality_trailer,
                            confidentiality_trailer_len);

  return (0);
}

int
fill_rmcpplus_open_session (uint8_t message_tag,
                            uint8_t requested_maximum_privilege_level,
                            uint32_t remote_console_session_id,
                            uint8_t authentication_algorithm,
                            uint8_t integrity_algorithm,
                            uint8_t confidentiality_algorithm,
                            fiid_obj_t obj_cmd_rq)
{
  if (!IPMI_2_0_PRIVILEGE_LEVEL_VALID (requested_maximum_privilege_level)
      || !IPMI_AUTHENTICATION_ALGORITHM_VALID (authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_VALID (integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_VALID (confidentiality_algorithm)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_rmcpplus_open_session_request) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "message_tag",
                     message_tag);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved1",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved2",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "requested_maximum_privilege_level",
                     requested_maximum_privilege_level);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "remote_console_session_id",
                     remote_console_session_id);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "authentication_payload.payload_type",
                     IPMI_AUTHENTICATION_PAYLOAD_TYPE);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved3",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "authentication_payload.payload_length",
                     IPMI_AUTHENTICATION_PAYLOAD_LENGTH);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "authentication_payload.authentication_algorithm",
                     authentication_algorithm);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved4",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved5",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "integrity_payload.payload_type",
                     IPMI_INTEGRITY_PAYLOAD_TYPE);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved6",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "integrity_payload.payload_length",
                     IPMI_INTEGRITY_PAYLOAD_LENGTH);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "integrity_payload.integrity_algorithm",
                     integrity_algorithm);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved7",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved8",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "confidentiality_payload.payload_type",
                     IPMI_CONFIDENTIALITY_PAYLOAD_TYPE);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved9",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "confidentiality_payload.payload_length",
                     IPMI_CONFIDENTIALITY_PAYLOAD_LENGTH);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "confidentiality_payload.confidentiality_algorithm",
                     confidentiality_algorithm);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved10",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved11",
                     0);

  return (0);
}

int
fill_rmcpplus_rakp_message_1 (uint8_t message_tag,
                              uint32_t managed_system_session_id,
                              const void *remote_console_random_number,
                              unsigned int remote_console_random_number_len,
                              uint8_t requested_maximum_privilege_level,
                              uint8_t name_only_lookup_flag,
                              const char *user_name,
                              unsigned int user_name_len,
                              fiid_obj_t obj_cmd_rq)
{
  if (!remote_console_random_number
      || (remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
      || !IPMI_PRIVILEGE_LEVEL_VALID (requested_maximum_privilege_level)
      || !IPMI_USER_NAME_LOOKUP_VALID (name_only_lookup_flag)
      || (user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_rmcpplus_rakp_message_1) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "message_tag",
                     message_tag);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved1",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "managed_system_session_id",
                     managed_system_session_id);
  FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                          "remote_console_random_number",
                          remote_console_random_number,
                          remote_console_random_number_len);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "requested_maximum_privilege_level",
                     requested_maximum_privilege_level);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "name_only_lookup",
                     name_only_lookup_flag);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved2",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved3",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "user_name_length",
                     user_name_len);

  if (user_name && user_name_len)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                            "user_name",
                            user_name,
                            user_name_len);

  return (0);
}

int
fill_rmcpplus_rakp_message_3 (uint8_t message_tag,
                              uint8_t rmcpplus_status_code,
                              uint32_t managed_system_session_id,
                              const void *key_exchange_authentication_code,
                              unsigned int key_exchange_authentication_code_len,
                              fiid_obj_t obj_cmd_rq)
{
  if ((key_exchange_authentication_code
       && key_exchange_authentication_code_len > IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)
      || !RMCPPLUS_STATUS_VALID (rmcpplus_status_code)
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_rmcpplus_rakp_message_3) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);

  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "message_tag",
                     message_tag);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "reserved1",
                     0);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "rmcpplus_status_code",
                     rmcpplus_status_code);
  FILL_FIID_OBJ_SET (obj_cmd_rq,
                     "managed_system_session_id",
                     managed_system_session_id);

  if (key_exchange_authentication_code && key_exchange_authentication_code_len > 0)
    FILL_FIID_OBJ_SET_DATA (obj_cmd_rq,
                            "key_exchange_authentication_code",
                            key_exchange_authentication_code,
                            key_exchange_authentication_code_len);

  return (0);
}

static int
_construct_payload_buf (uint8_t payload_type,
                        fiid_obj_t obj_lan_msg_hdr,
                        fiid_obj_t obj_cmd,
                        void *payload_buf,
                        unsigned int payload_buf_len)
{
  int obj_lan_msg_hdr_len = 0;
  int obj_cmd_len = 0;
  int obj_lan_msg_trlr_len = 0;
  int checksum_start_offset, len, rv = -1;
  unsigned int payload_len;
  uint8_t checksum;
  unsigned int indx = 0;
  fiid_obj_t obj_lan_msg_trlr = NULL;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid (obj_lan_msg_hdr)
                    && fiid_obj_template_compare (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
                    && fiid_obj_packet_valid (obj_lan_msg_hdr) == 1))
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1))
          && payload_buf
          && payload_buf_len);

  memset (payload_buf, '\0', payload_buf_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if ((obj_lan_msg_hdr_len = fiid_obj_len_bytes (obj_lan_msg_hdr)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
          goto cleanup;
        }
      if ((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }

  if ((obj_cmd_len = fiid_obj_len_bytes (obj_cmd)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }

  payload_len = obj_lan_msg_hdr_len + obj_cmd_len + obj_lan_msg_trlr_len;

  if (payload_len > IPMI_MAX_PAYLOAD_LENGTH)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  if (payload_len > payload_buf_len)
    {
      SET_ERRNO (ENOSPC);
      goto cleanup;
    }

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if ((len = fiid_obj_get_all (obj_lan_msg_hdr,
                                   payload_buf + indx,
                                   payload_buf_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
          goto cleanup;
        }
      indx += len;
    }

  if ((len = fiid_obj_get_all (obj_cmd,
                               payload_buf + indx,
                               payload_buf_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      goto cleanup;
    }
  indx += len;

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (!(obj_lan_msg_trlr = fiid_obj_create (tmpl_lan_msg_trlr)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if ((checksum_start_offset = fiid_template_field_end_bytes (tmpl_lan_msg_hdr_rq,
                                                                  "checksum1")) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
      checksum = ipmi_checksum (payload_buf + checksum_start_offset, indx - checksum_start_offset);

      if (fiid_obj_set_all (obj_lan_msg_trlr, &checksum, sizeof (checksum)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
          goto cleanup;
        }

      if ((len = fiid_obj_get_all (obj_lan_msg_trlr,
                                   payload_buf + indx,
                                   payload_buf_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
          goto cleanup;
        }
      indx += len;
    }

  rv = indx;
 cleanup:
  if (rv < 0)
    memset (payload_buf, '\0', payload_buf_len);
  fiid_obj_destroy (obj_lan_msg_trlr);
  return (rv);
}

static int
_construct_payload_confidentiality_none (uint8_t payload_type,
                                         fiid_obj_t obj_lan_msg_hdr,
                                         fiid_obj_t obj_cmd,
                                         fiid_obj_t obj_rmcpplus_payload)
{
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  int payload_len;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !fiid_obj_valid (obj_lan_msg_hdr))
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1))
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1);

  if ((payload_len = _construct_payload_buf (payload_type,
                                             obj_lan_msg_hdr,
                                             obj_cmd,
                                             payload_buf,
                                             IPMI_MAX_PAYLOAD_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         payload_buf,
                         payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  return (payload_len);
}

static int
_construct_payload_confidentiality_aes_cbc_128 (uint8_t payload_type,
                                                uint8_t payload_encrypted,
                                                fiid_obj_t obj_lan_msg_hdr,
                                                fiid_obj_t obj_cmd,
                                                const void *confidentiality_key,
                                                unsigned int confidentiality_key_len,
                                                fiid_obj_t obj_rmcpplus_payload)
{
  uint8_t iv[IPMI_CRYPT_AES_CBC_128_IV_LENGTH];
  int iv_len;
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_len, pad_tmp;
  int payload_len, cipher_keylen, cipher_blocklen, encrypt_len;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && payload_encrypted
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !fiid_obj_valid (obj_lan_msg_hdr))
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1))
          && fiid_obj_packet_valid (obj_cmd) == 1
          && confidentiality_key
          && confidentiality_key_len
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1);

  if ((cipher_keylen = crypt_cipher_key_len (IPMI_CRYPT_CIPHER_AES)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  assert (!(cipher_keylen < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));

  if (confidentiality_key_len < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  confidentiality_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;

  if ((cipher_blocklen = crypt_cipher_block_len (IPMI_CRYPT_CIPHER_AES)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  assert (cipher_blocklen == IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);

  if ((iv_len = ipmi_get_random (iv, IPMI_CRYPT_AES_CBC_128_IV_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (iv_len != IPMI_CRYPT_AES_CBC_128_IV_LENGTH)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if ((payload_len = _construct_payload_buf (payload_type,
                                             obj_lan_msg_hdr,
                                             obj_cmd,
                                             payload_buf,
                                             IPMI_MAX_PAYLOAD_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  /* Pad the data appropriately */

  /* +1 is for the pad length field */
  pad_tmp = ((payload_len + 1) % IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
  if (pad_tmp)
    pad_len = IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH - pad_tmp;
  else
    pad_len = 0;

  if ((payload_len + pad_len + 1) > IPMI_MAX_PAYLOAD_LENGTH)
    {
      SET_ERRNO (ENOSPC);
      return (-1);
    }

  if (pad_len)
    {
      unsigned int i;
      for (i = 0; i < pad_len; i++)
        payload_buf[payload_len + i] = i + 1;
    }
  payload_buf[payload_len + pad_len] = pad_len;

  /* +1 for pad length field */
  if ((encrypt_len = crypt_cipher_encrypt (IPMI_CRYPT_CIPHER_AES,
					   IPMI_CRYPT_CIPHER_MODE_CBC,
					   confidentiality_key,
					   confidentiality_key_len,
					   iv,
					   iv_len,
					   payload_buf,
					   payload_len + pad_len + 1)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (encrypt_len != (payload_len + pad_len + 1))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "confidentiality_header",
                         iv,
                         IPMI_CRYPT_AES_CBC_128_IV_LENGTH) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         payload_buf,
                         payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "confidentiality_trailer",
                         payload_buf + payload_len,
                         pad_len + 1) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  return (iv_len + payload_len + pad_len + 1);
}

static int
_construct_payload_rakp (uint8_t payload_type,
                         fiid_obj_t obj_cmd,
                         fiid_obj_t obj_rmcpplus_payload)
{
  uint8_t obj_cmd_buf[IPMI_MAX_PAYLOAD_LENGTH];
  int obj_cmd_len = 0;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
               && fiid_obj_template_compare (obj_cmd, tmpl_rmcpplus_open_session_request) < 0)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
               && fiid_obj_template_compare (obj_cmd, tmpl_rmcpplus_rakp_message_1) < 0)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
               && fiid_obj_template_compare (obj_cmd, tmpl_rmcpplus_rakp_message_3) < 0)
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && fiid_obj_packet_valid (obj_cmd) == 1);

  if ((obj_cmd_len = fiid_obj_get_all (obj_cmd,
                                       obj_cmd_buf,
                                       IPMI_MAX_PAYLOAD_LENGTH)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         obj_cmd_buf,
                         obj_cmd_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  return (obj_cmd_len);
}

static int
_construct_payload (uint8_t payload_type,
                    uint8_t payload_encrypted,
                    uint8_t authentication_algorithm,
                    uint8_t confidentiality_algorithm,
                    fiid_obj_t obj_lan_msg_hdr,
                    fiid_obj_t obj_cmd,
                    const void *confidentiality_key,
                    unsigned int confidentiality_key_len,
                    fiid_obj_t obj_rmcpplus_payload)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
	  && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !fiid_obj_valid (obj_lan_msg_hdr))
          && fiid_obj_valid (obj_cmd)
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI
      || payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {

      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return (_construct_payload_confidentiality_none (payload_type,
                                                         obj_lan_msg_hdr,
                                                         obj_cmd,
                                                         obj_rmcpplus_payload));
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return (_construct_payload_confidentiality_aes_cbc_128 (payload_type,
                                                                payload_encrypted,
                                                                obj_lan_msg_hdr,
                                                                obj_cmd,
                                                                confidentiality_key,
                                                                confidentiality_key_len,
                                                                obj_rmcpplus_payload));
    }
  else
    return (_construct_payload_rakp (payload_type,
                                     obj_cmd,
                                     obj_rmcpplus_payload));
}

static int
_construct_session_trlr_pad (uint8_t integrity_algorithm,
                             unsigned int ipmi_msg_len,
                             fiid_obj_t obj_rmcpplus_session_trlr)
{
  int pad_length_field_len, next_header_field_len;
  unsigned int pad_length = 0;
  uint8_t pad_bytes[IPMI_INTEGRITY_PAD_MULTIPLE] = { IPMI_INTEGRITY_PAD_DATA,
                                                     IPMI_INTEGRITY_PAD_DATA,
                                                     IPMI_INTEGRITY_PAD_DATA,
                                                     IPMI_INTEGRITY_PAD_DATA};

  assert (IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
          && fiid_obj_valid (obj_rmcpplus_session_trlr)
          && fiid_obj_template_compare (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr) == 1);

  if ((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr,
                                                             "pad_length")) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if ((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr,
                                                              "next_header")) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  ipmi_msg_len += pad_length_field_len;
  ipmi_msg_len += next_header_field_len;

  if (ipmi_msg_len % IPMI_INTEGRITY_PAD_MULTIPLE)
    pad_length = IPMI_INTEGRITY_PAD_MULTIPLE - (ipmi_msg_len % IPMI_INTEGRITY_PAD_MULTIPLE);

  if (pad_length)
    {
      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "integrity_pad",
                             pad_bytes,
                             pad_length) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          return (-1);
        }
    }

  if (fiid_obj_set (obj_rmcpplus_session_trlr,
                    "pad_length",
                    pad_length) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
      return (-1);
    }

  return (0);
}

static int
_calculate_authentication_code_len (uint8_t integrity_algorithm)
{
  int authentication_code_len;

  assert ((integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128));

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
    authentication_code_len = 0;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
  else /* IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128 */
    authentication_code_len = IPMI_HMAC_SHA256_128_AUTHENTICATION_CODE_LENGTH;

  return (authentication_code_len);
}

static int
_construct_session_trlr_authentication_code (uint8_t integrity_algorithm,
                                             const void *integrity_key,
                                             unsigned int integrity_key_len,
                                             const void *authentication_code_data,
                                             unsigned int authentication_code_data_len,
                                             fiid_obj_t obj_rmcpplus_session_trlr,
                                             void *pkt_data,
                                             unsigned int pkt_data_len,
                                             void *authentication_code_buf,
                                             unsigned int authentication_code_buf_len)
{
  int crypt_digest_len, authentication_code_len, integrity_digest_len, len, rv = -1;
  unsigned int hash_algorithm, hash_flags, expected_digest_len, copy_digest_len, hash_data_len;
  uint8_t hash_data[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t integrity_digest[IPMI_MAX_INTEGRITY_DATA_LENGTH];
  uint8_t pwbuf[IPMI_2_0_MAX_PASSWORD_LENGTH];

  assert ((integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128)
          && !(integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
               && authentication_code_data
               && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid (obj_rmcpplus_session_trlr)
          && pkt_data
          && pkt_data_len
          && authentication_code_buf
          && authentication_code_buf_len);

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  /* Check if the user provided an authentication code, if so, use it */

  memset (authentication_code_buf, '\0', authentication_code_buf_len);

  if ((len = fiid_obj_field_len_bytes (obj_rmcpplus_session_trlr,
                                       "authentication_code")) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
      return (-1);
    }

  if (len)
    {
      if ((len = fiid_obj_get_data (obj_rmcpplus_session_trlr,
                                    "authentication_code",
                                    authentication_code_buf,
                                    authentication_code_buf_len)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          return (-1);
        }
      return (len);
    }

  if ((authentication_code_len = _calculate_authentication_code_len (integrity_algorithm)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /* Note: Integrity Key for HMAC_SHA1_96 and HMAC_MD5_128 is K1 */

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA1;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA1_DIGEST_LENGTH;
      copy_digest_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
    }
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_MD5_DIGEST_LENGTH;
      copy_digest_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = 0;
      expected_digest_len = MD5_DIGEST_LENGTH;
      copy_digest_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
  else /* IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128 */
    {
      hash_algorithm = IPMI_CRYPT_HASH_SHA256;
      hash_flags = IPMI_CRYPT_HASH_FLAGS_HMAC;
      expected_digest_len = IPMI_HMAC_SHA256_DIGEST_LENGTH;
      copy_digest_len = IPMI_HMAC_SHA256_128_AUTHENTICATION_CODE_LENGTH;
    }

  if ((crypt_digest_len = crypt_hash_digest_len (hash_algorithm)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  assert (crypt_digest_len == expected_digest_len);

  memset (hash_data, '\0', IPMI_MAX_PAYLOAD_LENGTH);

  hash_data_len = 0;

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      /* achu: Password must be zero padded */
      memset (pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);

      if (authentication_code_data && authentication_code_data_len)
        memcpy (pwbuf, authentication_code_data, authentication_code_data_len);

      memcpy (hash_data + hash_data_len,
              pwbuf,
              IPMI_2_0_MAX_PASSWORD_LENGTH);
      secure_memset (pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  memcpy (hash_data + hash_data_len, pkt_data, pkt_data_len);
  hash_data_len += pkt_data_len;

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      /* achu: Password must be zero padded */
      memset (pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);

      if (authentication_code_data && authentication_code_data_len)
        memcpy (pwbuf, authentication_code_data, authentication_code_data_len);

      memcpy (hash_data + hash_data_len,
              pwbuf,
              IPMI_2_0_MAX_PASSWORD_LENGTH);
      secure_memset (pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }

  if ((integrity_digest_len = crypt_hash (hash_algorithm,
					  hash_flags,
					  integrity_key,
					  integrity_key_len,
					  hash_data,
					  hash_data_len,
					  integrity_digest,
					  IPMI_MAX_INTEGRITY_DATA_LENGTH)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (integrity_digest_len != crypt_digest_len)
    {
      SET_ERRNO (EINVAL);
      goto cleanup;
    }

  if (integrity_digest_len > authentication_code_buf_len)
    {
      SET_ERRNO (ENOSPC);
      goto cleanup;
    }

  memcpy (authentication_code_buf, integrity_digest, copy_digest_len);

  rv = copy_digest_len;
 cleanup:
  secure_memset (integrity_digest, '\0', IPMI_MAX_INTEGRITY_DATA_LENGTH);
  return (rv);
}

int
assemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm,
                            uint8_t integrity_algorithm,
                            uint8_t confidentiality_algorithm,
                            const void *integrity_key,
                            unsigned int integrity_key_len,
                            const void *confidentiality_key,
                            unsigned int confidentiality_key_len,
                            const void *authentication_code_data,
                            unsigned int authentication_code_data_len,
                            fiid_obj_t obj_rmcp_hdr,
                            fiid_obj_t obj_rmcpplus_session_hdr,
                            fiid_obj_t obj_lan_msg_hdr,
                            fiid_obj_t obj_cmd,
                            fiid_obj_t obj_rmcpplus_session_trlr,
                            void *pkt,
                            unsigned int pkt_len,
			    unsigned int flags)
{
  unsigned int indx = 0;
  int obj_rmcp_hdr_len, obj_len, oem_iana_len, oem_payload_id_len, payload_len, len, rv = -1;
  uint8_t payload_type, payload_authenticated, payload_encrypted;
  uint32_t session_id, session_sequence_number;
  uint64_t val;
  fiid_obj_t obj_rmcpplus_payload = NULL;
  fiid_obj_t obj_session_hdr_temp = NULL;
  fiid_obj_t obj_rmcpplus_session_trlr_temp = NULL;
  unsigned int flags_mask = 0;

  /* achu: obj_lan_msg_hdr only needed for payload type IPMI
   *
   * If integrity_algorithm != NONE, technically the key can be NULL
   */
  if (!IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
      || (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
          && authentication_code_data
          && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
          && !(confidentiality_key
               && confidentiality_key_len))
      || !fiid_obj_valid (obj_rmcp_hdr)
      || !fiid_obj_valid (obj_rmcpplus_session_hdr)
      || !fiid_obj_valid (obj_cmd)
      || !pkt
      || !pkt_len
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcp_hdr, tmpl_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (FIID_OBJ_PACKET_VALID (obj_rmcp_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  if (FIID_OBJ_PACKET_VALID (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  /*
   * Can't use fiid_obj_packet_valid() on obj_rmcpplus_session_hdr b/c
   * a ipmi_payload_len is required but may not be set yet.
   */

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  payload_type = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type.authenticated",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  payload_authenticated = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type.encrypted",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  payload_encrypted = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "session_id",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  session_id = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "session_sequence_number",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  session_sequence_number = val;

  /* Note: We don't check consider OPEN_SESSION_RESPONSE, RAKP2 or
   * RAKP4 payload types b/c they are responses, not requests.
   */
  if ((payload_type != IPMI_PAYLOAD_TYPE_IPMI
       && payload_type != IPMI_PAYLOAD_TYPE_SOL
       && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
       && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
      || (IPMI_PAYLOAD_TYPE_SESSION_SETUP (payload_type)
          && (payload_authenticated
              || payload_encrypted
              || session_id
              || session_sequence_number))
      || (session_id
          && payload_authenticated == IPMI_PAYLOAD_FLAG_UNAUTHENTICATED
          && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_NONE)
      || (session_id
          && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED
          && !(integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128))
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
          && payload_encrypted != IPMI_PAYLOAD_FLAG_UNENCRYPTED))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (!fiid_obj_valid (obj_lan_msg_hdr))
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
      if (FIID_OBJ_PACKET_VALID (obj_lan_msg_hdr) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_sol_payload_data) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_request) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_1) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_3) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }

  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      /*
       * Can't use fiid_obj_packet_valid() on obj_rmcpplus_session_trlr b/c
       * integrity pad, pad length, and authentication code may not be set.
       */
      if (!fiid_obj_valid (obj_rmcpplus_session_trlr))
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }

  if ((oem_iana_len = fiid_obj_field_len (obj_rmcpplus_session_hdr, "oem_iana")) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  if ((oem_payload_id_len = fiid_obj_field_len (obj_rmcpplus_session_hdr, "oem_payload_id")) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }

  /* achu: The OEM payload type isn't supported, but I'll leave this
   * code for the sake of potential future support
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }
  else
    {
      if (oem_iana_len
          || oem_payload_id_len)
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }
    }

  memset (pkt, '\0', pkt_len);

  /*
   * Copy RMCP header into packet
   */
  indx = 0;
  if ((obj_rmcp_hdr_len = fiid_obj_len_bytes (obj_rmcp_hdr)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }

  if (obj_rmcp_hdr_len > (pkt_len - indx))
    {
      SET_ERRNO (ENOSPC);
      return (-1);
    }

  if ((obj_rmcp_hdr_len = fiid_obj_get_all (obj_rmcp_hdr,
                                            pkt + indx,
                                            pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  indx += obj_rmcp_hdr_len;

  /*
   * Copy Session Header into packet
   */
  if ((len = fiid_obj_block_len_bytes (obj_rmcpplus_session_hdr,
                                       "authentication_type",
                                       "session_sequence_number")) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  if (len > (pkt_len - indx))
    {
      SET_ERRNO (ENOSPC);
      return (-1);
    }

  if ((len = fiid_obj_get_block (obj_rmcpplus_session_hdr,
                                 "authentication_type",
                                 "session_sequence_number",
                                 pkt + indx,
                                 pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  indx += len;

  /*
   * Construct/Encrypt Payload and copy into packet
   */
  if (!(obj_rmcpplus_payload = fiid_obj_create (tmpl_rmcpplus_payload)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((payload_len = _construct_payload (payload_type,
                                         payload_encrypted,
                                         authentication_algorithm,
                                         confidentiality_algorithm,
                                         obj_lan_msg_hdr,
                                         obj_cmd,
                                         confidentiality_key,
                                         confidentiality_key_len,
                                         obj_rmcpplus_payload)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  /*
   * Copy IPMI Payload Length into packet
   */

  if (!(obj_session_hdr_temp = fiid_obj_create (tmpl_rmcpplus_session_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (fiid_obj_clear (obj_session_hdr_temp) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr_temp);
      goto cleanup;
    }

  if (fiid_obj_set (obj_session_hdr_temp,
                    "ipmi_payload_len",
                    payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr_temp);
      goto cleanup;
    }

  if ((obj_len = fiid_obj_len_bytes (obj_session_hdr_temp)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr_temp);
      goto cleanup;
    }

  if (obj_len > (pkt_len - indx))
    {
      SET_ERRNO (ENOSPC);
      goto cleanup;
    }

  if ((obj_len = fiid_obj_get_all (obj_session_hdr_temp,
                                   pkt + indx,
                                   pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_session_hdr_temp);
      goto cleanup;
    }
  indx += obj_len;

  /*
   * Copy IPMI Payload into packet
   */

  if ((obj_len = fiid_obj_len_bytes (obj_rmcpplus_payload)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      goto cleanup;
    }

  if (obj_len > (pkt_len - indx))
    {
      SET_ERRNO (ENOSPC);
      goto cleanup;
    }

  if ((obj_len = fiid_obj_get_all (obj_rmcpplus_payload,
                                   pkt + indx,
                                   pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      goto cleanup;
    }
  indx += obj_len;

  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      uint8_t authentication_code_buf[IPMI_MAX_PAYLOAD_LENGTH];
      int authentication_code_len;

      if (!(obj_rmcpplus_session_trlr_temp = fiid_obj_dup (obj_rmcpplus_session_trlr)))
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          goto cleanup;
        }

      if (_construct_session_trlr_pad (integrity_algorithm,
                                       (indx - obj_rmcp_hdr_len),
                                       obj_rmcpplus_session_trlr_temp) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if ((len = fiid_obj_block_len_bytes (obj_rmcpplus_session_trlr_temp,
                                           "integrity_pad",
                                           "next_header")) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr_temp);
          goto cleanup;
        }
      if (len > (pkt_len - indx))
        {
          SET_ERRNO (ENOSPC);
          goto cleanup;
        }

      if ((len = fiid_obj_get_block (obj_rmcpplus_session_trlr_temp,
                                     "integrity_pad",
                                     "next_header",
                                     pkt + indx,
                                     pkt_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr_temp);
          goto cleanup;
        }
      indx += len;

      /* achu: Note that the integrity code is all data prior to the authentication_code, so this
       * call must be done after the pad, pad length, and next header are copied into
       * the pkt buffer.
       */
      if ((authentication_code_len = _construct_session_trlr_authentication_code (integrity_algorithm,
                                                                                  integrity_key,
                                                                                  integrity_key_len,
                                                                                  authentication_code_data,
                                                                                  authentication_code_data_len,
                                                                                  obj_rmcpplus_session_trlr_temp,
                                                                                  pkt + obj_rmcp_hdr_len,
                                                                                  indx - obj_rmcp_hdr_len,
                                                                                  authentication_code_buf,
                                                                                  IPMI_MAX_PAYLOAD_LENGTH)) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }

      if (authentication_code_len)
        {
          if (authentication_code_len > (pkt_len - indx))
            {
              SET_ERRNO (ENOSPC);
              goto cleanup;
            }
          memcpy (pkt + indx, authentication_code_buf, authentication_code_len);
          indx += authentication_code_len;
        }
    }

  if (indx > INT_MAX)
    {
      SET_ERRNO (EMSGSIZE);
      goto cleanup;
    }

  rv = indx;
 cleanup:
  fiid_obj_destroy (obj_rmcpplus_payload);
  fiid_obj_destroy (obj_session_hdr_temp);
  if (obj_rmcpplus_session_trlr_temp)
    fiid_obj_clear (obj_rmcpplus_session_trlr_temp);
  fiid_obj_destroy (obj_rmcpplus_session_trlr_temp);
  return (rv);
}

/* return 1 on full parse, 0 if not, -1 on error */
static int
_deconstruct_payload_buf (uint8_t payload_type,
                          fiid_obj_t obj_lan_msg_hdr,
                          fiid_obj_t obj_cmd,
                          fiid_obj_t obj_lan_msg_trlr,
                          const void *pkt,
                          unsigned int lan_msg_len)
{
  int obj_lan_msg_trlr_len, len;
  unsigned int obj_cmd_len, indx = 0;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid (obj_lan_msg_hdr)
                    && fiid_obj_template_compare (obj_lan_msg_hdr,
                                                  tmpl_lan_msg_hdr_rs) == 1
                    && fiid_obj_valid (obj_lan_msg_trlr)
                    && fiid_obj_template_compare (obj_lan_msg_trlr,
                                                  tmpl_lan_msg_trlr) == 1))
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare (obj_cmd,
                                               tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare (obj_cmd,
                                                  tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt
          && lan_msg_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if ((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }

      if ((len = fiid_obj_set_all (obj_lan_msg_hdr,
                                   pkt + indx,
                                   lan_msg_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
          return (-1);
        }
      indx += len;

      if (indx >= lan_msg_len)
        {
          /* cannot parse packet */
          ERR_TRACE ("malformed packet", EINVAL);
          return (0);
        }

      /* achu: For payload_type == IPMI Whatever is in between the
       * header and the trailer is the command data
       */

      if ((lan_msg_len - indx) <= obj_lan_msg_trlr_len)
        {
          /* cannot parse packet */
          ERR_TRACE ("malformed packet", EINVAL);
          return (0);
        }

      obj_cmd_len = (lan_msg_len - indx) - obj_lan_msg_trlr_len;
    }
  else /* payload_type == IPMI_PAYLOAD_TYPE_SOL */
    obj_cmd_len = lan_msg_len;

  if ((len = fiid_obj_set_all (obj_cmd,
                               pkt + indx,
                               obj_cmd_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }
  indx += len;

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (indx >= lan_msg_len)
        {
          /* cannot parse packet */
          ERR_TRACE ("malformed packet", EINVAL);
          return (0);
        }

      if ((len = fiid_obj_set_all (obj_lan_msg_trlr,
                                   pkt + indx,
                                   lan_msg_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
          return (-1);
        }
    }

  return (1);
}

/* return 1 on full parse, 0 if not, -1 on error */
static int
_deconstruct_payload_confidentiality_none (uint8_t payload_type,
                                           fiid_obj_t obj_rmcpplus_payload,
                                           fiid_obj_t obj_lan_msg_hdr,
                                           fiid_obj_t obj_cmd,
                                           fiid_obj_t obj_lan_msg_trlr,
                                           const void *pkt,
                                           uint16_t ipmi_payload_len)
{
  int ret;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid (obj_lan_msg_hdr)
                    && fiid_obj_template_compare (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1
                    && fiid_obj_valid (obj_lan_msg_trlr)
                    && fiid_obj_template_compare (obj_lan_msg_trlr, tmpl_lan_msg_trlr) == 1))
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt
          && ipmi_payload_len);

  /* achu: No encryption, so ipmi_payload_len is the length of
   * the msg_hdr, cmd, and msg_trlr.
   */
  if ((ret = _deconstruct_payload_buf (payload_type,
                                       obj_lan_msg_hdr,
                                       obj_cmd,
                                       obj_lan_msg_trlr,
                                       pkt,
                                       ipmi_payload_len)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (!ret)
    return (0);

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         pkt,
                         ipmi_payload_len) < 0)

    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  return (1);
}

/* return 1 on full parse, 0 if not, -1 on error */
static int
_deconstruct_payload_confidentiality_aes_cbc_128 (uint8_t payload_type,
                                                  uint8_t payload_encrypted,
                                                  fiid_obj_t obj_rmcpplus_payload,
                                                  fiid_obj_t obj_lan_msg_hdr,
                                                  fiid_obj_t obj_cmd,
                                                  fiid_obj_t obj_lan_msg_trlr,
                                                  const void *confidentiality_key,
                                                  unsigned int confidentiality_key_len,
                                                  const void *pkt,
                                                  uint16_t ipmi_payload_len)
{
  uint8_t iv[IPMI_CRYPT_AES_CBC_128_IV_LENGTH];
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_length;
  int cipher_keylen, cipher_blocklen, decrypt_len;
  unsigned int payload_data_len, cmd_data_len, indx = 0;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && payload_encrypted == IPMI_PAYLOAD_FLAG_ENCRYPTED
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid (obj_lan_msg_hdr)
                    && fiid_obj_template_compare (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1
                    && fiid_obj_valid (obj_lan_msg_trlr)
                    && fiid_obj_template_compare (obj_lan_msg_trlr, tmpl_lan_msg_trlr) == 1))
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare (obj_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && confidentiality_key
          && pkt
          && ipmi_payload_len);

  if ((cipher_keylen = crypt_cipher_key_len (IPMI_CRYPT_CIPHER_AES)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  assert (!(cipher_keylen < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));

  if (confidentiality_key_len < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  confidentiality_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;

  if ((cipher_blocklen = crypt_cipher_block_len (IPMI_CRYPT_CIPHER_AES)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  assert (cipher_blocklen == IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);

  if (ipmi_payload_len < IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  payload_data_len = ipmi_payload_len - IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;

  if (!payload_data_len)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  memcpy (iv, pkt, IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
  indx += IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;
  memcpy (payload_buf, pkt + indx, payload_data_len);

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "confidentiality_header",
                         iv,
                         IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  if ((decrypt_len = crypt_cipher_decrypt (IPMI_CRYPT_CIPHER_AES,
					   IPMI_CRYPT_CIPHER_MODE_CBC,
					   confidentiality_key,
					   confidentiality_key_len,
					   iv,
					   IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH,
					   payload_buf,
					   payload_data_len)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (decrypt_len != payload_data_len)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  pad_length = payload_buf[payload_data_len - 1];
  if (pad_length > IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if ((pad_length + 1) > payload_data_len)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  cmd_data_len = payload_data_len - pad_length - 1;

  if (!cmd_data_len)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         payload_buf,
                         cmd_data_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "confidentiality_trailer",
                         payload_buf + cmd_data_len,
                         pad_length + 1), 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  /* achu: User is responsible for checking if padding is not corrupt  */

  return _deconstruct_payload_buf (payload_type,
                                   obj_lan_msg_hdr,
                                   obj_cmd,
                                   obj_lan_msg_trlr,
                                   payload_buf,
                                   cmd_data_len);
}

/* return 1 on full parse, 0 if not, -1 on error */
static int
_deconstruct_payload_rakp (uint8_t payload_type,
                           fiid_obj_t obj_rmcpplus_payload,
                           fiid_obj_t obj_cmd,
                           const void *pkt,
                           uint16_t ipmi_payload_len)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && fiid_obj_valid (obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
               && fiid_obj_template_compare (obj_cmd, tmpl_rmcpplus_open_session_response) < 0)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
               && fiid_obj_template_compare (obj_cmd, tmpl_rmcpplus_rakp_message_2) < 0)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4
               && fiid_obj_template_compare (obj_cmd, tmpl_rmcpplus_rakp_message_4) < 0)
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && pkt
          && ipmi_payload_len);

  if (fiid_obj_set_data (obj_rmcpplus_payload,
                         "payload_data",
                         pkt,
                         ipmi_payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  if (fiid_obj_set_all (obj_cmd,
                        pkt,
                        ipmi_payload_len) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  return (1);
}

/* return 1 on full parse, 0 if not, -1 on error */
static int
_deconstruct_payload (uint8_t payload_type,
                      uint8_t payload_encrypted,
                      uint8_t authentication_algorithm,
                      uint8_t confidentiality_algorithm,
                      fiid_obj_t obj_rmcpplus_payload,
                      fiid_obj_t obj_lan_msg_hdr,
                      fiid_obj_t obj_cmd,
                      fiid_obj_t obj_lan_msg_trlr,
                      const void *confidentiality_key,
                      unsigned int confidentiality_key_len,
                      const void *pkt,
                      uint16_t ipmi_payload_len)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID (payload_encrypted)
          && IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
	  && IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
          && fiid_obj_valid (obj_rmcpplus_payload)
          && fiid_obj_template_compare (obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && fiid_obj_valid (obj_cmd)
          && pkt
          && ipmi_payload_len);

  /* Note: We don't check consider OPEN_SESSION_REQUEST, RAKP1 or
   * RAKP3 special b/c they are requests, not responses
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI
      || payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return (_deconstruct_payload_confidentiality_none (payload_type,
                                                           obj_rmcpplus_payload,
                                                           obj_lan_msg_hdr,
                                                           obj_cmd,
                                                           obj_lan_msg_trlr,
                                                           pkt,
                                                           ipmi_payload_len));
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return (_deconstruct_payload_confidentiality_aes_cbc_128 (payload_type,
                                                                  payload_encrypted,
                                                                  obj_rmcpplus_payload,
                                                                  obj_lan_msg_hdr,
                                                                  obj_cmd,
                                                                  obj_lan_msg_trlr,
                                                                  confidentiality_key,
                                                                  confidentiality_key_len,
                                                                  pkt,
                                                                  ipmi_payload_len));
    }
  else
    return (_deconstruct_payload_rakp (payload_type,
                                       obj_rmcpplus_payload,
                                       obj_cmd,
                                       pkt,
                                       ipmi_payload_len));
}

int
unassemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm,
                              uint8_t integrity_algorithm,
                              uint8_t confidentiality_algorithm,
                              const void *integrity_key,
                              unsigned int integrity_key_len,
                              const void *confidentiality_key,
                              unsigned int confidentiality_key_len,
                              const void *pkt,
                              unsigned int pkt_len,
                              fiid_obj_t obj_rmcp_hdr,
                              fiid_obj_t obj_rmcpplus_session_hdr,
                              fiid_obj_t obj_rmcpplus_payload,
                              fiid_obj_t obj_lan_msg_hdr,
                              fiid_obj_t obj_cmd,
                              fiid_obj_t obj_lan_msg_trlr,
                              fiid_obj_t obj_rmcpplus_session_trlr,
			      unsigned int flags)
{
  unsigned int indx = 0;
  int obj_rmcp_hdr_len, obj_len;
  uint8_t payload_type, payload_authenticated, payload_encrypted;
  uint32_t session_id, session_sequence_number;
  uint16_t ipmi_payload_len;
  int ret, check_session_trlr_valid = 0;
  uint64_t val;
  unsigned int flags_mask = (IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK);

  /* achu: obj_lan_msg_hdr & trlr only needed for payload type IPMI
   *
   * If integrity_algorithm != NONE, technically the key can be NULL
   */
  if (!IPMI_AUTHENTICATION_ALGORITHM_SUPPORTED (authentication_algorithm)
      || !IPMI_INTEGRITY_ALGORITHM_SUPPORTED (integrity_algorithm)
      || !IPMI_CONFIDENTIALITY_ALGORITHM_SUPPORTED (confidentiality_algorithm)
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
          && !(confidentiality_key
               && confidentiality_key_len))
      || !pkt
      || !fiid_obj_valid (obj_rmcp_hdr)
      || !fiid_obj_valid (obj_rmcpplus_session_hdr)
      || !fiid_obj_valid (obj_rmcpplus_payload)
      || !fiid_obj_valid (obj_cmd)
      || (flags & ~flags_mask))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcp_hdr, tmpl_rmcp_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
  if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_payload, tmpl_rmcpplus_payload) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (fiid_obj_clear (obj_rmcp_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }

  if (fiid_obj_clear (obj_rmcpplus_session_hdr) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }

  if (fiid_obj_clear (obj_rmcpplus_payload) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_payload);
      return (-1);
    }

  if (fiid_obj_clear (obj_cmd) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_cmd);
      return (-1);
    }

  /*
   * Extract RMCP header
   */
  if ((obj_rmcp_hdr_len = fiid_obj_set_all (obj_rmcp_hdr,
                                            pkt + indx,
                                            pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcp_hdr);
      return (-1);
    }
  indx += obj_rmcp_hdr_len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  /*
   * Extract auth_type and payload information
   */
  if ((obj_len = fiid_obj_set_block (obj_rmcpplus_session_hdr,
                                     "authentication_type",
                                     "payload_type.encrypted",
                                     pkt + indx,
                                     pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  indx += obj_len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  payload_type = val;

  if (payload_type != IPMI_PAYLOAD_TYPE_IPMI
      && payload_type != IPMI_PAYLOAD_TYPE_SOL
      && payload_type != IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
      && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
      && payload_type != IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }
  
#if 0
  /*
   * Extract OEM IANA and OEM Payload ID
   */

  /* achu: The OEM payload type isn't supported, but I'll leave this
   * code for the sake of potential future support
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      if ((obj_len = fiid_obj_set_block (obj_rmcpplus_session_hdr,
                                         "oem_iana",
                                         "oem_payload_id",
                                         pkt + indx,
                                         pkt_len - indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
          return (-1);
        }
      indx += obj_len;

      if (pkt_len <= indx)
        {
          /* cannot parse packet */
          ERR_TRACE ("malformed packet", EINVAL);
          return (0);
        }
    }
#endif

  /*
   * Extract Session ID, Session Sequence Number, and Payload Length
   */
  if ((obj_len = fiid_obj_set_block (obj_rmcpplus_session_hdr,
                                     "session_id",
                                     "ipmi_payload_len",
                                     pkt + indx,
                                     pkt_len - indx)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  indx += obj_len;

  if (pkt_len <= indx)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type.authenticated",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  payload_authenticated = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "payload_type.encrypted",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  payload_encrypted = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "session_id",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  session_id = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "session_sequence_number",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  session_sequence_number = val;

  if (FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                    "ipmi_payload_len",
                    &val) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_hdr);
      return (-1);
    }
  ipmi_payload_len = val;

  if ((IPMI_PAYLOAD_TYPE_SESSION_SETUP (payload_type)
       && (payload_authenticated
           || payload_encrypted
           || session_id
           || session_sequence_number))
      || (session_id
          && payload_authenticated == IPMI_PAYLOAD_FLAG_UNAUTHENTICATED
          && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_NONE)
      || (session_id
          && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED
          && !(integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
               || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128))
      || (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
          && payload_encrypted != IPMI_PAYLOAD_FLAG_UNENCRYPTED)
      || !ipmi_payload_len)
    {
      /* cannot parse packet */
      ERR_TRACE ("malformed packet", EINVAL);
      return (0);
    }

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      if (!fiid_obj_valid (obj_lan_msg_hdr)
          || !fiid_obj_valid (obj_lan_msg_trlr))
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_lan_msg_trlr, tmpl_lan_msg_trlr) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
      if (fiid_obj_clear (obj_lan_msg_hdr) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_hdr);
          return (-1);
        }
      if (fiid_obj_clear (obj_lan_msg_trlr) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_lan_msg_trlr);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_sol_payload_data) < 0
          && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_sol_payload_data_bmc_to_remote_console) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_open_session_response) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_2) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    {
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd, tmpl_rmcpplus_rakp_message_4) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }

  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      if (!fiid_obj_valid (obj_rmcpplus_session_trlr))
        {
          SET_ERRNO (EINVAL);
          return (-1);
        }
      if (FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
      if (fiid_obj_clear (obj_rmcpplus_session_trlr) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          return (-1);
        }

      check_session_trlr_valid++;
    }

  if ((pkt_len - indx) < ipmi_payload_len)
    {
      ERR_TRACE ("shorten ipmi_payload_len", EINVAL);
      ipmi_payload_len = pkt_len - indx;
    }

  /*
   * Deconstruct/Decrypt Payload
   */
  if ((ret = _deconstruct_payload (payload_type,
                                   payload_encrypted,
                                   authentication_algorithm,
                                   confidentiality_algorithm,
                                   obj_rmcpplus_payload,
                                   obj_lan_msg_hdr,
                                   obj_cmd,
                                   obj_lan_msg_trlr,
                                   confidentiality_key,
                                   confidentiality_key_len,
                                   pkt + indx,
                                   ipmi_payload_len)) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  if (!ret)
    return (0);

  indx += ipmi_payload_len;

  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      int pad_length_field_len, next_header_field_len;
      unsigned int authentication_code_len;
      uint8_t pad_length;

      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_code_len = 0;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
        authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
      else /* IPMI_INTEGRITY_ALGORITHM_HMAC_SHA256_128 */
        authentication_code_len = IPMI_HMAC_SHA256_128_AUTHENTICATION_CODE_LENGTH;

      if ((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "pad_length")) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
      if ((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "next_header")) < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }

      /* achu: There needs to be atleast the next_header and pad_length fields */
      if ((pkt_len - indx) < (authentication_code_len + pad_length_field_len + next_header_field_len))
        {
          /* cannot parse packet */
          ERR_TRACE ("malformed packet", EINVAL);
          return (0);
        }

      if (authentication_code_len)
        {
          if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                                 "authentication_code",
                                 pkt + indx + ((pkt_len - indx) - authentication_code_len),
                                 authentication_code_len) < 0)
            {
              FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
              return (-1);
            }
        }

      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "next_header",
                             pkt + indx + ((pkt_len - indx) - authentication_code_len - next_header_field_len),
                             next_header_field_len) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          return (-1);
        }

      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "pad_length",
                             pkt + indx + ((pkt_len - indx) - authentication_code_len - next_header_field_len - pad_length_field_len),
                             pad_length_field_len) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          return (-1);
        }

      if (FIID_OBJ_GET (obj_rmcpplus_session_trlr,
                        "pad_length",
                        &val) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          return (-1);
        }
      pad_length = val;

      if (pad_length > IPMI_INTEGRITY_PAD_MULTIPLE)
        {
          /* cannot parse packet */
          ERR_TRACE ("malformed packet", EINVAL);
          return (0);
        }

      if (pad_length > (pkt_len - indx - authentication_code_len - pad_length_field_len - next_header_field_len))
        {
          ERR_TRACE ("shorten pad_length", EINVAL);
          pad_length = (pkt_len - indx - authentication_code_len - pad_length_field_len - next_header_field_len);
        }

      if (fiid_obj_set_data (obj_rmcpplus_session_trlr,
                             "integrity_pad",
                             pkt + indx,
                             pad_length) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_rmcpplus_session_trlr);
          return (-1);
        }
    }

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {  
      if (FIID_OBJ_PACKET_VALID (obj_rmcp_hdr) == 1
          && FIID_OBJ_PACKET_VALID (obj_rmcpplus_session_hdr) == 1
          && FIID_OBJ_PACKET_VALID (obj_rmcpplus_payload) == 1
          && FIID_OBJ_PACKET_VALID (obj_lan_msg_hdr) == 1
	  && ((flags & IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK) || FIID_OBJ_PACKET_SUFFICIENT (obj_cmd) == 1)
          && FIID_OBJ_PACKET_VALID (obj_lan_msg_trlr) == 1
          && (!check_session_trlr_valid
              || FIID_OBJ_PACKET_VALID (obj_rmcpplus_session_trlr) == 1))
        return (1);
    }
  else 
    {
      if (FIID_OBJ_PACKET_VALID (obj_rmcp_hdr) == 1
          && FIID_OBJ_PACKET_VALID (obj_rmcpplus_session_hdr) == 1
          && FIID_OBJ_PACKET_VALID (obj_rmcpplus_payload) == 1
	  && ((flags & IPMI_INTERFACE_FLAGS_NO_LEGAL_CHECK) || FIID_OBJ_PACKET_SUFFICIENT (obj_cmd) == 1)
          && (!check_session_trlr_valid
              || FIID_OBJ_PACKET_VALID (obj_rmcpplus_session_trlr) == 1))
        return (1);
    }

  return (0);
}

ssize_t
ipmi_rmcpplus_sendto (int s,
		      const void *buf,
		      size_t len,
		      int flags,
		      const struct sockaddr *to,
		      socklen_t tolen)
{
  /* achu: Per specification table 13-8, no legacy padding for IPMI
   * 2.0 packets, so call common sendto.
   */
  return (ipmi_network_sendto (s, buf, len, flags, to, tolen));
}
 
ssize_t
ipmi_rmcpplus_recvfrom (int s,
			void *buf,
			size_t len,
			int flags,
			struct sockaddr *from,
			socklen_t *fromlen)
{
  return (ipmi_network_recvfrom (s, buf, len, flags, from, fromlen));
}
