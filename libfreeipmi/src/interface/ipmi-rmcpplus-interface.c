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
#include <assert.h>
#include <errno.h>

#include "freeipmi/interface/ipmi-rmcpplus-interface.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"
#include "freeipmi/cmds/ipmi-sol-cmds.h"
#include "freeipmi/interface/ipmi-lan-interface.h"
#include "freeipmi/interface/rmcp-interface.h"
#include "freeipmi/spec/ipmi-authentication-type-spec.h"
#include "freeipmi/spec/ipmi-privilege-level-spec.h"
#include "freeipmi/spec/ipmi-rmcpplus-status-spec.h"
#include "freeipmi/util/ipmi-rmcpplus-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-crypt.h"
#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"
#include "libcommon/ipmi-md5.h"

#include "freeipmi-portability.h"
#include "secure.h"

#define IPMI_MAX_RMCPPLUS_AUTHENTICATION_CODE_LENGTH      64
#define IPMI_MAX_CONFIDENTIALITY_HEADER_LENGTH            64
#define IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH           64
#define IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH  64

#define IPMI_MAX_INTEGRITY_DATA_LENGTH                    32

fiid_template_t tmpl_rmcpplus_session_hdr = 
  {
    {4,   "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,   "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,   "payload_type.authenticated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,   "payload_type.encrypted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "oem_iana", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {8,   "reserved2", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {16,  "oem_payload_id", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    {32,  "session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h outside of a session, seperate #'s if authenticated or unauthenticated session */    
    {32,  "session_sequence_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* length of just the payload */
    {16,   "ipmi_payload_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},     
    {0, "", 0}
  };

/* doesn't exist if session_id = 0h */
fiid_template_t tmpl_rmcpplus_session_trlr = 
  {
    {32,  "integrity_pad", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},     
    {8,   "pad_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "next_header", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {256, "authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},         
    {0, "", 0}
  };
/* note: the ipmi spec wording is terrible.  The integrity pad is to
 * ensure that the data passed to the HMAC is a multiple of 4, not
 * just the integrity field.  Sigh ... 
 */

fiid_template_t tmpl_rmcpplus_payload = 
  {
    {512,    "confidentiality_header", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},  
    /* 524288 = 65536 * 8 = 2^16 * 8, b/c ipmi_payload_len is 2 bytes */
    {524288, "payload_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE}, 
    {512,    "confidentiality_trailer", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_open_session_request = 
  {
    {8,   "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},        
    {4,   "requested_maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,   "authentication_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "authentication_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "authentication_payload.authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "integrity_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "integrity_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "integrity_payload.integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "confidentiality_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "confidentiality_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "confidentiality_payload.confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_open_session_response = 
  {
    {8,   "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* 0h not valid */
    {32,  "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8,   "authentication_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "authentication_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "authentication_payload.authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "reserved4", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "reserved5", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "integrity_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved6", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "integrity_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "integrity_payload.integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "reserved7", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "reserved8", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "confidentiality_payload.payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved9", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "confidentiality_payload.payload_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {6,   "confidentiality_payload.confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {2,   "reserved10", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "reserved11", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_1 = 
  {
    {8,   "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "remote_console_random_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "requested_maximum_privilege_level", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,   "name_only_lookup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3,   "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "user_name_length", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "user_name", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_2 = 
  {
    {8,   "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "managed_system_random_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {128, "managed_system_guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {512, "key_exchange_authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

fiid_template_t tmpl_rmcpplus_rakp_message_3 = 
  {
    {8,   "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "managed_system_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {512, "key_exchange_authentication_code", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

/* achu: The IPMI 2.0 Spec version 1.0 lists the 4th field as
 * "management_console_session_id", * not "managed_system_session_id"
 * or "remote_console_session_id".  I'm assuming this is a typo and
 * that "remote_console_session_id" is what is really meant.
 */
fiid_template_t tmpl_rmcpplus_rakp_message_4 = 
  {
    {8,   "message_tag", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,   "rmcpplus_status_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {16,  "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "remote_console_session_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {512, "integrity_check_value", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE}, 
    {0, "", 0}
  };

int8_t 
ipmi_rmcpplus_init(void)
{
  if (ipmi_crypt_init())
    return -1;
  return 0;
}

int8_t
fill_rmcpplus_session_hdr (uint8_t payload_type, 
                           uint8_t payload_authenticated, 
                           uint8_t payload_encrypted, 
                           uint32_t oem_iana, 
                           uint16_t oem_payload_id, 
                           uint32_t session_id, 
                           uint32_t session_sequence_number, 
                           fiid_obj_t obj_rmcpplus_session_hdr)
{
  ERR_EINVAL (IPMI_PAYLOAD_TYPE_VALID(payload_type)
	      && IPMI_PAYLOAD_AUTHENTICATED_FLAG_VALID(payload_authenticated)
	      && IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_encrypted)
	      && !(IPMI_PAYLOAD_TYPE_SESSION_SETUP(payload_type)
		   && (payload_authenticated 
		       || payload_encrypted 
		       || session_id 
		       || session_sequence_number))
	      && fiid_obj_valid(obj_rmcpplus_session_hdr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);

  FIID_OBJ_CLEAR (obj_rmcpplus_session_hdr);

  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "authentication_type", IPMI_AUTHENTICATION_TYPE_RMCPPLUS);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "reserved1", 0);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type", payload_type);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type.authenticated", payload_authenticated);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type.encrypted", payload_encrypted);
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      FIID_OBJ_SET (obj_rmcpplus_session_hdr, "oem_iana", oem_iana);
      FIID_OBJ_SET (obj_rmcpplus_session_hdr, "reserved2", 0);
      FIID_OBJ_SET (obj_rmcpplus_session_hdr, "oem_payload_id", oem_payload_id);
    }
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "session_id", session_id);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "session_sequence_number", session_sequence_number);

  /* ipmi_payload_len will be calculated during packet assembly */

  return (0);
}

int8_t
fill_rmcpplus_session_trlr(fiid_obj_t obj_rmcpplus_session_trlr)
{
  ERR_EINVAL (fiid_obj_valid(obj_rmcpplus_session_trlr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);
  
  FIID_OBJ_CLEAR (obj_rmcpplus_session_trlr);

  /* Computing hashes and checking for correct input is done during
   * the packet assembly.  Padding calculations will also be done
   * during packet assembly.
   */

  FIID_OBJ_SET (obj_rmcpplus_session_trlr, "next_header", IPMI_NEXT_HEADER);

  return (0);
}

int8_t
fill_rmcpplus_payload(uint8_t *confidentiality_header,
                      uint32_t confidentiality_header_len,
                      uint8_t *payload_data,
                      uint32_t payload_data_len,
                      uint8_t *confidentiality_trailer,
                      uint32_t confidentiality_trailer_len,
                      fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (!(confidentiality_header && confidentiality_header_len > IPMI_MAX_CONFIDENTIALITY_HEADER_LENGTH)
	      && !(payload_data && payload_data_len > IPMI_MAX_PAYLOAD_LENGTH)
	      && !(confidentiality_trailer && confidentiality_trailer_len > IPMI_MAX_CONFIDENTIALITY_TRAILER_LENGTH) 
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_rmcpplus_payload);

  FIID_OBJ_CLEAR (obj_cmd_rq);
  
  if (confidentiality_header)
    FIID_OBJ_SET_DATA(obj_cmd_rq,
		      "confidentiality_header",
		      confidentiality_header,
		      confidentiality_header_len);

  if (payload_data)
    FIID_OBJ_SET_DATA(obj_cmd_rq,
		      "payload_data",
		      payload_data,
		      payload_data_len);

  if (confidentiality_trailer)
    FIID_OBJ_SET_DATA(obj_cmd_rq,
		      "confidentiality_trailer",
		      confidentiality_trailer,
		      confidentiality_trailer_len);

  return (0);
}
                             
int8_t
fill_rmcpplus_open_session (uint8_t message_tag,
                            uint8_t requested_maximum_privilege_level,
                            uint32_t remote_console_session_id,
                            uint8_t authentication_algorithm,
                            uint8_t integrity_algorithm,
                            uint8_t confidentiality_algorithm,
                            fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (IPMI_2_0_PRIVILEGE_LEVEL_VALID(requested_maximum_privilege_level)
	      && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
	      && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
	      && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_rmcpplus_open_session_request);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved1", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved2", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "requested_maximum_privilege_level", 
                requested_maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd_rq,
                "remote_console_session_id",
                remote_console_session_id);
  FIID_OBJ_SET (obj_cmd_rq,
                "authentication_payload.payload_type",
                IPMI_AUTHENTICATION_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved3", 
                0);
  FIID_OBJ_SET (obj_cmd_rq,
                "authentication_payload.payload_length",
                IPMI_AUTHENTICATION_PAYLOAD_LENGTH);
  FIID_OBJ_SET (obj_cmd_rq,
                "authentication_payload.authentication_algorithm",
                authentication_algorithm);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved4", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved5", 
                0);
  FIID_OBJ_SET (obj_cmd_rq,
                "integrity_payload.payload_type",
                IPMI_INTEGRITY_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved6", 
                0);
  FIID_OBJ_SET (obj_cmd_rq,
                "integrity_payload.payload_length",
                IPMI_INTEGRITY_PAYLOAD_LENGTH);
  FIID_OBJ_SET (obj_cmd_rq,
                "integrity_payload.integrity_algorithm",
                integrity_algorithm);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved7", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved8", 
                0);
  FIID_OBJ_SET (obj_cmd_rq,
                "confidentiality_payload.payload_type",
                IPMI_CONFIDENTIALITY_PAYLOAD_TYPE);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved9", 
                0);
  FIID_OBJ_SET (obj_cmd_rq,
                "confidentiality_payload.payload_length",
                IPMI_CONFIDENTIALITY_PAYLOAD_LENGTH);
  FIID_OBJ_SET (obj_cmd_rq,
                "confidentiality_payload.confidentiality_algorithm",
                confidentiality_algorithm);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved10", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved11", 
                0);

  return (0);
}

int8_t
fill_rmcpplus_rakp_message_1(uint8_t message_tag,
                             uint32_t managed_system_session_id,
                             uint8_t *remote_console_random_number,
                             uint32_t remote_console_random_number_len,
                             uint8_t requested_maximum_privilege_level,
                             uint8_t name_only_lookup_flag,
                             char *user_name,
                             uint32_t user_name_len,
                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (remote_console_random_number
	      && !(remote_console_random_number_len < IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH)
	      && IPMI_PRIVILEGE_LEVEL_VALID(requested_maximum_privilege_level)
	      && IPMI_USER_NAME_LOOKUP_VALID(name_only_lookup_flag)
	      && !(user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_rmcpplus_rakp_message_1);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved1", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "managed_system_session_id", 
                managed_system_session_id);
  FIID_OBJ_SET_DATA (obj_cmd_rq,
                     "remote_console_random_number",
                     remote_console_random_number,
                     remote_console_random_number_len);
  FIID_OBJ_SET (obj_cmd_rq, 
                "requested_maximum_privilege_level", 
                requested_maximum_privilege_level);
  FIID_OBJ_SET (obj_cmd_rq, 
                "name_only_lookup", 
                name_only_lookup_flag);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved2", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved3", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "user_name_length", 
                user_name_len);

  if (user_name && user_name_len)
    FIID_OBJ_SET_DATA (obj_cmd_rq,
                       "user_name",
                       (uint8_t *)user_name,
                       user_name_len);

  return (0);
}

int8_t
fill_rmcpplus_rakp_message_3(uint8_t message_tag,
                             uint8_t rmcpplus_status_code,
                             uint32_t managed_system_session_id,
                             uint8_t *key_exchange_authentication_code,
                             uint32_t key_exchange_authentication_code_len,
                             fiid_obj_t obj_cmd_rq)
{
  ERR_EINVAL (!(key_exchange_authentication_code && key_exchange_authentication_code_len > IPMI_MAX_KEY_EXCHANGE_AUTHENTICATION_CODE_LENGTH)
	      && RMCPPLUS_STATUS_VALID(rmcpplus_status_code)
	      && fiid_obj_valid(obj_cmd_rq));

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_rmcpplus_rakp_message_3);

  FIID_OBJ_CLEAR (obj_cmd_rq);

  FIID_OBJ_SET (obj_cmd_rq, 
                "message_tag", 
                message_tag);
  FIID_OBJ_SET (obj_cmd_rq, 
                "reserved1", 
                0);
  FIID_OBJ_SET (obj_cmd_rq, 
                "rmcpplus_status_code", 
                rmcpplus_status_code);
  FIID_OBJ_SET (obj_cmd_rq, 
                "managed_system_session_id", 
                managed_system_session_id);

  if (key_exchange_authentication_code && key_exchange_authentication_code_len > 0)
    FIID_OBJ_SET_DATA (obj_cmd_rq,
		       "key_exchange_authentication_code",
		       key_exchange_authentication_code,
		       key_exchange_authentication_code_len);

  return (0);
}

static int32_t
_construct_payload_buf(uint8_t payload_type,
                       fiid_obj_t obj_lan_msg_hdr,
                       fiid_obj_t obj_cmd,
                       uint8_t *payload_buf,
                       uint32_t payload_buf_len)
{
  int32_t obj_lan_msg_hdr_len = 0;
  int32_t obj_cmd_len = 0;
  int32_t obj_lan_msg_trlr_len = 0;
  int32_t checksum_start_offset;
  uint32_t payload_len;
  uint8_t checksum;
  int32_t len, indx = 0, rv = -1;
  fiid_obj_t obj_lan_msg_trlr = NULL;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid(obj_lan_msg_hdr)
                    && fiid_obj_template_compare(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
                    && fiid_obj_packet_valid(obj_lan_msg_hdr) == 1))
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1))
          && payload_buf
          && payload_buf_len);

  memset(payload_buf, '\0', payload_buf_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      ERR_EXIT (!((obj_lan_msg_hdr_len = fiid_obj_len_bytes (obj_lan_msg_hdr)) < 0));
      ERR_EXIT (!((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));
    }

  ERR_CLEANUP (!((obj_cmd_len = fiid_obj_len_bytes (obj_cmd)) < 0));
  
  payload_len = obj_lan_msg_hdr_len + obj_cmd_len + obj_lan_msg_trlr_len;
  
  ERR_EINVAL (!(payload_len > IPMI_MAX_PAYLOAD_LENGTH));
  
  ERR_ENOSPC_CLEANUP (!(payload_len > payload_buf_len));
  
  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      FIID_OBJ_GET_ALL_LEN_CLEANUP(len, obj_lan_msg_hdr, payload_buf + indx, payload_buf_len - indx);
      indx += len;
    }

  FIID_OBJ_GET_ALL_LEN_CLEANUP(len, obj_cmd, payload_buf + indx, payload_buf_len - indx);
  indx += len;

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      FIID_OBJ_CREATE_CLEANUP(obj_lan_msg_trlr, tmpl_lan_msg_trlr);

      ERR_EXIT (!((checksum_start_offset = fiid_template_field_end_bytes (tmpl_lan_msg_hdr_rq, 
                                                                          "checksum1")) < 0));
      
      checksum = ipmi_checksum (payload_buf + checksum_start_offset, indx - checksum_start_offset);

      FIID_OBJ_SET_ALL_CLEANUP (obj_lan_msg_trlr, &checksum, sizeof(checksum));

      FIID_OBJ_GET_ALL_LEN_CLEANUP (len, obj_lan_msg_trlr, payload_buf + indx, payload_buf_len - indx);
      indx += len;
    }

  rv = indx;
 cleanup:
  if (rv < 0)
    memset(payload_buf, '\0', payload_buf_len);
  FIID_OBJ_DESTROY(obj_lan_msg_trlr);
  return (rv);
}

static int32_t
_construct_payload_confidentiality_none(uint8_t payload_type,
                                        fiid_obj_t obj_lan_msg_hdr,
                                        fiid_obj_t obj_cmd,
                                        fiid_obj_t obj_rmcpplus_payload)
{
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  int32_t payload_len;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !fiid_obj_valid(obj_lan_msg_hdr))
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1))
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1);

  FIID_OBJ_CLEAR (obj_rmcpplus_payload);

  ERR (!((payload_len = _construct_payload_buf(payload_type,
                                               obj_lan_msg_hdr, 
                                               obj_cmd, 
                                               payload_buf, 
                                               IPMI_MAX_PAYLOAD_LENGTH)) < 0));

  FIID_OBJ_SET_DATA (obj_rmcpplus_payload,
                     "payload_data",
                     payload_buf,
                     payload_len);

  return payload_len;
}

static int32_t
_construct_payload_confidentiality_aes_cbc_128(uint8_t payload_type,
                                               uint8_t payload_encrypted,
                                               fiid_obj_t obj_lan_msg_hdr,
                                               fiid_obj_t obj_cmd,
                                               uint8_t *confidentiality_key,
                                               uint32_t confidentiality_key_len,
                                               fiid_obj_t obj_rmcpplus_payload)
{
  uint8_t iv[IPMI_CRYPT_AES_CBC_128_IV_LENGTH];
  int32_t iv_len;
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_len;
  int32_t payload_len;
  int cipher_keylen, cipher_blocklen, encrypt_len;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && payload_encrypted
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !fiid_obj_valid(obj_lan_msg_hdr))
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1))
          && fiid_obj_packet_valid(obj_cmd) == 1
          && confidentiality_key
          && confidentiality_key_len
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1);
  
  ERR (!((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (!(cipher_keylen < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));
  ERR_EINVAL (!(confidentiality_key_len < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));
  confidentiality_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;

  ERR (!((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (cipher_blocklen == IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
     
  ERR (!((iv_len = ipmi_get_random(iv, IPMI_CRYPT_AES_CBC_128_IV_LENGTH)) < 0));
  ERR (!(iv_len != IPMI_CRYPT_AES_CBC_128_IV_LENGTH));
    
  ERR (!((payload_len = _construct_payload_buf(payload_type,
                                               obj_lan_msg_hdr, 
                                               obj_cmd, 
                                               payload_buf, 
                                               IPMI_MAX_PAYLOAD_LENGTH)) < 0));

  /* Pad the data appropriately */

  /* +1 is for the pad length field */
  pad_len = IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH - ((payload_len + 1) % IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
      
  ERR_ENOSPC (!((payload_len + pad_len + 1) > IPMI_MAX_PAYLOAD_LENGTH));
  
  if (pad_len)
    {
      int i;
      for (i = 0; i < pad_len; i++)
        payload_buf[payload_len + i] = i + 1;
      payload_buf[payload_len + pad_len] = pad_len;
    }

  /* +1 for pad length field */
  ERR (!((encrypt_len = ipmi_crypt_cipher_encrypt(IPMI_CRYPT_CIPHER_AES,
                                                  IPMI_CRYPT_CIPHER_MODE_CBC,
                                                  confidentiality_key,
                                                  confidentiality_key_len,
                                                  iv,
                                                  iv_len,
                                                  payload_buf,
                                                  payload_len + pad_len + 1)) < 0));
  ERR (!(encrypt_len != (payload_len + pad_len + 1)));

  FIID_OBJ_CLEAR (obj_rmcpplus_payload);

  FIID_OBJ_SET_DATA (obj_rmcpplus_payload,
                     "confidentiality_header",
                     iv,
                     IPMI_CRYPT_AES_CBC_128_IV_LENGTH);
      
  FIID_OBJ_SET_DATA (obj_rmcpplus_payload,
                     "payload_data",
                     payload_buf,
                     payload_len);
      
  FIID_OBJ_SET_DATA (obj_rmcpplus_payload,
                     "confidentiality_trailer",
                     payload_buf + payload_len,
                     pad_len + 1);
      
  return (iv_len + payload_len + pad_len + 1);
}

static int32_t
_construct_payload_rakp(uint8_t payload_type,
                        fiid_obj_t obj_cmd,
                        fiid_obj_t obj_rmcpplus_payload)
{
  uint8_t obj_cmd_buf[IPMI_MAX_PAYLOAD_LENGTH];
  int32_t obj_cmd_len = 0;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
               && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_request) != 1)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
               && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_1) != 1)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3
               && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_3) != 1)
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && fiid_obj_packet_valid(obj_cmd) == 1);

  FIID_OBJ_CLEAR (obj_rmcpplus_payload);

  FIID_OBJ_GET_ALL_LEN (obj_cmd_len, obj_cmd, obj_cmd_buf, IPMI_MAX_PAYLOAD_LENGTH);

  FIID_OBJ_SET_DATA (obj_rmcpplus_payload,
                     "payload_data",
                     obj_cmd_buf,
                     obj_cmd_len);
  
  return (obj_cmd_len);
}

static int32_t
_construct_payload(uint8_t payload_type,
                   uint8_t payload_encrypted,
                   uint8_t authentication_algorithm,
                   uint8_t confidentiality_algorithm,
                   fiid_obj_t obj_lan_msg_hdr,
                   fiid_obj_t obj_cmd,
                   uint8_t *confidentiality_key,
                   uint32_t confidentiality_key_len,
                   fiid_obj_t obj_rmcpplus_payload)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
          && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
          && (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
              || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !fiid_obj_valid(obj_lan_msg_hdr))
          && fiid_obj_valid(obj_cmd)
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq) == 1
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI
      || payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _construct_payload_confidentiality_none(payload_type,
                                                       obj_lan_msg_hdr,
                                                       obj_cmd,
                                                       obj_rmcpplus_payload);
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return _construct_payload_confidentiality_aes_cbc_128(payload_type,
                                                              payload_encrypted,
                                                              obj_lan_msg_hdr,
                                                              obj_cmd,
                                                              confidentiality_key,
                                                              confidentiality_key_len,
                                                              obj_rmcpplus_payload);
    }
  else
    return _construct_payload_rakp(payload_type,
                                   obj_cmd,
                                   obj_rmcpplus_payload);
}

static int8_t
_construct_session_trlr_pad(uint8_t integrity_algorithm,
                            uint32_t ipmi_msg_len,
                            fiid_obj_t obj_rmcpplus_session_trlr)
{
  int8_t pad_length_field_len, next_header_field_len, pad_length = 0;
  uint8_t pad_bytes[IPMI_INTEGRITY_PAD_MULTIPLE] = {IPMI_INTEGRITY_PAD_DATA,
                                                    IPMI_INTEGRITY_PAD_DATA,
                                                    IPMI_INTEGRITY_PAD_DATA,
                                                    IPMI_INTEGRITY_PAD_DATA};

  assert (IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
          && fiid_obj_valid(obj_rmcpplus_session_trlr)
          && fiid_obj_template_compare(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr));

  ERR_EXIT (!((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, 
                                                                     "pad_length")) < 0)); 
  ERR_EXIT (!((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, 
                                                                      "next_header")) < 0));
  
  ipmi_msg_len += pad_length_field_len;
  ipmi_msg_len += next_header_field_len;

  if (ipmi_msg_len % IPMI_INTEGRITY_PAD_MULTIPLE)
    pad_length = IPMI_INTEGRITY_PAD_MULTIPLE - (ipmi_msg_len % IPMI_INTEGRITY_PAD_MULTIPLE);
  
  FIID_OBJ_CLEAR_FIELD (obj_rmcpplus_session_trlr, "integrity_pad");

  if (pad_length)
    FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                       "integrity_pad",
                       pad_bytes,
                       pad_length);

  FIID_OBJ_SET (obj_rmcpplus_session_trlr,
		"pad_length",
		pad_length);
  
  return (0);
}

static int32_t
_calculate_authentication_code_len(uint8_t integrity_algorithm)
{
  int32_t authentication_code_len;

  assert ((integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128));

  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE) 
    authentication_code_len = 0;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
    authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
  else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
    authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
  else /* IPMI_INTEGRITY_ALGORITHM_MD5_128 */
    authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
  
  return (authentication_code_len);
}

static int32_t
_construct_session_trlr_authentication_code(uint8_t integrity_algorithm,
                                            uint8_t *integrity_key,
                                            uint32_t integrity_key_len,
                                            uint8_t *authentication_code_data,
                                            uint32_t authentication_code_data_len,
                                            fiid_obj_t obj_rmcpplus_session_trlr,
                                            uint8_t *pkt_data,
                                            uint32_t pkt_data_len,
                                            uint8_t *authentication_code_buf,
                                            uint32_t authentication_code_buf_len)
{
  int hash_algorithm, hash_flags, crypt_digest_len;
  unsigned int expected_digest_len, copy_digest_len, hash_data_len;
  uint8_t hash_data[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t integrity_digest[IPMI_MAX_INTEGRITY_DATA_LENGTH];
  int32_t len, authentication_code_len, integrity_digest_len;
  uint8_t pwbuf[IPMI_2_0_MAX_PASSWORD_LENGTH];
  int32_t rv = -1;

  assert ((integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
           || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
          && !(integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
               && authentication_code_data 
               && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
          && fiid_obj_valid(obj_rmcpplus_session_trlr)
          && pkt_data
          && pkt_data_len
          && authentication_code_buf
          && authentication_code_buf_len);

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  /* Check if the user provided an authentication code, if so, use it */

  memset(authentication_code_buf, '\0', authentication_code_buf_len);

  FIID_OBJ_FIELD_LEN_BYTES(len,
                           obj_rmcpplus_session_trlr,
                           "authentication_code");

  if (len)
    {
      FIID_OBJ_GET_DATA_LEN(len,
                            obj_rmcpplus_session_trlr,
                            "authentication_code",
                            authentication_code_buf,
                            authentication_code_buf_len);
      return (len);
    }

  ERR_CLEANUP (!((authentication_code_len = _calculate_authentication_code_len(integrity_algorithm)) < 0));

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
  else
    {
      hash_algorithm = IPMI_CRYPT_HASH_MD5;
      hash_flags = 0;
      expected_digest_len = MD5_DIGEST_LENGTH;
      copy_digest_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;
    }
      
  ERR_CLEANUP (!((crypt_digest_len = ipmi_crypt_hash_digest_len(hash_algorithm)) < 0));
      
  ERR_EXIT (crypt_digest_len == expected_digest_len);
      
  memset(hash_data, '\0', IPMI_MAX_PAYLOAD_LENGTH);
      
  hash_data_len = 0;
      
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      /* achu: Password must be zero padded */
      memset(pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      
      if (authentication_code_data && authentication_code_data_len)
        memcpy(pwbuf, authentication_code_data, authentication_code_data_len);

      memcpy(hash_data + hash_data_len, 
             pwbuf, 
	     IPMI_2_0_MAX_PASSWORD_LENGTH);
      secure_memset(pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }
  
  memcpy(hash_data + hash_data_len, pkt_data, pkt_data_len);
  hash_data_len += pkt_data_len;
  
  if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128)
    {
      /* achu: Password must be zero padded */
      memset(pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);

      if (authentication_code_data && authentication_code_data_len)
        memcpy(pwbuf, authentication_code_data, authentication_code_data_len);

      memcpy(hash_data + hash_data_len, 
             pwbuf, 
	     IPMI_2_0_MAX_PASSWORD_LENGTH);
      secure_memset(pwbuf, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH);
      hash_data_len += IPMI_2_0_MAX_PASSWORD_LENGTH;
    }
  
  ERR_CLEANUP (!((integrity_digest_len = ipmi_crypt_hash(hash_algorithm,
                                                         hash_flags,
                                                         integrity_key,
                                                         integrity_key_len,
                                                         hash_data,
                                                         hash_data_len,
                                                         integrity_digest,
                                                         IPMI_MAX_INTEGRITY_DATA_LENGTH)) < 0));
  
  ERR_CLEANUP (integrity_digest_len == crypt_digest_len);
  
  ERR_ENOSPC_CLEANUP (!(integrity_digest_len > authentication_code_buf_len));
  
  memcpy(authentication_code_buf, integrity_digest, copy_digest_len);
      
  rv = copy_digest_len;
 cleanup:
  secure_memset(integrity_digest, '\0', IPMI_MAX_INTEGRITY_DATA_LENGTH);
  return (rv);
}

int32_t
assemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm,
                            uint8_t integrity_algorithm,
                            uint8_t confidentiality_algorithm,
                            uint8_t *integrity_key,
                            uint32_t integrity_key_len,
                            uint8_t *confidentiality_key,
                            uint32_t confidentiality_key_len,
                            uint8_t *authentication_code_data,
                            uint32_t authentication_code_data_len,
                            fiid_obj_t obj_rmcp_hdr,
                            fiid_obj_t obj_rmcpplus_session_hdr,
                            fiid_obj_t obj_lan_msg_hdr,
                            fiid_obj_t obj_cmd,
                            fiid_obj_t obj_rmcpplus_session_trlr,
                            uint8_t *pkt,
                            uint32_t pkt_len)
{
  unsigned int indx = 0;
  int32_t obj_rmcp_hdr_len;
  uint64_t payload_type, payload_authenticated, payload_encrypted, session_id, session_sequence_number;
  int32_t payload_len;
  fiid_obj_t obj_rmcpplus_payload = NULL;
  fiid_obj_t obj_session_hdr_temp = NULL;
  fiid_obj_t obj_rmcpplus_session_trlr_temp = NULL;
  int32_t req_len, obj_len, len;
  int32_t oem_iana_len, oem_payload_id_len;
  int32_t rv = -1;

  /* achu: obj_lan_msg_hdr only needed for payload type IPMI 
   *
   * If integrity_algorithm != NONE, technically the key can be NULL
   */
  ERR_EINVAL (IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
  	      && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
              && !(integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128
                   && authentication_code_data 
                   && authentication_code_data_len > IPMI_2_0_MAX_PASSWORD_LENGTH)
              && (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
                  || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
              && !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
                   && !(confidentiality_key
                        && confidentiality_key_len))
	      && fiid_obj_valid(obj_rmcp_hdr)
	      && fiid_obj_valid(obj_rmcpplus_session_hdr)
	      && fiid_obj_valid(obj_cmd)
	      && pkt
	      && pkt_len);

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);

  FIID_OBJ_PACKET_VALID(obj_rmcp_hdr);
  FIID_OBJ_PACKET_VALID(obj_cmd);

  /*
   * Can't use FIID_OBJ_PACKET_VALID() on obj_rmcpplus_session_hdr b/c
   * a ipmi_payload_len is required but may not be set yet.
   */

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "authentication_type");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, "authentication_type");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "reserved1");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, "reserved1");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "payload_type");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, "payload_type");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "payload_type.authenticated");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, "payload_type.authenticated");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "payload_type.encrypted");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, "payload_type.encrypted");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "session_id");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, "session_id");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_hdr, "session_sequence_number");
  FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_hdr, "session_sequence_number");
  ERR_EINVAL (len == req_len);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr, 
                "payload_type",
                &payload_type);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.authenticated",
                &payload_authenticated);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.encrypted",
                &payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_id",
                &session_id);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_sequence_number",
                &session_sequence_number);

  /* Note: We don't check consider OPEN_SESSION_RESPONSE, RAKP2 or
   * RAKP4 payload types b/c they are responses, not requests.
   */
  ERR_EINVAL ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
               || payload_type == IPMI_PAYLOAD_TYPE_SOL
	       || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST
	       || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1
	       || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
              && !(IPMI_PAYLOAD_TYPE_SESSION_SETUP(payload_type)
                   && (payload_authenticated 
                       || payload_encrypted 
                       || session_id 
                       || session_sequence_number))
              && !(session_id
                   && payload_authenticated == IPMI_PAYLOAD_FLAG_UNAUTHENTICATED
                   && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_NONE)
	      && !(session_id 
		   && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED
		   && !(integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
                        || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
                        || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128))
	      && !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
		   && payload_encrypted != IPMI_PAYLOAD_FLAG_UNENCRYPTED));

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      ERR_EINVAL (fiid_obj_valid(obj_lan_msg_hdr));
      FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rq);
      FIID_OBJ_PACKET_VALID(obj_lan_msg_hdr);
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_SOL)
    ERR_EINVAL ((fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data) == 1
                 || fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data_remote_console_to_bmc) == 1));
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_REQUEST)
    FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_open_session_request);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_1)
    FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_rakp_message_1);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_3)
    FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_rakp_message_3);
  
  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      ERR_EINVAL (fiid_obj_valid(obj_rmcpplus_session_trlr));
      FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

      /*
       * Can't use FIID_OBJ_PACKET_VALID() on obj_rmcpplus_session_trlr b/c
       * integrity pad, pad length, and authentication code may not be set.
       */
      FIID_OBJ_FIELD_LEN (len, obj_rmcpplus_session_trlr, "next_header");
      FIID_TEMPLATE_FIELD_LEN(req_len, tmpl_rmcpplus_session_trlr, "next_header");
      ERR_EINVAL (len == req_len);
    }

  FIID_OBJ_FIELD_LEN(oem_iana_len, obj_rmcpplus_session_hdr, "oem_iana");
  FIID_OBJ_FIELD_LEN(oem_payload_id_len, obj_rmcpplus_session_hdr, "oem_payload_id");

  /* achu: The OEM payload type isn't supported, but I'll leave this
   * code for the sake of potential future support
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      int32_t oem_iana_req_len, oem_payload_id_req_len;

      FIID_TEMPLATE_FIELD_LEN(oem_iana_req_len, tmpl_rmcpplus_session_hdr, "oem_iana");
      FIID_TEMPLATE_FIELD_LEN(oem_payload_id_req_len, tmpl_rmcpplus_session_hdr, "oem_payload_id");
      
      ERR_EINVAL (oem_iana_len == oem_iana_req_len
		  && oem_payload_id_len == oem_payload_id_req_len);
    }
  else
    ERR_EINVAL (!oem_iana_len && !oem_payload_id_len);

  memset(pkt, '\0', pkt_len);

  /* 
   * Copy RMCP header into packet
   */
  indx = 0;
  FIID_OBJ_LEN_BYTES(obj_rmcp_hdr_len, obj_rmcp_hdr);
  ERR_ENOSPC(!(obj_rmcp_hdr_len > (pkt_len - indx)));
  FIID_OBJ_GET_ALL_LEN(obj_rmcp_hdr_len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += obj_rmcp_hdr_len;
  
  /* 
   * Copy Session Header into packet
   */
  FIID_OBJ_BLOCK_LEN_BYTES(len,
                           obj_rmcpplus_session_hdr,
                           "authentication_type",
                           "session_sequence_number");
  ERR_ENOSPC_CLEANUP(!(len > (pkt_len - indx)));
  FIID_OBJ_GET_BLOCK_LEN(len,
                         obj_rmcpplus_session_hdr,
                         "authentication_type",
                         "session_sequence_number",
                         pkt + indx,
                         pkt_len - indx);
  indx += len;

  /* 
   * Construct/Encrypt Payload and copy into packet
   */
  FIID_OBJ_CREATE (obj_rmcpplus_payload, tmpl_rmcpplus_payload);

  ERR (!((payload_len = _construct_payload(payload_type,
                                           payload_encrypted,
                                           authentication_algorithm,
                                           confidentiality_algorithm,
                                           obj_lan_msg_hdr,
                                           obj_cmd,
                                           confidentiality_key,
                                           confidentiality_key_len,
                                           obj_rmcpplus_payload)) < 0));
  
  /* 
   * Copy IPMI Payload Length into packet
   */

  FIID_OBJ_CREATE (obj_session_hdr_temp, tmpl_rmcpplus_session_hdr);  
  FIID_OBJ_CLEAR_CLEANUP (obj_session_hdr_temp);

  FIID_OBJ_SET_CLEANUP (obj_session_hdr_temp, 
                        "ipmi_payload_len",
                        payload_len);
  
  FIID_OBJ_LEN_BYTES_CLEANUP(obj_len, obj_session_hdr_temp);
  ERR_ENOSPC_CLEANUP(!(obj_len > (pkt_len - indx)));
  FIID_OBJ_GET_ALL_LEN_CLEANUP(obj_len, obj_session_hdr_temp, pkt + indx, pkt_len - indx);
  indx += obj_len;

  /* 
   * Copy IPMI Payload into packet
   */

  FIID_OBJ_LEN_BYTES_CLEANUP(obj_len, obj_rmcpplus_payload);
  ERR_ENOSPC_CLEANUP(!(obj_len > (pkt_len - indx)));
  FIID_OBJ_GET_ALL_LEN_CLEANUP(obj_len, obj_rmcpplus_payload, pkt + indx, pkt_len - indx);
  indx += obj_len;

  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      uint8_t authentication_code_buf[IPMI_MAX_PAYLOAD_LENGTH];
      int32_t authentication_code_len;

      FIID_OBJ_DUP_CLEANUP (obj_rmcpplus_session_trlr_temp, obj_rmcpplus_session_trlr);

      ERR_CLEANUP (!(_construct_session_trlr_pad(integrity_algorithm,
                                                 (indx - obj_rmcp_hdr_len),
                                                 obj_rmcpplus_session_trlr_temp) < 0));
      
      FIID_OBJ_BLOCK_LEN_BYTES_CLEANUP(len,
                                       obj_rmcpplus_session_trlr_temp,
                                       "integrity_pad",
                                       "next_header");
      ERR_ENOSPC_CLEANUP(!(len > (pkt_len - indx)));
      FIID_OBJ_GET_BLOCK_LEN_CLEANUP(len,
                                     obj_rmcpplus_session_trlr_temp,
                                     "integrity_pad",
                                     "next_header",
                                     pkt + indx,
                                     pkt_len - indx);
      indx += len;

      /* achu: Note that the integrity code is all data prior to the authentication_code, so this 
       * call must be done after the pad, pad length, and next header are copied into 
       * the pkt buffer.
       */
      ERR_CLEANUP (!((authentication_code_len = _construct_session_trlr_authentication_code(integrity_algorithm,
                                                                                            integrity_key,
                                                                                            integrity_key_len,
                                                                                            authentication_code_data,
                                                                                            authentication_code_data_len,
                                                                                            obj_rmcpplus_session_trlr_temp,
                                                                                            pkt + obj_rmcp_hdr_len,
                                                                                            indx - obj_rmcp_hdr_len,
                                                                                            authentication_code_buf,
                                                                                            IPMI_MAX_PAYLOAD_LENGTH)) < 0));
      
      if (authentication_code_len)
        {
          ERR_ENOSPC_CLEANUP(!(authentication_code_len > (pkt_len - indx)));
          memcpy(pkt + indx, authentication_code_buf, authentication_code_len);
          indx += authentication_code_len;
        }
    }

  rv = indx;
 cleanup:
  FIID_OBJ_DESTROY(obj_rmcpplus_payload);
  FIID_OBJ_DESTROY(obj_session_hdr_temp);
  FIID_OBJ_CLEAR_NO_RETURN(obj_rmcpplus_session_trlr_temp);
  FIID_OBJ_DESTROY(obj_rmcpplus_session_trlr_temp);
  return (rv);
}

static int32_t
_deconstruct_payload_buf(uint8_t payload_type,
                         fiid_obj_t obj_lan_msg_hdr,
                         fiid_obj_t obj_cmd,
                         fiid_obj_t obj_lan_msg_trlr,
                         uint8_t *pkt,
                         uint32_t lan_msg_len)
{
  int32_t obj_lan_msg_trlr_len, obj_cmd_len;
  int32_t len;
  unsigned int indx = 0;

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid(obj_lan_msg_hdr)
                    && fiid_obj_template_compare(obj_lan_msg_hdr, 
                                                 tmpl_lan_msg_hdr_rs) == 1
                    && fiid_obj_valid(obj_lan_msg_trlr)
                    && fiid_obj_template_compare(obj_lan_msg_trlr, 
                                                 tmpl_lan_msg_trlr) == 1))
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare(obj_cmd, 
                                              tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare(obj_cmd, 
                                                 tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt 
          && lan_msg_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      ERR_EXIT (!((obj_lan_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));
  
      FIID_OBJ_CLEAR (obj_lan_msg_hdr);
      FIID_OBJ_SET_ALL_LEN(len, 
                           obj_lan_msg_hdr,
                           pkt + indx,
                           lan_msg_len - indx);
      indx += len;

      if (indx >= lan_msg_len)
        return 0;
  
      /* achu: For payload_type == IPMI Whatever is in between the
       * header and the trailer is the command data
       */

      if ((lan_msg_len - indx) >= obj_lan_msg_trlr_len)
        obj_cmd_len = (lan_msg_len - indx) - obj_lan_msg_trlr_len;
      else
        obj_cmd_len = 0;
    }
  else /* payload_type == IPMI_PAYLOAD_TYPE_SOL */
    obj_cmd_len = lan_msg_len;

  if (obj_cmd_len)
    {
      FIID_OBJ_CLEAR (obj_cmd);
      FIID_OBJ_SET_ALL_LEN (len,
                            obj_cmd,
                            pkt + indx,
                            obj_cmd_len);
      indx += len;
      
      if (indx >= lan_msg_len)
        return 0;
    }

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      FIID_OBJ_CLEAR (obj_lan_msg_trlr);
      FIID_OBJ_SET_ALL_LEN (len,
                            obj_lan_msg_trlr,
                            pkt + indx,
                            lan_msg_len - indx);
    }
  
  return (0);
}

static int32_t
_deconstruct_payload_confidentiality_none(uint8_t payload_type,
                                          fiid_obj_t obj_rmcpplus_payload,
                                          fiid_obj_t obj_lan_msg_hdr,
                                          fiid_obj_t obj_cmd,
                                          fiid_obj_t obj_lan_msg_trlr,
                                          uint8_t *pkt,
                                          uint32_t ipmi_payload_len)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid(obj_lan_msg_hdr)
                    && fiid_obj_template_compare(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1
                    && fiid_obj_valid(obj_lan_msg_trlr)
                    && fiid_obj_template_compare(obj_lan_msg_trlr, tmpl_lan_msg_trlr) == 1))
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && pkt
          && ipmi_payload_len);

  /* achu: No encryption, so ipmi_payload_len is the length of 
   * the msg_hdr, cmd, and msg_trlr.
   */
  ERR (!(_deconstruct_payload_buf(payload_type,
                                  obj_lan_msg_hdr,
                                  obj_cmd,
                                  obj_lan_msg_trlr,
                                  pkt,
                                  ipmi_payload_len) < 0));

  FIID_OBJ_CLEAR(obj_rmcpplus_payload);
      
  FIID_OBJ_SET_DATA(obj_rmcpplus_payload,
                    "payload_data",
                    pkt,
                    ipmi_payload_len);

  return (0);
}

static int32_t
_deconstruct_payload_confidentiality_aes_cbc_128(uint8_t payload_type,
                                                 uint8_t payload_encrypted,
                                                 fiid_obj_t obj_rmcpplus_payload,
                                                 fiid_obj_t obj_lan_msg_hdr,
                                                 fiid_obj_t obj_cmd,
                                                 fiid_obj_t obj_lan_msg_trlr,
                                                 uint8_t *confidentiality_key,
                                                 uint32_t confidentiality_key_len,
                                                 uint8_t *pkt,
                                                 uint32_t ipmi_payload_len)
{
  uint8_t iv[IPMI_CRYPT_AES_CBC_128_IV_LENGTH];
  uint8_t payload_buf[IPMI_MAX_PAYLOAD_LENGTH];
  uint8_t pad_length;
  int cipher_keylen, cipher_blocklen;
  int32_t payload_data_len, decrypt_len, cmd_data_len, indx = 0;

  /* Note: Confidentiality Key for AES_CBS_128 is K2 */

  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL)
          && payload_encrypted == IPMI_PAYLOAD_FLAG_ENCRYPTED
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && !(payload_type == IPMI_PAYLOAD_TYPE_IPMI
               && !(fiid_obj_valid(obj_lan_msg_hdr)
                    && fiid_obj_template_compare(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs) == 1
                    && fiid_obj_valid(obj_lan_msg_trlr)
                    && fiid_obj_template_compare(obj_lan_msg_trlr, tmpl_lan_msg_trlr) == 1))
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_SOL
               && !(fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data) == 1
                    || fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1))
          && confidentiality_key
          && pkt
          && ipmi_payload_len);

  ERR (!((cipher_keylen = ipmi_crypt_cipher_key_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (!(cipher_keylen < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));
  ERR_EINVAL (!(confidentiality_key_len < IPMI_CRYPT_AES_CBC_128_KEY_LENGTH));
  confidentiality_key_len = IPMI_CRYPT_AES_CBC_128_KEY_LENGTH;

  ERR (!((cipher_blocklen = ipmi_crypt_cipher_block_len(IPMI_CRYPT_CIPHER_AES)) < 0));
  ERR_EXIT (cipher_blocklen == IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
  ERR_EINVAL (!(ipmi_payload_len < IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH));

  payload_data_len = ipmi_payload_len - IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;
  ERR_EINVAL (!(payload_data_len <= 0));

  memcpy(iv, pkt, IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);
  indx += IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH;
  memcpy(payload_buf, pkt + indx, payload_data_len);

  FIID_OBJ_CLEAR (obj_rmcpplus_payload);
  FIID_OBJ_SET_DATA(obj_rmcpplus_payload,
                    "confidentiality_header",
                    iv,
                    IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH);

  ERR (!((decrypt_len = ipmi_crypt_cipher_decrypt(IPMI_CRYPT_CIPHER_AES,
                                                  IPMI_CRYPT_CIPHER_MODE_CBC,
                                                  confidentiality_key,
                                                  confidentiality_key_len,
                                                  iv,
                                                  IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH,
                                                  payload_buf,
                                                  payload_data_len)) < 0));
  ERR (!((decrypt_len != payload_data_len)));

  pad_length = payload_buf[payload_data_len - 1];
  ERR_EINVAL (!(pad_length > IPMI_CRYPT_AES_CBC_128_BLOCK_LENGTH));

  cmd_data_len = payload_data_len - pad_length - 1;
  ERR_EINVAL (!(cmd_data_len <= 0));

  FIID_OBJ_SET_DATA(obj_rmcpplus_payload,
                    "payload_data",
                    payload_buf,
                    cmd_data_len);

  FIID_OBJ_SET_DATA(obj_rmcpplus_payload,
                    "confidentiality_trailer",
                    payload_buf + cmd_data_len,
                    pad_length + 1);
  
  /* achu: User is responsible for checking if padding is not corrupt  */
  
  ERR (!(_deconstruct_payload_buf(payload_type,
                                  obj_lan_msg_hdr, 
                                  obj_cmd, 
                                  obj_lan_msg_trlr,
                                  payload_buf,
                                  cmd_data_len) < 0));

  return (0);
}

static int32_t
_deconstruct_payload_rakp(uint8_t payload_type,
                          fiid_obj_t obj_rmcpplus_payload,
                          fiid_obj_t obj_cmd,
                          uint8_t *pkt,
                          uint32_t ipmi_payload_len)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && fiid_obj_valid(obj_cmd)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
               && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_open_session_response) != 1)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
               && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_2) != 1)
          && !(payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4
               && fiid_obj_template_compare(obj_cmd, tmpl_rmcpplus_rakp_message_4) != 1)
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && pkt
          && ipmi_payload_len);

  FIID_OBJ_CLEAR(obj_rmcpplus_payload);

  FIID_OBJ_SET_DATA(obj_rmcpplus_payload,
                    "payload_data",
                    pkt,
                    ipmi_payload_len);
  
  FIID_OBJ_CLEAR(obj_cmd);
  FIID_OBJ_SET_ALL(obj_cmd,
                   pkt,
                   ipmi_payload_len);

  return (0);
}

static int32_t
_deconstruct_payload(uint8_t payload_type,
                     uint8_t payload_encrypted,
                     uint8_t authentication_algorithm,
                     uint8_t confidentiality_algorithm,
                     fiid_obj_t obj_rmcpplus_payload,
                     fiid_obj_t obj_lan_msg_hdr,
                     fiid_obj_t obj_cmd,
                     fiid_obj_t obj_lan_msg_trlr,
                     uint8_t *confidentiality_key,
                     uint32_t confidentiality_key_len,
                     uint8_t *pkt,
                     uint32_t ipmi_payload_len)
{
  assert ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
           || payload_type == IPMI_PAYLOAD_TYPE_SOL
           || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
           || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
          && IPMI_PAYLOAD_ENCRYPTED_FLAG_VALID(payload_encrypted)
          && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
          && (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
              || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
          && fiid_obj_valid(obj_rmcpplus_payload)
          && fiid_obj_template_compare(obj_rmcpplus_payload, tmpl_rmcpplus_payload) == 1
          && fiid_obj_valid(obj_cmd)
          && pkt
          && ipmi_payload_len);

  /* Note: We don't check consider OPEN_SESSION_REQUEST, RAKP1 or
   * RAKP3 special b/c they are requests, not responses
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI
      || payload_type == IPMI_PAYLOAD_TYPE_SOL)
    {
      if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
        return _deconstruct_payload_confidentiality_none(payload_type,
                                                         obj_rmcpplus_payload,
                                                         obj_lan_msg_hdr,
                                                         obj_cmd,
                                                         obj_lan_msg_trlr,
                                                         pkt,
                                                         ipmi_payload_len);
      else /* IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 */
        return _deconstruct_payload_confidentiality_aes_cbc_128(payload_type,
                                                                payload_encrypted,
                                                                obj_rmcpplus_payload,
                                                                obj_lan_msg_hdr,
                                                                obj_cmd,
                                                                obj_lan_msg_trlr,
                                                                confidentiality_key,
                                                                confidentiality_key_len,
                                                                pkt,
                                                                ipmi_payload_len);
    }
  else
    return _deconstruct_payload_rakp(payload_type,
                                     obj_rmcpplus_payload,
                                     obj_cmd,
                                     pkt,
                                     ipmi_payload_len);
}

int32_t
unassemble_ipmi_rmcpplus_pkt (uint8_t authentication_algorithm,
                              uint8_t integrity_algorithm,
                              uint8_t confidentiality_algorithm,
                              uint8_t *integrity_key,
                              uint32_t integrity_key_len,
                              uint8_t *confidentiality_key,
                              uint32_t confidentiality_key_len,
                              uint8_t *pkt,
                              uint32_t pkt_len,
                              fiid_obj_t obj_rmcp_hdr,
                              fiid_obj_t obj_rmcpplus_session_hdr,
                              fiid_obj_t obj_rmcpplus_payload,
                              fiid_obj_t obj_lan_msg_hdr,
                              fiid_obj_t obj_cmd,
                              fiid_obj_t obj_lan_msg_trlr,
                              fiid_obj_t obj_rmcpplus_session_trlr)
{
  unsigned int indx = 0;
  int32_t obj_rmcp_hdr_len, obj_len;
  uint64_t payload_type, payload_authenticated, payload_encrypted, 
    session_id, session_sequence_number, ipmi_payload_len;

  /* achu: obj_lan_msg_hdr & trlr only needed for payload type IPMI 
   *
   * If integrity_algorithm != NONE, technically the key can be NULL
   */
  ERR_EINVAL (IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
	      && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
              && (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
                  || confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
              && !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128
                   && !(confidentiality_key
                        && confidentiality_key_len))
	      && pkt
	      && fiid_obj_valid(obj_rmcp_hdr)
	      && fiid_obj_valid(obj_rmcpplus_session_hdr)
	      && fiid_obj_valid(obj_rmcpplus_payload)
	      && fiid_obj_valid(obj_cmd)
	      && fiid_obj_valid(obj_rmcpplus_session_trlr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcp_hdr, tmpl_rmcp_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_hdr, tmpl_rmcpplus_session_hdr);
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_payload, tmpl_rmcpplus_payload);
  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);

  /*
   * Extract RMCP header
   */
  FIID_OBJ_CLEAR(obj_rmcp_hdr);
  FIID_OBJ_SET_ALL_LEN(obj_rmcp_hdr_len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += obj_rmcp_hdr_len;

  if (pkt_len <= indx)
    return 0;

  /*
   * Extract auth_type and payload information 
   */
  FIID_OBJ_CLEAR(obj_rmcpplus_session_hdr);
  FIID_OBJ_SET_BLOCK_LEN(obj_len, 
                         obj_rmcpplus_session_hdr, 
                         "authentication_type",
                         "payload_type.encrypted",
                         pkt + indx, 
                         pkt_len - indx);
  indx += obj_len;

  if (pkt_len <= indx)
    return 0;

  FIID_OBJ_GET (obj_rmcpplus_session_hdr, "payload_type", &payload_type);
  ERR_EINVAL ((payload_type == IPMI_PAYLOAD_TYPE_IPMI
               || payload_type == IPMI_PAYLOAD_TYPE_SOL
               || payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE
               || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2
               || payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4));
   
  /* 
   * Extract OEM IANA and OEM Payload ID
   */

  /* achu: The OEM payload type isn't supported, but I'll leave this
   * code for the sake of potential future support
   */
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      FIID_OBJ_SET_BLOCK_LEN(obj_len,
                             obj_rmcpplus_session_hdr,
                             "oem_iana",
                             "oem_payload_id",
                             pkt + indx,
                             pkt_len - indx);
      indx += obj_len;
      
      if (pkt_len <= indx)
        return 0;
    }

  /* 
   * Extract Session ID, Session Sequence Number, and Payload Length
   */
  FIID_OBJ_SET_BLOCK_LEN(obj_len,
                         obj_rmcpplus_session_hdr,
                         "session_id",
                         "ipmi_payload_len",
                         pkt + indx,
                         pkt_len - indx);
  indx += obj_len;
  
  if (pkt_len <= indx)
    return 0;

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.authenticated",
                &payload_authenticated);
  
  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "payload_type.encrypted",
                &payload_encrypted);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_id",
                &session_id);
  
  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "session_sequence_number",
                &session_sequence_number);

  FIID_OBJ_GET (obj_rmcpplus_session_hdr,
                "ipmi_payload_len",
                &ipmi_payload_len);

  ERR_EINVAL (!(IPMI_PAYLOAD_TYPE_SESSION_SETUP(payload_type)
		&& (payload_authenticated 
		    || payload_encrypted 
		    || session_id 
		    || session_sequence_number))
              && !(session_id
                   && payload_authenticated == IPMI_PAYLOAD_FLAG_UNAUTHENTICATED
                   && integrity_algorithm != IPMI_INTEGRITY_ALGORITHM_NONE)
              && !(session_id
                   && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED
                   && !(integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96
                        || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128
                        || integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128))
              && !(confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE
                   && payload_encrypted != IPMI_PAYLOAD_FLAG_UNENCRYPTED)
	      && ipmi_payload_len);

  if (payload_type == IPMI_PAYLOAD_TYPE_IPMI)
    {
      ERR_EINVAL (fiid_obj_valid(obj_lan_msg_hdr)
                  && fiid_obj_valid(obj_lan_msg_trlr));
      FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_hdr, tmpl_lan_msg_hdr_rs);
      FIID_OBJ_TEMPLATE_COMPARE(obj_lan_msg_trlr, tmpl_lan_msg_trlr);
    }
  else if (payload_type == IPMI_PAYLOAD_TYPE_SOL)
    ERR_EINVAL ((fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data) == 1
                 || fiid_obj_template_compare(obj_cmd, tmpl_sol_payload_data_bmc_to_remote_console) == 1));
  
  else if (payload_type == IPMI_PAYLOAD_TYPE_RMCPPLUS_OPEN_SESSION_RESPONSE)
    FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_open_session_response);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_2)
    FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_rakp_message_2);
  else if (payload_type == IPMI_PAYLOAD_TYPE_RAKP_MESSAGE_4)
    FIID_OBJ_TEMPLATE_COMPARE(obj_cmd, tmpl_rmcpplus_rakp_message_4);
  
  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      ERR_EINVAL (fiid_obj_valid(obj_rmcpplus_session_trlr));
      FIID_OBJ_TEMPLATE_COMPARE (obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);
    }

  if (pkt_len - indx < ipmi_payload_len)
    ipmi_payload_len = pkt_len - indx;
  
  /* 
   * Deconstruct/Decrypt Payload
   */
  ERR (!(_deconstruct_payload(payload_type,
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
                              ipmi_payload_len) < 0));
  indx += ipmi_payload_len;

  if (pkt_len <= indx)
    return 0;
  
  FIID_OBJ_CLEAR (obj_rmcpplus_session_trlr);

  if (session_id && payload_authenticated == IPMI_PAYLOAD_FLAG_AUTHENTICATED)
    {
      int32_t pad_length_field_len, next_header_field_len;
      uint32_t authentication_code_len;
      uint64_t pad_length;

      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE)
        authentication_code_len = 0;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96)
        authentication_code_len = IPMI_HMAC_SHA1_96_AUTHENTICATION_CODE_LENGTH;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
        authentication_code_len = IPMI_HMAC_MD5_128_AUTHENTICATION_CODE_LENGTH;
      else /* IPMI_INTEGRITY_ALGORITHM_MD5_128 */
        authentication_code_len = IPMI_MD5_128_AUTHENTICATION_CODE_LENGTH;

      ERR_EXIT (!((pad_length_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "pad_length")) < 0)); 
      ERR_EXIT (!((next_header_field_len = fiid_template_field_len_bytes (tmpl_rmcpplus_session_trlr, "next_header")) < 0));
      
      /* achu: There needs to be atleast the next_header and pad_length fields */
      if ((pkt_len - indx) < (authentication_code_len + pad_length_field_len + next_header_field_len))
        return 0;

      if (authentication_code_len)
        FIID_OBJ_SET_DATA(obj_rmcpplus_session_trlr,
                          "authentication_code",
                          pkt + indx + ((pkt_len - indx) - authentication_code_len),
                          authentication_code_len);

      FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                         "next_header",
                         pkt + indx + ((pkt_len - indx) - authentication_code_len - next_header_field_len),
                         next_header_field_len);
      
      FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                         "pad_length",
                         pkt + indx + ((pkt_len - indx) - authentication_code_len - next_header_field_len - pad_length_field_len),
                         pad_length_field_len);
      
      FIID_OBJ_GET (obj_rmcpplus_session_trlr,
                    "pad_length",
                    &pad_length);

      ERR_EINVAL (!(pad_length > IPMI_INTEGRITY_PAD_MULTIPLE));
      
      if (pad_length >= (pkt_len - indx - authentication_code_len - pad_length_field_len - next_header_field_len))
        pad_length = (pkt_len - indx - authentication_code_len - pad_length_field_len - next_header_field_len);
      
      FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
                         "integrity_pad",
                         pkt + indx,
                         pad_length);
    }

  return (0);
}
