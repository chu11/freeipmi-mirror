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

#include "freeipmi/ipmi-rmcpplus.h"
#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-messaging-support-cmds.h" /* XXX  - only for IPMI_MAX_USER_NAME_LENGTH */
#include "freeipmi/ipmi-privilege-level-spec.h"
#include "freeipmi/ipmi-rmcpplus-status-spec.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

fiid_template_t tmpl_rmcpplus_session_hdr = 
  {
    {4,   "authentication_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,   "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6,   "payload_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,   "payload_type.authenticated", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1,   "payload_type.encrypted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {32,  "oem_iana", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
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

fiid_template_t tmpl_rmcpplus_open_session_rq = 
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

fiid_template_t tmpl_rmcpplus_open_session_rs = 
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
    {128, "user_name", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
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
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "reserved", 0);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type", payload_type);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type.authenticated", payload_authenticated);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "payload_type.encrypted", payload_encrypted);
  if (payload_type == IPMI_PAYLOAD_TYPE_OEM_EXPLICIT)
    {
      FIID_OBJ_SET (obj_rmcpplus_session_hdr, "oem_iana", oem_iana);
      FIID_OBJ_SET (obj_rmcpplus_session_hdr, "oem_payload_id", oem_payload_id);
    }
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "session_id", session_id);
  FIID_OBJ_SET (obj_rmcpplus_session_hdr, "session_sequence_number", session_sequence_number);

  /* ipmi_payload_len will be calculated during packet assembly */

  return (0);
}

int8_t
fill_rmcpplus_session_trlr(uint8_t *authentication_code_data,
                           uint32_t authentication_code_data_len,
                           fiid_obj_t obj_rmcpplus_session_trlr)
{
  ERR_EINVAL (!(authentication_code_data && authentication_code_data_len > IPMI_MAX_RMCPPLUS_AUTHENTICATION_CODE_LENGTH)
	      && fiid_obj_valid(obj_rmcpplus_session_trlr));

  FIID_OBJ_TEMPLATE_COMPARE(obj_rmcpplus_session_trlr, tmpl_rmcpplus_session_trlr);
  
  FIID_OBJ_CLEAR (obj_rmcpplus_session_trlr);

  /* Computing hashes and checking for correct input is done during
   * the packet assembly.  Padding calculations will also be done
   * during packet assembly.
   */

  FIID_OBJ_SET (obj_rmcpplus_session_trlr, "next_header", IPMI_NEXT_HEADER);

  if (authentication_code_data && authentication_code_data_len > 0)
    FIID_OBJ_SET_DATA (obj_rmcpplus_session_trlr,
		       "authentication_code",
		       authentication_code_data,
		       authentication_code_data_len);

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
  ERR_EINVAL (IPMI_PRIVILEGE_LEVEL_VALID(requested_maximum_privilege_level)
	      && IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
	      && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
	      && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
	      && fiid_obj_valid(obj_cmd_rq));
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rq, tmpl_rmcpplus_open_session_rq);

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
                             uint8_t *user_name,
                             uint32_t user_name_len,
                             fiid_obj_t obj_cmd_rq)
{
  uint8_t buf[IPMI_MAX_USER_NAME_LENGTH];

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

  /* achu: user_name must be zero extended */
  memset(buf, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (user_name)
    strncpy((char *)buf, (char *)user_name, IPMI_MAX_USER_NAME_LENGTH);
  
  FIID_OBJ_SET_DATA (obj_cmd_rq,
		     "user_name",
		     buf,
                     IPMI_MAX_USER_NAME_LENGTH);

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
