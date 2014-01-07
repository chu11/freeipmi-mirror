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

#ifndef IPMI_LAN_SESSION_COMMON_H
#define IPMI_LAN_SESSION_COMMON_H

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_INTERNAL_WORKAROUND_FLAGS_GET_SESSION_CHALLENGE         0x00000001
#define IPMI_INTERNAL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE     0x00000002
#define IPMI_INTERNAL_WORKAROUND_FLAGS_CLOSE_SESSION_SKIP_RETRANSMIT 0x00000004

void api_lan_cmd_get_session_parameters (ipmi_ctx_t ctx,
					 uint8_t *authentication_type,
					 unsigned int *internal_workaround_flags);

void api_lan_2_0_cmd_get_session_parameters (ipmi_ctx_t ctx,
					     uint8_t *payload_authenticated,
					     uint8_t *payload_encrypted);

int api_lan_cmd_wrapper (ipmi_ctx_t ctx,
			 unsigned int internal_workaround_flags,
			 uint8_t lun,
			 uint8_t net_fn,
			 uint8_t authentication_type,
			 int check_authentication_code,
			 uint32_t *session_sequence_number,
			 uint32_t session_id,
			 uint8_t *rq_seq,
			 const char *password,
			 unsigned int password_len,
			 fiid_obj_t obj_cmd_rq,
			 fiid_obj_t obj_cmd_rs);

int api_lan_cmd_wrapper_ipmb (ipmi_ctx_t ctx,
			      fiid_obj_t obj_cmd_rq,
			      fiid_obj_t obj_cmd_rs);

int api_lan_open_session (ipmi_ctx_t ctx);

int api_lan_close_session (ipmi_ctx_t ctx);

int api_lan_2_0_cmd_wrapper (ipmi_ctx_t ctx,
			     unsigned int internal_workaround_flags,
			     uint8_t lun,
			     uint8_t net_fn,
			     uint8_t payload_type,
			     uint8_t payload_authenticated,
			     uint8_t payload_encrypted,
			     uint8_t *message_tag,
			     uint32_t *session_sequence_number,
			     uint32_t session_id,
			     uint8_t *rq_seq,
			     uint8_t authentication_algorithm,
			     uint8_t integrity_algorithm,
			     uint8_t confidentiality_algorithm,
			     const void *integrity_key,
			     unsigned int integrity_key_len,
			     const void *confidentiality_key,
			     unsigned int confidentiality_key_len,
			     const char *password,
			     unsigned int password_len,
			     fiid_obj_t obj_cmd_rq,
			     fiid_obj_t obj_cmd_rs);

int api_lan_2_0_cmd_wrapper_ipmb (ipmi_ctx_t ctx,
				  fiid_obj_t obj_cmd_rq,
				  fiid_obj_t obj_cmd_rs);

int api_lan_2_0_open_session (ipmi_ctx_t ctx);

int api_lan_2_0_close_session (ipmi_ctx_t ctx);

#endif /* IPMI_LAN_SESSION_COMMON_H */
