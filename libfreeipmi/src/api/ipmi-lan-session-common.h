#ifndef _IPMI_LAN_SESSION_UTIL_H
#define	_IPMI_LAN_SESSION_UTIL_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_LAN_INTERNAL_WORKAROUND_FLAGS_GET_SESSION_CHALLENGE     0x00000001
#define IPMI_LAN_INTERNAL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE 0x00000002

void ipmi_lan_cmd_get_session_parameters (ipmi_ctx_t ctx,
					  uint8_t *authentication_type,
					  uint32_t *internal_workaround_flags);

void ipmi_lan_2_0_cmd_get_session_parameters (ipmi_ctx_t ctx,
					      uint8_t *payload_authenticated,
					      uint8_t *payload_encrypted);

int8_t ipmi_lan_cmd_wrapper (ipmi_ctx_t ctx,
                             uint32_t internal_workaround_flags,
                             uint8_t lun,
                             uint8_t net_fn,
                             uint8_t authentication_type,
                             uint32_t *session_sequence_number,
                             uint32_t session_id,
                             uint8_t *rq_seq,
                             char *password,
                             uint32_t password_len,
                             fiid_obj_t obj_cmd_rq,
                             fiid_obj_t obj_cmd_rs);

int8_t ipmi_lan_cmd_wrapper_ipmb (ipmi_ctx_t ctx,
				  fiid_obj_t obj_cmd_rq,
				  fiid_obj_t obj_cmd_rs);

int8_t ipmi_lan_open_session (ipmi_ctx_t ctx);

int8_t ipmi_lan_close_session (ipmi_ctx_t ctx);

int8_t ipmi_lan_2_0_cmd_wrapper (ipmi_ctx_t ctx,
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
                                 uint8_t *integrity_key,
                                 uint32_t integrity_key_len,
                                 uint8_t *confidentiality_key,
                                 uint32_t confidentiality_key_len,
                                 char *password,
                                 uint32_t password_len,
                                 fiid_obj_t obj_cmd_rq,
                                 fiid_obj_t obj_cmd_rs);

int8_t ipmi_lan_2_0_cmd_wrapper_ipmb (ipmi_ctx_t ctx,
				      fiid_obj_t obj_cmd_rq,
				      fiid_obj_t obj_cmd_rs);

int8_t ipmi_lan_2_0_open_session (ipmi_ctx_t ctx);

int8_t ipmi_lan_2_0_close_session (ipmi_ctx_t ctx);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-lan-interface.h */


