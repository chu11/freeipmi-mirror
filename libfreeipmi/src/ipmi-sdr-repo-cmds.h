#ifndef _IPMI_SDR_REPO_CMDS_H
#define _IPMI_SDR_REPO_CMDS_H

#define IPMI_SDR_MODAL_NON_MODAL_REPO_UPDATE_OP_UNSPECIFIED    0x0
#define IPMI_SDR_NON_MODAL_REPO_UPDATE_OP_SUPPORTED            0x1
#define IPMI_SDR_MODAL_REPO_UPDATE_OP_SUPPORTED                0x2
#define IPMI_SDR_MODAL_NON_MODAL_REPO_UPDATE_OP_SUPPORTED      0x3

#define IPMI_SDR_IPMB_SLAVE_ADDRESS    0x0
#define IPMI_SDR_SYSTEM_SOFTWARE_ID    0x1



#ifdef __cplusplus
extern "C" {
#endif


extern fiid_template_t tmpl_get_sdr_repo_info_rq;
extern fiid_template_t tmpl_get_sdr_repo_info_rs;

extern fiid_template_t tmpl_get_sdr_repo_alloc_info_rq;
extern fiid_template_t tmpl_get_sdr_repo_alloc_info_rs;

extern fiid_template_t tmpl_reserve_sdr_repo_rq;
extern fiid_template_t tmpl_reserve_sdr_repo_rs;

extern fiid_template_t tmpl_get_sdr_rq;
extern fiid_template_t tmpl_get_sdr_rs;

extern fiid_template_t tmpl_sdr_sensor_record_header;


int8_t ipmi_kcs_get_repo_info (u_int16_t sms_io_base, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_get_repo_alloc_info (u_int16_t sms_io_base, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_reserve_repo (u_int16_t sms_io_base, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_get_sensor_record_header (u_int16_t sms_io_base, 
					  u_int16_t record_id, 
					  fiid_obj_t obj_data_rs, 
					  u_int8_t *sensor_record_header);
int8_t ipmi_kcs_get_sdr_chunk (u_int16_t sms_io_base, 
			       u_int16_t reservation_id, 
			       u_int16_t record_id, 
			       u_int8_t record_offset, 
			       u_int8_t bytes_read, 
			       fiid_obj_t obj_data_rs, 
			       u_int8_t *sensor_record_chunk);
int8_t ipmi_kcs_get_sdr (u_int16_t sms_io_base, 
			 u_int16_t record_id, 
			 u_int8_t record_length, 
			 u_int8_t *sensor_record, 
			 u_int8_t *comp_code);

#ifdef __cplusplus
}
#endif

#endif
