#ifndef _IPMI_SEL_WRAPPER_H
#define _IPMI_SEL_WRAPPER_H

struct local_sel_info 
{
  int sel_version_major;
  int sel_version_minor;
  int log_entry_count;
  int free_space;
  int recent_addition_timestamp;
  int recent_erase_timestamp;
  int get_sel_alloc_info_cmd_support;
  int reserve_sel_cmd_support;
  int partial_add_sel_entry_cmd_support;
  int delete_sel_cmd_support;
  int overflow_flag;
};

typedef struct local_sel_info local_sel_info_t;

int get_local_sel_info (ipmi_device_t *dev, local_sel_info_t *sel_info);
int get_local_sel_record (ipmi_device_t *dev, 
			  uint16_t record_id, 
			  sel_record_t *sel_rec, 
			  uint16_t *next_record_id);
int get_local_sel_record_raw (ipmi_device_t *dev, 
			      uint16_t record_id, 
			      uint8_t *record_data, 
			      uint32_t record_data_len, 
			      uint16_t *next_record_id);

#endif
