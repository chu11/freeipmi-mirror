#ifndef _IPMI_SEL_WRAPPER_H
#define _IPMI_SEL_WRAPPER_H

#define SEL_RECORD_SIZE 16

#include "ipmi-sel.h"

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

struct sel_record
{
  uint16_t record_id;
  char *timestamp;
  char *sensor_info;
  char *event_message;
  char *event_data2_message;
  char *event_data3_message;
};
typedef struct sel_record sel_record_t;

int get_sel_info (ipmi_sel_state_data_t *state_data,
                  local_sel_info_t *sel_info);
int get_sel_record (ipmi_sel_state_data_t *state_data,
                    uint16_t record_id, 
                    sel_record_t *sel_rec, 
                    uint16_t *next_record_id);
int get_sel_record_raw (ipmi_sel_state_data_t *state_data,
                        uint16_t record_id, 
                        uint8_t *record_data, 
                        uint32_t record_data_len, 
                        uint16_t *next_record_id);
int delete_sel_entry (ipmi_sel_state_data_t *state_data,
                      uint16_t record_id);
int clear_sel_entries (ipmi_sel_state_data_t *state_data);
int get_sel_clear_status (ipmi_sel_state_data_t *state_data,
                          int *status);

#endif
