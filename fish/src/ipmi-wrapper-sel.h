#ifndef _IPMI_WRAPPER_SEL_H
#define _IPMI_WRAPPER_SEL_H

#define SEL_RECORD_SIZE 16

struct sel_record
{
  u_int16_t record_id;
  time_t timestamp;
  char *sensor_desc;
  char *event_desc;
  char *generator_id;
};

enum sel_info_flag
  {
    get_sel_alloc_info_cmd_support = 1,
    reserve_sel_cmd_support = 2,
    partial_add_sel_entry_cmd_support = 4,
    delete_sel_cmd_support = 8,
    overflow_flag = 16,
  };
typedef enum sel_info_flag sel_info_flag_t;

struct sel_info
{
  unsigned long version_major;
  unsigned long version_minor;
  unsigned long entry_count;
  unsigned long free_space;
  unsigned long last_add_time;
  unsigned long last_erase_time;
  unsigned long flags;
};
typedef struct sel_info sel_info_t;

int get_sel_info (sel_info_t* pinfo);
sel_descriptor_t *get_seld ();
int display_sel_threshold_system_event_record (u_int8_t *record_data);
int display_sel_generic_discrete_system_event_record (u_int8_t *record_data);
int display_sel_discrete_system_event_record (u_int8_t *record_data);
int display_sel_oem_system_event_record (u_int8_t *record_data);
int display_sel_timestamped_oem_record (u_int8_t *record_data);
int display_sel_non_timestamped_oem_record (u_int8_t *record_data);
int display_sel_record (u_int8_t *record_data);

int get_sel_threshold_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec);
int get_sel_generic_discrete_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec);
int get_sel_discrete_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec);
int get_sel_oem_system_event_record (u_int8_t *record_data, struct sel_record *sel_rec);
int get_sel_timestamped_oem_record (u_int8_t *record_data, struct sel_record *sel_rec);
int get_sel_non_timestamped_oem_record (u_int8_t *record_data, struct sel_record *sel_rec);
int get_sel_record (u_int8_t *record_data, struct sel_record *sel_rec);

#endif 
