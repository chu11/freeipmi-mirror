#ifndef _IPMI_WRAPPER_SEL_H
#define _IPMI_WRAPPER_SEL_H

struct sel_record
{
  u_int16_t record_id;
  time_t timestamp;
  char *sensor_desc;
  char *event_desc;
  char *generator_id;
};

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
