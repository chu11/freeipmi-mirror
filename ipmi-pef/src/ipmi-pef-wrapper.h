#ifndef _IPMI_PEF_WRAPPER_H
#define _IPMI_PEF_WRAPPER_H

#define SET_SELECTOR      0x0
#define BLOCK_SELECTOR    0x0

struct pef_info 
{
  int pef_version_major;
  int pef_version_minor;
  int alert_action_support;
  int power_down_action_support;
  int reset_action_support;
  int power_cycle_action_support;
  int oem_action_support;
  int diagnostic_interrupt_action_support;
  int oem_event_record_filtering_support;
  int eft_entries_count;
  int num_event_filters;
  int num_alert_policies;
  int num_alert_strings;
};
typedef struct pef_info pef_info_t;


struct pef_event_filter_table 
{
  int filter_number;
  int filter_type;
  int enable_filter;
  int event_filter_action_alert;
  int event_filter_action_power_off;
  int event_filter_action_reset;
  int event_filter_action_power_cycle;
  int event_filter_action_oem;
  int event_filter_action_diagnostic_interrupt;
  int event_filter_action_group_control_operation;
  int alert_policy_number;
  int group_control_selector;
  int event_severity;
  int generator_id_byte1;
  int generator_id_byte2;
  int sensor_type;
  int sensor_number;
  int event_trigger;
  int event_data1_offset_mask;
  int event_data1_AND_mask;
  int event_data1_compare1;
  int event_data1_compare2;
  int event_data2_AND_mask;
  int event_data2_compare1;
  int event_data2_compare2;
  int event_data3_AND_mask;
  int event_data3_compare1;
  int event_data3_compare2;
};
typedef struct pef_event_filter_table pef_event_filter_table_t;

int get_pef_info (ipmi_device_t dev, pef_info_t *pef_info);
int get_event_filter_table (ipmi_device_t dev, int filter, pef_event_filter_table_t *evt);
int set_event_filter_table (ipmi_device_t dev, pef_event_filter_table_t *evt);
int get_number_of_event_filters (ipmi_device_t dev, int *num_event_filters);
int get_evt_list (FILE *fp, pef_event_filter_table_t **evt_list, int *count);

#endif
