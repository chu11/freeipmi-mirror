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

struct pef_alert_policy_table
{
  int alert_policy_number;
  int policy_type;
  int policy_enabled;
  int policy_number;
  int destination_selector;
  int channel_number;
  int alert_string_set_selector;
  int event_specific_alert_string_lookup;
};
typedef struct pef_alert_policy_table pef_alert_policy_table_t;

struct lan_alert_destination
{
  int destination_selector;
  int destination_type;
  int alert_acknowledge;
  int alert_acknowledge_timeout;
  int alert_retries;
  int gateway_selector;
  char alert_ip_address[16];
  char alert_mac_address[18];
};
typedef struct lan_alert_destination lan_alert_destination_t;

int get_number_of_lan_destinations (struct ipmi_pef_state_data *state_data, 
				    int *number_of_lan_destinations);

int get_number_of_alert_policy_entries (struct ipmi_pef_state_data *state_data, 
					int *num_alert_policy_entries);

int get_number_of_event_filters (struct ipmi_pef_state_data *state_data, 
                                 int *num_event_filters);

int get_pef_info (struct ipmi_pef_state_data *state_data, 
		  pef_info_t *pef_info);

int get_bmc_community_string (struct ipmi_pef_state_data *state_data,
                              uint8_t *community_string, 
                              uint32_t community_string_len);

int get_community_string (struct ipmi_pef_state_data *state_data,
                          FILE *fp,
                          uint8_t *community_string, 
                          uint32_t community_string_len);

int set_bmc_community_string (struct ipmi_pef_state_data *state_data, 
			      uint8_t *community_string) ;

int get_alert_policy_table (struct ipmi_pef_state_data *state_data, 
			    int policy_number, 
			    pef_alert_policy_table_t *apt);

int get_alert_policy_table_list (FILE *fp, pef_alert_policy_table_t **apt_list, int *count);

int set_alert_policy_table (struct ipmi_pef_state_data *state_data, pef_alert_policy_table_t *apt);


int get_event_filter_table (struct ipmi_pef_state_data *state_data, 
			    int filter, 
			    pef_event_filter_table_t *eft);

int get_event_filter_table_list (FILE *fp, pef_event_filter_table_t **eft_list, int *count);

int set_event_filter_table (struct ipmi_pef_state_data *state_data, pef_event_filter_table_t *eft);

int get_lan_alert_destination (struct ipmi_pef_state_data *state_data, 
			       int destination_selector, 
			       lan_alert_destination_t *lad);

int get_lan_alert_destination_list (FILE *fp, lan_alert_destination_t **lad_list, int *count);

int set_lan_alert_destination (struct ipmi_pef_state_data *state_data, 
			       lan_alert_destination_t *lad);

#endif
