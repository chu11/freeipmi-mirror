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

int get_pef_info (ipmi_device_t dev, pef_info_t *pef_info);

#endif
