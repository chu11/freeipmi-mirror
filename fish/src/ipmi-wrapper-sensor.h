#ifndef _IPMI_WRAPPER_SENSOR_H
#define _IPMI_WRAPPER_SENSOR_H

sdr_repo_cache_t *get_sdr_repo_cache ();

void clear_sensors_errno ();
int get_sensors_errno ();
void set_sensors_errno (int eno);

void display_current_threshold_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache);
void display_current_generic_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache);
void display_current_generic_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache);
void display_current_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache);
void display_current_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache);
void display_current_oem_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache);
void display_current_oem_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache);
u_int8_t display_current_sensor (sdr_repo_cache_t *sdr_repo_cache);

int get_sdr_total_records (sdr_repo_cache_t *sdr_repo_cache);

void display_verbose_current_threshold_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache, int is_very_verbose);
void display_verbose_current_generic_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache);
void display_verbose_current_generic_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache);
void display_verbose_current_discrete_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache);
void display_verbose_current_discrete_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache);
void display_verbose_current_oem_sensor_full_record (sdr_repo_cache_t *sdr_repo_cache);
void display_verbose_current_oem_sensor_compact_record (sdr_repo_cache_t *sdr_repo_cache);
u_int8_t display_verbose_current_sensor (sdr_repo_cache_t *sdr_repo_cache);

void display_very_verbose_entity_association_record (sdr_repo_cache_t *sdr_repo_cache);
void display_very_verbose_fru_dev_locator_record (sdr_repo_cache_t *sdr_repo_cache);
void display_very_verbose_mgmt_control_dev_locator_record (sdr_repo_cache_t *sdr_repo_cache);
void display_very_verbose_oem_record (sdr_repo_cache_t *sdr_repo_cache);
u_int8_t display_very_verbose_current_sensor (sdr_repo_cache_t *sdr_repo_cache);


#endif
