#ifndef _IPMI_SENSOR_CMDS_H
#define _IPMI_SENSOR_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif


#define IPMI_SNSR_GET_SENSOR_COUNT  0x00
#define IPMI_SNSR_GET_SDR_COUNT     0x01

/* #define IPMI_SDR_LUN_0_HAS_SENSORS  0x00 */
/* #define IPMI_SDR_LUN_1_HAS_SENSORS  0x01 */
/* #define IPMI_SDR_LUN_2_HAS_SENSORS  0x02 */
/* #define IPMI_SDR_LUN_3_HAS_SENSORS  0x03 */

#define SET_LOWER_NON_CRITICAL_THRESHOLD       1
#define SET_LOWER_CRITICAL_THRESHOLD           2 
#define SET_LOWER_NON_RECOVERABLE_THRESHOLD    4
#define SET_UPPER_NON_CRITICAL_THRESHOLD       8
#define SET_UPPER_CRITICAL_THRESHOLD           32
#define SET_UPPER_NON_RECOVERABLE_THRESHOLD    64


/* Intel - Not Implemented */
extern fiid_template_t tmpl_get_dev_sdr_info_rq;
extern fiid_template_t tmpl_get_dev_sdr_info_rs;
  
/* Intel - Not Implemented */
extern fiid_template_t tmpl_get_dev_sdr_rq;
extern fiid_template_t tmpl_get_dev_sdr_rs;

/* Intel - Not Implemented */
extern fiid_template_t tmpl_get_resrve_dev_sdr_repository_rq;
extern fiid_template_t tmpl_get_resrve_dev_sdr_repository_rs;

/* Intel - Not Implemented */
extern fiid_template_t tmpl_get_sensor_reading_factors_rq;
extern fiid_template_t tmpl_get_sensor_reading_factors_rs;

extern fiid_template_t tmpl_set_sensor_hysteresis_rq;
extern fiid_template_t tmpl_set_sensor_hysteresis_rs;

extern fiid_template_t tmpl_get_sensor_hysteresis_rq;
extern fiid_template_t tmpl_get_sensor_hysteresis_rs;

extern fiid_template_t tmpl_set_sensor_thresholds_rq;
extern fiid_template_t tmpl_set_sensor_thresholds_rs;

extern fiid_template_t tmpl_get_sensor_thresholds_rq;
extern fiid_template_t tmpl_get_sensor_thresholds_rs;

extern fiid_template_t tmpl_set_sensor_threshold_event_enable_rq;
extern fiid_template_t tmpl_set_sensor_threshold_event_enable_rs;
extern fiid_template_t tmpl_set_sensor_discrete_event_enable_rq;
extern fiid_template_t tmpl_set_sensor_discrete_event_enable_rs;

extern fiid_template_t tmpl_get_sensor_threshold_event_enable_rq;
extern fiid_template_t tmpl_get_sensor_threshold_event_enable_rs;
extern fiid_template_t tmpl_get_sensor_discrete_event_enable_rq;
extern fiid_template_t tmpl_get_sensor_discrete_event_enable_rs;

extern fiid_template_t tmpl_re_arm_sensor_threshold_events_rq;
extern fiid_template_t tmpl_re_arm_sensor_threshold_events_rs;
extern fiid_template_t tmpl_re_arm_sensor_discrete_events_rq;
extern fiid_template_t tmpl_re_arm_sensor_discrete_events_rs;

extern fiid_template_t tmpl_get_sensor_threshold_event_status_rq;
extern fiid_template_t tmpl_get_sensor_threshold_event_status_rs;
extern fiid_template_t tmpl_get_sensor_discrete_event_status_rq;
extern fiid_template_t tmpl_get_sensor_discrete_event_status_rs;

extern fiid_template_t tmpl_get_sensor_threshold_reading_rq;
extern fiid_template_t tmpl_get_sensor_threshold_reading_rs;
extern fiid_template_t tmpl_get_sensor_discrete_reading_rq;
extern fiid_template_t tmpl_get_sensor_discrete_reading_rs;

/* Intel - Not Implemented */
extern fiid_template_t tmpl_set_sensor_type_rq;
extern fiid_template_t tmpl_set_sensor_type_rs;

/* Intel - Not Implemented */
extern fiid_template_t tmpl_get_sensor_type_rq;
extern fiid_template_t tmpl_get_sensor_type_rs;


int8_t ipmi_kcs_get_threshold_reading (u_int8_t sensor_number, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_get_discrete_reading (u_int8_t sensor_number, fiid_obj_t obj_data_rs);
int8_t ipmi_kcs_get_sensor_thresholds (u_int8_t sensor_number, fiid_obj_t obj_data_rs);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-sensor-cmds.h */
