#ifndef _IPMI_SDR_RECORD_TYPES_H
#define _IPMI_SDR_RECORD_TYPES_H

#define IPMI_SDR_FORMAT_FULL_RECORD                        0x01
#define IPMI_SDR_FORMAT_COMPACT_RECORD                     0x02
#define IPMI_SDR_FORMAT_EVENT_ONLY_RECORD                  0x03
#define IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD                 0x08
#define IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD             0x09
#define IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD             0x10
#define IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD             0x11
#define IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD     0x12
#define IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD    0x13
#define IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD        0x14
#define IPMI_SDR_FORMAT_OEM_RECORD                         0xC0

#define IPMI_SDR_ENTIRE_SNSR_GLOBAL_DISABLE_SUPPORT    0x0
#define IPMI_SDR_ENTIRE_SNSR_SUPPORT_ONLY              0x1
#define IPMI_SDR_GLOBAL_DISABLE_SUPPORT_ONLY           0x2
#define IPMI_SDR_NO_EVENT_SUPPORT_ONLY                 0x3

#define IPMI_SDR_NO_THRESHOLDS_SUPPORT                   0x0
#define IPMI_SDR_READABLE_THRESHOLDS_SUPPORT             0x1
#define IPMI_SDR_READABLE_SETTABLE_THRESHOLDS_SUPPORT    0x2
#define IPMI_SDR_FIXED_UNREADABLE_THRESHOLDS_SUPPORT     0x3

#define IPMI_SDR_NO_HYSTERESIS_SUPPORT                   0x0
#define IPMI_SDR_READABLE_HYSTERESIS_SUPPORT             0x1
#define IPMI_SDR_READABLE_SETTABLE_HYSTERESIS_SUPPORT    0x2
#define IPMI_SDR_FIXED_UNREADABLE_HYSTERESIS_SUPPORT     0x3

#define IPMI_SDR_SENSOR_UNIT_MODIFIER_UNIT_NONE        0x0
#define IPMI_SDR_SENSOR_UNIT_MODIFIER_UNIT_DIVIDE      0x1
#define IPMI_SDR_SENSOR_UNIT_MODIFIER_UNIT_MULTIPLY    0x2

#define IPMI_SDR_SENSOR_UNIT_RATE_UNIT_NONE          0x0
#define IPMI_SDR_SENSOR_UNIT_RATE_UNIT_PER_USEC      0x1
#define IPMI_SDR_SENSOR_UNIT_RATE_UNIT_PER_MSEC      0x2
#define IPMI_SDR_SENSOR_UNIT_RATE_UNIT_PER_SEC       0x3
#define IPMI_SDR_SENSOR_UNIT_RATE_UNIT_PER_MINUTE    0x4
#define IPMI_SDR_SENSOR_UNIT_RATE_UNIT_PER_HOUR      0x5
#define IPMI_SDR_SENSOR_UNIT_RATE_UNIT_PER_DAY       0x6

#define IPMI_SDR_SENSOR_UNIT_ANALOG_DATA_FORMAT_UNSIGNED             0x0
#define IPMI_SDR_SENSOR_UNIT_ANALOG_DATA_FORMAT_1S_COMPLEMENT        0x1
#define IPMI_SDR_SENSOR_UNIT_ANALOG_DATA_FORMAT_2S_COMPLEMENT        0x2
#define IPMI_SDR_SENSOR_UNIT_ANALOG_DATA_FORMAT_NO_ANALOG_READING    0x3


#ifdef __cplusplus
extern "C" {
#endif


extern fiid_template_t tmpl_sdr_full_sensor_record;
extern fiid_template_t tmpl_sdr_compact_sensor_record;
extern fiid_template_t tmpl_sdr_event_only_sensor_record;
extern fiid_template_t tmpl_sdr_entity_association_sensor_record;
extern fiid_template_t tmpl_generic_device_locator_sensor_record;
extern fiid_template_t tmpl_sdr_logical_fru_device_locator_sensor_record;
extern fiid_template_t tmpl_sdr_non_intelligent_fru_device_locator_sensor_record;
extern fiid_template_t tmpl_sdr_management_controller_device_locator_sensor_record;
extern fiid_template_t tmpl_sdr_oem_sensor_record;


#ifdef __cplusplus
}
#endif

#endif
