/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

#ifndef _IPMI_SENSOR_CMDS_H
#define _IPMI_SENSOR_CMDS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_SENSOR_GET_SENSOR_COUNT  0x00
#define IPMI_SENSOR_GET_SDR_COUNT     0x01

#define IPMI_SENSOR_STATIC_SENSOR_POPULATION  0x0
#define IPMI_SENSOR_DYNAMIC_SENSOR_POPULATION 0x1

#define IPMI_SENSOR_SELECTED_EVENT_MESSAGES_DO_NOT_CHANGE_INDIVIDUAL_ENABLES 0x0
#define IPMI_SENSOR_SELECTED_EVENT_MESSAGES_ENABLE 0x1
#define IPMI_SENSOR_SELECTED_EVENT_MESSAGES_DISABLE 0x2
#define IPMI_SENSOR_SELECTED_EVENT_MESSAGES_RESERVED 0x3

#define IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_ENABLE  0x1
#define IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE 0x0

#define IPMI_SENSOR_ALL_EVENT_MESSAGES_ENABLE  0x1
#define IPMI_SENSOR_ALL_EVENT_MESSAGES_DISABLE 0x0

/* achu: Yes, this one is backwards.  I don't know why */
#define IPMI_SENSOR_RE_ARM_ALL_EVENT_STATUS_ENABLED 0x0
#define IPMI_SENSOR_RE_ARM_ALL_EVENT_STATUS_DISABLED 0x1

/* achu: Yes, this one is backwards.  I don't know why */
#define IPMI_SENSOR_READING_STATE_UNAVAILABLE 0x1
#define IPMI_SENSOR_READING_STATE_AVAILABLE 0x0

#define IPMI_SENSOR_THRESHOLD_SET     0x1
#define IPMI_SENSOR_THRESHOLD_NOT_SET 0x0

extern fiid_template_t tmpl_cmd_get_device_sdr_info_rq;
extern fiid_template_t tmpl_cmd_get_device_sdr_info_rs;
  
extern fiid_template_t tmpl_cmd_get_device_sdr_rq;
extern fiid_template_t tmpl_cmd_get_device_sdr_rs;

extern fiid_template_t tmpl_cmd_get_sensor_reading_factors_rq;
extern fiid_template_t tmpl_cmd_get_sensor_reading_factors_rs;

extern fiid_template_t tmpl_cmd_set_sensor_hysteresis_rq;
extern fiid_template_t tmpl_cmd_set_sensor_hysteresis_rs;

extern fiid_template_t tmpl_cmd_get_sensor_hysteresis_rq;
extern fiid_template_t tmpl_cmd_get_sensor_hysteresis_rs;

extern fiid_template_t tmpl_cmd_set_sensor_thresholds_rq;
extern fiid_template_t tmpl_cmd_set_sensor_thresholds_rs;

extern fiid_template_t tmpl_cmd_get_sensor_thresholds_rq;
extern fiid_template_t tmpl_cmd_get_sensor_thresholds_rs;

extern fiid_template_t tmpl_cmd_set_sensor_event_enable_rq;
extern fiid_template_t tmpl_cmd_set_sensor_event_enable_rs;
extern fiid_template_t tmpl_cmd_set_sensor_event_enable_threshold_rq;
extern fiid_template_t tmpl_cmd_set_sensor_event_enable_discrete_rq;

extern fiid_template_t tmpl_cmd_get_sensor_event_enable_rq;
extern fiid_template_t tmpl_cmd_get_sensor_event_enable_rs;
extern fiid_template_t tmpl_cmd_get_sensor_event_enable_threshold_rs;
extern fiid_template_t tmpl_cmd_get_sensor_event_enable_discrete_rs;

extern fiid_template_t tmpl_cmd_re_arm_sensor_events_rq;
extern fiid_template_t tmpl_cmd_re_arm_sensor_events_rs;
extern fiid_template_t tmpl_cmd_re_arm_sensor_events_threshold_rq;
extern fiid_template_t tmpl_cmd_re_arm_sensor_events_discrete_rq;

extern fiid_template_t tmpl_cmd_get_sensor_event_status_rq;
extern fiid_template_t tmpl_cmd_get_sensor_event_status_rs;
extern fiid_template_t tmpl_cmd_get_sensor_event_status_threshold_rq;
extern fiid_template_t tmpl_cmd_get_sensor_event_status_threshold_rs;
extern fiid_template_t tmpl_cmd_get_sensor_event_status_discerete_rq;
extern fiid_template_t tmpl_cmd_get_sensor_event_status_discerete_rs;

extern fiid_template_t tmpl_cmd_get_sensor_reading_rq;
extern fiid_template_t tmpl_cmd_get_sensor_reading_rs;
extern fiid_template_t tmpl_cmd_get_sensor_reading_threshold_rs;
extern fiid_template_t tmpl_cmd_get_sensor_reading_discrete_rs;

extern fiid_template_t tmpl_cmd_set_sensor_type_rq;
extern fiid_template_t tmpl_cmd_set_sensor_type_rs;

extern fiid_template_t tmpl_cmd_get_sensor_type_rq;
extern fiid_template_t tmpl_cmd_get_sensor_type_rs;

int8_t fill_cmd_set_sensor_thresholds (uint8_t sensor_number,
                                       uint8_t *lower_non_critical_threshold,
                                       uint8_t *lower_critical_threshold,
                                       uint8_t *lower_non_recoverable_threshold,
                                       uint8_t *upper_non_critical_threshold,
                                       uint8_t *upper_critical_threshold,
                                       uint8_t *upper_non_recoverable_threshold,
                                       fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_sensor_thresholds (uint8_t sensor_number, fiid_obj_t obj_cmd_rq);

int8_t fill_cmd_get_sensor_reading (uint8_t sensor_number, fiid_obj_t obj_cmd_rq);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-sensor-cmds.h */
