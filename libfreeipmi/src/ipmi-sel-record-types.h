/* 
   ipmi-sel-record-types.h - IPMI System Event Log Record Type Definitions
   
   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _IPMI_SEL_RECORD_TYPES_H
#define _IPMI_SEL_RECORD_TYPES_H

#define IPMI_SEL_FIRST_ENTRY    0x0 
#define IPMI_SEL_LAST_ENTRY     0xFFFF 

#define IPMI_V1_0_EVENT_MESSAGE_FORMAT    0x03
#define IPMI_V1_5_EVENT_MESSAGE_FORMAT    0x04

#define IPMI_SEL_UNSPECIFIED_BYTE                  0x0
#define IPMI_SEL_TRIGGER_THRESHOLD_VALUE           0x1
#define IPMI_SEL_OEM_CODE                          0x2
#define IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE    0x3
#define IPMI_SEL_TRIGGER_READING                   0x1
#define IPMI_SEL_PREV_STATE_SEVERITY               0x1

#ifdef __cplusplus
extern "C" {
#endif

enum ipmi_sel_record_type
  {
    IPMI_SEL_UNKNOWN_RECORD, 
    IPMI_SEL_SYSTEM_EVENT_RECORD = 0x02, 
    IPMI_SEL_TIMESTAMPED_OEM_RECORD = 0xDF, 
    IPMI_SEL_NON_TIMESTAMPED_OEM_RECORD = 0xFF 
  };

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

extern fiid_template_t tmpl_sel_record_header;

extern fiid_template_t tmpl_sel_system_event_record;
extern fiid_template_t tmpl_sel_timestamped_oem_record;
extern fiid_template_t tmpl_sel_non_timestamped_oem_record;

extern fiid_template_t tmpl_threshold_event_data;
extern fiid_template_t tmpl_discrete_event_data;
extern fiid_template_t tmpl_oem_event_data;

int ipmi_get_sel_record_type (uint8_t record_type);
int get_sel_record (uint8_t *record_data, uint32_t record_data_len, sel_record_t *sel_record);

#ifdef __cplusplus
}
#endif

#endif
