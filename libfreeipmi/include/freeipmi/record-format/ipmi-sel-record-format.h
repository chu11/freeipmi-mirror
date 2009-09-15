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

#ifndef _IPMI_SEL_RECORD_FORMAT_H
#define _IPMI_SEL_RECORD_FORMAT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/fiid/fiid.h>

#define IPMI_V1_0_EVENT_MESSAGE_FORMAT 0x03
#define IPMI_V1_5_EVENT_MESSAGE_FORMAT 0x04

/* Refer to Table 29-6 */
#define IPMI_SEL_UNSPECIFIED_BYTE                  0x0
#define IPMI_SEL_TRIGGER_THRESHOLD_VALUE           0x1
#define IPMI_SEL_OEM_CODE                          0x2
#define IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE    0x3
#define IPMI_SEL_TRIGGER_READING                   0x1
#define IPMI_SEL_PREV_STATE_SEVERITY               0x1

#define IPMI_SEL_RECORD_ASSERTION_EVENT   0x0
#define IPMI_SEL_RECORD_DEASSERTION_EVENT 0x1

#define IPMI_SEL_RECORD_EVENT_DIRECTION_VALID(__event_direction)        \
  (((__event_direction) == IPMI_SEL_RECORD_ASSERTION_EVENT              \
    || (__event_direction) == IPMI_SEL_RECORD_DEASSERTION_EVENT) ? 1 : 0)

#define IPMI_SEL_RECORD_TYPE_IS_EVENT(__record_type) \
  (((__record_type) == 0x02) ? 1 : 0)

#define IPMI_SEL_RECORD_TYPE_IS_TIMESTAMPED_OEM(__record_type) \
  (((__record_type) >= 0xC0 \
    && (__record_type) <= 0xDF) ? 1 : 0)

/* "== 0xFF" to remove warnings */
#define IPMI_SEL_RECORD_TYPE_IS_NON_TIMESTAMPED_OEM(__record_type) \
  (((__record_type) >= 0xE0 \
    && ((__record_type) <= 0xFE || (__record_type) == 0xFF)) ? 1 : 0)

extern fiid_template_t tmpl_sel_record_header;

extern fiid_template_t tmpl_sel_system_event_record;
extern fiid_template_t tmpl_sel_timestamped_oem_record;
extern fiid_template_t tmpl_sel_non_timestamped_oem_record;

extern fiid_template_t tmpl_threshold_event_data;
extern fiid_template_t tmpl_discrete_event_data;
extern fiid_template_t tmpl_oem_event_data;

#ifdef __cplusplus
}
#endif

#endif
