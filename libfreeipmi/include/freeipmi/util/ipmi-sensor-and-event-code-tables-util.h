/*
   Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifndef _IPMI_SENSOR_AND_EVENT_CODE_TABLES_UTIL_H
#define _IPMI_SENSOR_AND_EVENT_CODE_TABLES_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD                0x01
#define IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE         0x02
#define IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE 0x03
#define IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM                      0x04
#define IPMI_EVENT_READING_TYPE_CODE_CLASS_UNKNOWN                  0x05

int ipmi_event_reading_type_code_class (uint8_t event_reading_type_code);

/* return length of string written into buffer on success, -1 on error */
int ipmi_get_generic_event_message (uint8_t event_reading_type_code,
                                    unsigned int offset,
                                    char *buf,
                                    unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
int ipmi_get_sensor_type_message (uint8_t sensor_type,
                                  unsigned int offset,
                                  char *buf,
                                  unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
/* identical to above but returns "short" strings when appropriate */
int ipmi_get_generic_event_message_short (uint8_t event_reading_type_code,
                                          unsigned int offset,
                                          char *buf,
                                          unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
/* identical to above but returns "short" strings when appropriate */
int ipmi_get_sensor_type_message_short (uint8_t sensor_type,
                                        unsigned int offset,
                                        char *buf,
                                        unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
int ipmi_get_event_data2_message (uint8_t sensor_type,
                                  unsigned int offset,
                                  uint8_t event_data2,
                                  char *buf,
                                  unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
int ipmi_get_event_data3_message (uint8_t sensor_type,
                                  unsigned int offset,
                                  uint8_t event_data2,
                                  uint8_t event_data3,
                                  char *buf,
                                  unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
int ipmi_get_oem_generic_event_message (uint32_t manufacturer_id,
                                        uint16_t product_id,
                                        uint8_t event_reading_type_code,
                                        unsigned int offset,
                                        char *buf,
                                        unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
int ipmi_get_oem_sensor_type_message (uint32_t manufacturer_id,
                                      uint16_t product_id,
                                      uint8_t sensor_type,
                                      unsigned int offset,
                                      char *buf,
                                      unsigned int buflen);

/* return length of string written into buffer on success, -1 on error */
/* some vendors return values instead of event bitmasks in the 
 * sensor or SEL event, this is to handle this special case
 */
int ipmi_get_oem_sensor_event_bitmask_message (uint32_t manufacturer_id,
					       uint16_t product_id,
					       uint8_t event_reading_type_code,
					       uint8_t sensor_type,
					       uint16_t sensor_event_bitmask,
					       char *buf,
					       unsigned int buflen);
  
#ifdef __cplusplus
}
#endif

#endif
