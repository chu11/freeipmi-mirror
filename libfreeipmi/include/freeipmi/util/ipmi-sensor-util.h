/*
   Copyright (C) 2003-2010 FreeIPMI Core Team

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


#ifndef _IPMI_SENSOR_UTIL_H
#define _IPMI_SENSOR_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_DEFAULT            0x0000
#define IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_SHORT              0x0001
#define IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_INTERPRET_OEM_DATA 0x0002
#define IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_SENSOR_READING     0x0004

/* return length of string written into buffer on success, -1 on error */
int ipmi_get_threshold_message (uint8_t offset, char *buf, unsigned int buflen);

const char *ipmi_get_sensor_type_string (uint8_t sensor_type);

/* b_exponent - sometimes documented as k1 */
/* r_exponent - sometimes documented as k2 */
int ipmi_sensor_decode_value (int8_t r_exponent,
                              int8_t b_exponent,
                              int16_t m,
                              int16_t b,
                              uint8_t linearization,
                              uint8_t analog_data_format,
                              uint8_t raw_data,
                              double *value);

/* b_exponent - sometimes documented as k1 */
/* r_exponent - sometimes documented as k2 */
int ipmi_sensor_decode_raw_value (int8_t r_exponent,
                                  int8_t b_exponent,
                                  int16_t m,
                                  int16_t b,
                                  uint8_t linearization,
                                  uint8_t analog_data_format,
                                  double value,
                                  uint8_t *raw_data);

/* r_exponent - sometimes documented as k2 */
int ipmi_sensor_decode_tolerance (int8_t r_exponent,
                                  int16_t m,
                                  uint8_t raw_data,
                                  uint8_t linearization,
                                  double *value);

/* accuracy returned as percentage */
int ipmi_sensor_decode_accuracy (uint16_t accuracy_raw,
                                 uint8_t accuracy_exp,
                                 double *value);

/* r_exponent - sometimes documented as k2 */
int ipmi_sensor_decode_resolution (int8_t r_exponent,
                                   int16_t m,
                                   double *value);

/* wrapper function to handle retrieval of sensor event messages.  Note
 * that this is specifically for sensor event bitmasks/offsets
 * returned by a get sensor reading call.
 *
 * collectively wraps
 *
 * ipmi_get_generic_event_message()
 * ipmi_get_sensor_type_message()
 *
 * If flag IPMI_GET_SENSOR_EVENT_MESSAGE_FLAGS_SHORT is specified,
 * ipmi_get_generic_event_message_short() and
 * ipmi_get_sensor_type_message_short() are called respectively inplace
 * of ipmi_get_generic_event_message() and ipmi_get_sensor_type_message().
 *
 * If flag IPMI_GET_SENSOR_EVENT_MESSAGE_FLAGS_INTERPRET_OEM_DATA is
 * specified ipmi_get_oem_generic_event_message(),
 * ipmi_get_oem_sensor_type_message(), and
 * ipmi_get_oem_sensor_event_bitmask_message() are called respectively
 * if necessary.
 *
 * If flag IPMI_GET_SENSOR_EVENT_MESSAGES_FLAGS_SENSOR_READING is
 * specified ipmi_get_threshold_message() will be called for a
 * sensor_event_bitmask instead of ipmi_get_generic_event_message() if
 * it is a threshold sensor.
 *
 * If there are no event messages, and 'no_event_message_string' is non-NULL, it will
 * be placed into 'event_messages' as the lone event message.
 *
 * Returns 0 on success, -1 on error.  Number of messages allocated in
 * 'event_messages' is returned in 'event_messages_count'.
 *
 * User responsible for clearing memory created in 'event_messages'.
 */ 
int ipmi_get_sensor_event_messages (uint8_t event_reading_type_code,
                                    uint8_t sensor_type, /* ignored if not relevant for event_reading_type_code */
                                    uint16_t sensor_event_bitmask,
                                    uint32_t manufacturer_id, /* ignored if INTERPRET_OEM_DATA not set */
                                    uint16_t product_id, /* ignored if INTERPRET_OEM_DATA not set */
                                    char ***event_messages,
				    unsigned int *event_messages_count,
                                    const char *no_event_message_string,
                                    unsigned int flags);
#ifdef __cplusplus
}
#endif

#endif /* _IPMI_SENSOR_UTIL_H */
