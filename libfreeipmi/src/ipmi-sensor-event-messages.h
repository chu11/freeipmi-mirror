/* 
   ipmi-sensor-event-messages.h - IPMI Sensor Event Messages

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifndef _IPMI_SENSOR_EVENT_MESSAGES_H
#define _IPMI_SENSOR_EVENT_MESSAGES_H

/* char *get_01_generic_event_message (uint16_t offset); */
/* char *get_02_generic_event_message (uint16_t offset); */
/* char *get_03_generic_event_message (uint16_t offset); */
/* char *get_04_generic_event_message (uint16_t offset); */
/* char *get_05_generic_event_message (uint16_t offset); */
/* char *get_06_generic_event_message (uint16_t offset); */
/* char *get_07_generic_event_message (uint16_t offset); */
/* char *get_08_generic_event_message (uint16_t offset); */
/* char *get_09_generic_event_message (uint16_t offset); */
/* char *get_0A_generic_event_message (uint16_t offset); */
/* char *get_0B_generic_event_message (uint16_t offset); */
/* char *get_0C_generic_event_message (uint16_t offset); */

/* char *get_01_event_message (int offset); */
/* char *get_02_event_message (int offset); */
/* char *get_03_event_message (int offset); */
/* char *get_04_event_message (int offset); */
/* char *get_05_event_message (int offset); */
/* char *get_06_event_message (int offset); */
/* char *get_07_event_message (int offset); */
/* char *get_08_event_message (int offset); */
/* char *get_09_event_message (int offset); */
/* char *get_0C_event_message (int offset); */
/* char *get_0F_event_message (int offset); */
/* char *get_10_event_message (int offset); */
/* char *get_11_event_message (int offset); */
/* char *get_12_event_message (int offset); */
/* char *get_13_event_message (int offset); */
/* char *get_14_event_message (int offset); */
/* char *get_19_event_message (int offset); */
/* char *get_1D_event_message (int offset); */
/* char *get_1E_event_message (int offset); */
/* char *get_1F_event_message (int offset); */
/* char *get_20_event_message (int offset); */
/* char *get_21_event_message (int offset); */
/* char *get_22_event_message (int offset); */
/* char *get_23_event_message (int offset); */
/* char *get_24_event_message (int offset); */
/* char *get_25_event_message (int offset); */
/* char *get_27_event_message (int offset); */
/* char *get_28_event_message (int offset); */
/* char *get_29_event_message (int offset); */
/* char *get_2A_event_message (int offset); */
/* char *get_2B_event_message (int offset); */
/* char *get_2C_event_message (int offset); */

/* char *get_05_event_data2_message (int offset, uint8_t event_data); */
/* char *get_0F_event_data2_message (int offset, uint8_t event_data); */
/* char *get_10_event_data2_message (int offset, uint8_t event_data); */
/* char *get_12_event_data2_message (int offset, uint8_t event_data); */
/* char *get_19_event_data2_message (int offset, uint8_t event_data); */
/* char *get_21_event_data2_message (int offset, uint8_t event_data); */
/* char *get_23_event_data2_message (int offset, uint8_t event_data); */
/* char *get_2A_event_data2_message (int offset, uint8_t event_data); */
/* char *get_2B_event_data2_message (int offset, uint8_t event_data); */
/* char *get_2C_event_data2_message (int offset, uint8_t event_data); */

/* char *get_08_event_data3_message (int offset, uint8_t event_data); */
/* char *get_0C_event_data3_message (int offset, uint8_t event_data); */
/* char *get_10_event_data3_message (int offset, uint8_t event_data); */
/* char *get_19_event_data3_message (int offset, uint8_t event_data); */
/* char *get_21_event_data3_message (int offset, uint8_t event_data); */
/* char *get_2A_event_data3_message (int offset, uint8_t event_data); */

char *ipmi_get_generic_event_message (uint8_t event_reading_type, uint16_t offset);
char *ipmi_get_event_message (int sensor_type_code, int offset);
char *ipmi_get_event_data2_message (int sensor_type_code, int offset, uint8_t event_data);
char *ipmi_get_event_data3_message (int sensor_type_code, int offset, uint8_t event_data);
char **ipmi_get_generic_event_message_list (uint8_t event_reading_type, uint16_t sensor_state);
char **ipmi_get_event_message_list (int sensor_type_code, uint16_t sensor_state);

#endif
