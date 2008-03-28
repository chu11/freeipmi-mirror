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

#ifndef _IPMI_SEL_ENTRY_H
#define _IPMI_SEL_ENTRY_H

#include "ipmi-sel.h"

#define IPMI_SEL_RECORD_SIZE 16

int ipmi_sel_get_entry (ipmi_sel_state_data_t *state_data,
                        uint16_t record_id, 
                        uint16_t *next_record_id,
                        uint16_t *stored_record_id,
                        char **timestamp,
                        char **sensor_info,
                        char **event_message,
                        char **event_data2_message,
                        char **event_data3_message);

#endif
