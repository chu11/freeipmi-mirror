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

#ifndef _CONFIG_FIID_H_
#define _CONFIG_FIID_H_

#include <stdio.h>
#include <stdint.h>

#include "config-common.h"

fiid_obj_t Fiid_obj_create(fiid_template_t tmpl);

int8_t Fiid_obj_get(fiid_obj_t obj, char *field, uint64_t *val);

int32_t Fiid_obj_get_data(fiid_obj_t obj,
                          char *field,
                          uint8_t *data,
                          uint32_t data_len);

int32_t Fiid_obj_set_data (fiid_obj_t obj,
                           char *field,
                           uint8_t *data,
                           uint32_t data_len);

int Fiid_obj_clear(fiid_obj_t obj);

void Fiid_obj_destroy(fiid_obj_t obj);

#endif /* _CONFIG_FIID_H_ */
