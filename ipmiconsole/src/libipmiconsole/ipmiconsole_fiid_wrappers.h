/*****************************************************************************\
 *  $Id: ipmiconsole_fiid_wrappers.h,v 1.3 2007-09-05 20:13:29 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *  
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmiconsole is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmiconsole is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMICONSOLE_FIID_WRAPPERS_H
#define _IPMICONSOLE_FIID_WRAPPERS_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>

int32_t Fiid_template_len_bytes(ipmiconsole_ctx_t c, fiid_template_t tmpl);
int32_t Fiid_template_block_len_bytes(ipmiconsole_ctx_t c, fiid_template_t tmpl, char *field_start, char *field_end);

fiid_obj_t Fiid_obj_create(ipmiconsole_ctx_t c, fiid_template_t tmpl);
int8_t Fiid_obj_clear(ipmiconsole_ctx_t c, fiid_obj_t obj);
int8_t Fiid_obj_clear_field(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field);
void Fiid_obj_destroy(ipmiconsole_ctx_t c, fiid_obj_t obj);
int8_t Fiid_obj_get(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint64_t *val);
int32_t Fiid_obj_get_data(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len);
int8_t Fiid_obj_set(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint64_t val);
int32_t Fiid_obj_set_data(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len);
int32_t Fiid_obj_set_all(ipmiconsole_ctx_t c, fiid_obj_t obj, uint8_t *data, uint32_t data_len);

#endif /* _IPMICONSOLE_FIID_WRAPPERS_H */
