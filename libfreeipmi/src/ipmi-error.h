/* 
   ipmi-error.h - IPMI error handling

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


#ifndef _IPMI_ERROR_H
#define	_IPMI_ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

  /* XXX */
#include "fiid.h"

#define IPMI_ERR_STR_MAX_LEN                 0x0800

int8_t ipmi_strerror_r (uint8_t cmd, 
			uint8_t comp_code, 
			char *errstr, 
			size_t len);
int8_t ipmi_strerror_cmd_r (fiid_obj_t obj_cmd, 
			    char *errstr, 
			    size_t len);
int8_t ipmi_kcs_strstatus_r (uint8_t status_code, 
			     char *errstr, 
			     size_t len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-error.h */

