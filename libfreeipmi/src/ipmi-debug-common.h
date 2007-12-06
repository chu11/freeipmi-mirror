/*
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifndef _IPMI_DEBUG_COMMON_H
#define	_IPMI_DEBUG_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "ipmi-common.h"
#include "ipmi-err-wrappers.h"

#define IPMI_DEBUG_MAX_PREFIX_LEN 32

#define FREEIPMI_DPRINTF(args) \
        do { \
          ERR (!((freeipmi_dprintf args) < 0)); \
        } while(0) 

#define FREEIPMI_DPRINTF_CLEANUP(args) \
        do { \
          ERR_CLEANUP (!((freeipmi_dprintf args) < 0)); \
        } while(0) 

int8_t ipmi_debug_set_prefix(char *buf, unsigned int buflen, char *prefix);

int8_t ipmi_debug_output_str(int fd, char *prefix, char *str);

int8_t ipmi_debug_output_byte_array(int fd, char *prefix, uint8_t *buf, uint32_t buf_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-debug-common.h */


