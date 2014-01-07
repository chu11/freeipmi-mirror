/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef IPMI_DEBUG_COMMON_H
#define IPMI_DEBUG_COMMON_H

#include <stdint.h>

#include "libcommon/ipmi-trace.h"

#define IPMI_DEBUG_MAX_PREFIX_LEN 32
#define IPMI_DEBUG_MAX_HDR_LEN 1024
#define IPMI_DEBUG_MAX_PKT_LEN 65536

/* Portable version of the extremely unportable Linux dprintf() */
int debug_dprintf (int fd, const char *fmt, ...);

int debug_set_prefix (char *buf, unsigned int buflen, const char *prefix);

int debug_output_str (int fd, const char *prefix, const char *str);

int debug_output_byte_array (int fd, const char *prefix, const uint8_t *buf, unsigned int buf_len);

int debug_dump_ipmb (int fd,
		     const char *prefix,
		     const uint8_t *ipmb_buf,
		     unsigned int ipmb_buf_len,
		     fiid_template_t tmpl_ipmb_msg_hdr,
		     fiid_template_t tmpl_ipmb_cmd);

#endif /* IPMI_DEBUG_COMMON_H */
