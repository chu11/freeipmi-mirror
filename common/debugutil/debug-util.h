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

#ifndef DEBUG_UTIL_H
#define DEBUG_UTIL_H

#include <stdint.h>

#define DEBUG_UTIL_HDR_BUFLEN     1024

#define DEBUG_UTIL_TYPE_NONE      0x00
#define DEBUG_UTIL_TYPE_INBAND    0x01
#define DEBUG_UTIL_TYPE_IPMI_1_5  0x02
#define DEBUG_UTIL_TYPE_IPMI_2_0  0x03

#define DEBUG_UTIL_DIRECTION_NONE      0x00
#define DEBUG_UTIL_DIRECTION_REQUEST   0x01
#define DEBUG_UTIL_DIRECTION_RESPONSE  0x02

#define DEBUG_UTIL_OPEN_SESSION_STR   "Open Session"
#define DEBUG_UTIL_RAKP_1_STR         "RAKP Message 1"
#define DEBUG_UTIL_RAKP_2_STR         "RAKP Message 2"
#define DEBUG_UTIL_RAKP_3_STR         "RAKP Message 3"
#define DEBUG_UTIL_RAKP_4_STR         "RAKP Message 4"
#define DEBUG_UTIL_RMCPPING_STR       "RMCP Ping"
#define DEBUG_UTIL_RMCPPONG_STR       "RMCP Pong"

#define DEBUG_UTIL_FLAGS_DEFAULT         0x0000
#define DEBUG_UTIL_FLAGS_GROUP_EXTENSION 0x0001
#define DEBUG_UTIL_FLAGS_OEM_GROUP       0x0002
#define DEBUG_UTIL_FLAGS_OEM             0x0004

int debug_hdr_str (uint8_t packet_type,
                   uint8_t packet_direction,
                   unsigned int packet_flags,
                   const char *str,
                   char *hdrbuf,
                   unsigned int hdrbuf_len);

int debug_hdr_cmd (uint8_t packet_type,
                   uint8_t packet_direction,
                   uint8_t net_fn,
                   uint8_t cmd,
		   uint8_t group_extension,
                   char *hdrbuf,
                   unsigned int hdrbuf_len);

#endif /* DEBUG_UTIL_H */
