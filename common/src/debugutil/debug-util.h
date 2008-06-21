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

#ifndef _DEBUG_UTIL_H
#define	_DEBUG_UTIL_H	1

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

int debug_hdr_str(uint8_t packet_type,
                  uint8_t packet_direction,
                  const char *str,
                  char *hdrbuf,
                  unsigned int hdrbuf_len);

int debug_hdr_cmd(uint8_t packet_type,
                  uint8_t packet_direction,
                  uint8_t net_fn, 
                  uint8_t cmd,
                  char *hdrbuf,
                  unsigned int hdrbuf_len);

#endif 
