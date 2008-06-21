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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "freeipmi/util/ipmi-util.h"

#include "debug-util.h"

#include "freeipmi-portability.h"

int
debug_hdr_str(uint8_t packet_type,
              uint8_t packet_direction,
              const char *str,
              char *hdrbuf,
              unsigned int hdrbuf_len)
{
  char *fmt_inband =
    "=====================================================\n"
    "%s %s\n"
    "=====================================================";
  char *fmt_outofband =
    "=====================================================\n"
    "%s %s %s\n"
    "=====================================================";
  char *str_direction;
  int len;

  if (!((packet_type == DEBUG_UTIL_TYPE_NONE
         || packet_type == DEBUG_UTIL_TYPE_INBAND
         || packet_type == DEBUG_UTIL_TYPE_IPMI_1_5
         || packet_type == DEBUG_UTIL_TYPE_IPMI_2_0)
        && (packet_direction == DEBUG_UTIL_DIRECTION_NONE
            || packet_direction == DEBUG_UTIL_DIRECTION_REQUEST
            || packet_direction == DEBUG_UTIL_DIRECTION_RESPONSE)
        && str
        && hdrbuf
        && hdrbuf_len))
    return -1;
  
  memset(hdrbuf, '\0', hdrbuf_len);

  if (packet_direction == DEBUG_UTIL_DIRECTION_REQUEST)
    str_direction = "Request";
  else if (packet_direction == DEBUG_UTIL_DIRECTION_RESPONSE)
    str_direction = "Response";
  else
    str_direction = "";

  if (packet_type == DEBUG_UTIL_TYPE_NONE
      || packet_type == DEBUG_UTIL_TYPE_INBAND)
    len = snprintf(hdrbuf,
                   hdrbuf_len,
                   fmt_inband,
                   str,
                   str_direction);
  else
    {
      char *str_version;

      if (packet_type == DEBUG_UTIL_TYPE_IPMI_1_5)
        str_version = "IPMI 1.5";
      else
        str_version = "IPMI 2.0";

      len = snprintf(hdrbuf,
                     hdrbuf_len,
                     fmt_outofband,
                     str_version,
                     str,
                     str_direction);
    }

  if (len < 0 || len >= hdrbuf_len)
    return -1;

  return 0;
}

int
debug_hdr_cmd(uint8_t packet_type,
              uint8_t packet_direction,
              uint8_t net_fn, 
              uint8_t cmd,
              char *hdrbuf,
              unsigned int hdrbuf_len)
{
  const char *str_cmd;

  str_cmd = ipmi_cmd_str(net_fn, cmd);

  return debug_hdr_str(packet_type,
                       packet_direction,
                       str_cmd,
                       hdrbuf,
                       hdrbuf_len);
}
