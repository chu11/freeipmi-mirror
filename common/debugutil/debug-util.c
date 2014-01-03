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

#include <freeipmi/freeipmi.h>

#include "debug-util.h"

#include "freeipmi-portability.h"

int
debug_hdr_str (uint8_t packet_type,
               uint8_t packet_direction,
               unsigned int packet_flags,
               const char *str,
               char *hdrbuf,
               unsigned int hdrbuf_len)
{
  char *fmt_inband =
    "=====================================================\n"
    "%s%s %s\n"
    "=====================================================";
  char *fmt_outofband =
    "=====================================================\n"
    "%s %s%s %s\n"
    "=====================================================";
  char *str_direction;
  char *str_prefix;
  int len;

  assert (packet_type == DEBUG_UTIL_TYPE_NONE
	  || packet_type == DEBUG_UTIL_TYPE_INBAND
	  || packet_type == DEBUG_UTIL_TYPE_IPMI_1_5
	  || packet_type == DEBUG_UTIL_TYPE_IPMI_2_0);
  assert (packet_direction == DEBUG_UTIL_DIRECTION_NONE
	  || packet_direction == DEBUG_UTIL_DIRECTION_REQUEST
	  || packet_direction == DEBUG_UTIL_DIRECTION_RESPONSE);
  assert (str);
  assert (hdrbuf);
  assert (hdrbuf_len);

  memset (hdrbuf, '\0', hdrbuf_len);

  if (packet_direction == DEBUG_UTIL_DIRECTION_REQUEST)
    str_direction = "Request";
  else if (packet_direction == DEBUG_UTIL_DIRECTION_RESPONSE)
    str_direction = "Response";
  else
    str_direction = "";

  if (packet_flags & DEBUG_UTIL_FLAGS_GROUP_EXTENSION)
    str_prefix = "Group Extension - ";
  else if (packet_flags & DEBUG_UTIL_FLAGS_OEM_GROUP)
    str_prefix = "OEM Group - ";
  else if (packet_flags & DEBUG_UTIL_FLAGS_OEM)
    str_prefix = "OEM - ";
  else
    str_prefix = "";
  
  if (packet_type == DEBUG_UTIL_TYPE_NONE
      || packet_type == DEBUG_UTIL_TYPE_INBAND)
    len = snprintf (hdrbuf,
                    hdrbuf_len,
                    fmt_inband,
                    str_prefix,
                    str,
                    str_direction);
  else
    {
      char *str_version;

      if (packet_type == DEBUG_UTIL_TYPE_IPMI_1_5)
        str_version = "IPMI 1.5";
      else
        str_version = "IPMI 2.0";

      len = snprintf (hdrbuf,
                      hdrbuf_len,
                      fmt_outofband,
                      str_version,
                      str_prefix,
                      str,
                      str_direction);
    }

  if (len < 0 || len >= hdrbuf_len)
    return (-1);

  return (0);
}

int
debug_hdr_cmd (uint8_t packet_type,
               uint8_t packet_direction,
               uint8_t net_fn,
               uint8_t cmd,
	       uint8_t group_extension,
               char *hdrbuf,
               unsigned int hdrbuf_len)
{
  const char *str_cmd;
  unsigned int packet_flags = 0;

  if (IPMI_NET_FN_GROUP_EXTENSION (net_fn))
    {
      if (group_extension == IPMI_NET_FN_GROUP_EXTENSION_IDENTIFICATION_DCMI)
	str_cmd = ipmi_cmd_dcmi_str (cmd);
      else
	str_cmd = "Unknown";
      packet_flags = DEBUG_UTIL_FLAGS_GROUP_EXTENSION;
    }
  else if (IPMI_NET_FN_OEM_GROUP (net_fn))
    {
      str_cmd = "Unknown";
      packet_flags = DEBUG_UTIL_FLAGS_OEM_GROUP;
    }
  else if (IPMI_NET_FN_CONTROLLER_SPECIFIC_OEM_GROUP (net_fn))
    {
      str_cmd = "Unknown";
      packet_flags = DEBUG_UTIL_FLAGS_OEM;
    } 
  else
    str_cmd = ipmi_cmd_str (net_fn, cmd);

  return (debug_hdr_str (packet_type,
                         packet_direction,
                         packet_flags,
                         str_cmd,
                         hdrbuf,
                         hdrbuf_len));
}
