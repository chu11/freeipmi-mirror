/* 
   Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifndef _IPMI_SENSOR_READ_DEFS_H
#define _IPMI_SENSOR_READ_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <sys/param.h>

#include "freeipmi/sensor-read/ipmi-sensor-read.h"

#include "list.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SENSOR_READ_MAGIC 0xABCD1246

#define IPMI_SENSOR_READ_FLAGS_MASK \
  (IPMI_SENSOR_READ_FLAGS_DEBUG_DUMP \
   | IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS)

struct ipmi_sensor_read_ctx {
  uint32_t magic;
  unsigned int errnum;
  unsigned int flags;
  char *debug_prefix;

  ipmi_ctx_t ipmi_ctx;
  ipmi_sdr_cache_ctx_t sdr_cache_ctx;
};

#endif /* _IPMI_SENSOR_READ_DEFS_H */
