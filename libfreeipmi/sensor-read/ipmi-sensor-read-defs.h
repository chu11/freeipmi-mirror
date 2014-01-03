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

#ifndef IPMI_SENSOR_READ_DEFS_H
#define IPMI_SENSOR_READ_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <sys/param.h>

#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/sensor-read/ipmi-sensor-read.h"

#include "list.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SENSOR_READ_CTX_MAGIC 0xABCD1246

#define IPMI_SENSOR_READ_FLAGS_MASK                  \
  (IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS             \
   | IPMI_SENSOR_READ_FLAGS_DISCRETE_READING         \
   | IPMI_SENSOR_READ_FLAGS_IGNORE_SCANNING_DISABLED \
   | IPMI_SENSOR_READ_FLAGS_ASSUME_BMC_OWNER)

struct ipmi_sensor_read_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;

  ipmi_ctx_t ipmi_ctx;
  ipmi_sdr_ctx_t sdr_ctx;
};

#endif /* IPMI_SENSOR_READ_DEFS_H */
