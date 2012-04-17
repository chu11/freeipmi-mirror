/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#ifndef IPMI_SDR_PARSE_DEFS_H
#define IPMI_SDR_PARSE_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <sys/param.h>

#include "freeipmi/sdr-parse/ipmi-sdr-parse.h"

#include "list.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SDR_PARSE_CTX_MAGIC 0xFECD18B6

#define IPMI_SDR_PARSE_FLAGS_MASK           (0)

struct ipmi_sdr_parse_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;
};

#endif /* IPMI_SDR_PARSE_DEFS_H */
