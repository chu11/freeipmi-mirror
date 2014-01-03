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

#ifndef IPMI_FRU_DEFS_H
#define IPMI_FRU_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>

#include "freeipmi/fru/ipmi-fru.h"

#define IPMI_FRU_CTX_MAGIC 0x12CD1DBF

#define IPMI_FRU_FLAGS_MASK \
  (IPMI_FRU_FLAGS_DEBUG_DUMP \
   | IPMI_FRU_FLAGS_SKIP_CHECKSUM_CHECKS \
   | IPMI_FRU_FLAGS_INTERPRET_OEM_DATA \
   | IPMI_FRU_FLAGS_READ_RAW)

#define IPMI_FRU_BUF_LEN 2048

struct ipmi_fru_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;
  uint32_t manufacturer_id;
  uint16_t product_id;
  char *debug_prefix;

  ipmi_ctx_t ipmi_ctx;
  uint8_t fru_device_id;
  unsigned int fru_inventory_area_size;
  unsigned int chassis_info_area_starting_offset;
  unsigned int board_info_area_starting_offset;
  unsigned int product_info_area_starting_offset;
  unsigned int multirecord_area_starting_offset;
  unsigned int device_opened;

  int chassis_info_area_parsed;
  int board_info_area_parsed;
  int product_info_area_parsed;
  int multirecord_area_parsed;
  unsigned int multirecord_area_offset_in_bytes;
};

#endif /* IPMI_FRU_PARSE_DEFS_H */
