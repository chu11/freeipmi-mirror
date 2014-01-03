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

#ifndef IPMI_SEL_DEFS_H
#define IPMI_SEL_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <sys/param.h>

#include "freeipmi/interpret/ipmi-interpret.h"
#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/sel/ipmi-sel.h"

#include "list.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

#define IPMI_SEL_CTX_MAGIC 0xAECD1846

/* Table 21-1 */
#define IPMI_SEL_RECORD_LENGTH               16
#define IPMI_SEL_RECORD_HEADER_LENGTH         3

#define IPMI_SEL_RESERVATION_ID_RETRY         4

#define IPMI_SEL_FLAGS_MASK                    \
  (IPMI_SEL_FLAGS_DEBUG_DUMP                   \
   | IPMI_SEL_FLAGS_ASSUME_SYTEM_EVENT_RECORDS)

#define IPMI_SEL_SEPARATOR_STRING     " | "

#define IPMI_SEL_STRING_FLAGS_MASK                  \
  (IPMI_SEL_STRING_FLAGS_VERBOSE                    \
   | IPMI_SEL_STRING_FLAGS_IGNORE_UNAVAILABLE_FIELD \
   | IPMI_SEL_STRING_FLAGS_OUTPUT_NOT_AVAILABLE     \
   | IPMI_SEL_STRING_FLAGS_DATE_USE_SLASH           \
   | IPMI_SEL_STRING_FLAGS_DATE_MONTH_STRING        \
   | IPMI_SEL_STRING_FLAGS_NON_ABBREVIATED_UNITS    \
   | IPMI_SEL_STRING_FLAGS_ENTITY_SENSOR_NAMES      \
   | IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA       \
   | IPMI_SEL_STRING_FLAGS_UTC_TO_LOCALTIME	    \
   | IPMI_SEL_STRING_FLAGS_LOCALTIME_TO_UTC	    \
   | IPMI_SEL_STRING_FLAGS_LEGACY)

struct ipmi_sel_entry {
  uint8_t sel_event_record[IPMI_SEL_RECORD_LENGTH];
  unsigned int sel_event_record_len; /* should always be 16, but just in case */
};

struct ipmi_sel_oem_intel_node_manager {
  int node_manager_data_parsed;
  int node_manager_data_found;
  uint8_t nm_health_event_sensor_number;
  uint8_t nm_exception_event_sensor_number;
  uint8_t nm_operational_capabilities_sensor_number;
  uint8_t nm_alert_threshold_exceeded_sensor_number;
};

struct ipmi_sel_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t ipmi_version_major;
  uint8_t ipmi_version_minor;
  char *debug_prefix;
  char *separator;

  uint16_t reservation_id;
  int reservation_id_registered;

  ipmi_ctx_t ipmi_ctx;
  ipmi_sdr_ctx_t sdr_ctx;
  ipmi_interpret_ctx_t interpret_ctx;
  int utc_offset;

  int sel_entries_loaded;
  List sel_entries;
  ListIterator sel_entries_itr;
  struct ipmi_sel_entry *current_sel_entry;

  struct ipmi_sel_entry *callback_sel_entry;

  struct ipmi_sel_oem_intel_node_manager intel_node_manager;
};

#endif /* IPMI_SEL_DEFS_H */
