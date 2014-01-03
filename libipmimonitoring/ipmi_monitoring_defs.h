/*****************************************************************************\
 *  $Id: ipmi_monitoring_defs.h,v 1.41 2010-07-22 21:49:00 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMI_MONITORING_DEFS_H
#define IPMI_MONITORING_DEFS_H

#if HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <limits.h>             /* MAXHOSTNAMELEN */
#ifdef HAVE_NETDB_H
#include <netdb.h>              /* MAXHOSTNAMELEN Solaris */
#endif /* HAVE_NETDB_H */
#include <freeipmi/freeipmi.h>

#include "list.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

#ifndef MAXPATHLEN
#define MAXPATHLEN 4096
#endif /* MAXPATHLEN */

/* +1 to avoid gcc warnings */
#define IPMI_MONITORING_SENSOR_TYPE_VALID(__val)                    \
  (((((__val) + 1) >= (IPMI_MONITORING_SENSOR_TYPE_RESERVED + 1)    \
     && ((__val) - 1) <= (IPMI_MONITORING_SENSOR_TYPE_OEM_MAX - 1)) \
    || ((__val) == IPMI_MONITORING_SENSOR_TYPE_UNKNOWN)) ? 1 : 0)

#define IPMI_MONITORING_SENSOR_TYPE_INPUT_VALID(__val)              \
  ((((__val) + 1) >= (IPMI_MONITORING_SENSOR_TYPE_TEMPERATURE + 1)  \
    && ((__val) - 1) <= (IPMI_MONITORING_SENSOR_TYPE_OEM_MAX - 1)) ? 1 : 0)

#define IPMI_MONITORING_STATE_VALID(__val)                  \
  (((__val) == IPMI_MONITORING_STATE_NOMINAL                \
    || (__val) == IPMI_MONITORING_STATE_WARNING             \
    || (__val) == IPMI_MONITORING_STATE_CRITICAL            \
    || (__val) == IPMI_MONITORING_STATE_UNKNOWN) ? 1 : 0)

/* +1 to avoid gcc warnings */
#define IPMI_MONITORING_SENSOR_UNITS_VALID(__val)                   \
  ((((__val) + 1) >= (IPMI_MONITORING_SENSOR_UNITS_NONE + 1)        \
    && (__val) <= IPMI_MONITORING_SENSOR_UNITS_UNKNOWN) ? 1 : 0)

#define IPMI_MONITORING_SENSOR_READING_TYPE_VALID(__val)                  \
  (((__val) == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL \
    || (__val) == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32  \
    || (__val) == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE              \
    || (__val) == IPMI_MONITORING_SENSOR_READING_TYPE_UNKNOWN) ? 1 : 0)

/* +1 to avoid gcc warnings */
#define IPMI_MONITORING_SENSOR_BITMASK_TYPE_VALID(__val)                  \
  (((((__val) + 1) >= (IPMI_MONITORING_SENSOR_BITMASK_TYPE_THRESHOLD + 1) \
     && (__val) <= IPMI_MONITORING_SENSOR_BITMASK_TYPE_FRU_STATE)         \
    || (__val) == IPMI_MONITORING_SENSOR_BITMASK_TYPE_OEM                 \
    || (__val) == IPMI_MONITORING_SENSOR_BITMASK_TYPE_UNKNOWN) ? 1 : 0)

#define IPMI_MONITORING_MAX_USER_NAME_LENGTH IPMI_MAX_USER_NAME_LENGTH

#define IPMI_MONITORING_MAX_PASSWORD_LENGTH  IPMI_MAX_1_5_PASSWORD_LENGTH

#define IPMI_MONITORING_PRIVILEGE_LEVEL_VALID(__val)              \
  (((__val) == IPMI_MONITORING_PRIVILEGE_LEVEL_USER               \
    || (__val) == IPMI_MONITORING_PRIVILEGE_LEVEL_OPERATOR        \
    || (__val) == IPMI_MONITORING_PRIVILEGE_LEVEL_ADMIN) ? 1 : 0)

#define IPMI_MONITORING_AUTHENTICATION_TYPE_VALID(__val)                    \
  (((__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_NONE                     \
    || (__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY \
    || (__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_MD2                   \
    || (__val) == IPMI_MONITORING_AUTHENTICATION_TYPE_MD5) ? 1 : 0)

#define IPMI_MONITORING_FLAGS_MASK               \
  (IPMI_MONITORING_FLAGS_NONE                    \
   | IPMI_MONITORING_FLAGS_DEBUG                 \
   | IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS    \
   | IPMI_MONITORING_FLAGS_LOCK_MEMORY)

#define IPMI_MONITORING_SEL_FLAGS_MASK                    \
  (IPMI_MONITORING_SEL_FLAGS_REREAD_SDR_CACHE             \
   | IPMI_MONITORING_SEL_FLAGS_INTERPRET_OEM_DATA         \
   | IPMI_MONITORING_SEL_FLAGS_ASSUME_SYSTEM_EVENT_RECORD \
   | IPMI_MONITORING_SEL_FLAGS_ENTITY_SENSOR_NAMES        \
   | IPMI_MONITORING_SEL_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT)


#define IPMI_MONITORING_SENSOR_READING_FLAGS_MASK                          \
  (IPMI_MONITORING_SENSOR_READING_FLAGS_REREAD_SDR_CACHE                   \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_NON_INTERPRETABLE_SENSORS \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_BRIDGE_SENSORS                   \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_INTERPRET_OEM_DATA               \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS                   \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_DISCRETE_READING                 \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_SCANNING_DISABLED         \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_ASSUME_BMC_OWNER                 \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_ENTITY_SENSOR_NAMES              \
   | IPMI_MONITORING_SENSOR_READING_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT)

#define IPMI_MONITORING_AUTHENTICATION_TYPE_DEFAULT           IPMI_AUTHENTICATION_TYPE_MD5
#define IPMI_MONITORING_PRIVILEGE_LEVEL_DEFAULT               IPMI_PRIVILEGE_LEVEL_USER
#define IPMI_MONITORING_SESSION_TIMEOUT_LENGTH_DEFAULT        20000
#define IPMI_MONITORING_RETRANSMISSION_TIMEOUT_LENGTH_DEFAULT 500

#define IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH      32

#define IPMI_MONITORING_OEM_DATA_MAX                13

#define IPMI_MONITORING_MAGIC         0xABCD9876

#define IPMI_MONITORING_PACKET_BUFLEN 1024

struct ipmi_monitoring_sel_record {
  /* for all records */
  int record_id;
  int record_type;
  int record_type_class;
  int sel_state;

  /* for timestamped records */
  unsigned int timestamp;

  /* for system event records */
  int sensor_type;
  int sensor_number;
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH + 1];
  int event_direction;
  int event_offset_type;
  int event_offset;
  char *event_offset_string;
  int event_type_code;
  uint8_t event_data1;
  uint8_t event_data2;
  uint8_t event_data3;

  /* for oem timestamped records */
  int manufacturer_id;

  /* for oem timestamped & non-timestamped records */
  uint8_t oem_data[IPMI_MONITORING_OEM_DATA_MAX];
  unsigned int oem_data_len;
};

struct ipmi_monitoring_sensor_reading {
  int record_id;
  int sensor_number;
  int sensor_type;
  char sensor_name[IPMI_MONITORING_MAX_SENSOR_NAME_LENGTH + 1];
  int sensor_state;
  int sensor_units;
  int sensor_reading_type;
  int sensor_bitmask_type;
  int sensor_bitmask;
  char **sensor_bitmask_strings;
  union {
    uint8_t bool_val;
    uint32_t integer_val;
    double double_val;
  } sensor_reading;
  int event_reading_type_code;
};

struct ipmi_monitoring_ctx {
  uint32_t magic;
  int errnum;

  /* initialize this in ctx create */
  ipmi_interpret_ctx_t interpret_ctx;
  char sdr_cache_directory[MAXPATHLEN+1];
  int sdr_cache_directory_set;
  char sdr_cache_filename_format[MAXPATHLEN+1];
  int sdr_cache_filename_format_set;

  /* for use by both sel and sensor codepath */
  uint32_t manufacturer_id;
  uint16_t product_id;

  /* for use by both sel and sensor codepath */
  ipmi_sdr_ctx_t sdr_ctx;
  ipmi_ctx_t ipmi_ctx;
  Ipmi_Monitoring_Callback callback;
  void *callback_data;

  /* for sel codepath */
  ipmi_sel_ctx_t sel_parse_ctx;
  List sel_records;
  ListIterator sel_records_itr;
  struct ipmi_monitoring_sel_record *current_sel_record;
  struct ipmi_monitoring_sel_record *callback_sel_record;

  /* for sensor codepath */
  ipmi_sensor_read_ctx_t sensor_read_ctx;
  List sensor_readings;
  ListIterator sensor_readings_itr;
  struct ipmi_monitoring_sensor_reading *current_sensor_reading;
  struct ipmi_monitoring_sensor_reading *callback_sensor_reading;
};

#endif /* IPMI_MONITORING_DEFS_H */
