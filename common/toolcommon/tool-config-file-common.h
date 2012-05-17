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

#ifndef TOOL_CONFIG_FILE_COMMON_H
#define TOOL_CONFIG_FILE_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "tool-cmdline-common.h"
#include "conffile.h"

#define CONFIG_FILE_NONE        0x00
#define CONFIG_FILE_INBAND      0x01
#define CONFIG_FILE_OUTOFBAND   0x02
#define CONFIG_FILE_SDR         0x04
#define CONFIG_FILE_HOSTRANGE   0x08

#define CONFIG_FILE_TOOL_NONE                0x00000000
#define CONFIG_FILE_TOOL_BMC_CONFIG          0x00000001
#define CONFIG_FILE_TOOL_BMC_DEVICE          0x00000002
#define CONFIG_FILE_TOOL_BMC_INFO            0x00000004
#define CONFIG_FILE_TOOL_BMC_WATCHDOG        0x00000008
#define CONFIG_FILE_TOOL_IPMI_CHASSIS        0x00000010
#define CONFIG_FILE_TOOL_IPMI_CHASSIS_CONFIG 0x00000020
#define CONFIG_FILE_TOOL_IPMI_DCMI           0x00000040
#define CONFIG_FILE_TOOL_IPMI_FRU            0x00000080
#define CONFIG_FILE_TOOL_IPMI_OEM            0x00000100
#define CONFIG_FILE_TOOL_IPMI_PEF_CONFIG     0x00000200
#define CONFIG_FILE_TOOL_IPMI_PET            0x00000400
#define CONFIG_FILE_TOOL_IPMI_RAW            0x00000800
#define CONFIG_FILE_TOOL_IPMI_SEL            0x00001000
#define CONFIG_FILE_TOOL_IPMI_SENSORS        0x00002000
#define CONFIG_FILE_TOOL_IPMI_SENSORS_CONFIG 0x00004000
#define CONFIG_FILE_TOOL_IPMICONSOLE         0x00008000
#define CONFIG_FILE_TOOL_IPMIPOWER           0x00010000

/* achu:
 *
 * The stack on cygwin is smaller than on unixes, and the ability for
 * me to get a bigger stack in cygwin is difficult.  The normal unix
 * ways (i.e. ulimit -s unlimited) don't work.  Some posts online
 * indicate it's b/c of the way windows works with stack limits.
 *
 * So we're just going to use smaller values to deal with the problem.
 */
#ifdef __CYGWIN__
#define CONFIG_FILE_MAX_SENSOR_RECORD_IDS           128
#define CONFIG_FILE_MAX_SENSOR_TYPES                16
#else /* !__CYGWIN__ */
/* record id is 16 bits - 65536 */
#define CONFIG_FILE_MAX_SENSOR_RECORD_IDS           65536
#define CONFIG_FILE_MAX_SENSOR_TYPES                256
#endif /* !__CYGWIN__ */
#define CONFIG_FILE_MAX_SENSOR_TYPES_STRING_LENGTH  256

struct config_file_data_bmc_config
{
  int verbose_count;
  int verbose_count_count;
};

struct config_file_data_bmc_info
{
  int interpret_oem_data;
  int interpret_oem_data_count;
};

struct config_file_data_bmc_watchdog
{
  char *logfile;		/* deprecated */
  int logfile_count;		/* deprecated */
  int verbose_logging;
  int verbose_logging_count;
  int no_logging;
  int no_logging_count;
};

struct config_file_data_ipmi_chassis_config
{
  int verbose_count;
  int verbose_count_count;
};

struct config_file_data_ipmi_dcmi
{
  int interpret_oem_data;
  int interpret_oem_data_count;
};

struct config_file_data_ipmi_fru
{
  int verbose_count;
  int verbose_count_count;
  int skip_checks;
  int skip_checks_count;
  int bridge_fru;
  int bridge_fru_count;
  int interpret_oem_data;
  int interpret_oem_data_count;
};

struct config_file_data_ipmi_oem
{
  int verbose_count;
  int verbose_count_count;
};

struct config_file_data_ipmi_pef_config
{
  int verbose_count;
  int verbose_count_count;
};

struct config_file_data_ipmi_pet
{
  int verbose_count;
  int verbose_count_count;
  int output_event_severity;
  int output_event_severity_count;
  int output_event_state;
  int output_event_state_count;
  char *event_state_config_file;
  int event_state_config_file_count;
  int interpret_oem_data;
  int interpret_oem_data_count;
  int entity_sensor_names;
  int entity_sensor_names_count;
  int no_sensor_type_output;
  int no_sensor_type_output_count;
  int comma_separated_output;
  int comma_separated_output_count;
  int no_header_output;
  int no_header_output_count;
  int non_abbreviated_units;
  int non_abbreviated_units_count;
};

struct config_file_data_ipmi_sel
{
  int verbose_count;
  int verbose_count_count;
  char sensor_types[CONFIG_FILE_MAX_SENSOR_TYPES][CONFIG_FILE_MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int sensor_types_length;
  int sensor_types_count;
  char exclude_sensor_types[CONFIG_FILE_MAX_SENSOR_TYPES][CONFIG_FILE_MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int exclude_sensor_types_length;
  int exclude_sensor_types_count;
  int system_event_only;
  int system_event_only_count;
  int oem_event_only;
  int oem_event_only_count;
  int output_manufacturer_id;
  int output_manufacturer_id_count;
  int output_event_state;
  int output_event_state_count;
  char *event_state_config_file;
  int event_state_config_file_count;
  int assume_system_event_records;
  int assume_system_event_records_count;
  int interpret_oem_data;
  int interpret_oem_data_count;
  int output_oem_event_strings;
  int output_oem_event_strings_count;
  int entity_sensor_names;
  int entity_sensor_names_count;
  int no_sensor_type_output;
  int no_sensor_type_output_count;
  int comma_separated_output;
  int comma_separated_output_count;
  int no_header_output;
  int no_header_output_count;
  int non_abbreviated_units;
  int non_abbreviated_units_count;
  int legacy_output;
  int legacy_output_count;
};

struct config_file_data_ipmi_sensors
{
  int verbose_count;
  int verbose_count_count;
  int quiet_readings;
  int quiet_readings_count;
  unsigned int record_ids[CONFIG_FILE_MAX_SENSOR_RECORD_IDS];
  unsigned int record_ids_length;
  int record_ids_count;
  unsigned int exclude_record_ids[CONFIG_FILE_MAX_SENSOR_RECORD_IDS];
  unsigned int exclude_record_ids_length;
  int exclude_record_ids_count;
  char sensor_types[CONFIG_FILE_MAX_SENSOR_TYPES][CONFIG_FILE_MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int sensor_types_length;
  int sensor_types_count;
  char exclude_sensor_types[CONFIG_FILE_MAX_SENSOR_TYPES][CONFIG_FILE_MAX_SENSOR_TYPES_STRING_LENGTH+1];
  unsigned int exclude_sensor_types_length;
  int exclude_sensor_types_count;
  int bridge_sensors;
  int bridge_sensors_count;
  int shared_sensors;
  int shared_sensors_count;
  int interpret_oem_data;
  int interpret_oem_data_count;
  int ignore_not_available_sensors;
  int ignore_not_available_sensors_count;
  int ignore_unrecognized_events;
  int ignore_unrecognized_events_count;
  int entity_sensor_names;
  int entity_sensor_names_count;
  int output_event_bitmask;
  int output_event_bitmask_count;
  int output_sensor_state;
  int output_sensor_state_count;
  char *sensor_state_config_file;
  int sensor_state_config_file_count;
  int no_sensor_type_output;
  int no_sensor_type_output_count;
  int comma_separated_output_count;
  int comma_separated_output;
  int no_header_output;
  int no_header_output_count;
  int non_abbreviated_units;
  int non_abbreviated_units_count;
  int legacy_output;
  int legacy_output_count;
  int ipmimonitoring_legacy_output;
  int ipmimonitoring_legacy_output_count;
};

struct config_file_data_ipmi_sensors_config
{
  int verbose_count;
  int verbose_count_count;
};

struct config_file_data_ipmiconsole
{
  char escape_char;
  int escape_char_count;
  int dont_steal;
  int dont_steal_count;
  int serial_keepalive;
  int serial_keepalive_count;
  int serial_keepalive_empty;
  int serial_keepalive_empty_count;
  int lock_memory;
  int lock_memory_count;
};

struct config_file_data_ipmipower
{
  int on_if_off;
  int on_if_off_count;
  int wait_until_on;
  int wait_until_on_count;
  int wait_until_off;
  int wait_until_off_count;
  /* Parse string and let ipmipower determine if it is valid */
  char *oem_power_type_str;
  int oem_power_type_str_count;

  unsigned int retransmission_wait_timeout;
  int retransmission_wait_timeout_count;
  unsigned int retransmission_backoff_count;
  int retransmission_backoff_count_count;
  unsigned int ping_interval;
  int ping_interval_count;
  unsigned int ping_timeout;
  int ping_timeout_count;
  unsigned int ping_packet_count;
  int ping_packet_count_count;
  unsigned int ping_percent;
  int ping_percent_count;
  unsigned int ping_consec_count;
  int ping_consec_count_count;
};

int config_file_parse (const char *filename,
                       int no_error_if_not_found,
                       struct common_cmd_args *cmd_args,
                       unsigned int support,
                       unsigned int tool_support,
                       void *tool_data);

#endif /* TOOL_CONFIG_FILE_COMMON_H */
