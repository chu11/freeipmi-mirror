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

#ifndef _TOOL_CONFIG_FILE_COMMON_H
#define _TOOL_CONFIG_FILE_COMMON_H

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
#define CONFIG_FILE_TOOL_IPMI_FRU            0x00000040
#define CONFIG_FILE_TOOL_IPMI_OEM            0x00000080
#define CONFIG_FILE_TOOL_IPMI_RAW            0x00000100
#define CONFIG_FILE_TOOL_IPMI_SEL            0x00000200
#define CONFIG_FILE_TOOL_IPMI_SENSORS        0x00000400
#define CONFIG_FILE_TOOL_IPMI_SENSORS_CONFIG 0x00000800
#define CONFIG_FILE_TOOL_IPMICONSOLE         0x00001000
#define CONFIG_FILE_TOOL_IPMIMONITORING      0x00002000
#define CONFIG_FILE_TOOL_IPMIPOWER           0x00004000
#define CONFIG_FILE_TOOL_PEF_CONFIG          0x00008000

#define CONFIG_FILE_IPMI_SENSORS_MAX_GROUPS               256
#define CONFIG_FILE_IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH 256

#define CONFIG_FILE_IPMIMONITORING_MAX_GROUPS               256
#define CONFIG_FILE_IPMIMONITORING_MAX_GROUPS_STRING_LENGTH 256

struct config_file_data_bmc_watchdog
{
  char *logfile;
  int logfile_count;
  int no_logging;
  int no_logging_count;
};

struct config_file_data_ipmi_fru
{
  int skip_checks;
  int skip_checks_count;
};

struct config_file_data_ipmi_sensors
{
  int quiet_readings;
  int quiet_readings_count;
  char groups[CONFIG_FILE_IPMI_SENSORS_MAX_GROUPS][CONFIG_FILE_IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH+1];
  unsigned int groups_length;
  int groups_count;
  int bridge_sensors;
  int bridge_sensors_count;
};

struct config_file_data_ipmiconsole
{
  char escape_char;
  int escape_char_count;
  int dont_steal;
  int dont_steal_count;
  int lock_memory;
  int lock_memory_count;
};

struct config_file_data_ipmimonitoring
{
  int quiet_readings;
  int quiet_readings_count;
  char groups[CONFIG_FILE_IPMIMONITORING_MAX_GROUPS][CONFIG_FILE_IPMIMONITORING_MAX_GROUPS_STRING_LENGTH+1];
  unsigned int groups_length;
  int groups_count;
  int bridge_sensors;
  int bridge_sensors_count;
  char *sensor_config_file;
  int sensor_config_file_count;
};

struct config_file_data_ipmipower
{
  int on_if_off;
  int on_if_off_count;
  int wait_until_on;
  int wait_until_on_count;
  int wait_until_off;
  int wait_until_off_count;

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

int config_file_parse(const char *filename,
                      int no_error_if_not_found,
                      struct common_cmd_args *cmd_args, 
                      struct sdr_cmd_args *sdr_args, 
                      struct hostrange_cmd_args *hostrange_args, 
                      unsigned int support,
                      unsigned int tool_support,
                      void *tool_data);

#endif
