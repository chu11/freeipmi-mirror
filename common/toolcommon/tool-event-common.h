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

#ifndef TOOL_EVENT_COMMON_H
#define TOOL_EVENT_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "tool-oem-common.h"
#include "tool-sensor-common.h"
#include "pstdout.h"

#define EVENT_FMT_BUFLEN       4096
#define EVENT_OUTPUT_BUFLEN    4096
#define EVENT_NA_STRING        "N/A"
#define EVENT_OUTPUT_SEPARATOR " ; "

int event_load_event_state_config_file (pstdout_state_t pstate,
					ipmi_interpret_ctx_t interpret_ctx,
					const char *event_state_config_file);

/* All functions below
 * return 1 on success
 * return (0) on non-success, data ill-formed/invalid
 * return (-1) on error
 */

int event_output_time (pstdout_state_t pstate,
		       ipmi_sel_ctx_t sel_ctx,
		       uint8_t *sel_record,
		       unsigned int sel_record_len,
		       int comma_separated_output,
		       int debug,
		       unsigned int flags);
				       
int event_output_not_available_time (pstdout_state_t pstate,
				     int comma_separated_output);

int event_output_sensor_name (pstdout_state_t pstate,
			      ipmi_sel_ctx_t sel_ctx,
			      uint8_t *sel_record,
			      unsigned int sel_record_len,
			      struct sensor_column_width *column_width,
			      struct common_cmd_args *common_args,
			      int comma_separated_output,
			      unsigned int flags);

int event_output_not_available_sensor_name (pstdout_state_t pstate,
					    struct sensor_column_width *column_width,
					    int comma_separated_output);

int event_output_sensor_type (pstdout_state_t pstate,
			      ipmi_sel_ctx_t sel_ctx,
			      uint8_t *sel_record,
			      unsigned int sel_record_len,
			      struct sensor_column_width *column_width,
			      int comma_separated_output,
			      int debug,
			      unsigned int flags);

int event_output_not_available_sensor_type (pstdout_state_t pstate,
					    struct sensor_column_width *column_width,
					    int comma_separated_output);

int event_output_event_state (pstdout_state_t pstate,
			      ipmi_sel_ctx_t sel_ctx,
			      uint8_t *sel_record,
			      unsigned int sel_record_len,
			      int comma_separated_output,
			      int debug,
			      unsigned int flags);

int event_output_event_direction (pstdout_state_t pstate,
				  ipmi_sel_ctx_t sel_ctx,
				  uint8_t *sel_record,
				  unsigned int sel_record_len,
				  int comma_separated_output,
				  int debug,
				  unsigned int flags);

int event_output_not_available_event_direction (pstdout_state_t pstate,
						int comma_separated_output);

int event_output_event (pstdout_state_t pstate,
			ipmi_sel_ctx_t sel_ctx,
			uint8_t *sel_record,
			unsigned int sel_record_len,
			int comma_separated_output,
			int debug,
			unsigned int flags);

int event_output_not_available_event (pstdout_state_t pstate,
				      int comma_separated_output);

#endif /* TOOL_EVENT_COMMON_H */
