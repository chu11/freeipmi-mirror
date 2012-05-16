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

#ifndef TOOL_SDR_CACHE_COMMON_H
#define TOOL_SDR_CACHE_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

#define IPMI_SDR_CACHE_MAX_ID_STRING         16
#define IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING  16

int sdr_cache_create_and_load (ipmi_sdr_ctx_t ctx,
                               pstdout_state_t pstate,
                               ipmi_ctx_t ipmi_ctx,
                               const char *hostname,
			       const struct sdr_cmd_args *sdr_args);

int sdr_cache_flush_cache (pstdout_state_t pstate,
                           const char *hostname,
			   const struct sdr_cmd_args *sdr_args);

int sdr_cache_setup_debug (ipmi_sdr_ctx_t ctx,
			   pstdout_state_t pstate,
			   int debug,
			   const char *hostname);

/* wrapper for ipmi_sdr_cache_search_sensor, handles some additional special workarounds */
int ipmi_sdr_cache_search_sensor_wrapper (ipmi_sdr_ctx_t ctx,
					  uint8_t sensor_number,
					  uint8_t generator_id);

#endif /* TOOL_SDR_CACHE_COMMON_H */
