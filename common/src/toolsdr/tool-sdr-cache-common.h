/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
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

#ifndef _TOOL_SDR_CACHE_COMMON_H
#define _TOOL_SDR_CACHE_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include <freeipmi/freeipmi.h>

#include "pstdout.h"

#define IPMI_SDR_CACHE_MAX_ID_STRING         16
#define IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING  16

int sdr_cache_create_and_load (ipmi_sdr_cache_ctx_t ctx,
                               pstdout_state_t pstate,
                               ipmi_ctx_t ipmi_ctx,
                               int quiet_cache,
                               int sdr_cache_recreate,
                               const char *hostname,
                               const char *cache_dir,
			       const char *cache_file);

int sdr_cache_flush_cache (ipmi_sdr_cache_ctx_t ctx,
                           pstdout_state_t pstate,
                           int quiet_cache,
                           const char *hostname,
                           const char *cache_dir,
			   const char *cache_file);

#endif
