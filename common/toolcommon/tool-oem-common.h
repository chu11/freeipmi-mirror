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

#ifndef TOOL_OEM_COMMON_H
#define TOOL_OEM_COMMON_H

#include <stdio.h>
#include <stdint.h>

#include <freeipmi/freeipmi.h>

#include "pstdout.h"

struct ipmi_oem_data
{
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint8_t ipmi_version_major;
  uint8_t ipmi_version_minor;
};

int ipmi_get_oem_data (pstdout_state_t pstate,
                       ipmi_ctx_t ipmi_ctx,
                       struct ipmi_oem_data *oem_data);

#endif /* TOOL_OEM_COMMON_H */
