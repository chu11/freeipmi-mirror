/*
 * Copyright (C) 2008-2010 FreeIPMI Core Team
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

#ifndef _IPMI_OEM_QUANTA_H
#define _IPMI_OEM_QUANTA_H

#include "ipmi-oem.h"

#if 0
/* dangerous - truly nukes settings, including SDR */
int ipmi_oem_quanta_reset_to_defaults (ipmi_oem_state_data_t *state_data);
#endif

int ipmi_oem_quanta_get_processor_information (ipmi_oem_state_data_t *state_data);

#endif
