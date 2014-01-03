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

#ifndef IPMI_INTERPRET_CONFIG_SEL_H
#define IPMI_INTERPRET_CONFIG_SEL_H

#include "freeipmi/interpret/ipmi-interpret.h"

#include "ipmi-interpret-defs.h"

int interpret_sel_init (ipmi_interpret_ctx_t ctx);

void interpret_sel_destroy (ipmi_interpret_ctx_t ctx);

int interpret_sel_config_parse (ipmi_interpret_ctx_t ctx,
				const char *sel_config_file);

#endif /* IPMI_INTERPRET_CONFIG_SEL_H */
