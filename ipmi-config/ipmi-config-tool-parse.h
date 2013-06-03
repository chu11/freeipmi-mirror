/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

#ifndef IPMI_CONFIG_TOOL_PARSE_H
#define IPMI_CONFIG_TOOL_PARSE_H

#include "ipmi-config.h"
#include "pstdout.h"

ipmi_config_err_t ipmi_config_parse (pstdout_state_t pstate,
				     struct ipmi_config_section *sections,
				     struct ipmi_config_arguments *cmd_args,
				     FILE *fp);

#endif /* IPMI_CONFIG_TOOL_PARSE_H */
