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

#ifndef IPMI_CONFIG_TOOL_UTIL_H
#define IPMI_CONFIG_TOOL_UTIL_H

#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

#include "ipmi-config.h"
#include "pstdout.h"

/* note: keypair and section_str functions for argp parsing, no pstate involved */

int ipmi_config_keypair_parse_string (const char *str,
				      char **section_name,
				      char **key_name,
				      char **value);

int ipmi_config_keypair_append (struct ipmi_config_keypair **keypairs,
				struct ipmi_config_keypair *keypair);

/* XXX */
/* no config_keypairs_create, responsibility of config tool to create list */

void ipmi_config_keypairs_destroy (struct ipmi_config_keypair *keypairs);

struct ipmi_config_keypair *ipmi_config_keypair_create (const char *section_name,
							const char *key_name,
							const char *value_pair);

void ipmi_config_keypair_destroy (struct ipmi_config_keypair *keypair);

/* XXX */
/* no config_section_strs_create, responsibility of ipmi_config tool to create list */

struct ipmi_config_section_str *ipmi_config_section_str_create (const char *section_name);

int ipmi_config_section_str_append (struct ipmi_config_section_str **section_strs,
				    struct ipmi_config_section_str *section_str);

void ipmi_config_section_str_destroy (struct ipmi_config_section_str *section_str);

int ipmi_config_ipv4_address_string2int (pstdout_state_t pstate,
					 const char *src,
					 uint32_t *dest);

int ipmi_config_mac_address_string2int (pstdout_state_t pstate,
					const char *src,
					uint64_t *dest);

struct ipmi_config_section *ipmi_config_find_section (struct ipmi_config_section *sections,
						      const char *section_name);

struct ipmi_config_key *ipmi_config_find_key (struct ipmi_config_section *section,
					      const char *key_name);

struct ipmi_config_keyvalue *ipmi_config_find_keyvalue (struct ipmi_config_section *section,
							const char *key_name);

int ipmi_config_is_non_fatal_error (ipmi_ctx_t ipmi_ctx,
				    fiid_obj_t obj_cmd_rs,
				    ipmi_config_err_t *non_fatal_err);

int ipmi_config_is_config_param_non_fatal_error (ipmi_ctx_t ipmi_ctx,
						 fiid_obj_t obj_cmd_rs,
						 ipmi_config_err_t *non_fatal_err);

int ipmi_config_pstdout_fprintf (pstdout_state_t pstate, FILE *stream, const char *format, ...);

#endif /* IPMI_CONFIG_TOOL_UTIL_H */
