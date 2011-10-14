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

#ifndef _CONFIG_TOOL_UTIL_H_
#define _CONFIG_TOOL_UTIL_H_

#include <stdio.h>
#include <stdint.h>

#include "config-tool-common.h"
#include "pstdout.h"

/* note: keypair and section_str functions for argp parsing, no pstate involved */

int config_keypair_parse_string (const char *str,
                                 char **section_name,
                                 char **key_name,
                                 char **value);

int config_keypair_append (struct config_keypair **keypairs,
                           struct config_keypair *keypair);

/* no config_keypairs_create, responsibility of config tool to create list */

void config_keypairs_destroy (struct config_keypair *keypairs);

struct config_keypair *config_keypair_create (const char *section_name,
                                              const char *key_name,
                                              const char *value_pair);

void config_keypair_destroy (struct config_keypair *keypair);

/* no config_section_strs_create, responsibility of config tool to create list */

struct config_section_str *config_section_str_create (const char *section_name);

int config_section_str_append (struct config_section_str **section_strs,
                               struct config_section_str *section_str);

void config_section_str_destroy (struct config_section_str *section_str);

int config_ipv4_address_string2int (pstdout_state_t pstate,
                                    const char *src,
                                    uint32_t *dest);

int config_mac_address_string2int (pstdout_state_t pstate,
                                   const char *src,
                                   uint64_t *dest);

struct config_section *config_find_section (struct config_section *sections,
                                            const char *section_name);

struct config_key *config_find_key (struct config_section *section,
                                    const char *key_name);

struct config_keyvalue *config_find_keyvalue (struct config_section *section,
                                              const char *key_name);

int config_is_non_fatal_error (ipmi_ctx_t ipmi_ctx,
                               fiid_obj_t obj_cmd_rs,
                               config_err_t *non_fatal_err);

int config_is_config_param_non_fatal_error (ipmi_ctx_t ipmi_ctx,
                                            fiid_obj_t obj_cmd_rs,
                                            config_err_t *non_fatal_err);

#endif /* _CONFIG_TOOL_UTIL_H_ */
