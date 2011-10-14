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

#ifndef _CONFIG_TOOL_SECTION_H_
#define _CONFIG_TOOL_SECTION_H_

#include "config-tool-common.h"
#include "pstdout.h"

/* no config_sections_create, responsibility of config tool to create list */

int config_section_append (struct config_section **sections,
                           struct config_section *section);

void config_sections_destroy (struct config_section *sections);

struct config_section *config_section_create (pstdout_state_t pstate,
                                              const char *section_name,
                                              const char *section_comment_section_name,
                                              const char *section_comment,
                                              unsigned int flags,
                                              Section_Pre_Commit section_pre_commit,
                                              Section_Post_Commit section_post_commit);

/* -1 for channel index indicates do nothing, do same as config_section_create */
struct config_section *config_section_multi_channel_create (pstdout_state_t pstate,
							    const char *section_name_base_str,
							    const char *section_comment,
							    Section_Pre_Commit section_pre_commit,
							    Section_Post_Commit section_post_commit,
							    unsigned int config_flags,
							    int channel_index,
							    uint8_t *channel_numbers,
							    unsigned int channel_numbers_count);

void config_section_destroy (struct config_section *section);

int config_section_add_key (pstdout_state_t pstate,
                            struct config_section *section,
                            const char *key_name,
                            const char *description,
                            unsigned int flags,
                            Key_Checkout checkout,
                            Key_Commit commit,
                            Key_Validate validate);

/* -1 for channel index indicates do nothing, do same as config_section_add_key */
int config_section_multi_channel_add_key (pstdout_state_t pstate,
					  struct config_section *section,
					  const char *key_name_base_str,
					  const char *description,
					  unsigned int flags,
					  Key_Checkout checkout,
					  Key_Commit commit,
					  Key_Validate validate,
					  int channel_index,
					  uint8_t *channel_numbers,
					  unsigned int channel_numbers_count);

int config_section_add_keyvalue (pstdout_state_t pstate,
                                 struct config_section *section,
                                 struct config_key *key,
                                 const char *value_input,
                                 const char *value_output);

int config_section_update_keyvalue_input (pstdout_state_t pstate,
                                          struct config_keyvalue *keyvalue,
                                          const char *value_input);

int config_section_update_keyvalue_output (pstdout_state_t pstate,
                                           struct config_keyvalue *keyvalue,
                                           const char *value_output);

int config_section_update_keyvalue_output_unsigned_int (pstdout_state_t pstate,
                                                        struct config_keyvalue *keyvalue,
                                                        unsigned int value_output);

int config_section_update_keyvalue_output_hex (pstdout_state_t pstate,
                                               struct config_keyvalue *keyvalue,
                                               unsigned int value_output);

int config_section_update_keyvalue_output_double (pstdout_state_t pstate,
                                                  struct config_keyvalue *keyvalue,
                                                  double value_output);

/* returns -1 on error, number of non-valid values otherwise */
int config_sections_validate_keyvalue_inputs (pstdout_state_t pstate,
                                              struct config_section *sections,
                                              void *arg);

/* returns -1 on error, 0 on success */
int config_sections_insert_keyvalues (pstdout_state_t pstate,
                                      struct config_section *sections,
                                      struct config_keypair *keypairs);

config_err_t config_output_sections_list (pstdout_state_t pstate,
                                          struct config_section *sections);

#endif /* _CONFIG_TOOL_SECTION_H_ */
