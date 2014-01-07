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

#ifndef IPMI_CONFIG_SECTION_H
#define IPMI_CONFIG_SECTION_H

#include "ipmi-config.h"

/* XXX */
/* no ipmi_config_sections_create, responsibility of config tool to create list */

int ipmi_config_section_append (struct ipmi_config_section **sections,
                                struct ipmi_config_section *section);

void ipmi_config_sections_destroy (struct ipmi_config_section *sections);

struct ipmi_config_section *ipmi_config_section_create (ipmi_config_state_data_t *state_data,
                                                        const char *section_name,
                                                        const char *section_comment_section_name,
                                                        const char *section_comment,
                                                        unsigned int flags,
                                                        Section_Pre_Commit section_pre_commit,
                                                        Section_Post_Commit section_post_commit);

/* -1 for channel index indicates do nothing, do same as ipmi_config_section_create */
struct ipmi_config_section *ipmi_config_section_multi_channel_create (ipmi_config_state_data_t *state_data,
                                                                      const char *section_name_base_str,
                                                                      const char *section_comment,
                                                                      Section_Pre_Commit section_pre_commit,
                                                                      Section_Post_Commit section_post_commit,
                                                                      unsigned int config_flags,
                                                                      int channel_index,
                                                                      uint8_t *channel_numbers,
                                                                      unsigned int channel_numbers_count);

void ipmi_config_section_destroy (struct ipmi_config_section *section);

/* Why not set in ipmi_config_section_create?
 *
 * Some sections may cross categories, b/c of a legacy.  Must be done
 * after all sections for a category are created.
 */
int ipmi_config_set_category (struct ipmi_config_section *sections, unsigned int category);

int ipmi_config_set_line_length (struct ipmi_config_section *sections, unsigned int line_length);

int ipmi_config_section_add_key (ipmi_config_state_data_t *state_data,
                                 struct ipmi_config_section *section,
                                 const char *key_name,
                                 const char *description,
                                 unsigned int flags,
                                 Key_Checkout checkout,
                                 Key_Commit commit,
                                 Key_Validate validate);

/* -1 for channel index indicates do nothing, do same as ipmi_config_section_add_key */
int ipmi_config_section_multi_channel_add_key (ipmi_config_state_data_t *state_data,
                                               struct ipmi_config_section *section,
                                               const char *key_name_base_str,
                                               const char *description,
                                               unsigned int flags,
                                               Key_Checkout checkout,
                                               Key_Commit commit,
                                               Key_Validate validate,
                                               int channel_index,
                                               uint8_t *channel_numbers,
                                               unsigned int channel_numbers_count);

int ipmi_config_section_add_keyvalue (ipmi_config_state_data_t *state_data,
                                      struct ipmi_config_section *section,
                                      struct ipmi_config_key *key,
                                      const char *value_input,
                                      const char *value_output);

int ipmi_config_section_update_keyvalue_input (ipmi_config_state_data_t *state_data,
                                               struct ipmi_config_keyvalue *keyvalue,
                                               const char *value_input);

int ipmi_config_section_update_keyvalue_output (ipmi_config_state_data_t *state_data,
                                                struct ipmi_config_keyvalue *keyvalue,
                                                const char *value_output);

int ipmi_config_section_update_keyvalue_output_unsigned_int (ipmi_config_state_data_t *state_data,
                                                             struct ipmi_config_keyvalue *keyvalue,
                                                             unsigned int value_output);

int ipmi_config_section_update_keyvalue_output_hex (ipmi_config_state_data_t *state_data,
                                                    struct ipmi_config_keyvalue *keyvalue,
                                                    unsigned int value_output);

int ipmi_config_section_update_keyvalue_output_double (ipmi_config_state_data_t *state_data,
                                                       struct ipmi_config_keyvalue *keyvalue,
                                                       double value_output);

/* returns -1 on error, number of non-valid values otherwise */
int ipmi_config_sections_validate_keyvalue_inputs (ipmi_config_state_data_t *state_data);

/* returns -1 on error, 0 on success */
int ipmi_config_sections_insert_keyvalues (ipmi_config_state_data_t *state_data,
                                           struct ipmi_config_keypair *keypairs);

ipmi_config_err_t ipmi_config_output_sections_list (ipmi_config_state_data_t *state_data);

#endif /* IPMI_CONFIG_SECTION_H */
