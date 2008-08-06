/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifndef _CONFIG_TOOL_SECTION_H_
#define _CONFIG_TOOL_SECTION_H_

#include "config-tool-common.h"
#include "pstdout.h"

/* no config_sections_create, responsibility of config tool to create list */

int config_section_append(pstdout_state_t pstate,
                          struct config_section **sections,
                          struct config_section *section);

void config_sections_destroy (pstdout_state_t pstate,
                              struct config_section *sections);

struct config_section *config_section_create (pstdout_state_t pstate,
                                              char *section_name,
                                              char *section_comment_section_name,
                                              char *section_comment,
                                              unsigned int flags,
                                              Section_Pre_Commit section_pre_commit,
                                              Section_Post_Commit section_post_commit);

void config_section_destroy (pstdout_state_t pstate,
                             struct config_section *section);

int config_section_add_key (pstdout_state_t pstate,
                            struct config_section *section,
                            const char *key_name,
                            const char *description,
                            unsigned int flags,
                            Key_Checkout checkout,
                            Key_Commit commit,
                            Key_Validate validate);

int config_section_add_keyvalue(pstdout_state_t pstate,
                                struct config_section *section,
                                struct config_key *key,
                                const char *value_input,
                                const char *value_output);

int config_section_update_keyvalue_input(pstdout_state_t pstate,
                                         struct config_keyvalue *keyvalue,
                                         const char *value_input);

int config_section_update_keyvalue_output(pstdout_state_t pstate,
                                          struct config_keyvalue *keyvalue,
                                          const char *value_output);

int config_section_update_keyvalue_output_int(pstdout_state_t pstate,
                                              struct config_keyvalue *keyvalue,
                                              unsigned int value_output);

int config_section_update_keyvalue_output_double(pstdout_state_t pstate,
                                                 struct config_keyvalue *keyvalue,
                                                 double value_output);

/* returns -1 on error, number of non-valid values otherwise */
int config_sections_validate_keyvalue_inputs(pstdout_state_t pstate,
                                             struct config_section *sections,
                                             int value_input_required,
                                             void *arg);

/* returns -1 on error, 0 on success */
int config_sections_insert_keyvalues(pstdout_state_t pstate,
                                     struct config_section *sections,
                                     struct config_keypair *keypairs);

config_err_t config_output_sections_list (pstdout_state_t pstate,
                                          struct config_section *sections);

#endif /* _CONFIG_TOOL_SECTION_H_ */
