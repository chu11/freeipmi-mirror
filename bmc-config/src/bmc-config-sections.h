/* 

bmc-config-sections.h

Copyright (C) 2006 FreeIPMI Core Team

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


#ifndef _BMC_CONFIG_SECTIONS_H_
#define _BMC_CONFIG_SECTIONS_H_

#include "bmc-config.h"
#include "bmc-config-common.h"

struct config_section * bmc_config_sections_list_create (bmc_config_state_data_t *state_data);

void bmc_config_sections_list_destroy (bmc_config_state_data_t *state_data,
                                       struct config_section *sections);

struct config_section * bmc_config_section_create (bmc_config_state_data_t *state_data, 
                                                   char *section_name,
                                                   char *section_comment_section_name,
                                                   char *section_comment,
                                                   unsigned int flags);

void bmc_config_section_destroy (bmc_config_state_data_t *state_data, 
                                 struct config_section *section);

int bmc_config_section_add_keyvalue (bmc_config_state_data_t *state_data,
                                     struct config_section *section,
                                     const char *key_name,
                                     const char *description,
                                     unsigned int flags,
                                     Key_Checkout checkout,
                                     Key_Commit commit,
                                     Key_Diff diff,
                                     Key_Validate validate);

struct config_keyvalue * bmc_config_section_find_keyvalue (bmc_config_state_data_t *state_data,
                                                           const char *section_name,
                                                           const char *key_name);

int bmc_config_section_set_value (bmc_config_state_data_t *state_data,
                                  const char *section_name,
                                  const char *key_name,
                                  const char *value);

config_err_t bmc_config_section_commit_value (bmc_config_state_data_t *state_data,
                                              const char *section_name,
                                              const char *key_name,
                                              const char *value);

int bmc_config_section_diff_value (bmc_config_state_data_t *state_data,
                                   const char *section_name,
                                   const char *key_name,
                                   const char *value);

config_err_t bmc_config_sections_list (bmc_config_state_data_t *state_data);

#endif /* _BMC_CONFIG_SECTIONS_H_ */
