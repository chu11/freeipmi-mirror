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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/


#ifndef _BMC_CONFIG_SECTIONS_H_
#define _BMC_CONFIG_SECTIONS_H_

#include "bmc-config.h"
#include "bmc-config-common.h"

#define BMC_CHECKOUT_KEY_COMMENTED_OUT                  0x01
#define BMC_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY   0x02
#define BMC_DO_NOT_CHECKOUT                             0x04

struct section {
  struct section *next;
  char *section_name;
  struct keyvalue *keyvalues;
};

/* checkout procedure fills the value into kv->value as printable string */
typedef bmc_err_t (*Keyvalue_Checkout) (bmc_config_state_data_t *state_data,
                                        const struct section *sect,
                                        struct keyvalue *kv);

/* commit procedure takes string value from kv->value and converts and
   does ipmi calls to set it */
typedef bmc_err_t (*Keyvalue_Commit) (bmc_config_state_data_t *state_data,
                                      const struct section *sect,
                                      const struct keyvalue *kv);

/* diff procedure finds the difference with the ipmi actual value
   and kv->value */
typedef bmc_diff_t (*Keyvalue_Diff) (bmc_config_state_data_t *state_data,
                                     const struct section *sect,
                                     const struct keyvalue *kv);

/* validate procedure finds if value is suitable to be set as kv->value */
typedef bmc_validate_t (*Keyvalue_Validate) (bmc_config_state_data_t *state_data,
                                             const struct section *sect,
                                             const char *value);

struct keyvalue {
  struct keyvalue *next;
  const char *key;
  const char *desc;
  unsigned int flags;
  char *value;
  Keyvalue_Checkout checkout;
  Keyvalue_Commit commit;
  Keyvalue_Diff diff;
  Keyvalue_Validate validate;
};

struct section * bmc_config_sections_list_create (bmc_config_state_data_t *state_data);

void bmc_config_sections_list_destroy (bmc_config_state_data_t *state_data,
                                       struct section *sections);

struct section * bmc_config_section_create (bmc_config_state_data_t *state_data, 
                                            char *section_name);

void bmc_config_section_destroy (bmc_config_state_data_t *state_data, 
                                 struct section *section);

int bmc_config_section_add_keyvalue (bmc_config_state_data_t *state_data,
                                     struct section *section,
                                     const char *key,
                                     const char *desc,
                                     unsigned int flags,
                                     Keyvalue_Checkout checkout,
                                     Keyvalue_Commit commit,
                                     Keyvalue_Diff diff,
                                     Keyvalue_Validate validate);

struct keyvalue * bmc_config_section_find_keyvalue (bmc_config_state_data_t *state_data,
                                                    const char *section_name,
                                                    const char *key_name);

int bmc_config_section_set_value (bmc_config_state_data_t *state_data,
                                  const char *section_name,
                                  const char *key_name,
                                  const char *value);

bmc_err_t bmc_config_section_commit_value (bmc_config_state_data_t *state_data,
                                           const char *section_name,
                                           const char *key_name,
                                           const char *value);

int bmc_config_section_diff_value (bmc_config_state_data_t *state_data,
                                   const char *section_name,
                                   const char *key_name,
                                   const char *value);

bmc_err_t bmc_config_sections_list (bmc_config_state_data_t *state_data);

#endif /* _BMC_CONFIG_SECTIONS_H_ */
