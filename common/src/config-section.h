#ifndef _CONFIG_SECTION_H_
#define _CONFIG_SECTION_H_

#include <stdio.h>

#include "config-common.h"

/* no config_setions_create, responsibility of config tool to create list */

int config_section_append(struct config_section **sections, 
                          struct config_section *section);

void config_sections_destroy(struct config_section *sections);

struct config_section *config_section_create(const char *section_name,
                                             const char *section_comment_section_name,
                                             const char *section_comment,
                                             unsigned int flags,
                                             Section_Checkout checkout,
                                             Section_Commit commit);

void config_section_destroy(struct config_section *section);

int config_section_add_key(struct config_section *section,
                           const char *key_name,
                           const char *description,
                           unsigned int flags,
                           Key_Validate validate);

int config_section_add_keyvalue(struct config_section *section,
                                struct config_key *key,
                                const char *value_input,
                                const char *value_output);

int config_section_update_keyvalue(struct config_keyvalue *keyvalue,
                                   const char *value_input,
                                   const char *value_output);

/* returns -1 on error, number of non-valid values otherwise */
int config_sections_validate_keyvalue_inputs(struct config_section *sections,
                                             int value_input_required,
                                             int debug,
                                             void *arg);

/* returns -1 on error, 0 on success */
int config_sections_insert_keyvalues(struct config_section *sections,
                                     struct config_keyinput *keyinputs,
                                     int debug);

config_err_t config_sections_output_list(struct config_section *sections);

#endif /* _CONFIG_SECTION_H_ */
