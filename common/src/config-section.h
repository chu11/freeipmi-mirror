#ifndef _CONFIG_SECTION_H_
#define _CONFIG_SECTION_H_

#include "config-common.h"

/* no config_sections_create, responsibility of config tool to create list */

int config_section_append(struct config_section **sections,
                          struct config_section *section);

void config_sections_destroy (struct config_section *sections);

struct config_section *config_section_create (char *section_name,
                                              char *section_comment_section_name,
                                              char *section_comment,
                                              unsigned int flags);

void config_section_destroy (struct config_section *section);

int config_section_add_keyvalue (struct config_section *section,
                                 const char *key_name,
                                 const char *description,
                                 unsigned int flags,
                                 Key_Checkout checkout,
                                 Key_Commit commit,
                                 Key_Validate validate);

struct config_keyvalue *config_section_find_keyvalue (struct config_section *sections,
                                                      const char *section_name,
                                                      const char *key_name);

int config_section_set_value_input (struct config_section *sections,
                                    const char *section_name,
                                    const char *key_name,
                                    const char *value);

config_err_t config_section_commit_value (struct config_section *sections,
                                          const char *section_name,
                                          const char *key_name,
                                          const char *value,
                                          void *arg);

config_err_t config_output_sections_list (struct config_section *sections);

#endif /* _CONFIG_SECTION_H_ */
