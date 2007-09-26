#ifndef _CONFIG_UTIL_H_
#define _CONFIG_UTIL_H_

#include <stdio.h>

#include "config-common.h"

int config_keypair_parse_string(char *str,
                                char **section_name,
                                char **key_name,
                                char **value);

/* no config_keypairs_create, responsibility of config tool to create list */

int config_keypair_append(struct config_keypair **keypairs,
                          struct config_keypair *keypair);

void config_keypairs_destroy(struct config_keypair *keypairs);

struct config_keypair *config_keypair_create(const char *section_name,
                                             const char *key_name,
                                             const char *value_pair);

void config_keypair_destroy(struct config_keypair *keypair);

/* no config_section_strs_create, responsibility of config tool to create list */

struct config_section_str *config_section_str_create(char *section_name);

int config_section_str_append(struct config_section_str **section_strs,
                              struct config_section_str *section_str);

void config_section_str_destroy(struct config_section_str *section_str);

struct config_section *config_find_section(struct config_section *sections, 
                                           const char *section_name);

struct config_key *config_find_key(struct config_section *section, 
                                   const char *key_name);

struct config_keyvalue *config_find_keyvalue(struct config_section *section, 
                                             const char *key_name);

#endif /* _CONFIG_UTIL_H_ */
