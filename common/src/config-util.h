#ifndef _CONFIG_UTIL_H_
#define _CONFIG_UTIL_H_

#include <stdio.h>

#include "config-common.h"

struct config_section *config_find_section(struct config_section *sections, 
                                           const char *section_name);

struct config_key *config_find_key(struct config_section *section, 
                                   const char *key_name);

struct config_keyvalue *config_find_keyvalue(struct config_section *section, 
                                             const char *key_name);

#endif /* _CONFIG_UTIL_H_ */
