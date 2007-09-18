#ifndef _CONFIG_UTIL_H_
#define _CONFIG_UTIL_H_

#include <stdio.h>

#include "config-common.h"

struct config_section *find_section(struct config_section *sections, const char *section_name);

struct config_key *find_key(struct config_section *section, const char *key_name);


#endif /* _CONFIG_UTIL_H_ */
