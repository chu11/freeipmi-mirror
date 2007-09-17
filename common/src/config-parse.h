#ifndef _CONFIG_PARSE_H_
#define _CONFIG_PARSE_H_

#include <stdio.h>

#include "config-common.h"

/* returns 0 on success, -1 on error */
int config_parse (struct config_section *sections, FILE *fp, int debug);

#endif /* _CONFIG_PARSE_H_ */
