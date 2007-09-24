#ifndef _CONFIG_PARSE_H_
#define _CONFIG_PARSE_H_

#include "config-common.h"

config_err_t config_parse (struct config_section *sections, 
                           struct config_arguments *cmd_args,
                           FILE *fp);

#endif /* _CONFIG_PARSE_H_ */
