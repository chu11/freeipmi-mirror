#ifndef _PEF_CONFIG_PARSER_H_
#define _PEF_CONFIG_PARSER_H_

#include "pef-config.h"
#include "pef-config-sections.h"

config_err_t pef_config_parser (struct config_section *sections,
                                struct config_arguments *cmd_args,
                                FILE *fp);

#endif /* _PEF_CONFIG_PARSER_H_ */
