#ifndef _IPMI_PEF_PARSER_H_
#define _IPMI_PEF_PARSER_H_

#include "ipmi-pef.h"
#include "ipmi-pef-sections.h"

pef_err_t ipmi_pef_parser (ipmi_pef_state_data_t *state_data, FILE *fp);

#endif /* _IPMI_PEF_PARSER_H_ */
