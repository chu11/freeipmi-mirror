#ifndef _IPMI_PEF_VALIDATE_H
#define _IPMI_PEF_VALIDATE_H

#include "ipmi-pef.h"
#include "ipmi-pef-common.h"
#include "ipmi-pef-sections.h"

pef_validate_t yes_no_validate (ipmi_pef_state_data_t *state_data, 
                                const struct section *sect, 
                                const char *value);

pef_validate_t number_range_one_byte (ipmi_pef_state_data_t *state_data, 
                                      const struct section *sect, 
                                      const char *value);

pef_validate_t ip_address_validate (ipmi_pef_state_data_t *state_data,
                                    const struct section *sect,
                                    const char *value);

pef_validate_t mac_address_validate (ipmi_pef_state_data_t *state_data,
                                     const struct section *sect,
                                     const char *value);

pef_validate_t alert_destination_type_validate (ipmi_pef_state_data_t *state_data, 
                                                const struct section *sect, 
                                                const char *value);

pef_validate_t alert_gateway_validate (ipmi_pef_state_data_t *state_data, 
                                       const struct section *sect, 
                                       const char *value);

#endif
