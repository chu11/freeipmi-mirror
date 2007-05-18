#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <errno.h>
#include <assert.h>

#include "ipmi-pef.h"
#include "ipmi-pef-map.h"
#include "ipmi-pef-validate.h"

pef_validate_t 
yes_no_validate (ipmi_pef_state_data_t *state_data,
                 const struct section *sect,
                 const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return PEF_VALIDATE_VALID_VALUE;
  return PEF_VALIDATE_INVALID_VALUE;
}

pef_validate_t 
number_range_one_byte (ipmi_pef_state_data_t *state_data,
                       const struct section *sect,
                       const char *value)
{
  long int conv;
  char *endptr;

  conv = strtol (value, &endptr, 0);

  if (*endptr)
    return PEF_VALIDATE_INVALID_VALUE;

  if (conv < 0 || conv > 255)
    return PEF_VALIDATE_INVALID_VALUE;

  return PEF_VALIDATE_VALID_VALUE;
}

pef_validate_t
ip_address_validate (ipmi_pef_state_data_t *state_data,
                     const struct section *sect,
                     const char *value)
{
  struct in_addr a;

  if (inet_aton (value, &a))
    return PEF_VALIDATE_VALID_VALUE;
  return PEF_VALIDATE_INVALID_VALUE;
}

pef_validate_t
mac_address_validate (ipmi_pef_state_data_t *state_data,
                      const struct section *sect,
                      const char *value)
{
  unsigned int foo;

  if (sscanf (value,
              "%02x:%02x:%02x:%02x:%02x:%02x",
              &foo,
              &foo,
              &foo,
              &foo,
              &foo,
              &foo) == 6)
    return PEF_VALIDATE_VALID_VALUE;
  return PEF_VALIDATE_INVALID_VALUE;
}

pef_validate_t 
alert_destination_type_validate (ipmi_pef_state_data_t *state_data,
                                 const struct section *sect,
                                 const char *value)
{
  if (alert_destination_type_number (value) != -1)
    return PEF_VALIDATE_VALID_VALUE;
  return PEF_VALIDATE_INVALID_VALUE;
}

pef_validate_t 
alert_gateway_validate (ipmi_pef_state_data_t *state_data,
                        const struct section *sect,
                        const char *value)
{
  if (alert_gateway_number (value) != -1)
    return PEF_VALIDATE_VALID_VALUE;
  return PEF_VALIDATE_INVALID_VALUE;
}
