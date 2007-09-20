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

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-validate.h"

#include "config-common.h"

config_validate_t 
alert_destination_type_validate (const char *section_name,
                                 const char *key_name,
                                 const char *value)
{
  if (alert_destination_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
alert_gateway_validate (const char *section_name,
                        const char *key_name,
                        const char *value)
{
  if (alert_gateway_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t
policy_type_validate (const char *section_name,
                      const char *key_name,
                      const char *value)
{
  if (policy_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
filter_type_validate (const char *section_name,
                      const char *key_name,
                      const char *value)
{
  if (filter_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
event_severity_validate (const char *section_name,
                         const char *key_name,
                         const char *value)
{
  if (event_severity_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
sensor_type_validate (const char *section_name,
                      const char *key_name,
                      const char *value)
{
  if (sensor_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}
