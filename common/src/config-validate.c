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

#include "config-validate.h"

config_validate_t 
config_yes_no_validate(const char *section_name, 
                       const char *key_name,
                       const char *value)
{
  assert(value);

  if (!strcasecmp(value, "yes") || !strcasecmp(value, "no"))
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
_number_range(const char *value,
              int min,
              int max)
{
  long int conv;
  char *endptr;

  assert(value);

  conv = strtol(value, &endptr, 0);

  if (*endptr)
    return CONFIG_VALIDATE_INVALID_VALUE;

  if (conv < min || conv > max)
    return CONFIG_VALIDATE_INVALID_VALUE;

  return CONFIG_VALIDATE_VALID_VALUE;
}

config_validate_t 
config_number_range_three_bits(const char *section_name, 
                               const char *key_name,
                               const char *value)
{
  assert(value);

  return _number_range(value, 0, 7);
}

config_validate_t 
config_number_range_four_bits(const char *section_name, 
                              const char *key_name,
                              const char *value)
{
  assert(value);

  return _number_range(value, 0, 15);
}

config_validate_t 
config_number_range_seven_bits(const char *section_name, 
                               const char *key_name,
                               const char *value)
{
  assert(value);

  return _number_range(value, 0, 127);
}

config_validate_t 
config_number_range_one_byte(const char *section_name, 
                             const char *key_name,
                             const char *value)
{
  assert(value);

  return _number_range(value, 0, 255);
}

config_validate_t 
config_number_range_one_byte_non_zero(const char *section_name, 
                                      const char *key_name,
                                      const char *value)
{
  assert(value);

  return _number_range(value, 1, 255);
}

config_validate_t 
config_number_range_two_bytes(const char *section_name, 
                              const char *key_name,
                              const char *value)
{
  assert(value);

  return _number_range(value, 0, 65535);
}

config_validate_t 
config_ip_address_validate(const char *section_name, 
                           const char *key_name,
                           const char *value)
{
  struct in_addr a;

  assert(value);

  if (inet_aton(value, &a))
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
config_mac_address_validate(const char *section_name, 
                            const char *key_name,
                            const char *value)
{
  unsigned int foo;

  assert(value);

  if (sscanf(value,
             "%02x:%02x:%02x:%02x:%02x:%02x",
             &foo,
             &foo,
             &foo,
             &foo,
             &foo,
             &foo) == 6)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

