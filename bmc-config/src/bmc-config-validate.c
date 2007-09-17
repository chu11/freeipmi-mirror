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

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

bmc_validate_t 
yes_no_validate (bmc_config_state_data_t *state_data,
                 const struct section *sect,
                 const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
number_range_one_byte (bmc_config_state_data_t *state_data,
                       const struct section *sect,
                       const char *value)
{
  long int conv;
  char *endptr;

  conv = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (conv < 0 || conv > 255)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

bmc_validate_t 
number_range_one_byte_non_zero (bmc_config_state_data_t *state_data,
                                const struct section *sect,
                                const char *value)
{
  long int conv;
  char *endptr;

  conv = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (conv < 1 || conv > 255)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

bmc_validate_t
number_range_two_bytes (bmc_config_state_data_t *state_data,
                        const struct section *sect,
                        const char *value)
{
  long int conv;
  char *endptr;

  conv = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (conv < 0 || conv > 65535)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

bmc_validate_t
ip_address_validate (bmc_config_state_data_t *state_data,
                     const struct section *sect,
                     const char *value)
{
  struct in_addr a;

  if (inet_aton (value, &a))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t
mac_address_validate (bmc_config_state_data_t *state_data,
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
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
channel_access_mode_validate (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const char *value)
{
  if (channel_access_mode (value) >= 0)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
get_privilege_limit_number_validate (bmc_config_state_data_t *state_data,
                                     const struct section *sect,
                                     const char *value)
{
  if (get_privilege_limit_number (value) > 0)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
privilege_level_number_validate (bmc_config_state_data_t *state_data,
                                 const struct section *sect,
                                 const char *value)
{
  if (privilege_level_number (value) > 0)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
rmcpplus_priv_number_validate (bmc_config_state_data_t *state_data,
                               const struct section *sect,
                               const char *value)
{
  if (rmcpplus_priv_number (value) >= 0)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
ip_address_source_number_validate (bmc_config_state_data_t *state_data,
                                   const struct section *sect,
                                   const char *value)
{
  if (ip_address_source_number (value) >= 0)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
power_restore_policy_number_validate (bmc_config_state_data_t *state_data,
                                      const struct section *sect,
                                      const char *value)
{
  if (power_restore_policy_number (value) != -1)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
connect_mode_number_validate (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const char *value)
{
  if (connect_mode_number (value) != -1)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
flow_control_number_validate (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const char *value)
{
  if (flow_control_number (value) > -1)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
bit_rate_number_validate (bmc_config_state_data_t *state_data,
                          const struct section *sect,
                          const char *value)
{
  if (bit_rate_number (value) > -1)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
sol_bit_rate_number_validate (bmc_config_state_data_t *state_data,
                              const struct section *sect,
                              const char *value)
{
  if (sol_bit_rate_number (value) != -1)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
alert_destination_type_number_validate (bmc_config_state_data_t *state_data,
                                        const struct section *sect,
                                        const char *value)
{
  if (alert_destination_type_number (value) != -1)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

bmc_validate_t 
alert_gateway_number_validate (bmc_config_state_data_t *state_data,
                               const struct section *sect,
                               const char *value)
{
  if (alert_gateway_number (value) != -1)
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}
