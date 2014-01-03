/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

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

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"

ipmi_config_validate_t
yes_no_validate (ipmi_config_state_data_t *state_data,
		 const char *section_name,
                 const char *key_name,
                 const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (!strcasecmp (value, "yes") || !strcasecmp (value, "no"))
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
check_number_range (const char *value,
                    int min,
                    int max)
{
  long int conv;
  char *endptr;

  assert (value);

  errno = 0;
  conv = strtol (value, &endptr, 0);

  if (errno
      || endptr[0] != '\0')
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);

  if (conv < min || conv > max)
    return (IPMI_CONFIG_VALIDATE_OUT_OF_RANGE_VALUE);

  return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
}

ipmi_config_validate_t
check_number_range_unsigned (const char *value,
			     unsigned int min,
			     unsigned int max)
{
  unsigned long conv;
  char *endptr;

  assert (value);

  errno = 0;
  conv = strtoul (value, &endptr, 0);

  if (errno
      || endptr[0] != '\0')
    return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);

  if (conv < min || conv > max)
    return (IPMI_CONFIG_VALIDATE_OUT_OF_RANGE_VALUE);

  return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
}

ipmi_config_validate_t
number_range_three_bits_validate (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  const char *key_name,
                                  const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, 7));
}

ipmi_config_validate_t
number_range_four_bits_validate (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const char *key_name,
                                 const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, 15));
}

ipmi_config_validate_t
number_range_seven_bits_validate (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  const char *key_name,
                                  const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, 127));
}

ipmi_config_validate_t
number_range_twelve_bits_validate (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   const char *key_name,
                                   const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, 4095));
}

ipmi_config_validate_t
number_range_one_byte_validate (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                const char *key_name,
                                const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, 255));
}

ipmi_config_validate_t
number_range_one_byte_non_zero_validate (ipmi_config_state_data_t *state_data,
					 const char *section_name,
                                         const char *key_name,
                                         const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 1, 255));
}

ipmi_config_validate_t
number_range_two_bytes_validate (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const char *key_name,
                                 const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range (value, 0, 65535));
}

ipmi_config_validate_t
number_range_four_bytes_validate (ipmi_config_state_data_t *state_data,
				  const char *section_name,
				  const char *key_name,
				  const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  return (check_number_range_unsigned (value, 0, 4294967295));
}

ipmi_config_validate_t
ip_address_validate (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     const char *key_name,
                     const char *value)
{
  struct in_addr a;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (inet_aton (value, &a))
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
mac_address_validate (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      const char *key_name,
                      const char *value)
{
  unsigned int foo;

  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (sscanf (value,
              "%02x:%02x:%02x:%02x:%02x:%02x",
              &foo,
              &foo,
              &foo,
              &foo,
              &foo,
              &foo) == 6)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
channel_access_mode_validate (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const char *key_name,
                              const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (channel_access_mode (value) >= 0)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
get_privilege_limit_number_validate (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     const char *key_name,
                                     const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (get_privilege_limit_number (value) > 0)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
privilege_level_number_validate (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const char *key_name,
                                 const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (privilege_level_number (value) > 0)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
rmcpplus_priv_number_validate (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const char *key_name,
                               const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (rmcpplus_priv_number (value) >= 0)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
ip_address_source_number_validate (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   const char *key_name,
                                   const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (ip_address_source_number (value) >= 0)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
power_restore_policy_number_validate (ipmi_config_state_data_t *state_data,
				      const char *section_name,
                                      const char *key_name,
                                      const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (power_restore_policy_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
connect_mode_number_validate (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const char *key_name,
                              const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (connect_mode_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
flow_control_number_validate (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const char *key_name,
                              const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (flow_control_number (value) > -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
bit_rate_number_validate (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const char *key_name,
                          const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (bit_rate_number (value) > -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
sol_bit_rate_number_validate (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const char *key_name,
                              const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (sol_bit_rate_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
alert_destination_type_number_validate (ipmi_config_state_data_t *state_data,
					const char *section_name,
                                        const char *key_name,
                                        const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (alert_destination_type_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
alert_gateway_number_validate (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const char *key_name,
                               const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (alert_gateway_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
bios_boot_type_number_validate (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                const char *key_name,
                                const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (bios_boot_type_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
boot_device_number_validate (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const char *key_name,
                             const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (boot_device_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
device_instance_selector_number_validate (ipmi_config_state_data_t *state_data,
					  const char *section_name,
                                          const char *key_name,
                                          const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (device_instance_selector_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
firmware_bios_verbosity_number_validate (ipmi_config_state_data_t *state_data,
					 const char *section_name,
                                         const char *key_name,
                                         const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (firmware_bios_verbosity_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
console_redirection_number_validate (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     const char *key_name,
                                     const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (console_redirection_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
alert_destination_type_validate (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const char *key_name,
                                 const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (alert_destination_type_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
alert_gateway_validate (ipmi_config_state_data_t *state_data,
			const char *section_name,
                        const char *key_name,
                        const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (alert_gateway_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
policy_type_validate (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      const char *key_name,
                      const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (policy_type_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
filter_type_validate (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      const char *key_name,
                      const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (filter_type_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
event_severity_validate (ipmi_config_state_data_t *state_data,
			 const char *section_name,
                         const char *key_name,
                         const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  if (event_severity_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);
  return (IPMI_CONFIG_VALIDATE_INVALID_VALUE);
}

ipmi_config_validate_t
sensor_type_validate (ipmi_config_state_data_t *state_data,
		      const char *section_name,
                      const char *key_name,
                      const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  /* can be string or hex code */

  if (sensor_type_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);

  return (number_range_one_byte_validate (state_data,
					  section_name,
					  key_name,
					  value));
}

ipmi_config_validate_t
exception_actions_validate (ipmi_config_state_data_t *state_data,
			    const char *section_name,
			    const char *key_name,
			    const char *value)
{
  assert (state_data);
  assert (section_name);
  assert (key_name);
  assert (value);

  /* can be string or hex code, hex code range limited */

  if (exception_actions_number (value) != -1)
    return (IPMI_CONFIG_VALIDATE_VALID_VALUE);

  return (check_number_range (value,
			      IPMI_DCMI_EXCEPTION_ACTIONS_MIN,
			      IPMI_DCMI_EXCEPTION_ACTIONS_MAX));
}
