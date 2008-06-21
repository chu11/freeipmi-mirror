/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
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

#include "config-tool-validate.h"

#include "freeipmi-portability.h"

config_validate_t 
config_yes_no_validate(const char *section_name, 
                       const char *key_name,
                       const char *value,
                       void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  if (!strcasecmp(value, "yes") || !strcasecmp(value, "no"))
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
config_check_number_range(const char *value,
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
    return CONFIG_VALIDATE_OUT_OF_RANGE_VALUE;

  return CONFIG_VALIDATE_VALID_VALUE;
}

config_validate_t 
config_number_range_three_bits(const char *section_name, 
                               const char *key_name,
                               const char *value,
                               void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  return config_check_number_range(value, 0, 7);
}

config_validate_t 
config_number_range_four_bits(const char *section_name, 
                              const char *key_name,
                              const char *value,
                              void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  return config_check_number_range(value, 0, 15);
}

config_validate_t 
config_number_range_seven_bits(const char *section_name, 
                               const char *key_name,
                               const char *value,
                               void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  return config_check_number_range(value, 0, 127);
}

config_validate_t 
config_number_range_twelve_bits(const char *section_name, 
                                const char *key_name,
                                const char *value,
                                void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  return config_check_number_range(value, 0, 4095);
}

config_validate_t 
config_number_range_one_byte(const char *section_name, 
                             const char *key_name,
                             const char *value,
                             void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  return config_check_number_range(value, 0, 255);
}

config_validate_t 
config_number_range_one_byte_non_zero(const char *section_name, 
                                      const char *key_name,
                                      const char *value,
                                      void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  return config_check_number_range(value, 1, 255);
}

config_validate_t 
config_number_range_two_bytes(const char *section_name, 
                              const char *key_name,
                              const char *value,
                              void *arg)
{
  assert(section_name);
  assert(key_name);
  assert(value);

  return config_check_number_range(value, 0, 65535);
}

config_validate_t 
config_ip_address_validate(const char *section_name, 
                           const char *key_name,
                           const char *value,
                           void *arg)
{
  struct in_addr a;

  assert(section_name);
  assert(key_name);
  assert(value);

  if (inet_aton(value, &a))
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
config_mac_address_validate(const char *section_name, 
                            const char *key_name,
                            const char *value,
                            void *arg)
{
  unsigned int foo;

  assert(section_name);
  assert(key_name);
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


