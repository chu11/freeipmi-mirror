/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
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

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-validate.h"

#include "freeipmi-portability.h"

config_validate_t 
alert_destination_type_validate (const char *section_name,
                                 const char *key_name,
                                 const char *value,
                                 void *arg)
{
  if (alert_destination_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
alert_gateway_validate (const char *section_name,
                        const char *key_name,
                        const char *value,
                        void *arg)
{
  if (alert_gateway_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t
policy_type_validate (const char *section_name,
                      const char *key_name,
                      const char *value,
                      void *arg)
{
  if (policy_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
filter_type_validate (const char *section_name,
                      const char *key_name,
                      const char *value,
                      void *arg)
{
  if (filter_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
event_severity_validate (const char *section_name,
                         const char *key_name,
                         const char *value,
                         void *arg)
{
  if (event_severity_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  return CONFIG_VALIDATE_INVALID_VALUE;
}

config_validate_t 
sensor_type_validate (const char *section_name,
                      const char *key_name,
                      const char *value,
                      void *arg)
{
  /* can be string or hex code */
  
  if (sensor_type_number (value) != -1)
    return CONFIG_VALIDATE_VALID_VALUE;
  
  return (config_number_range_one_byte (section_name,
                                        key_name,
                                        value,
                                        arg));
}
