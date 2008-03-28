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

#ifndef _PEF_CONFIG_VALIDATE_H
#define _PEF_CONFIG_VALIDATE_H

#include "pef-config.h"

config_validate_t alert_destination_type_validate (const char *section_name, 
                                                   const char *key_name, 
                                                   const char *value,
                                                   void *arg);

config_validate_t alert_gateway_validate (const char *section_name, 
                                          const char *key_name, 
                                          const char *value,
                                          void *arg);

config_validate_t policy_type_validate (const char *section_name,
                                        const char *key_name,
                                        const char *value,
                                        void *arg);

config_validate_t filter_type_validate (const char *section_name,
                                        const char *key_name,
                                        const char *value,
                                        void *arg);

config_validate_t event_severity_validate (const char *section_name,
                                           const char *key_name,
                                           const char *value,
                                           void *arg);

config_validate_t sensor_type_validate (const char *section_name,
                                        const char *key_name,
                                        const char *value,
                                        void *arg);

#endif
