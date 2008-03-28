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

#ifndef _BMC_CONFIG_VALIDATE_H
#define _BMC_CONFIG_VALIDATE_H

#include "bmc-config.h"
#include "bmc-config-sections.h"

config_validate_t channel_access_mode_validate (const char *section_name, 
                                                const char *key_name, 
                                                const char *value,
                                                void *arg);

config_validate_t get_privilege_limit_number_validate (const char *section_name, 
                                                       const char *key_name, 
                                                       const char *value,
                                                       void *arg);

config_validate_t privilege_level_number_validate (const char *section_name, 
                                                   const char *key_name, 
                                                   const char *value,
                                                   void *arg);

config_validate_t rmcpplus_priv_number_validate (const char *section_name, 
                                                 const char *key_name, 
                                                 const char *value,
                                                 void *arg);

config_validate_t ip_address_source_number_validate (const char *section_name, 
                                                     const char *key_name, 
                                                     const char *value,
                                                     void *arg);

config_validate_t power_restore_policy_number_validate (const char *section_name, 
                                                        const char *key_name, 
                                                        const char *value,
                                                        void *arg);

config_validate_t connect_mode_number_validate (const char *section_name, 
                                                const char *key_name, 
                                                const char *value,
                                                void *arg);

config_validate_t flow_control_number_validate (const char *section_name, 
                                                const char *key_name, 
                                                const char *value,
                                                void *arg);

config_validate_t bit_rate_number_validate (const char *section_name, 
                                            const char *key_name, 
                                            const char *value,
                                            void *arg);

config_validate_t sol_bit_rate_number_validate (const char *section_name, 
                                                const char *key_name, 
                                                const char *value,
                                                void *arg);

config_validate_t alert_destination_type_number_validate (const char *section_name, 
                                                          const char *key_name, 
                                                          const char *value,
                                                          void *arg);

config_validate_t alert_gateway_number_validate (const char *section_name, 
                                                 const char *key_name, 
                                                 const char *value,
                                                 void *arg);

#endif
