/* 
   bmc-validate.h: map functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#ifndef _BMC_VALIDATE_H
#define _BMC_VALIDATE_H

#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-sections.h"

bmc_validate_t yes_no_validate (const struct bmc_config_arguments *args, 
                                const struct section *sect, 
                                const char *value);

bmc_validate_t number_range_one_byte (const struct bmc_config_arguments *args, 
                                      const struct section *sect, 
                                      const char *value);

bmc_validate_t ip_address_validate (const struct bmc_config_arguments *args,
                                    const struct section *sect,
                                    const char *value);

bmc_validate_t mac_address_validate (const struct bmc_config_arguments *args,
                                     const struct section *sect,
                                     const char *value);

bmc_validate_t channel_access_mode_validate (const struct bmc_config_arguments *args, 
                                             const struct section *sect, 
                                             const char *value);

bmc_validate_t get_privilege_limit_number_validate (const struct bmc_config_arguments *args, 
                                                    const struct section *sect, 
                                                    const char *value);

bmc_validate_t privilege_level_number_validate (const struct bmc_config_arguments *args, 
                                                const struct section *sect, 
                                                const char *value);

bmc_validate_t rmcpplus_priv_number_validate (const struct bmc_config_arguments *args, 
                                              const struct section *sect, 
                                              const char *value);

bmc_validate_t ip_address_source_number_validate (const struct bmc_config_arguments *args, 
                                                  const struct section *sect, 
                                                  const char *value);

bmc_validate_t power_restore_policy_number_validate (const struct bmc_config_arguments *args, 
                                                     const struct section *sect, 
                                                     const char *value);

bmc_validate_t connect_mode_number_validate (const struct bmc_config_arguments *args, 
                                             const struct section *sect, 
                                             const char *value);

bmc_validate_t flow_control_number_validate (const struct bmc_config_arguments *args, 
                                             const struct section *sect, 
                                             const char *value);

bmc_validate_t bit_rate_number_validate (const struct bmc_config_arguments *args, 
                                         const struct section *sect, 
                                         const char *value);

bmc_validate_t sol_bit_rate_number_validate (const struct bmc_config_arguments *args, 
                                             const struct section *sect, 
                                             const char *value);

#endif
