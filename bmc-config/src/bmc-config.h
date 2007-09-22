/* 

   bmc-config.h 

   Copyright (C) 2006 FreeIPMI Core Team

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


#ifndef _BMC_CONFIG_H_
#define _BMC_CONFIG_H_

#include <stdint.h>
#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "cmdline-parse-common.h"

#include "config-common.h"
#include "config-comment.h"
#include "config-util.h"
#include "config-validate.h"

#define CIPHER_SUITE_LEN 16

enum argp_option_keys
  { 
    CHECKOUT_KEY = 'o', 
    COMMIT_KEY = 'c', 
    DIFF_KEY = 'd',
    FILENAME_KEY = 'f',
    KEYPAIR_KEY = 'e',
    SECTIONS_KEY = 'S',
    LIST_SECTIONS_KEY = 'L',
    VERBOSE_KEY = 'v',
  };

typedef struct bmc_config_prog_data
{
  char *progname;
  struct config_arguments *args;
} bmc_config_prog_data_t;

typedef struct bmc_config_state_data
{
  bmc_config_prog_data_t *prog_data;
  ipmi_device_t dev;
  struct config_section *sections;

  /* achu: caching to make rmcpplus priv go faster */
  int cipher_suite_entry_count;
  int cipher_suite_id_supported[CIPHER_SUITE_LEN];
  int cipher_suite_id_supported_set;
  uint8_t cipher_suite_priv[CIPHER_SUITE_LEN];
  int cipher_suite_priv_set;

  /* achu: caching to make bmc-config work more quickly */
  int lan_channel_number_initialized;
  int8_t lan_channel_number;
  int serial_channel_number_initialized;
  int8_t serial_channel_number;
  int sol_channel_number_initialized;
  int8_t sol_channel_number;
  int number_of_users_initialized;
  int8_t number_of_users;
} bmc_config_state_data_t;

#endif /* _BMC_CONFIG_H_ */
