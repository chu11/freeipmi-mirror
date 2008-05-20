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


#ifndef _BMC_CONFIG_H_
#define _BMC_CONFIG_H_

#include <stdint.h>
#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"

#include "config-argp.h"
#include "config-common.h"
#include "config-comment.h"
#include "config-checkout.h"
#include "config-commit.h"
#include "config-diff.h"
#include "config-fiid.h"
#include "config-parse.h"
#include "config-section.h"
#include "config-utils.h"
#include "config-validate.h"

#define CIPHER_SUITE_LEN 16

struct bmc_config_arguments
{
  struct config_arguments config_args;
};

typedef struct bmc_config_prog_data
{
  char *progname;
  struct bmc_config_arguments *args;
} bmc_config_prog_data_t;

typedef struct bmc_config_state_data
{
  bmc_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;

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
