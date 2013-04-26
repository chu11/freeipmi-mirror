/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

#ifndef BMC_CONFIG_H
#define BMC_CONFIG_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

#include "config-tool-argp.h"
#include "config-tool-common.h"
#include "config-tool-comment.h"
#include "config-tool-checkout.h"
#include "config-tool-commit.h"
#include "config-tool-diff.h"
#include "config-tool-parse.h"
#include "config-tool-section.h"
#include "config-tool-utils.h"
#include "config-tool-validate.h"

#define CIPHER_SUITE_LEN 16

struct bmc_config_arguments
{
  struct config_arguments config_args;
};

typedef struct bmc_config_prog_data
{
  char *progname;
  struct bmc_config_arguments *args;
  int hosts_count;
} bmc_config_prog_data_t;

typedef struct bmc_config_enable_user_after_password
{
  int enable_user_failed;
  struct config_keyvalue *kv;
} bmc_config_enable_user_after_password_t;

typedef struct bmc_config_state_data
{
  bmc_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  struct config_section *sections;

  /* achu: workaround for OEM compliance issue, see user section */
  int enable_user_after_password_len;
  bmc_config_enable_user_after_password_t *enable_user_after_password;

  /* achu: caching to make lan authentication enables go faster */
  int authentication_type_initialized;
  uint8_t authentication_type_channel_number;
  uint8_t authentication_type_none;
  uint8_t authentication_type_md2;
  uint8_t authentication_type_md5;
  uint8_t authentication_type_straight_password;
  uint8_t authentication_type_oem_proprietary;

  /* achu: caching to make rmcpplus priv go faster */
  uint8_t cipher_suite_entry_count;
  int cipher_suite_entry_count_set;
  uint8_t cipher_suite_id_supported[CIPHER_SUITE_LEN];
  int cipher_suite_id_supported_set;
  uint8_t cipher_suite_priv[CIPHER_SUITE_LEN];
  int cipher_suite_priv_set;
  uint8_t cipher_suite_channel_number;

  /* For multi-channel settings
   *
   * base is for base section name (e.g. "Lan_Conf")
   * channel is for channel suffixed section name (e.g. "Lan_Conf_Channel_1")
   */
  unsigned int lan_base_config_flags;
  unsigned int lan_channel_config_flags;
  unsigned int serial_base_config_flags;
  unsigned int serial_channel_config_flags;
  unsigned int sol_base_config_flags;
  unsigned int sol_channel_config_flags;

  /* For channel reading */
  uint8_t lan_channel_numbers[IPMI_CHANNEL_NUMBERS_MAX];
  unsigned int lan_channel_numbers_count;
  unsigned int lan_channel_numbers_loaded;
  uint8_t serial_channel_numbers[IPMI_CHANNEL_NUMBERS_MAX];
  unsigned int serial_channel_numbers_count;
  unsigned int serial_channel_numbers_loaded;
  
  /* cache for multi-channel */
  uint8_t sol_channel_numbers_lan_channel[IPMI_CHANNEL_NUMBERS_MAX];
  uint8_t sol_channel_numbers_sol_channel[IPMI_CHANNEL_NUMBERS_MAX];
  unsigned int sol_channel_numbers_count;
  uint8_t sol_channel_numbers_unique[IPMI_CHANNEL_NUMBERS_MAX];
  unsigned int sol_channel_numbers_unique_count;
  unsigned int sol_channel_numbers_loaded;
} bmc_config_state_data_t;

#endif /* BMC_CONFIG_H */
