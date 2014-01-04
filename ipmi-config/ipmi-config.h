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

#ifndef IPMI_CONFIG_H
#define IPMI_CONFIG_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

#define IPMI_CONFIG_CIPHER_SUITE_LEN 16

enum ipmi_config_argp_option_keys
  {
    IPMI_CONFIG_ARGP_CATEGORY_KEY = 'g',
    IPMI_CONFIG_ARGP_CHECKOUT_KEY = 'o',
    IPMI_CONFIG_ARGP_COMMIT_KEY = 'c',
    IPMI_CONFIG_ARGP_DIFF_KEY = 'd',
    IPMI_CONFIG_ARGP_FILENAME_KEY_LEGACY = 'f',
    IPMI_CONFIG_ARGP_FILENAME_KEY = 'n',
    IPMI_CONFIG_ARGP_KEYPAIR_KEY = 'e',
    IPMI_CONFIG_ARGP_SECTIONS_KEY = 'S',
    IPMI_CONFIG_ARGP_LIST_SECTIONS_KEY = 'L',
    IPMI_CONFIG_ARGP_VERBOSE_KEY = 'v',
    IPMI_CONFIG_ARGP_PEF_INFO_KEY = 'i', /* legacy */
    IPMI_CONFIG_ARGP_LAN_CHANNEL_NUMBER_KEY = 200,
    IPMI_CONFIG_ARGP_SERIAL_CHANNEL_NUMBER_KEY = 201,
    IPMI_CONFIG_ARGP_SOL_CHANNEL_NUMBER_KEY = 202,
  };

#define IPMI_CTX_ERRNUM_IS_FATAL_ERROR(__ipmi_ctx)                      \
  (((ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_CTX_NULL               \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_CTX_INVALID         \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_PERMISSION          \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_USERNAME_INVALID    \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_PASSWORD_INVALID    \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_K_G_INVALID         \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_AUTHENTICATION_TYPE_UNAVAILABLE \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_CIPHER_SUITE_ID_UNAVAILABLE \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_IPMI_2_0_UNAVAILABLE \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_CONNECTION_TIMEOUT  \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_SESSION_TIMEOUT     \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_DEVICE_ALREADY_OPEN \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_DEVICE_NOT_OPEN     \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_DEVICE_NOT_SUPPORTED \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_DEVICE_NOT_FOUND    \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_DRIVER_TIMEOUT      \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_BAD_RMCPPLUS_STATUS_CODE \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_OUT_OF_MEMORY       \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_HOSTNAME_INVALID    \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_PARAMETERS          \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_DRIVER_PATH_REQUIRED \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_IPMI_ERROR          \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_SYSTEM_ERROR        \
    || (ipmi_ctx_errnum ((__ipmi_ctx))) == IPMI_ERR_INTERNAL_ERROR) ? 1 : 0)

#define IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT                       0x01
#define IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY        0x02
#define IPMI_CONFIG_DO_NOT_CHECKOUT                                  0x04
#define IPMI_CONFIG_READABLE_ONLY                                    0x08
#define IPMI_CONFIG_UNDEFINED                                        0x10
#define IPMI_CONFIG_USERNAME_NOT_SET_YET                             0x20
#define IPMI_CONFIG_DO_NOT_LIST                                      0x40

#define IPMI_CONFIG_USERNAME_NOT_SET_YET_STR     "<username-not-set-yet>"

#define IPMI_CONFIG_CHECKOUT_LINE_LEN                           45

#define IPMI_CONFIG_PARSE_BUFLEN                                4096
#define IPMI_CONFIG_OUTPUT_BUFLEN                               8192

#define IPMI_CONFIG_MAX_SECTION_NAME_LEN                        128
#define IPMI_CONFIG_MAX_KEY_NAME_LEN                            128
#define IPMI_CONFIG_MAX_DESCRIPTION_LEN                         1024

#define IPMI_CONFIG_NON_FATAL_EXIT_VALUE                        1
#define IPMI_CONFIG_FATAL_EXIT_VALUE                            2

#define same(a,b) (strcasecmp (a,b) == 0)

typedef enum
  {
    IPMI_CONFIG_ACTION_CHECKOUT = 1,
    IPMI_CONFIG_ACTION_COMMIT,
    IPMI_CONFIG_ACTION_DIFF,
    IPMI_CONFIG_ACTION_LIST_SECTIONS,
  } ipmi_config_action_t;

typedef enum
  {
    IPMI_CONFIG_ERR_FATAL_ERROR = -5,
    IPMI_CONFIG_ERR_NON_FATAL_ERROR = -4,
    IPMI_CONFIG_ERR_NON_FATAL_ERROR_READ_ONLY = -3,
    IPMI_CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED = -2,
    IPMI_CONFIG_ERR_NON_FATAL_ERROR_INVALID_UNSUPPORTED_CONFIG = -1,
    IPMI_CONFIG_ERR_SUCCESS = 0,
  } ipmi_config_err_t;

typedef enum
  {
    IPMI_CONFIG_VALIDATE_FATAL_ERROR = -5,
    IPMI_CONFIG_VALIDATE_NON_FATAL_ERROR = -4,
    IPMI_CONFIG_VALIDATE_VALUE_CANNOT_BE_ENCODED_ACCURATELY = -3,
    IPMI_CONFIG_VALIDATE_OUT_OF_RANGE_VALUE = -2,
    IPMI_CONFIG_VALIDATE_INVALID_VALUE = -1,
    IPMI_CONFIG_VALIDATE_VALID_VALUE = 0,
  } ipmi_config_validate_t;

#define IPMI_CONFIG_IS_NON_FATAL_ERROR(__ret)                           \
  (((__ret) == IPMI_CONFIG_ERR_NON_FATAL_ERROR                          \
    || (__ret) == IPMI_CONFIG_ERR_NON_FATAL_ERROR_READ_ONLY             \
    || (__ret) == IPMI_CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED         \
    || (__ret) == IPMI_CONFIG_ERR_NON_FATAL_ERROR_INVALID_UNSUPPORTED_CONFIG) ? 1 : 0)

#define IPMI_CONFIG_CATEGORY_MASK_CORE     0x0001
#define IPMI_CONFIG_CATEGORY_MASK_BMC      IPMI_CONFIG_CATEGORY_MASK_CORE /* legacy name */
#define IPMI_CONFIG_CATEGORY_MASK_CHASSIS  0x0002
#define IPMI_CONFIG_CATEGORY_MASK_SENSORS  0x0004
#define IPMI_CONFIG_CATEGORY_MASK_PEF      0x0008
#define IPMI_CONFIG_CATEGORY_MASK_DCMI     0x0010
#define IPMI_CONFIG_CATEGORY_MASK_ALL      0x001F

#define IPMI_CONFIG_CATEGORY_VALID(__val) \
  (((__val) == IPMI_CONFIG_CATEGORY_MASK_CORE \
    || (__val) == IPMI_CONFIG_CATEGORY_MASK_CHASSIS \
    || (__val) == IPMI_CONFIG_CATEGORY_MASK_SENSORS \
    || (__val) == IPMI_CONFIG_CATEGORY_MASK_PEF \
    || (__val) == IPMI_CONFIG_CATEGORY_MASK_DCMI) ? 1 : 0)

struct ipmi_config_keypair
{
  char *section_name;
  char *key_name;
  char *value_input;
  struct ipmi_config_keypair *next;
};

struct ipmi_config_section_str
{
  char *section_name;
  struct ipmi_config_section_str *next;
};

struct ipmi_config_keyvalue {
  struct ipmi_config_key *key;
  char *value_input;
  char *value_output;
  struct ipmi_config_keyvalue *next;
};

/* forward declare */
struct ipmi_config_state_data;

typedef struct ipmi_config_state_data ipmi_config_state_data_t;

/* Fills in kv->value_output as a printable string  */
typedef ipmi_config_err_t (*Key_Checkout)(ipmi_config_state_data_t *state_data,
					  const char *section_name,
                                          struct ipmi_config_keyvalue *kv);

/* Takes kv->value_input and commits it */
typedef ipmi_config_err_t (*Key_Commit)(ipmi_config_state_data_t *state_data,
					const char *section_name,
                                        const struct ipmi_config_keyvalue *kv);

/* Determines if an inputted value is valid */
typedef ipmi_config_validate_t (*Key_Validate)(ipmi_config_state_data_t *state_data,
					       const char *section_name,
                                               const char *key_name,
                                               const char *value);

/* Sectional pre commit call */
typedef ipmi_config_err_t (*Section_Pre_Commit)(ipmi_config_state_data_t *state_data,
						const char *section_name);

/* Sectional post commit call */
typedef ipmi_config_err_t (*Section_Post_Commit)(ipmi_config_state_data_t *state_data,
						 const char *section_name);

struct ipmi_config_key {
  char *key_name;
  char *description;
  unsigned int flags;
  Key_Checkout checkout;
  Key_Commit commit;
  Key_Validate validate;
  struct ipmi_config_key *next;
};

struct ipmi_config_section {
  char *section_name;
  char *section_comment_section_name;
  char *section_comment;
  unsigned int flags;
  Section_Pre_Commit section_pre_commit;
  Section_Post_Commit section_post_commit;
  /* keys in this section */
  struct ipmi_config_key *keys;
  /* key and values for checkout/commit/diff */
  struct ipmi_config_keyvalue *keyvalues;
  struct ipmi_config_section *next;
  unsigned int category;
  unsigned int line_length;
};

struct ipmi_config_arguments
{
  struct common_cmd_args common_args;

  unsigned int category_mask;

  ipmi_config_action_t action;

  unsigned int verbose_count;
  char *filename;
  uint8_t lan_channel_number;
  int lan_channel_number_set;
  uint8_t serial_channel_number;
  int serial_channel_number_set;
  uint8_t sol_channel_number;
  int sol_channel_number_set;
  struct ipmi_config_keypair *keypairs;
  struct ipmi_config_section_str *section_strs;

  /* 
   * Legacy options
   */
  int info;
};

typedef struct ipmi_config_prog_data
{
  char *progname;
  struct ipmi_config_arguments *args;
  int hosts_count;
} ipmi_config_prog_data_t;

typedef struct ipmi_config_enable_user_after_password
{
  int enable_user_failed;
  struct ipmi_config_keyvalue *kv;
} ipmi_config_enable_user_after_password_t;

struct ipmi_config_state_data
{
  ipmi_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  struct ipmi_config_section *sections;
  ipmi_sdr_ctx_t sdr_ctx;

  /* 
   * For Core / BMC configuration
   */

  /* achu: workaround for OEM compliance issue, see user section */
  int enable_user_after_password_len;
  ipmi_config_enable_user_after_password_t *enable_user_after_password;

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
  uint8_t cipher_suite_id_supported[IPMI_CONFIG_CIPHER_SUITE_LEN];
  int cipher_suite_id_supported_set;
  uint8_t cipher_suite_priv[IPMI_CONFIG_CIPHER_SUITE_LEN];
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

  /* 
   * For Chassis configuration
   */

  /* achu: workaround for IPMI limitation */
  int front_panel_enable_standby_button_for_entering_standby_initialized;
  uint8_t front_panel_enable_standby_button_for_entering_standby;
  int front_panel_enable_diagnostic_interrupt_button_initialized;
  uint8_t front_panel_enable_diagnostic_interrupt_button;
  int front_panel_enable_reset_button_initialized;
  uint8_t front_panel_enable_reset_button;
  int front_panel_enable_power_off_button_for_power_off_only_initialized;
  uint8_t front_panel_enable_power_off_button_for_power_off_only;
};

#endif /* IPMI_CONFIG_H */
