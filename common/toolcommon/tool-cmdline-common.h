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

#ifndef TOOL_CMDLINE_COMMON_H
#define TOOL_CMDLINE_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */

#include <freeipmi/freeipmi.h>

#include "parse-common.h"

enum argp_common_option_keys
  {
    ARGP_DRIVER_TYPE_KEY = 'D',
    ARGP_NO_PROBING_KEY = 131,      /* for backwards compatability */
    ARGP_DISABLE_AUTO_PROBE_KEY = 130,
    ARGP_DRIVER_ADDRESS_KEY = 132,
    ARGP_DRIVER_DEVICE_KEY = 133,
    ARGP_TIMEOUT_KEY = 134,      /* for backwards compatability */
    ARGP_SESSION_TIMEOUT_KEY = 135,
    ARGP_RETRY_TIMEOUT_KEY = 136,     /* for backwards compatability */
    ARGP_RETRANSMISSION_TIMEOUT_KEY = 137,
    ARGP_REG_SPACE_KEY = 138,     /* for backwards compatability */
    ARGP_REGISTER_SPACING_KEY = 139,
    ARGP_TARGET_CHANNEL_NUMBER_KEY = 140,
    ARGP_TARGET_SLAVE_ADDRESS_KEY = 141,
    ARGP_HOSTNAME_KEY = 'h',
    ARGP_USERNAME_KEY = 'u',
    ARGP_PASSWORD_KEY = 'p',
    ARGP_PASSWORD_PROMPT_KEY = 'P',
    ARGP_K_G_KEY = 'k',
    ARGP_K_G_PROMPT_KEY = 'K',
    ARGP_AUTH_TYPE_KEY = 142,     /* for backwards compatability */
    ARGP_AUTHENTICATION_TYPE_KEY = 'a',
    ARGP_CIPHER_SUITE_ID_KEY = 'I',
    ARGP_PRIVILEGE_KEY = 143,     /* for backwards compatability */
    ARGP_PRIV_LEVEL_KEY = 144,     /* for backwards compatability */
    ARGP_PRIVILEGE_LEVEL_KEY = 'l',
    ARGP_CONFIG_KEY = 145,          /* for backwards compatability */
    ARGP_CONFIG_FILE_KEY = 146,
    ARGP_WORKAROUND_FLAGS_KEY = 'W',
    ARGP_DEBUG_KEY = 147,
    /* sdr options */
    ARGP_FLUSH_CACHE_KEY = 'f',
    ARGP_QUIET_CACHE_KEY = 'Q',
    ARGP_SDR_CACHE_RECREATE_KEY = 149,
    ARGP_SDR_CACHE_FILE_KEY = 148,
    ARGP_SDR_CACHE_DIRECTORY_KEY = 150,
    ARGP_IGNORE_SDR_CACHE_KEY = 151,
    /* time options */
    ARGP_UTC_TO_LOCALTIME_KEY = 152,
    ARGP_LOCALTIME_TO_UTC_KEY = 153,
    ARGP_UTC_OFFSET_KEY = 154,
    /* hostrange options */
    ARGP_BUFFER_OUTPUT_KEY = 'B',
    ARGP_CONSOLIDATE_OUTPUT_KEY = 'C',
    ARGP_FANOUT_KEY = 'F',
    ARGP_ELIMINATE_KEY = 'E',
    ARGP_ALWAYS_PREFIX_KEY = 155,
  };

/*
 * achu:
 *
 * argp's help/usage layout has various bugs when the description
 * buffer gets to 150 characters in length.  So we're going to shorten
 * argp usage help output from what is in the manpages.
 */

#define ARGP_COMMON_OPTIONS_DRIVER                                                                              \
  { "driver-type",    ARGP_DRIVER_TYPE_KEY, "IPMIDRIVER", 0,                                                    \
      "Specify the driver type to use instead of doing an auto selection.", 0}

/* no-probing is maintained for backwards compatability */
/* reg-space is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_INBAND                                                                              \
  { "no-probing",     ARGP_NO_PROBING_KEY, 0, OPTION_HIDDEN,                                                    \
      "Do not probe in-band IPMI devices for default settings.", 1},                                            \
  { "disable-auto-probe", ARGP_DISABLE_AUTO_PROBE_KEY, 0, 0,                                                    \
      "Do not probe in-band IPMI devices for default settings.", 1},                                            \
  { "driver-address", ARGP_DRIVER_ADDRESS_KEY, "DRIVER-ADDRESS", 0,                                             \
      "Specify the in-band driver address to be used instead of the probed value.", 2},                         \
  { "driver-device",  ARGP_DRIVER_DEVICE_KEY, "DEVICE", 0,                                                      \
      "Specify the in-band driver device path to be used instead of the probed path.", 3},                      \
  { "reg-space", ARGP_REG_SPACE_KEY, "REGISTER-SPACING", OPTION_HIDDEN,                                         \
      "Specify the in-band driver register spacing instead of the probed value.", 4},                           \
  { "register-spacing", ARGP_REGISTER_SPACING_KEY, "REGISTER-SPACING", 0,                                       \
      "Specify the in-band driver register spacing instead of the probed value.", 4},                           \
  { "target-channel-number", ARGP_TARGET_CHANNEL_NUMBER_KEY, "CHANNEL-NUMBER", 0,                               \
      "Specify an in-band driver target channel number to send IPMI requests to.", 5},                          \
  { "target-slave-address", ARGP_TARGET_SLAVE_ADDRESS_KEY, "SLAVE-ADDRESS", 0,                                  \
      "Specify an in-band driver target slave address to send IPMI requests to.", 6}

#define ARGP_COMMON_OPTIONS_OUTOFBAND                                                                           \
  { "hostname",       ARGP_HOSTNAME_KEY, "IPMIHOST", 0,                                                         \
      "Specify the remote host to communicate with.", 7},                                                       \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON

#define ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED                                                                \
  { "hostname",       ARGP_HOSTNAME_KEY, "IPMIHOST", 0,                                                         \
      "Specify the remote host(s) to communicate with.", 7},                                                    \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON

/* retry-timeout is maintained for backwards compatability */
/* timeout is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON                                                                    \
  { "username",       ARGP_USERNAME_KEY, "USERNAME", 0,                                                         \
      "Specify the username to use when authenticating with the remote host.", 8},                              \
  { "password",       ARGP_PASSWORD_KEY, "PASSWORD", 0,                                                         \
      "Specify the password to use when authenticationg with the remote host. ", 9},                            \
  { "password-prompt", ARGP_PASSWORD_PROMPT_KEY, 0, 0,                                                          \
      "Prompt for password to avoid possibility of listing it in process lists.", 10},                          \
  { "k-g",       ARGP_K_G_KEY, "K_G", 0,                                                                        \
      "Specify the K_g BMC key to use when authenticating with the remote host for IPMI 2.0.", 11},             \
  { "k-g-prompt", ARGP_K_G_PROMPT_KEY, 0, 0,                                                                    \
      "Prompt for k-g to avoid possibility of listing it in process lists.", 12},                               \
  { "timeout", ARGP_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,                                                 \
      "Specify the session timeout in milliseconds.", 13},                                                      \
  { "session-timeout", ARGP_SESSION_TIMEOUT_KEY, "MILLISECONDS", 0,                                             \
      "Specify the session timeout in milliseconds.", 13},                                                      \
  { "retry-timeout", ARGP_RETRY_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,                                     \
      "Specify the packet retransmission timeout in milliseconds.", 14},                                        \
  { "retransmission-timeout", ARGP_RETRANSMISSION_TIMEOUT_KEY, "MILLISECONDS", 0,                               \
      "Specify the packet retransmission timeout in milliseconds.", 14}

/* auth-type is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE                                                                 \
  { "auth-type", ARGP_AUTH_TYPE_KEY, "AUTHENTICATION-TYPE", OPTION_HIDDEN,                                      \
      "Specify the IPMI 1.5 authentication type to use.", 15},                                                  \
  { "authentication-type", ARGP_AUTHENTICATION_TYPE_KEY, "AUTHENTICATION-TYPE", 0,                              \
      "Specify the IPMI 1.5 authentication type to use.", 15}

#define ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID                                                                     \
  { "cipher-suite-id",     ARGP_CIPHER_SUITE_ID_KEY, "CIPHER-SUITE-ID", 0,                                      \
      "Specify the IPMI 2.0 cipher suite ID to use.", 16}

/* privilege is maintained for backwards compatability */
/* priv-level is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL                                                                     \
  { "privilege",  ARGP_PRIVILEGE_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                                         \
      "Specify the privilege level to be used. ", 17},                                                          \
  { "priv-level",  ARGP_PRIV_LEVEL_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                                       \
      "Specify the privilege level to be used.", 17},                                                           \
  { "privilege-level",  ARGP_PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0,                                         \
      "Specify the privilege level to be used.", 17}

#define ARGP_COMMON_OPTIONS_CONFIG_FILE                                                                         \
  { "config-file", ARGP_CONFIG_FILE_KEY, "FILE", 0,                                                             \
      "Specify alternate configuration file.", 18}

#define ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS                                                                    \
  { "workaround-flags",  ARGP_WORKAROUND_FLAGS_KEY, "WORKAROUNDS", 0,                                           \
      "Specify workarounds to vendor compliance issues.", 19}

#define ARGP_COMMON_SDR_CACHE_OPTIONS                                                                           \
  { "flush-cache", ARGP_FLUSH_CACHE_KEY,  0, 0,                                                                 \
      "Flush a cached version of the sensor data repository (SDR) cache.", 20},                                 \
  { "quiet-cache", ARGP_QUIET_CACHE_KEY,  0, 0,                                                                 \
      "Do not output information about cache creation/deletion.", 21},                                          \
  { "sdr-cache-recreate", ARGP_SDR_CACHE_RECREATE_KEY,  0, 0,                                                   \
      "Recreate sensor data repository (SDR) cache if cache is out of date or invalid.", 22}

#define ARGP_COMMON_SDR_CACHE_OPTIONS_FILE_DIRECTORY                                                            \
  { "sdr-cache-file", ARGP_SDR_CACHE_FILE_KEY, "FILE", 0,                                                       \
      "Specify a specific file for the sensor data repository (SDR) cache to be stored or read from.", 23},     \
  { "sdr-cache-directory", ARGP_SDR_CACHE_DIRECTORY_KEY, "DIRECTORY", 0,                                        \
      "Specify an alternate directory for sensor data repository (SDR) caches to be stored or read from.", 24}

#define ARGP_COMMON_SDR_CACHE_OPTIONS_IGNORE                                                                    \
  { "ignore-sdr-cache", ARGP_IGNORE_SDR_CACHE_KEY, 0, 0,                                                        \
      "Ignore all SDR cache related processing.", 25}

#define ARGP_COMMON_TIME_OPTIONS                                                                                \
  { "utc-to-localtime", ARGP_UTC_TO_LOCALTIME_KEY, 0, 0,                                                        \
      "Assume times stored UTC, convert to localtime.", 26},                                                    \
  { "localtime-to-utc", ARGP_LOCALTIME_TO_UTC_KEY, 0, 0,                                                        \
      "Assume times stored in localtime, convert to UTC.", 27},                                                 \
  { "utc-offset", ARGP_UTC_OFFSET_KEY, "SECONDS", 0,                                                            \
      "Specify a specific UTC offset to be added to timestamps.", 28}

#define ARGP_COMMON_HOSTRANGED_OPTIONS                                                                          \
  { "buffer-output", ARGP_BUFFER_OUTPUT_KEY, 0, 0,                                                              \
      "Buffer hostranged output.", 29},                                                                         \
  { "consolidate-output", ARGP_CONSOLIDATE_OUTPUT_KEY, 0, 0,                                                    \
      "Consolidate hostranged output.", 30},                                                                    \
  { "fanout", ARGP_FANOUT_KEY, "NUM", 0,                                                                        \
      "Specify multiple host fanout.", 31},                                                                     \
  { "eliminate", ARGP_ELIMINATE_KEY, 0, 0,                                                                      \
      "Eliminate undetected nodes.", 32},                                                                       \
  { "always-prefix", ARGP_ALWAYS_PREFIX_KEY, 0, 0,                                                              \
      "Always prefix output.", 33}

#define ARGP_COMMON_OPTIONS_DEBUG                                                                               \
  { "debug",     ARGP_DEBUG_KEY, 0, 0,                                                                          \
      "Turn on debugging.", 34}

struct common_cmd_args
{
  /* inband options */
  ipmi_driver_type_t driver_type;
  int driver_type_outofband_only; /* flag - set internally only */
  int disable_auto_probe;
  unsigned int driver_address;
  char *driver_device;
  unsigned int register_spacing;
  uint8_t target_channel_number;
  int target_channel_number_is_set; /* user can input zero, so need a flag */
  uint8_t target_slave_address;
  int target_slave_address_is_set; /* user can input zero so need a flag */

  /* outofband options */
  char *hostname;
  char *username;
  char *password;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
  unsigned int k_g_len;
  unsigned int session_timeout;
  unsigned int retransmission_timeout;
  int authentication_type;
  int cipher_suite_id;
  int privilege_level;

  /* 
   * misc options
   */
  char *config_file;
  unsigned int workaround_flags_outofband;
  unsigned int workaround_flags_outofband_2_0;
  unsigned int workaround_flags_inband;
  unsigned int workaround_flags_sdr;
  unsigned int section_specific_workaround_flags;
  int debug;

  /* sdr options */
  int flush_cache;
  int quiet_cache;
  int sdr_cache_recreate;
  char *sdr_cache_file;
  char *sdr_cache_directory;
  int ignore_sdr_cache;

  /* time options */
  int utc_to_localtime;
  int localtime_to_utc;
  int utc_offset;

  /* hostrange options */
  int buffer_output;
  int consolidate_output;
  unsigned int fanout;
  int eliminate;
  int always_prefix;
};

error_t common_parse_opt (int key,
                          char *arg,
                          struct common_cmd_args *common_args);

void init_common_cmd_args_user (struct common_cmd_args *common_args);
void init_common_cmd_args_operator (struct common_cmd_args *common_args);
void init_common_cmd_args_admin (struct common_cmd_args *common_args);
void verify_common_cmd_args_inband (struct common_cmd_args *common_args);
void verify_common_cmd_args_outofband (struct common_cmd_args *common_args, int check_hostname);
void verify_common_cmd_args (struct common_cmd_args *common_args);

/* to parse only the --config-file option */
error_t cmdline_config_file_parse (int key, char *arg, struct argp_state *state);

#endif /* TOOL_CMDLINE_COMMON_H */
