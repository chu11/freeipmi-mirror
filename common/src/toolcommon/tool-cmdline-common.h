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

#ifndef _TOOL_CMDLINE_COMMON_H
#define _TOOL_CMDLINE_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/cmds/ipmi-messaging-support-cmds.h"

enum argp_common_option_keys
  { 
    ARGP_DRIVER_TYPE_KEY = 'D', 
    ARGP_NO_PROBING_KEY = 131,  /* for backwards compatability */
    ARGP_DISABLE_AUTO_PROBE_KEY = 130,
    ARGP_DRIVER_ADDRESS_KEY = 132, 
    ARGP_DRIVER_DEVICE_KEY = 133, 
    ARGP_TIMEOUT_KEY = 134,  /* for backwards compatability */
    ARGP_SESSION_TIMEOUT_KEY = 135,
    ARGP_RETRY_TIMEOUT_KEY = 136, /* for backwards compatability */
    ARGP_RETRANSMISSION_TIMEOUT_KEY = 137, 
    ARGP_REG_SPACE_KEY = 138, /* for backwards compatability */
    ARGP_REGISTER_SPACING_KEY = 139,
    ARGP_HOSTNAME_KEY = 'h', 
    ARGP_USERNAME_KEY = 'u', 
    ARGP_PASSWORD_KEY = 'p', 
    ARGP_PASSWORD_PROMPT_KEY = 'P',
    ARGP_K_G_KEY = 'k', 
    ARGP_K_G_PROMPT_KEY = 'K',
    ARGP_AUTH_TYPE_KEY = 140, /* for backwards compatability */
    ARGP_AUTHENTICATION_TYPE_KEY = 'a', 
    ARGP_CIPHER_SUITE_ID_KEY = 'I',
    ARGP_PRIVILEGE_KEY = 141, /* for backwards compatability */
    ARGP_PRIV_LEVEL_KEY = 142, /* for backwards compatability */
    ARGP_PRIVILEGE_LEVEL_KEY = 'l',
    ARGP_CONFIG_KEY = 143,      /* for backwards compatability */
    ARGP_CONFIG_FILE_KEY = 144,
    ARGP_WORKAROUND_FLAGS_KEY = 'W',
    ARGP_DEBUG_KEY = 145,
    ARGP_FLUSH_CACHE_KEY = 'f',
    ARGP_QUIET_CACHE_KEY = 'Q',
    ARGP_SDR_CACHE_DIRECTORY_KEY = 146,
    ARGP_SDR_CACHE_RECREATE_KEY = 147,
    ARGP_IGNORE_SDR_CACHE_KEY = 148,
    ARGP_BUFFER_OUTPUT_KEY = 'B',
    ARGP_CONSOLIDATE_OUTPUT_KEY = 'C',
    ARGP_FANOUT_KEY = 'F',
    ARGP_ELIMINATE_KEY = 'E',
    ARGP_ALWAYS_PREFIX_KEY = 149,
  };

/*
 * achu:
 *
 * argp's help/usage layout has various bugs when the description
 * buffer gets to 150 characters in length.  So we're going to shorten
 * argp usage help output from what is in the manpages. 
 */

#define ARGP_COMMON_OPTIONS_DRIVER                                                           \
    {"driver-type",    ARGP_DRIVER_TYPE_KEY, "IPMIDRIVER", 0, 	                             \
     "Specify the driver type to use instead of doing an auto selection.", 0}

/* no-probing is maintained for backwards compatability */
/* reg-space is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_INBAND                                                           \
    {"no-probing",     ARGP_NO_PROBING_KEY, 0, OPTION_HIDDEN,                                \
     "Do not probe in-band IPMI devices for default settings.", 1},                          \
    {"disable-auto-probe", ARGP_DISABLE_AUTO_PROBE_KEY, 0, 0, 	                             \
     "Do not probe in-band IPMI devices for default settings.", 1},                          \
    {"driver-address", ARGP_DRIVER_ADDRESS_KEY, "DRIVER-ADDRESS", 0,                         \
     "Specify the in-band driver address to be used instead of the probed value.", 2}, 	     \
    {"driver-device",  ARGP_DRIVER_DEVICE_KEY, "DEVICE", 0,                                  \
     "Specify the in-band driver device path to be used instead of the probed path.", 3},    \
    {"reg-space", ARGP_REG_SPACE_KEY, "REGISTER-SPACING", OPTION_HIDDEN,                     \
     "Specify the in-band driver register spacing instead of the probed value.", 4},         \
    {"register-spacing", ARGP_REGISTER_SPACING_KEY, "REGISTER-SPACING", 0,                   \
     "Specify the in-band driver register spacing instead of the probed value.", 4}

#define ARGP_COMMON_OPTIONS_OUTOFBAND                                                        \
    {"hostname",       ARGP_HOSTNAME_KEY, "IPMIHOST", 0, 			             \
     "Specify the remote host to communicate with.", 5},		                     \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON,                                                    \
    ARGP_COMMON_OPTIONS_OUTOFBAND_TIMEOUT

#define ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED                                             \
    {"hostname",       ARGP_HOSTNAME_KEY, "IPMIHOST", 0, 			             \
     "Specify the remote host(s) to communicate with.", 5},                                  \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON,                                                    \
    ARGP_COMMON_OPTIONS_OUTOFBAND_TIMEOUT

#define ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON                                                       \
    {"username",       ARGP_USERNAME_KEY, "USERNAME", 0, 			                   \
     "Specify the username to use when authenticating with the remote host.", 6},                  \
    {"password",       ARGP_PASSWORD_KEY, "PASSWORD", 0,	                                   \
     "Specify the password to use when authenticationg with the remote host. ", 7},                \
    {"password-prompt", ARGP_PASSWORD_PROMPT_KEY, 0, 0,	                                           \
     "Prompt for password to avoid possibility of listing it in process lists.", 8},               \
    {"k-g",       ARGP_K_G_KEY, "K_G", 0,	                                                   \
     "Specify the K_g BMC key to use when authenticating with the remote host for IPMI 2.0.", 9},  \
    {"k-g-prompt", ARGP_K_G_PROMPT_KEY, 0, 0,	                                                   \
     "Prompt for k-g to avoid possibility of listing it in process lists.", 10}

/* retry-timeout is maintained for backwards compatability */
/* timeout is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_OUTOFBAND_TIMEOUT                                                      \
    {"timeout", ARGP_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,                                   \
     "Specify the session timeout in milliseconds.", 11},                                          \
    {"session-timeout", ARGP_SESSION_TIMEOUT_KEY, "MILLISECONDS", 0,                               \
     "Specify the session timeout in milliseconds.", 11},                                          \
    {"retry-timeout", ARGP_RETRY_TIMEOUT_KEY, "MILLISECONDS", OPTION_HIDDEN,                       \
     "Specify the packet retransmission timeout in milliseconds.", 12},                            \
    {"retransmission-timeout", ARGP_RETRANSMISSION_TIMEOUT_KEY, "MILLISECONDS", 0,                 \
     "Specify the packet retransmission timeout in milliseconds.", 12}                             \

/* auth-type is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE                                                     \
    {"auth-type", ARGP_AUTH_TYPE_KEY, "AUTHENTICATION-TYPE", OPTION_HIDDEN, 	                    \
     "Specify the IPMI 1.5 authentication type to use.", 13},                                       \
    {"authentication-type", ARGP_AUTHENTICATION_TYPE_KEY, "AUTHENTICATION-TYPE", 0, 	            \
     "Specify the IPMI 1.5 authentication type to use.", 13}

#define ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID                                                         \
    {"cipher-suite-id",     ARGP_CIPHER_SUITE_ID_KEY, "CIPHER-SUITE-ID", 0,                         \
     "Specify the IPMI 2.0 cipher suite ID to use.", 14}

/* privilege is maintained for backwards compatability */
/* priv-level is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_USER                                                    \
    {"privilege",  ARGP_PRIVILEGE_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                            \
     "Specify the privilege level to be used. ", 15},                                               \
    {"priv-level",  ARGP_PRIV_LEVEL_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                          \
     "Specify the privilege level to be used.", 15},                                                \
    {"privilege-level",  ARGP_PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	                    \
     "Specify the privilege level to be used.", 15}     

/* privilege is maintained for backwards compatability */
/* priv-level is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_OPERATOR                                                \
    {"privilege",  ARGP_PRIVILEGE_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                            \
     "Specify the privilege level to be used. ", 15},                                               \
    {"priv-level",  ARGP_PRIV_LEVEL_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                          \
     "Specify the privilege level to be used.", 15},                                                \
    {"privilege-level",  ARGP_PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	                    \
     "Specify the privilege level to be used.", 15}     

/* privilege is maintained for backwards compatability */
/* priv-level is maintained for backwards compatability */
#define ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN                                                   \
    {"privilege",  ARGP_PRIVILEGE_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                            \
     "Specify the privilege level to be used. ", 15},                                               \
    {"priv-level",  ARGP_PRIV_LEVEL_KEY, "PRIVILEGE-LEVEL", OPTION_HIDDEN,                          \
     "Specify the privilege level to be used.", 15},                                                \
    {"privilege-level",  ARGP_PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	                    \
     "Specify the privilege level to be used.", 15}     

#define ARGP_COMMON_OPTIONS_CONFIG_FILE                                                             \
  {"config-file", ARGP_CONFIG_FILE_KEY, "FILE", 0,                                                  \
   "Specify alternate configuration file.", 16}                                                     \

#define ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS                                                        \
    {"workaround-flags",  ARGP_WORKAROUND_FLAGS_KEY, "WORKAROUNDS", 0, 	                            \
     "Specify workarounds to vendor compliance issues.", 17}

#define ARGP_COMMON_SDR_OPTIONS                                                                                      \
    {"flush-cache", ARGP_FLUSH_CACHE_KEY,  0, 0,                                                                     \
     "Flush a cached version of the sensor data repository (SDR) cache.", 18},                                       \
    {"quiet-cache", ARGP_QUIET_CACHE_KEY,  0, 0,                                                                     \
     "Do not output information about cache creation/deletion.", 19},                                                \
    {"sdr-cache-directory", ARGP_SDR_CACHE_DIRECTORY_KEY, "DIRECTORY", 0,                                            \
     "Specify an alternate directory for sensor data repository (SDR) caches to be stored or read from.", 20},       \
    {"sdr-cache-recreate", ARGP_SDR_CACHE_RECREATE_KEY,  0, 0,                                                       \
     "Recreate sensor data repository (SDR) cache if cache is out of date or invalid.", 21}

#define ARGP_COMMON_IGNORE_SDR_OPTIONS                                                                               \
    {"ignore-sdr-cache", ARGP_IGNORE_SDR_CACHE_KEY, 0, 0,                                                            \
     "Ignore all SDR cache related processing.", 22} 

#define ARGP_COMMON_HOSTRANGED_OPTIONS                                     \
    {"buffer-output", ARGP_BUFFER_OUTPUT_KEY, 0, 0,                        \
     "Buffer hostranged output.", 23},                                     \
    {"consolidate-output", ARGP_CONSOLIDATE_OUTPUT_KEY, 0, 0,              \
     "Consolidate hostranged output.", 24},                                \
    {"fanout", ARGP_FANOUT_KEY, "NUM", 0,                                  \
     "Specify multiple host fanout.", 25},                                 \
    {"eliminate", ARGP_ELIMINATE_KEY, 0, 0,                                \
     "Eliminate undetected nodes.", 26},                                   \
    {"always-prefix", ARGP_ALWAYS_PREFIX_KEY, 0, 0,                        \
     "Always prefix output.", 27}

#define ARGP_COMMON_OPTIONS_DEBUG                                          \
    {"debug",     ARGP_DEBUG_KEY, 0, 0, 	                           \
     "Turn on debugging.", 28}                                             

struct common_cmd_args 
{
  ipmi_driver_type_t driver_type;
  int driver_type_outofband_only; /* flag - set internally only */
  int disable_auto_probe;
  unsigned int driver_address;
  char *driver_device;
  unsigned int register_spacing;
  unsigned int session_timeout;
  unsigned int retransmission_timeout;
  char *hostname;
  char *username;
  char *password;
  uint8_t k_g[IPMI_MAX_K_G_LENGTH+1];
  unsigned int k_g_len;
  int authentication_type;
  int cipher_suite_id;
  int privilege_level; 
  char *config_file;
  unsigned int workaround_flags;
  int debug;
};

struct sdr_cmd_args
{  
  int flush_cache;
  int quiet_cache;
  char *sdr_cache_directory;
  int sdr_cache_recreate;
  int ignore_sdr_cache;
};

struct hostrange_cmd_args
{
  int buffer_output;
  int consolidate_output;
  unsigned int fanout;
  int eliminate;
  int always_prefix;
};

#define IPMI_DEVICE_LAN_STR      "lan"
#define IPMI_DEVICE_LAN_2_0_STR  "lan_2_0"
#define IPMI_DEVICE_KCS_STR      "kcs"
#define IPMI_DEVICE_SSIF_STR     "ssif"
#define IPMI_DEVICE_OPENIPMI_STR "openipmi"
#define IPMI_DEVICE_SUNBMC_STR   "sunbmc"

#define IPMI_TOOL_WORKAROUND_FLAGS_DEFAULT                     0x00000000
#define IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO      0x00000001
#define IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION 0x00000002
#define IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE   0x00000004
#define IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER  0x00000008
#define IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES 0x00000010
#define IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION           0x00000020
#define IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION      0x00000040
#define IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION             0x00000080
#define IPMI_TOOL_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE      0x00000100
#define IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE     0x00000200
#define IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PORT             0x00000400
#define IPMI_TOOL_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS  0x00000800

#define IPMI_TOOL_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO_STR      "idzero"
#define IPMI_TOOL_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION_STR "forcepermsg"
#define IPMI_TOOL_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE_STR   "unexpectedauth"
#define IPMI_TOOL_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER_STR  "endianseq"
#define IPMI_TOOL_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES_STR "authcap"
#define IPMI_TOOL_WORKAROUND_FLAGS_INTEL_2_0_SESSION_STR           "intel20"
#define IPMI_TOOL_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION_STR      "supermicro20"
#define IPMI_TOOL_WORKAROUND_FLAGS_SUN_2_0_SESSION_STR             "sun20"
#define IPMI_TOOL_WORKAROUND_FLAGS_OPEN_SESSION_PRIVILEGE_STR      "opensesspriv"
#define IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PAYLOAD_SIZE_STR     "solpayloadsize"
#define IPMI_TOOL_WORKAROUND_FLAGS_IGNORE_SOL_PORT_STR             "solport"
#define IPMI_TOOL_WORKAROUND_FLAGS_SKIP_SOL_ACTIVATION_STATUS_STR  "solstatus"

#define IPMI_AUTHENTICATION_TYPE_NONE_STR                               "none"
#define IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR              "straight_password_key"
#define IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR_OLD          "plain"
#define IPMI_AUTHENTICATION_TYPE_MD2_STR                                "md2"
#define IPMI_AUTHENTICATION_TYPE_MD5_STR                                "md5"

#define IPMI_PRIVILEGE_LEVEL_USER_STR                                   "user" 
#define IPMI_PRIVILEGE_LEVEL_OPERATOR_STR                               "operator"
#define IPMI_PRIVILEGE_LEVEL_ADMIN_STR                                  "admin"
#define IPMI_PRIVILEGE_LEVEL_ADMIN_STR2                                 "administrator"

int parse_inband_driver_type(char *str);

int parse_outofband_driver_type(char *str);

int parse_driver_type(char *str);

int parse_authentication_type(char *str);

int parse_privilege_level(char *str);

int parse_workaround_flags(char *str);

error_t common_parse_opt (int key, 
			  char *arg, 
			  struct argp_state *state, 
			  struct common_cmd_args *common_cmd_args);

error_t sdr_parse_opt (int key, 
                       char *arg, 
                       struct argp_state *state, 
                       struct sdr_cmd_args *sdr_cmd_args);

error_t hostrange_parse_opt (int key, 
                             char *arg, 
                             struct argp_state *state, 
                             struct hostrange_cmd_args *hostrange_cmd_args);

void init_common_cmd_args_user (struct common_cmd_args *cmd_args);
void init_common_cmd_args_operator (struct common_cmd_args *cmd_args);
void init_common_cmd_args_admin (struct common_cmd_args *cmd_args);
void free_common_cmd_args (struct common_cmd_args *cmd_args);
void verify_common_cmd_args (struct common_cmd_args *cmd_args);

void init_sdr_cmd_args (struct sdr_cmd_args *cmd_args);
void free_sdr_cmd_args (struct sdr_cmd_args *cmd_args);
void verify_sdr_cmd_args (struct sdr_cmd_args *cmd_args);

void init_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args);
void free_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args);
void verify_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args);

/* to parse only the --config-file option */
error_t cmdline_config_file_parse (int key, char *arg, struct argp_state *state);

#endif
