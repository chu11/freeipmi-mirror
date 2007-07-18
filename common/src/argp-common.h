/*
   argp-common.h: common work for argp for freeipmi tools.
   Copyright (C) 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*/

#ifndef _ARGP_COMMON_H
#define _ARGP_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <argp.h>

#include "freeipmi/ipmi-messaging-support-cmds.h"
#include "freeipmi/udm/ipmi-udm.h"

enum argp_common_option_keys
  { 
    DRIVER_TYPE_KEY = 'D', 
    DUMMY_KEY = 129, 
    NO_PROBING_KEY = 130, 
    DRIVER_ADDRESS_KEY = 131, 
    DRIVER_DEVICE_KEY = 132, 
    RETRY_TIMEOUT_KEY = 133, 
    SESSION_TIMEOUT_KEY = 134,
    REGISTER_SPACING_KEY = 135,
    HOSTNAME_KEY = 'h', 
    USERNAME_KEY = 'u', 
    PASSWORD_KEY = 'p', 
    PASSWORD_PROMPT_KEY = 'P',
    K_G_KEY = 'k', 
    K_G_PROMPT_KEY = 'K',
    AUTHENTICATION_TYPE_KEY = 'a', 
    CIPHER_SUITE_ID_KEY = 'I',
    PRIVILEGE_LEVEL_KEY = 'l',
    FLUSH_CACHE_KEY = 'f',
    QUIET_CACHE_KEY = 'Q',
    SDR_CACHE_DIR_KEY = 136,
    BUFFER_KEY = 'B',
    CONSOLIDATE_KEY = 'C',
    FANOUT_KEY = 'F',
    ELIMINATE_KEY = 'E',
    WORKAROUND_FLAGS_KEY = 'W',
    DEBUG_KEY = 137
  };

#define ARGP_COMMON_OPTIONS_DRIVER                                                           \
    {"driver-type",    DRIVER_TYPE_KEY, "IPMIDRIVER", 0, 	                             \
     "Specify  the  driver  type to use instead of doing an auto selection.", 0}

#define ARGP_COMMON_OPTIONS_INBAND                                                           \
    {"no-probing",     NO_PROBING_KEY, 0, 0, 	                                             \
     "Do not probe IPMI devices for default settings.", 1},                                  \
    {"driver-address", DRIVER_ADDRESS_KEY, "DRIVERADDR", 0,                                  \
     "Specify the in-band driver address to be used instead of the probed value.", 2}, 	     \
    {"driver-device",  DRIVER_DEVICE_KEY, "DEVICE", 0,                                       \
     "Specify the in-band driver device path to be used instead of the probed path.", 3},    \
    {"register-spacing", REGISTER_SPACING_KEY, "REGISTERSPACING", 0,                         \
     "Specify the in-band driver register spacing instead of the probed value.", 4}

#define ARGP_COMMON_OPTIONS_OUTOFBAND                                                        \
    {"hostname",       HOSTNAME_KEY, "IPMIHOST", 0, 			                     \
     "Specify the remote host to communicate with.", 5},		                     \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON

#define ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED                                             \
    {"hostname",       HOSTNAME_KEY, "IPMIHOST", 0, 			                     \
     "Specify the remote host(s) to communicate with.", 5},                                  \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON

#define ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON                                                       \
    {"username",       USERNAME_KEY, "USERNAME", 0, 			                           \
     "Specify the username to use when authenticating with the remote host. "                      \
     "If not specified, a null (i.e. anonymous) username is assumed.", 6},                         \
    {"password",       PASSWORD_KEY, "PASSWORD", 0,	                                           \
     "Specify the password to use when authenticationg with the remote host. "                     \
     "If not specified, a null password is assumed.", 7}                                           \
    {"password-prompt", PASSWORD_PROMPT_KEY, 0, 0,	                                           \
     "Prompt for password to avoid possibility of listing it in process lists.", 8},               \
    {"k-g",       K_G_KEY, "K_G", 0,	                                                           \
     "Specify the K_g BMC key to use when authenticating with the remote host for IPMI 2.0. "      \
     "If not specified, a null key is assumed.", 9},                                               \
    {"k-g-prompt", K_G_PROMPT_KEY, 0, 0,	                                                   \
     "Prompt for k-g to avoid possibility of listing it in process lists.", 10},                   \
    {"retry-timeout", RETRY_TIMEOUT_KEY, "MILLISECONDS", 0,                                        \
     "Specify the packet retransmission timeout in milliseconds. "                                 \ 
     "Defaults to 20000 milliseconds (20 seconds) if not specified.", 11},                         \
    {"session-timeout", SESSION_TIMEOUT_KEY, "MILLISECONDS", 0,                                    \
     "Specify the session timeout in milliseconds. "                                               \
     "Defaults to 1000 milliseconds (1 second) if not specified.", 12}

#define ARGP_COMMON_OPTIONS_AUTHTYPE                                                                \
    {"authentication-type", AUTHENTICATION_TYPE_KEY, "AUTHTYPE", 0, 	                            \
     "Specify the IPMI 1.5 authentication type to use. "                                            \
     "The currently available authentication types are NONE, STRAIGHT_PASSWORD_KEY, MD2, and MD5. " \
     "Defaults to MD5 if not specified", 12}

#define ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID                                                         \
    {"cipher-suite-id",     CIPHER_SUITE_ID_KEY, "CIPHER_SUITE_ID", 0,                              \
     "Specify the IPMI 2.0 cipher suite ID to use. "				                    \
     "The currently supported cipher suite ids are: 0, 1, 2, 3, 6, 7, 8, 11, 12. "                  \
     "Defaults to 3 if not specified.", 13}

#define ARGP_COMMON_OPTIONS_PRIVLEVEL_USER                                                          \
    {"privilege-level",  PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	                            \
     "Specify  the  privilege  level to be used. "		                                    \
     "The currently available privilege levels are CALLBACK, USER, OPERATOR, ADMIN, and OEM. "	    \
     "Defaults to USER if not specified.", 14}     

#define ARGP_COMMON_OPTIONS_PRIVLEVEL_USER                                                          \
    {"privilege-level",  PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	                            \
     "Specify  the  privilege  level to be used. "		                                    \
     "The currently available privilege levels are CALLBACK, USER, OPERATOR, ADMIN, and OEM. "	    \
     "Defaults to ADMIN if not specified.", 14}     

#define ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS                               \
    {"workaround-flags",  WORKAROUND_FLAGS_KEY, "WORKAROUNDS", 0, 	   \
     "Specify workarounds to vendor compliance issues.", 16}	           \

#define ARGP_COMMON_SDR_OPTIONS                                                                                      \
    {"flush-cache", FLUSH_CACHE_KEY,  0, 0,                                                                          \
     "Flush a cached version of the sensor data repository (SDR) cache.", 17},                                       \
    {"quiet-cache", QUIET_CACHE_KEY,  0, 0,                                                                          \
     "Do not output information about cache creation/deletion.", 18},                                                \
    {"sdr-cache-directory", SDR_CACHE_DIR_KEY, "DIRECTORY", 0,                                                       \
     "Specify an alternate directory for sensor data repository (SDR) caches to be stored or read from.", 19} 

#define ARGP_COMMON_HOSTRANGED_OPTIONS                                     \
    {"buffer-output", BUFFER_KEY, 0, 0,                                    \
      "Buffer hostranged output.", 20},                                    \
    {"consolidate-output", CONSOLIDATE_KEY, 0, 0,                          \
     "Consolidate hostranged output.", 21},                                \
    {"fanout", FANOUT_KEY, "NUM", 0,                                       \
     "Specify multiple host fanout.", 22},                                 \
    {"eliminate", ELIMINATE_KEY, 0, 0,                                     \
     "Eliminate undetected nodes.", 23}

#ifndef NDEBUG

#define ARGP_COMMON_OPTIONS_DEBUG                                          \
    {"debug",     DEBUG_KEY, 0, 0, 	                                   \
     "Turn on debugging.", 24}                                             

#endif

struct common_cmd_args 
{
  ipmi_driver_type_t driver_type;
  int disable_auto_probe;
  unsigned int driver_address;
  char *driver_device;
  unsigned int register_spacing;
  unsigned int session_timeout;
  unsigned int retry_timeout;
  char *host;
  char *username;
  char *password;
  char k_g[IPMI_MAX_K_G_LENGTH];
  int k_g_configured;
  int authentication_type;
  int cipher_suite_id;
  int privilege_level;
  unsigned int workaround_flags;
  unsigned int flags;
};

struct sdr_cmd_args
{  
  int flush_cache_wanted;
  int quiet_cache_wanted;
  int sdr_cache_dir_wanted;
  char *sdr_cache_dir;
};

struct hostrange_cmd_args
{
  int buffer_hostrange_output;
  int consolidate_hostrange_output;
  int fanout;
  int eliminate;
};

#define IPMI_OUTOFBAND_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO_STR      "idzero"
#define IPMI_OUTOFBAND_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION_STR "forcepermsg"
#define IPMI_OUTOFBAND_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE_STR   "unexpectedauth"
#define IPMI_OUTOFBAND_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER_STR  "endianseq"

#define IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_INTEL_2_0_SESSION_STR       "intel20"
#define IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION_STR  "supermicro20"
#define IPMI_OUTOFBAND_2_0_WORKAROUND_FLAGS_SUN_2_0_SESSION_STR         "sun20"

int parse_driver_type(char *str);

int parse_outofband_workaround_flags(char *str);

int parse_outofband_2_0_workaround_flags(char *str);

int parse_inband_workaround_flags(char *str);

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

void init_common_cmd_args (struct common_cmd_args *cmd_args);
void free_common_cmd_args (struct common_cmd_args *cmd_args);
void verify_common_cmd_args (struct common_cmd_args *cmd_args);

void init_sdr_cmd_args (struct sdr_cmd_args *cmd_args);
void free_sdr_cmd_args (struct sdr_cmd_args *cmd_args);
void verify_sdr_cmd_args (struct sdr_cmd_args *cmd_args);

void init_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args);
void free_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args);
void verify_hostrange_cmd_args (struct hostrange_cmd_args *cmd_args);

#endif
