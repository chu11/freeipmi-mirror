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
    REGISTER_SPACING_KEY = 'r',
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
    SDR_CACHE_DIR_KEY = 135,
    BUFFER_KEY = 'B',
    CONSOLIDATE_KEY = 'C',
    FANOUT_KEY = 'F',
    ELIMINATE_KEY = 'E',
    WORKAROUND_FLAGS_KEY = 'W',
    DEBUG_KEY = 136
  };

#define ARGP_COMMON_OPTIONS_DRIVER                                          \
    {"driver-type",    DRIVER_TYPE_KEY, "IPMIDRIVER", 0, 	            \
     "Use this IPMIDRIVER instead of auto selection.  "		            \
     "Allowed values are LAN, LAN_2_0, KCS, SMIC, SSIF, and OPENIPMI.", 0}  \

#define ARGP_COMMON_OPTIONS_INBAND                                         \
    {"no-probing",     NO_PROBING_KEY, 0, 0, 	                           \
     "Do not probe IPMI devices.", 1},		                           \
    {"driver-address", DRIVER_ADDRESS_KEY, "DRIVERADDR", 0,                \
     "Use this DRIVERADDR address instead of probed one.", 2}, 	           \
    {"driver-device",  DRIVER_DEVICE_KEY, "DEVICE", 0,                     \
     "Use this DEVICE for IPMI driver.", 3},                               \
    {"register-spacing", REGISTER_SPACING_KEY, "REGISTERSPACING", 0,       \
     "Use this REGISTERSPACING instead of the probed one", 4}

#define ARGP_COMMON_OPTIONS_OUTOFBAND                                             \
    {"hostname",       HOSTNAME_KEY, "IPMIHOST", 0, 			          \
     "Connect to IPMIHOST.", 5},					          \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON

#define ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED                                  \
    {"hostname",       HOSTNAME_KEY, "IPMIHOST", 0, 			          \
     "Connect to IPMIHOST.  IPMIHOST may include host ranges", 5},                \
    ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON

#define ARGP_COMMON_OPTIONS_OUTOFBAND_COMMON                                      \
    {"username",       USERNAME_KEY, "USERNAME", 0, 			          \
     "Use USERNAME instead of NULL.", 6},                                         \
    {"password",       PASSWORD_KEY, "PASSWORD", 0,	                          \
     "Use PASSWORD instead of NULL.", 7},                                         \
    {"password-prompt", PASSWORD_PROMPT_KEY, 0, 0,	                          \
     "Prompt for PASSWORD instead of NULL.", 8},                                  \
    {"k-g",       K_G_KEY, "K_G", 0,	                                          \
     "Use K_G key instead of NULL.", 9},                                          \
    {"k-g-prompt", K_G_PROMPT_KEY, 0, 0,	                                  \
     "Prompt for K_G instead of NULL.", 10},                                      \
    {"retry-timeout", RETRY_TIMEOUT_KEY, "RETRY_TIMEOUT", 0,                      \
     "Use RETRY_TIMEOUT milliseconds before re-sending LAN packets.", 11},        \
    {"session-timeout", SESSION_TIMEOUT_KEY, "SESSION_TIMEOUT", 0,                \
     "Use SESSION_TIMEOUT milliseconds before ending a session.", 12}

#define ARGP_COMMON_OPTIONS_AUTHTYPE                                       \
    {"auth-type",      AUTHENTICATION_TYPE_KEY, "AUTHTYPE", 0, 		   \
     "Use AUTHTYPE instead of MD5.  "				           \
     "Allowed values are NONE, MD2, MD5, and PLAIN.", 12}	           \

#define ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID                                \
    {"cipher-suite-id",      CIPHER_SUITE_ID_KEY, "CIPHER_SUITE_ID", 0,    \
     "Use CIPHER_SUITE_ID instead of 3.  "				   \
     "Allowed values are 0, 1, 2, 3, 6, 7, 8, 11, 12.", 13}	           \

#define ARGP_COMMON_OPTIONS_PRIVLEVEL_USER                                 \
    {"priv-level",     PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	   \
     "Use this PRIVILEGE-LEVEL instead of USER.  "		           \
     "Allowed values are CALLBACK, USER, OPERATOR, ADMIN and OEM.", 14}     

#define ARGP_COMMON_OPTIONS_PRIVLEVEL_ADMIN                                \
    {"priv-level",     PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	   \
     "Use this PRIVILEGE-LEVEL instead of ADMIN.  "		           \
     "Allowed values are CALLBACK, USER, OPERATOR, ADMIN and OEM.", 15}     

#define ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS                               \
    {"workaround-flags",  WORKAROUND_FLAGS_KEY, "WORKAROUNDS", 0, 	   \
     "Specify workarounds to vendor compliance issues.", 16}	           \

#define ARGP_COMMON_SDR_OPTIONS                                            \
    {"flush-cache", FLUSH_CACHE_KEY,  0, 0,                                \
     "Flush sensor SDR cache.", 17},                                       \
    {"quiet-cache", QUIET_CACHE_KEY,  0, 0,                                \
     "Do not output cache creation information.", 18},                     \
    {"sdr-cache-directory", SDR_CACHE_DIR_KEY, "DIRECTORY", 0,             \
     "Use DIRECTORY for sensor cache.", 19} 

#define ARGP_COMMON_HOSTRANGED_OPTIONS                                     \
    {"buffer-output", BUFFER_KEY, 0, 0,                                    \
      "Buffer hostranged output.", 20},                                    \
    {"consolidate-output", CONSOLIDATE_KEY, 0, 0,                          \
     "Consolidate hostranged output.", 21},                                \
    {"fanout", FANOUT_KEY, "NUM", 0,                                       \
     "Set multiple host fanout.", 22},                                     \
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

unsigned int parse_outofband_workaround_flags(char *str);

unsigned int parse_outofband_2_0_workaround_flags(char *str);

unsigned int parse_inband_workaround_flags(char *str);

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
