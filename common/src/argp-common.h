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
    AUTHENTICATION_TYPE_KEY = 'a', 
    PRIVILEGE_LEVEL_KEY = 'l'
  };

#define ARGP_COMMON_OPTIONS_INBAND                                         \
    {"no-probing",     NO_PROBING_KEY, 0, 0, 	                           \
     "Do not probe IPMI devices.", 0},		                           \
    {"driver-type",    DRIVER_TYPE_KEY, "IPMIDRIVER", 0, 	           \
     "Use this IPMIDRIVER instead of auto selection.  "		           \
     "Allowed values are KCS, SMIC, SSIF and LAN.", 1},		           \
    {"driver-address", DRIVER_ADDRESS_KEY, "DRIVERADDR", 0,                \
     "Use this DRIVERADDR address instead of probed one.", 2}, 	           \
    {"driver-device",  DRIVER_DEVICE_KEY, "DEVICE", 0,                     \
     "Use this DEVICE for IPMI driver.", 3},                               \
    {"register-spacing", REGISTER_SPACING_KEY, "REGISTERSPACING", 0,       \
     "Use this REGISTERSPACING instead of probed one", 4}

#define ARGP_COMMON_OPTIONS_OUTOFBAND                                      \
    {"hostname",       HOSTNAME_KEY, "IPMIHOST", 0, 			   \
     "Connect to IPMIHOST.", 5},					   \
    {"username",       USERNAME_KEY, "USERNAME", 0, 			   \
     "Use USERNAME instead of NULL.  Maximum USERNAME length is 16.", 6},  \
    {"password",       PASSWORD_KEY, "PASSWORD", 0, 			   \
     "Use PASSWORD instead of NULL.  Maximum PASSWORD length is 16.", 7},  \
    {"retry-timeout", RETRY_TIMEOUT_KEY, "RETRY_TIMEOUT", 0,               \
     "Use RETRY_TIMEOUT milliseconds before re-sending LAN packets.", 8},  \
    {"session-timeout", SESSION_TIMEOUT_KEY, "SESSION_TIMEOUT", 0,         \
     "Use SESSION_TIMEOUT milliseconds before ending a session.", 9}

#define ARGP_COMMON_OPTIONS_AUTHTYPE                                       \
    {"auth-type",      AUTHENTICATION_TYPE_KEY, "AUTHTYPE", 0, 		   \
     "Use AUTHTYPE instead of MD5.  "				           \
     "Allowed values are NONE, MD2, MD5, PLAIN.", 10}	                   \

#define ARGP_COMMON_OPTIONS_PRIVLEVEL                                      \
    {"priv-level",     PRIVILEGE_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 	   \
     "Use this PRIVILEGE-LEVEL instead of USER.  "		           \
     "Allowed values are CALLBACK, USER, OPERATOR, ADMIN and OEM.", 11}     

#define ARGP_COMMON_OPTIONS                                                \
       ARGP_COMMON_OPTIONS_INBAND,                                         \
       ARGP_COMMON_OPTIONS_OUTOFBAND,                                      \
       ARGP_COMMON_OPTIONS_AUTHTYPE,                                       \
       ARGP_COMMON_OPTIONS_PRIVLEVEL 

struct common_cmd_args 
{
  int disable_auto_probe;
  ipmi_driver_type_t driver_type;
  unsigned int driver_address;
  char *driver_device;
  unsigned int register_spacing;
  unsigned int session_timeout;
  unsigned int retry_timeout;
  char *host;
  char *username;
  char *password;
  int authentication_type;
  int privilege_level;
};

error_t common_parse_opt (int key, 
			  char *arg, 
			  struct argp_state *state, 
			  struct common_cmd_args *common_cmd_args);
void init_common_cmd_args (struct common_cmd_args *cmd_args);
void free_common_cmd_args (struct common_cmd_args *cmd_args);

#endif
