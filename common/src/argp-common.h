/*
argp-common.h: common work for argp for all freeipmi tools.
Copyright (C) 2005 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _ARGP_COMMON_H
#define _ARGP_COMMON_H

enum argp_common_option_keys
  { 
    DRIVER_TYPE_KEY = 'D', 
    DUMMY_KEY = 129, 
    NO_PROBING_KEY = 130, 
    DRIVER_ADDRESS_KEY = 131, 
    DRIVER_DEVICE_KEY = 132, 
    HOSTNAME_KEY = 'h', 
    USERNAME_KEY = 'u', 
    PASSWORD_KEY = 'p', 
    AUTH_TYPE_KEY = 'a', 
    PRIV_LEVEL_KEY = 'l'
  };

#define ARGP_COMMON_OPTIONS                                                \
    {"no-probing",     NO_PROBING_KEY, 0, 0, 	                           \
     "Do not probe IPMI devices.", 0},		                           \
    {"driver-type",    DRIVER_TYPE_KEY, "IPMIDRIVER", 0, 	           \
     "Use this IPMIDRIVER instead of auto selection.  "		           \
     "Allowed values are KCS, SMIC, SSIF and LAN.", 1},		           \
    {"driver-address", DRIVER_ADDRESS_KEY, "DRIVERADDR", 0,                \
     "Use this DRIVERADDR address instead of probed one.", 2}, 	           \
    {"driver-device",  DRIVER_DEVICE_KEY, "DEVICE", 0,                     \
     "Use this DEVICE for IPMI driver.", 3},                               \
    {"hostname",       HOSTNAME_KEY, "IPMIHOST", 0, 			   \
     "Connect to IPMIHOST.", 4},					   \
    {"username",       USERNAME_KEY, "USERNAME", 0, 			   \
     "Use USERNAME instead of NULL.  Maximum USERNAME length is 16.", 5},  \
    {"password",       PASSWORD_KEY, "PASSWORD", 0, 			   \
     "Use PASSWORD instead of NULL.  Maximum PASSWORD length is 16.", 6},  \
    {"auth-type",      AUTH_TYPE_KEY, "AUTHTYPE", 0, 			   \
     "Use AUTHTYPE instead of NONE.  "				           \
     "Allowed values are NONE, MD2, MD5, PLAIN and OEM.", 7},	           \
    {"priv-level",     PRIV_LEVEL_KEY, "PRIVILEGE-LEVEL", 0, 		   \
     "Use this PRIVILEGE-LEVEL instead of USER.  "		           \
     "Allowed values are CALLBACK, USER, OPERATOR, ADMIN and OEM.", 8}      



struct common_cmd_args 
{
  int disable_auto_probe;
  ipmi_driver_type_t driver_type;
  unsigned int driver_address;
  char *driver_device;
  char *host;
  char *username;
  char *password;
  int auth_type;
  int priv_level;
};

error_t common_parse_opt (int key, 
			  char *arg, 
			  struct argp_state *state, 
			  struct common_cmd_args *common_cmd_args);
void init_common_cmd_args (struct common_cmd_args *cmd_args);
void free_common_cmd_args (struct common_cmd_args *cmd_args);

#endif
