/* 
   Copyright (C) 2008 FreeIPMI Core Team

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

#ifndef _BMC_CONFIG_MAP_H
#define _BMC_CONFIG_MAP_H

#include "ipmi-chassis-config.h"

int power_restore_policy_number (const char *string);

char *power_restore_policy_string (uint8_t value);

int bios_boot_type_number (const char *string);

char *bios_boot_type_string (uint8_t value);

int boot_device_number (const char *string);

char *boot_device_string (uint8_t value);

int firmware_bios_verbosity_number (const char *string);

char *firmware_bios_verbosity_string (uint8_t value);

int console_redirection_number (const char *string);

char *console_redirection_string (uint8_t value);

#endif
