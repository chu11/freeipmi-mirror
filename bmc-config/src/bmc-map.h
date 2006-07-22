/* 
   ipmi_map.h: map functions
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */


#ifndef _BMC_IPMI_MAP_H
#define _BMC_IPMI_MAP_H

#include "bmc-common.h"
#include "bmc-sections.h"
#include "bmc-types.h"

int channel_access_mode (const char *string);

char *channel_access_mode_string (uint8_t mode);

uint8_t get_privilege_limit_number (const char *value);

char *get_privilege_limit_string (uint8_t limit);

int privilege_level_number (const char *string);

char *privilege_level_string (uint8_t value);

int rmcpplus_priv_number (const char *value);

char *rmcpplus_priv_string (int value);

int ip_address_source_number (const char *source);

char *ip_address_source_string (uint8_t source);

int power_restore_policy_number (const char *string);

char *power_restore_policy_string (uint8_t value);

int connect_mode_number (const  char *string);

char *connect_mode_string (uint8_t mode);

int flow_control_number (const char *string);

char *flow_control_string (uint8_t value);

int bit_rate_number (const char *string);

char * bit_rate_string (uint8_t value);

int sol_bit_rate_number (const char *string);

char *sol_bit_rate_string (uint8_t value);

#endif
