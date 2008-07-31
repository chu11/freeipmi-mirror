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

#ifndef _CONFIG_TOOL_CHECKOUT_H_
#define _CONFIG_TOOL_CHECKOUT_H_

#include "config-tool-common.h"
#include "pstdout.h"

config_err_t config_checkout_section(pstdout_state_t pstate,
                                     struct config_section *section,
                                     struct config_arguments *cmd_args,
                                     int all_keys_if_none_specified,
                                     FILE *fp,
                                     unsigned int line_length,
                                     void *arg);

config_err_t config_checkout (pstdout_state_t pstate,
                              struct config_section *sections,
                              struct config_arguments *cmd_args,
                              int all_keys_if_none_specified,
                              FILE *fp,
                              unsigned int line_length,
                              void *arg);

#endif /* _CONFIG_TOOL_CHECKOUT_H_ */
