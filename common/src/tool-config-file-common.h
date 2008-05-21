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

#ifndef _TOOL_CONFIG_FILE_COMMON_H
#define _TOOL_CONFIG_FILE_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "tool-cmdline-common.h"
#include "conffile.h"

#define CONFIG_FILE_NONE        0x00
#define CONFIG_FILE_INBAND      0x01
#define CONFIG_FILE_OUTOFBAND   0x02
#define CONFIG_FILE_MISC        0x03

int config_file_parse(const char *filename,
                      int no_error_if_not_found,
                      struct common_cmd_args *cmd_args, 
                      unsigned int support,
                      struct conffile_option *tool_options,
                      unsigned int tool_options_len,
                      void *app_ptr,
                      int app_data);

#endif
