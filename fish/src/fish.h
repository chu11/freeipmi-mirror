/* 
   [Free IPMI SHell]
   An extensible console based shell for managing large number of IPMI 
   compatible systems.

   fish.h: fish header

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#ifndef _FISH_H
#define _FISH_H

#define FI_DEFAULT_PROMPT_STRING    "fish> "
#define FI_CONFIG_DIRECTORY         ".fish"
#define FI_CONFIG_DIRECTORY_MODE    S_IRWXU
#define FI_CONFIG_FILE              ".fish/fish.scm"
#define FI_CONFIG_FILE_MODE         S_IRWXU
#define FI_DEFAULT_CONFIG_DIRECTORY PATH_CFG "/fish"
#define FI_DEFAULT_CONFIG_FILE      FI_DEFAULT_CONFIG_DIRECTORY "/fish.scm"

#define FI_GLOBAL_EXTENSIONS_DIRECTORY DATADIR "/fish/extensions"
#define FI_LOCAL_EXTENSIONS_DIRECTORY  FI_CONFIG_DIRECTORY "/extensions"
#define FI_INIT_FILE                   "init.scm"
#define FI_GLOBAL_INIT_FILE            FI_GLOBAL_EXTENSIONS_DIRECTORY "/" FI_INIT_FILE

#define FI_SDR_CACHE_DIR                "sdr-cache"
#define FI_SDR_CACHE_FILENAME_PREFIX    "sdr-cache"

void set_setup_mode (int setup_mode_value);

#endif
