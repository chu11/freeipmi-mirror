/* 
   [Free IPMI SHell]
   An extensible console based shell for managing large number of IPMI 
   compatible systems.

   fish.h: fish header

   Copyright (C) 2003, 2004 FreeIPMI Core Team

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

#define FI_DEFAULT_SOCK_TIMEOUT     3000
#define FI_DEFAULT_RETRY_COUNT      1

#define FI_DEFAULT_SDR_REPO_CACHE_FILENAME    IPMI_DEFAULT_SDR_REPO_CACHE_FILENAME

int fi_get_verbose ();
void fi_set_verbose (int verbose);
void running_for_first_time (void);
char *get_default_prompt (void);
void set_default_prompt (char *prompt_default_value);
int get_setup_mode (void);
void set_setup_mode (int setup_mode_value);
void set_driver_poll_interval (int driver_poll_interval_value);
int fi_get_sockfd (void);
unsigned int fi_get_sock_timeout (void);
void fi_set_sock_timeout (unsigned int sock_timeout);
unsigned int fi_get_retry_count (void);
int get_script_argc ();
char **get_script_argv ();
u_int16_t fi_get_sms_io_base ();
void fi_set_sms_io_base (u_int16_t io_base);
