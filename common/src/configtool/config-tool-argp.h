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

#ifndef _CONFIG_TOOL_ARGP_H_
#define _CONFIG_TOOL_ARGP_H_

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdint.h>

#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */

#include "config-tool-common.h"

enum config_argp_option_keys
  {
    CONFIG_ARGP_CHECKOUT_KEY = 'o',
    CONFIG_ARGP_COMMIT_KEY = 'c',
    CONFIG_ARGP_DIFF_KEY = 'd',
    CONFIG_ARGP_FILENAME_KEY_LEGACY = 'f',
    CONFIG_ARGP_FILENAME_KEY = 'n',
    CONFIG_ARGP_KEYPAIR_KEY = 'e',
    CONFIG_ARGP_SECTIONS_KEY = 'S',
    CONFIG_ARGP_LIST_SECTIONS_KEY = 'L',
    CONFIG_ARGP_VERBOSE_KEY = 'v',
  };

#define CONFIG_ARGP_COMMON_OPTIONS                                                            \
    {"checkout", CONFIG_ARGP_CHECKOUT_KEY, 0, 0,                                              \
     "Fetch configuration information.", 31},                                                 \
    {"commit", CONFIG_ARGP_COMMIT_KEY, 0, 0,                                                  \
     "Update configuration information from a config file or key pairs.", 32},                \
    {"diff", CONFIG_ARGP_DIFF_KEY, 0, 0,                                                      \
     "Show differences between stored information and a config file or key pairs.", 33},      \
    {"filename", CONFIG_ARGP_FILENAME_KEY, "FILENAME", 0,                                     \
     "Specify a config file for checkout/commit/diff.", 34},                                  \
    {"key-pair", CONFIG_ARGP_KEYPAIR_KEY, "KEY-PAIR", 0,                                      \
     "Specify KEY=VALUE pairs for checkout/commit/diff.", 35},                                \
    {"section", CONFIG_ARGP_SECTIONS_KEY, "SECTION", 0,                                       \
     "Specify a SECTION for checkout.", 36},                                                  \
    {"listsections", CONFIG_ARGP_LIST_SECTIONS_KEY, 0, 0,                                     \
     "List available sections for checkout.", 37},                                            \
    {"verbose", CONFIG_ARGP_VERBOSE_KEY, 0, 0,                                                \
     "Print additional detailed information.", 38}

/* legacy short-option */
#define CONFIG_ARGP_COMMON_OPTIONS_LEGACY                                                     \
    {"foobar", CONFIG_ARGP_FILENAME_KEY_LEGACY, "FILENAME", OPTION_HIDDEN,                    \
     "Specify a config file for checkout/commit/diff.", 39}

void init_config_args (struct config_arguments *config_args);

error_t config_parse_opt (int key,
                          char *arg,
                          struct argp_state *state,
                          struct config_arguments *config_args);

void config_args_validate (struct config_arguments *config_args);

#endif /* _CONFIG_TOOL_ARGP_H_ */
