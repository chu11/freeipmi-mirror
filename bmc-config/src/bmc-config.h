/* 

   bmc-config.h 

   Copyright (C) 2006 FreeIPMI Core Team

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


#ifndef _BMC_CONFIG_H_
#define _BMC_CONFIG_H_

#include <freeipmi/freeipmi.h>

#include "argp-common.h"

typedef enum
  {
    BMC_ACTION_CHECKOUT = 1,
    BMC_ACTION_COMMIT,
    BMC_ACTION_DIFF,
    BMC_ACTION_LIST_SECTIONS,
  } bmc_action_t;

typedef enum
  {
    BMC_ERR_FATAL_ERROR = -2,
    BMC_ERR_NON_FATAL_ERROR = -1,
    BMC_ERR_SUCCESS = 0,
  } bmc_err_t;

typedef enum
  {
    BMC_DIFF_FATAL_ERROR = -2,
    BMC_DIFF_NON_FATAL_ERROR = -1,
    BMC_DIFF_SAME = 0,
    BMC_DIFF_DIFFERENT = 1,
  } bmc_diff_t;

typedef enum
  {
    BMC_VALIDATE_FATAL_ERROR = -2,
    BMC_VALIDATE_INVALID_VALUE = -1,
    BMC_VALIDATE_VALID_VALUE = 0,
  } bmc_validate_t;

struct keypair
{
  char *keypair;
  struct keypair *next;
};

struct sectionstr
{
  char *sectionstr;
  struct sectionstr *next;
};

struct bmc_config_arguments
{
  struct common_cmd_args common;
  ipmi_device_t dev;
  unsigned char silent;
  unsigned char verbose;

  char *filename;
  struct keypair *keypairs;
  struct sectionstr *sectionstrs;

  bmc_action_t action;
};

typedef struct bmc_config_prog_data
{
  char *progname;
  struct bmc_config_arguments *args;
  uint32_t debug_flags;
} bmc_config_prog_data_t;


#endif /* _BMC_CONFIG_H_ */
