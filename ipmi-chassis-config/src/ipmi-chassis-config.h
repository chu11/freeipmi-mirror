/*
  Copyright (C) 2008 FreeIPMI Core Team

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

#ifndef _IPMI_CHASSIS_CONFIG_H
#define _IPMI_CHASSIS_CONFIG_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"
#include "pstdout.h"

#include "config-tool-argp.h"
#include "config-tool-common.h"
#include "config-tool-comment.h"
#include "config-tool-checkout.h"
#include "config-tool-commit.h"
#include "config-tool-diff.h"
#include "config-tool-parse.h"
#include "config-tool-section.h"
#include "config-tool-utils.h"
#include "config-tool-validate.h"

struct ipmi_chassis_config_arguments
{
  struct config_arguments config_args;
};

typedef struct ipmi_chassis_config_prog_data
{ 
  char *progname;
  struct ipmi_chassis_config_arguments *args;
  int hosts_count;
} ipmi_chassis_config_prog_data_t;

typedef struct ipmi_chassis_config_state_data
{ 
  ipmi_chassis_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx;
  pstdout_state_t pstate;
  
  /* achu: workaround for IPMI limitation */
  int front_panel_enable_standby_button_for_entering_standby_initialized;
  uint8_t front_panel_enable_standby_button_for_entering_standby;
  int front_panel_enable_diagnostic_interrupt_button_initialized;
  uint8_t front_panel_enable_diagnostic_interrupt_button;
  int front_panel_enable_reset_button_initialized;
  uint8_t front_panel_enable_reset_button;
  int front_panel_enable_power_off_button_for_power_off_only_initialized;
  uint8_t front_panel_enable_power_off_button_for_power_off_only;
} ipmi_chassis_config_state_data_t;

#endif
