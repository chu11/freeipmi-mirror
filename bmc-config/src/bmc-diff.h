/* 
   bmd-diff.h: diff functions
   
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


#ifndef _BMC_DIFF_H
#define _BMC_DIFF_H

#include "bmc-config.h"
#include "bmc-sections.h"

bmc_err_t bmc_diff (bmc_config_state_data_t *state_data);

void report_diff (const char *section,
                  const char *key,
                  const char *input_value,
                  const char *actual_value);

#endif
