/* 

   bmc-config.h - function prototypes

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

#include "bmc-types.h"
#include "bmc-sections.h"

int bmc_argp (int argc, char *argv[], struct arguments *args);
int bmc_commit (struct arguments *args, struct section *sections);
int bmc_checkout (struct arguments *args, struct section *sections);
int bmc_diff (struct arguments *args, struct section *sections);
int bmc_parser (struct arguments *args, struct section *sections, FILE *fp);
void report_diff (const char *section, 
		  const char *key, 
		  const char *input_value,
		  const char *actual_value);

#endif /* _BMC_CONFIG_H_ */
