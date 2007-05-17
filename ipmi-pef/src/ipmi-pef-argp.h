/* 
   $Id: ipmi-pef-argp.h,v 1.7 2007-05-17 17:26:52 chu11 Exp $ 
   
   ipmi-pef-argp.h - Platform Event Filtering utility.
   
   Copyright (C) 2005 FreeIPMI Core Team
   
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

#ifndef _IPMI_PEF_ARGP_H
#define _IPMI_PEF_ARGP_H

#include "ipmi-pef.h"

void ipmi_pef_argp_parse (int argc, char **argv, struct ipmi_pef_arguments *cmd_args);
int ipmi_pef_args_validate (struct ipmi_pef_arguments *args);

#endif
