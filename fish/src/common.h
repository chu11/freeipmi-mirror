/* 
   $Id: common.h,v 1.2 2005-10-11 04:06:44 balamurugan Exp $ 

   common.c - Common header definitions.

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

#ifndef _COMMON_H
#define _COMMON_H

#include "freeipmi.h"

#include <argp.h>
#include <pwd.h>
#include <termios.h>
#include <guile/gh.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "xmalloc.h"
#include "fish-argp.h"
#include "fi-utils.h"
#include "guile-wrapper.h"
#include "extension.h"
#include "interpreter.h"
#include "ipmi-wrapper.h"
#include "bmc-conf2.h"
#include "scm-procedures.h"
#include "fi-commands.h"
#include "fish.h"

#define SET_SELECTOR      0x0
#define BLOCK_SELECTOR    0x0

#endif
