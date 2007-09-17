/* 

   config-parse.h - function prototypes

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/


#ifndef _CONFIG_PARSE_H_
#define _CONFIG_PARSE_H_

#include "config-parse.h"

struct config_keypair
{
  char *section_name;
  char *key_name;
  struct config_keypair *next;
};

struct config_keyvalue {
  char *key_name;
  char *description;
  unsigned int flags;
  char *value;
  struct config_keyvalue *next;
};

struct config_section {
  Section_Comment comment; 
  unsigned int flags;
  Section_Checkout checkout;
  Section_Commit commit;
  Section_Diff diff;
  Section_Validate validate;
  struct config_keyvalue *keyvalues;
  struct config_section *next;
};

/* returns 0 on success, -1 on error */
int config_parse (struct config_section *sections, FILE *fp, int debug);

#endif /* _CONFIG_PARSE_H_ */
