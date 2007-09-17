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

#include <stdio.h>

#include "config-parse.h"

#define CONFIG_CHECKOUT_KEY_COMMENTED_OUT                  0x01
#define CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY   0x02
#define CONFIG_DO_NOT_CHECKOUT                             0x04

typedef enum
  {
    CONFIG_ERR_FATAL_ERROR = -2,
    CONFIG_ERR_NON_FATAL_ERROR = -1,
    CONFIG_ERR_SUCCESS = 0,
  } config_err_t;

typedef enum
  {
    CONFIG_DIFF_FATAL_ERROR = -2,
    CONFIG_DIFF_NON_FATAL_ERROR = -1,
    CONFIG_DIFF_SAME = 0,
    CONFIG_DIFF_DIFFERENT = 1,
  } config_diff_t;

typedef enum
  {
    CONFIG_VALIDATE_FATAL_ERROR = -2,
    CONFIG_VALIDATE_INVALID_VALUE = -1,
    CONFIG_VALIDATE_VALID_VALUE = 0,
  } config_validate_t;

struct config_key
{
  char *key_name;
  char *description;
  unsigned int flags;
  struct config_key *next;
};

struct config_keyvalue {
  char *key_name;
  char *value;
  struct config_keyvalue *next;
};

/* Output a comment/instructions for a particular section */
typedef config_err_t (*Section_Comment) (const char *section_name,
                                         FILE *fp);

/* checkout procedure fills the value into kv->value as printable string */
typedef config_err_t (*Section_Checkout) (const char *section_name,
                                          FILE *fp);

/* commit procedure takes string value from kv->value and converts and
   does ipmi calls to set it */
typedef config_err_t (*Section_Commit) (const char *section_name,
                                        struct config_keyvalue *keyvalues);

/* diff procedure finds the difference with the ipmi actual value
   and kv->value */
typedef config_diff_t (*Section_Diff) (const char *section_name,
                                       struct config_keyvalue *keyvalues);

/* validate procedure finds if value is suitable to be set as kv->value */
typedef config_validate_t (*Section_Validate) (const char *section_name,
                                               struct config_keyvalue *keyvalues);

struct config_section {
  char *section_name;
  unsigned int flags;
  Section_Comment comment; 
  Section_Checkout checkout;
  Section_Commit commit;
  Section_Diff diff;
  Section_Validate validate;
  /* keys in this section */
  struct config_key *keys;
  /* key and values read from the file */
  struct config_keyvalue *keyvalues;
  struct config_keyvalue *keyvalues_last;
  struct config_section *next;
};

/* returns 0 on success, -1 on error */
int config_parse (struct config_section *sections, FILE *fp, int debug);

#endif /* _CONFIG_PARSE_H_ */
