/* 

   bmc-sections.h

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


#ifndef _BMC_SECTIONS_H_
#define _BMC_SECTIONS_H_

#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"

#define BMC_CHECKOUT_KEY_COMMENTED_OUT                  0x1
#define BMC_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY   0x2
#define BMC_DO_NOT_CHECKOUT                             0x4

#define same(a,b) (strcasecmp(a,b) == 0)

#define add_section(db, extra) do { \
  if (db) {                         \
    struct section *trav = db;      \
    while (trav->next)              \
      trav = trav->next;            \
    trav->next = extra;             \
  } else {                          \
    db = extra;                     \
  }                                 \
} while (0)

#define add_keyvalue(_sect, _key, _doc, _f, _o, _i, _d, _v) do { \
  struct keyvalue *_new ;                                        \
  _new = (void *) calloc (1, sizeof (*_new));                    \
  _new->key = _key;                                              \
  _new->desc = _doc;                                             \
  _new->flags = _f;                                              \
  _new->commit = _i;                                             \
  _new->checkout = _o;                                           \
  _new->diff = _d;                                               \
  _new->validate = _v;                                           \
  if (_sect->keyvalues) {                                        \
    struct keyvalue *trav = _sect->keyvalues;                    \
    while (trav->next)                                           \
      trav = trav->next;                                         \
    trav->next = _new;                                           \
  } else {                                                       \
    _sect->keyvalues = _new;                                     \
  }                                                              \
} while (0)

struct section {
  struct section *next;
  const char *section;
  struct keyvalue *keyvalues;
};

struct keyvalue {
  struct keyvalue *next;
  const char *key;
  const char *desc;
  unsigned int flags;
  char *value;

  /* checkout procedure fills the value into kv->value as printable string */
  int (*checkout) (const struct bmc_config_arguments *args, 
		   const struct section *sect,
		   struct keyvalue *kv);

  /* commit procedure takes string value from kv->value and converts and
     does ipmi calls to set it */
  int (*commit) (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv);

  /* diff procedure finds the difference with the ipmi actual value
     and kv->value */
  int (*diff) (const struct bmc_config_arguments *args,
	       const struct section *sect,
	       const struct keyvalue *kv);

  /* validate procedure finds if value is suitable to be set as kv->value */
  int (*validate) (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const char *value);
};

struct section * bmc_sections_init (struct bmc_config_arguments *args);

struct keyvalue * bmc_section_find_keyvalue (const char *section_name,
					     const char *key_name,
					     const struct section *sections);

int bmc_section_set_value (const char *section_name,
			   const char *key_name,
			   const char *value,
			   struct bmc_config_arguments *args,
			   struct section *sections);

int bmc_section_commit_value (const char *section_name,
			      const char *key_name,
			      const char *value,
			      struct bmc_config_arguments *args,
			      struct section *sections);

int bmc_section_diff_value (const char *section_name,
			    const char *key_name,
			    const char *value,
			    struct bmc_config_arguments *args,
			    struct section *sections);

int bmc_sections_list (struct bmc_config_arguments *args, 
                       struct section *sections);

#endif /* _BMC_SECTIONS_H_ */
