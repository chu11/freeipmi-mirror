/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifndef _IPMI_FIID_UTIL_H
#define _IPMI_FIID_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/fiid/fiid.h"

#define FIID_TEMPLATE_FREE(__tmpl)		\
  do {						\
    if ((__tmpl))				\
      {						\
	fiid_template_free ((__tmpl));		\
	(__tmpl) = NULL;			\
      }						\
  } while (0)

#define FIID_OBJ_DESTROY(__obj)			\
  do {						\
    if ((__obj))				\
      {						\
	fiid_obj_destroy ((__obj));		\
	(__obj) = NULL;				\
      }						\
  } while (0)

  void set_errno_by_fiid_object (fiid_obj_t obj);

  void set_errno_by_fiid_iterator (fiid_iterator_t iter);

  int Fiid_obj_packet_valid (fiid_obj_t obj);

  int Fiid_obj_template_compare (fiid_obj_t obj, fiid_template_t tmpl);

  int Fiid_obj_field_lookup (fiid_obj_t obj, char *field);

  int Fiid_obj_get (fiid_obj_t obj, char *field, uint64_t *val);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-fiid-util.h */

