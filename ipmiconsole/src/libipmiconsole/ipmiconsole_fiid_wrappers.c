/*****************************************************************************\
 *  $Id: ipmiconsole_fiid_wrappers.c,v 1.7 2007-10-17 23:13:01 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *  
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmiconsole is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmiconsole is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "ipmiconsole_fiid_wrappers.h"
#include "ipmiconsole_ctx.h"
#include "ipmiconsole_debug.h"

int32_t
Fiid_template_len_bytes(ipmiconsole_ctx_t c, fiid_template_t tmpl)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(tmpl);

  if ((rv = fiid_template_len_bytes(tmpl)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_template_len_bytes: %s", strerror(errno)));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}

int32_t
Fiid_template_block_len_bytes(ipmiconsole_ctx_t c, fiid_template_t tmpl, char *field_start, char *field_end)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(tmpl);
  assert(field_start);
  assert(field_end);

  if ((rv = fiid_template_block_len_bytes(tmpl, field_start, field_end)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_template_len_bytes: field_start=%s; field_end=%s; %s", field_start, field_end, strerror(errno)));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}

fiid_obj_t
Fiid_obj_create(ipmiconsole_ctx_t c, fiid_template_t tmpl) 
{
  fiid_obj_t obj;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(tmpl);

  if (!(obj = fiid_obj_create(tmpl)))
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_create: %s", strerror(errno)));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_OUT_OF_MEMORY);
      return NULL;
    }

  return obj;
}

int8_t
Fiid_obj_clear(ipmiconsole_ctx_t c, fiid_obj_t obj)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));

  if ((rv = fiid_obj_clear(obj)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_clear: %s", fiid_strerror(fiid_obj_errnum(obj))));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}

int8_t
Fiid_obj_clear_field(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);

  if ((rv = fiid_obj_clear_field(obj, field)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_clear_field: %s", fiid_strerror(fiid_obj_errnum(obj))));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}

void
Fiid_obj_destroy(ipmiconsole_ctx_t c, fiid_obj_t obj) 
{
  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));

  fiid_obj_destroy(obj);
}

int8_t
Fiid_obj_get(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint64_t *val)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);
  assert(val);

  if ((rv = fiid_obj_get(obj, field, val)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_get: field=%s; %s", field, fiid_strerror(fiid_obj_errnum(obj))));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  if (!rv)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_get: field=%s; no data set", field));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }
  
  return rv;
}

int32_t 
Fiid_obj_get_data(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);
  assert(data);
  assert(data_len);

  if ((rv = fiid_obj_get_data(obj, field, data, data_len)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_get_data: field=%s; %s", field, fiid_strerror(fiid_obj_errnum(obj))));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}

int8_t
Fiid_obj_set(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint64_t val)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);

  if ((rv = fiid_obj_set(obj, field, val)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_set: field=%s; %s", field, fiid_strerror(fiid_obj_errnum(obj))));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}

int32_t 
Fiid_obj_set_data(ipmiconsole_ctx_t c, fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);
  assert(data);
  assert(data_len);

  if ((rv = fiid_obj_set_data(obj, field, data, data_len)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_set_data: %s", fiid_strerror(fiid_obj_errnum(obj))));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}

int32_t 
Fiid_obj_set_all(ipmiconsole_ctx_t c, fiid_obj_t obj, uint8_t *data, uint32_t data_len)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMICONSOLE_CTX_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(data);
  assert(data_len);

  if ((rv = fiid_obj_set_all(obj, data, data_len)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG(c, ("fiid_obj_set_all: %s", fiid_strerror(fiid_obj_errnum(obj))));
      ipmiconsole_ctx_set_errnum(c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return -1;
    }

  return rv;
}
